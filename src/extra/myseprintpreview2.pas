{ <PiNote - free source code editor>

Copyright (C) <2021> <Enzo Antonio Calogiuri> <ecalogiuri(at)gmail.com>

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}
unit MySePrintPreview2;

{$M+}
{$I MySynEdit.inc}

interface

uses
  Classes, SysUtils, Controls, LMessages, Graphics, Forms, MySEPrint,
  LCLType{$IFdef MSWindows}, Windows{$ENDIF}, Dialogs, LCLintf;

Type
  TPreviewPageEvent = procedure(Sender: TObject; PageNumber: Integer) of object;
  TSynPreviewScale = (pscWholePage, pscPageWidth, pscUserScaled);

  { TSynEditPrintPreview2 }

  TSynEditPrintPreview2             = Class(TCustomControl)
    Protected
      FBorderStyle: TBorderStyle;
      FSynEditPrint: TMySEPrint;
      FScaleMode: TSynPreviewScale;
      FScalePercent: Integer;
          // these are in pixels ( = screen device units)
      FVirtualSize: TPoint;
      FVirtualOffset: TPoint;
      FPageSize: TPoint;
      FScrollPosition: TPoint;
      FPageBG: TColor;
      FPageNumber: Integer;
      FShowScrollHint: Boolean;
      FOnPreviewPage: TPreviewPageEvent;
      FOnScaleChange: TNotifyEvent;                                               // JD 2002-01-9
      FWheelAccumulator: Integer;

      procedure SetBorderStyle(Value: TBorderStyle); Override;
      procedure SetPageBG(Value: TColor);
      procedure SetSynEditPrint(Value: TMySEPrint);
      procedure SetScaleMode(Value: TSynPreviewScale);
      procedure SetScalePercent(Value: Integer);

      Procedure CreateParams(var Params: TCreateParams); override;
      function GetPageHeightFromWidth(AWidth: Integer): Integer;
      function GetPageHeight100Percent: Integer;
      function GetPageWidthFromHeight(AHeight: Integer): Integer;
      function GetPageWidth100Percent: Integer;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure ScrollHorzFor(Value: Integer);
      procedure ScrollHorzTo(Value: Integer); virtual;
      procedure ScrollVertFor(Value: Integer);
      procedure ScrollVertTo(Value: Integer); virtual;
      procedure UpdateScrollbars; virtual;

      procedure SizeChanged; virtual;

    Private
      procedure WMEraseBkgnd(var lMsg: TLMEraseBkgnd); Message LM_ERASEBKGND;
      Procedure WMHScroll(Var Msg: TLMHScroll); Message LM_HSCROLL;
      Procedure WMSize(Var Msg: TLMSize); Message LM_SIZE;
      Procedure WMVScroll(Var Msg: TLMVScroll); Message LM_VSCROLL;
      Procedure WMMouseWheel(Var Msg: TCMMouseWheel); Message LM_MOUSEWHEEL;
      procedure PaintPaper;
      function GetPageCount: Integer;

    Public
      constructor Create(AOwner: TComponent); override;
      procedure Paint; override;
      procedure UpdatePreview;
      procedure NextPage;
      procedure PreviousPage;
      procedure FirstPage;
      procedure LastPage;
      procedure Print;
      property PageNumber: Integer read FPageNumber;
      property PageCount: Integer read GetPageCount;

    Published
      property Align default alClient;
      property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
      property Color default clAppWorkspace;
      property Cursor;
      property PageBGColor: TColor read FPageBG write SetPageBG default clWhite;
      property PopupMenu;                                                         // JD 2002-01-9
      property SynEditPrint: TMySEPrint read FSynEditPrint write SetSynEditPrint;
      property ScaleMode: TSynPreviewScale read FScaleMode write SetScaleMode default pscUserScaled;
      property ScalePercent: Integer read FScalePercent write SetScalePercent default 100;
      property Visible default True;
      property ShowScrollHint: Boolean read FShowScrollHint write FShowScrollHint default True;
      property OnClick;
      property OnMouseDown;
      property OnMouseUp;
      property OnPreviewPage: TPreviewPageEvent read FOnPreviewPage write FOnPreviewPage;
      property OnScaleChange: TNotifyEvent read FOnScaleChange write FOnScaleChange; // JD 2002-01-9
  end;

implementation

Uses SynEditStrConst;

Const
  MARGIN_X = 12; // margin width left and right of page
  MARGIN_Y = 12; // margin height above and below page
  SHADOW_SIZE = 2; // page shadow width

{ TSynEditPrintPreview2 }

procedure TSynEditPrintPreview2.SetBorderStyle(Value: TBorderStyle);
begin
 if (Value <> FBorderStyle) then
  Begin
   FBorderStyle := Value;

   RecreateWnd(Self);
  end;

 Inherited SetBorderStyle(Value);
end;

procedure TSynEditPrintPreview2.SetPageBG(Value: TColor);
begin
 If (FPageBG <> Value) Then
  Begin
   FPageBG := Value;

   Invalidate;
  End;
end;

procedure TSynEditPrintPreview2.SetSynEditPrint(Value: TMySEPrint);
begin
 If (FSynEditPrint <> Value) Then
  Begin
   FSynEditPrint := Value;

   If Assigned(FSynEditPrint) Then
    FSynEditPrint.FreeNotification(Self);
  End;
end;

procedure TSynEditPrintPreview2.SetScaleMode(Value: TSynPreviewScale);
begin
 If (FScaleMode <> Value) Then
  Begin
   FScaleMode := Value;
   FScrollPosition := Classes.Point(0, 0);

   SizeChanged;

   If Assigned(FOnScaleChange) Then
    FOnScaleChange(Self);

   Invalidate;
  End;
end;

procedure TSynEditPrintPreview2.SetScalePercent(Value: Integer);
begin
 If (FScalePercent <> Value) Then
  Begin
   FScaleMode := pscUserScaled;
   FScrollPosition := Classes.Point(0, 0);
   FScalePercent := Value;

   SizeChanged;

   Invalidate;
  End
 Else
  ScaleMode := pscUserScaled;

 If Assigned(FOnScaleChange) Then
  FOnScaleChange(Self);
end;

procedure TSynEditPrintPreview2.CreateParams(var Params: TCreateParams);
 Const
      BorderStyles: array[TBorderStyle] of DWord = (0, WS_BORDER);
begin
 Inherited CreateParams(Params);

 With Params Do
  Begin
   Style := Style Or WS_HSCROLL Or WS_VSCROLL Or BorderStyles[FBorderStyle] Or WS_CLIPCHILDREN;

   If NewStyleControls {And Ctl3D} And (FBorderStyle = bsSingle) Then
    Begin
     Style := Style And Not WS_BORDER;
     ExStyle := ExStyle Or WS_EX_CLIENTEDGE;
    End;
  End;
end;

function TSynEditPrintPreview2.GetPageHeightFromWidth(AWidth: Integer): Integer;
begin
 If Assigned(FSynEditPrint) Then
  Begin
   With FSynEditPrint.PrinterInfo Do
    Result := MulDiv(AWidth, PhysicalHeight, PhysicalWidth);
  End
 Else
  Result := MulDiv(AWidth, 141, 100); // fake A4 size
end;

function TSynEditPrintPreview2.GetPageHeight100Percent: Integer;
 Var DC : HDC;
     ScreenDPI: Integer;
begin
 Result := 0;
 DC := GetDC(0);

 ScreenDPI := GetDeviceCaps(DC, LogPixelsY);

 ReleaseDC(0, DC);

 If Assigned(FSynEditPrint) Then
  With FSynEditPrint.PrinterInfo Do
   Result := MulDiv(PhysicalHeight, ScreenDPI, YPixPrInch);
end;

function TSynEditPrintPreview2.GetPageWidthFromHeight(AHeight: Integer): Integer;
begin
 If Assigned(FSynEditPrint) Then
  Begin
   With FSynEditPrint.PrinterInfo Do
    Result := MulDiv(AHeight, PhysicalWidth, PhysicalHeight);
  End
 Else
  Result := MulDiv(AHeight, 100, 141); // fake A4 size
end;

function TSynEditPrintPreview2.GetPageWidth100Percent: Integer;
 Var DC : HDC;
     ScreenDPI: Integer;
begin
 Result := 0;
 DC := GetDC(0);

 ScreenDPI := GetDeviceCaps(DC, LogPixelsX);

 ReleaseDC(0, DC);

 If Assigned(FSynEditPrint) Then
  With FSynEditPrint.PrinterInfo Do
   Result := MulDiv(PhysicalHeight, ScreenDPI, XPixPrInch);
end;

procedure TSynEditPrintPreview2.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
 Inherited Notification(AComponent, Operation);

 If (Operation = opRemove) And (AComponent = FSynEditPrint) Then
  SynEditPrint := Nil;
end;

procedure TSynEditPrintPreview2.ScrollHorzFor(Value: Integer);
begin
 ScrollHorzTo(FScrollPosition.X + Value);
end;

procedure TSynEditPrintPreview2.ScrollHorzTo(Value: Integer);
 Var nW, n: Integer;
begin
 nW := ClientWidth;
 n := nW - FVirtualSize.X;

 If Value < n Then
  Value := n;

 If Value > 0 Then
  Value := 0;

 If (Value <> FScrollPosition.X) Then
  Begin
   n := Value - FScrollPosition.X;
   FScrollPosition.X := Value;

   UpdateScrollbars;

   If (Abs(n) > nW Div 2) Then
    Invalidate
   Else
    Begin
     ScrollWindow(Handle, n, 0, nil, nil);

     Update;
    End;
  End;
end;

procedure TSynEditPrintPreview2.ScrollVertFor(Value: Integer);
begin
 ScrollVertTo(FScrollPosition.Y + Value);
end;

procedure TSynEditPrintPreview2.ScrollVertTo(Value: Integer);
 Var nH, n: Integer;
begin
 nH := ClientHeight;
 n := nH - FVirtualSize.Y;

 If Value < n Then
  Value := n;

 If Value > 0 Then
  Value := 0;

 If (Value <> FScrollPosition.Y) Then
  Begin
   n := Value - FScrollPosition.Y;
   FScrollPosition.Y := Value;

   UpdateScrollbars;

   If (Abs(n) > nH Div 2) Then
    Invalidate
   Else
    Begin
     ScrollWindow(Handle, 0, n, nil, nil);

     Update;
    End;
  End;
end;

procedure TSynEditPrintPreview2.UpdateScrollbars;
 Var si: TScrollInfo;
begin
 FillChar(si, SizeOf(TScrollInfo), 0);

 si.cbSize := SizeOf(TScrollInfo);
 si.fMask := SIF_ALL;

 case FScaleMode of
    pscWholePage: begin
        // hide horizontal scrollbar
        ShowScrollbar(Handle, SB_HORZ, False);
        // show vertical scrollbar, enable if more than one page
        si.fMask := si.fMask or SIF_DISABLENOSCROLL;
        si.nMin := 1;
        if Assigned(FSynEditPrint) then begin
          {$Ifdef Windows}
          si.nMax := FSynEditPrint.PageCount;
          {$else}
          si.nMax := FSynEditPrint.PageCount + 1;
          {$endif}
          si.nPos := FPageNumber;
        end
        else begin
          si.nMax := 1;
          si.nPos := 1;
        end;
        si.nPage := 1;

        {$IFDEF Windows}
        Windows.SetScrollInfo(Handle, SB_VERT, si, True);
        {$Else}
        SetScrollInfo(Handle, SB_VERT, si, True);
        {$endif}
      end;
    pscPageWidth: begin
        // hide horizontal scrollbar
        ShowScrollbar(Handle, SB_HORZ, False);
        // show vertical scrollbar
        si.fMask := si.fMask or SIF_DISABLENOSCROLL;
        si.nMax := FVirtualSize.Y;
        si.nPos := -FScrollPosition.Y;
        si.nPage := ClientHeight;

        {$IFDEF Windows}
        Windows.SetScrollInfo(Handle, SB_VERT, si, True);
        {$Else}
        SetScrollInfo(Handle, SB_VERT, si, True);
        {$endif}
      end;
    pscUserScaled: begin
        ShowScrollbar(Handle, SB_HORZ, True);
        ShowScrollbar(Handle, SB_VERT, True);
        si.fMask := si.fMask or SIF_DISABLENOSCROLL;
        // show horizontal scrollbar
        si.nMax := FVirtualSize.X;
        si.nPos := -FScrollPosition.X;
        si.nPage := ClientWidth;

        {$IFDEF Windows}
        Windows.SetScrollInfo(Handle, SB_VERT, si, True);
        {$Else}
        SetScrollInfo(Handle, SB_VERT, si, True);
        {$endif}

        // show vertical scrollbar
        si.nMax := FVirtualSize.Y;
        si.nPos := -FScrollPosition.Y;
        si.nPage := ClientHeight;

        {$IFDEF Windows}
        Windows.SetScrollInfo(Handle, SB_VERT, si, True);
        {$Else}
        SetScrollInfo(Handle, SB_VERT, si, True);
        {$endif}
      end;
end;
end;

procedure TSynEditPrintPreview2.SizeChanged;
 Var nWDef: Integer;
begin
 If Not (HandleAllocated And Assigned(FSynEditPrint)) Then
  Exit;

 Case fScaleMode Of
  pscWholePage           : Begin
                            FPageSize.X := ClientWidth - 2 * MARGIN_X - SHADOW_SIZE;
                            FPageSize.Y := ClientHeight - 2 * MARGIN_Y - SHADOW_SIZE;
                            nWDef := GetPageWidthFromHeight(FPageSize.Y);

                            If (nWDef < FPageSize.X) Then
                             FPageSize.X := nWDef
                            Else
                             FPageSize.Y := GetPageHeightFromWidth(FPageSize.X);
                           End;
  pscPageWidth           : Begin
                            FPageSize.X := ClientWidth - 2 * MARGIN_X - SHADOW_SIZE;
                            FPageSize.Y := GetPageHeightFromWidth(FPageSize.X);
                           End;
  pscUserScaled          : Begin
                            FPageSize.X := MulDiv(GetPageWidth100Percent, fScalePercent, 100);
                            FPageSize.Y := MulDiv(GetPageHeight100Percent, fScalePercent, 100);
                           End;
 End;

 FVirtualSize.X := FPageSize.X + 2 * MARGIN_X + SHADOW_SIZE;
 FVirtualSize.Y := FPageSize.Y + 2 * MARGIN_Y + SHADOW_SIZE;
 FVirtualOffset.X := MARGIN_X;

 If (FVirtualSize.X < ClientWidth) Then
  Inc(FVirtualOffset.X, (ClientWidth - FVirtualSize.X) div 2);

 FVirtualOffset.Y := MARGIN_Y;

 If (FVirtualSize.Y < ClientHeight) Then
  Inc(FVirtualOffset.Y, (ClientHeight - FVirtualSize.Y) div 2);

 UpdateScrollbars;

 FScrollPosition.X := 0;
 FScrollPosition.Y := 0;
end;

procedure TSynEditPrintPreview2.WMEraseBkgnd(var lMsg: TLMEraseBkgnd);
begin
 lMsg.Result := 1;
end;

procedure TSynEditPrintPreview2.WMHScroll(var Msg: TLMHScroll);
 Var nW : Integer;
begin
 If (FScaleMode <> pscWholePage) Then
  Begin
   nW := ClientWidth;

   Case Msg.ScrollCode Of
        SB_TOP           : ScrollHorzTo(0);
        SB_BOTTOM        : ScrollHorzTo(-FVirtualSize.X);
        SB_LINEDOWN      : ScrollHorzFor(-(nW Div 10));
        SB_LINEUP        : ScrollHorzFor(nW Div 10);
        SB_PAGEDOWN      : ScrollHorzFor(-(nW Div 2));
        SB_PAGEUP        : ScrollHorzFor(nW Div 2);
        SB_THUMBPOSITION,
        SB_THUMBTRACK    : ScrollHorzTo(-Msg.Pos);
   End;
  End;
end;

procedure TSynEditPrintPreview2.WMSize(var Msg: TLMSize);
begin
 inherited;

 If Not (csDesigning in ComponentState) Then
  SizeChanged;
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then begin
    ScrollHintWnd := HintWindowClass.Create(Application);
    ScrollHintWnd.Visible := FALSE;
  end;
  Result := ScrollHintWnd;
end;

procedure TSynEditPrintPreview2.WMVScroll(var Msg: TLMVScroll);
 Var nH: Integer;
     s: String;
     rc: TRect;
     pt, pTmp: TPoint;
     ScrollHint: THintWindow;
begin
 If (FScaleMode = pscWholePage) Then
  Begin
   If Assigned(FSynEditPrint) Then
    case Msg.ScrollCode of
         SB_TOP         : FPageNumber := 1;
         SB_BOTTOM      : FPageNumber := FSynEditPrint.PageCount;

         SB_LINEDOWN,
         SB_PAGEDOWN    : Begin
                           FPageNumber := FPageNumber + 1;

                           If FPageNumber > FSynEditPrint.PageCount Then
                            FPageNumber := FSynEditPrint.PageCount;
                          end;

         SB_LINEUP,
         SB_PAGEUP      : Begin
                           FPageNumber := FPageNumber - 1;

                           If FPageNumber < 1 Then
                            FPageNumber := 1;
                          end;

         SB_THUMBPOSITION,
         SB_THUMBTRACK  : Begin
                           FPageNumber := Msg.Pos;

                           If FShowScrollHint Then
                            Begin
                             ScrollHint := GetScrollHint;

                             If Not ScrollHint.Visible Then
                              Begin
                               ScrollHint.Color := Application.HintColor;
                               ScrollHint.Visible := TRUE;
                              end;

                             s := Format(SYNS_PreviewScrollInfoFmt, [FPageNumber]);
                             rc := ScrollHint.CalcHintRect(200, s, nil);

                             pTmp.x := ClientWidth - rc.Right;
                             pTmp.y := 10;

                             //pt := ClientToScreen(Point(ClientWidth - rc.Right - 4, 10));
                             pt := ClientToScreen(pTmp);

                             OffsetRect(rc, pt.x, pt.y);

                             ScrollHint.ActivateHint(rc, s);

                             SendMessage(ScrollHint.Handle, LM_NCPAINT, 1, 0);

                             ScrollHint.Update;
                            end;
                          end;

         SB_ENDSCROLL   : Begin
                           If FShowScrollHint Then
                            Begin
                             ScrollHint := GetScrollHint;
                             ScrollHint.Visible := False;

                             ShowWindow(ScrollHint.Handle, SW_HIDE);
                            end;
                          end;
    end;

   FScrollPosition.Y := -(FPageNumber - 1);

   UpdateScrollbars;

   If Assigned(FOnPreviewPage) Then
    FOnPreviewPage(Self, FPageNumber);

   Invalidate;
  end
 Else
  Begin
   nH := ClientHeight;

   Case Msg.ScrollCode Of
        SB_TOP: ScrollVertTo(0);
        SB_BOTTOM: ScrollVertTo(-FVirtualSize.Y);
        SB_LINEDOWN: ScrollVertFor(-(nH div 10));
        SB_LINEUP: ScrollVertFor(nH div 10);
        SB_PAGEDOWN: ScrollVertFor(-(nH div 2));
        SB_PAGEUP: ScrollVertFor(nH div 2);
        SB_THUMBPOSITION, SB_THUMBTRACK: ScrollVertTo(-Msg.Pos);
   end;
  end;
end;

procedure TSynEditPrintPreview2.WMMouseWheel(var Msg: TCMMouseWheel);
 Const
      WHEEL_DELTA = 120;

 Var bCtrl: Boolean;

 Procedure MouseWheelUp;
 Begin
  if bCtrl and (fPageNumber > 1) then
   PreviousPage
  else
   ScrollVertFor(WHEEL_DELTA);
 end;

 Procedure MouseWheelDown;
 Begin
  if bCtrl and (fPageNumber < PageCount) then
    NextPage
  else
   ScrollVertFor(-WHEEL_DELTA);
 end;

 Var MousePos: TPoint;
     IsNeg: Boolean;
begin
 bCtrl := GetKeyState(VK_CONTROL) < 0;

 {$IFDEF WINDOWS}
 MousePos:= SmallPointToPoint(WINDOWS.TSmallPoint(Msg.Pos));
 {$ENDIF}
 {$IFDEF LINUX}
 MousePos := SmallPointToPoint(Msg.Pos);
 {$ENDIF}

 Inc(FWheelAccumulator, Msg.WheelDelta);

 While Abs(FWheelAccumulator) >= WHEEL_DELTA Do
  Begin
   IsNeg := FWheelAccumulator < 0;
   FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;

   If IsNeg Then
    Begin
     If FWheelAccumulator <> 0 Then
      FWheelAccumulator := -FWheelAccumulator;

     MouseWheelDown;
    end
   Else
    MouseWheelUp;
  end;
end;

procedure TSynEditPrintPreview2.PaintPaper;
 Var rcClip, rcPaper: TRect;
     i: Integer;
     rgnPaper: HRGN;
begin
 With Canvas Do
  Begin
   rcClip := ClipRect;

   If IsRectEmpty(rcClip) Then
    Exit;

   Brush.Color := Self.Color;
   Brush.Style := bsSolid;
   Pen.Color := clBlack;
   Pen.Width := 1;
   Pen.Style := psSolid;

   If (csDesigning In ComponentState) Or (Not Assigned(FSynEditPrint)) Then
    Begin
     FillRect(rcClip);

     Brush.Color := FPageBG;

     Rectangle(MARGIN_X, MARGIN_Y, MARGIN_X + 30, MARGIN_Y + 43);

     Exit;
    End;

   With rcPaper Do
    Begin
     Left := FVirtualOffset.X + FScrollPosition.X;

     If ScaleMode = pscWholePage Then
      Top := FVirtualOffset.Y
     Else
      Top := FVirtualOffset.Y + FScrollPosition.Y;

     Right := Left + FPageSize.X;
     Bottom := Top + FPageSize.Y;

     rgnPaper := CreateRectRgn(Left, Top, Right + 1, Bottom + 1);
    End;

   If (NULLREGION <> ExtSelectClipRgn(Handle, rgnPaper, RGN_DIFF)) Then
    FillRect(rcClip);

   Brush.Color := clDkGray;

   With rcPaper Do
    Begin
     For i := 1 To SHADOW_SIZE Do
      PolyLine([Classes.Point(Left + i, Bottom + i),
                Classes.Point(Right + i, Bottom + i),
                Classes.Point(Right + i, Top + i)]);
    End;

   SelectClipRgn(Handle, rgnPaper);

   Brush.Color := FPageBG;

   With rcPaper Do
    Begin
     Rectangle(Left, Top, Right + 1, Bottom + 1);

     DeleteObject(rgnPaper);
    End;
  End;
end;

function TSynEditPrintPreview2.GetPageCount: Integer;
begin
 Result := SynEditPrint.PageCount;
end;

constructor TSynEditPrintPreview2.Create(AOwner: TComponent);
begin
 Inherited Create(AOwner);

 ControlStyle := ControlStyle + [csNeedsBorderPaint];
 FBorderStyle := bsSingle;
 FScaleMode := pscUserScaled;
 FScalePercent := 100;
 FPageBG := clWhite;
 Width := 200;
 Height := 120;
 ParentColor := False;
 Color := clAppWorkspace;
 Visible := True;
 FPageNumber := 1;
 FShowScrollHint := True;
 Align := alClient;
 FWheelAccumulator := 0;
end;

procedure TSynEditPrintPreview2.Paint;
 Var ptOrgScreen: TPoint;
begin
 With Canvas Do
  Begin
   PaintPaper;

   if (csDesigning in ComponentState) or (not Assigned(FSynEditPrint)) then
    Exit;

   SetMapMode(Handle, MM_ANISOTROPIC);

   with FSynEditPrint.PrinterInfo do
    Begin
     SetWindowExtEx(Handle, PhysicalWidth, PhysicalHeight, nil);
     SetViewPortExtEx(Handle, FPageSize.X, FPageSize.Y, nil);
     ptOrgScreen.X := MulDiv(LeftGutter, FPageSize.X, PhysicalWidth);
     ptOrgScreen.Y := MulDiv(TopGutter, FPageSize.Y, PhysicalHeight);
     Inc(ptOrgScreen.X, FVirtualOffset.X + FScrollPosition.X);

     if ScaleMode = pscWholePage then
        Inc(ptOrgScreen.Y, FVirtualOffset.Y)
     else
        Inc(ptOrgScreen.Y, FVirtualOffset.Y + FScrollPosition.Y);

     SetViewPortOrgEx(Handle, ptOrgScreen.X, ptOrgScreen.Y, nil);
          // clip the output to the print margins
     IntersectClipRect(Handle, 0, 0, PrintableWidth, PrintableHeight);
    end;
  end;

 FSynEditPrint.PrintToCanvas(Canvas, FPageNumber);
end;

procedure TSynEditPrintPreview2.UpdatePreview;
 Var OldScale: Integer;
     OldMode: TSynPreviewScale;
begin
 OldScale := ScalePercent;
 OldMode := ScaleMode;
 ScalePercent := 100;

 If Assigned(FSynEditPrint) Then
  FSynEditPrint.UpdatePages(Canvas);

 SizeChanged;

 Invalidate;

 ScaleMode := OldMode;

 If ScaleMode = pscUserScaled Then
  ScalePercent := OldScale;

 If Assigned(FOnPreviewPage) Then
  FOnPreviewPage(Self, FPageNumber);
end;

procedure TSynEditPrintPreview2.NextPage;
begin
 FPageNumber := FPageNumber + 1;

 If Assigned(FSynEditPrint) And (FPageNumber > FSynEditPrint.PageCount) Then
  FPageNumber := FSynEditPrint.PageCount;

 If Assigned(FOnPreviewPage) Then
  FOnPreviewPage(Self, FPageNumber);

 Invalidate;
end;

procedure TSynEditPrintPreview2.PreviousPage;
begin
 FPageNumber := FPageNumber - 1;

 If Assigned(FSynEditPrint) And (FPageNumber < 1) Then
  FPageNumber := 1;

 If Assigned(FOnPreviewPage) Then
  FOnPreviewPage(Self, FPageNumber);

 Invalidate;
end;

procedure TSynEditPrintPreview2.FirstPage;
begin
 FPageNumber := 1;

 If Assigned(FOnPreviewPage) Then
  FOnPreviewPage(Self, FPageNumber);

 Invalidate;
end;

procedure TSynEditPrintPreview2.LastPage;
begin
 If Assigned(FSynEditPrint) Then
  FPageNumber := FSynEditPrint.PageCount;

 If Assigned(FOnPreviewPage) Then
  FOnPreviewPage(Self, FPageNumber);

 Invalidate;
end;

procedure TSynEditPrintPreview2.Print;
begin
 If Assigned(FSynEditPrint) Then
  Begin
   FSynEditPrint.Print;

   UpdatePreview;
  End;
end;

end.

