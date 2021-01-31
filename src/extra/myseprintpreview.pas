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
unit MySEPrintPreview;

{$M+}
{$I MySynEdit.inc}

interface

uses
  Classes, SysUtils, Controls, LMessages, Graphics, Forms, MySEPrint,
  LCLType{$IFdef MSWindows}, Windows{$ENDIF}, Dialogs, LCLintf;

Type
    TPreviewPageEvent = Procedure(Sender: TObject; PageNumber: Integer) of object;
    TSynPreviewScale = (pscWholePage, pscPageWidth, pscUserScaled);

    { TMySEPrintPreview }

    TMySEPrintPreview                 = Class(TCustomControl)
     Protected
      FBorderStyle                    : TBorderStyle;
      FSynEditPrint                   : TMySEPrint;
      FScaleMode                      : TSynPreviewScale;
      FScalePercent                   : Integer;
      FVirtualSize                    : TPoint;
      FVirtualOffset                  : TPoint;
      FPageSize                       : TPoint;
      FScrollPosition                 : TPoint;
      FPageBG                         : TColor;
      FPageNumber                     : Integer;
      FShowScrollHint                 : Boolean;
      FOnPreviewPage                  : TPreviewPageEvent;
      FOnScaleChange                  : TNotifyEvent;
      FWheelAccumulator               : Integer;

      Procedure SetBorderStyle(Value: TBorderStyle); Override;
      Procedure SetPageBG(Value: TColor);
      Procedure SetSynEditPrint(Value: TMySEPrint);
      Procedure SetScaleMode(Value: TSynPreviewScale);
      Procedure SetScalePercent(Value: Integer);

     Private
      Procedure WMEraseBkgnd(Var Msg: TLMEraseBkgnd); Message LM_ERASEBKGND;
      Procedure WMHScroll(Var Msg: TLMHScroll); Message LM_HSCROLL;
      Procedure WMSize(Var Msg: TLMSize); Message LM_SIZE;
      Procedure WMVScroll(Var Msg: TLMVScroll); Message LM_VSCROLL;
      Procedure WMMouseWheel(Var Msg: TCMMouseWheel); Message LM_MOUSEWHEEL;

      Procedure PaintPaper;
      Function GetPageCount: Integer;

     Protected
      Procedure CreateParams(Var Params: TCreateParams); Override;

      Function GetPageHeightFromWidth(AWidth: Integer): Integer;
      Function GetPageHeight100Percent: Integer;
      Function GetPageWidthFromHeight(AHeight: Integer): Integer;
      Function GetPageWidth100Percent: Integer;
      Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      Procedure ScrollHorzFor(Value: Integer);
      Procedure ScrollHorzTo(Value: Integer); virtual;
      Procedure ScrollVertFor(Value: Integer);
      Procedure ScrollVertTo(Value: Integer); virtual;
      Procedure UpdateScrollbars; virtual;
      Procedure SizeChanged; virtual;

     Public
      Constructor Create(AOwner: TComponent); override;
      Procedure Paint; override;
      Procedure UpdatePreview;
      Procedure NextPage;
      Procedure PreviousPage;
      Procedure FirstPage;
      Procedure LastPage;
      Procedure Print;

      Property PageNumber: Integer read FPageNumber;
      Property PageCount: Integer read GetPageCount;

     Published
      property Align default alClient;
      property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
      property Color default clAppWorkspace;
      property Cursor;
      property PageBGColor: TColor read FPageBG write SetPageBG default clWhite;
      property PopupMenu;
      property SynEditPrint: TMySEPrint read FSynEditPrint write SetSynEditPrint;
      property ScaleMode: TSynPreviewScale read FScaleMode write SetScaleMode default pscUserScaled;
      property ScalePercent: Integer read FScalePercent write SetScalePercent default 100;
      property Visible default True;
      property ShowScrollHint: Boolean read FShowScrollHint write FShowScrollHint default True;
      property OnClick;
      property OnMouseDown;
      property OnMouseUp;
      property OnPreviewPage: TPreviewPageEvent read FOnPreviewPage write FOnPreviewPage;
      property OnScaleChange: TNotifyEvent read FOnScaleChange write FOnScaleChange;
    End;

implementation

Uses SynEditStrConst;

Const
     MARGIN_X                 = 12;
     MARGIN_Y                 = 12;
     SHADOW_SIZE              = 2;

{ TMySEPrintPreview }

procedure TMySEPrintPreview.SetBorderStyle(Value: TBorderStyle);
begin
 If (Value <> FBorderStyle) Then
  Begin
   FBorderStyle := Value;

   RecreateWnd(Self);
  End;

 Inherited SetBorderStyle(Value);
end;

procedure TMySEPrintPreview.SetPageBG(Value: TColor);
begin
 If (FPageBG <> Value) Then
  Begin
   FPageBG := Value;

   Invalidate;
  End;
end;

procedure TMySEPrintPreview.SetSynEditPrint(Value: TMySEPrint);
begin
 If (FSynEditPrint <> Value) Then
  Begin
   FSynEditPrint := Value;

   If Assigned(FSynEditPrint) Then
    FSynEditPrint.FreeNotification(Self);
  End;
end;

procedure TMySEPrintPreview.SetScaleMode(Value: TSynPreviewScale);
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

procedure TMySEPrintPreview.SetScalePercent(Value: Integer);
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

procedure TMySEPrintPreview.WMEraseBkgnd(var Msg: TLMEraseBkgnd);
begin
 Msg.Result := 1;
end;

procedure TMySEPrintPreview.WMHScroll(var Msg: TLMHScroll);
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

procedure TMySEPrintPreview.WMSize(var Msg: TLMSize);
begin
 Inherited;

 If Not (csDesigning In ComponentState) Then
  SizeChanged;
end;

Var
   ScrollHintWnd     : THintWindow;

Function GetScrollHint: THintWindow;
Begin
 If ScrollHintWnd = Nil Then
  Begin
   ScrollHintWnd := HintWindowClass.Create(Application);
   ScrollHintWnd.Visible := False;
  End;

 Result := ScrollHintWnd;
End;

procedure TMySEPrintPreview.WMVScroll(var Msg: TLMVScroll);
 Var nH: Integer;
     s: ShortString;
     rc: TRect;
     pt: TPoint;
     ScrollHint: THintWindow;
begin
 If (FScaleMode = pscWholePage) Then
  Begin
   If Assigned(FSynEditPrint) Then
    Case Msg.ScrollCode Of
         SB_TOP               : FPageNumber := 1;
         SB_BOTTOM            : FPageNumber := FSynEditPrint.PageCount;
         SB_LINEDOWN,
         SB_PAGEDOWN          : Begin
                                 FPageNumber := FPageNumber + 1;

                                 If FPageNumber > FSynEditPrint.PageCount Then
                                  FPageNumber := FSynEditPrint.PageCount;
                                End;
         SB_LINEUP,
         SB_PAGEUP            : Begin
                                 FPageNumber := FPageNumber - 1;

                                 If FPageNumber < 1 Then
                                  FPageNumber := 1;
                                End;
         SB_THUMBPOSITION,
         SB_THUMBTRACK        : Begin
                                 FPageNumber := Msg.Pos;

                                 If FShowScrollHint Then
                                  Begin
                                   ScrollHint := GetScrollHint;

                                   If Not ScrollHint.Visible Then
                                    Begin
                                     ScrollHint.Color := Application.HintColor;
                                     ScrollHint.Visible := True;
                                    End;

                                   s := Format(SYNS_PreviewScrollInfoFmt, [FPageNumber]);
                                   rc := ScrollHint.CalcHintRect(200, s, Nil);
                                   pt := ClientToScreen(Classes.Point(ClientWidth - rc.Right - 4, 10));

                                   {$IFdef MSWindows}
                                   OffsetRect(rc, pt.x, pt.y);
                                   {$ELSE}
                                   LCLIntf.OffsetRect(rc, pt.x, pt.y);
                                   {$ENDIF}

                                   ScrollHint.ActivateHint(rc, s);

                                   ScrollHint.Invalidate;

                                   ScrollHint.Update;
                                  End;
                                End;
         SB_ENDSCROLL         : Begin
                                 If FShowScrollHint Then
                                  Begin
                                   ScrollHint := GetScrollHint;
                                   ScrollHint.Visible := False;

                                   ScrollHint.Invalidate;
                                  End;
                                End;
    End;

   FScrollPosition.Y := -(FPageNumber - 1);

   UpdateScrollbars;

   If Assigned(FOnPreviewPage) Then
    FOnPreviewPage(Self, FPageNumber);

   Invalidate;
  End
 Else
  Begin
   nH := ClientHeight;

   Case Msg.ScrollCode Of
        SB_TOP           : ScrollVertTo(0);
        SB_BOTTOM        : ScrollVertTo(-FVirtualSize.Y);
        SB_LINEDOWN      : ScrollVertFor(-(nH div 10));
        SB_LINEUP        : ScrollVertFor(nH div 10);
        SB_PAGEDOWN      : ScrollVertFor(-(nH div 2));
        SB_PAGEUP        : ScrollVertFor(nH div 2);
        SB_THUMBPOSITION,
        SB_THUMBTRACK    : ScrollVertTo(-Msg.Pos);
   End;
  End;
end;

procedure TMySEPrintPreview.WMMouseWheel(var Msg: TCMMouseWheel);
 Const
      WHEEL_DELTA = 120;

 Procedure MouseWheelUp;
 Begin
  ScrollVertFor(WHEEL_DELTA);
 End;

 Procedure MouseWheelDown;
 Begin
  ScrollVertFor(-WHEEL_DELTA);
 End;

 Var MousePos : TPoint;
     IsNeg: Boolean;
begin
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
    End
   Else
    MouseWheelUp;
  End;
end;

procedure TMySEPrintPreview.PaintPaper;
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

function TMySEPrintPreview.GetPageCount: Integer;
begin
 Result := SynEditPrint.PageCount;
end;

procedure TMySEPrintPreview.CreateParams(var Params: TCreateParams);
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

function TMySEPrintPreview.GetPageHeightFromWidth(AWidth: Integer): Integer;
begin
 If Assigned(FSynEditPrint) Then
  Begin
   With FSynEditPrint.PrinterInfo Do
    Result := MulDiv(AWidth, PhysicalHeight, PhysicalWidth);
  End
 Else
  Result := MulDiv(AWidth, 141, 100); // fake A4 size
end;

function TMySEPrintPreview.GetPageHeight100Percent: Integer;
 Var ScreenDPI : Integer;
begin
 Result := 0;
 ScreenDPI := Screen.PixelsPerInch;

 If Assigned(FSynEditPrint) Then
  With FSynEditPrint.PrinterInfo Do
   Result := MulDiv(PhysicalHeight, ScreenDPI, YPixPrInch);
end;

function TMySEPrintPreview.GetPageWidthFromHeight(AHeight: Integer): Integer;
begin
 If Assigned(FSynEditPrint) Then
  Begin
   With FSynEditPrint.PrinterInfo Do
    Result := MulDiv(AHeight, PhysicalWidth, PhysicalHeight);
  End
 Else
  Result := MulDiv(AHeight, 100, 141); // fake A4 size
end;

function TMySEPrintPreview.GetPageWidth100Percent: Integer;
 Var ScreenDPI: Integer;
begin
 Result := 0;
 ScreenDPI := Screen.PixelsPerInch;

 If Assigned(FSynEditPrint) Then
  With FSynEditPrint.PrinterInfo Do
   Result := MulDiv(PhysicalWidth, ScreenDPI, XPixPrInch);
end;

procedure TMySEPrintPreview.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
 Inherited Notification(AComponent, Operation);

 If (Operation = opRemove) And (AComponent = FSynEditPrint) Then
  SynEditPrint := Nil;
end;

procedure TMySEPrintPreview.ScrollHorzFor(Value: Integer);
begin
 ScrollHorzTo(FScrollPosition.X + Value);
end;

procedure TMySEPrintPreview.ScrollHorzTo(Value: Integer);
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

procedure TMySEPrintPreview.ScrollVertFor(Value: Integer);
begin
 ScrollVertTo(FScrollPosition.Y + Value);
end;

procedure TMySEPrintPreview.ScrollVertTo(Value: Integer);
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

procedure TMySEPrintPreview.UpdateScrollbars;
 Var si: TScrollInfo;
begin
 FillChar(si, SizeOf(TScrollInfo), 0);

 si.cbSize := SizeOf(TScrollInfo);
 si.fMask := SIF_ALL;

 Case FScaleMode Of
  pscWholePage                    : Begin
                                     ShowScrollbar(Handle, SB_HORZ, False);

                                     si.fMask := si.fMask Or SIF_DISABLENOSCROLL;
                                     si.nMin := 1;

                                     If Assigned(FSynEditPrint) Then
                                      Begin
                                       si.nMax := FSynEditPrint.PageCount;
                                       si.nPos := FPageNumber;
                                      End
                                     Else
                                      Begin
                                       si.nMax := 1;
                                       si.nPos := 1;
                                      End;

                                     si.nPage := 1;

                                     {$IFdef MSWindows}
                                     Windows.SetScrollInfo(Handle, SB_VERT, si, True);
                                     {$ELSE}
                                     SetScrollInfo(Handle, SB_VERT, si, True);
                                     {$ENDIF}
                                    End;
  pscPageWidth                    : Begin
                                     ShowScrollbar(Handle, SB_HORZ, False);

                                     si.fMask := si.fMask Or SIF_DISABLENOSCROLL;
                                     si.nMax := FVirtualSize.Y;
                                     si.nPos := -FScrollPosition.Y;
                                     si.nPage := ClientHeight;

                                     {$IFdef MSWindows}
                                     Windows.SetScrollInfo(Handle, SB_VERT, si, True);
                                     {$ELSE}
                                     SetScrollInfo(Handle, SB_VERT, si, True);
                                     {$ENDIF}
                                    End;
  pscUserScaled                   : Begin
                                     ShowScrollbar(Handle, SB_HORZ, True);
                                     ShowScrollbar(Handle, SB_VERT, True);

                                     si.fMask := si.fMask or SIF_DISABLENOSCROLL;
                                     si.nMax := FVirtualSize.X;
                                     si.nPos := -FScrollPosition.X;
                                     si.nPage := ClientWidth;

                                     {$IFdef MSWindows}
                                     Windows.SetScrollInfo(Handle, SB_HORZ, si, True);
                                     {$ELSE}
                                      SetScrollInfo(Handle, SB_HORZ, si, True);
                                     {$ENDIF}

                                     si.nMax := FVirtualSize.Y;
                                     si.nPos := -FScrollPosition.Y;
                                     si.nPage := ClientHeight;

                                     {$IFdef MSWindows}
                                     Windows.SetScrollInfo(Handle, SB_VERT, si, True);
                                     {$ELSE}
                                     SetScrollInfo(Handle, SB_VERT, si, True);
                                     {$ENDIF}
                                    End;
 End;
end;

procedure TMySEPrintPreview.SizeChanged;
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

constructor TMySEPrintPreview.Create(AOwner: TComponent);
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

procedure TMySEPrintPreview.Paint;
 Var ptOrgScreen: TPoint;
begin
 With Canvas Do
  Begin
   PaintPaper;

   If (csDesigning In ComponentState) Or (Not Assigned(FSynEditPrint)) Then
    Exit;

   {$IFdef MSWindows}
   SetMapMode(Handle, MM_ANISOTROPIC);
   {$ENDIF}

   With FSynEditPrint.PrinterInfo Do
    Begin
     {$IFdef MSWindows}
     SetWindowExtEx(Handle, PhysicalWidth, PhysicalHeight, nil);
     SetViewPortExtEx(Handle, FPageSize.X, FPageSize.Y, nil);
     {$ENDIF}

     ptOrgScreen.X := MulDiv(LeftGutter, FPageSize.X, PhysicalWidth);
     ptOrgScreen.Y := MulDiv(TopGutter, FPageSize.Y, PhysicalHeight);

     Inc(ptOrgScreen.X, FVirtualOffset.X + FScrollPosition.X);

     If ScaleMode = pscWholePage Then
      Inc(ptOrgScreen.Y, FVirtualOffset.Y)
     Else
      Inc(ptOrgScreen.Y, FVirtualOffset.Y + FScrollPosition.Y);

     {$IFdef MSWindows}
     SetViewPortOrgEx(Handle, ptOrgScreen.X, ptOrgScreen.Y, nil);
     {$ENDIF}

     IntersectClipRect(Handle, 0, 0, PrintableWidth, PrintableHeight);
    End;

   FSynEditPrint.PrintToCanvas(Canvas, FPageNumber);
  End;
end;

procedure TMySEPrintPreview.UpdatePreview;
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

procedure TMySEPrintPreview.NextPage;
begin
 FPageNumber := FPageNumber + 1;

 If Assigned(FSynEditPrint) And (FPageNumber > FSynEditPrint.PageCount) Then
  FPageNumber := FSynEditPrint.PageCount;

 If Assigned(FOnPreviewPage) Then
  FOnPreviewPage(Self, FPageNumber);

 Invalidate;
end;

procedure TMySEPrintPreview.PreviousPage;
begin
 FPageNumber := FPageNumber - 1;

 If Assigned(FSynEditPrint) And (FPageNumber < 1) Then
  FPageNumber := 1;

 If Assigned(FOnPreviewPage) Then
  FOnPreviewPage(Self, FPageNumber);

 Invalidate;
end;

procedure TMySEPrintPreview.FirstPage;
begin
 FPageNumber := 1;

 If Assigned(FOnPreviewPage) Then
  FOnPreviewPage(Self, FPageNumber);

 Invalidate;
end;

procedure TMySEPrintPreview.LastPage;
begin
 If Assigned(FSynEditPrint) Then
  FPageNumber := FSynEditPrint.PageCount;

 If Assigned(FOnPreviewPage) Then
  FOnPreviewPage(Self, FPageNumber);

 Invalidate;
end;

procedure TMySEPrintPreview.Print;
begin
 If Assigned(FSynEditPrint) Then
  Begin
   FSynEditPrint.Print;

   UpdatePreview;
  End;
end;

end.

