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
unit MySEPrint;

{$M+}
{$I MySynEdit.inc}

interface

uses
  Classes, SysUtils, Printers, Graphics, SynEdit, SynEditTypes, LCLintf, LCLType,
  MySEPrinterType, MySEPrinterHeaderFooter, MySEPrinterInfo, MySEPrinterMargins,
  SynEditMiscProcs, SynEditHighlighter{$IFdef MSWindows}, Windows{$ENDIF};

Const
     TSynTabChar                       = #9;

Type

    TConvertTabsProc = function(const Line: AnsiString; TabWidth: integer): AnsiString;

    { TPageLine }

    TPageLine                          = Class
     Public
      FirstLine                        : Integer;
    End;

    TBufferCoord                       = Record
     Char                              : integer;
     Line                              : integer;
    End;

    { TMySEPrint }

    TMySEPrint                         = Class(TComponent)
     Private
      FCopies                          : Integer;
      FFooter                          : TFooter;
      FHeader                          : THeader;
      FLines                           : TStrings;
      FMargins                         : TMySEPrintMargins;
      FPageCount                       : Integer;
      FFont                            : TFont;
      FTitle                           : String;
      FDocTitle                        : String;
      FPrinterInfo                     : TMySEPrinterInfo;
      FPages                           : TList;
      FCanvas                          : TCanvas;

      FCharWidth                       : Integer;
      FMaxLeftChar                     : Integer;
      FETODist                         : PIntArray;
      FWrap                            : Boolean;
      FOnPrintLine                     : TPrintLineEvent;
      FOnPrintStatus                   : TPrintStatusEvent;
      FYPos                            : Integer;
      FLineHeight                      : Integer;
      FHighlight                       : Boolean;
      FColors                          : Boolean;
      FHighlighter                     : TSynCustomHighlighter;
      FOldFont                         : TFont;
      FSynOK                           : Boolean;
      FLineNumbers                     : Boolean;
      FLineNumber                      : Integer;
      FLineOffset                      : Integer;
      FAbort                           : Boolean;
      FPrinting                        : Boolean;
      FDefaultBG                       : TColor;
      FPageOffset                      : Integer;
      FRangesOK                        : Boolean;
      FMaxWidth                        : Integer;
      FMaxCol                          : Integer;
      FPagesCounted                    : Boolean;
      FLineNumbersInMargin             : Boolean;
      FTabWidth                        : Integer;
      fFontColor                       : TColor;
      fSelectedOnly                    : Boolean;
      fSelAvail                        : Boolean;
      fSelMode                         : TSynSelectionMode;
      fBlockBegin                      : TBufferCoord;
      fBlockEnd                        : TBufferCoord;

      procedure CalcPages;
      procedure SetLines(const Value: TStrings);
      procedure SetFont(const Value: TFont);
      procedure SetCharWidth(const Value: Integer);
      procedure SetMaxLeftChar(const Value: Integer);
      procedure PrintPage(Num: Integer);
      procedure WriteLine(Text: string);
      procedure WriteLineNumber;
      procedure HandleWrap(Text: string; MaxWidth: Integer);
      procedure TextOut(Text: string; AList: TList);
      procedure SetHighlighter(const Value: TSynCustomHighlighter);
      procedure RestoreCurrentFont;
      procedure SaveCurrentFont;
      procedure SetPixelsPrInch;
      procedure InitPrint;
      procedure InitRanges;
      function GetPageCount: Integer;
      procedure SetSynEdit(const Value: TSynEdit);
      procedure SetFooter(const Value: TFooter);
      procedure SetHeader(const Value: THeader);
      procedure SetMargins(const Value: TMySEPrintMargins);
      function ClipLineToRect(S: string; R: TRect): string;

     Protected
      property MaxLeftChar: Integer read FMaxLeftChar write SetMaxLeftChar;
      property CharWidth: Integer read FCharWidth write SetCharWidth;

      procedure PrintStatus(Status: TSynPrintStatus; PageNumber: integer; var Abort: boolean); virtual;

      procedure PrintLine(LineNumber, PageNumber: Integer); virtual;

     Public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure UpdatePages(ACanvas: TCanvas);
      procedure PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
      procedure Print;
      procedure PrintRange(StartPage, EndPage: Integer);
      property PrinterInfo: TMySEPrinterInfo read FPrinterInfo;
      property PageCount: Integer read GetPageCount;
      property SynEdit: TSynEdit write SetSynEdit;

      procedure LoadFromStream(AStream: TStream);
      procedure SaveToStream(AStream: TStream);

     Published
      property Copies: integer read FCopies write FCopies;
      property Header: THeader read FHeader write SetHeader;
      property Footer: TFooter read FFooter write SetFooter;
      property Margins: TMySEPrintMargins read FMargins write SetMargins;
      property Lines: TStrings read FLines write SetLines;
      property Font: TFont read FFont write SetFont;
      property Title: string read FTitle write FTitle;
      property DocTitle: string read FDocTitle write FDocTitle;                   //JJV  2000-10-13
      property Wrap: Boolean read FWrap write FWrap default True;
      property Highlight: Boolean read FHighlight write FHighlight default True;
      property SelectedOnly: Boolean read FSelectedOnly write FSelectedOnly       // jj 2001-07-23
      default False;
      property Colors: Boolean read FColors write FColors default False;
      property LineNumbers: Boolean read FLineNumbers write FLineNumbers
      default False;
      property LineOffset: Integer read FLineOffset write FLineOffset default 0;
      property PageOffset: Integer read FPageOffset write FPageOffset default 0;
      property OnPrintLine: TPrintLineEvent read FOnPrintLine write FOnPrintLine;
      property OnPrintStatus: TPrintStatusEvent read FOnPrintStatus
      write FOnPrintStatus;
      property Highlighter: TSynCustomHighlighter read FHighlighter
      write SetHighlighter;
      property LineNumbersInMargin: Boolean read FLineNumbersInMargin
      write FLineNumbersInMargin default False;
      property TabWidth: integer read fTabWidth write fTabWidth;                  // djlp 2000-09-19
      property Color: TColor read fDefaultBG write fDefaultBG;                    // djlp 2000-09-19
    End;

implementation

function IsPowerOfTwo(TabWidth: integer): boolean;
 Var nW: integer;
begin
 nW := 2;

 Repeat
  If (nW >= TabWidth) Then
   Break;

  Inc(nW, nW);
 Until (nW >= $10000);  // we don't want 64 kByte spaces...

 Result := (nW = TabWidth);
end;

Function GetHasTabs(pLine: PChar; var CharsBefore: integer): boolean;
begin
 CharsBefore := 0;

 If Assigned(pLine) Then
  Begin
   While (pLine^ <> #0) Do
    Begin
     If (pLine^ = #9) Then
      Break;

     Inc(CharsBefore);
     Inc(pLine);
    End;

   Result := (pLine^ = #9);
  End
 Else
  Result := FALSE;
end;

function ConvertTabs1Ex(const Line: AnsiString; TabWidth: integer;
                        var HasTabs: boolean): AnsiString;
 Var pDest: PChar;
     nBeforeTab: integer;
begin
  Result := Line;  // increment reference count only

  If GetHasTabs(pointer(Line), nBeforeTab) Then
   Begin
    HasTabs := TRUE;
    pDest := @Result[nBeforeTab + 1]; // this will make a copy of Line
    // We have at least one tab in the string, and the tab width is 1.
    // pDest points to the first tab char. We overwrite all tabs with spaces.
    repeat
      if (pDest^ = #9) then pDest^ := ' ';
      Inc(pDest);
    until (pDest^ = #0);
  End
 Else
  HasTabs := FALSE;
end;

Function ConvertTabs1(const Line: AnsiString; TabWidth: integer): AnsiString;
 Var HasTabs: boolean;
begin
 Result := ConvertTabs1Ex(Line, TabWidth, HasTabs);
end;

function ConvertTabs2nEx(const Line: AnsiString; TabWidth: integer;
  var HasTabs: boolean): AnsiString;
var
  i, DestLen, TabCount, TabMask: integer;
  pSrc, pDest: PChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then begin
    HasTabs := TRUE;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width equals 2^n.
    // pSrc points to the first tab char in Line. We get the number of tabs
    // and the length of the expanded string now.
    TabCount := 0;
    TabMask := (TabWidth - 1) xor $7FFFFFFF;
    repeat
      if (pSrc^ = #9) then begin
        DestLen := (DestLen + TabWidth) and TabMask;
        Inc(TabCount);
      end else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    // We use another TabMask here to get the difference to 2^n.
    TabMask := TabWidth - 1;
    repeat
      if (pSrc^ = #9) then begin
        i := TabWidth - (DestLen and TabMask);
        Inc(DestLen, i);
        //This is used for both drawing and other stuff and is meant to be #9 and not #32
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if (TabCount = 0) then begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end else begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end else
    HasTabs := FALSE;
end;

function ConvertTabs2n(const Line: AnsiString; TabWidth: integer): AnsiString;
 Var HasTabs: boolean;
begin
 Result := ConvertTabs2nEx(Line, TabWidth, HasTabs);
end;

function ConvertTabsEx(const Line: AnsiString; TabWidth: integer;
  var HasTabs: boolean): AnsiString;
var
  i, DestLen, TabCount: integer;
  pSrc, pDest: PChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then begin
    HasTabs := TRUE;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width is greater
    // than 1. pSrc points to the first tab char in Line. We get the number
    // of tabs and the length of the expanded string now.
    TabCount := 0;
    repeat
      if (pSrc^ = #9) then begin
        DestLen := DestLen + TabWidth - DestLen mod TabWidth;
        Inc(TabCount);
      end else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    repeat
      if (pSrc^ = #9) then begin
        i := TabWidth - (DestLen mod TabWidth);
        Inc(DestLen, i);
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if (TabCount = 0) then begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end else begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end else
    HasTabs := FALSE;
end;

function ConvertTabs(const Line: AnsiString; TabWidth: integer): AnsiString;
 Var HasTabs: boolean;
begin
 Result := ConvertTabsEx(Line, TabWidth, HasTabs);
end;

Function GetBestConvertTabsProc(TabWidth: integer): TConvertTabsProc;
begin
 If (TabWidth < 2) Then
  Result := TConvertTabsProc(@ConvertTabs1)
 Else
  If IsPowerOfTwo(TabWidth) Then
   Result := TConvertTabsProc(@ConvertTabs2n)
  Else
   Result := TConvertTabsProc(@ConvertTabs);
end;

{ TMySEPrint }

procedure TMySEPrint.CalcPages;
 Var AStr, Text: string;
     StrWidth, i: Integer;
     j: Integer;
     AList: TList;
     YPos: Integer;
     PageLine: TPageLine;
     iStartLine, iEndLine: integer;
     iSelStart, iSelLen: integer;

 Procedure CountWrapped;
  Var j: Integer;
 Begin
  For j := 0 To AList.Count - 1 Do
   YPos := YPos + FLineHeight;
 End;

begin
 InitRanges;

 For i := 0 To FPages.Count - 1 Do
  TPageLine(FPages[i]).Free;

 FPages.Clear;

 FMaxWidth := FMargins.PRight - FMargins.PLeft;
 AStr := '';
 FMaxCol := 0;

 While FCanvas.TextWidth(AStr) < FMaxWidth Do
  Begin
   AStr := AStr + 'W';
   FMaxCol := FMaxCol + 1;
  End;

 FMaxCol := FMaxCol - 1;
 AStr := StringOfChar('W', FMaxCol);
 FMaxWidth := FCanvas.TextWidth(AStr);

 FPageCount := 1;
 PageLine := TPageLine.Create;
 PageLine.FirstLine := 0;

 FPages.Add(PageLine);

 YPos := FMargins.PTop;

 If SelectedOnly Then
  Begin
   iStartLine := fBlockBegin.Line -1;
   iEndLine := fBlockEnd.Line -1;
  End
 Else
  Begin
   iStartLine := 0;
   iEndLine := Lines.Count -1;
  End;

 For i := iStartLine To iEndLine Do
  Begin
   If (Not fSelectedOnly Or (fSelMode = smLine)) Then
    Text := Lines[i]
   Else
    Begin
     If (fSelMode = smColumn) Or (i = fBlockBegin.Line -1) Then
      iSelStart := fBlockBegin.Char
     Else
      iSelStart := 1;

     If (fSelMode = smColumn) Or (i = fBlockEnd.Line -1) Then
      iSelLen := fBlockEnd.Char  - iSelStart
     Else
      iSelLen := MaxInt;

     Text := Copy(Lines[i], iSelStart, iSelLen);
    End;

   If (YPos + FLineHeight > FMargins.PBottom) Then
    Begin
     YPos := FMargins.PTop;
     FPageCount := FPageCount + 1;
     PageLine := TPageLine.Create;
     PageLine.FirstLine := i;
     FPages.Add(PageLine);
    End;

   StrWidth := FCanvas.TextWidth(Text);

   If Wrap And (StrWidth > FMaxWidth) Then
    Begin
     AList := TList.Create;

     If WrapTextEx(Text, [' ', '-', #9, ','], FMaxCol, AList) Then
      CountWrapped
     Else
      Begin
       If WrapTextEx(Text, [';', ')', '.'], FMaxCol, AList) Then
        CountWrapped
       Else
        Begin
         While Length(Text) > 0 Do
          Begin
           AStr := Copy(Text, 1, FMaxCol);

           Delete(Text, 1, FMaxCol);

           If Length(Text) > 0 Then
            YPos := YPos + FLineHeight;
          End;
        End;
      End;

     For j := 0 To AList.Count - 1 Do
      TWrapPos(AList[j]).Free;

     AList.Free;
    End;

   YPos := YPos + FLineHeight;
  End;

 FPagesCounted := True;
end;

procedure TMySEPrint.SetLines(const Value: TStrings);
 Var i,j: integer;
     ConvertTabsProc: TConvertTabsProc;
     TmpString: String;
begin
 ConvertTabsProc := GetBestConvertTabsProc(FTabWidth);

 With FLines Do
  Begin
   BeginUpdate;

   Try
     Clear;

     For i := 0 To Value.Count - 1 Do
      Begin
       TmpString := ConvertTabsProc(Value[i], FTabWidth);
       j := Pos(TSynTabChar, TmpString);

       While j > 0 Do
        Begin
         TmpString[j] := ' ';

         j := Pos(TSynTabChar, TmpString);
        End;

       Add(TmpString);
      End;
   Finally
     EndUpdate;
   End;
  End;

 FRangesOK := False;
 FPagesCounted := False;
end;

procedure TMySEPrint.SetFont(const Value: TFont);
begin
 FFont.Assign(Value);
 FPagesCounted := False;
end;

procedure TMySEPrint.SetCharWidth(const Value: Integer);
 Var i : Integer;
begin
 If FCharWidth <> Value Then
  Begin
   FCharWidth := Value;

   For i := 0 To FMaxLeftChar - 1 Do
{$IFOPT R+}{$DEFINE SYN_RESET_RANGE_CHECK}{$R-}{$ENDIF}
    FETODist^[i] := FCharWidth;
{$IFDEF SYN_RESET_RANGE_CHECK}{$R+}{$UNDEF SYN_RESET_RANGE_CHECK}{$ENDIF}
  End;
end;

procedure TMySEPrint.SetMaxLeftChar(const Value: Integer);
 Var i : Integer;
begin
 If FMaxLeftChar <> Value Then
  Begin
   FMaxLeftChar := Value;

   ReallocMem(FETODist, FMaxLeftChar * SizeOf(Integer));

   For i := 0 To FMaxLeftChar - 1 Do
{$IFOPT R+}{$DEFINE SYN_RESET_RANGE_CHECK}{$R-}{$ENDIF}
    FETODist^[i] := FCharWidth;
{$IFDEF SYN_RESET_RANGE_CHECK}{$R+}{$UNDEF SYN_RESET_RANGE_CHECK}{$ENDIF}
  End;
end;

procedure TMySEPrint.PrintPage(Num: Integer);
 Var i, iEnd: Integer;
     iSelStart, iSelLen: integer;
begin
 PrintStatus(psNewPage, Num, FAbort);

 If Not FAbort Then
  Begin
   FCanvas.Brush.Color := Color;

   With FMargins Do
    FCanvas.FillRect(Classes.Rect(PLeft, PTop, PRight, PBottom));

   FMargins.InitPage(FCanvas, Num, FPrinterInfo, FLineNumbers,
                     FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);

   FHeader.Print(FCanvas, Num + FPageOffset);

   If FPages.Count > 0 Then
    Begin
     FYPos := FMargins.PTop;

     If Num = FPageCount Then
      iEnd := FLines.Count - 1
     Else
      iEnd := TPageLine(FPages[Num]).FirstLine - 1;

     For i := TPageLine(FPages[Num - 1]).FirstLine To iEnd Do
      Begin
       FLineNumber := i + 1;

       If (Not fSelectedOnly Or ((i >= fBlockBegin.Line - 1) And (i <= fBlockEnd.Line - 1))) Then
        Begin
         If (Not fSelectedOnly Or (fSelMode = smLine)) Then
          WriteLine(Lines[i])
         Else
          Begin
           If (fSelMode = smColumn) Or (i = fBlockBegin.Line -1) Then
            iSelStart := fBlockBegin.Char
           Else
            iSelStart := 1;

           If (fSelMode = smColumn) Or (i = fBlockEnd.Line -1) Then
            iSelLen := fBlockEnd.Char  - iSelStart
           Else
            iSelLen := MaxInt;

           WriteLine(Copy(Lines[i], iSelStart, iSelLen));
          End;

         PrintLine(i + 1, Num);
        End;
      End;
    End;

   FFooter.Print(FCanvas, Num + FPageOffset);
  End;
end;

procedure TMySEPrint.WriteLine(Text: string);
 Var StrWidth: integer;
begin
 If FLineNumbers Then
  WriteLineNumber;

 StrWidth := FCanvas.TextWidth(Text);

 If Wrap And (StrWidth > FMaxWidth) Then
  HandleWrap(Text, FMaxWidth)
 Else
  TextOut(Text, Nil);

 FYPos := FYPos + FLineHeight;
end;

procedure TMySEPrint.WriteLineNumber;
 Var AStr : String;
begin
 SaveCurrentFont;

 AStr := IntToStr(FLineNumber + FLineOffset) + ': ';

 FCanvas.Brush.Color := FDefaultBG;
 FCanvas.Font.Style := [];
 FCanvas.Font.Color := clBlack;
 FCanvas.TextOut(FMargins.PLeft - FCanvas.TextWidth(AStr), FYPos, AStr);

 RestoreCurrentFont;
end;

procedure TMySEPrint.HandleWrap(Text: string; MaxWidth: Integer);
 Var AStr: string;
     AList: TList;
     j: Integer;

 Procedure WrapPrimitive;
  Var i: Integer;
      WrapPos: TWrapPos;
 Begin
  i := 1;

  While i <= Length(Text) Do
   Begin
    AStr := '';

    While (Length(AStr) < FMaxCol) And (i <= Length(Text)) Do
     Begin
      AStr := AStr + Text[i];

      i := i + 1;
     End;

    WrapPos := TWrapPos.Create;

    WrapPos.Index := i - 1;

    AList.Add(WrapPos);

    If (Length(AStr) - i) <= FMaxCol Then
     Break;
   End;
 End;

begin
 AStr := '';
 AList := TList.Create;

 If WrapTextEx(Text, [' ', '-', #9, ','], FMaxCol, AList) Then
  TextOut(Text, AList)
 Else
  Begin
   If WrapTextEx(Text, [';', ')', '.'], FMaxCol, AList) Then
    TextOut(Text, AList)
   Else
    Begin
     WrapPrimitive;

     TextOut(Text, AList);
    End;
  End;

 For j := 0 To AList.Count - 1 Do
  TWrapPos(Alist[j]).Free;

 AList.Free;
end;

procedure TMySEPrint.TextOut(Text: string; AList: TList);
 Var Token: string;
     TokenPos: Integer;
     Attr: TSynHighlighterAttributes;
     AColor: TColor;
     TokenStart: Integer;
     LCount: Integer;
     Handled: Boolean;
     aStr: string;
     i, WrapPos, OldWrapPos: Integer;
     lLines: TStringList;
     ClipRect: TRect;

 Procedure ClippedTextOut(X, Y: Integer; Text: string);
 Begin
  Text := ClipLineToRect(Text, ClipRect);

  FCanvas.TextOut(X, Y, Text);
 End;

 Procedure SplitToken;
  Var AStr: string;
      Last: Integer;
      FirstPos: Integer;
      TokenEnd: Integer;
 Begin
  Last := TokenPos;
  FirstPos := TokenPos;
  TokenEnd := TokenPos + Length(Token);

  While (LCount < AList.Count) And (TokenEnd > TWrapPos(AList[LCount]).Index) Do
   Begin
    AStr := Copy(Text, Last + 1, TWrapPos(AList[LCount]).Index - Last);
    Last := TWrapPos(AList[LCount]).Index;

    ClippedTextOut(FMargins.PLeft + FirstPos * FCharWidth, FYPos, AStr);

    FirstPos := 0;
    LCount := LCount + 1;
    FYPos := FYPos + FLineHeight;
   End;

  AStr := Copy(Text, Last + 1, TokenEnd - Last);

  ClippedTextOut(FMargins.PLeft + FirstPos * FCharWidth, FYPos, AStr);

  TokenStart := TokenPos + Length(Token) - Length(AStr);
 End;

begin
 With FMargins Do
  ClipRect := Classes.Rect(PLeft, PTop, PRight, PBottom);

 If FSynOK Then
  Begin
   SaveCurrentFont;

   If FLineNumber = 1 Then
    FHighlighter.ResetRange
   Else
    FHighlighter.SetRange(FLines.Objects[FLineNumber - 2]);

   FHighlighter.SetLine(Text, FLineNumber);

   Token := '';
   TokenStart := 0;
   LCount := 0;

   While Not FHighLighter.GetEol Do
    Begin
     Token := FHighLighter.GetToken;
     TokenPos := FHighLighter.GetTokenPos;
     Attr := FHighLighter.GetTokenAttribute;

     If Assigned(Attr) Then
      Begin
       FCanvas.Font.Style := Attr.Style;

       If FColors Then
        Begin
         AColor := Attr.Foreground;

         If AColor = clNone Then
          AColor := FFont.Color;

         FCanvas.Font.Color := AColor;
         AColor := Attr.Background;

         If AColor = clNone Then
          AColor := FDefaultBG;

         FCanvas.Brush.Color := AColor;
        End
       Else
        Begin
         FCanvas.Font.Color := fFontColor;
         FCanvas.Brush.Color := FDefaultBG;
        End;
      End
     Else
      Begin
       FCanvas.Font.Color := fFontColor;
       FCanvas.Brush.Color := FDefaultBG;
      End;

     Handled := False;

     If Assigned(AList) Then
      Begin
       If (LCount < AList.Count) Then
        Begin
         If (TokenPos >= TWrapPos(AList[LCount]).Index) Then
          Begin
           LCount := LCount + 1;
           TokenStart := TokenPos;
           FYPos := FYPos + FLineHeight;
          End
         Else
          Begin
           If (TokenPos + Length(Token) > TWrapPos(AList[LCount]).Index) Then
            Begin
             Handled := True;

             SplitToken;
            End;
          End;
        End;
      End;

     If Not Handled Then
      ClippedTextOut(FMargins.PLeft + (TokenPos - TokenStart) * FCharWidth, FYPos, Token);

     FHighLighter.Next;
    End;

   RestoreCurrentFont;
  End
 Else
  Begin
   lLines := TStringList.Create;

   Try
     OldWrapPos := 0;

     If Assigned(AList) Then
      For i := 0 To AList.Count - 1 Do
       Begin
        WrapPos := TWrapPos(AList[i]).Index;

        If i = 0 Then
         AStr := Copy(Text, 1, WrapPos)
        Else
         AStr := Copy(Text, OldWrapPos + 1, WrapPos - OldWrapPos);

        lLines.Add(AStr);

        OldWrapPos := WrapPos;
       End;

     If Length(Text) > 0 Then
      lLines.Add(Copy(Text, OldWrapPos + 1, MaxInt));

     For i := 0 To lLines.Count - 1 Do
      Begin
       ClippedTextOut(FMargins.PLeft, FYPos, lLines[i]);

       If i < lLines.Count - 1 Then
        FYPos := FYPos + FLineHeight;
      End;
   Finally
     lLines.Free;
   End;
  End;
end;

procedure TMySEPrint.SetHighlighter(const Value: TSynCustomHighlighter);
begin
 FHighlighter := Value;
 FRangesOK := False;
 FPagesCounted := False;
end;

procedure TMySEPrint.RestoreCurrentFont;
begin
 FCanvas.Font.Assign(FOldFont);
end;

procedure TMySEPrint.SaveCurrentFont;
begin
 FOldFont.Assign(FCanvas.Font);
end;

procedure TMySEPrint.SetPixelsPrInch;
 Var TmpSize: Integer;
begin
 FHeader.SetPixPrInch(FPrinterInfo.YPixPrInch);
 FFooter.SetPixPrInch(FPrinterInfo.YPixPrInch);

 TmpSize := FFont.Size;

 FFont.PixelsPerInch := FPrinterInfo.YPixPrInch;

 FFont.Size := TmpSize;
end;

procedure TMySEPrint.InitPrint;
 Var TmpSize: Integer;
begin
 fFontColor := FFont.Color;

 FCanvas.Font.Assign(FFont);

 If Not FPrinting Then
  Begin
   SetPixelsPrInch;

   TmpSize := FCanvas.Font.Size;
   FCanvas.Font.PixelsPerInch := FFont.PixelsPerInch;
   FCanvas.Font.Size := TmpSize;
  End;

 FCanvas.Font.Style := [fsBold, fsItalic, fsUnderline, fsStrikeOut];
 CharWidth := FCanvas.TextWidth('W');
 FLineHeight := FCanvas.TextHeight('Wp¹');
 FCanvas.Font.Style := FFont.Style;

 FMargins.InitPage(FCanvas, 1, FPrinterInfo, FLineNumbers, FLineNumbersInMargin,
                   FLines.Count - 1 + FLineOffset);

 CalcPages;

 FHeader.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
 FFooter.InitPrint(FCanvas, FPageCount, FTitle, FMargins);

 FSynOK := Highlight And Assigned(FHighLighter) And (FLines.Count > 0);
end;

procedure TMySEPrint.InitRanges;
 Var I : Integer;
begin
 If (Not FRangesOK) And Assigned(FHighlighter) And (Lines.Count > 0) Then
  Begin
   FHighlighter.ResetRange;

   For i :=  0 To Lines.Count -1 Do
    Begin
     FHighlighter.SetLine(FLines[I], I);

     FHighlighter.NextToEol;

     FLines.Objects[I] := TObject(FHighlighter.GetRange);
    End;

   FRangesOK := True;
  End;
end;

function TMySEPrint.GetPageCount: Integer;
 Var TmpCanvas: TCanvas;
     {$IFdef MSWindows}
     DC: HDC;
     {$ELSE}
     DC : LCLType.HDC;
     {$ENDIF}
begin
 Result := 0;

 If FPagesCounted Then
  Result := FPageCount
 Else
  Begin
   TmpCanvas := TCanvas.Create;

   Try
     {$IFdef MSWindows}
     DC := GetDC(0);
     {$ELSE}
     DC := LCLIntf.GetDC(0);
     {$ENDIF}

     Try
       If DC <> 0 Then
        Begin
         TmpCanvas.Handle := DC;

         UpdatePages(TmpCanvas);

         TmpCanvas.Handle := 0;
         Result := FPageCount;
         FPagesCounted := True;
        End;
     Finally
       {$IFdef MSWindows}
       ReleaseDC(0, DC);
       {$ELSE}
       LCLIntf.ReleaseDC(0, DC);
       {$ENDIF}
     End;
   Finally
     TmpCanvas.Free;
   End;
  End;
end;

procedure TMySEPrint.SetSynEdit(const Value: TSynEdit);
begin
 HighLighter := Value.Highlighter;
 Font := Value.Font;
 FTabWidth := Value.TabWidth;
 Lines := Value.Lines;
 fSelAvail := Value.SelAvail;

 fBlockBegin.Char := Value.BlockBegin.x;
 fBlockBegin.Line := Value.BlockBegin.y;

 fBlockEnd.Char := Value.BlockEnd.x;
 fBlockEnd.Line := Value.BlockEnd.y;

 fSelMode := Value.SelectionMode;
end;

procedure TMySEPrint.SetFooter(const Value: TFooter);
begin
 FFooter.Assign(Value);
end;

procedure TMySEPrint.SetHeader(const Value: THeader);
begin
 FHeader.Assign(Value);
end;

procedure TMySEPrint.SetMargins(const Value: TMySEPrintMargins);
begin
 FMargins.Assign(Value);
end;

function TMySEPrint.ClipLineToRect(S: string; R: TRect): string;
begin
 While FCanvas.TextWidth(S) > FMaxWidth Do
  SetLength(S, Length(S) - 1);

 Result := S;
end;

procedure TMySEPrint.PrintStatus(Status: TSynPrintStatus; PageNumber: integer;
  var Abort: boolean);
begin
 Abort := False;

 If Assigned(FOnPrintStatus) Then
  FOnPrintStatus(Self, Status, PageNumber, Abort);

 If Abort Then
  If FPrinting Then
   Printer.Abort;
end;

procedure TMySEPrint.PrintLine(LineNumber, PageNumber: Integer);
begin
 If Assigned(FOnPrintLine) Then
  FOnPrintLine(Self, LineNumber, PageNumber);
end;

constructor TMySEPrint.Create(AOwner: TComponent);
begin
 Inherited Create(AOwner);

 FCopies := 1;
 FFooter := TFooter.Create;
 FHeader := THeader.Create;
 FLines := TStringList.Create;
 FMargins := TMySEPrintMargins.Create;
 FPrinterInfo := TMySEPrinterInfo.Create;
 FFont := TFont.Create;
 FOldFont := TFont.Create;
 FETODist := AllocMem(1);
 MaxLeftChar := 1024;
 FWrap := True;
 FHighlight := True;
 FColors := False;
 FLineNumbers := False;
 FLineOffset := 0;
 FPageOffset := 0;
 FLineNumbersInMargin := False;
 FPages := TList.Create;
 FTabWidth := 8;
 FDefaultBG := clWhite;
end;

destructor TMySEPrint.Destroy;
 Var i : Integer;
begin
 FFooter.Free;
 FHeader.Free;
 FLines.Free;
 FMargins.Free;
 FPrinterInfo.Free;
 FFont.Free;
 FOldFont.Free;

 For i := 0 To FPages.Count - 1 Do
  TPageLine(FPages[i]).Free;

 FPages.Free;
 FreeMem(FETODist);

 Inherited Destroy;
end;

procedure TMySEPrint.UpdatePages(ACanvas: TCanvas);
begin
 FCanvas := ACanvas;

 FPrinterInfo.UpdatePrinter;

 InitPrint;
end;

procedure TMySEPrint.PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
begin
 FAbort := False;
 FPrinting := False;
 FCanvas := ACanvas;

 PrintPage(PageNumber);
end;

procedure TMySEPrint.Print;
begin
 PrintRange(1, -1);
end;

procedure TMySEPrint.PrintRange(StartPage, EndPage: Integer);
 Var i, ii : Integer;
begin
 If fSelectedOnly And Not fSelAvail Then
  Exit;

 FPrinting := True;
 FAbort := False;

 If FDocTitle <> '' Then
  Printer.Title := FDocTitle
 Else
  Printer.Title := FTitle;

 Printer.BeginDoc;

 PrintStatus(psBegin, StartPage, FAbort);

 UpdatePages(Printer.Canvas);

 For ii:=1 To Copies Do
  Begin
   i := StartPage;

   If EndPage < 0 Then
    EndPage := FPageCount;

   While (i <= EndPage) And (Not FAbort) Do
    Begin
     PrintPage(i);

     If ((i < EndPage) Or (ii < Copies)) And Not(FAbort) Then
      Printer.NewPage;

     i := i + 1;
    End;
  End;

 If Not(FAbort) Then
  PrintStatus(psEnd, EndPage, FAbort);

 Printer.EndDoc;

 FPrinting := False;
end;

procedure TMySEPrint.LoadFromStream(AStream: TStream);
 Var bufSize: Integer;
     buffer: PChar;
begin
 FHeader.LoadFromStream(AStream);
 FFooter.LoadFromStream(AStream);
 FMargins.LoadFromStream(AStream);

 With AStream Do
  Begin
   Read(bufSize, SizeOf(bufSize));
   GetMem(buffer, bufSize+1);

   Try
     Read(buffer^, bufSize);
     buffer[bufSize] := #0;
     FTitle := buffer;
   Finally
     FreeMem(buffer);
   End;

   Read(bufSize, SizeOf(bufSize));
   GetMem(buffer, bufSize+1);

   Try
     Read(buffer^, bufSize);
     buffer[bufSize] := #0;
     FDocTitle := buffer;
   Finally
     FreeMem(buffer);
   End;

   Read(FWrap, SizeOf(FWrap));
   Read(FHighlight, SizeOf(FHighlight));
   Read(FColors, SizeOf(FColors));
   Read(FLineNumbers, SizeOf(FLineNumbers));
   Read(FLineOffset, SizeOf(FLineOffset));
   Read(FPageOffset, SizeOf(FPageOffset));
  End;
end;

procedure TMySEPrint.SaveToStream(AStream: TStream);
 Var aLen: Integer;
begin
 FHeader.SaveToStream(AStream);
 FFooter.SaveToStream(AStream);
 FMargins.SaveToStream(AStream);

 With AStream Do
  Begin
   aLen := Length(FTitle);

   Write(aLen, SizeOf(aLen));
   Write(PChar(FTitle)^, aLen);

   aLen := Length(FDocTitle);

   Write(aLen, SizeOf(aLen));
   Write(PChar(FDocTitle)^, aLen);
   Write(FWrap, SizeOf(FWrap));
   Write(FHighlight, SizeOf(FHighlight));
   Write(FColors, SizeOf(FColors));
   Write(FLineNumbers, SizeOf(FLineNumbers));
   Write(FLineOffset, SizeOf(FLineOffset));
   Write(FPageOffset, SizeOf(FPageOffset));
  End;
end;

end.

