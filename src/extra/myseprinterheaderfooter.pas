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
unit MySEPrinterHeaderFooter;
{$M+}

{$I MySynEdit.inc}

interface

uses
  Classes, SysUtils, Graphics, MySEPrinterType, MySEPrinterMargins;

Type

    { THeaderFooterItem }

    THeaderFooterItem          = Class
     Private
      FText                    : String;
      FFont                    : TFont;
      FLineNumber              : Integer;
      FAlignment               : TAlignment;
      FIndex                   : Integer;

      function GetAsString: string;
      procedure SetAsString(const Value: string);
      procedure SetFont(const Value: TFont);

     Public
      constructor Create;
      destructor Destroy; override;

      function GetText(NumPages, PageNum: Integer; Roman: Boolean;
               Title, ATime, ADate: string): string;
      procedure LoadFromStream(AStream: TStream);
      procedure SaveToStream(AStream: TStream);

      property Alignment: TAlignment read FAlignment write FAlignment;
      property AsString: string read GetAsString write SetAsString;               //gp 2000-06-24
      property Font: TFont read FFont write SetFont;
      property LineNumber: Integer read FLineNumber write FLineNumber;
      property Text: string read FText write FText;
    End;

    THeaderFooterType            = (hftHeader, hftFooter);

    TLineInfo                    = Class
     Public
      LineHeight                 : Integer;
      MaxBaseDist                : Integer;
    End;

    THeaderFooter                = Class(TPersistent)
     Private
      FType                      : THeaderFooterType;
      FFrameTypes                : TFrameTypes;
      FShadedColor               : TColor;
      FLineColor                 : TColor;
      FItems                     : TList;
      FDefaultFont               : TFont;
      FDate, FTime               : String;
      FNumPages                  : Integer;
      FTitle                     : String;
      FMargins                   : TMySEPrintMargins;
      FFrameHeight               : Integer;
      FOldPen                    : TPen;
      FOldBrush                  : TBrush;
      FOldFont                   : TFont;
      FRomanNumbers              : Boolean;
      FLineInfo                  : TList;
      FLineCount                 : Integer;
      FMirrorPosition            : Boolean;

      procedure SetDefaultFont(const Value: TFont);
      procedure DrawFrame(ACanvas: TCanvas);
      procedure CalcHeight(ACanvas: TCanvas);
      procedure SaveFontPenBrush(ACanvas: TCanvas);
      procedure RestoreFontPenBrush(ACanvas: TCanvas);

      function GetAsString: string;
      procedure SetAsString(const Value: string);

     Public
      constructor Create;
      destructor Destroy; override;

      function Add(Text: string; Font: TFont; Alignment: TAlignment;
               LineNumber: Integer): Integer;
      procedure Delete(Index: Integer);
      procedure Clear;
      function Count: Integer;
      function Get(Index: Integer): THeaderFooterItem;
      procedure SetPixPrInch(Value: Integer);
      procedure InitPrint(ACanvas: TCanvas; NumPages: Integer; Title: string;
                Margins: TMySEPrintMargins);
      procedure Print(ACanvas: TCanvas; PageNum: Integer);
      procedure Assign(Source: TPersistent); override;
      procedure FixLines;

      property AsString: string read GetAsString write SetAsString;               //gp 2000-06-24

      procedure LoadFromStream(AStream: TStream);
      procedure SaveToStream(AStream: TStream);

     Published
      property FrameTypes: TFrameTypes read FFrameTypes write FFrameTypes default [ftLine];
      property ShadedColor: TColor read FShadedColor write FShadedColor default clSilver;
      property LineColor: TColor read FLineColor write FLineColor default clBlack;
      property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
      property RomanNumbers: Boolean read FRomanNumbers write FRomanNumbers default False;
      property MirrorPosition: Boolean read FMirrorPosition write FMirrorPosition default False;
    End;

    THeader                    = class(THeaderFooter)
     Public
      constructor Create;
    end;

    { TFooter }

    TFooter                    = class(THeaderFooter)
     Public
      constructor Create;
    End;

implementation

Uses SynEditMiscProcs;

function GetFirstEl(var value: string; delim: char): string;
var
  p: integer;
begin
  p := Pos(delim, value);
  if p = 0 then
    p := Length(value) + 1;
  Result := Copy(value, 1, p - 1);
  Delete(value, 1, p);
end;

Function FontStyle_To_Byte(Val : TFontStyles) : Byte;
Begin
 Result := 0;

 If Integer(fsBold In Val) > 0 Then
  Inc(Result, 1);

 If Integer(fsItalic In Val) > 0 Then
  Inc(Result, 2);

 If Integer(fsStrikeOut In Val) > 0 Then
  Inc(Result, 4);

 If Integer(fsUnderline In Val) > 0 Then
  Inc(Result, 8);
End;

Function Byte_To_FontStyle(Val : Byte) : TFontStyles;
Begin
 Result := [];

 If Val = 0 Then
  Exit;

 If Val >= 8 Then
  Begin
   Result := Result + [fsUnderline];

   Dec(Val, 8);
  End;

 If Val >= 4 Then
  Begin
   Result := Result + [fsStrikeOut];

   Dec(Val, 4);
  End;

 If Val >= 2 Then
  Begin
   Result := Result + [fsItalic];

   Dec(Val, 2);
  End;

 If Val >= 1 Then
  Begin
   Result := Result + [fsBold];

   Dec(Val, 1);
  End;
End;

function EncodeString(s: String): String;
 Var I, J : Integer;
Begin
 SetLength(Result, 2 * Length(s)); // worst case
  j := 0;
  for i := 1 to Length(s) do
  begin
    Inc(j);
    if s[i] = '\' then
    begin
      Result[j] := '\';
      Result[j + 1] := '\';
      Inc(j);
    end
    else if s[i] = '/' then
    begin
      Result[j] := '\';
      Result[j + 1] := '.';
      Inc(j);
    end
    else
      Result[j] := s[i];
  end; //for
  SetLength(Result, j);
end;

function DecodeString(s: String): String;
 Var I, J : Integer;
Begin
 SetLength(Result, Length(s)); // worst case
  j := 0;
  i := 1;
  while i <= Length(s) do
  begin
    Inc(j);
    if s[i] = '\' then
    begin
      Inc(i);
      if s[i] = '\' then
        Result[j] := '\'
      else
        Result[j] := '/';
    end
    else
      Result[j] := s[i];
    Inc(i);
  end; //for
  SetLength(Result,j);
end;

{ THeaderFooterItem }

function THeaderFooterItem.GetAsString: string;
begin
 Result := EncodeString(FText) + '/' +
           IntToStr(FFont.CharSet) + '/' +
           IntToStr(FFont.Color) + '/' +
           IntToStr(FFont.Height) + '/' +
           EncodeString(FFont.Name) + '/' +
           IntToStr(Ord(FFont.Pitch)) + '/' +
           IntToStr(FFont.PixelsPerInch) + '/' +
           IntToStr(FFont.Size) + '/' +
           IntToStr(FontStyle_To_Byte(FFont.Style)) + '/' +
           IntToStr(FLineNumber) + '/' +
           IntToStr(Ord(FAlignment));
end;

procedure THeaderFooterItem.SetAsString(const Value: string);
 Var S : String;
     //Sty : TFontStyles;
     Sty : Byte;
begin
 S := Value;

 FText := DecodeString(GetFirstEl(s, '/'));

 FFont.Charset := StrToIntDef(GetFirstEl(s, '/'), 0);
 FFont.Color := StrToIntDef(GetFirstEl(s, '/'), 0);
 FFont.Height := StrToIntDef(GetFirstEl(s, '/'), 0);
 FFont.Name := DecodeString(GetFirstEl(s, '/'));
 FFont.Pitch := TFontPitch(StrToIntDef(GetFirstEl(s, '/'), 0));
 FFont.PixelsPerInch := StrToIntDef(GetFirstEl(s, '/'), 0);
 FFont.Size := StrToIntDef(GetFirstEl(s, '/'), 0);
 Sty := StrToIntDef(GetFirstEl(s, '/'), 0);
 FFont.Style := Byte_To_FontStyle(Sty);
 FLineNumber := StrToIntDef(GetFirstEl(s, '/'), 0);
 FAlignment := TAlignment(StrToIntDef(GetFirstEl(s, '/'), 0));
end;

procedure THeaderFooterItem.SetFont(const Value: TFont);
begin
 FFont.Assign(Value);
end;

constructor THeaderFooterItem.Create;
begin
 Inherited;

 FFont := TFont.Create;
end;

destructor THeaderFooterItem.Destroy;
begin
 Inherited Destroy;

 FFont.Free;
end;

function THeaderFooterItem.GetText(NumPages, PageNum: Integer; Roman: Boolean;
  Title, ATime, ADate: string): string;
 Var Len, Start, Run : Integer;
     AStr : String;

 Procedure DoAppend(AText : String);
 Begin
  Result := Result + AText;
 End;

 Procedure TryAppend(Var First : Integer; After : Integer);
 Begin
  If After > First Then
   Begin
    DoAppend(Copy(AStr, First, After - First));

    First := After;
   End;
 End;

 Function TryExecuteMacro : Boolean;
  Var Macro : String;
 Begin
  Result := True;

  Macro := UpperCase(Copy(FText, Start, Run - Start + 1));

  If Macro = '$PAGENUM$' Then
   Begin
    If Roman Then
     DoAppend(IntToRoman(PageNum))
    Else
     DoAppend(IntToStr(PageNum));

    Exit;
   End;

  If Macro = '$PAGECOUNT$' Then
   Begin
    If Roman Then
     DoAppend(IntToRoman(NumPages))
    Else
     DoAppend(IntToStr(NumPages));

    Exit;
   End;

  If Macro = '$TITLE$' Then
   Begin
    DoAppend(Title);

    Exit;
   End;

  If Macro = '$DATE$' Then
   Begin
    DoAppend(ADate);

    Exit;
   End;

  If Macro = '$TIME$' Then
   Begin
    DoAppend(ATime);

    Exit;
   End;

  If Macro = '$DATETIME$' Then
   Begin
    DoAppend(ADate + ' ' + ATime);

    Exit;
   End;

  If Macro = '$TIMEDATE$' Then
   Begin
    DoAppend(ATime + ' ' + ADate);

    Exit;
   End;

  Result := False;
 End;

begin
 Result := '';
 AStr := FText;

 If Trim(AStr) = '' Then
  Exit;

 Len := Length(AStr);

 If Len > 0 Then
  Begin
   Start := 1;
   Run := 1;

   While Run <= Len Do
    Begin
     If AStr[Run] = '$' Then
      Begin
       TryAppend(Start, Run);

       Inc(Run);

       While Run <= Len Do
        Begin
         If AStr[Run] = '$' Then
          Begin
           If TryExecuteMacro Then
            Begin
             Inc(Run);

             Start := Run;

             Break;
            End
           Else
            Begin
             TryAppend(Start, Run);

             Inc(Run);
            End;
          End
         Else
          Inc(Run);
        End;
      End
     Else
      Inc(Run);
    End;

   TryAppend(Start, Run);
  End;
end;

procedure THeaderFooterItem.LoadFromStream(AStream: TStream);
 Var aCharset: TFontCharset;
     aColor: TColor;
     aHeight: Integer;
     aName: TFontName;
     aPitch: TFontPitch;
     aSize: Integer;
     aStyle: TFontStyles;
     bufSize : integer;
     buffer : PChar;
begin
 With AStream Do
  Begin
   Read(bufSize, SizeOf(bufSize));
   GetMem(buffer, bufSize + 1);

   Try
     Read(buffer^, bufSize);

     buffer[bufSize] := #0;
     FText := buffer;
   Finally
     FreeMem(buffer);
   End;

   Read(FLineNumber, SizeOf(FLineNumber));

   Read(aCharset, SizeOf(aCharset));
   Read(aColor, SizeOf(aColor));
   Read(aHeight, SizeOf(aHeight));
   Read(bufSize, SizeOf(bufSize));
   GetMem(buffer, bufSize+1);

   Try
     Read(buffer^, bufSize);

     buffer[bufSize] := #0;
     aName := buffer;
   Finally
     FreeMem(buffer);
   End;

   Read(aPitch, SizeOf(aPitch));
   Read(aSize, SizeOf(aSize));
   Read(aStyle, SizeOf(aStyle));

   FFont.Charset := aCharset;

   FFont.Color   := aColor;
   FFont.Height  := aHeight;
   FFont.Name    := aName;
   FFont.Pitch   := aPitch;
   FFont.Size    := aSize;
   FFont.Style   := aStyle;

   Read(FAlignment, SizeOf(FAlignment));
  End;
end;

procedure THeaderFooterItem.SaveToStream(AStream: TStream);
 Var aCharset: TFontCharset;
     aColor: TColor;
     aHeight: Integer;
     aName: TFontName;
     aPitch: TFontPitch;
     aSize: Integer;
     aStyle: TFontStyles;
     aLen : integer;
begin
 With AStream Do
  Begin
   aLen := Length(FText);

   Write(aLen, SizeOf(aLen));
   Write(PChar(FText)^, aLen);
   Write(FLineNumber, SizeOf(FLineNumber));

   aCharset := FFont.Charset;
   aColor   := FFont.Color;
   aHeight  := FFont.Height;
   aName    := FFont.Name;
   aPitch   := FFont.Pitch;
   aSize    := FFont.Size;
   aStyle   := FFont.Style;

   Write(aCharset, SizeOf(aCharset));
   Write(aColor, SizeOf(aColor));
   Write(aHeight, SizeOf(aHeight));

   aLen := Length(aName);

   Write(aLen, SizeOf(aLen));
   Write(PChar(aName)^, aLen);
   Write(aPitch, SizeOf(aPitch));
   Write(aSize, SizeOf(aSize));
   Write(aStyle, SizeOf(aStyle));
   Write(FAlignment, SizeOf(FAlignment));
  End;
end;

{ THeaderFooter }

procedure THeaderFooter.SetDefaultFont(const Value: TFont);
begin
 FDefaultFont.Assign(Value);
end;

procedure THeaderFooter.DrawFrame(ACanvas: TCanvas);
begin
 If (FrameTypes = []) Then
  Exit;

 With ACanvas, FMargins Do
  Begin
   Pen.Color := LineColor;
   Brush.Color := ShadedColor;

   If ftShaded In FrameTypes Then
    Brush.Style := bsSolid
   Else
    Brush.Style := bsClear;

   If ftBox In FrameTypes Then
    Pen.Style := psSolid
   Else
    Pen.Style := psClear;

   If FrameTypes * [ftBox, ftShaded] <> [] Then
    Begin
     If FType = hftHeader Then
      Rectangle(PLeft, PHeader - FFrameHeight, PRight, PHeader)
     Else
      Rectangle(PLeft, PFooter, PRight, PFooter + FFrameHeight);
    End;

   If ftLine In FrameTypes Then
    Begin
     Pen.Style := psSolid;

     If FType = hftHeader Then
      Begin
       MoveTo(PLeft, PHeader);

       LineTo(PRight, PHeader);
      End
     Else
      Begin
       MoveTo(PLeft, PFooter);

       LineTo(PRight, PFooter);
      End;
    End;
  End;
end;

procedure THeaderFooter.CalcHeight(ACanvas: TCanvas);
 Var i, CurLine: Integer;
     AItem: THeaderFooterItem;
     FOrgHeight: Integer;
begin
 FFrameHeight := -1;

 If FItems.Count <= 0 Then
  Exit;

 CurLine := 1;
 FFrameHeight := 0;
 FOrgHeight := FFrameHeight;

 For i := 0 To FItems.Count - 1 Do
  Begin
   AItem := THeaderFooterItem(FItems[i]);

   If AItem.LineNumber <> CurLine Then
    Begin
     CurLine := AItem.LineNumber;
     FOrgHeight := FFrameHeight;
    End;

   ACanvas.Font.Assign(AItem.Font);

   FFrameHeight := Max(FFrameHeight, FOrgHeight + ACanvas.TextHeight('W'));
  End;

 FFrameHeight := FFrameHeight + 2 * FMargins.PHFInternalMargin;
end;

procedure THeaderFooter.SaveFontPenBrush(ACanvas: TCanvas);
begin
 FOldFont.Assign(ACanvas.Font);
 FOldPen.Assign(ACanvas.Pen);
 FOldBrush.Assign(ACanvas.Brush);
end;

procedure THeaderFooter.RestoreFontPenBrush(ACanvas: TCanvas);
begin
 ACanvas.Font.Assign(FOldFont);
 ACanvas.Pen.Assign(FOldPen);
 ACanvas.Brush.Assign(FOldBrush);
end;

function THeaderFooter.GetAsString: string;
 Var I : Integer;
begin
 FixLines;

 Result := '';

 For I := 0 To FItems.Count - 1 Do
  Begin
   If Result <> '' Then
    Result := Result + '/';

   Result := Result + EncodeString(THeaderFooterItem(FItems[i]).AsString);
  End;
end;

procedure THeaderFooter.SetAsString(const Value: string);
 Var item: THeaderFooterItem;
     S : String;
begin
 Clear;

 item := THeaderFooterItem.Create;

 Try
   S := Value;

   While S <> '' Do
    Begin
     item.AsString := DecodeString(GetFirstEl(s, '/'));

     Add(item.Text, item.Font, item.Alignment, item.LineNumber);
    End;
 Finally
    item.Free;
 End;
end;

constructor THeaderFooter.Create;
begin
 Inherited;

 FFrameTypes := [ftLine];
 FShadedColor := clSilver;
 FLineColor := clBlack;
 FItems := TList.Create;
 FDefaultFont := TFont.Create;
 FOldPen := TPen.Create;
 FOldBrush := TBrush.Create;
 FOldFont := TFont.Create;
 FRomanNumbers := False;
 FMirrorPosition := False;
 FLineInfo := TList.Create;

 With FDefaultFont Do
  Begin
   Name := 'Arial';
   Size := 10;
   Color := clBlack;
  End;
end;

destructor THeaderFooter.Destroy;
 Var I : Integer;
begin
 Clear;

 FItems.Free;
 FDefaultFont.Free;
 FOldPen.Free;
 FOldBrush.Free;
 FOldFont.Free;

 For I := 0 To FLineInfo.Count - 1 Do
  TLineInfo(FLineInfo[I]).Free;

 FLineInfo.Free;

 Inherited Destroy;
end;

function THeaderFooter.Add(Text: string; Font: TFont; Alignment: TAlignment;
  LineNumber: Integer): Integer;
 Var AItem: THeaderFooterItem;
begin
 AItem := THeaderFooterItem.Create;

 If Font = Nil Then
  AItem.Font := FDefaultFont
 Else
  AItem.Font := Font;

 AItem.Alignment := Alignment;
 AItem.LineNumber := LineNumber;
 AItem.FIndex := FItems.Add(AItem);
 AItem.Text := Text;

 Result := AItem.FIndex;
end;

procedure THeaderFooter.Delete(Index: Integer);
 Var I : Integer;
begin
 For I := 0 To FItems.Count - 1 Do
  If THeaderFooterItem(FItems[I]).FIndex = Index Then
   Begin
    FItems.Delete(I);

    Break;
   End;
end;

procedure THeaderFooter.Clear;
 Var I : Integer;
begin
 For I := 0 To FItems.Count - 1 Do
  THeaderFooterItem(FItems[I]).Free;

 FItems.Clear;
end;

function THeaderFooter.Count: Integer;
begin
 Result := FItems.Count;
end;

function THeaderFooter.Get(Index: Integer): THeaderFooterItem;
begin
 Result := THeaderFooterItem(FItems[Index]);
end;

procedure THeaderFooter.SetPixPrInch(Value: Integer);
 Var i, TmpSize: Integer;
     AFont: TFont;
begin
 For i := 0 To FItems.Count - 1 Do
  Begin
   AFont := THeaderFooterItem(FItems[i]).Font;
   TmpSize := AFont.Size;
   AFont.PixelsPerInch := Value;
   AFont.Size := TmpSize;
  End;
end;

Function CompareItems(Item1, Item2: Pointer): Integer;
begin
 Result := THeaderFooterItem(Item1).LineNumber - THeaderFooterItem(Item2).LineNumber;

 If Result = 0 Then
  Result := Integer(Item1) - Integer(Item2);
end;

procedure THeaderFooter.InitPrint(ACanvas: TCanvas; NumPages: Integer;
  Title: string; Margins: TMySEPrintMargins);
begin
 SaveFontPenBrush(ACanvas);
 FDate := DateToStr(Now);
 FTime := TimeToStr(Now);
 FNumPages := NumPages;
 FMargins := Margins;
 FTitle := Title;
 FItems.Sort(@CompareItems);
 FixLines;
 CalcHeight(ACanvas);
 RestoreFontPenBrush(ACanvas);
end;

procedure THeaderFooter.Print(ACanvas: TCanvas; PageNum: Integer);
 Var i, X, Y, CurLine: Integer;
     AStr: string;
     AItem: THeaderFooterItem;
     TheAlignment: TAlignment;
begin
 If (FFrameHeight <= 0) Then
  Exit;

 SaveFontPenBrush(ACanvas);
 DrawFrame(ACanvas);
 ACanvas.Brush.Style := bsClear;

 If FType = hftHeader Then
  Y := FMargins.PHeader - FFrameHeight
 Else
  Y := FMargins.PFooter;

 Y := Y + FMargins.PHFInternalMargin;
 CurLine := 1;

 For i := 0 To FItems.Count - 1 Do
  Begin
   AItem := THeaderFooterItem(FItems[i]);
   ACanvas.Font := AItem.Font;

   If AItem.LineNumber <> CurLine Then
    Begin
     Y := Y + TLineInfo(FLineInfo[CurLine - 1]).LineHeight;

     CurLine := AItem.LineNumber;
    End;

   AStr := AItem.GetText(FNumPages, PageNum, FRomanNumbers, FTitle, FTime, FDate);
   TheAlignment := AItem.Alignment;

   If MirrorPosition And ((PageNum Mod 2) = 0) Then
    Begin
     Case AItem.Alignment Of
          taRightJustify    : TheAlignment := taLeftJustify;
          taLeftJustify     : TheAlignment := taRightJustify;
     End;
    End;

   With FMargins Do
    Begin
     X := PLeftHFTextIndent;

     Case TheAlignment Of
          taRightJustify    : X := PRightHFTextIndent - ACanvas.TextWidth(AStr);
          taCenter          : X := (PLeftHFTextIndent + PRightHFTextIndent - ACanvas.TextWidth(AStr)) div 2;
     End;
    End;

   ACanvas.TextOut(X, Y + TLineInfo(FLineInfo[CurLine - 1]).MaxBaseDist, AStr);
  End;

 RestoreFontPenBrush(ACanvas);
end;

procedure THeaderFooter.Assign(Source: TPersistent);
 Var Src: THeaderFooter;
     i: Integer;
begin
 If (Source <> Nil) And (Source Is THeaderFooter) Then
  Begin
   Src := THeaderFooter(Source);
   Clear;
   FType := Src.FType;
   FFrameTypes := Src.FFrameTypes;
   FShadedColor := Src.FShadedColor;
   FLineColor := Src.FLineColor;

   For i := 0 To Src.FItems.Count - 1 Do
    Begin
     With THeaderFooterItem(Src.FItems[i]) Do
      Add(Text, Font, Alignment, LineNumber);
    End;

   FDefaultFont.Assign(Src.FDefaultFont);
   FRomanNumbers := Src.FRomanNumbers;
   FMirrorPosition := Src.FMirrorPosition;
  End
 Else
  Inherited Assign(Source);
end;

procedure THeaderFooter.FixLines;
 Var i, CurLine: Integer;
     LineInfo: TLineInfo;
begin
 For i := 0 To FLineInfo.Count - 1 Do
  TLineInfo(FLineInfo[i]).Free;

 FLineInfo.Clear;

 CurLine := 0;
 FLineCount := 0;

 For i := 0 To FItems.Count - 1 Do
  Begin
   If THeaderFooterItem(FItems[i]).LineNumber <> CurLine Then
    Begin
     CurLine := THeaderFooterItem(FItems[i]).LineNumber;
     FLineCount := FLineCount + 1;

     LineInfo := TLineInfo.Create;

     FLineInfo.Add(LineInfo);
    End;

   THeaderFooterItem(FItems[i]).LineNumber := FLineCount;
  End;
end;

procedure THeaderFooter.LoadFromStream(AStream: TStream);
 Var Num, idx: integer;
     aCharset: TFontCharset;
     aColor: TColor;
     aHeight: Integer;
     aName: TFontName;
     aPitch: TFontPitch;
     aSize: Integer;
     aStyle: TFontStyles;
     bufSize : integer;
     buffer : PChar;
begin
 With AStream Do
  Begin
   Read(FFrameTypes, SizeOf(FFrameTypes));
   Read(FShadedColor, SizeOf(FShadedColor));
   Read(FLineColor, SizeOf(FLineColor));
   Read(FRomanNumbers, SizeOf(FRomanNumbers));
   Read(FMirrorPosition, SizeOf(FMirrorPosition));

   Read(aCharset, SizeOf(aCharset));
   Read(aColor, SizeOf(aColor));
   Read(aHeight, SizeOf(aHeight));
   Read(bufSize, SizeOf(bufSize));

   GetMem(buffer, bufSize+1);

   Try
     Read(buffer^, bufSize);
     buffer[bufSize] := #0;
     aName := buffer;
   Finally
     FreeMem(buffer);
   End;

   Read(aPitch, SizeOf(aPitch));
   Read(aSize, SizeOf(aSize));
   Read(aStyle, SizeOf(aStyle));

   FDefaultFont.Charset := aCharset;
   FDefaultFont.Color   := aColor;
   FDefaultFont.Height  := aHeight;
   FDefaultFont.Name    := aName;
   FDefaultFont.Pitch   := aPitch;
   FDefaultFont.Size    := aSize;
   FDefaultFont.Style   := aStyle;

   Read(Num, SizeOf(Num));

   While Num > 0 Do
    Begin
     idx := Add('', Nil, taLeftJustify, 1);

     Get(idx).LoadFromStream(AStream);

     Dec(Num);
    End;
  End;
end;

procedure THeaderFooter.SaveToStream(AStream: TStream);
 Var i, Num: integer;
     aCharset: TFontCharset;
     aColor: TColor;
     aHeight: Integer;
     aName: TFontName;
     aPitch: TFontPitch;
     aSize: Integer;
     aStyle: TFontStyles;
     aLen : integer;
begin
 With AStream Do
  Begin
   Write(FFrameTypes, SizeOf(FFrameTypes));
   Write(FShadedColor, SizeOf(FShadedColor));
   Write(FLineColor, SizeOf(FLineColor));
   Write(FRomanNumbers, SizeOf(FRomanNumbers));
   Write(FMirrorPosition, SizeOf(FMirrorPosition));

   aCharset := FDefaultFont.Charset;
   aColor   := FDefaultFont.Color;
   aHeight  := FDefaultFont.Height;
   aName    := FDefaultFont.Name;
   aPitch   := FDefaultFont.Pitch;
   aSize    := FDefaultFont.Size;
   aStyle   := FDefaultFont.Style;

   Write(aCharset, SizeOf(aCharset));
   Write(aColor, SizeOf(aColor));
   Write(aHeight, SizeOf(aHeight));

   aLen := Length(aName);

   Write(aLen, SizeOf(aLen));
   Write(PChar(aName)^, Length(aName));

   Write(aPitch, SizeOf(aPitch));
   Write(aSize, SizeOf(aSize));
   Write(aStyle, SizeOf(aStyle));

   Num := Count;
   Write(Num, SizeOf(Num));

   For i := 0 To Num - 1 Do
    Get(i).SaveToStream(AStream);
  End;
end;

{ THeader }

constructor THeader.Create;
begin
 Inherited;

 FType := hftHeader;
end;

{ TFooter }

constructor TFooter.Create;
begin
 Inherited;

 FType := hftFooter;
end;

end.

