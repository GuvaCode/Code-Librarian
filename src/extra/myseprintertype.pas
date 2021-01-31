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
unit MySEPrinterType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  DefLeft = 25; //Default left margin [mm]
  DefRight = 15; //Default right margin [mm]
  DefTop = 25; //Default top margin [mm]
  DefBottom = 25; //Default bottom margin [mm]
  DefHeader = 15; //Default margin from top of paper to bottom of header [mm]
  DefFooter = 15; //Default margin from top of footer to bottom of paper [mm]
  DefLeftHFTextIndent = 2; //Default Header/footer indent from left margin [mm]
  DefRightHFTextIndent = 2; //Default Header/footer indent from right margin [mm]
  DefHFInternalMargin = 0.5; //Default Internal margin between Header/footer text and lines [mm]
  DefGutter = 0; //Default Binding gutter - added to left or right margin [mm]

type
//Frame around header/footer
  TFrameType = (ftLine, ftBox, ftShaded);
  TFrameTypes = set of TFrameType;
//Margin units (internally is allways used [mm])
  TUnitSystem = (usMM, usCM, usInch, muThousandthsOfInches);
//Print status events
  TSynPrintStatus = (psBegin, psNewPage, psEnd);
  TPrintStatusEvent = procedure(Sender: TObject; Status: TSynPrintStatus;
    PageNumber: Integer; var Abort: Boolean) of object;
//Event raised when a line is printed (can be used to generate Table of Contents)
  TPrintLineEvent = procedure(Sender: TObject; LineNumber, PageNumber: Integer) of object;
  TSysCharSet = set of Char;
type
  TWrapPos = class
  public
    Index: Integer;
  end;

Function IntToRoman(Value: Integer): string;

Function WrapTextEx(const Line: string; BreakChars: TSysCharSet;
                    MaxCol: Integer; AList: TList): Boolean;

implementation

function IntToRoman(Value: Integer): string;
begin
 Result := '';

 While Value >= 1000 Do
  Begin
   Result := Result + 'M';
   Value := Value - 1000;
  End;

 If Value >= 900 Then
  Begin
   Result := Result + 'CM';
   Value := Value - 900;
  End;

 While Value >= 500 Do
  Begin
   Result := Result + 'D';
   Value := Value - 500;
  End;

 If Value >= 400 Then
  Begin
   Result := Result + 'CD';
   Value := Value - 400;
  End;

 While Value >= 100 Do
  Begin
   Result := Result + 'C';
   Value := Value - 100;
  End;

 If Value >= 90 Then
  Begin
   Result := Result + 'XC';
   Value := Value - 90;
  End;

 While Value >= 50 Do
  Begin
   Result := Result + 'L';
   Value := Value - 50;
  End;

 If Value >= 40 Then
  Begin
   Result := Result + 'XL';
   Value := Value - 40;
  End;

 While Value >= 10 Do
  Begin
   Result := Result + 'X';
   Value := Value - 10;
  End;

 If Value >= 9 Then
  Begin
   Result := Result + 'IX';
   Value := Value - 9;
  End;

 While Value >= 5 Do
  Begin
   Result := Result + 'V';
   Value := Value - 5;
  End;

 If Value >= 4 Then
  Begin
   Result := Result + 'IV';
   Value := Value - 4;
  End;

 While Value > 0 Do
  Begin
   Result := Result + 'I';

   Dec(Value);
  End;
end;

function WrapTextEx(const Line: string; BreakChars: TSysCharSet;
  MaxCol: Integer; AList: TList): Boolean;
 Var WrapPos: TWrapPos;
     Pos, PreviousPos: Integer;
     Found: Boolean;
begin
 If Length(Line) <= MaxCol Then
  Begin
   Result := True;

   Exit;
  End;

 Result := False;
 Pos := 1;
 PreviousPos := 0;

 WrapPos := TWrapPos.Create;

 While Pos <= Length(Line) Do
  Begin
   Found := (Pos - PreviousPos > MaxCol) And (WrapPos.Index <> 0);

   If Not Found And (Line[Pos] In BreakChars) Then
    WrapPos.Index := Pos;

   If Found Then
    Begin
     Result := True;

     AList.Add(WrapPos);

     PreviousPos := WrapPos.Index;

     If ((Length(Line) - PreviousPos) > MaxCol) And (Pos < Length(Line)) Then
      WrapPos := TWrapPos.Create
     Else
      Break;
    End;

   Pos := Pos + 1;
  End;

 If (AList.Count = 0) Or (TWrapPos(AList.Last) <> WrapPos) Then
  WrapPos.Free;
end;

end.

