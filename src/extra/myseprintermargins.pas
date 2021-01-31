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
unit MySEPrinterMargins;

{$M+}

{$I MySynEdit.inc }

interface

uses
  Classes, SysUtils, Graphics, MySEPrinterType, MySEPrinterInfo;

Type

    { TMySEPrintMargins }

    TMySEPrintMargins          = Class(TPersistent)
     Private
      FLeft, FRight, FTop, FBottom        : Double;
      FHeader, FFooter                    : Double;
      FLeftHFTextIndent                   : Double;
      FRightHFTextIndent                  : Double;
      FHFInternalMargin                   : Double;
      FGutter                             : Double;
      FMirrorMargins                      : Boolean;
      FUnitSystem                         : TUnitSystem;

      function ConvertTo(Value: Double): Double;
      function ConvertFrom(Value: Double): Double;
      function GetBottom: Double;
      function GetFooter: Double;
      function GetGutter: Double;
      function GetHeader: Double;
      function GetLeft: Double;
      function GetRight: Double;
      function GetTop: Double;
      function GetLeftHFTextIndent: Double;
      function GetRightHFTextIndent: Double;
      function GetHFInternalMargin: Double;
      procedure SetBottom(const Value: Double);
      procedure SetFooter(const Value: Double);
      procedure SetGutter(const Value: Double);
      procedure SetHeader(const Value: Double);
      procedure SetLeft(const Value: Double);
      procedure SetRight(const Value: Double);
      procedure SetTop(const Value: Double);
      procedure SetLeftHFTextIndent(const Value: Double);
      procedure SetRightHFTextIndent(const Value: Double);
      procedure SetHFInternalMargin(const Value: Double);

     Public
      PLeft, PRight, PTop, PBottom        : Integer;
      PHeader, PFooter                    : Integer;
      PLeftHFTextIndent                   : Integer;
      PRightHFTextIndent                  : Integer;
      PHFInternalMargin                   : Integer;
      PGutter                             : Integer;

      constructor Create;
      procedure InitPage(ACanvas: TCanvas; PageNum: Integer;
                PrinterInfo: TMySEPrinterInfo; LineNumbers,
                LineNumbersInMargin: Boolean; MaxLineNum: Integer);
      procedure Assign(Source: TPersistent); override;
      procedure LoadFromStream(AStream: TStream);
      procedure SaveToStream(AStream: TStream);

     Published
      property UnitSystem: TUnitSystem read FUnitSystem write FUnitSystem default usMM;
      property Left: Double read GetLeft write SetLeft;
      property Right: Double read GetRight write SetRight;
      property Top: Double read GetTop write SetTop;
      property Bottom: Double read GetBottom write SetBottom;
      property Header: Double read GetHeader write SetHeader;
      property Footer: Double read GetFooter write SetFooter;
      property LeftHFTextIndent: Double read GetLeftHFTextIndent write SetLeftHFTextIndent;
      property RightHFTextIndent: Double read GetRightHFTextIndent write SetRightHFTextIndent;
      property HFInternalMargin: Double read GetHFInternalMargin write SetHFInternalMargin;
      property Gutter: Double read GetGutter write SetGutter;
      property MirrorMargins: Boolean read FMirrorMargins write FMirrorMargins;
    End;

implementation

Const
     mmPrInch  = 25.4;
     mmPrCm    = 10;

{ TMySEPrintMargins }

function TMySEPrintMargins.ConvertTo(Value: Double): Double;
begin
 Case FUnitSystem Of
  usCM                     : Result := Value * mmPrCm;
  usInch                   : Result := Value * mmPrInch;
  muThousandthsOfInches    : Result := mmPrInch * Value / 1000;
  Else                       Result := Value;
 End;
end;

function TMySEPrintMargins.ConvertFrom(Value: Double): Double;
begin
 Case FUnitSystem Of
  usCM                     : Result := Value / mmPrCm;
  usInch                   : Result := Value / mmPrInch;
  muThousandthsOfInches    : Result := 1000 * Value / mmPrInch;
  Else                       Result := Value;
 End;
end;

function TMySEPrintMargins.GetBottom: Double;
begin
 Result := ConvertFrom(FBottom);
end;

function TMySEPrintMargins.GetFooter: Double;
begin
 Result := ConvertFrom(FFooter);
end;

function TMySEPrintMargins.GetGutter: Double;
begin
 Result := ConvertFrom(FGutter);
end;

function TMySEPrintMargins.GetHeader: Double;
begin
 Result := ConvertFrom(FHeader);
end;

function TMySEPrintMargins.GetLeft: Double;
begin
 Result := ConvertFrom(FLeft);
end;

function TMySEPrintMargins.GetRight: Double;
begin
 Result := ConvertFrom(FRight);
end;

function TMySEPrintMargins.GetTop: Double;
begin
 Result := ConvertFrom(FTop);
end;

function TMySEPrintMargins.GetLeftHFTextIndent: Double;
begin
 Result := ConvertFrom(FLeftHFTextIndent);
end;

function TMySEPrintMargins.GetRightHFTextIndent: Double;
begin
 Result := ConvertFrom(FRightHFTextIndent);
end;

function TMySEPrintMargins.GetHFInternalMargin: Double;
begin
 Result := ConvertFrom(FHFInternalMargin);
end;

procedure TMySEPrintMargins.SetBottom(const Value: Double);
begin
 FBottom := ConvertTo(Value);
end;

procedure TMySEPrintMargins.SetFooter(const Value: Double);
begin
 FFooter := ConvertTo(Value);
end;

procedure TMySEPrintMargins.SetGutter(const Value: Double);
begin
 FGutter := ConvertTo(Value);
end;

procedure TMySEPrintMargins.SetHeader(const Value: Double);
begin
 FHeader := ConvertTo(Value);
end;

procedure TMySEPrintMargins.SetLeft(const Value: Double);
begin
 FLeft := ConvertTo(Value);
end;

procedure TMySEPrintMargins.SetRight(const Value: Double);
begin
 FRight := ConvertTo(Value);
end;

procedure TMySEPrintMargins.SetTop(const Value: Double);
begin
 FTop := ConvertTo(Value);
end;

procedure TMySEPrintMargins.SetLeftHFTextIndent(const Value: Double);
begin
 FLeftHFTextIndent := ConvertTo(Value);
end;

procedure TMySEPrintMargins.SetRightHFTextIndent(const Value: Double);
begin
 FRightHFTextIndent := ConvertTo(Value);
end;

procedure TMySEPrintMargins.SetHFInternalMargin(const Value: Double);
begin
 FHFInternalMargin := ConvertTo(Value);
end;

constructor TMySEPrintMargins.Create;
begin
 Inherited;

 FUnitSystem := usMM;
 FLeft := DefLeft;
 FRight := DefRight;
 FTop := DefTop;
 FBottom := DefBottom;
 FHeader := DefHeader;
 FFooter := DefFooter;
 FLeftHFTextIndent := DefLeftHFTextIndent;
 FRightHFTextIndent := DefRightHFTextIndent;
 FHFInternalMargin := DefHFInternalMargin;
 FGutter := DefGutter;
 FMirrorMargins := False;
end;

procedure TMySEPrintMargins.InitPage(ACanvas: TCanvas; PageNum: Integer;
  PrinterInfo: TMySEPrinterInfo; LineNumbers, LineNumbersInMargin: Boolean;
  MaxLineNum: Integer);
begin
 If FMirrorMargins And ((PageNum Mod 2) = 0) Then
  Begin
   PLeft := PrinterInfo.PixFromLeft(FRight);
   PRight := PrinterInfo.PrintableWidth - PrinterInfo.PixFromRight(FLeft + FGutter);
  End
 Else
  Begin
   PLeft := PrinterInfo.PixFromLeft(FLeft + FGutter);
   PRight := PrinterInfo.PrintableWidth - PrinterInfo.PixFromRight(FRight);
  End;

 If LineNumbers And (Not LineNumbersInMargin) Then
  PLeft := PLeft + ACanvas.TextWidth(IntToStr(MaxLineNum) + ': ');

 PTop := PrinterInfo.PixFromTop(FTop);
 PBottom := PrinterInfo.PrintableHeight - PrinterInfo.PixFromBottom(FBottom);
 PHeader := PrinterInfo.PixFromTop(FHeader);
 PFooter := PrinterInfo.PrintableHeight - PrinterInfo.PixFromBottom(FFooter);
 PHFInternalMargin := Round(PrinterInfo.YPixPrmm * FHFInternalMargin);
 PGutter := Round(PrinterInfo.XPixPrmm * FGutter);
 PRightHFTextIndent := PRight - Round(PrinterInfo.XPixPrmm * FRightHFTextIndent);
 PLeftHFTextIndent := PLeft + Round(PrinterInfo.XPixPrmm * FLeftHFTextIndent);
end;

procedure TMySEPrintMargins.Assign(Source: TPersistent);
 Var Src : TMySEPrintMargins;
begin
 If (Source <> Nil) And (Source Is TMySEPrintMargins) Then
  Begin
   Src := TMySEPrintMargins(Source);

   FLeft := Src.FLeft;
   FRight := Src.FRight;
   FTop := Src.FTop;
   FBottom := Src.FBottom;
   FHeader := Src.FHeader;
   FFooter := Src.FFooter;
   FLeftHFTextIndent := Src.FLeftHFTextIndent;
   FRightHFTextIndent := Src.FRightHFTextIndent;
   FHFInternalMargin := Src.FHFInternalMargin;
   FGutter := Src.FGutter;
   FMirrorMargins := Src.FMirrorMargins;
   FUnitSystem := Src.FUnitSystem;
  End
 Else
  Inherited;
end;

procedure TMySEPrintMargins.LoadFromStream(AStream: TStream);
begin
 With AStream Do
  Begin
   Read(FUnitSystem, SizeOf(FUnitSystem));
   Read(FLeft, SizeOf(FLeft));
   Read(FRight, SizeOf(FRight));
   Read(FTop, SizeOf(FTop));
   Read(FBottom, SizeOf(FBottom));
   Read(FHeader, SizeOf(FHeader));
   Read(FFooter, SizeOf(FFooter));
   Read(FLeftHFTextIndent, SizeOf(FLeftHFTextIndent));
   Read(FRightHFTextIndent, SizeOf(FRightHFTextIndent));
   Read(FHFInternalMargin, SizeOf(FHFInternalMargin));
   Read(FGutter, SizeOf(FGutter));
   Read(FMirrorMargins, SizeOf(FMirrorMargins));
  End;
end;

procedure TMySEPrintMargins.SaveToStream(AStream: TStream);
begin
 With AStream Do
  Begin
   Write(FUnitSystem, SizeOf(FUnitSystem));
   Write(FLeft, SizeOf(FLeft));
   Write(FRight, SizeOf(FRight));
   Write(FTop, SizeOf(FTop));
   Write(FBottom, SizeOf(FBottom));
   Write(FHeader, SizeOf(FHeader));
   Write(FFooter, SizeOf(FFooter));
   Write(FLeftHFTextIndent, SizeOf(FLeftHFTextIndent));
   Write(FRightHFTextIndent, SizeOf(FRightHFTextIndent));
   Write(FHFInternalMargin, SizeOf(FHFInternalMargin));
   Write(FGutter, SizeOf(FGutter));
   Write(FMirrorMargins, SizeOf(FMirrorMargins));
  End;
end;

end.

