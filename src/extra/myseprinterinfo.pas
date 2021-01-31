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
unit MySEPrinterInfo;

{$I MySynEdit.inc}

interface

uses
  Classes, SysUtils, Printers;

Type

    { TMySEPrinterInfo }

    TMySEPrinterInfo          = Class
     Private
      FPhysicalWidth          : Integer;
      FPhysicalHeight         : Integer;
      FPrintableWidth         : Integer;
      FPrintableHeight        : Integer;
      FLeftGutter             : Integer;
      FRightGutter            : Integer;
      FTopGutter              : Integer;
      FBottomGutter           : Integer;
      FXPixPrInch             : Integer;
      FYPixPrInch             : Integer;
      FXPixPrmm               : Single;
      FYPixPrmm               : Single;
      FIsUpdated              : Boolean;

      procedure FillDefault;
      function GetBottomGutter: Integer;
      function GetLeftGutter: Integer;
      function GetPhysicalHeight: Integer;
      function GetPhysicalWidth: Integer;
      function GetPrintableHeight: Integer;
      function GetPrintableWidth: Integer;
      function GetRightGutter: Integer;
      function GetTopGutter: Integer;
      function GetXPixPrInch: Integer;
      function GetYPixPrInch: Integer;
      function GetXPixPrmm: Single;
      function GetYPixPrmm: Single;

     Public
      procedure UpdatePrinter;
      function PixFromLeft(mmValue: Double): Integer;
      function PixFromRight(mmValue: Double): Integer;
      function PixFromTop(mmValue: Double): Integer;
      function PixFromBottom(mmValue: Double): Integer;

      property PhysicalWidth: Integer read GetPhysicalWidth;
      property PhysicalHeight: Integer read GetPhysicalHeight;
      property PrintableWidth: Integer read GetPrintableWidth;
      property PrintableHeight: Integer read GetPrintableHeight;
      property LeftGutter: Integer read GetLeftGutter;
      property RightGutter: Integer read GetRightGutter;
      property TopGutter: Integer read GetTopGutter;
      property BottomGutter: Integer read GetBottomGutter;
      property XPixPrInch: Integer read GetXPixPrInch;
      property YPixPrInch: Integer read GetYPixPrInch;
      property XPixPrmm: Single read GetXPixPrmm;
      property YPixPrmm: Single read GetYPixPrmm;
    End;

implementation

{ TMySEPrinterInfo }

procedure TMySEPrinterInfo.FillDefault;
begin
 FPhysicalWidth := 2481;
 FPhysicalHeight := 3507;
 FPrintableWidth := 2358;
 FPrintableHeight := 3407;
 FLeftGutter := 65;
 FRightGutter := 58;
 FTopGutter := 50;
 FBottomGutter := 50;
 FXPixPrInch := 300;
 FYPixPrInch := 300;
 FXPixPrmm := FXPixPrInch / 25.4;
 FYPixPrmm := FYPixPrInch / 25.4;
end;

function TMySEPrinterInfo.GetBottomGutter: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FBottomGutter;
end;

function TMySEPrinterInfo.GetLeftGutter: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FLeftGutter;
end;

function TMySEPrinterInfo.GetPhysicalHeight: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FPhysicalHeight;
end;

function TMySEPrinterInfo.GetPhysicalWidth: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FPhysicalWidth;
end;

function TMySEPrinterInfo.GetPrintableHeight: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FPrintableHeight;
end;

function TMySEPrinterInfo.GetPrintableWidth: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FPrintableWidth;
end;

function TMySEPrinterInfo.GetRightGutter: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FRightGutter;
end;

function TMySEPrinterInfo.GetTopGutter: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FTopGutter;
end;

function TMySEPrinterInfo.GetXPixPrInch: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FXPixPrInch;
end;

function TMySEPrinterInfo.GetYPixPrInch: Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FYPixPrInch;
end;

function TMySEPrinterInfo.GetXPixPrmm: Single;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FXPixPrmm;
end;

function TMySEPrinterInfo.GetYPixPrmm: Single;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := FYPixPrmm;
end;

procedure TMySEPrinterInfo.UpdatePrinter;
begin
 FIsUpdated := True;

 If Printer.Printers.Count <= 0 Then
  Begin
   FillDefault;

   Exit;
  End
 Else
  Begin
   //FPhysicalWidth := Printer.PaperSize.Width;
   //FPhysicalHeight := Printer.PaperSize.Height;

   FPhysicalWidth :=Printer.PaperSize.PaperRect.PhysicalRect.BottomRight.x -
                    Printer.PaperSize.PaperRect.PhysicalRect.TopLeft.x;

   FPhysicalHeight :=Printer.PaperSize.PaperRect.PhysicalRect.BottomRight.y -
                     Printer.PaperSize.PaperRect.PhysicalRect.TopLeft.y;

   FPrintableWidth := Printer.PageWidth;
   FPrintableHeight := Printer.PageHeight;

   FXPixPrInch := Printer.XDPI;
   FYPixPrInch := Printer.YDPI;
  End;

 FPrintableWidth := Printer.PageWidth;
 FPrintableHeight := Printer.PageHeight;

 FRightGutter := FPhysicalWidth - FPrintableWidth - FLeftGutter;
 FBottomGutter := FPhysicalHeight - FPrintableHeight - FTopGutter;

 FXPixPrmm := FXPixPrInch / 25.4;
 FYPixPrmm := FYPixPrInch / 25.4;
end;

function TMySEPrinterInfo.PixFromLeft(mmValue: Double): Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := Round(mmValue * FXPixPrmm - FLeftGutter);
end;

function TMySEPrinterInfo.PixFromRight(mmValue: Double): Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := Round(mmValue * FXPixPrmm - FRightGutter);
end;

function TMySEPrinterInfo.PixFromTop(mmValue: Double): Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := Round(mmValue * FYPixPrmm - FTopGutter);
end;

function TMySEPrinterInfo.PixFromBottom(mmValue: Double): Integer;
begin
 If Not FIsUpdated Then
  UpdatePrinter;

 Result := Round(mmValue * FYPixPrmm - FBottomGutter);
end;

end.

