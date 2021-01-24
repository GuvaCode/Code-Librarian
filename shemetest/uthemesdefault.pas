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
unit uThemesDefault;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

Type
    TPiNoteTheme              = Packed Record
      Name                    : String[200];
      IsInternal              : Boolean;
      MainForeground          : TColor;
      MainBackground          : TColor;
      ColorComment            : TColor;
      ColorNumber             : TColor;
      ColorPreProcessor       : TColor;
      ColorReservedWord       : TColor;
      ColorString             : TColor;
      ColorSymbol             : TColor;
      LineForeground          : TColor;
      LineBackground          : TColor;
    end;

Var
   PiNoteThemes               : Array Of TPiNoteTheme;
   ThemeInUseIdx              : Integer = 0;

Procedure CreateDefaultThemes;
Procedure DestroyDefaultThemes;

implementation

Procedure AddTheme(Const Th : TPiNoteTheme);
 Var tPos : Integer;
Begin
 tPos := Length(PiNoteThemes);
 SetLength(PiNoteThemes, Length(PiNoteThemes) + 1);

 PiNoteThemes[tPos] := Th;
end;

procedure CreateDefaultThemes;
 Var R : TPiNoteTheme;
begin
 SetLength(PiNoteThemes, 0);

 R.Name := 'Default';
 R.IsInternal := True;
 R.MainForeground := clBlack;
 R.MainBackground := clWhite;
 R.ColorComment := clGray;
 R.ColorNumber := clFuchsia;
 R.ColorPreProcessor := clMaroon;
 R.ColorReservedWord := clBlue;
 R.ColorString := clRed;
 R.ColorSymbol := clMaroon;
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Amber';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(254, 195, 40);
 R.MainBackground := RgbToColor(29, 27, 22);
 R.ColorComment := RgbToColor(122, 27, 22);
 R.ColorNumber := RgbToColor(163, 69, 22);
 R.ColorPreProcessor := RgbToColor(57, 27, 22);
 R.ColorReservedWord := RgbToColor(195, 139, 22);
 R.ColorString := RgbToColor(253, 204, 91);
 R.ColorSymbol := RgbToColor(195, 139, 22);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(49, 46, 37);

 AddTheme(R);

 R.Name := 'Amstrad CPC';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(254, 251, 0);
 R.MainBackground := RgbToColor(0, 0, 98);
 R.ColorComment := RgbToColor(254, 251, 0);
 R.ColorNumber := RgbToColor(254, 251, 0);
 R.ColorPreProcessor := RgbToColor(254, 251, 0);
 R.ColorReservedWord := RgbToColor(254, 251, 0);
 R.ColorString := RgbToColor(254, 251, 0);
 R.ColorSymbol := RgbToColor(254, 251, 0);
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Bespins';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(189, 174, 157);
 R.MainBackground := RgbToColor(42, 33, 28);
 R.ColorComment := RgbToColor(30, 154, 224);
 R.ColorNumber := RgbToColor(255, 58, 131);
 R.ColorPreProcessor := RgbToColor(255, 170, 0);
 R.ColorReservedWord := RgbToColor(246, 240, 128);
 R.ColorString := RgbToColor(85, 228, 57);
 R.ColorSymbol := RgbToColor(255, 170, 0);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(75, 60, 52);

 AddTheme(R);

 R.Name := 'Black Book';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(248, 248, 248);
 R.MainBackground := RgbToColor(12, 16, 33);
 R.ColorComment := RgbToColor(174, 174, 174);
 R.ColorNumber := RgbToColor(216, 250, 60);
 R.ColorPreProcessor := RgbToColor(251, 222, 45);
 R.ColorReservedWord := RgbToColor(251, 222, 45);
 R.ColorString := RgbToColor(97, 206, 60);
 R.ColorSymbol := RgbToColor(251, 222, 45);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(18, 24, 48);

 AddTheme(R);

 R.Name := 'Black Coffe';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(179, 49, 0);
 R.MainBackground := RgbToColor(33, 25, 18);
 R.ColorComment := RgbToColor(148, 151, 149);
 R.ColorNumber := clYellow;
 R.ColorPreProcessor := RgbToColor(156, 33, 185);
 R.ColorReservedWord := RgbToColor(230, 118, 0);
 R.ColorString := RgbToColor(99, 150, 76);
 R.ColorSymbol := RgbToColor(18, 91, 143);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(76, 57, 42);

 AddTheme(R);

 R.Name := 'Black and White';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(255, 255, 255);
 R.MainBackground := RgbToColor(50, 50, 50);
 R.ColorComment := RgbToColor(139, 139, 139);
 R.ColorNumber := RgbToColor(20, 20, 20);
 R.ColorPreProcessor := RgbToColor(185, 185, 185);
 R.ColorReservedWord := RgbToColor(151, 151, 151);
 R.ColorString := RgbToColor(100, 100, 100);
 R.ColorSymbol := RgbToColor(90, 90, 90);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(70, 70, 70);

 AddTheme(R);

 R.Name := 'Boxy';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(175, 175, 170);
 R.MainBackground := RgbToColor(27, 43, 52);
 R.ColorComment := RgbToColor(78, 91, 100);
 R.ColorNumber := RgbToColor(212, 128, 81);
 R.ColorPreProcessor := RgbToColor(74, 149, 151);
 R.ColorReservedWord := RgbToColor(213, 173, 89);
 R.ColorString := RgbToColor(76, 120, 158);
 R.ColorSymbol := RgbToColor(161, 202, 194);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(40, 55, 63);

 AddTheme(R);

 R.Name := 'Chocolate';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(195, 190, 152);
 R.MainBackground := RgbToColor(26, 15, 11);
 R.ColorComment := RgbToColor(103, 157, 71);
 R.ColorNumber := RgbToColor(218, 86, 89);
 R.ColorPreProcessor := RgbToColor(137, 150, 168);
 R.ColorReservedWord := RgbToColor(241, 230, 148);
 R.ColorString := RgbToColor(124, 165, 99);
 R.ColorSymbol := RgbToColor(179, 147, 92);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(40, 23, 17);

 AddTheme(R);

 R.Name := 'Christmas';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(246, 245, 239);
 R.MainBackground := RgbToColor(73, 38, 33);
 R.ColorComment := RgbToColor(106, 203, 97);
 R.ColorNumber := RgbToColor(194, 103, 141);
 R.ColorPreProcessor := RgbToColor(222, 68, 116);
 R.ColorReservedWord := RgbToColor(230, 101, 86);
 R.ColorString := RgbToColor(44, 84, 40);
 R.ColorSymbol := RgbToColor(238, 162, 159);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(101, 55, 49);

 AddTheme(R);

 R.Name := 'Classic Mac';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(0, 0, 0);
 R.MainBackground := RgbToColor(221, 221, 221);
 R.ColorComment := RgbToColor(102, 102, 128);
 R.ColorNumber := RgbToColor(204, 102, 52);
 R.ColorPreProcessor := RgbToColor(99, 80, 18);
 R.ColorReservedWord := RgbToColor(153, 153, 255);
 R.ColorString := RgbToColor(99, 99, 156);
 R.ColorSymbol := RgbToColor(51, 51, 153);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(153, 153, 153);

 AddTheme(R);

 R.Name := 'Commodore 16';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(0, 0, 0);
 R.MainBackground := RgbToColor(238, 238, 238);
 R.ColorComment := RgbToColor(0, 0, 0);
 R.ColorNumber := RgbToColor(0, 0, 0);
 R.ColorPreProcessor := RgbToColor(0, 0, 0);;
 R.ColorReservedWord := RgbToColor(183, 155, 255);
 R.ColorString := RgbToColor(0, 0, 0);
 R.ColorSymbol := RgbToColor(0, 0, 0);
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Commodore 64';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(134, 122, 222);
 R.MainBackground := RgbToColor(72, 58, 170);
 R.ColorComment := RgbToColor(134, 122, 222);
 R.ColorNumber := RgbToColor(134, 122, 222);
 R.ColorPreProcessor := RgbToColor(134, 122, 222);
 R.ColorReservedWord := RgbToColor(134, 122, 222);
 R.ColorString := RgbToColor(134, 122, 222);
 R.ColorSymbol := RgbToColor(134, 122, 222);
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Commodore 128';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(172, 234, 132);
 R.MainBackground := RgbToColor(84, 82, 84);
 R.ColorComment := RgbToColor(172, 234, 132);
 R.ColorNumber := RgbToColor(172, 234, 132);
 R.ColorPreProcessor := RgbToColor(172, 234, 132);
 R.ColorReservedWord := RgbToColor(172, 234, 132);
 R.ColorString := RgbToColor(172, 234, 132);
 R.ColorSymbol := RgbToColor(172, 234, 132);
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Commodore Amiga 500';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(255, 255, 255);
 R.MainBackground := RgbToColor(0, 85, 170);
 R.ColorComment := RgbToColor(255, 255, 255);
 R.ColorNumber := RgbToColor(255, 255, 255);
 R.ColorPreProcessor := RgbToColor(255, 255, 255);
 R.ColorReservedWord := RgbToColor(255, 255, 255);
 R.ColorString := RgbToColor(255, 255, 255);
 R.ColorSymbol := RgbToColor(255, 255, 255);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(255, 136, 0);

 AddTheme(R);

 R.Name := 'Commodore PET';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(0, 255, 0);
 R.MainBackground := RgbToColor(0, 0, 0);
 R.ColorComment := RgbToColor(0, 255, 0);
 R.ColorNumber := RgbToColor(0, 255, 0);
 R.ColorPreProcessor := RgbToColor(0, 255, 0);
 R.ColorReservedWord := RgbToColor(0, 255, 0);
 R.ColorString := RgbToColor(0, 255, 0);
 R.ColorSymbol := RgbToColor(0, 255, 0);
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Cyperpunk Runner';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(0, 251, 138);
 R.MainBackground := RgbToColor(0, 16, 16);
 R.ColorComment := RgbToColor(49, 141, 100);
 R.ColorNumber := RgbToColor(255, 95, 95);
 R.ColorPreProcessor := RgbToColor(148, 56, 69);
 R.ColorReservedWord := RgbToColor(255, 35, 56);
 R.ColorString := RgbToColor(1, 252, 157);
 R.ColorSymbol := RgbToColor(254, 254, 254);
 R.LineForeground := clWhite;
 R.LineBackground := RgbToColor(255, 37, 54);

 AddTheme(R);

 R.Name := 'Dark Blue';
 R.IsInternal := True;
 R.MainForeground := clWhite;
 R.MainBackground := RgbToColor(0, 0, 64);
 R.ColorComment := RgbToColor(128, 160, 255);
 R.ColorNumber := clWhite;
 R.ColorPreProcessor := clWhite;
 R.ColorReservedWord := clYellow;
 R.ColorString := RgbToColor(255, 160, 160);
 R.ColorSymbol := clWhite;
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Dark Matter';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(91, 111, 136);
 R.MainBackground := RgbToColor(20, 25, 31);
 R.ColorComment := RgbToColor(128, 160, 255);
 R.ColorNumber := RgbToColor(81, 92, 112);
 R.ColorPreProcessor := RgbToColor(182, 178, 192);
 R.ColorReservedWord := RgbToColor(18, 60, 100);
 R.ColorString := RgbToColor(65, 104, 119);
 R.ColorSymbol := RgbToColor(150, 164, 177);
 R.LineForeground := RgbToColor(91, 111, 136);
 R.LineBackground := RgbToColor(25, 60, 102);

 AddTheme(R);

 R.Name := 'Deep Dark';
 R.IsInternal := True;
 R.MainForeground := clWhite;
 R.MainBackground := clBlack;
 R.ColorComment := clGreen;
 R.ColorNumber := RgbToColor(255, 128, 0);
 R.ColorPreProcessor := RgbToColor(192, 192, 192);
 R.ColorReservedWord := RgbToColor(102, 255, 0);
 R.ColorString := RgbToColor(102, 255, 0);
 R.ColorSymbol := RgbToColor(205, 204, 0);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(51, 51, 51);

 AddTheme(R);

 R.Name := 'Fairy Floss';
 R.IsInternal := True;
 R.MainForeground := clWhite;
 R.MainBackground := RgbToColor(92, 83, 120);
 R.ColorComment := RgbToColor(176, 114, 122);
 R.ColorNumber := RgbToColor(205, 158, 255);
 R.ColorPreProcessor := RgbToColor(255, 217, 76);
 R.ColorReservedWord := RgbToColor(178, 255, 221);
 R.ColorString := RgbToColor(255, 236, 0);
 R.ColorSymbol := RgbToColor(255, 180, 210);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(116, 101, 157);

 AddTheme(R);

 R.Name := 'Foxy';
 R.IsInternal := True;
 R.MainForeground := clWhite;
 R.MainBackground := clBlack;
 R.ColorComment := clGreen;
 R.ColorNumber := RgbToColor(37, 200, 73);
 R.ColorPreProcessor := RgbToColor(255, 217, 76);
 R.ColorReservedWord := RgbToColor(45, 211, 253);
 R.ColorString := RgbToColor(245, 189, 0);
 R.ColorSymbol := RgbToColor(45, 211, 253);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(37, 37, 37);

 AddTheme(R);

 R.Name := 'Frozen';
 R.IsInternal := True;
 R.MainForeground := clWhite;
 R.MainBackground := RgbToColor(54, 68, 80);
 R.ColorComment := RgbToColor(91, 117, 122);
 R.ColorNumber := RgbToColor(173, 225, 237);
 R.ColorPreProcessor := RgbToColor(143, 199, 242);
 R.ColorReservedWord := RgbToColor(156, 170, 173);
 R.ColorString := RgbToColor(175, 228, 232);
 R.ColorSymbol := RgbToColor(168, 233, 250);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(82, 103, 122);

 AddTheme(R);

 R.Name := 'Garden of Eden';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(243, 219, 189);
 R.MainBackground := RgbToColor(0, 80, 80);
 R.ColorComment := RgbToColor(90, 156, 145);
 R.ColorNumber := RgbToColor(58, 127, 126);
 R.ColorPreProcessor := RgbToColor(0, 198, 199);
 R.ColorReservedWord := RgbToColor(0, 239, 12);
 R.ColorString := RgbToColor(64, 173, 101);
 R.ColorSymbol := RgbToColor(78, 140, 156);
 R.LineForeground := RgbToColor(0, 40, 40);
 R.LineBackground := RgbToColor(0, 126, 126);

 AddTheme(R);

 R.Name := 'Google style';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(81, 69, 53);
 R.MainBackground := RgbToColor(231, 237, 245);
 R.ColorComment := RgbToColor(174, 173, 172);
 R.ColorNumber := RgbToColor(227, 85, 255);
 R.ColorPreProcessor := RgbToColor(214, 149, 0);
 R.ColorReservedWord := RgbToColor(234, 67, 53);
 R.ColorString := RgbToColor(66, 133, 244);
 R.ColorSymbol := RgbToColor(52, 168, 83);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(121, 121, 134);

 AddTheme(R);

 R.Name := 'Gotham';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(99, 157, 164);
 R.MainBackground := RgbToColor(10, 15, 19);
 R.ColorComment := RgbToColor(45, 85, 96);
 R.ColorNumber := RgbToColor(138, 141, 170);
 R.ColorPreProcessor := RgbToColor(188, 45, 38);
 R.ColorReservedWord := RgbToColor(23, 88, 108);
 R.ColorString := RgbToColor(45, 181, 149);
 R.ColorSymbol := RgbToColor(210, 106, 59);
 R.LineForeground := RgbToColor(172, 215, 223);
 R.LineBackground := RgbToColor(36, 82, 97);

 AddTheme(R);

 R.Name := 'Green Term';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(215, 255, 104);
 R.MainBackground := RgbToColor(27, 29, 22);
 R.ColorComment := RgbToColor(97, 116, 47);
 R.ColorNumber := RgbToColor(27, 122, 48);
 R.ColorPreProcessor := RgbToColor(47, 213, 84);
 R.ColorReservedWord := RgbToColor(157, 185, 40);
 R.ColorString := RgbToColor(230, 255, 164);
 R.ColorSymbol := RgbToColor(124, 122, 22);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(51, 55, 42);

 AddTheme(R);

 R.Name := 'Iceberg';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(199, 200, 209);
 R.MainBackground := RgbToColor(22, 24, 32);
 R.ColorComment := RgbToColor(101, 112, 135);
 R.ColorNumber := RgbToColor(158, 148, 195);
 R.ColorPreProcessor := RgbToColor(182, 189, 136);
 R.ColorReservedWord := RgbToColor(137, 160, 195);
 R.ColorString := RgbToColor(146, 182, 192);
 R.ColorSymbol := RgbToColor(199, 200, 209);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(30, 33, 48);

 AddTheme(R);

 R.Name := 'Idle Atom';
 R.IsInternal := True;
 R.MainForeground := clBlack;
 R.MainBackground := clWhite;
 R.ColorComment := clRed;
 R.ColorNumber := clBlack;
 R.ColorPreProcessor := RgbToColor(71, 0, 255);
 R.ColorReservedWord := RgbToColor(255, 107, 0);
 R.ColorString := RgbToColor(0, 180, 15);
 R.ColorSymbol := clBlack;
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(196, 196, 196);

 AddTheme(R);

 R.Name := 'Imgres';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(155, 131, 100);
 R.MainBackground := RgbToColor(31, 25, 19);
 R.ColorComment := RgbToColor(108, 91, 70);
 R.ColorNumber := RgbToColor(152, 85, 68);
 R.ColorPreProcessor := RgbToColor(104, 117, 14);
 R.ColorReservedWord := RgbToColor(165, 106, 43);
 R.ColorString := RgbToColor(147, 161, 71);
 R.ColorSymbol := RgbToColor(165, 104, 33);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(43, 35, 27);

 AddTheme(R);

 R.Name := 'Khaki';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(95, 95, 0);
 R.MainBackground := RgbToColor(215, 215, 175);
 R.ColorComment := RgbToColor(135, 135, 95);
 R.ColorNumber := RgbToColor(0, 95, 0);
 R.ColorPreProcessor := RgbToColor(95, 95, 0);
 R.ColorReservedWord := RgbToColor(95, 0, 95);
 R.ColorString := RgbToColor(0, 95, 95);
 R.ColorSymbol := RgbToColor(0, 0, 95);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(175, 175, 135);

 AddTheme(R);

 R.Name := 'Light Storm';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(56, 58, 66);
 R.MainBackground := RgbToColor(247, 247, 247);
 R.ColorComment := RgbToColor(201, 153, 153);
 R.ColorNumber := RgbToColor(228, 91, 73);
 R.ColorPreProcessor := RgbToColor(130, 95, 42);
 R.ColorReservedWord := RgbToColor(64, 139, 243);
 R.ColorString := RgbToColor(80, 161, 79);
 R.ColorSymbol := RgbToColor(166, 38, 164);
 R.LineForeground := clWhite;
 R.LineBackground := RgbToColor(160, 121, 121);

 AddTheme(R);

 R.Name := 'Manuscript';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(0, 148, 164);
 R.MainBackground := RgbToColor(238, 234, 202);
 R.ColorComment := RgbToColor(172, 171, 147);
 R.ColorNumber := RgbToColor(249, 174, 3);
 R.ColorPreProcessor := RgbToColor(128, 188, 43);
 R.ColorReservedWord := RgbToColor(62, 124, 212);
 R.ColorString := RgbToColor(222, 64, 135);
 R.ColorSymbol := RgbToColor(91, 90, 98);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(241, 234, 147);

 AddTheme(R);

 R.Name := 'Mossy-Lawn';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(242, 196, 118);
 R.MainBackground := RgbToColor(88, 105, 61);
 R.ColorComment := RgbToColor(42, 57, 14);
 R.ColorNumber := RgbToColor(255, 220, 135);
 R.ColorPreProcessor := RgbToColor(255, 187, 170);
 R.ColorReservedWord := RgbToColor(239, 197, 61);
 R.ColorString := RgbToColor(255, 220, 135);
 R.ColorSymbol := RgbToColor(255, 238, 136);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(113, 122, 57);

 AddTheme(R);

 R.Name := 'MSX 1';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(255, 255, 255);
 R.MainBackground := RgbToColor(32, 32, 255);
 R.ColorComment := RgbToColor(255, 255, 255);
 R.ColorNumber := RgbToColor(255, 255, 255);
 R.ColorPreProcessor := RgbToColor(255, 255, 255);
 R.ColorReservedWord := RgbToColor(255, 255, 255);
 R.ColorString := RgbToColor(255, 255, 255);
 R.ColorSymbol := RgbToColor(255, 255, 255);
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Nautilus';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(245, 245, 245);
 R.MainBackground := RgbToColor(30, 40, 61);
 R.ColorComment := RgbToColor(242, 55, 50);
 R.ColorNumber := RgbToColor(93, 246, 250);
 R.ColorPreProcessor := RgbToColor(45, 92, 109);
 R.ColorReservedWord := RgbToColor(247, 203, 67);
 R.ColorString := RgbToColor(139, 144, 154);
 R.ColorSymbol := RgbToColor(142, 117, 39);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(41, 55, 84);

 AddTheme(R);

 R.Name := 'Navy Gray Blue';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(212, 207, 205);
 R.MainBackground := RgbToColor(25, 54, 85);
 R.ColorComment := RgbToColor(207, 141, 68);
 R.ColorNumber := RgbToColor(212, 207, 205);
 R.ColorPreProcessor := RgbToColor(91, 152, 200);
 R.ColorReservedWord := RgbToColor(189, 192, 134);
 R.ColorString := RgbToColor(135, 183, 163);
 R.ColorSymbol := RgbToColor(135, 183, 163);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(71, 87, 136);

 AddTheme(R);

 R.Name := 'No Caffeine';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(213, 213, 209);
 R.MainBackground := RgbToColor(32, 32, 34);
 R.ColorComment := RgbToColor(90, 89, 88);
 R.ColorNumber := RgbToColor(241, 239, 139);
 R.ColorPreProcessor := RgbToColor(209, 133, 79);
 R.ColorReservedWord := RgbToColor(164, 130, 86);
 R.ColorString := RgbToColor(151, 193, 103);
 R.ColorSymbol := RgbToColor(145, 137, 111);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(23, 23, 24);

 AddTheme(R);

 R.Name := 'Nord';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(212, 222, 232);
 R.MainBackground := RgbToColor(47, 52, 63);
 R.ColorComment := RgbToColor(99, 110, 133);
 R.ColorNumber := RgbToColor(180, 142, 173);
 R.ColorPreProcessor := RgbToColor(152, 187, 187);
 R.ColorReservedWord := RgbToColor(129, 161, 193);
 R.ColorString := RgbToColor(168, 189, 145);
 R.ColorSymbol := RgbToColor(129, 161, 193);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(60, 66, 80);

 AddTheme(R);

 R.Name := 'Obsidian';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(224, 226, 228);
 R.MainBackground := RgbToColor(41, 49, 52);
 R.ColorComment := RgbToColor(102, 116, 123);
 R.ColorNumber := RgbToColor(255, 205, 34);
 R.ColorPreProcessor := RgbToColor(160, 130, 189);
 R.ColorReservedWord := RgbToColor(147, 199, 99);
 R.ColorString := RgbToColor(236, 118, 0);
 R.ColorSymbol := RgbToColor(232, 226, 183);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(47, 57, 60);

 AddTheme(R);

 R.Name := 'Ocean';
 R.IsInternal := True;
 R.MainForeground := clYellow;
 R.MainBackground := clNavy;
 R.ColorComment := clGray;
 R.ColorNumber := clFuchsia;
 R.ColorPreProcessor := clRed;
 R.ColorReservedWord := clAqua;
 R.ColorString := clSkyBlue;
 R.ColorSymbol := clAqua;
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Old guy';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(126, 126, 254);
 R.MainBackground := RgbToColor(2, 2, 2);
 R.ColorComment := RgbToColor(82, 82, 82);
 R.ColorNumber := RgbToColor(254, 254, 254);
 R.ColorPreProcessor := RgbToColor(196, 196, 196);
 R.ColorReservedWord := RgbToColor(86, 254, 86);
 R.ColorString := RgbToColor(254, 86, 254);
 R.ColorSymbol := RgbToColor(170, 2, 2);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(78, 95, 95);

 AddTheme(R);

 R.Name := 'Pinky Pie';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(235, 164, 172);
 R.MainBackground := RgbToColor(32, 35, 48);
 R.ColorComment := RgbToColor(109, 122, 114);
 R.ColorNumber := RgbToColor(88, 184, 150);
 R.ColorPreProcessor := RgbToColor(162, 194, 235);
 R.ColorReservedWord := RgbToColor(241, 71, 145);
 R.ColorString := RgbToColor(255, 240, 245);
 R.ColorSymbol := RgbToColor(255, 200, 91);
 R.LineForeground := RgbToColor(68, 33, 38);;
 R.LineBackground := RgbToColor(254, 124, 142);

 AddTheme(R);

 R.Name := 'Quantun Night';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(0, 204, 204);
 R.MainBackground := RgbToColor(0, 0, 0);
 R.ColorComment := RgbToColor(102, 102, 102);
 R.ColorNumber := RgbToColor(0, 204, 0);
 R.ColorPreProcessor := RgbToColor(204, 0, 0);
 R.ColorReservedWord := RgbToColor(204, 204, 204);
 R.ColorString := RgbToColor(170, 20, 167);
 R.ColorSymbol := RgbToColor(204, 204, 204);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(62, 62, 62);

 AddTheme(R);

 R.Name := 'Queen';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(121, 193, 255);
 R.MainBackground := RgbToColor(14, 18, 28);
 R.ColorComment := RgbToColor(44, 141, 54);
 R.ColorNumber := RgbToColor(197, 129, 195);
 R.ColorPreProcessor := RgbToColor(83, 194, 235);
 R.ColorReservedWord := RgbToColor(54, 194, 176);
 R.ColorString := RgbToColor(253, 166, 161);
 R.ColorSymbol := RgbToColor(235, 244, 248);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(18, 32, 54);

 AddTheme(R);

 R.Name := 'Red';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(238, 225, 219);
 R.MainBackground := RgbToColor(56, 0, 1);
 R.ColorComment := RgbToColor(164, 116, 116);
 R.ColorNumber := RgbToColor(81, 123, 147);
 R.ColorPreProcessor := RgbToColor(188, 45, 51);
 R.ColorReservedWord := RgbToColor(211, 120, 89);
 R.ColorString := RgbToColor(197, 139, 138);
 R.ColorSymbol := RgbToColor(249, 198, 117);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(118, 0, 0);

 AddTheme(R);

 R.Name := 'Rob Blue';
 R.IsInternal := True;
 R.MainForeground := clWhite;
 R.MainBackground := RgbToColor(17, 36, 53);
 R.ColorComment := clWhite;
 R.ColorNumber := clWhite;
 R.ColorPreProcessor := clWhite;
 R.ColorReservedWord := clWhite;
 R.ColorString := clWhite;
 R.ColorSymbol := clWhite;
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(39, 58, 75);

 AddTheme(R);

 R.Name := 'Rose Light';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(103, 126, 132);
 R.MainBackground := RgbToColor(240, 223, 217);
 R.ColorComment := RgbToColor(43, 53, 56);
 R.ColorNumber := RgbToColor(219, 56, 128);
 R.ColorPreProcessor := RgbToColor(129, 152, 42);
 R.ColorReservedWord := RgbToColor(0, 139, 205);
 R.ColorString := RgbToColor(0, 160, 151);
 R.ColorSymbol := RgbToColor(210, 75, 39);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(234, 210, 201);

 AddTheme(R);

 R.Name := 'Seoul Light';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(97, 97, 97);
 R.MainBackground := RgbToColor(225, 225, 225);
 R.ColorComment := RgbToColor(113, 152, 114);
 R.ColorNumber := RgbToColor(178, 177, 156);
 R.ColorPreProcessor := RgbToColor(154, 117, 162);
 R.ColorReservedWord := RgbToColor(225, 120, 153);
 R.ColorString := RgbToColor(0, 151, 171);
 R.ColorSymbol := RgbToColor(190, 117, 114);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(217, 217, 217);

 AddTheme(R);

 R.Name := 'Sublime UI';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(77, 77, 76);
 R.MainBackground := RgbToColor(243, 243, 243);
 R.ColorComment := RgbToColor(148, 148, 171);
 R.ColorNumber := RgbToColor(185, 144, 143);
 R.ColorPreProcessor := RgbToColor(150, 89, 168);
 R.ColorReservedWord := RgbToColor(215, 95, 0);
 R.ColorString := RgbToColor(0, 255, 0);
 R.ColorSymbol := RgbToColor(48, 48, 48);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(235, 235, 235);

 AddTheme(R);

 R.Name := 'Tech49';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(157, 195, 181);
 R.MainBackground := clBlack;
 R.ColorComment := RgbToColor(57, 98, 105);
 R.ColorNumber := RgbToColor(138, 136, 71);
 R.ColorPreProcessor := RgbToColor(252, 255, 199);
 R.ColorReservedWord := RgbToColor(100, 100, 100);
 R.ColorString := RgbToColor(96, 199, 210);
 R.ColorSymbol := RgbToColor(252, 103, 69);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(19, 18, 19);

 AddTheme(R);

 R.Name := 'Turbo C++';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(0, 255, 0);
 R.MainBackground := RgbToColor(0, 0, 128);
 R.ColorComment := RgbToColor(12, 161, 185);
 R.ColorNumber := RgbToColor(192, 192, 192);
 R.ColorPreProcessor := RgbToColor(0, 128, 128);
 R.ColorReservedWord := RgbToColor(255, 255, 255);
 R.ColorString := RgbToColor(128, 0, 29);
 R.ColorSymbol := RgbToColor(255, 255, 0);
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Turbo Pascal 7';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(255, 255, 63);
 R.MainBackground := RgbToColor(0, 0, 192);
 R.ColorComment := RgbToColor(192, 192, 192);
 R.ColorNumber := RgbToColor(0, 191, 192);
 R.ColorPreProcessor := RgbToColor(255, 255, 63);
 R.ColorReservedWord := RgbToColor(255, 255, 255);
 R.ColorString := RgbToColor(63, 255, 255);
 R.ColorSymbol := RgbToColor(255, 255, 63);
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);

 R.Name := 'Twilight';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(248, 248, 248);
 R.MainBackground := RgbToColor(20, 20, 20);
 R.ColorComment := RgbToColor(95, 90, 96);
 R.ColorNumber := RgbToColor(207, 106, 76);
 R.ColorPreProcessor := RgbToColor(137, 150, 168);
 R.ColorReservedWord := RgbToColor(249, 238, 152);
 R.ColorString := RgbToColor(143, 157, 106);
 R.ColorSymbol := RgbToColor(205, 168, 105);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(41, 41, 41);

 AddTheme(R);

 R.Name := 'Vibrant';
 R.IsInternal := True;
 R.MainForeground := clWhite;
 R.MainBackground := clBlack;
 R.ColorComment := RgbToColor(153, 51, 204);
 R.ColorNumber := RgbToColor(255, 128, 0);
 R.ColorPreProcessor := RgbToColor(237, 248, 249);
 R.ColorReservedWord := RgbToColor(255, 102, 0);
 R.ColorString := RgbToColor(102, 255, 0);
 R.ColorSymbol := RgbToColor(255, 204, 0);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(51, 51, 51);

 AddTheme(R);

 R.Name := 'Wild Cherry';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(248, 246, 237);
 R.MainBackground := RgbToColor(43, 32, 39);
 R.ColorComment := RgbToColor(142, 91, 216);
 R.ColorNumber := RgbToColor(151, 140, 154);
 R.ColorPreProcessor := RgbToColor(207, 92, 145);
 R.ColorReservedWord := RgbToColor(52, 179, 115);
 R.ColorString := RgbToColor(238, 221, 136);
 R.ColorSymbol := RgbToColor(248, 246, 237);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(75, 8, 136);

 AddTheme(R);

 R.Name := 'Zen';
 R.IsInternal := True;
 R.MainForeground := RgbToColor(220, 220, 204);
 R.MainBackground := RgbToColor(63, 63, 63);
 R.ColorComment := RgbToColor(127, 159, 127);
 R.ColorNumber := RgbToColor(140, 208, 211);
 R.ColorPreProcessor := RgbToColor(205, 207, 175);
 R.ColorReservedWord := RgbToColor(223, 196, 125);
 R.ColorString := RgbToColor(204, 147, 147);
 R.ColorSymbol := RgbToColor(159, 157, 109);
 R.LineForeground := clNone;
 R.LineBackground := RgbToColor(16, 16, 16);

 AddTheme(R);

 R.Name := 'ZX Spectrum';
 R.IsInternal := True;
 R.MainForeground := clBlack;
 R.MainBackground := RgbToColor(200, 196, 200);
 R.ColorComment := clBlack;
 R.ColorNumber := clBlack;
 R.ColorPreProcessor := clBlack;
 R.ColorReservedWord := clBlack;
 R.ColorString := clBlack;
 R.ColorSymbol := clBlack;
 R.LineForeground := clNone;
 R.LineBackground := clNone;

 AddTheme(R);
end;

procedure DestroyDefaultThemes;
begin
 SetLength(PiNoteThemes, 0);
end;

end.


