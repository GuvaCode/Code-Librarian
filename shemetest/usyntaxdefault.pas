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
unit uSyntaxDefault;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uSyntaxList, Graphics;

Const
     CommentTypeSingleLine       = 0;
     CommentTypeMultiLine        = 1;

Type
    TCommentData                  = Record
      LanguageID                  : Word;
      CommentType                 : Byte;
      SingleLineID                : String[10];
      MultiLineIDStart            : String[10];
      MultiLineIDEnd              : String[10];
    end;

Var UndefinedSampleSource         : Array[0..30] Of String;
    CommentDataList               : Array Of TCommentData;
    CommTmp                       : TCommentData;
    MainForeground                : TColor = clBlack;
    MainBackground                : TColor = clWhite;
    OldMainForeground             : TColor = clBlack;
    OldMainBackground             : TColor = clWhite;
    LineForeground                : TColor = clNone;
    LineBackGround                : TColor = clNone;
    OldLineForeground             : TColor = clNone;
    OldLineBackGround             : TColor = clNone;

    ColorComment                 : TColor = clGray;
    ColorNumber                  : TColor = clFuchsia;
    ColorPreProcessor            : TColor = clMaroon;
    ColorReservedWord            : TColor = clBlack;
    ColorString                  : TColor = clRed;
    ColorSymbol                  : TColor = clMaroon;

Procedure BuildDefaultAttribs;
//Procedure BuildListASMWordFinder;
Procedure SelectTheme(ThemeIdx : Integer; WSynList : TSyntaxList);

//Procedure ClearListASMWordFinder;
//Procedure AddIntoListASMWordFinder(Const UniqueWord : String; SyntaxIDX : Integer);

Procedure ClearCommentDataList;
Procedure CommentDataListAdd(Val : TCommentData);

implementation

Uses uThemesDefault, SynEditStrConst;

Const
     DOSIdx                       = 0;
     CPPIdx                       = 1;
     CSSIdx                       = 2;
     HTMLIdx                      = 3;
     VBIdx                        = 4;
     PASIdx                       = 5;
     JAVAIdx                      = 6;
     JSIdx                        = 7;
     PERLIdx                      = 8;
     XMLIdx                       = 9;
     USSIdx                       = 10;
     PHPIdx                       = 11;
     TeXIdx                       = 12;
     SQLIdx                       = 13;
     PyTIdx                       = 14;
     IniIdx                       = 15;
     PoIdx                        = 16;
     //FORIdx                       = 17;
     CSIdx                        = 17;
     HSKIdx                       = 19;
     RubyIdx                      = 20;
     x86Idx                       = 21;
     Z80Idx                       = 22;
     M68Idx                       = 23;
     M8051Idx                     = 24;
     P18FIdx                      = 25;
     TCLIdx                       = 26;
     InnoIdx                      = 27;
     HC86Idx                      = 28;
     ATMegaIdx                    = 29;
     NMos6585Idx                  = 30;
     x6800IDX                     = 31;
     x6809Idx                     = 32;
     ArmV7Idx                     = 33;
     P16Idx                       = 34;
     LuaIdx                       = 35;
     PrologIdx                    = 36;
     VHDLIdx                      = 37;
     DIdx                         = 38;
     PIC32MXIdx                   = 39;
     ST6Idx                       = 40;
     ST7Idx                       = 41;
     ADAIdx                       = 42;
     ACSIdx                       = 43;
     FRBIdx                       = 44;
     VRLIdx                       = 45;
     GOIdx                        = 46;
     TMSIdx                       = 47;
     PWSHIdx                      = 48;
     CMakeIdx                     = 49;
     GroovyIdx                    = 50;
     RustIDX                      = 51;
     AwkIdx                       = 52;
     HaxeIdx                      = 53;
     FSharpIdx                    = 54;
     RIdx                         = 55;
     VIdx                         = 56;
     MatlabIdx                    = 57;


procedure CreateDefaultDOSAttribs;
Begin
 //Comments
 SetAttributeOn(DOSIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(DOSIdx, 3, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(DOSIdx, 2, clNone, ColorReservedWord, [fsBold]);
 //Variable
 SetAttributeOn(DOSIdx, 5, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := DOSIdx;
 CommTmp.SingleLineID := 'rem';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultCPPAttribs;
begin
 //Comments
 SetAttributeOn(CPPIdx, 1, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(CPPIdx, 4, clNone, ColorNumber, []);
 //Preprocessor
 SetAttributeOn(CPPIdx, 5, clNone, ColorPreProcessor, []);
 //Reserved Word
 SetAttributeOn(CPPIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(CPPIdx, 8, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(CPPIdx, 9, clNone, ColorSymbol, []);


 UndefinedSampleSource[CPPIdx] := '//include headers; these are modules that include functions that you may use in your '  + Chr(13) +
                                  '//program; we will almost always need to include the header that'  + Chr(13) +
                                  '//defines cin and cout; the header is called iostream'  + Chr(13) +
                                  '#include <iostream>'  + Chr(13) +
                                  ''  + Chr(13) +
                                  'using namespace std;'  + Chr(13) +
                                  ''  + Chr(13) +
                                  'int main()'  + Chr(13) +
                                  '{'  + Chr(13) +
                                  '  // print output to user'  + Chr(13) +
                                  '  cout << "Hello World!" << endl;'  + Chr(13) +
                                  '  return 0;'  + Chr(13) +
                                  '}';

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := CPPIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultCssAttribs;
Begin
 //Comments
 SetAttributeOn(CSSIdx, 0, clNone, ColorComment, [fsItalic]);
 //Reserved Word
 SetAttributeOn(CSSIdx, 2, clNone, ColorReservedWord, [fsBold]);
 //Numbers
 SetAttributeOn(CSSIdx, 4, clNone, ColorNumber, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := CSSIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultHtmlAttribs;
Begin
 //Asp
 SetAttributeOn(HTMLIdx, 0, clNone, MainForeground, [fsItalic]);
 //CData
 SetAttributeOn(HTMLIdx, 1, clNone, MainForeground, []);
 //Comments
 SetAttributeOn(HTMLIdx, 2, clNone, ColorComment, [fsItalic]);
 //DocType
 SetAttributeOn(HTMLIdx, 3, clNone, MainForeground, [fsBold]);
 //Escape
 SetAttributeOn(HTMLIdx, 4, clNone, ColorSymbol, [fsBold]);
 //Reserved Word
 SetAttributeOn(HTMLIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //Symbol
 SetAttributeOn(HTMLIdx, 8, clNone, ColorSymbol, []);
 //String
 SetAttributeOn(HTMLIdx, 9, clNone, ColorString, []);
 //Unknown
 SetAttributeOn(HTMLIdx, 10, clNone, MainForeground, [fsUnderLine]);
 //Value
 SetAttributeOn(HTMLIdx, 11, clNone, ColorPreProcessor, []);

 UndefinedSampleSource[HTMLIdx] := '<html>' + Chr(13) +
                                   '<head>' + Chr(13) +
                                   '<title>       </title>' + Chr(13) +
                                   '<style type="text/css">' + Chr(13) +
                                   '<!--' + Chr(13) +
                                   'h1	{text-align:center;' + Chr(13) +
                                   '	font-family:Arial, Helvetica, Sans-Serif;' + Chr(13) +
                                   '	}' + Chr(13) +
                                   '' + Chr(13) +
                                   'p	{text-indent:20px;' + Chr(13) +
                                   '	}' + Chr(13) +
                                   '-->' + Chr(13) +
                                   '</style>' + Chr(13) +
                                   '</head>' + Chr(13) +
                                   '<body bgcolor = "#ffffcc" text = "#000000">' + Chr(13) +
                                   '<h1>Hello, World!</h1>' + Chr(13) +
                                   '</body>' + Chr(13) +
                                   '</html>';

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := HTMLIdx;
 CommTmp.SingleLineID := '';
 CommTmp.MultiLineIDStart := '<!--';
 CommTmp.MultiLineIDEnd := '-->';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultVBAttribs;
Begin
 //Comments
 SetAttributeOn(VBIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(VBIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(VBIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(VBIdx, 5, clNone, ColorString, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := VBIdx;
 CommTmp.SingleLineID := '''';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultPASAttribs;
Begin
 UndefinedSampleSource[PASIdx] := '//Test code' + Chr(13) +
                                  'Program AddNums(output);' + Chr(13) +
                                  ' var x,y,z:integer;' + Chr(13) +
                                  'begin' + Chr(13) +
                                  '' + Chr(13) +
                                  '    x:=10;' + Chr(13) +
                                  '    y:=25;' + Chr(13) +
                                  '    z:=x+y;' + Chr(13) +
                                  '    writeln(''The sum of x+y is '', z);' + Chr(13) +
                                  'end.';

 //Comments
 SetAttributeOn(PASIdx, 2, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(PASIdx, 6, clNone, ColorNumber, []);

 If SyntaxSchemeList[PASIdx].AttrCount < 12 Then
  Begin
   //Reserved Word
   SetAttributeOn(PASIdx, 7, clNone, ColorReservedWord, [fsBold]);
   //String
   SetAttributeOn(PASIdx, 9, clNone, ColorString, []);
   //Symbol
   SetAttributeOn(PASIdx, 10, clNone, ColorSymbol, []);
  end
 Else
  Begin
   //Reserved Word
   SetAttributeOn(PASIdx, 8, clNone, ColorReservedWord, [fsBold]);
   //String
   SetAttributeOn(PASIdx, 10, clNone, ColorString, []);
   //Symbol
   SetAttributeOn(PASIdx, 11, clNone, ColorSymbol, []);
  end;

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := PASIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '(*';
 CommTmp.MultiLineIDEnd := '*)';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultJavaAttribs;
Begin
 //Comments
 SetAttributeOn(JAVAIdx, 1, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(JAVAIdx, 5, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(JAVAIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(JAVAIdx, 8, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(JAVAIdx, 9, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := JAVAIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultJSAttribs;
Begin
 If SyntaxSchemeList[JSIdx].AttrCount > 9 Then
  Begin
   //Comments
   SetAttributeOn(JSIdx, 1, clNone, ColorComment, [fsItalic]);
   //Numbers
   SetAttributeOn(JSIdx, 5, clNone, ColorNumber, []);
   //Reserved Word
   SetAttributeOn(JSIdx, 6, clNone, ColorReservedWord, [fsBold]);
   //String
   SetAttributeOn(JSIdx, 8, clNone, ColorString, []);
   //Symbol
   SetAttributeOn(JSIdx, 9, clNone, ColorSymbol, []);
  end
 Else
  Begin
   //Comments
   SetAttributeOn(JSIdx, 0, clNone, ColorComment, [fsItalic]);
   //Numbers
   SetAttributeOn(JSIdx, 4, clNone, ColorNumber, []);
   //Reserved Word
   SetAttributeOn(JSIdx, 5, clNone, ColorReservedWord, [fsBold]);
   //String
   SetAttributeOn(JSIdx, 7, clNone, ColorString, []);
   //Symbol
   SetAttributeOn(JSIdx, 8, clNone, ColorSymbol, []);
  end;

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := JSIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultPERLAttribs;
Begin
 UndefinedSampleSource[PERLIdx] := '#!/usr/bin/perl' + Chr(13) +
                                   'use strict;' + Chr(13) +
                                   'use warnings;' + Chr(13) +
                                   '' + Chr(13) +
                                   'use Path::Tiny;' + Chr(13) +
                                   'use autodie; # die if problem reading or writing a file' + Chr(13) +
                                   '' + Chr(13) +
                                   'my $dir = path("/tmp"); # /tmp' + Chr(13) +
                                   '' + Chr(13) +
                                   'my $file = $dir->child("file.txt"); # /tmp/file.txt' + Chr(13) +
                                   '' + Chr(13) +
                                   '# Get a file_handle (IO::File object) you can write to' + Chr(13) +
                                   '# with a UTF-8 encoding layer' + Chr(13) +
                                   'my $file_handle = $file->openw_utf8();' + Chr(13) +
                                   '' + Chr(13) +
                                   'my @list = (''a'', ''list'', ''of'', ''lines'');' + Chr(13) +
                                   '' + Chr(13) +
                                   'foreach my $line ( @list ) {' + Chr(13) +
                                   '    # Add the line to the file' + Chr(13) +
                                   '    $file_handle->print($line . "\n");' + Chr(13) +
                                   '}';
 //Comments
 SetAttributeOn(PERLIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(PERLIdx, 3, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(PERLIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(PERLIdx, 8, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(PERLIdx, 9, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := PERLIdx;
 CommTmp.SingleLineID := '#';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultXMLAttribs;
Begin
 //Attribute name
 SetAttributeOn(XMLIdx, 0, clNone, ColorReservedWord, []);
 //Attribute Value
 SetAttributeOn(XMLIdx, 1, clNone, MainForeground, [fsBold]);
 //CData
 SetAttributeOn(XMLIdx, 2, clNone, ColorPreProcessor, []);
 //Comments
 SetAttributeOn(XMLIdx, 3, clNone, ColorComment, [fsItalic]);
 //DocType
 SetAttributeOn(XMLIdx, 4, clNone, ColorNumber, []);
 //Element name
 SetAttributeOn(XMLIdx, 5, clNone, ColorReservedWord, [fsBold]);
 //Entity
 SetAttributeOn(XMLIdx, 6, clNone, ColorNumber, [fsBold]);
 //Namespace Atribute name
 SetAttributeOn(XMLIdx, 7, clNone, ColorReservedWord, []);
 //Namespace Atribute value
 SetAttributeOn(XMLIdx, 8, clNone, ColorReservedWord, [fsBold]);
 //Processing
 SetAttributeOn(XMLIdx, 9, clNone, ColorComment, [fsBold]);
 //Symbol
 SetAttributeOn(XMLIdx, 10, clNone, ColorSymbol, []);
 //String
 SetAttributeOn(XMLIdx, 11, clNone, ColorString, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := XMLIdx;
 CommTmp.SingleLineID := '';
 CommTmp.MultiLineIDStart := '<!--';
 CommTmp.MultiLineIDEnd := '-->';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultUSSAttribs;
Begin
 //Comments
 SetAttributeOn(USSIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(USSIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(USSIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //Second Reserved Word
 SetAttributeOn(USSIdx, 4, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(USSIdx, 6, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(USSIdx, 7, clNone, ColorSymbol, []);
 //Variable
 SetAttributeOn(USSIdx, 8, clNone, MainForeground, []);

 UndefinedSampleSource[USSIdx] := '#!/bin/sh' + Chr(13) +
                                  '' + Chr(13) +
                                  'a=0' + Chr(13) +
                                  '' + Chr(13) +
                                  'while [ $a -lt 10 ]' + Chr(13) +
                                  'do' + Chr(13) +
                                  '   echo $a' + Chr(13) +
                                  '   if [ $a -eq 5 ]' + Chr(13) +
                                  '   then' + Chr(13) +
                                  '      break' + Chr(13) +
                                  '   fi' + Chr(13) +
                                  '   a=`expr $a + 1`' + Chr(13) +
                                  'done';

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := USSIdx;
 CommTmp.SingleLineID := '#';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultPHPAttribs;
Begin
 //Comments
 SetAttributeOn(PHPIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(PHPIdx, 3, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(PHPIdx, 4, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(PHPIdx, 6, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(PHPIdx, 7, clNone, ColorSymbol, []);
 //Variable
 SetAttributeOn(PHPIdx, 8, clNone, MainForeground, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := PHPIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultTeXAttribs;
Begin
 //Comments
 SetAttributeOn(TeXIdx, 0, clNone, ColorComment, [fsItalic]);
 //Math mode
 SetAttributeOn(TeXIdx, 1, clNone, MainForeground, [fsBold]);
 //Round Bracket
 SetAttributeOn(TeXIdx, 2, clNone, ColorSymbol, []);
 //Space
 SetAttributeOn(TeXIdx, 3, clNone, clNone, []);
 //Square Bracket
 SetAttributeOn(TeXIdx, 4, clNone, ColorSymbol, []);
 //TeX Command
 SetAttributeOn(TeXIdx, 5, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(TeXIdx, 6, clNone, ColorString, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := TeXIdx;
 CommTmp.SingleLineID := '%';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultSQLAttribs;
Begin
 //Comments
 SetAttributeOn(SQLIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(SQLIdx, 6, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(SQLIdx, 7, clNone, ColorReservedWord, [fsBold]);
 //Reserved Word2
 SetAttributeOn(SQLIdx, 8, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(SQLIdx, 11, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(SQLIdx, 12, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := SQLIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultIniAttribs;
Begin
 //Comments
 SetAttributeOn(IniIdx, 0, clNone, ColorComment, [fsItalic]);
 //Key
 SetAttributeOn(IniIdx, 1, clNone, ColorReservedWord, [fsBold]);
 //Numbers
 SetAttributeOn(IniIdx, 2, clNone, ColorNumber, []);
 //Sections
 SetAttributeOn(IniIdx, 3, clNone, ColorPreProcessor, []);
 //String
 SetAttributeOn(IniIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(IniIdx, 6, clNone, ColorSymbol, []);
 //Text
 SetAttributeOn(IniIdx, 7, clNone, ColorString, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := IniIdx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultPoAttribs;
Begin
 //Comments
 SetAttributeOn(PoIdx, 0, clNone, ColorComment, [fsItalic]);
 //Flags
 SetAttributeOn(PoIdx, 1, clNone, ColorPreProcessor, []);
 //Key
 SetAttributeOn(PoIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //Previous value
 SetAttributeOn(PoIdx, 4, clNone, ColorSymbol, [fsItalic]);
 //String
 SetAttributeOn(PoIdx, 6, clNone, ColorString, []);
 //Text
 SetAttributeOn(PoIdx, 7, clNone, ColorString, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := PoIdx;
 CommTmp.SingleLineID := '#.';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultPyTAttribs;
Begin
 //Comments
 SetAttributeOn(PyTIdx, 0, clNone, ColorComment, [fsItalic]);
 //Documentation
 SetAttributeOn(PyTIdx, 1, clNone, ColorComment, [fsItalic]);
 //Float
 SetAttributeOn(PyTIdx, 2, clNone, ColorNumber, []);
 //Hex
 SetAttributeOn(PyTIdx, 3, clNone, ColorNumber, []);
 //Non reserved word
 SetAttributeOn(PyTIdx, 5, clNone, MainForeground, []);
 //Numbers
 SetAttributeOn(PyTIdx, 6, clNone, ColorNumber, []);
 //Octal
 SetAttributeOn(PyTIdx, 7, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(PyTIdx, 8, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(PyTIdx, 10, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(PyTIdx, 11, clNone, ColorSymbol, []);
 //System func
 SetAttributeOn(PyTIdx, 13, clNone, MainForeground, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := PyTIdx;
 CommTmp.SingleLineID := '#';
 CommTmp.MultiLineIDStart := '"""';
 CommTmp.MultiLineIDEnd := '"""';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultFortranAttribs;
Begin
 (*
 UndefinedSampleSource[FORIdx] := '*     cmplxd.f (FORTRAN 77)' + Chr(13) +
                                  '*     Demonstration of COMPLEX numbers' + Chr(13) +
                                  '*' + Chr(13) +
                                  '*     Prints the values of e ** (j * i * pi / 4) for i = 0, 1, 2, ..., 7' + Chr(13) +
                                  '*         where j is the imaginary number sqrt(-1)' + Chr(13) +
                                  '' + Chr(13) +
                                  '      PROGRAM CMPLXD' + Chr(13) +
                                  '        IMPLICIT COMPLEX(X)' + Chr(13) +
                                  '        PARAMETER (PI = 3.141592653589793, XJ = (0, 1))' + Chr(13) +
                                  '        DO 1, I = 0, 7' + Chr(13) +
                                  '          X = EXP(XJ * I * PI / 4)' + Chr(13) +
                                  '          IF (AIMAG(X).LT.0) THEN' + Chr(13) +
                                  '            PRINT 2, ''e**(j*'', I, ''*pi/4) = '', REAL(X), '' - j'',-AIMAG(X)' + Chr(13) +
                                  '          ELSE' + Chr(13) +
                                  '            PRINT 2, ''e**(j*'', I, ''*pi/4) = '', REAL(X), '' + j'', AIMAG(X)' + Chr(13) +
                                  '          END IF' + Chr(13) +
                                  '    2     FORMAT (A, I1, A, F10.7, A, F9.7)' + Chr(13) +
                                  '    1     CONTINUE' + Chr(13) +
                                  '        STOP' + Chr(13) +
                                  '      END';
 *)
end;

procedure CreateDefaultCSAttribs;
Begin
 //Comments
 SetAttributeOn(CSIdx, 1, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(CSIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(CSIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(CSIdx, 8, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(CSIdx, 9, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := CSIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultHaskellAttribs;
Begin
 //Comments
 SetAttributeOn(HSKIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(HSKIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(HSKIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(HSKIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(HSKIdx, 6, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := HSKIdx;
 CommTmp.SingleLineID := '--';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultRubyAttribs;
Begin
 //Comments
 SetAttributeOn(RubyIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(RubyIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(RubyIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //Second Reserved Word
 SetAttributeOn(RubyIdx, 4, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(RubyIdx, 6, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(RubyIdx, 7, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := RubyIdx;
 CommTmp.SingleLineID := '#';
 CommTmp.MultiLineIDStart := '=begin';
 CommTmp.MultiLineIDEnd := '=end';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultx86Attribs;
Begin
 //Comments
 SetAttributeOn(x86Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(x86Idx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(x86Idx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(x86Idx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(x86Idx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(x86Idx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := x86Idx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultZ80Attribs;
Begin
 //Comments
 SetAttributeOn(Z80Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(Z80Idx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(Z80Idx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(Z80Idx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(Z80Idx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(Z80Idx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := Z80Idx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultM68000Attribs;
Begin
 //Comments
 SetAttributeOn(M68Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(M68Idx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(M68Idx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(M68Idx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(M68Idx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(M68Idx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := M68Idx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefault8051Attribs;
Begin
 //Comments
 SetAttributeOn(M8051Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(M8051Idx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(M8051Idx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(M8051Idx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(M8051Idx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(M8051Idx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := M8051Idx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultP18FAttribs;
Begin
 //Comments
 SetAttributeOn(P18FIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(P18FIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(P18FIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(P18FIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(P18FIdx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(P18FIdx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := P18FIdx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultTCLTKFAttribs;
Begin
 //Comments
 SetAttributeOn(TCLIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(TCLIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(TCLIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(TCLIdx, 6, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(TCLIdx, 7, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := TCLIdx;
 CommTmp.SingleLineID := '#';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultInnoAttribs;
Begin
 //Comments
 SetAttributeOn(InnoIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(InnoIdx, 1, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(InnoIdx, 4, clNone, ColorNumber, []);
 //PreProcessor
 SetAttributeOn(InnoIdx, 5, clNone, ColorPreProcessor, []);
 //Reserved Word
 SetAttributeOn(InnoIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //Section
 SetAttributeOn(InnoIdx, 7, clNone, ColorPreProcessor, [fsBold]);
 //String
 SetAttributeOn(InnoIdx, 9, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(InnoIdx, 10, clNone, ColorSymbol, []);


 UndefinedSampleSource[InnoIdx] := '[Setup]' + Chr(13) +
                                   'AppName=My Program' + Chr(13) +
                                   'AppVersion=1.5' + Chr(13) +
                                   'DefaultDirName={code:MyConst}\My Program' + Chr(13) +
                                   'DefaultGroupName=My Program' + Chr(13) +
                                   'UninstallDisplayIcon={app}\MyProg.exe' + Chr(13) +
                                   'InfoBeforeFile=Readme.txt' + Chr(13) +
                                   'OutputDir=userdocs:Inno Setup Examples Output' + Chr(13) +
                                   '' + Chr(13) +
                                   '[Files]' + Chr(13) +
                                   'Source: "MyProg.exe"; DestDir: "{app}"; Check: MyProgCheck; BeforeInstall: BeforeMyProgInstall(''MyProg.exe''); AfterInstall: AfterMyProgInstall(''MyProg.exe'')' + Chr(13) +
                                   'Source: "MyProg.chm"; DestDir: "{app}"; Check: MyProgCheck; BeforeInstall: BeforeMyProgInstall(''MyProg.chm''); AfterInstall: AfterMyProgInstall(''MyProg.chm'')' + Chr(13) +
                                   'Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme' + Chr(13) +
                                   '' + Chr(13) +
                                   '[Icons]' + Chr(13) +
                                   'Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"';

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := InnoIdx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefault68HC11Attribs;
Begin
 //Comments
 SetAttributeOn(HC86Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(HC86Idx, 3, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(HC86Idx, 5, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(HC86Idx, 7, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(HC86Idx, 8, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := HC86Idx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultATMegaAttribs;
Begin
 //Comments
 SetAttributeOn(ATMegaIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(ATMegaIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(ATMegaIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(ATMegaIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(ATMegaIdx, 6, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := ATMegaIdx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultNMos6585IdxAttribs;
Begin
 //Comments
 SetAttributeOn(NMos6585Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(NMos6585Idx, 3, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(NMos6585Idx, 4, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(NMos6585Idx, 6, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(NMos6585Idx, 7, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := NMos6585Idx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefault6800Attribs;
Begin
 //Comments
 SetAttributeOn(x6800IDX, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(x6800IDX, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(x6800IDX, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(x6800IDX, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(x6800IDX, 6, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := x6800IDX;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefault6809Attribs;
Begin
 //Comments
 SetAttributeOn(x6809Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(x6809Idx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(x6809Idx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(x6809Idx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(x6809Idx, 6, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := x6809Idx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultArmV7Attribs;
Begin
 //Comments
 SetAttributeOn(ArmV7Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(ArmV7Idx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(ArmV7Idx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(ArmV7Idx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(ArmV7Idx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(ArmV7Idx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := ArmV7Idx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultP16Attribs;
Begin
 //Comments
 SetAttributeOn(P16Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(P16Idx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(P16Idx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(P16Idx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(P16Idx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(P16Idx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := P16Idx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultLUAAttribs;
Begin
 //Comments
 SetAttributeOn(LUAIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(LUAIdx, 3, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(LUAIdx, 4, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(LUAIdx, 6, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(LUAIdx, 7, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := LUAIdx;
 CommTmp.SingleLineID := '--';
 CommTmp.MultiLineIDStart := '--[[';
 CommTmp.MultiLineIDEnd := '--]]';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultPrologAttribs;
Begin
 //Comments
 SetAttributeOn(PrologIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(PrologIdx, 3, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(PrologIdx, 4, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(PrologIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(PrologIdx, 6, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := PrologIdx;
 CommTmp.SingleLineID := '%';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultVHDLAttribs;
Begin
 //Comments
 SetAttributeOn(VHDLIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(VHDLIdx, 3, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(VHDLIdx, 4, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(VHDLIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(VHDLIdx, 6, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := VHDLIdx;
 CommTmp.SingleLineID := '--';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultDAttribs;
Begin
 //Comments
 SetAttributeOn(DIdx, 2, clNone, ColorComment, [fsItalic]);
 //Identifier
 SetAttributeOn(DIdx, 5, clNone, MainForeground, []);
 //Numbers
 SetAttributeOn(DIdx, 7, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(DIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //Special word
 SetAttributeOn(DIdx, 8, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(DIdx, 9, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(DIdx, 10, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := DIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultPIC32MXAttribs;
Begin
 //Comments
 SetAttributeOn(PIC32MXIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(PIC32MXIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(PIC32MXIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(PIC32MXIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(PIC32MXIdx, 6, clNone, ColorSymbol, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := PIC32MXIdx;
 CommTmp.SingleLineID := ';';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultST6Attribs;
Begin
 //Comments
 SetAttributeOn(ST6Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(ST6Idx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(ST6Idx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(ST6Idx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(ST6Idx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(ST6Idx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := ST6Idx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultST7Attribs;
Begin
 //Comments
 SetAttributeOn(ST7Idx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(ST7Idx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(ST7Idx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(ST7Idx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(ST7Idx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(ST7Idx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := ST7Idx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultADAAttribs;
Begin
 //Comments
 SetAttributeOn(ADAIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(ADAIdx, 1, clNone, ColorPreProcessor, []);
 //Numbers
 SetAttributeOn(ADAIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(ADAIdx, 5, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(ADAIdx, 7, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(ADAIdx, 8, clNone, ColorSymbol, []);
 //System values
 //SetAttributeOn(ADAIdx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := ADAIdx;
 CommTmp.SingleLineID := '--';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultASAttribs;
Begin
 //Comments
 SetAttributeOn(ACSIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(ACSIdx, 1, clNone, ColorPreProcessor, []);
 //Numbers
 SetAttributeOn(ACSIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(ACSIdx, 5, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(ACSIdx, 7, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(ACSIdx, 8, clNone, ColorSymbol, []);
 //System values
 //SetAttributeOn(ADAIdx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := ACSIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultFRBAttribs;
Begin
 //Comments
 SetAttributeOn(FRBIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(FRBIdx, 1, clNone, ColorPreProcessor, []);
 //Numbers
 SetAttributeOn(FRBIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(FRBIdx, 5, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(FRBIdx, 7, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(FRBIdx, 8, clNone, ColorSymbol, []);
 //System values
 //SetAttributeOn(ADAIdx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := FRBIdx;
 CommTmp.SingleLineID := '''';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultVerilogAttribs;
Begin
 //Comments
 SetAttributeOn(VRLIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(VRLIdx, 1, clNone, ColorPreProcessor, []);
 //Numbers
 SetAttributeOn(VRLIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(VRLIdx, 5, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(VRLIdx, 7, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(VRLIdx, 8, clNone, ColorSymbol, []);
 //System values
 //SetAttributeOn(ADAIdx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := VRLIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultGOAttribs;
Begin
 //Comments
 SetAttributeOn(GOIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(GOIdx, 1, clNone, ColorPreProcessor, []);
 //Numbers
 SetAttributeOn(GOIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(GOIdx, 5, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(GOIdx, 7, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(GOIdx, 8, clNone, ColorSymbol, []);
 //System values
 //SetAttributeOn(ADAIdx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := GOIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultTMS9900Attribs;
Begin
 //Comments
 SetAttributeOn(TMSIdx, 0, clNone, ColorComment, [fsItalic]);
 //Numbers
 SetAttributeOn(TMSIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(TMSIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(TMSIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(TMSIdx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(TMSIdx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := TMSIdx;
 CommTmp.SingleLineID := '*';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultPowerShellAttribs;
Begin
 //Comments
 SetAttributeOn(PWSHIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(PWSHIdx, 1, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(PWSHIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(PWSHIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(PWSHIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(PWSHIdx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(PWSHIdx, 7, clNone, clBlack, [fsBold]);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := PWSHIdx;
 CommTmp.SingleLineID := '#';
 CommTmp.MultiLineIDStart := '<#';
 CommTmp.MultiLineIDEnd := '#>';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultCMakeAttribs;
Begin
 //Comments
 SetAttributeOn(CMakeIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(CMakeIdx, 1, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(CMakeIdx, 2, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(CMakeIdx, 3, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(CMakeIdx, 5, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(CMakeIdx, 6, clNone, ColorSymbol, []);
 //System values
 SetAttributeOn(CMakeIdx, 7, clNone, clBlack, [fsBold]);
 //Variable
 SetAttributeOn(CMakeIdx, 9, clNone, clBlack, [fsItalic]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := CMakeIdx;
 CommTmp.SingleLineID := '#';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultGroovyAttribs;
Begin
 //Comments
 SetAttributeOn(GroovyIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(GroovyIdx, 1, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(GroovyIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(GroovyIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(GroovyIdx, 8, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(GroovyIdx, 9, clNone, ColorSymbol, []);
 //Primitive
 SetAttributeOn(GroovyIdx, 5, clNone, clBlack, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := GroovyIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultRustAttribs;
Begin
 //Comments
 SetAttributeOn(RustIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(RustIdx, 1, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(RustIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(RustIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(RustIdx, 8, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(RustIdx, 9, clNone, ColorSymbol, []);
 //Primitive
 SetAttributeOn(RustIdx, 5, clNone, ColorReservedWord, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := RustIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultAwkAttribs;
Begin
 //Comments
 SetAttributeOn(AwkIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(AwkIdx, 1, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(AwkIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(AwkIdx, 5, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(AwkIdx, 8, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(AwkIdx, 9, clNone, ColorSymbol, []);
 //Primitive
 SetAttributeOn(AwkIdx, 7, clNone, ColorReservedWord, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := AwkIdx;
 CommTmp.SingleLineID := '#';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultHaxeAttribs;
Begin
 //Comments
 SetAttributeOn(HaxeIdx, 0, clNone, ColorComment, [fsItalic]);
 //Directive
 SetAttributeOn(HaxeIdx, 1, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(HaxeIdx, 4, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(HaxeIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(HaxeIdx, 8, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(HaxeIdx, 9, clNone, ColorSymbol, []);
 //Preprocessor
 SetAttributeOn(HaxeIdx, 5, clNone, ColorPreProcessor, []);
 //Typed word
 SetAttributeOn(HaxeIdx, 10, clNone, ColorReservedWord, [fsBold]);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := HaxeIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultFSharpAttribs;
Begin
 //Comments
 SetAttributeOn(FSharpIdx, 0, clNone, ColorComment, [fsItalic]);
 //DataType
 SetAttributeOn(FSharpIdx, 1, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(FSharpIdx, 5, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(FSharpIdx, 7, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(FSharpIdx, 9, clNone, ColorString, []);
 //Symbol
 SetAttributeOn(FSharpIdx, 10, clNone, ColorSymbol, []);
 //Preprocessor
 SetAttributeOn(FSharpIdx, 6, clNone, ColorPreProcessor, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := FSharpIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '(*';
 CommTmp.MultiLineIDEnd := '*)';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultRAttribs;
Begin
 //Comments
 SetAttributeOn(RIdx, 0, clNone, ColorComment, [fsItalic]);
 //Functions
 SetAttributeOn(RIdx, 3, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(RIdx, 5, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(RIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(RIdx, 8, clNone, ColorString, []);
 //Symbol
 //SetAttributeOn(RIdx, 10, clNone, ColorSymbol, []);
 //Preprocessor
 //SetAttributeOn(RIdx, 6, clNone, ColorPreProcessor, []);

 CommTmp.CommentType := CommentTypeSingleLine;
 CommTmp.LanguageID := RIdx;
 CommTmp.SingleLineID := '#';
 CommTmp.MultiLineIDStart := '';
 CommTmp.MultiLineIDEnd := '';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultVAttribs;
Begin
 //Comments
 SetAttributeOn(VIdx, 0, clNone, ColorComment, [fsItalic]);
 //DataType
 SetAttributeOn(VIdx, 1, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(VIdx, 5, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(VIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(VIdx, 8, clNone, ColorString, []);
 //Symbol
 //SetAttributeOn(RIdx, 10, clNone, ColorSymbol, []);
 //Preprocessor
 //SetAttributeOn(RIdx, 6, clNone, ColorPreProcessor, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := VIdx;
 CommTmp.SingleLineID := '//';
 CommTmp.MultiLineIDStart := '/*';
 CommTmp.MultiLineIDEnd := '*/';

 CommentDataListAdd(CommTmp);
end;

procedure CreateDefaultMatlabAttribs;
Begin
 //Comments
 SetAttributeOn(MatlabIdx, 0, clNone, ColorComment, [fsItalic]);
 //Functions
 SetAttributeOn(MatlabIdx, 3, clNone, MainForeground, [fsBold]);
 //Numbers
 SetAttributeOn(MatlabIdx, 5, clNone, ColorNumber, []);
 //Reserved Word
 SetAttributeOn(MatlabIdx, 6, clNone, ColorReservedWord, [fsBold]);
 //String
 SetAttributeOn(MatlabIdx, 8, clNone, ColorString, []);
 //Symbol
 //SetAttributeOn(RIdx, 10, clNone, ColorSymbol, []);
 //Preprocessor
 //SetAttributeOn(RIdx, 6, clNone, ColorPreProcessor, []);

 CommTmp.CommentType := CommentTypeMultiLine;
 CommTmp.LanguageID := MatlabIdx;
 CommTmp.SingleLineID := '%';
 CommTmp.MultiLineIDStart := '%{';
 CommTmp.MultiLineIDEnd := '%}';

 CommentDataListAdd(CommTmp);
end;

procedure BuildDefaultAttribs;
begin
 CreateDefaultDOSAttribs;
 CreateDefaultCPPAttribs;
 CreateDefaultCssAttribs;
 CreateDefaultHtmlAttribs;
 CreateDefaultVBAttribs;
 CreateDefaultPASAttribs;
 CreateDefaultJavaAttribs;
 CreateDefaultJSAttribs;
 CreateDefaultPERLAttribs;
 CreateDefaultXMLAttribs;
 CreateDefaultUSSAttribs;
 CreateDefaultPHPAttribs;
 CreateDefaultTeXAttribs;
 CreateDefaultSQLAttribs;
 CreateDefaultIniAttribs;
 CreateDefaultPoAttribs;
 CreateDefaultPyTAttribs;
 CreateDefaultFortranAttribs;
 CreateDefaultCSAttribs;
 CreateDefaultHaskellAttribs;
 CreateDefaultRubyAttribs;
 CreateDefaultx86Attribs;
 CreateDefaultZ80Attribs;
 CreateDefaultM68000Attribs;
 CreateDefault8051Attribs;
 CreateDefaultP18FAttribs;
 CreateDefaultTCLTKFAttribs;
 CreateDefaultInnoAttribs;
 CreateDefault68HC11Attribs;
 CreateDefaultATMegaAttribs;
 CreateDefaultNMos6585IdxAttribs;
 CreateDefault6800Attribs;
 CreateDefault6809Attribs;
 CreateDefaultArmV7Attribs;
 CreateDefaultP16Attribs;
 CreateDefaultLUAAttribs;
 CreateDefaultPrologAttribs;
 CreateDefaultVHDLAttribs;
 CreateDefaultDAttribs;
 CreateDefaultPIC32MXAttribs;
 CreateDefaultST6Attribs;
 CreateDefaultST7Attribs;
 CreateDefaultADAAttribs;
 CreateDefaultASAttribs;
 CreateDefaultFRBAttribs;
 CreateDefaultVerilogAttribs;
 CreateDefaultGOAttribs;
 CreateDefaultTMS9900Attribs;
 CreateDefaultPowerShellAttribs;
 CreateDefaultCMakeAttribs;
 CreateDefaultGroovyAttribs;
 CreateDefaultRustAttribs;
 CreateDefaultAwkAttribs;
 CreateDefaultHaxeAttribs;
 CreateDefaultFSharpAttribs;
 CreateDefaultRAttribs;
 CreateDefaultVAttribs;
 CreateDefaultMatlabAttribs;
end;

procedure BuildListASMWordFinder;
begin

end;

procedure SelectTheme(ThemeIdx : Integer; WSynList : TSyntaxList);
 Var I, A : Integer;
begin
 MainForeground := PiNoteThemes[ThemeIdx].MainForeground;
 MainBackground := PiNoteThemes[ThemeIdx].MainBackground;
 LineForeground := PiNoteThemes[ThemeIdx].LineForeground;
 LineBackground := PiNoteThemes[ThemeIdx].LineBackground;
 ColorComment := PiNoteThemes[ThemeIdx].ColorComment;
 ColorNumber := PiNoteThemes[ThemeIdx].ColorNumber;
 ColorPreProcessor := PiNoteThemes[ThemeIdx].ColorPreProcessor;
 ColorReservedWord := PiNoteThemes[ThemeIdx].ColorReservedWord;
 ColorString := PiNoteThemes[ThemeIdx].ColorString;
 ColorSymbol := PiNoteThemes[ThemeIdx].ColorSymbol;

 BuildDefaultAttribs;

 For I := 0 To WSynList.Count - 1 Do
  For A := 0 To WSynList[I].AttrCount - 1 Do
   Begin
    If (WSynList[I].Attribute[A].Foreground = clNone) Or
       (WSynList[I].Attribute[A].Foreground = clBlack) Then
     WSynList[I].Attribute[A].Foreground := MainForeground;

    If WSynList[I].Attribute[A].StoredName = SYNS_XML_AttrIdentifier Then
     WSynList[I].Attribute[A].Foreground := MainForeground;

    If WSynList[I].Attribute[A].StoredName = SYNS_XML_AttrDirective Then
     WSynList[I].Attribute[A].Foreground := ColorPreProcessor;

    If WSynList[I].Attribute[A].StoredName = SYNS_XML_AttrProcedureHeaderName Then
     WSynList[I].Attribute[A].Foreground := MainForeground;

    If WSynList[I].Attribute[A].StoredName = SYNS_XML_AttrCaseLabel Then
     WSynList[I].Attribute[A].Foreground := MainForeground;
   end;
end;

procedure ClearCommentDataList;
begin
 SetLength(CommentDataList, 0);
end;

procedure CommentDataListAdd(Val: TCommentData);
 Var Pos : Word;
begin
 Pos := Length(CommentDataList);

 SetLength(CommentDataList, Pos + 1);

 CommentDataList[Pos] := Val;
end;

end.
