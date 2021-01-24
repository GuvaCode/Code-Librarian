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
unit uSyntaxList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fgl, SynEditHighlighter, SynHighlighterCpp, Menus,
  SynHighlighterBat, SynHighlighterVB, SynHighlighterCss, SynHighlighterHTML,
  SynHighlighterPas, SynHighlighterJava, SynHighlighterJScript,
  SynHighlighterPerl, SynHighlighterXML, synhighlighterunixshellscript,
  SynHighlighterPHP, SynHighlighterTeX, SynHighlighterSQL, SynHighlighterPython,
  SynHighlighterIni, umysynhighlighterpo, MySEHighlighterFortran,
  MySynHighlighterCS, MySynHighlighterCobol, MySynHighlighterHaskell,
  MySynHighlighterRuby, MySynHighlighterX86Asm, MySEHighlighterZ80,
  MySEHighlighter68K, MySEHighlighter8051, MySEHighlighter18F,
  MySynHighlighterTclTk, MySynHighlighterInno, MySynHighlighterHC11, Graphics,
  MySEHighlighterATMega, u6510Asm_Highlighter, u6800Asm_Highlighter,
  u6809Asm_Highlighter, uArmV7Asm_Highlighter, MySEHighlighter16F,
  uSynHighlighterLua, uSynHighlighterProlog, uSynHighlighterVhdl,
  uSynHighlighterD, MySEHighlighterPIC32MXAsm, MySEHighlighterST6,
  MySEHighlighterST7, MySEHighlighterADA, MySEHighlighterActionScript,
  MySEHighlighterFreeBasic, MySEHighlighterVerilog, MySEHighlighterGO,
  MySEHighlighterTMS9900, MySEHighlighterPowerShell, MySEHighlighterCMake,
  MySEHighlighterGroovy, MySEHighlighterRust, MySEHighlighterAwk,
  MySEHighlighterHaxe, MySEHighlighterFSharp, MySEHighlighterR,
  MySEHighlighterV, MySEHighlighterMatlab;

Type
  TSyntaxList             = Specialize TFPGObjectList <TSynCustomHighlighter>;

Var
  SyntaxSchemeList        : TSyntaxList;
  OldSyntaxSchemeList     : TSyntaxList;
  SyntaxSchemeNameList    : TStringList;
  SyntaxSchemeNameDiv     : TStringList;

Procedure CreateSyntaxSchemeList;

Function GetSyntaxSchemeIndex(Language : String) : Integer;
Procedure DestroySyntaxSchemeList;

Procedure StoreSyntaxSchemeToMemory;
Procedure GetSyntaxSchemeFromMemory;

Procedure SetAttributeOn(SyntaxID, AttribID : Integer; SectionB, SectionF : TColor;
                         FontStyle : TFontStyles);

implementation

Uses {uMain,} uSyntaxDefault, Dialogs;

procedure CreateSyntaxSchemeList;
 Var Ind : Integer;
begin
 SyntaxSchemeList := TSyntaxList.Create();
 OldSyntaxSchemeList := TSyntaxList.Create();
 SyntaxSchemeNameList := TStringList.Create;
 SyntaxSchemeNameList.Sorted := True;

 SyntaxSchemeNameDiv := TStringList.Create;
 SyntaxSchemeNameDiv.Sorted := True;
 SyntaxSchemeNameDiv.Duplicates := dupIgnore;

 SyntaxSchemeList.Add(TSynBatSyn.Create(Nil));        //0- MSDos Bat File
 OldSyntaxSchemeList.Add(TSynBatSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynCppSyn.Create(Nil));        //1- Cpp
 OldSyntaxSchemeList.Add(TSynCppSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynCssSyn.Create(Nil));        //2- Css
 OldSyntaxSchemeList.Add(TSynCssSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynHTMLSyn.Create(Nil));       //3- HTML
 OldSyntaxSchemeList.Add(TSynHTMLSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynVBSyn.Create(Nil));         //4- Visual Basic
 OldSyntaxSchemeList.Add(TSynVBSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynPasSyn.Create(Nil));        //5- Pascal
 OldSyntaxSchemeList.Add(TSynPasSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynJavaSyn.Create(Nil));       //6- Java
 OldSyntaxSchemeList.Add(TSynJavaSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynJScriptSyn.Create(Nil));    //7- Javascript
 OldSyntaxSchemeList.Add(TSynJScriptSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynPerlSyn.Create(Nil));       //8- Perl
 OldSyntaxSchemeList.Add(TSynPerlSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynXMLSyn.Create(Nil));        //9- XML
 OldSyntaxSchemeList.Add(TSynXMLSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynUNIXShellScriptSyn.Create(Nil));        //10- Shell Unix
 OldSyntaxSchemeList.Add(TSynUNIXShellScriptSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynPHPSyn.Create(Nil));        //11- PHP
 OldSyntaxSchemeList.Add(TSynPHPSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynTeXSyn.Create(Nil));        //12- TeX
 OldSyntaxSchemeList.Add(TSynTeXSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynSQLSyn.Create(Nil));        //13- SQL
 OldSyntaxSchemeList.Add(TSynSQLSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynPythonSyn.Create(Nil));     //14- Python
 OldSyntaxSchemeList.Add(TSynPythonSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynIniSyn.Create(Nil));        //15- Ini
 OldSyntaxSchemeList.Add(TSynIniSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynPoSyn.Create(Nil));         //16- Po
 OldSyntaxSchemeList.Add(TSynPoSyn.Create(Nil));
 //SyntaxSchemeList.Add(TMySynFortranSyn.Create(Nil));  //17- Fortran*
 //OldSyntaxSchemeList.Add(TMySynFortranSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynCSSyn.Create(Nil));         //17- CS
 OldSyntaxSchemeList.Add(TSynCSSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynCobolSyn.Create(Nil));      //18- COBOL
 OldSyntaxSchemeList.Add(TSynCobolSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynHaskellSyn.Create(Nil));    //19- Haskell
 OldSyntaxSchemeList.Add(TSynHaskellSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynRubySyn.Create(Nil));       //20- Ruby
 OldSyntaxSchemeList.Add(TSynRubySyn.Create(Nil));
 SyntaxSchemeList.Add(TSynAsmSyn.Create(Nil));        //21- x86 Assembly
 OldSyntaxSchemeList.Add(TSynAsmSyn.Create(Nil));
 SyntaxSchemeList.Add(TMySynZ80Syn.Create(Nil));      //22- Z80 Assembly
 OldSyntaxSchemeList.Add(TMySynZ80Syn.Create(Nil));
 SyntaxSchemeList.Add(TMySyn68KSyn.Create(Nil));      //23- 68000 Assembly
 OldSyntaxSchemeList.Add(TMySyn68KSyn.Create(Nil));
 SyntaxSchemeList.Add(TMySyn8051Syn.Create(Nil));     //24- 8051 Assembly
 OldSyntaxSchemeList.Add(TMySyn8051Syn.Create(Nil));
 SyntaxSchemeList.Add(TMySynP18FSyn.Create(Nil));     //25- PIC18F Assembly
 OldSyntaxSchemeList.Add(TMySynP18FSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynTclTkSyn.Create(Nil));      //26- Tcl/Tk
 OldSyntaxSchemeList.Add(TSynTclTkSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynInnoSyn.Create(Nil));       //27- InnoSetup
 OldSyntaxSchemeList.Add(TSynInnoSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynHC11Syn.Create(Nil));       //28- 68HC11 Assembly
 OldSyntaxSchemeList.Add(TSynHC11Syn.Create(Nil));
 SyntaxSchemeList.Add(TMySynATMEGASyn.Create(Nil));   //29- ATMega Assembly
 OldSyntaxSchemeList.Add(TMySynATMEGASyn.Create(Nil));
 SyntaxSchemeList.Add(T6510AsmSyn.Create(Nil));       //30- 65xx/85xx Assembly
 OldSyntaxSchemeList.Add(T6510AsmSyn.Create(Nil));
 SyntaxSchemeList.Add(T6800AsmSyn.Create(Nil));       //31- Motorola 6800 Assembly
 OldSyntaxSchemeList.Add(T6800AsmSyn.Create(Nil));
 SyntaxSchemeList.Add(T6809AsmSyn.Create(Nil));       //32- Motorola 6809 Assembly
 OldSyntaxSchemeList.Add(T6809AsmSyn.Create(Nil));
 SyntaxSchemeList.Add(TArmV7AsmSyn.Create(Nil));      //33- Arm V7 Assembly
 OldSyntaxSchemeList.Add(TArmV7AsmSyn.Create(Nil));
 SyntaxSchemeList.Add(TMySynP16FSyn.Create(Nil));     //34- PIC16 Assembly
 OldSyntaxSchemeList.Add(TMySynP16FSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynLuaSyn.Create(Nil));        //35- Lua
 OldSyntaxSchemeList.Add(TSynLuaSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynPrologSyn.Create(Nil));     //36- Prolog
 OldSyntaxSchemeList.Add(TSynPrologSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynVhdlSyn.Create(Nil));       //37- VHDL
 OldSyntaxSchemeList.Add(TSynVhdlSyn.Create(Nil));
 SyntaxSchemeList.Add(TSynD2Syn.Create(Nil));         //38- D
 OldSyntaxSchemeList.Add(TSynD2Syn.Create(Nil));
 SyntaxSchemeList.Add(TMySynPIC32MXSyn.Create(Nil));   //39- PIC32MX Assembly
 OldSyntaxSchemeList.Add(TMySynPIC32MXSyn.Create(Nil));
 SyntaxSchemeList.Add(TMySynST6Syn.Create(Nil));       //40- ST6 Assembly
 OldSyntaxSchemeList.Add(TMySynST6Syn.Create(Nil));
 SyntaxSchemeList.Add(TMySynST7Syn.Create(Nil));       //41- ST7 Assembly
 OldSyntaxSchemeList.Add(TMySynST7Syn.Create(Nil));

 SyntaxSchemeList.Add(TMySEHighlighterADA.Create(Nil));    //42- ADA
 OldSyntaxSchemeList.Add(TMySEHighlighterADA.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterActionScript.Create(Nil));    //43- ActionScript
 OldSyntaxSchemeList.Add(TMySEHighlighterActionScript.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterFreeBasic.Create(Nil));      //44- FreeBasic
 OldSyntaxSchemeList.Add(TMySEHighlighterFreeBasic.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterVerilog.Create(Nil));      //45- Verilog
 OldSyntaxSchemeList.Add(TMySEHighlighterVerilog.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterGO.Create(Nil));      //46- Go
 OldSyntaxSchemeList.Add(TMySEHighlighterGO.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterTMS9900.Create(Nil));      //47- TMS9900
 OldSyntaxSchemeList.Add(TMySEHighlighterTMS9900.Create(Nil));
 SyntaxSchemeList.Add(TSEHighlighterPowerShell.Create(Nil));      //48- PowerShell
 OldSyntaxSchemeList.Add(TSEHighlighterPowerShell.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterCMake.Create(Nil));         //49- CMake
 OldSyntaxSchemeList.Add(TMySEHighlighterCMake.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterGroovy.Create(Nil));         //50- Groovy
 OldSyntaxSchemeList.Add(TMySEHighlighterGroovy.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterRust.Create(Nil));         //51- Rust
 OldSyntaxSchemeList.Add(TMySEHighlighterRust.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterAwk.Create(Nil));         //52- Awk
 OldSyntaxSchemeList.Add(TMySEHighlighterAwk.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterHaxe.Create(Nil));         //53- Haxe
 OldSyntaxSchemeList.Add(TMySEHighlighterHaxe.Create(Nil));
 //21/12/20
 SyntaxSchemeList.Add(TMySEHighlighterFSharp.Create(Nil));         //54- F#
 OldSyntaxSchemeList.Add(TMySEHighlighterFSharp.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterR.Create(Nil));              //55- R
 OldSyntaxSchemeList.Add(TMySEHighlighterR.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterV.Create(Nil));              //56- V
 OldSyntaxSchemeList.Add(TMySEHighlighterV.Create(Nil));
 SyntaxSchemeList.Add(TMySEHighlighterMatlab.Create(Nil));         //57- MATLAB
 OldSyntaxSchemeList.Add(TMySEHighlighterMatlab.Create(Nil));

 For Ind := 0 To SyntaxSchemeList.Count - 1 Do
  Begin
   SyntaxSchemeNameList.Add(SyntaxSchemeList[Ind].LanguageName);

   SyntaxSchemeNameDiv.Add(SyntaxSchemeList[Ind].LanguageName[1]);

   SyntaxSchemeList[Ind].Tag := 0;      //Non modificato
  end;

 BuildDefaultAttribs;
end;

Function GetSyntaxSchemeIndex(Language : String) : Integer;
 Var Ind : Integer;
     Tmp : String;
Begin
 Result := -1;

 For Ind := 0 To SyntaxSchemeList.Count - 1 Do
  Begin
   Tmp := SyntaxSchemeList[Ind].LanguageName;

   If Language = Tmp Then
    Begin
     Result := Ind;

     Break;
    end;
  End;
end;

Function GetNumberOfSchemeDiv(Const FirstChar : String) : Integer;
 Var I : Integer;
Begin
 Result := 0;

 For I := 0 To SyntaxSchemeNameList.Count - 1 Do
  If SyntaxSchemeNameList[I][1] = FirstChar Then
   Inc(Result);
end;



procedure DestroySyntaxSchemeList;
begin
 SyntaxSchemeNameList.Free;
 SyntaxSchemeNameDiv.Free;

 SyntaxSchemeList.Free;
 OldSyntaxSchemeList.Free;
end;

procedure StoreSyntaxSchemeToMemory;
 Var I, A : Integer;
begin
 For I := 0 To SyntaxSchemeList.Count - 1 Do
  For A := 0 To SyntaxSchemeList[I].AttrCount - 1 Do
   Begin
    OldSyntaxSchemeList[I].Attribute[A].Style := SyntaxSchemeList[I].Attribute[A].Style;
    OldSyntaxSchemeList[I].Attribute[A].Background := SyntaxSchemeList[I].Attribute[A].Background;
    OldSyntaxSchemeList[I].Attribute[A].Foreground := SyntaxSchemeList[I].Attribute[A].Foreground;
    OldSyntaxSchemeList[I].Tag := SyntaxSchemeList[I].Tag;
   end;
end;

procedure GetSyntaxSchemeFromMemory;
 Var I, A : Integer;
begin
 For I := 0 To SyntaxSchemeList.Count - 1 Do
  For A := 0 To SyntaxSchemeList[I].AttrCount - 1 Do
   Begin
    SyntaxSchemeList[I].Attribute[A].Style := OldSyntaxSchemeList[I].Attribute[A].Style;
    SyntaxSchemeList[I].Attribute[A].Background := OldSyntaxSchemeList[I].Attribute[A].Background;
    SyntaxSchemeList[I].Attribute[A].Foreground := OldSyntaxSchemeList[I].Attribute[A].Foreground;
    SyntaxSchemeList[I].Tag := OldSyntaxSchemeList[I].Tag;
   end;
end;

procedure SetAttributeOn(SyntaxID, AttribID: Integer; SectionB, SectionF: TColor;
 FontStyle: TFontStyles);
begin
 SyntaxSchemeList[SyntaxID].Attribute[AttribID].Style := FontStyle;
 SyntaxSchemeList[SyntaxID].Attribute[AttribID].Background := SectionB;
 SyntaxSchemeList[SyntaxID].Attribute[AttribID].Foreground := SectionF;
end;

end.
