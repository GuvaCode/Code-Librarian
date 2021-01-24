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
unit uSynHighlighterD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter, SynEditHighlighterFoldBase,
  SynEditTypes;

Type
    TCharSet = set of Char;

    TTokenKind = (tkCommt, tkIdent, tkKeywd, tkStrng, tkBlank, tkSymbl, tkNumbr,
                  tkDDocs, tkSpecK, tkError, tkAsmbl, tkAttri, tkLost,  tkTypes);

    TRangeKind = (rkString1, rkString2, rkBlockCom1, rkBlockCom2,
                  rkBlockDoc1, rkBlockDoc2, rkAsm);

    TRangeKinds = set of TRangeKind;

    TFoldKinds = set of (fkBrackets, fkComments1, fkComments2, fkStrings, fkRegion,
                         fkDDoc);

    { TSynD2SynRange }

    TSynD2SynRange       = Class(TSynCustomHighlighterRange)
     Private
       namedRegionCount       : Integer;
       nestedCommentsCount    : Integer;
       rangeKinds             : TRangeKinds;
       rString                : Boolean;

     Public
       Procedure Assign(source: TSynCustomHighlighterRange); override;
       Function Compare(range: TSynCustomHighlighterRange): integer; override;
       Procedure Clear; override;

       Procedure copyFrom(source: TSynD2SynRange);
    end;

    TSynD2Syn            = Class (TSynCustomFoldHighlighter)
     Private
       fWhiteAttrib      : TSynHighlighterAttributes;
       fNumbrAttrib      : TSynHighlighterAttributes;
       fSymblAttrib      : TSynHighlighterAttributes;
       fIdentAttrib      : TSynHighlighterAttributes;
       fCommtAttrib      : TSynHighlighterAttributes;
       fStrngAttrib      : TSynHighlighterAttributes;
       fKeywdAttrib      : TSynHighlighterAttributes;
       fDDocsAttrib      : TSynHighlighterAttributes;
       fAsblrAttrib      : TSynHighlighterAttributes;
       fSpeckAttrib      : TSynHighlighterAttributes;
       fErrorAttrib      : TSynHighlighterAttributes;
       fAttriAttrib      : TSynHighlighterAttributes;
       fLost_Attrib      : TSynHighlighterAttributes;
       fTypesAttrib      : TSynHighlighterAttributes;
       fLineBuf          : String;
       fTokStart,
       fTokStop          : Integer;
       fTokKind          : TTokenKind;
       fCurrRange        : TSynD2SynRange;
       fFoldKinds        : TFoldKinds;
       fAttribLut        : Array[TTokenKind] of TSynHighlighterAttributes;
       fPhobosStyleType  : Boolean;

       procedure setFoldKinds(value: TFoldKinds);
       procedure setWhiteAttrib(value: TSynHighlighterAttributes);
       procedure setNumbrAttrib(value: TSynHighlighterAttributes);
       procedure setSymblAttrib(value: TSynHighlighterAttributes);
       procedure setIdentAttrib(value: TSynHighlighterAttributes);
       procedure setCommtAttrib(value: TSynHighlighterAttributes);
       procedure setStrngAttrib(value: TSynHighlighterAttributes);
       procedure setKeywdAttrib(value: TSynHighlighterAttributes);
       procedure setDDocsAttrib(value: TSynHighlighterAttributes);
       procedure setAsblrAttrib(value: TSynHighlighterAttributes);
       procedure setSpeckAttrib(value: TSynHighlighterAttributes);
       procedure setErrorAttrib(value: TSynHighlighterAttributes);
       procedure setAttriAttrib(value: TSynHighlighterAttributes);
       procedure setTypesAttrib(value: TSynHighlighterAttributes);
       procedure doAttribChange(sender: TObject);
       procedure doChanged;

     Protected
       function GetRangeClass: TSynCustomHighlighterRangeClass; override;
       function GetIdentChars: TSynIdentChars; override;
       function GetSampleSource: string; override;

     Published
       property phobosStyleType: boolean read fPhobosStyleType write fPhobosStyleType stored true;
       property foldKinds:   TFoldKinds read fFoldKinds write setFoldKinds stored true;
       property whites:      TSynHighlighterAttributes read fWhiteAttrib write setWhiteAttrib stored true;
       property numbers:     TSynHighlighterAttributes read fNumbrAttrib write setNumbrAttrib stored true;
       property symbols:     TSynHighlighterAttributes read fSymblAttrib write setSymblAttrib stored true;
       property identifiers: TSynHighlighterAttributes read fIdentAttrib write setIdentAttrib stored true;
       property comments:    TSynHighlighterAttributes read fCommtAttrib write setCommtAttrib stored true;
       property strings:     TSynHighlighterAttributes read fStrngAttrib write setStrngAttrib stored true;
       property keywords:    TSynHighlighterAttributes read fKeywdAttrib write setKeywdAttrib stored true;
       property ddoc:        TSynHighlighterAttributes read fDDocsAttrib write setDDocsAttrib stored true;
       property inlineAsm:   TSynHighlighterAttributes read fAsblrAttrib write setAsblrAttrib stored true;
       property special:     TSynHighlighterAttributes read fSpeckAttrib write setSpeckAttrib stored true;
       property errors:      TSynHighlighterAttributes read fErrorAttrib write setErrorAttrib stored true;
       property attributes:  TSynHighlighterAttributes read fAttriAttrib write setAttriAttrib stored true;
       property types:       TSynHighlighterAttributes read fTypesAttrib write setTypesAttrib stored true;
       property DefaultFilter stored false;
       property enabled stored false;

     Public
       constructor create(aOwner: TComponent); override;
       destructor destroy; override;
       procedure Assign(Source: TPersistent); override;
       procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
       function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
       procedure setLine(const NewValue: string; LineNumber: Integer); override;
       procedure next; override;
       function  GetTokenAttribute: TSynHighlighterAttributes; override;
       function GetToken: string; override;
       function GetTokenKind: integer; override;
       function GetTokenPos: Integer; override;
       function GetEol: Boolean; override;
       procedure SetRange(value: Pointer); override;
       procedure ResetRange; override;
       function GetRange: Pointer; override;
       class function GetLanguageName: string; override;
    end;

implementation

Const
     stringPrefixes: TCharSet = ['r', 'x', 'q', '"'];
     stringStopChecks: TCharSet = ['\', '"'];
     stringPostfixes: TCharSet = ['c', 'w', 'd'];
     hexaChars: TCharSet = ['0'..'9', 'a'..'f', 'A'..'F', '_'];
     symbChars: TCharSet = [';', '{', '}', '(', ')', '[', ']', ',', '.', ':', '?', '$', '"', #39];

     SYNS_LangD                 =  'D';

function readDelim(var reader: PChar; var position: Integer; const aDelim: string): boolean; Overload;
 Var I : Integer;
Begin
 For I := 1 To Length(aDelim) Do
  Begin
   If reader^ = #10 Then
    Exit(False);

   If reader^ <> aDelim[I] Then
    Exit(False);

   Inc(reader);
   Inc(position);
  end;

 Result := True;
end;

function readDelim(var reader: PChar; var position: Integer; const aDelims: TCharSet): boolean; overload;
Begin
 If Not (reader^ in aDelims) Then
  Exit(False);

 Inc(reader);
 Inc(position);

 Result := True;
end;

function readLine(var reader: PChar; var position: Integer): boolean;
Begin
 Result := True;

 While reader^ <> #10 Do
  Begin
   Inc(reader);
   Inc(position);
  end;
end;

function isWhite(const c: Char): boolean;
Begin
 Result := c In [#0..#32];
end;

function readUntil(var reader: PChar; var position: Integer; const aDelim: string): boolean;
Begin
 While reader[0..length(aDelim)-1] <> aDelim Do
  Begin
   If reader^ = #10 Then
    Exit(False);

   Inc(reader);
   Inc(position);
  end;

 Inc(reader, length(aDelim));
 Inc(position, length(aDelim));

 Result := True;
end;

function readUntilAmong(var reader: PChar; var position: Integer; const aDelim: TCharSet): boolean;
Begin
 while not (reader^ in aDelim) do
  begin
    if reader^ = #10 then
      exit(false);
    inc(reader);
    inc(position);
  end;
  exit(true);
end;

function readWhile(var aReader: PChar; var position: Integer; const aDelim: TCharSet): boolean;
Begin
 result := false;
  while aReader^ in aDelim do
  begin
    inc(aReader);
    inc(position);
    result := true;
  end;
end;

function tryReadDelim(var reader: PChar; var position: Integer; const aDelim: string): boolean;
 Var savedReader: PChar;
     savedPos: Integer;
Begin
 savedReader := reader;
  savedPos := position;
  if readDelim(reader, position, aDelim) then
    exit(true);
  reader := savedReader;
  position := savedPos;
  exit(false);
end;

function isOperator1(const c: char): boolean;
Begin
 exit(c in ['/', '*', '-', '+', '%', '>', '<', '=', '!', '&', '|', '^', '~']);
end;

function isOperator2(const s: string): boolean;
begin
  result := false;
  case s[1] of
    '.': result := s[2] = '.';
    '*': result := s[2] = '=';

    '>': result := s[2] in ['>', '='];
    '<': result := s[2] in ['<', '=', '>'];
    '=': result := s[2] in ['=', '>'];
    '!': result := s[2] in ['=', '>', '<'];

    '+': result := s[2] in ['+', '='];
    '-': result := s[2] in ['-', '='];
    '/': result := s[2] in ['='];
    '%': result := s[2] in ['='];
    '~': result := s[2] in ['='];

    '&': result := s[2] in ['&', '='];
    '|': result := s[2] in ['|', '='];
    '^': result := s[2] in ['^', '='];
  end;
end;

function isOperator3(const s: string): boolean;
begin
  result := false;
  case s[1] of
    '.': result := (s[2] = '.')   and (s[3] = '.');
    '^': result := (s[2] = '^')   and (s[3] = '=');
    '>': result := (s[2] = '>')   and (s[3] in ['>', '=']);
    '<': result := ((s[2] = '<')  and (s[3] in ['<', '=']))
                  or (s[2] = '>') and (s[3] = '=');
    '!': result := ((s[2] = '<')  and (s[3] in ['>', '=']))
                  or ((s[2] = '>')and (s[3] = '='));
  end;
end;

function isOperator4(const s: string): boolean;
Begin
 result := (s = '>>>=') or (s = '!<>=');
end;

function isSymbol(const c: char): boolean;
Begin
 exit(c in [';', '{', '}', '(', ')', '[', ']', ',', '.', ':' , '"', #39, '?', '$', #35]);
end;

function isNumber(const c: Char): boolean;
Begin
 exit(c in ['0'..'9']);
end;

function isIdentifier(const c: char): boolean;
Begin
 exit((not isSymbol(c)) and (not isOperator1(c)) and (not isWhite(c)));
end;

function isAlpha(const c: Char): boolean;
Begin
 exit((c in ['a'..'z']) or (c in ['A'..'Z']));
end;

function isAlNum(const c: Char): boolean;
Begin
 exit(isAlpha(c) or isNumber(c));
end;

function isFirstIdentifier(const c: char): boolean;
Begin
 exit(isIdentifier(c) and (not isNumber(c)));
end;

Function Words_hash(const w: string): Word;

 Const fCoeffs: array[0..255] of Byte =
    (
      93, 12, 147, 37, 246, 76, 204, 47, 77, 0, 217, 84, 225, 244, 62, 63, 81, 2,
      46, 137, 104, 245, 184, 87, 229, 148, 69, 207, 24, 10, 239, 172, 27, 34, 60,
      251, 113, 66, 175, 29, 10, 1, 158, 38, 157, 120, 224, 173, 11, 199, 49, 173,
      88, 229, 213, 191, 217, 177, 90, 19, 83, 212, 97, 12, 136, 154, 243, 105,
      97, 29, 94, 226, 71, 60, 28, 245, 38, 212, 156, 116, 254, 70, 207, 211, 93,
      67, 32, 42, 149, 101, 98, 4, 83, 160, 228, 128, 231, 188, 100, 178, 22, 172,
      198, 218, 13, 166, 45, 54, 49, 152, 14, 123, 232, 223, 86, 10, 62, 46, 220,
      55, 161, 22, 210, 86, 14, 79, 8, 28, 66, 67, 84, 116, 159, 144, 37, 46, 199,
      218, 233, 188, 207, 168, 89, 64, 245, 3, 6, 199, 144, 165, 216, 145, 141, 70,
      69, 20, 149, 252, 119, 75, 153, 97, 14, 196, 74, 48, 91, 145, 70, 90, 59, 69,
      92, 252, 233, 161, 169, 155, 9, 28, 234, 103, 172, 225, 164, 49, 161, 95, 81,
      201, 217, 217, 58, 119, 169, 230, 11, 8, 137, 65, 165, 159, 4, 243, 225, 236,
      178, 209, 133, 35, 68, 222, 237, 114, 64, 158, 72, 66, 151, 208, 169, 232, 83,
      229, 157, 233, 123, 135, 65, 187, 161, 100, 217, 63, 124, 36, 108, 198, 2,
      103, 156, 241, 140, 163, 128, 196, 45, 166, 41, 61, 19, 139, 25, 115, 72, 175
    );

 Var I : Integer;
Begin
 Result := 0;
  for i := 1 to length(w) do
    Result += fCoeffs[Byte(w[i])];
  Result := Result and $1FF;
end;

function keywordsMap_match(const w: string): boolean;

 const fWords: array [0..511] of string =
    (
      'double', '', '', '', '', 'volatile', 'synchronized', '', 'wchar', '', '',
      '', '', '', 'goto', '', 'assert', '', '', 'void', '', '', '', 'override',
      'pure', '', '', '', '', '', '', 'delegate', '', '', 'super', '', 'case',
      '', '', '', 'pragma', '', '', '', 'string', '', 'debug', '', '', '', '',
      '', 'module', '', '', '', '', '', '', '', '', '', '', 'immutable', '',
      'template', 'dstring', '', '__parameters', '', '', '', '', '__vector', '',
      '', '', '', '', '', 'invariant', '', 'unittest', '', '', 'protected', '',
      '', 'break', 'alias', '', '', '', '', '', '', '', '', '', 'wstring', '',
      '', 'private', 'final', '', 'false', '', 'catch', 'float', '', '', '', '',
      '', '', '', '', '', '', '', '', '', '', 'align', '', '', '', '', '', '',
      'ptrdiff_t', '', '', '', '', '', '', 'delete', '', '', '', '', '', '', '',
      'do', '', 'mixin', '', 'ireal', '', '', '', '', 'static', 'extern', '', '',
      'null', '', '', 'creal', '', '', 'typeid', '', 'idouble', '', '', '', 'try',
      '', '', '', 'finally', '', 'is', '', 'cdouble', '', 'in', '', '', '', '',
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
      'scope', '', '', 'package', '', '', '', '', '', 'interface', '', '', 'macro',
      '', '', '', '', '', '', '', '', '', 'default', '', '', '', '', '', 'out',
      '', '', '', '', 'size_t', '', '', '', '', 'new', 'int', '', '', '', '', '',
      '', '', '', 'this', '', '', '', '', '', '', '', 'public', '', '', '',
      'continue', '', '', '', 'body', '', '', '', '', '', '', 'ifloat', '', '',
      '', '', 'version', '', '', 'deprecated', '', '', '', 'cfloat', '', 'uint',
      'function', '', '', '', '', 'short', '', 'with', 'typeof', '', '', '', '',
      '', '', '', '', '', '', '', 'import', '', '', '', '', '', '', '', '',
      '__traits', '', '', '', '', '', 'export', '', '', '', '', '', '', '', '',
      '', '', '', '', '', '', 'throw', 'ushort', '', '', '', '', '', '', '', '',
      '', 'asm', '', '', '', '', '', 'byte', '', '', '', '', '', 'abstract',
      'union', 'if', '', 'true', '', 'typedef', '', '', '', '', '', '', '', '',
      '', '', '', '', '', '', '', 'enum', '', '', 'const', '', '', '', '', '', '',
      '', 'bool', '', '', '', '', '', '', 'ubyte', 'else', 'long', '', '', 'for',
      '', '', '', 'inout', '', '', '', '', '', '', '', 'auto', '', '', '', '', '',
      '', 'cent', '', '', '', '', '', '', '', '', 'class', '', '', 'cast', '', '',
      '', '', '', 'struct', '', 'foreach', '', '', '', 'ulong', '', '', '__gshared',
      '', 'while', 'ref', '', '', '', '', '', '', '', '', 'char', 'return', '',
      'foreach_reverse', 'lazy', '', '', 'ucent', '', '', '', 'nothrow', '', '',
      '', '', '', '', '', 'switch', '', '', 'dchar', '', '', '', 'shared', '', '',
      '', 'real', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''
    );

 Const fHasEntry: array [0..511] of boolean =
    (
      true, false, false, false, false, true, true, false, true, false, false,
      false, false, false, true, false, true, false, false, true, false, false,
      false, true, true, false, false, false, false, false, false, true, false,
      false, true, false, true, false, false, false, true, false, false, false,
      true, false, true, false, false, false, false, false, true, false, false,
      false, false, false, false, false, false, false, false, true, false, true,
      true, false, true, false, false, false, false, true, false, false, false,
      false, false, false, true, false, true, false, false, true, false, false,
      true, true, false, false, false, false, false, false, false, false, false,
      true, false, false, true, true, false, true, false, true, true, false, false,
      false, false, false, false, false, false, false, false, false, false, false,
      false, true, false, false, false, false, false, false, true, false, false,
      false, false, false, false, true, false, false, false, false, false, false,
      false, true, false, true, false, true, false, false, false, false, true,
      true, false, false, true, false, false, true, false, false, true, false,
      true, false, false, false, true, false, false, false, true, false, true,
      false, true, false, true, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, true, false, false, true, false, false, false,
      false, false, true, false, false, true, false, false, false, false, false,
      false, false, false, false, true, false, false, false, false, false, true,
      false, false, false, false, true, false, false, false, false, true, true,
      false, false, false, false, false, false, false, false, true, false, false,
      false, false, false, false, false, true, false, false, false, true, false,
      false, false, true, false, false, false, false, false, false, true, false,
      false, false, false, true, false, false, true, false, false, false, true,
      false, true, true, false, false, false, false, true, false, true, true, false,
      false, false, false, false, false, false, false, false, false, false, true,
      false, false, false, false, false, false, false, false, true, false, false,
      false, false, false, true, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, true, true, false, false,
      false, false, false, false, false, false, false, true, false, false, false,
      false, false, true, false, false, false, false, false, true, true, true,
      false, true, false, true, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, true, false, false,
      true, false, false, false, false, false, false, false, true, false, false,
      false, false, false, false, true, true, true, false, false, true, false,
      false, false, true, false, false, false, false, false, false, false, true,
      false, false, false, false, false, false, true, false, false, false, false,
      false, false, false, false, true, false, false, true, false, false, false,
      false, false, true, false, true, false, false, false, true, false, false,
      true, false, true, true, false, false, false, false, false, false, false,
      false, true, true, false, true, true, false, false, true, false, false,
      false, true, false, false, false, false, false, false, false, true, false,
      false, true, false, false, false, true, false, false, false, true, false,
      false, false, false, false, false, false, false, false, false, false, false,
      false, false, false, false
    );

 Var H : Word;
Begin
 Result := False;

 if (length(w) < 2) or (length(w) > 15) then
  Exit;

 H := Words_hash(w);

 if fHasEntry[H] then
  result := fWords[h] = w;
end;

Function SPWords_hash(const w: string): Byte;

 Const fCoeffs: array[0..255] of Byte =
    (
      162, 105, 225, 180, 180, 12, 125, 73, 237, 109, 3, 67, 160, 192, 35, 42,
      131, 170, 41, 106, 103, 53, 105, 74, 29, 64, 247, 248, 184, 146, 172, 142,
      239, 232, 158, 168, 29, 243, 40, 241, 255, 85, 184, 38, 44, 242, 193, 222,
      86, 131, 181, 101, 161, 209, 115, 124, 91, 118, 188, 67, 172, 115, 24, 221,
      142, 99, 17, 30, 231, 80, 185, 182, 185, 55, 4, 23, 152, 63, 126, 37, 158,
      36, 28, 235, 65, 220, 243, 62, 169, 129, 127, 76, 149, 232, 21, 119, 134,
      144, 20, 89, 103, 65, 109, 12, 95, 200, 41, 14, 52, 25, 56, 228, 4, 227,
      86, 113, 77, 158, 46, 246, 90, 25, 210, 214, 149, 219, 219, 27, 95, 203,
      43, 21, 191, 94, 216, 113, 100, 222, 245, 224, 127, 174, 214, 44, 78, 89,
      213, 184, 73, 77, 236, 131, 46, 90, 58, 171, 34, 215, 201, 104, 138, 251,
      54, 103, 75, 235, 12, 149, 49, 19, 128, 72, 138, 224, 73, 174, 151, 50,
      152, 32, 135, 238, 132, 34, 3, 230, 201, 166, 31, 119, 50, 155, 125, 103,
      133, 250, 253, 218, 48, 167, 207, 107, 235, 53, 214, 213, 49, 8, 13, 247,
      37, 251, 21, 43, 34, 108, 162, 160, 133, 199, 169, 218, 189, 1, 128, 17,
      67, 186, 55, 2, 23, 23, 133, 114, 240, 176, 124, 127, 217, 231, 129, 220,
      250, 17, 136, 92, 191, 172, 16, 137, 23, 109, 37, 191, 74, 218
    );

 Var I : Integer;
Begin
 Result := 0;
  for i := 1 to length(w) do
    Result += fCoeffs[Byte(w[i])];
  Result := Result and $F;
end;

function specialKeywordsMap_match(const w: string): boolean;

 Const fWords: array [0..15] of string =
    (
      '__VERSION__', '', '__FILE_FULL_PATH__', '__TIME__', '__FILE__', '__VENDOR__',
      '', '__DATE__', '__FUNCTION__', '__LINE__', '__EOF__', '__MODULE__',
      '__PRETTY_FUNCTION__', '', '', '__TIMESTAMP__'
    );

 Const fHasEntry: array [0..15] of boolean =
    (
      true, false, true, true, true, true, false, true, true, true, true, true,
      true, false, false, true
    );

 Var H : Byte;
Begin
 result := false;

 if (length(w) < 7) or (length(w) > 19) then
  exit;

 H := SPWords_hash(w);

 if fHasEntry[H] then
  result := fWords[h] = w;
end;

{ TSynD2Syn }

procedure TSynD2Syn.setFoldKinds(value: TFoldKinds);
begin
 fFoldKinds := value;

 DoFoldConfigChanged(Self);

 doChanged;
end;

procedure TSynD2Syn.setWhiteAttrib(value: TSynHighlighterAttributes);
begin
 fWhiteAttrib.Assign(value);
end;

procedure TSynD2Syn.setNumbrAttrib(value: TSynHighlighterAttributes);
begin
 fNumbrAttrib.Assign(value);
end;

procedure TSynD2Syn.setSymblAttrib(value: TSynHighlighterAttributes);
begin
 fSymblAttrib.Assign(value);
end;

procedure TSynD2Syn.setIdentAttrib(value: TSynHighlighterAttributes);
begin
 fIdentAttrib.Assign(value);
end;

procedure TSynD2Syn.setCommtAttrib(value: TSynHighlighterAttributes);
begin
 fCommtAttrib.Assign(value);
end;

procedure TSynD2Syn.setStrngAttrib(value: TSynHighlighterAttributes);
begin
 fStrngAttrib.Assign(value);
end;

procedure TSynD2Syn.setKeywdAttrib(value: TSynHighlighterAttributes);
begin
 fKeywdAttrib.Assign(value);
end;

procedure TSynD2Syn.setDDocsAttrib(value: TSynHighlighterAttributes);
begin
 fDDocsAttrib.Assign(value);
end;

procedure TSynD2Syn.setAsblrAttrib(value: TSynHighlighterAttributes);
begin
 fAsblrAttrib.Assign(value);
end;

procedure TSynD2Syn.setSpeckAttrib(value: TSynHighlighterAttributes);
begin
 fSpeckAttrib.Assign(value);
end;

procedure TSynD2Syn.setErrorAttrib(value: TSynHighlighterAttributes);
begin
 fErrorAttrib.Assign(value);
end;

procedure TSynD2Syn.setAttriAttrib(value: TSynHighlighterAttributes);
begin
 fAttriAttrib.Assign(value);
end;

procedure TSynD2Syn.setTypesAttrib(value: TSynHighlighterAttributes);
begin
 fTypesAttrib.Assign(value);
end;

procedure TSynD2Syn.doAttribChange(sender: TObject);
begin
 doChanged;
end;

procedure TSynD2Syn.doChanged;
begin
 BeginUpdate;

 fUpdateChange := True;

 EndUpdate;
end;

function TSynD2Syn.GetRangeClass: TSynCustomHighlighterRangeClass;
begin
 Result := TSynD2SynRange;
end;

function TSynD2Syn.GetIdentChars: TSynIdentChars;
begin
 Result := ['_', 'A'..'Z', 'a'..'z', '0'..'9'];
end;

function TSynD2Syn.GetSampleSource: string;
begin
 Result := 'void main()'+#13#10+
           '{'+#13#10+
           '     import std.stdio, std.string, std.algorithm, std.conv;'+#13#10+
           ''+#13#10+
           '     // Reduce the RPN expression using a stack'+#13#10+
           '     readln.split.fold!((stack, op)'+#13#10+
           '     {'+#13#10+
           '         switch (op)'+#13#10+
           '         {'+#13#10+
           '                // Generate operator switch cases statically'+#13#10+
           '                static foreach (c; "+-*/")'+#13#10+
           '                       case [c]:'+#13#10+
           '                            return stack[0 .. $ - 2] ~'+#13#10+
           '                              mixin("stack[$ - 2] " ~ c ~'+#13#10+
           '                              " stack[$ - 1]");'+#13#10+
           '                       default: return stack ~ op.to!real;'+#13#10+
           '         }'+#13#10+
           '     })((real[]).init).writeln;'+#13#10+
           '}';
end;

constructor TSynD2Syn.create(aOwner: TComponent);
begin
 Inherited create(aOwner);

 SetSubComponent(true);

 fDefaultFilter := 'D source files (*.d,*.di)|*.d;*.di';

 fFoldKinds := [fkBrackets,fkRegion];

 WordBreakChars := WordBreakChars - ['@'];

 fWhiteAttrib := TSynHighlighterAttributes.Create('Space','White');
 fNumbrAttrib := TSynHighlighterAttributes.Create('Number','Numbr');
 fSymblAttrib := TSynHighlighterAttributes.Create('Symbol','Symbl');
 fIdentAttrib := TSynHighlighterAttributes.Create('Identifier','Ident');
 fCommtAttrib := TSynHighlighterAttributes.Create('Comment','Commt');
 fStrngAttrib := TSynHighlighterAttributes.Create('String','Strng');
 fKeywdAttrib := TSynHighlighterAttributes.Create('Keyword','Keywd');
 fDDocsAttrib := TSynHighlighterAttributes.Create('Documentation','DDocs');
 fAsblrAttrib := TSynHighlighterAttributes.Create('Assembler','Asblr');
 fSpeckAttrib := TSynHighlighterAttributes.Create('Special','Speck');
 fErrorAttrib := TSynHighlighterAttributes.Create('Error','Error');
 fAttriAttrib := TSynHighlighterAttributes.Create('Attribute','Attri');
 fTypesAttrib := TSynHighlighterAttributes.Create('Types','Types');
 fLost_Attrib := TSynHighlighterAttributes.Create('Lost','Lost');

 fNumbrAttrib.Foreground := $000079F2;
 fSymblAttrib.Foreground := clMaroon;
 fIdentAttrib.Foreground := clBlack;
 fCommtAttrib.Foreground := clGreen;
 fStrngAttrib.Foreground := clBlue;
 fKeywdAttrib.Foreground := clNavy;
 fAsblrAttrib.Foreground := clGray;
 fSpeckAttrib.Foreground := clNavy;
 fAttriAttrib.Foreground := clNavy;
 fLost_Attrib.Foreground := clLime;
 fDDocsAttrib.Foreground := clTeal;
 fTypesAttrib.Foreground := clBlack;

 fLost_Attrib.Background := clBlack;

 fCommtAttrib.Style := [fsItalic];
 fKeywdAttrib.Style := [fsBold];
 fAsblrAttrib.Style := [fsBold];
 fSpeckAttrib.Style := [fsBold];
 fAttriAttrib.Style := [fsBold];
 fLost_Attrib.Style := [fsBold];
 fTypesAttrib.Style := [fsBold];

 fErrorAttrib.Foreground:= fIdentAttrib.Foreground;
 fErrorAttrib.FrameStyle:= slsWaved;
 fErrorAttrib.FrameColor:= clRed;
 fErrorAttrib.FrameEdges:= sfeBottom;

 AddAttribute(fWhiteAttrib);
 AddAttribute(fNumbrAttrib);
 AddAttribute(fSymblAttrib);
 AddAttribute(fIdentAttrib);
 AddAttribute(fCommtAttrib);
 AddAttribute(fStrngAttrib);
 AddAttribute(fKeywdAttrib);
 AddAttribute(fDDocsAttrib);
 AddAttribute(fAsblrAttrib);
 AddAttribute(fSpeckAttrib);
 AddAttribute(fErrorAttrib);
 AddAttribute(fAttriAttrib);
 AddAttribute(fTypesAttrib);

 fAttribLut[TTokenKind.tkident] := fIdentAttrib;
 fAttribLut[TTokenKind.tkBlank] := fWhiteAttrib;
 fAttribLut[TTokenKind.tkCommt] := fCommtAttrib;
 fAttribLut[TTokenKind.tkKeywd] := fKeywdAttrib;
 fAttribLut[TTokenKind.tkNumbr] := fNumbrAttrib;
 fAttribLut[TTokenKind.tkStrng] := fStrngAttrib;
 fAttribLut[TTokenKind.tksymbl] := fSymblAttrib;
 fAttribLut[TTokenKind.tkDDocs] := fDDocsAttrib;
 fAttribLut[TTokenKind.tkSpecK] := fSpeckAttrib;
 fAttribLut[TTokenKind.tkError] := fErrorAttrib;
 fAttribLut[TTokenKind.tkAsmbl] := fAsblrAttrib;
 fAttribLut[TTokenKind.tkAttri] := fAttriAttrib;
 fAttribLut[TTokenKind.tkTypes] := fTypesAttrib;
 fAttribLut[TTokenKind.tkLost]  := fLost_Attrib;

 SetAttributesOnChange(@doAttribChange);
 fTokStop := 1;
 next;
end;

destructor TSynD2Syn.destroy;
begin
 fLost_Attrib.Free;
 fCurrRange.Free;

 Inherited destroy;
end;

procedure TSynD2Syn.Assign(Source: TPersistent);
 Var srcsyn : TSynD2Syn;
begin
 Inherited Assign(Source);

 If Source Is TSynD2Syn Then
  Begin
   srcsyn := TSynD2Syn(Source);
   foldKinds := srcsyn.foldKinds;
   fPhobosStyleType := srcsyn.fPhobosStyleType;
  end;
end;

procedure TSynD2Syn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
 TokenStart  := @fLineBuf[FTokStart];
 TokenLength := fTokStop - FTokStart;
end;

function TSynD2Syn.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
 Result := Nil;
end;

procedure TSynD2Syn.setLine(const NewValue: string; LineNumber: Integer);
begin
 Inherited setLine(NewValue, LineNumber);

 fLineBuf := NewValue + #10;
 fTokStop := 1;
 next;
end;

procedure TSynD2Syn.next;
 Var reader: PChar = Nil;

 Label _notRawStrng, _notDotFloat;

 Procedure readerReset;
 Begin
  fTokStop := fTokStart;
  reader := @fLineBuf[fTokStop];
 end;

 Function readerNext : PChar;
 Begin
  Inc(reader);
  Inc(fTokStop);

  Exit(reader);
 end;

 Function readerPrev : PChar;
 Begin
  Dec(reader);
  Dec(fTokStop);

  Exit(reader);
 end;

begin
 fTokStart := fTokStop;
 fTokStop  := fTokStart;

 If fTokStop > Length(fLineBuf) Then
  Exit;

 readerReset;

 If (LineIndex = 0) And (fTokStart = 1) And readDelim(reader, fTokStop, '#!') Then
  Begin
   fTokKind := tkCommt;

   readLine(reader, fTokStop);

   exit;
  end
 Else
  readerReset;

 If (isWhite(reader^)) Then
  Begin
   fTokKind := tkBlank;

   While True Do
    Begin
     If isWhite(reader^) Then
      readerNext;

     Exit;
    end;
  end;

 If Not Assigned(fCurrRange) Then
  fCurrRange := TSynD2SynRange.Create(Nil);

 If (fCurrRange.rangeKinds = []) Or (fCurrRange.rangeKinds = [rkAsm]) Then
  If readDelim(reader, fTokStop, '//') Then
   Begin
    fTokKind := tkCommt;

    If readDelim(reader, fTokStop, '/') Then
     fTokKind := tkDDocs;

    readLine(reader, fTokStop);

    If (fkRegion In fFoldKinds) And (fTokStop - fTokStart > 4) Then
     Begin
      While isWhite(reader^) Do
       Begin
        Dec(reader);
        Dec(fTokStop);
       end;

      Dec(reader, 3);
      Dec(fTokStop, 3);

      If reader[0..3] = '---+' Then
       Begin
        fCurrRange.namedRegionCount += 1;

        StartCodeFoldBlock(Nil);
       end
      Else
       If (reader[0..3] = '----') And (fCurrRange.namedRegionCount > 0) Then
        Begin
         EndCodeFoldBlock();

         fCurrRange.namedRegionCount -= 1;
        end;

      readLine(reader, fTokStop);
     end;

    Exit;
   end
  Else
   readerReset;

 If (fCurrRange.rangeKinds = []) Or (fCurrRange.rangeKinds = [rkAsm]) Then
  If readDelim(reader, fTokStop, '/*') Then
   Begin
    fTokKind := tkCommt;

    If readDelim(reader, fTokStop, '*') Then
     If readDelim(reader, fTokStop, '/') Then
      Exit
     Else
      fTokKind := tkDDocs;

    If readUntil(reader, fTokStop, '*/') Then
     Exit;

    If fTokKind = tkDDocs Then
     fCurrRange.rangeKinds += [rkBlockDoc1]
    Else
     fCurrRange.rangeKinds += [rkBlockCom1];

    readLine(reader, fTokStop);

    If (fTokKind = tkCommt) Then
     StartCodeFoldBlock(Nil, fkComments1 In fFoldKinds)
    Else
     If (fTokKind = tkDDocs) Then
      StartCodeFoldBlock(Nil, fkDDoc In fFoldKinds);

   Exit;
  End
 Else
  readerReset;

 If (rkBlockCom1 In fCurrRange.rangeKinds) Or (rkBlockDoc1 In fCurrRange.rangeKinds) Then
  begin
   If (rkBlockDoc1 In fCurrRange.rangeKinds) Then
    fTokKind := tkDDocs
   Else
    fTokKind := tkCommt;

   If readUntil(reader, fTokStop, '*/') Then
    Begin
     If (fTokKind = tkCommt) Then
      EndCodeFoldBlock(fkComments1 in fFoldKinds)
     Else
      If (fTokKind = tkDDocs) Then
       EndCodeFoldBlock(fkDDoc In fFoldKinds);

     fCurrRange.rangeKinds -= [rkBlockDoc1, rkBlockCom1];
     Exit;
    end;

   readLine(reader, fTokStop);
   Exit;
  End;

 if (fCurrRange.rangeKinds = []) or (fCurrRange.rangeKinds = [rkAsm]) then
    if readDelim(reader, fTokStop, '/+') then
  begin
    fTokKind := tkCommt;
    if readDelim(reader, fTokStop, '+') then
      if readDelim(reader, fTokStop, '/') then
        exit
      else
        fTokKind := tkDDocs;
    inc(fCurrRange.nestedCommentsCount);
    while (reader^ <> #10) and (fCurrRange.nestedCommentsCount > 0) do
    begin
      if readUntilAmong(reader, fTokStop, ['+', '/']) then
      begin
        if readDelim(reader, fTokStop, ['+', '/']) then
        begin
          if ((reader-1)^ = '/') and (reader^ = '+') then
          begin
            inc(fCurrRange.nestedCommentsCount);
            readerNext;
            continue;
          end;
          if ((reader-1)^ = '+') and (reader^ = '/') then
          begin
            dec(fCurrRange.nestedCommentsCount);
            readerNext;
            continue;
          end;
        end else readerNext;
      end;
    end;
    if (fCurrRange.nestedCommentsCount > 0) then
    begin
      if fTokKind = tkDDocs then
        fCurrRange.rangeKinds += [rkBlockDoc2]
      else
        fCurrRange.rangeKinds += [rkBlockCom2];
      if (fTokKind = tkCommt) then
        StartCodeFoldBlock(nil, fkComments2 in fFoldKinds)
      else if (fTokKind = tkDDocs) then
        StartCodeFoldBlock(nil, fkDDoc in fFoldKinds);
    end;
    exit;
  end else readerReset;
  if (rkBlockCom2 in fCurrRange.rangeKinds) or (rkBlockDoc2 in fCurrRange.rangeKinds) then
  begin
    if (rkBlockDoc2 in fCurrRange.rangeKinds) then
      fTokKind := tkDDocs
    else
      fTokKind := tkCommt;
    while (reader^ <> #10) and (fCurrRange.nestedCommentsCount > 0) do
    begin
      if readUntilAmong(reader, fTokStop, ['+', '/']) then
      begin
        if readDelim(reader, fTokStop, ['+', '/']) then
        begin
          if ((reader-1)^ = '/') and (reader^ = '+') then
          begin
            inc(fCurrRange.nestedCommentsCount);
            readerNext;
            continue;
          end;
          if ((reader-1)^ = '+') and (reader^ = '/') then
          begin
            dec(fCurrRange.nestedCommentsCount);
            readerNext;
            continue;
          end;
        end else readerNext;
      end;
    end;
    if fCurrRange.nestedCommentsCount = 0 then
    begin
      if (fTokKind = tkCommt) then
        EndCodeFoldBlock(fkComments2 in fFoldKinds)
      else if (fTokKind = tkDDocs) then
        EndCodeFoldBlock(fkDDoc in fFoldKinds);
      fCurrRange.rangeKinds -= [rkBlockDoc2, rkBlockCom2];
    end;
    exit;
  end;

 // double quoted strings | raw double quoted strings
  if (fCurrRange.rangeKinds = []) and readDelim(reader, fTokStop, stringPrefixes) then
  begin
    if readerPrev^ in ['r','x','q'] then
    begin
      fCurrRange.rString := reader^ = 'r';
      if not (readerNext^ = '"') then
      begin
        fCurrRange.rString := false;
        readerPrev;
        goto _notRawStrng;
      end;
    end;
    readerNext;
    fTokKind := tkStrng;
    while(true) do
    begin
      if not readUntilAmong(reader, fTokStop, stringStopChecks) then
        break;
      if (reader^ = '\') then
      begin
        readerNext;
        if reader^ <> #10 then
        begin
          if fCurrRange.rString then
            continue;
          readerNext;
        end;
      end
      else if reader^ = '"' then
      begin
        readerNext;
        readDelim(reader, fTokStop, stringPostfixes);
        fCurrRange.rString := false;
        exit;
      end;
    end;
    fCurrRange.rangeKinds += [rkString1];
    StartCodeFoldBlock(nil, fkStrings in fFoldKinds);
    exit;
  end else _notRawStrng: readerReset;
  if rkString1 in fCurrRange.rangeKinds then
  begin
    fTokKind := tkStrng;
    while(true) do
    begin
      if not readUntilAmong(reader, fTokStop, stringStopChecks) then
        break;
      if reader^ = '\' then
      begin
        readerNext;
        if reader^ <> #10 then
        begin
          if fCurrRange.rString then
            continue;
          readerNext;
        end;
      end
      else if reader^ = '"' then
      begin
        readerNext;
        fCurrRange.rangeKinds -= [rkString1];
        readDelim(reader, fTokStop, stringPostfixes);
        fCurrRange.rString := false;
        EndCodeFoldBlock(fkStrings in fFoldKinds);
        exit;
      end
      else break;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  // backticks strings
  if (fCurrRange.rangeKinds = []) and readDelim(reader, fTokStop, '`') then
  begin
    fTokKind := tkStrng;
    if readUntil(reader, fTokStop, '`') then
    begin
      readDelim(reader, fTokStop, stringPostfixes);
      exit;
    end;
    fCurrRange.rangeKinds += [rkString2];
    readLine(reader, fTokStop);
    StartCodeFoldBlock(nil, fkStrings in fFoldKinds);
    exit;
  end else readerReset;
  if rkString2 in fCurrRange.rangeKinds then
  begin
    fTokKind := tkStrng;
    if readUntil(reader, fTokStop, '`') then
    begin
      fCurrRange.rangeKinds -= [rkString2];
      EndCodeFoldBlock(fkStrings in fFoldKinds);
      readDelim(reader, fTokStop, stringPostfixes);
      exit;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  // token string
  if readDelim(reader, fTokStop, 'q{') then
  begin
    fTokKind := tkSymbl;
    StartCodeFoldBlock(nil, fkBrackets in fFoldKinds);
    exit;
  end else readerReset;

  // char literals
  if (fCurrRange.rangeKinds = []) and readDelim(reader, fTokStop, #39) then
  begin
    fTokKind := tkStrng;
    while true do
    begin
      if reader^ = '\' then
      begin
        readerNext;
        if reader^ = #10 then
          exit;
        readerNext;
      end;
      if reader^ = #10 then
        exit;
      if reader^ = #39 then
        break;
      readerNext;
    end;
    readerNext;
    exit;
  end else readerReset;

  // bin & hex literals
  if reader^ = '0' then
    if readerNext^ in ['b','B', 'x', 'X'] then
  begin
    fTokKind:= tkNumbr;
    readerNext;
    if (reader-1)^ in ['b','B'] then
      readWhile(reader, fTokStop, ['0','1','_'])
    else
      readWhile(reader, fTokStop, hexaChars + ['.']);
    // exponent, sign tokenized later as op and the value as number
    if reader^ in ['P','p'] then
    begin
      readerNext;
      exit;
    end;
    if not tryReadDelim(reader, fTokStop, 'uL')
      and not tryReadDelim(reader, fTokStop, 'UL')
      and not tryReadDelim(reader, fTokStop, 'Lu')
      and not tryReadDelim(reader, fTokStop, 'LU')
      and (reader^ in ['U', 'L', 'u', 'i']) then
        readerNext;
    if not isWhite(reader^) and not isOperator1(reader^) and
      not isSymbol(reader^) then
    begin
      fTokKind:= tkError;
      readUntilAmong(reader, fTokStop, [#0..#32] + symbChars);
    end;
    exit;
  end
  else readerPrev;

  // int and float literals
  if (reader^ = '.') or (isNumber(reader^)) then
  begin
    if (reader^= '.') then
    begin
      readerPrev;
      if reader^= '.' then
      begin
        readerNext;
        goto _notDotFloat;
      end else
        readerNext;
      readerNext;
      if not isNumber(reader^) then
      begin
        readerPrev;
        goto _notDotFloat;
      end else
        readerPrev;
    end;
    fTokKind:= tkNumbr;
    if reader^ <> '.' then
      while isNumber(readerNext^) or (reader^ = '_') do (*!*);
    if reader^ = '.' then
    begin
      if isNumber(readerNext^) then
      begin
        while isNumber(reader^) or (reader^ = '_') do
          readerNext;
      end else
        readerPrev;
    end;
    if reader^= '.' then
    begin
      readerNext;
      // .init .min .max etc.
      if not isNumber(reader^) then
      begin
        readerPrev;
        exit;
      end;
      readerPrev;
    end;
    // exponent, sign tokenized later as op and the value as number
    if reader^ in ['E','e'] then
    begin
      readerNext;
      exit;
    end;
    // try valid suffixes
    if not tryReadDelim(reader, fTokStop, 'uL')
      and not tryReadDelim(reader, fTokStop, 'UL')
      and not tryReadDelim(reader, fTokStop, 'Lu')
      and not tryReadDelim(reader, fTokStop, 'LU')
      and not tryReadDelim(reader, fTokStop, 'fi')
      and not tryReadDelim(reader, fTokStop, 'Fi')
      and not tryReadDelim(reader, fTokStop, 'Li')
      and (reader^ in ['U','L','u', 'i', 'f','F']) then
        readerNext;
    if not isWhite(reader^) and not isOperator1(reader^) and
      (not isSymbol(reader^) or (reader^ = '.')) then
    begin
      fTokKind:= tkError;
      readUntilAmong(reader, fTokStop, [#0..#32] + symbChars - ['.']);
    end;
    if (fTokStop - fTokStart = 10) and (fLineBuf[fTokStart..fTokStop-1] = '4815162342') then
      fTokKind:=tkLost;
    exit;
  end;
  _notDotFloat:

  // symbols
  if isSymbol(reader^) then
  begin
    fTokKind := tkSymbl;
    case reader^ of
      '{': StartCodeFoldBlock(nil, fkBrackets in fFoldKinds);
      '}':
      begin
        EndCodeFoldBlock(fkBrackets in fFoldKinds);
        if (reader^ = '}') and (rkAsm in fCurrRange.rangeKinds) then
          fCurrRange.rangeKinds -= [rkAsm]; ;
        if ((reader+1)^ in stringPostfixes) and not isIdentifier((reader+2)^) then
          readerNext;
      end;
    end;
    readerNext;
    exit;
  end;

  // operators
  if isOperator1(reader^) then
  begin
    fTokKind := tkSymbl;
    while isOperator1(readerNext^) do
      if (fTokStop - fTokStart = 4) or (reader^= #10) then
        break;
    if (fTokStop - fTokStart = 4) then
    begin
      if isOperator4(fLineBuf[fTokStart..fTokStart+3]) then
        exit;
      if isOperator3(fLineBuf[fTokStart..fTokStart+2]) then
      begin
        readerPrev;
        exit;
      end;
      if isOperator2(fLineBuf[fTokStart..fTokStart+1]) then
      begin
        readerPrev;
        readerPrev;
        exit;
      end;
      if isOperator1(fLineBuf[fTokStart]) then
      begin
        readerPrev;
        readerPrev;
        readerPrev;
        exit;
      end;
    end;
    if (fTokStop - fTokStart = 3) then
    begin
      if isOperator3(fLineBuf[fTokStart..fTokStart+2]) then
        exit;
      if isOperator2(fLineBuf[fTokStart..fTokStart+1]) then
      begin
        readerPrev;
        exit;
      end;
      if isOperator1(fLineBuf[fTokStart]) then
      begin
        readerPrev;
        readerPrev;
        exit;
      end;
    end;
    if (fTokStop - fTokStart = 2) then
    begin
      if isOperator2(fLineBuf[fTokStart..fTokStart+1]) then
        exit;
      if isOperator1(fLineBuf[fTokStart]) then
      begin
        readerPrev;
        exit;
      end;
    end;
    if (fTokStop - fTokStart = 1) and isOperator1(fLineBuf[fTokStart]) then
      exit;
    fTokKind := tkError;
    exit;
  end;

  // attributes
  if reader^ = '@' then
  begin
    if isAlpha(readerNext^) then
    begin
      fTokKind:=tkAttri;
      while isAlNum(reader^) or (reader^ = '_') do
      begin
        if (reader^= #10) or (reader^= '@') then
          exit;
        readerNext;
      end;
      exit;
    end else
      readerPrev;
  end;

  // Keywords & identifiers
  if isFirstIdentifier(reader^) then
  begin
    fTokKind := tkIdent;
    while(true) do
    begin
      if isWhite(readerNext^) or isSymbol(reader^) or isOperator1(reader^) or
        (reader^ = '@') then
          break;
    end;
    if keywordsMap_match(fLineBuf[FTokStart..fTokStop-1]) then
    begin
      fTokKind := tkKeywd;
      if (fLineBuf[FTokStart..fTokStop-1] = 'asm') then
        fCurrRange.rangeKinds += [rkAsm];
    end
    else if specialKeywordsMap_match(fLineBuf[FTokStart..fTokStop-1]) then
      fTokKind := tkSpecK
    else if fPhobosStyleType and ('A' <= fLineBuf[FTokStart]) and (fLineBuf[FTokStart] <= 'Z') then
      fTokKind:= tkTypes
    else if rkAsm in fCurrRange.rangeKinds then
      fTokKind:=tkAsmbl;
    exit;
  end;

  if fLineBuf[fTokStop] = #10 then
    exit;

  readUntilAmong(reader, fTokStop, [#9, #10, ' ']);
  fTokKind := tkError;
end;

function TSynD2Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
 result := fAttribLut[fTokKind];
end;

function TSynD2Syn.GetToken: string;
begin
 Result := copy(fLineBuf, FTokStart, fTokStop - FTokStart);
end;

function TSynD2Syn.GetTokenKind: integer;
begin
 Result := Integer(fTokKind);
end;

function TSynD2Syn.GetTokenPos: Integer;
begin
 Result := fTokStart - 1;
end;

function TSynD2Syn.GetEol: Boolean;
begin
 Result := fTokStop > length(fLineBuf);
end;

procedure TSynD2Syn.SetRange(value: Pointer);
 Var stored: TSynD2SynRange;
begin
 Inherited SetRange(value);

 stored := TSynD2SynRange(CodeFoldRange.RangeType);

 If Not assigned(fCurrRange) or not Assigned(stored) Then
  Exit;

 fCurrRange.copyFrom(stored);
end;

procedure TSynD2Syn.ResetRange;
begin
 If fCurrRange = Nil Then
  fCurrRange := TSynD2SynRange.Create(nil)
 Else
  fCurrRange.Clear;
end;

function TSynD2Syn.GetRange: Pointer;
 Var stored: TSynD2SynRange;
begin
 stored := TSynD2SynRange(inherited GetRange);

 If stored = Nil Then
  stored := TSynD2SynRange.Create(nil);

 stored.copyFrom(fCurrRange);

 CodeFoldRange.RangeType := Pointer(stored);

 Result := inherited GetRange;
end;

class function TSynD2Syn.GetLanguageName: string;
begin
 Result := SYNS_LangD;
end;

{ TSynD2SynRange }

procedure TSynD2SynRange.Assign(source: TSynCustomHighlighterRange);
 Var Rng : TSynD2SynRange;
begin
 Inherited Assign(source);

 If source Is TSynD2SynRange Then
  Begin
   Rng := TSynD2SynRange(source);
   rangeKinds := Rng.rangeKinds;
   namedRegionCount := Rng.namedRegionCount;
  end;
end;

function TSynD2SynRange.Compare(range: TSynCustomHighlighterRange): integer;

 Const cmpRes: Array[Boolean] of Integer = (-1, 1);

 Var src_t: TSynD2SynRange;
begin
 Result := inherited Compare(range);

 If range <> Nil Then
  Begin
   If Result <> 0 Then
    Exit;

   If range Is TSynD2SynRange Then
    Begin
     src_t := TSynD2SynRange(range);

     If src_t.rangeKinds <> rangeKinds Then
      Exit(1);

     If src_t.rString <> rString Then
      Exit(1);

     If src_t.nestedCommentsCount <> nestedCommentsCount Then
      Exit(cmpRes[src_t.nestedCommentsCount > nestedCommentsCount]);

     If src_t.namedRegionCount <> namedRegionCount Then
      Exit(cmpRes[src_t.namedRegionCount > namedRegionCount]);
    end;
  end;
end;

procedure TSynD2SynRange.Clear;
begin
 Inherited Clear;

 nestedCommentsCount := 0;
 namedRegionCount := 0;
 rangeKinds := [];
 rString := False;
end;

procedure TSynD2SynRange.copyFrom(source: TSynD2SynRange);
begin
 If Assigned(source) Then
  Begin
   nestedCommentsCount := source.nestedCommentsCount;
   namedRegionCount := source.namedRegionCount;
   rangeKinds := source.rangeKinds;
   rString := source.rString;
  end;
end;

end.

