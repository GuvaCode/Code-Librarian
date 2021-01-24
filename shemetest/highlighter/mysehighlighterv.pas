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
unit MySEHighlighterV;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

  { TMySEHighlighterV }

  TMySEHighlighterV                      = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;
      tnDataType                         : Integer;

     Protected
      function IsFilterStored: Boolean; override;
      function GetSampleSource: string; override;

     Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;

      class function GetLanguageName: string; override;
    end;

implementation

Uses SynFacilBasic, SynEditStrConst;

Const
     SYNS_FilterV        = 'V files (*.v)|*.v';
     SYNS_LangV          = 'V';

     VKeyWords           = 'as,assert,break,case,chan,const,continue,default,' +
                           'defer,else,enum,fn,for,if,import,in,interface,' +
                           'is,match,module,mut,or,pub,return,struct,switch,' +
                           'type,unsafe,exit,false,none,panic,print,' +
                           'print_backtrace,println,true';

     VDataTypes          = 'any,bool,byte,byteptr,charptr,f32,f64,i128,i16,i32,' +
                           'i64,i8,int,rune,size_t,string,u128,u16,u32,u64,u8,' +
                           'voidptr';

{ TMySEHighlighterV }

function TMySEHighlighterV.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterV;
end;

function TMySEHighlighterV.GetSampleSource: string;
begin
 Result := '/* This is a multiline comment.' + #13#10 +
           '   Second comment line' + #13#10 +
           '*/' + #13#10 +
           '' + #13#10 +
           'fn add(x int, y int) int {' + #13#10 +
           #9'return 0xff + 1.20 + false + None ' + #13#10 +
           '}' + #13#10 +
           '' + #13#10 +
           'interface Speaker {' + #13#10 +
           '' + #13#10 +
           '  println('#39'Hello, $name!'#39')  // `$` is used for string interpolation ' + #13#10 +
           '}' + #13#10 +
           '' + #13#10 +
           'enum Color {' + #13#10 +
           #9'red green blue ' + #13#10 +
           '} ' + #13#10 +
           '' + #13#10 +
           'struct User {' + #13#10 +
           'mut:' + #13#10 +
           #9'is_registered bool ' + #13#10 +
           '}';
end;

constructor TMySEHighlighterV.Create(AOwner: TComponent);
 Var I : Word;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := VKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 tnDataType := NewTokType(SYNS_AttrDataType);

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Clear;
 fKeyWordList.DelimitedText := VDataTypes;

 For I := 0 To fKeyWordList.Count - 1 Do
  AddIdentSpec(fKeyWordList[I], tnDataType);

 fKeyWordList.Free;

 DefTokDelim('''','''', tnString, tdMulLin);
 DefTokDelim('//','', tnComment);
 DefTokDelim('/\*','\*/', tnComment, tdMulLin, False);

 DefTokContent('[0123456789]','[0-9]', tnNumber);
 DefTokContent('0x','[0-9A-Fa-f]*', tnNumber);
 DefTokContent('0b','[0-1]*', tnNumber);
 DefTokContent('0o','[0-7]*', tnNumber);

 fDefaultFilter := SYNS_FilterV;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterV.Destroy;
begin
 Inherited Destroy;
end;

class function TMySEHighlighterV.GetLanguageName: string;
begin
 Result := SYNS_LangV;
end;

end.

