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
unit MySEHighlighterGroovy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

  { TMySEHighlighterGroovy }

  TMySEHighlighterGroovy               = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;
      tnPrimitive                        : Integer;

      Procedure AddPrimitive(sPrimitive : String);

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
     SYNS_FilterGroovy        = 'Groovy source files (*.groovy;*.jenkinsfile;*.gradle)|*.groovy;*.jenkinsfile;*.gradle';
     SYNS_LangGroovy          = 'Groovy';

     GroovyKeyWords           = 'as,assert,break,case,catch,class,const,continue,' +
                                'def,default,do,else,enum,extends,finally,for,' +
                                'goto,if,implements,import,in,instanceof,interface,' +
                                'new,package,return,super,switch,throw,throws,' +
                                'trait,try,while,true,false,null,this';

     GroovyPrimitive          = 'byte,char,short,int,long,BigInteger';

{ TMySEHighlighterGroovy }

procedure TMySEHighlighterGroovy.AddPrimitive(sPrimitive: String);
begin
 AddIdentSpec(sPrimitive, tnPrimitive);
end;

function TMySEHighlighterGroovy.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterGroovy;
end;

function TMySEHighlighterGroovy.GetSampleSource: string;
begin
 Result := '// Simple groovy test code' + #13#10 +
           '' + #13#10 +
           'def writer = new StringWriter()' + #13#10 +
           'def builder = new groovy.xml.MarkupBuilder(writer)' + #13#10 +
           'builder.languages {' + #13#10 +
           '  language(year: 1995) {' + #13#10 +
           '    name "Java"' + #13#10 +
           '    paradigm "object oriented"' + #13#10 +
           '    typing "static"' + #13#10 +
           '  }' + #13#10 +
           '  language (year: 1995) {' + #13#10 +
           '    name "Ruby"' + #13#10 +
           '    paradigm "functional, object oriented"' + #13#10 +
           '    typing "duck typing, dynamic"' + #13#10 +
           '  }' + #13#10 +
           '  language (year: 2003) {' + #13#10 +
           '    name "Groovy"' + #13#10 +
           '    paradigm "functional, object oriented"' + #13#10 +
           '    typing "duck typing, dynamic, static"' + #13#10 +
           '  }' + #13#10 +
           '}';
end;

constructor TMySEHighlighterGroovy.Create(AOwner: TComponent);
 Var I : Word;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := GroovyKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 tnPrimitive := NewTokType('Primitive');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Clear;
 fKeyWordList.DelimitedText := GroovyPrimitive;

 For I := 0 To fKeyWordList.Count - 1 Do
  AddPrimitive(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString, tdMulLin);
 DefTokDelim('\\','', tnString);
 DefTokDelim('//','', tnComment);
 DefTokDelim('/\*','\*/', tnComment, tdMulLin, True);

 DefTokContent('[0123456789]','[0-9]', tnNumber);
 DefTokContent('0x','[0-9A-Fa-f]*', tnNumber);
 DefTokContent('0b','[0-1]', tnNumber);

 fDefaultFilter := SYNS_FilterGroovy;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterGroovy.Destroy;
begin
 inherited Destroy;
end;

class function TMySEHighlighterGroovy.GetLanguageName: string;
begin
 Result := SYNS_LangGroovy;
end;

end.

