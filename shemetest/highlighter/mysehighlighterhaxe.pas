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
unit MySEHighlighterHaxe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TMySEHighlighterHaxe }

    TMySEHighlighterHaxe                  = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;
      tnTypeKeywords,
      tnPreProcessor                     : Integer;

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
     SYNS_FilterHaxe        = 'Haxe script files (*.hx)|*.hx';
     SYNS_LangHaxe          = 'Haxe Script';

     HaxeKeyWords           = 'break,callback,case,cast,catch,continue,' +
                              'default,do,dynamic,else,extends,extern,' +
                              'false,for,function,here,if,implements,import,in,' +
                              'inline,interface,never,new,null,override,package,' +
                              'private,public,return,static,super,switch,this,' +
                              'throw,trace,true,try,typedef,untyped,using,var,while';

     HaxeTypeKeyword        = 'Class,Enum,Void,Float,Int,UInt,Bool,' +
                              'Iterator,Iterable,Array,Date,Hash,EReg,IntHash,' +
                              'IntIter,List,String,StringBuf,Xml';

{ TMySEHighlighterHaxe }

function TMySEHighlighterHaxe.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterHaxe;
end;

function TMySEHighlighterHaxe.GetSampleSource: string;
begin
 Result := '// Sample Haxe source' + #13#10 +
           'class FooBar {' + #13#10 +
           '	public var foo:Int;' + #13#10 +
           '	public var bar:String;' + #13#10 +
           '' + #13#10 +
           '	public function new() {' + #13#10 +
           '		foo = 1;' + #13#10 +
           '		bar = "2";' + #13#10 +
           '' + #13#10 +
           '            #if !debug' + #13#10 +
           '            trace("ok");' + #13#10 +
           '            #end' + #13#10 +
           '' + #13#10 +
           '	}' + #13#10 +
           '' + #13#10 +
           '	function anyFooBar(v:{foo:Int, bar:String})' + #13#10 +
           '		trace(v.foo);' + #13#10 +
           '' + #13#10 +
           '	static function test() {' + #13#10 +
           '		var fb = new FooBar();' + #13#10 +
           '		fb.anyFooBar(fb);' + #13#10 +
           '		fb.anyFooBar({foo: 123, bar: "456"});' + #13#10 +
           '	}' + #13#10 +
           '}';
end;

constructor TMySEHighlighterHaxe.Create(AOwner: TComponent);
 Var I : Word;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := HaxeKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 tnTypeKeywords := NewTokType('Type keyword');
 tnPreProcessor := NewTokType(SYNS_AttrPreprocessor);

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Clear;
 fKeyWordList.DelimitedText := HaxeTypeKeyword;

 For I := 0 To fKeyWordList.Count - 1 Do
  AddIdentSpec(fKeyWordList[I], tnTypeKeywords);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString, tdMulLin);
 //DefTokDelim('''','''', tnString);
 DefTokDelim('//','', tnComment);
 DefTokDelim('#','', tnPreProcessor);
 //DefTokDelim('/\*','\*/', tnComment, tdMulLin, True);

 DefTokContent('[0123456789]','[0-9]', tnNumber);
 DefTokContent('0x','[0-9A-Fa-f]*', tnNumber);

 fDefaultFilter := SYNS_FilterHaxe;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterHaxe.Destroy;
begin
  inherited Destroy;
end;

class function TMySEHighlighterHaxe.GetLanguageName: string;
begin
 Result := SYNS_LangHaxe;
end;

end.

