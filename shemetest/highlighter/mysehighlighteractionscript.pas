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
unit MySEHighlighterActionScript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TMySEHighlighterActionScript }

    TMySEHighlighterActionScript                  = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;

     Protected
      function IsFilterStored: Boolean; override;
      function GetSampleSource: string; override;

     Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;

      class function GetLanguageName: string; override;
    end;

implementation

Uses SynFacilBasic;

Const
     SYNS_FilterAS        = 'Adobe ActionScript Files (*.fla)|*.fla';
     SYNS_LangAS          = 'Adobe ActionScript';

     ASKeyWords           = 'break,case,continue,default,do,while,else,for,in,' +
                            'each,if,label,return,super,switch,throw,try,catch,' +
                            'finally,with,dynamic,final,internal,native,override,' +
                            'private,protected,public,static,rest,class,const,' +
                            'extends,function,get,implements,interface,namespace,' +
                            'package,set,var,import,include,use,false,null,this,' +
                            'true,int,string,object,number,array,boolean,uint,' +
                            'void';

{ TMySEHighlighterActionScript }

function TMySEHighlighterActionScript.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterAS;
end;

function TMySEHighlighterActionScript.GetSampleSource: string;
begin
  Result := '//Simple ActionScript test code' + #13#10 +
            '' + #13#10 +
            'var testArray:Array = new Array();' + #13#10 +
            'testArray[0] = "fee";' + #13#10 +
            'testArray[1] = "fi";' + #13#10 +
            'testArray[4] = "foo";' + #13#10 +
            '' + #13#10 +
            'for (i = 0; i < 6; i++) {' + #13#10 +
            '  if (testArray[i] == null) {' + #13#10 +
            '  trace("testArray[" + i + "] == null");' + #13#10 +
            ' }' + #13#10 +
            '}';
end;

constructor TMySEHighlighterActionScript.Create(AOwner: TComponent);
 Var I : Integer;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := ASKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString);
 DefTokDelim('''','''', tnString);
 DefTokDelim('//','', tnComment);
 DefTokDelim('/\*', '\*/', tnComment, tdMulLin, False);
 AddBlock('{','}', False);

 DefTokContent('[0123456789]','[0-9]', tnNumber);
 DefTokContent('0x','[0-9a-fA-F]', tnNumber);

 fDefaultFilter := SYNS_FilterAS;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterActionScript.Destroy;
begin
  inherited Destroy;
end;

class function TMySEHighlighterActionScript.GetLanguageName: string;
begin
 Result := SYNS_LangAS;
end;

end.

