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
unit MySEHighlighterADA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TMySEHighlighterADA }

    TMySEHighlighterADA                  = Class(TSynFacilSyn)
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

Const
     SYNS_FilterADA        = 'Ada Files (*.ada;*.adb)|*.ada;*.adb';
     SYNS_LangADA          = 'Ada';

     ADAKeyWords           = 'abort,abs,abstract,accept,access,aliased,all,and,' +
                             'array,at,begin,body,case,constant,declare,delay,' +
                             'delta,digits,do,else,elsif,end,entry,exception,' +
                             'exit,for,function,generic,goto,if,in,interface,' +
                             'is,limited,loop,mod,new,not,null,of,or,others,out,' +
                             'overriding,package,pragma,private,procedure,' +
                             'protected,raise,range,record,rem,renames,requeue,' +
                             'return,reverse,select,separate,some,subtype,' +
                             'synchronized,tagged,task,terminate,then,type,until,' +
                             'use,when,while,with,xor';


{ TMySEHighlighterADA }

function TMySEHighlighterADA.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterADA;
end;

function TMySEHighlighterADA.GetSampleSource: string;
begin
  Result := '--Simple ADA source code' + #13#10 +
            '' + #13#10 +
            'with Ada.Text_IO; use Ada.Text_IO;' + #13#10 +
            '' + #13#10 +
            'procedure Learn is' + #13#10 +
            '' + #13#10 +
            'subtype Alphabet is Character range ''A'' .. ''Z'';' + #13#10 +
            '' + #13#10 +
            'begin' + #13#10 +
            ' Put_Line ("Learning Ada from " & Alphabet''First & " to " & Alphabet''Last);' + #13#10 +
            'end Learn;';
end;

constructor TMySEHighlighterADA.Create(AOwner: TComponent);
 Var I : Integer;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := ADAKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString);
 DefTokDelim('''','''', tnString);
 DefTokDelim('--','', tnComment);

 DefTokContent('[0123456789]','[0-9]', tnNumber);

 fDefaultFilter := SYNS_FilterADA;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterADA.Destroy;
begin
 Inherited Destroy;
end;

class function TMySEHighlighterADA.GetLanguageName: string;
begin
 Result := SYNS_LangADA;
end;

end.

