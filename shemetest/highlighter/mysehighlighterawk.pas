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
unit MySEHighlighterAwk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TMySEHighlighterAwk }

    TMySEHighlighterAwk                   = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;
      tnVariable                         : Integer;

      Procedure AddVariable(sVariable : String);

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
     SYNS_FilterAwk        = 'AWK script files (*.awk)|*.awk';
     SYNS_LangAwk          = 'AWK Script';

     AwkKeyWords           = 'break,case,continue,do,else,exit,function,for,if,' +
                             'in,next,return,switch,while,@include,delete,' +
                             'nextfile,BEGIN,END,BEGINFILE,ENDFILE';

     AwkVariable           = 'ARGC,ARGIND,ARGV,FILENAME,' +
                             'FNR,FS,NF,NR,OFMT,OFS,ORS,RLENGTH,RS,RSTART,' +
                             'SUBSEP,BINMODE,CONVFMT,FIELDWIDTHS,FPAT,' +
                             'IGNORECASE,LINT,TEXTDOMAiN,ENVIRON,ERRNO,PROCINFO,RT';

{ TMySEHighlighterAwk }

procedure TMySEHighlighterAwk.AddVariable(sVariable : String);
begin
 AddIdentSpec(sVariable, tnVariable);
end;

function TMySEHighlighterAwk.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterAwk;
end;

function TMySEHighlighterAwk.GetSampleSource: string;
begin
 Result := '# Simple SWK code' + #13#10 +
           '' + #13#10 +
           'BEGIN {' + #13#10 +
           '    pattern = ARGV[1]' + #13#10 +
           '    for (i = 1; i < ARGC; i++) # remove first argument' + #13#10 +
           '        ARGV[i] = ARGV[i + 1]' + #13#10 +
           '    ARGC--' + #13#10 +
           '    if (ARGC == 1) { # the pattern was the only thing, so force read from standard input (used by book)' + #13#10 +
           '        ARGC = 2' + #13#10 +
           '        ARGV[1] = "-"' + #13#10 +
           '    }' + #13#10 +
           '}';
end;

constructor TMySEHighlighterAwk.Create(AOwner: TComponent);
 Var I : Word;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := AwkKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 tnVariable := NewTokType(SYNS_AttrSpecialVariable);

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Clear;
 fKeyWordList.DelimitedText := AwkVariable;

 For I := 0 To fKeyWordList.Count - 1 Do
  AddVariable(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString, tdMulLin);
 DefTokDelim('''','''', tnString);
 DefTokDelim('#','', tnComment);
 //DefTokDelim('/\*','\*/', tnComment, tdMulLin, True);

 DefTokContent('[0123456789]','[0-9]', tnNumber);
 DefTokContent('0x','[0-9A-Fa-f]*', tnNumber);

 fDefaultFilter := SYNS_FilterAwk;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterAwk.Destroy;
begin
  inherited Destroy;
end;

class function TMySEHighlighterAwk.GetLanguageName: string;
begin
 Result := SYNS_LangAwk;
end;

end.

