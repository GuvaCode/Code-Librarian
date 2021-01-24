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
unit MySEHighlighterPowerShell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TSEHighlighterPowerShell }

    TSEHighlighterPowerShell              = Class(TSynFacilSyn)
      Private
        fKeyWordList                       : TStringList;

        Procedure AddDirective(sDirective : String);

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
     SYNS_FilterPowerShell          = 'PowerShell script (*.ps1)|*.ps1';
     SYNS_LangPowerShell            = 'PowerShell';

     PowerShellKeyWords             = 'Begin,Break,Catch,Continue,Data,Do,' +
                                      'Dynamicparam,Else,Elseif,End,Exit,' +
                                      'Filter,Finally,For,Foreach,From,' +
                                      'Function,If,In,Local,Param,Private,' +
                                      'Process,Return,Switch,Throw,Trap,Try,' +
                                      'Until,Where,While';

     PowerShellDirective            =  'add-content,' +
                                       'add-history,' +
                                       'add-member,' +
                                       'add-pssnapin,' +
                                       'clear-content,' +
                                       'clear-item,' +
                                       'clear-itemproperty,' +
                                       'clear-variable,' +
                                       'compare-object,' +
                                       'convertfrom-securestring,' +
                                       'convert-path,' +
                                       'convertto-html,' +
                                       'convertto-securestring,' +
                                       'copy-item,' +
                                       'copy-itemproperty,' +
                                       'export-alias,' +
                                       'export-clixml,' +
                                       'export-console,' +
                                       'export-csv,' +
                                       'foreach-object,' +
                                       'format-custom,' +
                                       'format-list,' +
                                       'format-table,' +
                                       'format-wide,' +
                                       'get-acl,' +
                                       'get-alias,' +
                                       'get-authenticodesignature,' +
                                       'get-childitem,' +
                                       'get-command,' +
                                       'get-content,' +
                                       'get-credential,' +
                                       'get-culture,' +
                                       'get-date,' +
                                       'get-eventlog,' +
                                       'get-executionpolicy,' +
                                       'get-help,' +
                                       'get-history,' +
                                       'get-host,' +
                                       'get-item,' +
                                       'get-itemproperty,' +
                                       'get-location,' +
                                       'get-member,' +
                                       'get-pfxcertificate,' +
                                       'get-process,' +
                                       'get-psdrive,' +
                                       'get-psprovider,' +
                                       'get-pssnapin,' +
                                       'get-service,' +
                                       'get-tracesource,' +
                                       'get-uiculture,' +
                                       'get-unique,' +
                                       'get-variable,' +
                                       'get-wmiobject,' +
                                       'group-object,' +
                                       'import-alias,' +
                                       'import-clixml,' +
                                       'import-csv,' +
                                       'invoke-expression,' +
                                       'invoke-history,' +
                                       'invoke-item,' +
                                       'join-path,' +
                                       'measure-command,' +
                                       'measure-object,' +
                                       'move-item,' +
                                       'move-itemproperty,' +
                                       'new-alias,' +
                                       'new-item,' +
                                       'new-itemproperty,' +
                                       'new-object,' +
                                       'new-psdrive,' +
                                       'new-service,' +
                                       'new-timespan,' +
                                       'new-variable,' +
                                       'out-default,' +
                                       'out-file,' +
                                       'out-host,' +
                                       'out-null,' +
                                       'out-printer,' +
                                       'out-string,' +
                                       'pop-location,' +
                                       'push-location,' +
                                       'read-host,' +
                                       'remove-item,' +
                                       'remove-itemproperty,' +
                                       'remove-psdrive,' +
                                       'remove-pssnapin,' +
                                       'remove-variable,' +
                                       'rename-item,' +
                                       'rename-itemproperty,' +
                                       'resolve-path,' +
                                       'restart-service,' +
                                       'resume-service,' +
                                       'select-object,' +
                                       'select-string,' +
                                       'set-acl,' +
                                       'set-alias,' +
                                       'set-authenticodesignature,' +
                                       'set-content,' +
                                       'set-date,' +
                                       'set-executionpolicy,' +
                                       'set-item,' +
                                       'set-itemproperty,' +
                                       'set-location,' +
                                       'set-psdebug,' +
                                       'set-service,' +
                                       'set-tracesource,' +
                                       'set-variable,' +
                                       'sort-object,' +
                                       'split-path,' +
                                       'start-service,' +
                                       'start-sleep,' +
                                       'start-transcript,' +
                                       'stop-process,' +
                                       'stop-service,' +
                                       'stop-transcript,' +
                                       'suspend-service,' +
                                       'tee-object,' +
                                       'test-path,' +
                                       'trace-command,' +
                                       'update-formatdata,' +
                                       'update-typedata,' +
                                       'where-object,' +
                                       'write-debug,' +
                                       'write-error,' +
                                       'write-host,' +
                                       'write-output,' +
                                       'write-progress,' +
                                       'write-verbose,' +
                                       'write-warning';


{ TSEHighlighterPowerShell }

procedure TSEHighlighterPowerShell.AddDirective(sDirective: String);
begin
 AddIdentSpec(sDirective, tnDirective);
end;

function TSEHighlighterPowerShell.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterPowerShell;
end;

function TSEHighlighterPowerShell.GetSampleSource: string;
begin
 Result := '# Convert any text file to UTF-8' + #13#10 +
           'param( [string] $infile = $(throw "Please specify a filename.") )' + #13#10 +
           '$outfile = "$infile.utf8"' + #13#10 +
           'get-content -Path $infile | out-file $outfile -encoding utf8' + #13#10 +
           '' + #13#10 +
           '$s = "My name is `"Joe"", hi" + ''Joe''''test'' test' + #13#10 +
           '$string = @"' + #13#10 +
           'one' + #13#10 +
           '"@' + #13#10 +
           '' + #13#10 +
           '<# Block comment' + #13#10 +
           '#>';
end;

constructor TSEHighlighterPowerShell.Create(AOwner: TComponent);
 Var I : Integer;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := PowerShellKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Clear;
 fKeyWordList.DelimitedText := PowerShellDirective;

 For I := 0 To fKeyWordList.Count - 1 Do
  AddDirective(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString);
 DefTokDelim('''','''', tnString);
 DefTokDelim('`','', tnString);
 DefTokDelim('@"','"@', tnString, tdMulLin);
 DefTokDelim('#','', tnComment);
 DefTokDelim('<#','#>', tnComment, tdMulLin);

 //DefTokContent('$', '[A-Za-z]*', tnIdentif);
 DefTokContent('[0123456789]','[0-9]', tnNumber);

 fDefaultFilter := SYNS_FilterPowerShell;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TSEHighlighterPowerShell.Destroy;
begin
  inherited Destroy;
end;

class function TSEHighlighterPowerShell.GetLanguageName: string;
begin
 Result := SYNS_LangPowerShell;
end;

end.

