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
unit MySEHighlighterVerilog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TMySEHighlighterVerilog }

    TMySEHighlighterVerilog               = Class(TSynFacilSyn)
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
     SYNS_FilterVL        = 'Verilog source files (*.v;*.vh)|*.v;*.vh';
     SYNS_LangVL          = 'Verilog';

     VLKeyWords           = 'always,assign,automatic,begin,case,casex,casez,cell,' +
                            'config,deassign,default,defparam,design,disable,edge,' +
                            'else,end,endcase,endconfig,endfunction,endgenerate,' +
                            'endmodule,endprimitive,endspecify,endtable,endtask,' +
                            'event,for,force,forever,fork,function,generate,genvar,' +
                            'if,ifnone,incdir,include,initial,inout,input,instance,' +
                            'join,liblist,library,localparam,macromodule,module,' +
                            'negedge,noshowcancelled,output,parameter,posedge,' +
                            'primitive,pulsestyle_ondetect,pulsestyle_onevent,' +
                            'reg,release,repeat,scalared,showcancelled,signed,' +
                            'specify,specparam,strength,table,task,tri,tri0,tri1,' +
                            'triand,wand trior,wor,trireg,unsigned,use,vectored,' +
                            'wait,while,wire,or';

{ TMySEHighlighterVerilog }

function TMySEHighlighterVerilog.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterVL;
end;

function TMySEHighlighterVerilog.GetSampleSource: string;
begin
 Result := 'initial' + #13#10 +
           ' fork' + #13#10 +
           '   $write("A"); // Print Char A' + #13#10 +
           '   $write("B"); // Print Char B' + #13#10 +
           '   begin' + #13#10 +
           '     #1; // Wait 1 time unit' + #13#10 +
           '     $write("C");// Print Char C' + #13#10 +
           '   end' + #13#10 +
           ' join';
end;

constructor TMySEHighlighterVerilog.Create(AOwner: TComponent);
 Var I : Integer;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := VLKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString);
 DefTokDelim('//','', tnComment);

 DefTokContent('$', '[A-Za-z]*', tnDirective);
 DefTokContent('[0123456789]','[0-9]', tnNumber);
 DefTokContent('''','[0-9A-FHOa-fho]*', tnNumber);

 fDefaultFilter := SYNS_FilterVL;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterVerilog.Destroy;
begin
  inherited Destroy;
end;

class function TMySEHighlighterVerilog.GetLanguageName: string;
begin
 Result := SYNS_LangVL;
end;

end.

