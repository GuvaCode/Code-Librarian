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
unit MySEHighlighterFSharp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

  { TMySEHighlighterFSharp }

  TMySEHighlighterFSharp               = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;
      tnPreProcessor                     : Integer;
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

     SYNS_FilterFSharp        = 'F# files (*.fs)|*.fs';
     SYNS_LangFSharp          = 'F#';

     FSharpKeyWords           = 'abstract,and,as,asr,assert,atomic,base,begin,' +
                                'break,checked,class,component,const,constraint,' +
                                'constructor,continue,default,delegate,do,done,' +
                                'downcast,downto,eager,elif,else,end,event,' +
                                'exception,extern,external,false,finally,fixed,' +
                                'for,fun,function,functor,global,if,in,include,' +
                                'inherit,inline,interface,internal,land,lazy,' +
                                'let,let!,lor,lsl,lsr,lxor,match,member,method,' +
                                'mixin,mod,module,mutable,namespace,new,not,null,' +
                                'object,of,open,or,override,parallel,private,' +
                                'process,protected,public,pure,rec,return,return!,' +
                                'sealed,select,sig,static,struct,tailcall,then,' +
                                'to,trait,true,try,type,upcast,use,use!,val,' +
                                'virtual,void,volatile,when,while,with,yield,' +
                                'yield!';

     FSharpDataTypes          = 'bool,byte,sbyte,int16,uint16,int,uint,int64,' +
                                'uint64,nativeint,unativeint,decimal,float,' +
                                'double,float32,single,char,string,unit';


{ TMySEHighlighterFSharp }

function TMySEHighlighterFSharp.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterFSharp;
end;

function TMySEHighlighterFSharp.GetSampleSource: string;
begin
 Result := '#light' + #13#10 +
           '(* Comment' + #13#10 +
           '*)' + #13#10 +
           '' + #13#10 +
           'let people = ["Granville"; "John"]' + #13#10 +
           '' + #13#10 +
           'let! printPeople p =' + #13#10 +
           '    List.iter print_string p' + #13#10 +
           '' + #13#10 +
           'printPeople addAFewMore' + #13#10 +
           '' + #13#10 +
           'let numbers = [[1;2;3];[4;5;6]]' + #13#10 +
           '' + #13#10 +
           'let rec concat l = ' + #13#10 +
           '    match l with' + #13#10 +
           '    | head::tail -> head @ (concat tail)' + #13#10 +
           '    | [] -> []' + #13#10 +
           '    ' + #13#10 +
           'let cross_tup ls1 ls2 =    ' + #13#10 +
           '  ls2 |> List.map ( fun el2 -> ' + #13#10 +
           '    ls1 |> List.map ( fun el1 -> (el1, el2) ) ) |> List.concat';
end;

constructor TMySEHighlighterFSharp.Create(AOwner: TComponent);
 Var I : Word;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := FSharpKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 tnPreProcessor := NewTokType(SYNS_AttrPreprocessor);
 tnDataType := NewTokType(SYNS_AttrDataType);

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Clear;
 fKeyWordList.DelimitedText := FSharpDataTypes;

 For I := 0 To fKeyWordList.Count - 1 Do
  AddIdentSpec(fKeyWordList[I], tnDataType);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString, tdMulLin);
 //DefTokDelim('''','''', tnString);
 DefTokDelim('//','', tnComment);
 DefTokDelim('#','', tnPreProcessor);
 DefTokDelim('\(\*','\*\)', tnComment, tdMulLin);

 DefTokContent('[0123456789]','[0-9]', tnNumber);
 DefTokContent('0x','[0-9A-Fa-f]*', tnNumber);
 DefTokContent('0b','[0-1]*', tnNumber);

 fDefaultFilter := SYNS_FilterFSharp;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterFSharp.Destroy;
begin
  inherited Destroy;
end;

class function TMySEHighlighterFSharp.GetLanguageName: string;
begin
 Result := SYNS_LangFSharp;
end;

end.

