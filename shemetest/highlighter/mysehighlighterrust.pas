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
unit MySEHighlighterRust;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TMySEHighlighterRust }

    TMySEHighlighterRust               = Class(TSynFacilSyn)
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
     SYNS_FilterRust        = 'Rust source files (*.rs)|*.rs';
     SYNS_LangRust          = 'Rust';

     RustKeyWords           = 'as,break,const,continue,create,else,enum,extern,' +
                              'false,fn,for,if,impl,in,let,loop,match,mod,move,' +
                              'mut,pub,ref,return,self,static,struct,super,trait,' +
                              'true,type,unsafe,use,where,while';

     RustPrimitive          = 'bool.char,str,i8,i16,i32,i64,u8,u16,u32,u64,' +
                              'isize,usize,f32,f64';

{ TMySEHighlighterRust }

procedure TMySEHighlighterRust.AddPrimitive(sPrimitive: String);
begin
 AddIdentSpec(sPrimitive, tnPrimitive);
end;

function TMySEHighlighterRust.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterRust;
end;

function TMySEHighlighterRust.GetSampleSource: string;
begin
 Result := '// Simple Rust code' + #13#10 +
           '' + #13#10 +
           'fn main() {' + #13#10 +
            'let Text = "ByBy";' + #13#10 +
            'let tNumber : u8 = 10;' + #13#10 +
            '' + #13#10 +
            'let mut threads = Vec::new();' + #13#10 +
            'for num in 0..10 {' + #13#10 +
            '    threads.push(thread::spawn(move || {' + #13#10 +
            '        println!("{} from thread number {}", Text, num);' + #13#10 +
            '    }));' + #13#10 +
            '}' + #13#10 +
            '' + #13#10 +
            'for thread in threads {' + #13#10 +
            '    thread.join().unwrap();' + #13#10 +
             '}' + #13#10 +
            '}';
end;

constructor TMySEHighlighterRust.Create(AOwner: TComponent);
 Var I : Word;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := RustKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 tnPrimitive := NewTokType('Primitive');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Clear;
 fKeyWordList.DelimitedText := RustPrimitive;

 For I := 0 To fKeyWordList.Count - 1 Do
  AddPrimitive(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString, tdMulLin);
 DefTokDelim('''','''', tnString);
 DefTokDelim('//','', tnComment);
 DefTokDelim('/\*','\*/', tnComment, tdMulLin, True);

 DefTokContent('[0123456789]','[0-9]', tnNumber);
 DefTokContent('0x','[0-9A-Fa-f]*', tnNumber);
 DefTokContent('0b','[0-1]', tnNumber);
 DefTokContent('0o','[0-7]', tnNumber);

 fDefaultFilter := SYNS_FilterRust;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterRust.Destroy;
begin
 inherited Destroy;
end;

class function TMySEHighlighterRust.GetLanguageName: string;
begin
 Result := SYNS_LangRust;
end;

end.

