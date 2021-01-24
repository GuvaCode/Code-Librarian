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
unit umysynhighlighterpo;

interface

uses
  Classes, SysUtils,
  Graphics,
  SynEditTypes, SynEditHighlighter, SynEditStrConst;

type
  TtkTokenKind = (tkComment, tkText, tkKey, tkNull, tkSpace, tkString,
                  tkIdentifier, tkPrevValue, tkFlags, tkUnknown);

  TProcTableProc = procedure of object;

type

  { TSynPoSyn }

  TSynPoSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fIdentAttri: TSynHighlighterAttributes;
    fPrevAttri: TSynHighlighterAttributes;
    fFlagAttri: TSynHighlighterAttributes;
    procedure IdentProc;
    procedure KeyProc;
    procedure CRProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure HashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure MakeMethodTables;
  protected
    {General Stuff}
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: String; override;
  public
    class function GetLanguageName: string; override;
    function IsKeyword(const AKeyword: string): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property TextAttri   : TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property KeyAttri    : TSynHighlighterAttributes read fKeyAttri
      write fKeyAttri;
    property SpaceAttri  : TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
  end;

implementation



const
  PoKeysCount = 3;
  PoKeys: array[1..PoKeysCount] of string = (
    'msgid', 'msgstr', 'msgctxt');


procedure TSynPoSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      #0      : fProcTable[i] := @NullProc;
      #10 {LF}: fProcTable[i] := @LFProc;
      #13 {CR}: fProcTable[i] := @CRProc;
      #34 {"} : fProcTable[i] := @StringProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      '#' {#} : fProcTable[i] := @HashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[i] := @SpaceProc;
    else
      fProcTable[i] := @TextProc;
    end;
end;

constructor TSynPoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri            := TSynHighlighterAttributes.Create(@SYNS_AttrComment);
  fCommentAttri.Style      := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fTextAttri               := TSynHighlighterAttributes.Create(@SYNS_AttrText);
  AddAttribute(fTextAttri);

  fKeyAttri                := TSynHighlighterAttributes.Create(@SYNS_AttrKey);
  fKeyAttri.Foreground     := clBlue;
  fKeyAttri.Style          := [fsBold];
  AddAttribute(fKeyAttri);

  fIdentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  fIdentAttri.Foreground   := clGreen;
  fIdentAttri.Style        := [fsBold];
  AddAttribute(fIdentAttri);

  fPrevAttri  := TSynHighlighterAttributes.Create(@SYNS_AttrPrevValue, SYNS_XML_AttrPrevValue);
  fPrevAttri.Foreground    := clOlive;
  fPrevAttri.Style         := [fsItalic];
  AddAttribute(fPrevAttri);

  fFlagAttri  := TSynHighlighterAttributes.Create(@SYNS_AttrFlags, SYNS_XML_AttrFlags);
  fFlagAttri.Foreground    := clTeal;
  AddAttribute(fFlagAttri);

  fSpaceAttri              := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri             := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  fStringAttri.Foreground  := clFuchsia;
  AddAttribute(fStringAttri);

  SetAttributesOnChange(@DefHighlightChange);

  fDefaultFilter      := SYNS_FilterPo;
  MakeMethodTables;
end; { Create }

procedure TSynPoSyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;


procedure TSynPoSyn.IdentProc;
begin
  while fLine[Run] in GetIdentChars {['A'..'Z','a'..'z']} do inc(Run);
  if IsKeyWord(GetToken) then begin
    fTokenId := tkKey;
    Exit;
  end
  else fTokenId := tkUnknown;
end;

procedure TSynPoSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;


procedure TSynPoSyn.KeyProc;
begin
  fTokenID := tkKey;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      #32: break;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynPoSyn.TextProc;
begin
  if Run = 0 then
    IdentProc
  else begin
    inc(Run);
    while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
     ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @TextProc)) do inc(Run);
    fTokenID := tkText;
  end;
end;

procedure TSynPoSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPoSyn.NullProc;
begin
  fTokenID := tkNull;
end;



procedure TSynPoSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;


procedure TSynPoSyn.StringProc;
var
  FirstQuotePos, LastQuotePos: longint;
begin
  FirstQuotePos := Run;
  LastQuotePos := FirstQuotePos;
  fTokenID := tkString;
  while FLine[Run] <> #0 do
  begin
    case FLine[Run] of
      #10, #13: break;
      #34: if (Run <= 0) or (FLine[Run - 1] <> '\') then LastQuotePos := Run;
    end;
    inc(Run);
  end;
  if FirstQuotePos <> LastQuotePos then
    Run := LastQuotePos;
  if FLine[Run] <> #0 then
    inc(Run);
end;


procedure TSynPoSyn.HashProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    fTokenID := tkText;
    inc(Run);
    Exit;
  end;

  // this is column 0 --> ok
  fTokenID := tkComment;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      #10: break;
      #13: break;
      ':': begin if (Run = 1) then fTokenId := tkIdentifier; Inc(Run) end;
      ',': begin if (Run = 1) then  fTokenId := tkFlags;  Inc(Run) end;
      '|': begin if (Run = 1) then  fTokenId := tkPrevValue; Inc(Run) end;
    else inc(Run);
    end;
end;

procedure TSynPoSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynPoSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynPoSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynPoSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynPoSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart := FLine + fTokenPos;
end;

function TSynPoSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPoSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkText   : Result := fTextAttri;
    tkKey    : Result := fKeyAttri;
    tkSpace  : Result := fSpaceAttri;
    tkString : Result := fStringAttri;
    tkIdentifier: Result := fIdentAttri;
    tkFlags:       Result := fFlagAttri;
    tkPrevValue:  Result := fPrevAttri;
    tkUnknown: Result := fTextAttri;
    else Result := nil;
  end;
end;

function TSynPoSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynPoSyn.GetTokenPos: Integer;
begin
 Result := fTokenPos;
end;

function TSynPoSyn.GetIdentChars: TSynIdentChars;
begin
  Result := [#33..#255];
end;

class function TSynPoSyn.GetLanguageName: string;
begin
  Result := 'Po language';
end;

function TSynPoSyn.GetSampleSource: String;
begin
  Result := '"Project-Id-Version: \n"' + LineEnding +
            '"POT-Creation-Date: \n"' + LineEnding +
            '"MIME-Version: 1.0\n"' + LineEnding +
            '"Content-Type: text/plain; charset=UTF-8\n"' + LineEnding +
            '"Content-Transfer-Encoding: 8bit\n"' + LineEnding +
            LineEnding +
            '#: lazarusidestrconsts.dlgcochecks' + LineEnding +
            '#, fuzzy' + LineEnding +
            '#| msgid "Checks:"' + LineEnding +
            'msgid "Checks"' + LineEnding +
            'msgstr "Controleert:"' + LineEnding +
            LineEnding +
            '#: lazarusidestrconsts.listemplateeditparamcellhelp' + LineEnding +
            'msgid ""' + LineEnding +
            '"Inserts an editable Cell, with a default value\n"' + LineEnding +
            '"\"\",Sync=n (,S=n), to Sync with a previous cell (n=1 to highest prev cell\n"' + LineEnding +
            '"\"default\",Sync, to Sync with a previous cell of equal default\n"' + LineEnding +
            'msgstr ""';

end;

function TSynPoSyn.IsKeyword(const AKeyword: string): boolean;
var
  Token: String;
  i: Integer;
begin
  //There are only 3 keywords, so no need to make a hashtable
  Token := LowerCase(AKeyWord);
  for i := 1 to PoKeysCount do if (PoKeys[i] = Token) then
  begin
    Exit(True);
  end;
  Result := False;
end;

end.

