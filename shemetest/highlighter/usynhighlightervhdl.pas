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
unit uSynHighlighterVhdl;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, SynEditHighlighter, SynEditTypes, SynEditStrConst;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type

  { TSynVhdlSyn }

  TSynVhdlSyn = class(TSynCustomHighLighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: longint;
    fStringLen: integer;
    fToIdent: PChar;
    fTokenPos: integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..152] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighLighterAttributes;
    fDirecAttri: TSynHighLighterAttributes;
    fIdentifierAttri: TSynHighLighterAttributes;
    fKeyAttri: TSynHighLighterAttributes;
    fNumberAttri: TSynHighLighterAttributes;
    fSpaceAttri: TSynHighLighterAttributes;
    fStringAttri: TSynHighLighterAttributes;
    fSymbolAttri: TSynHighLighterAttributes;
    function KeyHash(ToHash: PChar): integer;
    function KeyComp(const aKey: string): boolean;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func152: TtkTokenKind;
    procedure AndSymbolProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure DirectiveProc;
    procedure DotProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure QuestionProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure WaveProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
      override;
    function GetEOL: boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: string; LineNumber: integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
       override;
    function GetTokenAttribute: TSynHighLighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    property IdentChars;
    class function GetLanguageName: string; override;
  published
    property CommentAttri: TSynHighLighterAttributes read fCommentAttri write fCommentAttri;
    property DirectiveAttri: TSynHighLighterAttributes read fDirecAttri write fDirecAttri;
    property IdentifierAttri: TSynHighLighterAttributes
      read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighLighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighLighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighLighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighLighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighLighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

uses LazUTF8;

Const
     SYNS_FilterVHDL               =  'VHDL Files (*.vhdl)|*.vhdl';
     SYNS_LangVHDL                 =  'VHDL';

var
  Identifiers: array[#0..#255] of bytebool;
  mHashTable: array[#0..#255] of integer;

procedure MakeIdentTable;
var
  I, J: char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
      else
        Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
        else
          mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynVhdlSyn.InitIdent;
var
  I: integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
  begin
    pF^ := @AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[15]  := @Func15;
  fIdentFuncTable[19]  := @Func19;
  fIdentFuncTable[21]  := @Func21;
  fIdentFuncTable[22]  := @Func22;
  fIdentFuncTable[23]  := @Func23;
  fIdentFuncTable[25]  := @Func25;
  fIdentFuncTable[28]  := @Func28;
  fIdentFuncTable[29]  := @Func29;
  fIdentFuncTable[30]  := @Func30;
  fIdentFuncTable[32]  := @Func32;
  fIdentFuncTable[33]  := @Func33;
  fIdentFuncTable[35]  := @Func35;
  fIdentFuncTable[36]  := @Func36;
  fIdentFuncTable[37]  := @Func37;
  fIdentFuncTable[38]  := @Func38;
  fIdentFuncTable[39]  := @Func39;
  fIdentFuncTable[41]  := @Func41;
  fIdentFuncTable[42]  := @Func42;
  fIdentFuncTable[43]  := @Func43;
  fIdentFuncTable[44]  := @Func44;
  fIdentFuncTable[45]  := @Func45;
  fIdentFuncTable[46]  := @Func46;
  fIdentFuncTable[47]  := @Func47;
  fIdentFuncTable[49]  := @Func49;
  fIdentFuncTable[50]  := @Func50;
  fIdentFuncTable[51]  := @Func51;
  fIdentFuncTable[53]  := @Func53;
  fIdentFuncTable[55]  := @Func55;
  fIdentFuncTable[56]  := @Func56;
  fIdentFuncTable[57]  := @Func57;
  fIdentFuncTable[58]  := @Func58;
  fIdentFuncTable[59]  := @Func59;
  fIdentFuncTable[60]  := @Func60;
  fIdentFuncTable[61]  := @Func61;
  fIdentFuncTable[62]  := @Func62;
  fIdentFuncTable[63]  := @Func63;
  fIdentFuncTable[64]  := @Func64;
  fIdentFuncTable[66]  := @Func66;
  fIdentFuncTable[69]  := @Func69;
  fIdentFuncTable[70]  := @Func70;
  fIdentFuncTable[71]  := @Func71;
  fIdentFuncTable[75]  := @Func75;
  fIdentFuncTable[76]  := @Func76;
  fIdentFuncTable[77]  := @Func77;
  fIdentFuncTable[79]  := @Func79;
  fIdentFuncTable[82]  := @Func82;
  fIdentFuncTable[83]  := @Func83;
  fIdentFuncTable[85]  := @Func85;
  fIdentFuncTable[88]  := @Func88;
  fIdentFuncTable[91]  := @Func91;
  fIdentFuncTable[92]  := @Func92;
  fIdentFuncTable[93]  := @Func93;
  fIdentFuncTable[95]  := @Func95;
  fIdentFuncTable[96]  := @Func96;
  fIdentFuncTable[101] := @Func101;
  fIdentFuncTable[102] := @Func102;
  fIdentFuncTable[105] := @Func105;
  fIdentFuncTable[106] := @Func106;
  fIdentFuncTable[108] := @Func108;
  fIdentFuncTable[115] := @Func115;
  fIdentFuncTable[116] := @Func116;
  fIdentFuncTable[123] := @Func123;
  fIdentFuncTable[124] := @Func124;
  fIdentFuncTable[131] := @Func131;
  fIdentFuncTable[141] := @Func141;
  fIdentFuncTable[152] := @Func152;
end;

function TSynVhdlSyn.KeyHash(ToHash: PChar): integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynVhdlSyn.KeyComp(const aKey: string): boolean;
var
  I: integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end
  else
    Result := False;
end;

function TSynVhdlSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func19: TtkTokenKind;
begin
  if KeyComp('and') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func21: TtkTokenKind;
begin
  if KeyComp('of') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func22: TtkTokenKind;
begin
  if KeyComp('abs') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func23: TtkTokenKind;
begin
  if KeyComp('in') then Result := tkKey
  else if KeyComp('end') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func25: TtkTokenKind;
begin
  if KeyComp('all') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func28: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey
  else if KeyComp('is') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func29: TtkTokenKind;
begin
  if KeyComp('on') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func30: TtkTokenKind;
begin
  if KeyComp('map') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func32: TtkTokenKind;
begin
  if KeyComp('label') then Result := tkKey
  else if KeyComp('file') then Result := tkKey
  else if KeyComp('mod') then Result := tkKey
  else if KeyComp('sla') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func33: TtkTokenKind;
begin
  if KeyComp('nand') then Result := tkKey
  else if KeyComp('or') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func35: TtkTokenKind;
begin
  if KeyComp('to') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func36: TtkTokenKind;
begin
  if KeyComp('rem') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func37: TtkTokenKind;
begin
  if KeyComp('begin') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func38: TtkTokenKind;
begin
  if KeyComp('sra') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func39: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func41: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func42: TtkTokenKind;
begin
  if KeyComp('new') then Result := tkKey
  else if KeyComp('alias') then Result := tkKey
  else if KeyComp('bus') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func43: TtkTokenKind;
begin
  if KeyComp('block') then Result := tkKey
  else if KeyComp('sll') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func44: TtkTokenKind;
begin
  if KeyComp('package') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func45: TtkTokenKind;
begin
  if KeyComp('rol') then Result := tkKey
  else if KeyComp('use') then Result := tkKey
  else if KeyComp('range') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func46: TtkTokenKind;
begin
  if KeyComp('body') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func47: TtkTokenKind;
begin
  if KeyComp('then') then Result := tkKey
  else if KeyComp('defined') then Result := tkKey
  else if KeyComp('nor') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func49: TtkTokenKind;
begin
  if KeyComp('srl') then Result := tkKey
  else if KeyComp('not') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func50: TtkTokenKind;
begin
  if KeyComp('after') then Result := tkKey
  else if KeyComp('when') then Result := tkKey
  else if KeyComp('open') then Result := tkKey
  else if KeyComp('access') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func51: TtkTokenKind;
begin
  if KeyComp('ror') then Result := tkKey
  else if KeyComp('elsif') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func53: TtkTokenKind;
begin
  if KeyComp('wait') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func55: TtkTokenKind;
begin
  if KeyComp('shared') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func56: TtkTokenKind;
begin
  if KeyComp('out') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func57: TtkTokenKind;
begin
  if KeyComp('while') then Result := tkKey
  else if KeyComp('xor') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func58: TtkTokenKind;
begin
  if KeyComp('exit') then Result := tkKey
  else if KeyComp('loop') then Result := tkKey
  else if KeyComp('buffer') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func59: TtkTokenKind;
begin
  if KeyComp('linkage') then Result := tkKey
  else if KeyComp('null') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func60: TtkTokenKind;
begin
  if KeyComp('guarded') then Result := tkKey
  else if KeyComp('pure') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func61: TtkTokenKind;
begin
  if KeyComp('reject') then Result := tkKey
  else if KeyComp('generic') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func62: TtkTokenKind;
begin
  if KeyComp('signal') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func63: TtkTokenKind;
begin
  if KeyComp('record') then Result := tkKey
  else if KeyComp('next') then Result := tkKey
  else if KeyComp('array') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func64: TtkTokenKind;
begin
  if KeyComp('select') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func66: TtkTokenKind;
begin
  if KeyComp('type') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func69: TtkTokenKind;
begin
  if KeyComp('port') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func70: TtkTokenKind;
begin
  if KeyComp('variable') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func71: TtkTokenKind;
begin
  if KeyComp('xnor') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func75: TtkTokenKind;
begin
  if KeyComp('generate') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func76: TtkTokenKind;
begin
  if KeyComp('until') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func77: TtkTokenKind;
begin
  if KeyComp('literal') then Result := tkKey
  else if KeyComp('group') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func79: TtkTokenKind;
begin
  if KeyComp('inout') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func82: TtkTokenKind;
begin
  if KeyComp('impure') then Result := tkKey
  else if KeyComp('assert') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func83: TtkTokenKind;
begin
  if KeyComp('units') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func85: TtkTokenKind;
begin
  if KeyComp('others') then Result := tkKey
  else if KeyComp('unaffected') then Result := tkKey
  else if KeyComp('library') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func88: TtkTokenKind;
begin
  if KeyComp('inertial') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func91: TtkTokenKind;
begin
  if KeyComp('downto') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func92: TtkTokenKind;
begin
  if KeyComp('report') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func93: TtkTokenKind;
begin
  if KeyComp('entity') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func95: TtkTokenKind;
begin
  if KeyComp('process') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func96: TtkTokenKind;
begin
  if KeyComp('return') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func101: TtkTokenKind;
begin
  if KeyComp('register') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func102: TtkTokenKind;
begin
  if KeyComp('function') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func105: TtkTokenKind;
begin
  if KeyComp('procedure') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func106: TtkTokenKind;
begin
  if KeyComp('constant') then Result := tkKey
  else if KeyComp('disconnect') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func108: TtkTokenKind;
begin
  if KeyComp('subtype') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func115: TtkTokenKind;
begin
  if KeyComp('component') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func116: TtkTokenKind;
begin
  if KeyComp('attribute') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func123: TtkTokenKind;
begin
  if KeyComp('severity') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func124: TtkTokenKind;
begin
  if KeyComp('postponed') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func131: TtkTokenKind;
begin
  if KeyComp('architecture') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func141: TtkTokenKind;
begin
  if KeyComp('transport') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.Func152: TtkTokenKind;
begin
  if KeyComp('configuration') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVhdlSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynVhdlSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: integer;
begin
  fToIdent := MayBe;
  HashKey  := KeyHash(MayBe);
  if HashKey < 153 then
    Result := TIdentFuncTableFunc(fIdentFuncTable[HashKey])()
  else
    Result := tkIdentifier;
end;

procedure TSynVhdlSyn.MakeMethodTables;
var
  I: char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I]      := @AndSymbolProc;
      '@': fProcTable[I]      := @AtSymbolProc;
      '}': fProcTable[I]      := @BraceCloseProc;
      '{': fProcTable[I]      := @BraceOpenProc;
      #13: fProcTable[I]      := @CRProc;
      ':': fProcTable[I]      := @ColonProc;
      '#': fProcTable[I]      := @DirectiveProc;
      #39: fProcTable[I]      := @DotProc;
      '=': fProcTable[I]      := @EqualProc;
      '>': fProcTable[I]      := @GreaterProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      #10: fProcTable[I]      := @LFProc;
      '<': fProcTable[I]      := @LowerProc;
      '-': fProcTable[I]      := @MinusProc;
      '%': fProcTable[I]      := @ModSymbolProc;
      '!': fProcTable[I]      := @NotSymbolProc;
      #0: fProcTable[I]       := @NullProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      '|': fProcTable[I]      := @OrSymbolProc;
      '+': fProcTable[I]      := @PlusProc;
      '.': fProcTable[I]      := @PointProc;
      '?': fProcTable[I]      := @QuestionProc;
      ')': fProcTable[I]      := @RoundCloseProc;
      '(': fProcTable[I]      := @RoundOpenProc;
      ';': fProcTable[I]      := @SemiColonProc;
      '/': fProcTable[I]      := @SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @SpaceProc;
      ']': fProcTable[I]      := @SquareCloseProc;
      '[': fProcTable[I]      := @SquareOpenProc;
      '*': fProcTable[I]      := @StarProc;
      #34: fProcTable[I]      := @StringProc;
      '~': fProcTable[I]      := @TildeProc;
      '`': fProcTable[I]      := @WaveProc;
      '^': fProcTable[I]      := @XOrSymbolProc;
      else
        fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynVhdlSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fDirecAttri := TSynHighLighterAttributes.Create(SYNS_AttrDirective);
  fDirecAttri.Foreground := $00C05000;
  AddAttribute(fDirecAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Foreground := clWindowText;
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := clBlue;
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clPurple;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrWhitespace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clMaroon;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground := clNavy;
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(@DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterVHDL;
end;

procedure TSynVhdlSyn.SetLine(const NewValue: string; LineNumber: integer);
begin
  fLine       := PChar(NewValue);
  Run         := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynVhdlSyn.AndSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVhdlSyn.AtSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynVhdlSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVhdlSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVhdlSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynVhdlSyn.ColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.DirectiveProc; //Detect directives keywords   //Kan
begin
  fTokenID := tkDirective;
  inc(Run);

  {#define}
  if (FLine[Run] = 'd') and (FLine[Run + 1] = 'e') and (FLine[Run + 2] = 'f')
    and (FLine[Run + 3] = 'i') and (FLine[Run + 4] = 'n') and (FLine[Run + 5] = 'e') then
  begin
    inc(Run, 5);
    fTokenID := tkDirective;
    inc(Run);
  end;

  {#elif}
  if (FLine[Run] = 'e') and (FLine[Run + 1] = 'l') and (FLine[Run + 2] = 'i')
    and (FLine[Run + 3] = 'f') then
  begin
    inc(Run, 3);
    fTokenID := tkDirective;
    inc(Run);
  end;

  {#else}
  if (FLine[Run] = 'e') and (FLine[Run + 1] = 'l') and (FLine[Run + 2] = 's')
    and (FLine[Run + 3] = 'e') then
  begin
    inc(Run, 3);
    fTokenID := tkDirective;
    inc(Run);
  end;

  {#endif}
  if (FLine[Run] = 'e') and (FLine[Run + 1] = 'n') and (FLine[Run + 2] = 'd')
    and (FLine[Run + 3] = 'i') and (FLine[Run + 4] = 'f') then
  begin
    inc(Run, 4);
    fTokenID := tkDirective;
    inc(Run);
  end;

  {#if/#ifdef/#ifndef}
  if (FLine[Run] = 'i') and (FLine[Run + 1] = 'f') then
  begin
    inc(Run, 1);
    fTokenID := tkDirective;
    inc(Run);
    case FLine[Run] of
      'd':
        begin
          if (FLine[Run + 1] = 'e') and (FLine[Run + 2] = 'f') then
          begin
            inc(Run, 2);
            fTokenID := tkDirective;
            inc(Run);
          end;
        end;
      'n':
        begin
          if (FLine[Run + 1] = 'd') and (FLine[Run + 2] = 'e') and (FLine[Run + 3] = 'f') then
          begin
            inc(Run, 3);
            fTokenID := tkDirective;
            inc(Run);
          end;
        end;
    end;
  end;

  {#include}
  if (FLine[Run] = 'i') and (FLine[Run + 1] = 'n') and (FLine[Run + 2] = 'c')
    and (FLine[Run + 3] = 'l') and (FLine[Run + 4] = 'u') and (FLine[Run + 5] = 'd') and
    (FLine[Run + 6] = 'e') then
  begin
    inc(Run, 6);
    fTokenID := tkDirective;
    inc(Run);
  end;

  {#pragma}
  if (FLine[Run] = 'p') and (FLine[Run + 1] = 'r') and (FLine[Run + 2] = 'a')
    and (FLine[Run + 3] = 'g') and (FLine[Run + 4] = 'm') and (FLine[Run + 5] = 'a') then
  begin
    inc(Run, 5);
    fTokenID := tkDirective;
    inc(Run);
  end;

  {#undef}
  if (FLine[Run] = 'u') and (FLine[Run + 1] = 'n') and (FLine[Run + 2] = 'd')
    and (FLine[Run + 3] = 'e') and (FLine[Run + 4] = 'f') then
  begin
    inc(Run, 4);
    fTokenID := tkDirective;
    inc(Run);
  end;
end;

procedure TSynVhdlSyn.DotProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.EqualProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.GreaterProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynVhdlSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynVhdlSyn.LowerProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (FLine[Run] = '-') then
  begin
    fTokenID := tkComment;
    while not (FLine[Run] in [#0, #10, #13]) do inc(Run);
  end;
end;

procedure TSynVhdlSyn.ModSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVhdlSyn.NotSymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynVhdlSyn.NumberProc;
begin
  if (FLine[Run] = '1') and ((FLine[Run + 1] = '+') or (FLine[Run + 1] = '-')) and
    (FLine[Run + 2] in [#0, #10, #13, #32]) then
  begin
    fTokenID := tkKey;
    inc(Run, 2);
  end
  else
  begin
    fTokenID := tkNumber;
    while FLine[Run] in ['0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F'] do
      inc(Run);
  end;
end;

procedure TSynVhdlSyn.OrSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVhdlSyn.PlusProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.PointProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.QuestionProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynVhdlSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.SlashProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynVhdlSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.StarProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVhdlSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        if FLine[Run + 1] = #10 then inc(Run);
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynVhdlSyn.TildeProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVhdlSyn.WaveProc; //Kan
begin
  fTokenID := tkSymbol;
  inc(Run);

  {`ascending}
  if ((FLine[Run] = 'a') or (FLine[Run] = 'A')) and
    ((FLine[Run + 1] = 's') or (FLine[Run + 1] = 'S')) and
    ((FLine[Run + 2] = 'c') or (FLine[Run + 2] = 'C')) and
    ((FLine[Run + 3] = 'e') or (FLine[Run + 3] = 'E')) and
    ((FLine[Run + 4] = 'n') or (FLine[Run + 4] = 'N')) and
    ((FLine[Run + 5] = 'd') or (FLine[Run + 5] = 'D')) and
    ((FLine[Run + 6] = 'i') or (FLine[Run + 6] = 'I')) and
    ((FLine[Run + 7] = 'n') or (FLine[Run + 7] = 'N')) and
    ((FLine[Run + 8] = 'g') or (FLine[Run + 8] = 'G')) then
  begin
    inc(Run, 8);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`base}
  if ((FLine[Run] = 'b') or (FLine[Run] = 'B')) and
    ((FLine[Run + 1] = 'a') or (FLine[Run + 1] = 'A')) and
    ((FLine[Run + 2] = 's') or (FLine[Run + 2] = 'S')) and
    ((FLine[Run + 3] = 'e') or (FLine[Run + 3] = 'E')) then
  begin
    inc(Run, 3);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`delayed}
  if ((FLine[Run] = 'd') or (FLine[Run] = 'D')) and
    ((FLine[Run + 1] = 'e') or (FLine[Run + 1] = 'E')) and
    ((FLine[Run + 2] = 'l') or (FLine[Run + 2] = 'L')) and
    ((FLine[Run + 3] = 'a') or (FLine[Run + 3] = 'A')) and
    ((FLine[Run + 4] = 'y') or (FLine[Run + 4] = 'Y')) and
    ((FLine[Run + 5] = 'e') or (FLine[Run + 5] = 'E')) and
    ((FLine[Run + 6] = 'd') or (FLine[Run + 6] = 'D')) then
  begin
    inc(Run, 6);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`driving/`driving_value}
  if ((FLine[Run] = 'd') or (FLine[Run] = 'D')) and
    ((FLine[Run + 1] = 'r') or (FLine[Run + 1] = 'R')) and
    ((FLine[Run + 2] = 'i') or (FLine[Run + 2] = 'I')) and
    ((FLine[Run + 3] = 'v') or (FLine[Run + 3] = 'V')) and
    ((FLine[Run + 4] = 'i') or (FLine[Run + 4] = 'I')) and
    ((FLine[Run + 5] = 'n') or (FLine[Run + 5] = 'N')) and
    ((FLine[Run + 6] = 'g') or (FLine[Run + 6] = 'G')) then
  begin
    inc(Run, 6);
    fTokenID := tkKey;
    inc(Run);
    if (FLine[Run] = '_') and ((FLine[Run + 1] = 'v') or (FLine[Run + 1] = 'V')) and
      ((FLine[Run + 2] = 'a') or (FLine[Run + 2] = 'A')) and
      ((FLine[Run + 3] = 'l') or (FLine[Run + 3] = 'L')) and
      ((FLine[Run + 4] = 'u') or (FLine[Run + 4] = 'U')) and
      ((FLine[Run + 5] = 'e') or (FLine[Run + 5] = 'E')) then
    begin
      inc(Run, 5);
      fTokenID := tkKey;
      inc(Run);
    end;
  end;

  {`event}
  if ((FLine[Run] = 'e') or (FLine[Run] = 'E')) and
    ((FLine[Run + 1] = 'v') or (FLine[Run + 1] = 'V')) and
    ((FLine[Run + 2] = 'e') or (FLine[Run + 2] = 'E')) and
    ((FLine[Run + 3] = 'n') or (FLine[Run + 3] = 'N')) and
    ((FLine[Run + 4] = 't') or (FLine[Run + 4] = 'T')) then
  begin
    inc(Run, 4);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`high}
  if ((FLine[Run] = 'h') or (FLine[Run] = 'H')) and
    ((FLine[Run + 1] = 'i') or (FLine[Run + 1] = 'I')) and
    ((FLine[Run + 2] = 'g') or (FLine[Run + 2] = 'G')) and
    ((FLine[Run + 3] = 'h') or (FLine[Run + 3] = 'H')) then
  begin
    inc(Run, 3);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`image}
  if ((FLine[Run] = 'i') or (FLine[Run] = 'I')) and
    ((FLine[Run + 1] = 'm') or (FLine[Run + 1] = 'M')) and
    ((FLine[Run + 2] = 'a') or (FLine[Run + 2] = 'A')) and
    ((FLine[Run + 3] = 'g') or (FLine[Run + 3] = 'G')) and
    ((FLine[Run + 4] = 'e') or (FLine[Run + 4] = 'E')) then
  begin
    inc(Run, 4);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`instance_name}
  if ((FLine[Run] = 'i') or (FLine[Run] = 'I')) and
    ((FLine[Run + 1] = 'n') or (FLine[Run + 1] = 'N')) and
    ((FLine[Run + 2] = 's') or (FLine[Run + 2] = 'S')) and
    ((FLine[Run + 3] = 't') or (FLine[Run + 3] = 'T')) and
    ((FLine[Run + 4] = 'a') or (FLine[Run + 4] = 'A')) and
    ((FLine[Run + 5] = 'n') or (FLine[Run + 5] = 'N')) and
    ((FLine[Run + 6] = 'c') or (FLine[Run + 6] = 'C')) and
    ((FLine[Run + 7] = 'e') or (FLine[Run + 7] = 'E')) and (FLine[Run + 8] = '_') and
    ((FLine[Run + 9] = 'n') or (FLine[Run + 9] = 'N')) and
    ((FLine[Run + 10] = 'a') or (FLine[Run + 10] = 'A')) and
    ((FLine[Run + 11] = 'm') or (FLine[Run + 11] = 'M')) and
    ((FLine[Run + 12] = 'e') or (FLine[Run + 12] = 'E')) then
  begin
    inc(Run, 12);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`last_active}
  if ((FLine[Run] = 'l') or (FLine[Run] = 'L')) and
    ((FLine[Run + 1] = 'a') or (FLine[Run + 1] = 'A')) and
    ((FLine[Run + 2] = 's') or (FLine[Run + 2] = 'S')) and
    ((FLine[Run + 3] = 't') or (FLine[Run + 3] = 'T')) and (FLine[Run + 4] = '_') and
    ((FLine[Run + 5] = 'a') or (FLine[Run + 5] = 'A')) and
    ((FLine[Run + 6] = 'c') or (FLine[Run + 6] = 'C')) and
    ((FLine[Run + 7] = 't') or (FLine[Run + 7] = 'T')) and
    ((FLine[Run + 8] = 'i') or (FLine[Run + 8] = 'I')) and
    ((FLine[Run + 9] = 'v') or (FLine[Run + 9] = 'V')) and
    ((FLine[Run + 10] = 'e') or (FLine[Run + 10] = 'E')) then
  begin
    inc(Run, 10);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`last_event}
  if ((FLine[Run] = 'l') or (FLine[Run] = 'L')) and
    ((FLine[Run + 1] = 'a') or (FLine[Run + 1] = 'A')) and
    ((FLine[Run + 2] = 's') or (FLine[Run + 2] = 'S')) and
    ((FLine[Run + 3] = 't') or (FLine[Run + 3] = 'T')) and (FLine[Run + 4] = '_') and
    ((FLine[Run + 5] = 'e') or (FLine[Run + 5] = 'E')) and
    ((FLine[Run + 6] = 'v') or (FLine[Run + 6] = 'V')) and
    ((FLine[Run + 7] = 'e') or (FLine[Run + 7] = 'E')) and
    ((FLine[Run + 8] = 'n') or (FLine[Run + 8] = 'N')) and
    ((FLine[Run + 9] = 't') or (FLine[Run + 9] = 'T')) then
  begin
    inc(Run, 9);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`last_value}
  if ((FLine[Run] = 'l') or (FLine[Run] = 'L')) and
    ((FLine[Run + 1] = 'a') or (FLine[Run + 1] = 'A')) and
    ((FLine[Run + 2] = 's') or (FLine[Run + 2] = 'S')) and
    ((FLine[Run + 3] = 't') or (FLine[Run + 3] = 'T')) and (FLine[Run + 4] = '_') and
    ((FLine[Run + 5] = 'v') or (FLine[Run + 5] = 'V')) and
    ((FLine[Run + 6] = 'a') or (FLine[Run + 6] = 'A')) and
    ((FLine[Run + 7] = 'l') or (FLine[Run + 7] = 'L')) and
    ((FLine[Run + 8] = 'u') or (FLine[Run + 8] = 'U')) and
    ((FLine[Run + 9] = 'e') or (FLine[Run + 9] = 'E')) then
  begin
    inc(Run, 9);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`left/`leftof}
  if ((FLine[Run] = 'l') or (FLine[Run] = 'L')) and
    ((FLine[Run + 1] = 'e') or (FLine[Run + 1] = 'E')) and
    ((FLine[Run + 2] = 'f') or (FLine[Run + 2] = 'F')) and
    ((FLine[Run + 3] = 't') or (FLine[Run + 3] = 'T')) then
  begin
    inc(Run, 3);
    fTokenID := tkKey;
    inc(Run);
    if ((FLine[Run] = 'o') or (FLine[Run] = 'O')) and
      ((FLine[Run + 1] = 'f') or (FLine[Run + 1] = 'F')) then
    begin
      inc(Run, 1);
      fTokenID := tkKey;
      inc(Run);
    end;
  end;

  {`length}
  if ((FLine[Run] = 'l') or (FLine[Run] = 'L')) and
    ((FLine[Run + 1] = 'e') or (FLine[Run + 1] = 'E')) and
    ((FLine[Run + 2] = 'n') or (FLine[Run + 2] = 'N')) and
    ((FLine[Run + 3] = 'g') or (FLine[Run + 3] = 'G')) and
    ((FLine[Run + 4] = 't') or (FLine[Run + 4] = 'T')) and
    ((FLine[Run + 5] = 'h') or (FLine[Run + 5] = 'H')) then
  begin
    inc(Run, 5);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`low}
  if ((FLine[Run] = 'l') or (FLine[Run] = 'L')) and
    ((FLine[Run + 1] = 'o') or (FLine[Run + 1] = 'O')) and
    ((FLine[Run + 2] = 'w') or (FLine[Run + 2] = 'W')) then
  begin
    inc(Run, 2);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`path_name}
  if ((FLine[Run] = 'p') or (FLine[Run] = 'P')) and
    ((FLine[Run + 1] = 'a') or (FLine[Run + 1] = 'A')) and
    ((FLine[Run + 2] = 't') or (FLine[Run + 2] = 'T')) and
    ((FLine[Run + 3] = 'h') or (FLine[Run + 3] = 'H')) and (FLine[Run + 4] = '_') and
    ((FLine[Run + 5] = 'n') or (FLine[Run + 5] = 'N')) and
    ((FLine[Run + 6] = 'a') or (FLine[Run + 6] = 'A')) and
    ((FLine[Run + 7] = 'm') or (FLine[Run + 7] = 'M')) and
    ((FLine[Run + 8] = 'e') or (FLine[Run + 8] = 'E')) then
  begin
    inc(Run, 8);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`pos}
  if ((FLine[Run] = 'p') or (FLine[Run] = 'P')) and
    ((FLine[Run + 1] = 'o') or (FLine[Run + 1] = 'O')) and
    ((FLine[Run + 2] = 's') or (FLine[Run + 2] = 'S')) then
  begin
    inc(Run, 2);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`pred}
  if ((FLine[Run] = 'p') or (FLine[Run] = 'P')) and
    ((FLine[Run + 1] = 'r') or (FLine[Run + 1] = 'R')) and
    ((FLine[Run + 2] = 'e') or (FLine[Run + 2] = 'E')) and
    ((FLine[Run + 3] = 'd') or (FLine[Run + 3] = 'D')) then
  begin
    inc(Run, 3);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`quiet}
  if ((FLine[Run] = 'q') or (FLine[Run] = 'Q')) and
    ((FLine[Run + 1] = 'u') or (FLine[Run + 1] = 'U')) and
    ((FLine[Run + 2] = 'i') or (FLine[Run + 2] = 'I')) and
    ((FLine[Run + 3] = 'e') or (FLine[Run + 3] = 'E')) and
    ((FLine[Run + 4] = 't') or (FLine[Run + 4] = 'T')) then
  begin
    inc(Run, 4);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`range}
  if ((FLine[Run] = 'r') or (FLine[Run] = 'R')) and
    ((FLine[Run + 1] = 'a') or (FLine[Run + 1] = 'A')) and
    ((FLine[Run + 2] = 'n') or (FLine[Run + 2] = 'N')) and
    ((FLine[Run + 3] = 'g') or (FLine[Run + 3] = 'G')) and
    ((FLine[Run + 4] = 'e') or (FLine[Run + 4] = 'E')) then
  begin
    inc(Run, 4);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`reverse_range}
  if ((FLine[Run] = 'r') or (FLine[Run] = 'R')) and
    ((FLine[Run + 1] = 'e') or (FLine[Run + 1] = 'E')) and
    ((FLine[Run + 2] = 'v') or (FLine[Run + 2] = 'V')) and
    ((FLine[Run + 3] = 'e') or (FLine[Run + 3] = 'E')) and
    ((FLine[Run + 4] = 'r') or (FLine[Run + 4] = 'R')) and
    ((FLine[Run + 5] = 's') or (FLine[Run + 5] = 'S')) and
    ((FLine[Run + 6] = 'e') or (FLine[Run + 6] = 'E')) and (FLine[Run + 7] = '_') and
    ((FLine[Run + 8] = 'r') or (FLine[Run + 8] = 'R')) and
    ((FLine[Run + 9] = 'a') or (FLine[Run + 9] = 'A')) and
    ((FLine[Run + 10] = 'n') or (FLine[Run + 10] = 'N')) and
    ((FLine[Run + 11] = 'g') or (FLine[Run + 11] = 'G')) and
    ((FLine[Run + 12] = 'e') or (FLine[Run + 12] = 'E')) then
  begin
    inc(Run, 12);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`right/`rightof}
  if ((FLine[Run] = 'r') or (FLine[Run] = 'R')) and
    ((FLine[Run + 1] = 'i') or (FLine[Run + 1] = 'I')) and
    ((FLine[Run + 2] = 'g') or (FLine[Run + 2] = 'G')) and
    ((FLine[Run + 3] = 'h') or (FLine[Run + 3] = 'H')) and
    ((FLine[Run + 4] = 't') or (FLine[Run + 4] = 'T')) then
  begin
    inc(Run, 4);
    fTokenID := tkKey;
    inc(Run);
    if ((FLine[Run] = 'o') or (FLine[Run] = 'O')) and
      ((FLine[Run + 1] = 'f') or (FLine[Run + 1] = 'F')) then
    begin
      inc(Run, 1);
      fTokenID := tkKey;
      inc(Run);
    end;
  end;

  {`simple_name}
  if ((FLine[Run] = 's') or (FLine[Run] = 'S')) and
    ((FLine[Run + 1] = 'i') or (FLine[Run + 1] = 'I')) and
    ((FLine[Run + 2] = 'm') or (FLine[Run + 2] = 'M')) and
    ((FLine[Run + 3] = 'p') or (FLine[Run + 3] = 'P')) and
    ((FLine[Run + 4] = 'l') or (FLine[Run + 4] = 'L')) and
    ((FLine[Run + 5] = 'e') or (FLine[Run + 5] = 'E')) and (FLine[Run + 6] = '_') and
    ((FLine[Run + 7] = 'n') or (FLine[Run + 7] = 'N')) and
    ((FLine[Run + 8] = 'a') or (FLine[Run + 8] = 'A')) and
    ((FLine[Run + 9] = 'm') or (FLine[Run + 9] = 'M')) and
    ((FLine[Run + 10] = 'e') or (FLine[Run + 10] = 'E')) then
  begin
    inc(Run, 10);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`stable}
  if ((FLine[Run] = 's') or (FLine[Run] = 'S')) and
    ((FLine[Run + 1] = 't') or (FLine[Run + 1] = 'T')) and
    ((FLine[Run + 2] = 'a') or (FLine[Run + 2] = 'A')) and
    ((FLine[Run + 3] = 'b') or (FLine[Run + 3] = 'B')) and
    ((FLine[Run + 4] = 'l') or (FLine[Run + 4] = 'L')) and
    ((FLine[Run + 5] = 'e') or (FLine[Run + 5] = 'E')) then
  begin
    inc(Run, 5);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`succ}
  if ((FLine[Run] = 's') or (FLine[Run] = 'S')) and
    ((FLine[Run + 1] = 'u') or (FLine[Run + 1] = 'U')) and
    ((FLine[Run + 2] = 'c') or (FLine[Run + 2] = 'C')) and
    ((FLine[Run + 3] = 'c') or (FLine[Run + 3] = 'C')) then
  begin
    inc(Run, 3);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`transaction}
  if ((FLine[Run] = 't') or (FLine[Run] = 'T')) and
    ((FLine[Run + 1] = 'r') or (FLine[Run + 1] = 'R')) and
    ((FLine[Run + 2] = 'a') or (FLine[Run + 2] = 'A')) and
    ((FLine[Run + 3] = 'n') or (FLine[Run + 3] = 'N')) and
    ((FLine[Run + 4] = 's') or (FLine[Run + 4] = 'S')) and
    ((FLine[Run + 5] = 'a') or (FLine[Run + 5] = 'A')) and
    ((FLine[Run + 6] = 'c') or (FLine[Run + 6] = 'C')) and
    ((FLine[Run + 7] = 't') or (FLine[Run + 7] = 'T')) and
    ((FLine[Run + 8] = 'i') or (FLine[Run + 8] = 'I')) and
    ((FLine[Run + 9] = 'o') or (FLine[Run + 9] = 'O')) and
    ((FLine[Run + 10] = 'n') or (FLine[Run + 10] = 'N')) then
  begin
    inc(Run, 10);
    fTokenID := tkKey;
    inc(Run);
  end;

  {`val/`value}
  if ((FLine[Run] = 'v') or (FLine[Run] = 'V')) and
    ((FLine[Run + 1] = 'a') or (FLine[Run + 1] = 'A')) and
    ((FLine[Run + 2] = 'l') or (FLine[Run + 2] = 'L')) then
  begin
    inc(Run, 2);
    fTokenID := tkKey;
    inc(Run);
    if ((FLine[Run] = 'u') or (FLine[Run] = 'U')) and
      ((FLine[Run + 1] = 'e') or (FLine[Run + 1] = 'E')) then
    begin
      inc(Run, 1);
      fTokenID := tkKey;
      inc(Run);
    end;
  end;
end;

procedure TSynVhdlSyn.XOrSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVhdlSyn.UnknownProc;
var
  i:Integer;
begin
  if fLine[Run]>#127 then
    i:=UTF8CharacterLength(@fLine[Run])
    else
      i:=1;
  Inc(Run,i);
  fTokenID := tkUnknown;
end;

procedure TSynVhdlSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynVhdlSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result    := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result    := fKeyAttri;
    SYN_ATTR_STRING: Result     := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else
      Result := nil;
  end;
end;

function TSynVhdlSyn.GetEOL: boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynVhdlSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynVhdlSyn.GetToken: string;
var
  Len: longint;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynVhdlSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer
  );
begin
  TokenStart:=fLine+fTokenPos;
  TokenLength:=Run-fTokenPos;
end;

function TSynVhdlSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynVhdlSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result    := fCommentAttri;
    tkDirective: Result  := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result        := fKeyAttri;
    tkNumber: Result     := fNumberAttri;
    tkSpace: Result      := fSpaceAttri;
    tkString: Result     := fStringAttri;
    tkSymbol: Result     := fSymbolAttri;
    tkUnknown: Result    := fIdentifierAttri;
    else
      Result := nil;
  end;
end;

function TSynVhdlSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynVhdlSyn.GetTokenPos: integer;
begin
  Result := fTokenPos;
end;

procedure TSynVhdlSyn.ReSetRange;
begin
  fRange := rsUnknown;
end;

class function TSynVhdlSyn.GetLanguageName: string;
begin
 Result := SYNS_LangVHDL;
end;

procedure TSynVhdlSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

function TSynVhdlSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

function TSynVhdlSyn.GetSampleSource: string;
begin
  Result := '-- VHDL code for AND-OR-INVERT gate'+#13#10+
            ''+#13#10+
            'library IEEE;'+#13#10+
            'use IEEE.STD_LOGIC_1164.all;'+#13#10+
            ''+#13#10+
            'entity AOI is'+#13#10+
            'port ('+#13#10+
            '  A, B, C, D: in STD_LOGIC;'+#13#10+
            '  F : out STD_LOGIC'+#13#10+
            ');'+#13#10+
            'end AOI;'+#13#10+
            ''+#13#10+
            'architecture V1 of AOI is'+#13#10+
            'begin'+#13#10+
            '  F <= not ((A and B) or (C and D));'+#13#10+
            'end V1;';
end;

function TSynVhdlSyn.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterVHDL;
end;

initialization
  MakeIdentTable;
end.

