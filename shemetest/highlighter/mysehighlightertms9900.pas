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
unit MySEHighlighterTMS9900;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter,
  SynHighlighterHashEntries;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown, tkSystemValue);

  TProcTableProc = procedure of object;

type

  { TMySEHighlighterTMS9900 }

  TMySEHighlighterTMS9900 = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    fSystemValueAttri : TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    procedure CommentProc;
    procedure CRProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SingleQuoteStringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure astProc;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(Const NewValue: String; LineNumber:Integer); Override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property SystemValueAttri : TSynHighlighterAttributes Read fSystemValueAttri
      write fSystemValueAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

const
  OpCodes : String = 'a,ab,abs,ai,andi,b,bl,blwp,c,cb,ci,ckof,ckon,clr,coc,czc,' +
                     'dec,dect,div,idle,inc,inct,inv,jeq,jgt,jh,jhe,jl,jle,jlt,' +
                     'jmp,jnc,jne,jno,joc,jop,ldcr,li,limi,lrex,lwpi,mov,movb,' +
                     'mpy,neg,ori,rset,rtwp,s,sb,sbo,sbz,seto,sla,sra,src,srl,' +
                     'soc,socb,stcr,stst,stwp,swpb,szc,szcb,tb,x,xop,xor,rt';

  SystemValues : String = 'pc,st,wp,st0,st1,st2,st3,st4,st5,st6,st7,st8,st9,' +
                          'st10,st11,st12,st13,st14,st15,r0,r1,r2,r3,r4,r5,r6,' +
                          'r7,r8,r9,r10,r11,r12,r13,r14,r15';

  SYNS_FilterTMSAssembly        =  'TMS9900 Assembly Files (*.tms,*.asm)|*.tms;*.asm';
  SYNS_LangTMSAsm               =  'Assembly Texas Instruments TMS9900';

procedure MakeIdentTable;
var
  c: char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for c := 'a' to 'z' do
    Identifiers[c] := TRUE;
  for c := 'A' to 'Z' do
    Identifiers[c] := TRUE;
  for c := '0' to '9' do
    Identifiers[c] := TRUE;
  Identifiers['_'] := TRUE;

  FillChar(mHashTable, SizeOf(mHashTable), 0);
  for c := 'a' to 'z' do
    mHashTable[c] := 1 + Ord(c) - Ord('a');
  for c := 'A' to 'Z' do
    mHashTable[c] := 1 + Ord(c) - Ord('A');
  for c := '0' to '9' do
    mHashTable[c] := 27 + Ord(c) - Ord('0');
end;

function TMySEHighlighterTMS9900.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while Identifiers[ToHash^] do begin
{$IFOPT Q-}
    Result := 7 * Result + mHashTable[ToHash^];
{$ELSE}
    Result := (7 * Result + mHashTable[ToHash^]) and $FFFFFF;
{$ENDIF}
    inc(ToHash);
  end;
  Result := Result and $3FF;
  fStringLen := ToHash - fToIdent;
end;

function TMySEHighlighterTMS9900.KeyComp(const aKey: String): Boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := fToIdent;
  // Note: fStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for i := 1 to fStringLen do
  begin
    if mHashTable[pKey1^] <> mHashTable[pKey2^] then
    begin
      Result := FALSE;
      exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := TRUE;
end;

procedure TMySEHighlighterTMS9900.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TMySEHighlighterTMS9900.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if KeyComp(Entry.Keyword) then begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TMySEHighlighterTMS9900.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
       #0 : fProcTable[I] := @NullProc;
      #10 : fProcTable[I] := @LFProc;
      #13 : fProcTable[I] := @CRProc;
      #34 : fProcTable[I] := @StringProc;
      #39 : fProcTable[I] := @SingleQuoteStringProc;
      '>' : fProcTable[I] := @GreaterProc;
      '<' : fProcTable[I] := @LowerProc;
      '/' : fProcTable[I] := @SlashProc;

      '*' : fProcTable[I] := @astProc;

      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := @IdentProc;
      '0'..'9':
        fProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := @SpaceProc;
      '#', ';':
        fProcTable[I] := @CommentProc;
      '.', ':', '&', '{', '}', '=', '^', '-', '+', '(', ')':
        fProcTable[I] := @SymbolProc;
      else
        fProcTable[I] := @UnknownProc;
    end;
end;

constructor TMySEHighlighterTMS9900.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeywords := TSynHashEntryList.Create;

  fCommentAttri       := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri    := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri           := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style     := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri        := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri         := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri        := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri        := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fSystemValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystemValue);
  AddAttribute(fSystemValueAttri);

  MakeMethodTables;
  EnumerateKeywords(Ord(tkKey), OpCodes, IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkSystemValue), SystemValues, IdentChars, @DoAddKeyword);
  SetAttributesOnChange(@DefHighlightChange);
  fDefaultFilter      := SYNS_FilterTMSAssembly;
end;

destructor TMySEHighlighterTMS9900.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TMySEHighlighterTMS9900.SetLine(const NewValue: String; LineNumber: Integer);
begin
  Inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TMySEHighlighterTMS9900.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TMySEHighlighterTMS9900.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TMySEHighlighterTMS9900.GreaterProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TMySEHighlighterTMS9900.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TMySEHighlighterTMS9900.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TMySEHighlighterTMS9900.LowerProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TMySEHighlighterTMS9900.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TMySEHighlighterTMS9900.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'a'..'f', 'h', 'A'..'F', 'H'] do
    Inc(Run);
end;

procedure TMySEHighlighterTMS9900.SlashProc;
begin
  Inc(Run);
  if fLine[Run] = '/' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else
    fTokenID := tkSymbol;
end;

procedure TMySEHighlighterTMS9900.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TMySEHighlighterTMS9900.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then
    inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TMySEHighlighterTMS9900.SingleQuoteStringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then
    inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TMySEHighlighterTMS9900.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TMySEHighlighterTMS9900.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkIdentifier;
end;

procedure TMySEHighlighterTMS9900.astProc;
begin
 If FLine[Run + 1] = ' ' Then
  CommentProc
 Else
  SymbolProc;
end;

procedure TMySEHighlighterTMS9900.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TMySEHighlighterTMS9900.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TMySEHighlighterTMS9900.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TMySEHighlighterTMS9900.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TMySEHighlighterTMS9900.GetTokenEx(out TokenStart: PChar; out TokenLength: integer
  );
begin
 TokenLength:=Run-fTokenPos;
 TokenStart:=FLine + fTokenPos;
end;

function TMySEHighlighterTMS9900.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkSystemValue : Result := fSystemValueAttri;
    else Result := nil;
  end;
end;

function TMySEHighlighterTMS9900.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TMySEHighlighterTMS9900.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TMySEHighlighterTMS9900.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TMySEHighlighterTMS9900.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TMySEHighlighterTMS9900.GetLanguageName: string;
begin
  Result := SYNS_LangTMSAsm;
end;

function TMySEHighlighterTMS9900.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterTMSAssembly;
end;

function TMySEHighlighterTMS9900.GetSampleSource: string;
begin
  Result := '* SIMPLE NON WORKING TMS9900 ASSEMBLER CODE' + #13#10 +
            'MOV  R5,R0' + #13#10 +
            'SLA  R0,5*2' + #13#10 +
            'SOC  R5,R0' + #13#10 +
            'ANDI R0,>FF07' + #13#10 +
            'MOV  R4,R3' + #13#10 +
            'ANDI R3,''F''' + #13#10 +
            'A    R4,R0' + #13#10 +
            'S    R3,R0' + #13#10 +
            'RT';
end;

initialization
  MakeIdentTable;
end.

