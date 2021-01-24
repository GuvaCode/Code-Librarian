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
unit uArmV7Asm_Highlighter;

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

  { TArmV7AsmSyn }

  TArmV7AsmSyn = class(TSynCustomHighlighter)
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
    fSystemValueAttri       : TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
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
  OpCodes : String = 'adc,add,adr,and,asr,b,bfc,bfi,bic,bkpt,bl,blx,bx,cbnz,' +
                        'cbz,cdp,cdp2,clrex,clz,cmn,cmp,cps,cpy,dbg,dmb,dsb,eor,' +
                        'isb,it,ldc,ldc2,ldm,ldmia,ldmfd,LDMDB,LDMEA,ldr,ldrb,' +
                        'ldrbt,ldrd,ldrex,ldrexb,ldrexh,ldrh,ldrht,ldrsb,ldrsbt,' +
                        'ldrsh,ldrsht,ldrt,lsl,lsr,mcr,mcr2,mcrr,mcrr2,mla,mls,' +
                        'mov,movt,mrc,mrc2,mrrc,mrrc2,mrs,msr,mul,mvn,neg,nop,' +
                        'orn,orr,PKHBT,PKHTB,pld,pli,pop,push,qadd,qadd16,qadd8,' +
                        'qasx,qdadd,qdsub,qsax,qsub,qsub16,qsub8,rbit,rev,rev16,' +
                        'revsh,ror,rrx,rsb,sadd16,sadd8,sasx,sbc,sbfx,sdiv,sel,' +
                        'sev,shadd16,shadd8,shasx,shsax,shsub16,shsub8,' +
                        'SMLABB,SMLABT,SMLATB,SMLATT,SMLAD,SMLADX,smlal,' +
                        'SMLALBB,SMLALBT,SMLALTB,SMLALTT,SMLALD,SMLALDX,' +
                        'SMLAWB,SMLAWT,SMLSD,SMLSDX,SMLSLD,SMLSLDX,SMMLA,SMMLAR,' +
                        'SMMLS,SMMLSR,SMMUL,SMMULR,SMUAD,SMUADX,SMULBB,SMULBT,' +
                        'SMULTB,SMULTT,smull,SMULWB,SMULWT,SMUSD,SMUSDX,ssat,' +
                        'ssat16,ssax,ssub16,ssub8,stc,stc2,stm,stmia,stmea,stmdb,' +
                        'stmfd,str,strb,strbt,strd,strex,strexb,strexh,strh,strht,' +
                        'strt,sub,svc,sxtab,sxtab16,sxtah,sxtb,sxtb16,sxth,tbb,' +
                        'tbh,teq,tst,uadd16,uadd8,uasx,ubfx,udiv,uhadd16,uhadd8,' +
                        'uhasx,uhsax,uhsub16,uhsub8,umaal,umlal,umull,uqadd16,' +
                        'uqadd8,uqasx,uqsax,uqsub16,uqsub8,usad8,usada8,usat,' +
                        'usat16,usax,usub16,usub8,uxtab,uxtab16,uxtah,uxtb,uxtb16,' +
                        'uxth,vabs,vadd,vcmp,vcmpe,vcvt,vcvtr,vcvtb,vcvtt,vdiv,' +
                        'vfma,vfms,vfnma,vfnms,vldm,vldr,vmla,vmls,vmov,vmrs,' +
                        'vmsr,vmul,vneg,vnmla,vnmls,vnmul,vpop,vpush,vsqrt,' +
                        'vstm,vstr,vsub,wfe,wfi,yield';

     SystemValue : String = 'r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,' +
                            'r15,sp,lr,pc,sp_main,msp,sp_process,psp,apsr,ipsr,' +
                            'epsr,iapsr,eapsr,xpsr,iepsr,primask,basepri,' +
                            'faultmask,control,npriv,spsel,fpca,actrl,afsr,aircr,' +
                            'bfar,bfsr,ccr,cfsr,cpacr,cpuid,fpcar,fpccr,fpdscr,' +
                            'hfsr,icsr,ictr,id_afr0,id_dfr0,id_isar0,id_isar1,' +
                            'id_isar2,id_isar3,id_isar4,id_mmfr0,id_mmfr1,id_mmfr2,' +
                            'id_mmfr3,id_pfr0,id_pfr1,mmfar,mmfsr,mpu_ctrl,' +
                            'mpu_rasr,mpu_rbar,mpu_rnr,mpu_type,mvfr0,mvfr1,' +
                            'nvic_icer0,nvic_icer1,nvic_icer2,nvic_icer3,nvic_icer4,' +
                            'nvic_icer5,nvic_icer6,nvic_icer7,nvic_icer8,nvic_icer9,' +
                            'nvic_icer10,nvic_icer11,nvic_icer12,nvic_icer13,' +
                            'nvic_icer14,nvic_icer15,nvic_iabr0,nvic_iabr1,' +
                            'nvic_iabr2,nvic_iabr3,nvic_iabr4,nvic_iabr5,nvic_iabr6,' +
                            'nvic_iabr7,nvic_iabr8,nvic_iabr9,nvic_iabr10,' +
                            'nvic_iabr11,nvic_iabr12,nvic_iabr13,nvic_iabr14,' +
                            'nvic_iabr15,nvic_icpr0,nvic_icpr1,nvic_icpr2,' +
                            'nvic_icpr3,nvic_icpr4,nvic_icpr5,nvic_icpr6,nvic_icpr7,' +
                            'nvic_icpr8,nvic_icpr9,nvic_icpr10,nvic_icpr11,' +
                            'nvic_icpr12,nvic_icpr13,nvic_icpr14,nvic_icpr15,' +
                            'nvic_ipr0,nvic_ipr1,nvic_ipr2,' +
                            'nvic_ipr3,nvic_ipr4,nvic_ipr5,nvic_ipr6,nvic_ipr7,' +
                            'nvic_ipr8,nvic_ipr9,nvic_ipr10,nvic_ipr11,' +
                            'nvic_ipr12,nvic_ipr13,nvic_ipr14,nvic_ipr15,' +
                            'nvic_iser0,nvic_iser1,nvic_iser2,nvic_iser3,nvic_iser4,' +
                            'nvic_iser5,nvic_iser6,nvic_iser7,nvic_iser8,nvic_iser9,' +
                            'nvic_iser10,nvic_iser11,nvic_iser12,nvic_iser13,' +
                            'nvic_iser14,nvic_iser15,nvic_ispr0,nvic_ispr1,' +
                            'nvic_ispr2,nvic_ispr3,nvic_ispr4,nvic_ispr5,nvic_ispr6,' +
                            'nvic_ispr7,nvic_ispr8,nvic_ispr9,nvic_ispr10,' +
                            'nvic_ispr11,nvic_ispr12,nvic_ispr13,nvic_ispr14,' +
                            'nvic_ispr15,scr,shcsr,shpr1,shpr2,shpr3,stir,' +
                            'syst_calib,syst_csr,syst_cvr,syst_rvr,ufsr,vtor';

     SYNS_FilterArmV7Assembly        =  'ARM v7 Assembly Files (*.arm,*.asm)|*.arm;*.asm';
     SYNS_LangArmV7Asm               =  'Assembly ARM v7';

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

function TArmV7AsmSyn.KeyHash(ToHash: PChar): Integer;
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

function TArmV7AsmSyn.KeyComp(const aKey: String): Boolean;
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

procedure TArmV7AsmSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TArmV7AsmSyn.IdentKind(MayBe: PChar): TtkTokenKind;
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

procedure TArmV7AsmSyn.MakeMethodTables;
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

      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := @IdentProc;
      '0'..'9':
        fProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := @SpaceProc;
      '#', ';':
        fProcTable[I] := @CommentProc;
      '.', ':', '&', '{', '}', '=', '^', '-', '+', '(', ')', '*':
        fProcTable[I] := @SymbolProc;
      else
        fProcTable[I] := @UnknownProc;
    end;
end;

constructor TArmV7AsmSyn.Create(AOwner: TComponent);
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
  EnumerateKeywords(Ord(tkSystemValue), SystemValue, IdentChars, @DoAddKeyword);
  SetAttributesOnChange(@DefHighlightChange);
  fDefaultFilter      := SYNS_FilterArmV7Assembly;
end;

destructor TArmV7AsmSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TArmV7AsmSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  Inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TArmV7AsmSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TArmV7AsmSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TArmV7AsmSyn.GreaterProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TArmV7AsmSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TArmV7AsmSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TArmV7AsmSyn.LowerProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TArmV7AsmSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TArmV7AsmSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'a'..'f', 'h', 'A'..'F', 'H'] do
    Inc(Run);
end;

procedure TArmV7AsmSyn.SlashProc;
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

procedure TArmV7AsmSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TArmV7AsmSyn.StringProc;
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

procedure TArmV7AsmSyn.SingleQuoteStringProc;
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

procedure TArmV7AsmSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TArmV7AsmSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkIdentifier;
end;

procedure TArmV7AsmSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TArmV7AsmSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TArmV7AsmSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TArmV7AsmSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TArmV7AsmSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer
  );
begin
 TokenLength:=Run-fTokenPos;
 TokenStart:=FLine + fTokenPos;
end;

function TArmV7AsmSyn.GetTokenAttribute: TSynHighlighterAttributes;
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
	tkSystemValue : Result               := fSystemValueAttri;
    else Result := nil;
  end;
end;

function TArmV7AsmSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TArmV7AsmSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TArmV7AsmSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TArmV7AsmSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TArmV7AsmSyn.GetLanguageName: string;
begin
  Result := SYNS_LangArmV7Asm;
end;

function TArmV7AsmSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterArmV7Assembly;
end;

function TArmV7AsmSyn.GetSampleSource: string;
begin
  Result := '// Simple not working ARM v7 assembly' + #13#10 +
           '' + #13#10 +
           '    lsl    r3, r1, #4' + #13#10 +
           '    sub    r3, r3, r1' + #13#10 +
           '    add    r3, r3, r0' + #13#10 +
           '' + #13#10 +
           '    strh   r2, [r3]';
end;

initialization
  MakeIdentTable;
end.


