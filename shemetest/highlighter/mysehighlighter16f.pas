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
unit MySEHighlighter16F;

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

  { TMySynP16FSyn }

  TMySynP16FSyn = class(TSynCustomHighlighter)
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
  OpCodes : String = 'addwf,andwf,clrf,clrw,comf,decf,decfsz,incf,incfsz,iorwf,' +
                        'movf,movwf,nop,rlf,rrf,subwf,swapf,xorwf,bcf,bsf,btfsc,' +
                        'btfss,addlw,andlw,call,clrwdt,goto,iorlw,movlw,retfie,'+
                        'retlw,return,sleep,sublw,xorlw';

  SystemValues : String = 'adcapl,adcaph,adcon0,adcon1,adres,adtmrl,admrh,ccp1con,' +
                             'ccp2con,ccpr1l,ccpr1h,ccpr2l,ccpr2h,cmcon,eeadr,eecon1,' +
                             'eecon2,eedata,fsr,gpio,i2cadd,i2ccon,i2cstat,indf,intcon,' +
                             'lcdcon,lcdd00,lcdd01,lcdd02,lcdd03,lcdd04,lcdd05,lcdd06,' +
                             'lcdd07,lcdd08,lcdd09,lcdd10,lcdd11,lcdd12,lcdd13,lcdd14,' +
                             'lcdd15,lcdps,lcdse,option_reg,osccal,pcl,pclath,pcon,'+
                             'pie1,pie2,pir1,pir2,porta,portb,portc,portd,porte,portf,' +
                             'portg,pr2,prefa,prefb,rcsta,rcreg,slpcon,spbrg,sspbuf,' +
                             'sspcon,sspadd,sspstat,status,t1con,t2con,tmr0,tmr1l,' +
                             'tmr1h,tmr2,tris,trisa,trisb,trisc,trisd,trise,trisf,' +
                             'trisg,txreg,txsta,vrcon,w,mpeen,per,por,bor,irp,rp1,' +
                             'rp0,to,pd,z,dc,c,rbpu,intedg,t0cs,t0se,psa,ps2,ps1,ps0,' +
                             'wrerr,wren,wr,rd,gie,peie,t0ie,inte,rbie,t0if,' +
                             'intf,rbif,tmr1ie,tmr2ie,ccp1ie,ccp2ie,sspie,rcie,txie,' +
                             'adie,adcie,ovfie,pspie,eeie,lcdie,cmie,tmr1if,tmr2if,' +
                             'ccp1if,ccp2if,sspif,rcif,txif,adcif,ovfif,pspif,' +
                             'lcdif,cmif,ibf,obf,ibov,pspmode,trise2,trise1,' +
                             'trise0,t1ckps1,t1ckps0,t1oscen,t1sync,tmr1cs,tmr1on,' +
                             'toutps3,toutps2,toutps1,toutps0,tmr2on,t2ckps1,t2ckps0,' +
                             'dc1b1,dc2b1,dc1b0,dc2b0,ccp1m3,ccp1m2,ccp1m1,ccp1m0,' +
                             'ccp2m3,ccp2m2,ccp2m1,ccp2m0,smp,cke,da,p,s,rw,ua,bf,' +
                             'wcol,sspov,sspen,ckp,sspm3,sspm2,sspm1,sspm0,gcen,' +
                             'ackstat,ackdt,acken,rcen,pen,rsen,sen,csrc,tx9,txen,' +
                             'sync,brgh,trmt,tx9d,spen,rx9,sren,cren,ferr,oerr,rx9d,' +
                             'vren,vroe,vrr,vr3,vr2,vr1,vr0,c2out,c1out,cis,cm2,cm1,' +
                             'cm0,chs2,chs1,chs0,go,done,adon,pcfg2,' +
                             'pcfg1,pcfg0,adif,pcfg3,adfm,adcs3,adcs2,adcs1,' +
                             'amuxoe,adrst,adzero,addac3,addac2,addac1,addac0,refoff,' +
                             'oscoff,adoff,lcden,slpen,vgen,cs1,cs0,lmux1,lmux0,' +
                             'lp3,lp2,lp1,lp0,seg0com0,seg1com1,seg2com2,seg3com3,' +
                             'seg4com4,seg5com5,seg6com6,seg7com7,se29,se27,se20,' +
                             'se16,se12,se9,se5,se0';

     SYNS_FilterP16Assembly        =  'PIC16 Assembly Files (*.p16,*.asm)|*.p16;*.asm';
     SYNS_LangP16Asm               =  'Assembly Microchip PIC16';

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

function TMySynP16FSyn.KeyHash(ToHash: PChar): Integer;
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

function TMySynP16FSyn.KeyComp(const aKey: String): Boolean;
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

procedure TMySynP16FSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TMySynP16FSyn.IdentKind(MayBe: PChar): TtkTokenKind;
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

procedure TMySynP16FSyn.MakeMethodTables;
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

constructor TMySynP16FSyn.Create(AOwner: TComponent);
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
  fDefaultFilter      := SYNS_FilterP16Assembly;
end;

destructor TMySynP16FSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TMySynP16FSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  Inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TMySynP16FSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TMySynP16FSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TMySynP16FSyn.GreaterProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TMySynP16FSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TMySynP16FSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TMySynP16FSyn.LowerProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TMySynP16FSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TMySynP16FSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'a'..'f', 'h', 'A'..'F', 'H'] do
    Inc(Run);
end;

procedure TMySynP16FSyn.SlashProc;
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

procedure TMySynP16FSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TMySynP16FSyn.StringProc;
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

procedure TMySynP16FSyn.SingleQuoteStringProc;
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

procedure TMySynP16FSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TMySynP16FSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkIdentifier;
end;

procedure TMySynP16FSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TMySynP16FSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TMySynP16FSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TMySynP16FSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TMySynP16FSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer
  );
begin
 TokenLength:=Run-fTokenPos;
 TokenStart:=FLine + fTokenPos;
end;

function TMySynP16FSyn.GetTokenAttribute: TSynHighlighterAttributes;
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

function TMySynP16FSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TMySynP16FSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TMySynP16FSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TMySynP16FSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TMySynP16FSyn.GetLanguageName: string;
begin
  Result := SYNS_LangP16Asm;
end;

function TMySynP16FSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterP16Assembly;
end;

function TMySynP16FSyn.GetSampleSource: string;
begin
  Result := '; PICmicro Pic16 MCU not working assembly A/D Conversion' + #13#10 +
           '' + #13#10 +
           '    bsf    Status, RP0' + #13#10 +
           '    clrf   ADCON1' + #13#10 +
           '    bcf    Status, RP0' + #13#10 +
           '    movlw  0xC1' + #13#10 +
           '    movwf  ADCON0' + #13#10 +
           '    bsf    INTCON, ADIE' + #13#10 +
           '    bsf    INTCON, GIE' + #13#10 +
           '' + #13#10 +
           '    bsf    ADCON0, GO';
end;

initialization
  MakeIdentTable;
end.


