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
unit MySEHighlighter18F;

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

  { TMySynP18FSyn }

  TMySynP18FSyn = class(TSynCustomHighlighter)
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
  OpCodes : String = 'addwf,addwfc,andwf,clrf,comf,cpfseq,cpfsgt,cpfslt,decf,' +
                        'decfsz,dcfsnz,incf,incfsz,infsnz,iorwf,movf,movff,movwf,' +
                        'mulwf,negf,rlcf,rlncf,rrcf,rrncf,setf,subfwb,subwf,subwfb,' +
                        'swapf,tstfsz,xorwf,bcf,bsf,btfsc,btfss,btg,bc,bn,bnc,' +
                        'bnn,bnov,bnz,bov,bra,bz,call,clrwdt,daw,goto,nop,pop,' +
                        'push,rcall,reset,retfie,return,sleep,addlw,andlw,' +
                        'iorlw,lfsr,movlb,movlw,mullw,retlw,sublw,xorlw,tblrd,' +
                        'tblwt';

     SystemValues : String = 'tosu,tosh,tosl,stkptr,pclatu,pclath,pcl,tblptru,' +
                             'tblptrh,tblptrl,tablat,prodh,prodl,intcon,intcon2,' +
                             'intcon3,indf0,postinc0,postdec0,preinc0,plusw0,fsr0h,' +
                             'fsr0l,wreg,indf1,postinc1,postdec1,preinc1,plusw1,' +
                             'fsr1h,fsr1l,bsr,indf2,postinc2,postdec2,preinc2,plusw2,' +
                             'fsr2h,fsr2l,status,tmr0h,tmr0l,t0con,osccon,lvdcon,' +
                             'wdtcon,rcon,tmr1h,tmr1l,t1con,tmr2,pr2,t2con,sspbuf,' +
                             'sspadd,sspstat,sspcon1,sspcon2,adresh,adresl,adcon0,' +
                             'adcon1,ccpr1h,ccpr1l,ccp1con,ccpr2h,ccpr2l,ccp2con,' +
                             'tmr3h,tmr3l,t3con,spbrg,rcreg,txreg,txsta,rcsta,ipr2,' +
                             'pir2,pie2,ipr1,pir1,pie1,trise,trisd,trisc,trisb,tris,' +
                             'late,latd,latc,latb,lata,porte,portd,portc,portb,porta,' +
                             'gie,gieh,peie,geil,tmr0ie,int0ie,rbie,tmr0if,intoif,' +
                             'rbif,rbup,intedg0,intedg1,intedg2,tmr0ip,rbip,int2ip,' +
                             'int1ip,int2ie,int1ie,int2if,int1if,tmr1ie,tmr2ie,tmr3ie,' +
                             'ccp1ie,cpp2ie,eccp1ie,eccp2ie,sspie,msspie,rcie,txie,' +
                             'irxie,wakie,errie,txb2ie,txb1ie,txb0ie,rxb1ie,rxb0ie,' +
                             'adie,pspie,eeie,cmie,bclie,lvdie,tmr1if,tmr2if,tmr3if,' +
                             'ccp1if,ccp2if,eccp1if,eccp2if,irxif,wakif,errif,txb2if,' +
                             'txb1if,txb0if,rxb1if,rxb0if,sspif,msspif,rcif,txif,' +
                             'pspif,eeif,cmif,bclif,lvdif,tmr1ip,tmr2ip,tmp3ip,' +
                             'ccp1ip,ccp2ip,eccp1ip,eccp2ip,msspip,sspip,rcip,txip,' +
                             'adip,pspip,irxip,wakip,errip,txb2ip,txb1ip,txb0ip,' +
                             'rxb1ip,rxb0ip,eeip,cmip,bclip,lvdip,ipen,lwrt,ri,to,' +
                             'pd,por,bor,adcs0,chs2,chs1,chs0,go,done,chs3,' +
                             'adon,adfm,adcs2,pcfg3,pcfg2,pcfg1,pcfg0';

     SYNS_FilterP18FAssembly        =  'PIC18F Assembly Files (*.p18f,*.asm)|*.p18f;*.asm';
     SYNS_LangP18FAsm               =  'Assembly Microchip PIC18F';

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

function TMySynP18FSyn.KeyHash(ToHash: PChar): Integer;
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

function TMySynP18FSyn.KeyComp(const aKey: String): Boolean;
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

procedure TMySynP18FSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TMySynP18FSyn.IdentKind(MayBe: PChar): TtkTokenKind;
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

procedure TMySynP18FSyn.MakeMethodTables;
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

constructor TMySynP18FSyn.Create(AOwner: TComponent);
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
  fDefaultFilter      := SYNS_FilterP18FAssembly;
end;

destructor TMySynP18FSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TMySynP18FSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  Inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TMySynP18FSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TMySynP18FSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TMySynP18FSyn.GreaterProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TMySynP18FSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TMySynP18FSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TMySynP18FSyn.LowerProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TMySynP18FSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TMySynP18FSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'a'..'f', 'h', 'A'..'F', 'H'] do
    Inc(Run);
end;

procedure TMySynP18FSyn.SlashProc;
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

procedure TMySynP18FSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TMySynP18FSyn.StringProc;
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

procedure TMySynP18FSyn.SingleQuoteStringProc;
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

procedure TMySynP18FSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TMySynP18FSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkIdentifier;
end;

procedure TMySynP18FSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TMySynP18FSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TMySynP18FSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TMySynP18FSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TMySynP18FSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer
  );
begin
 TokenLength:=Run-fTokenPos;
 TokenStart:=FLine + fTokenPos;
end;

function TMySynP18FSyn.GetTokenAttribute: TSynHighlighterAttributes;
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

function TMySynP18FSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TMySynP18FSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TMySynP18FSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TMySynP18FSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TMySynP18FSyn.GetLanguageName: string;
begin
  Result := SYNS_LangP18FAsm;
end;

function TMySynP18FSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterP18FAssembly;
end;

function TMySynP18FSyn.GetSampleSource: string;
begin
  Result := '//This is simple not working PIC18F code' + Chr(13) +
           'COUNT equ 0x01' + Chr(13) +
           'NUM equ 0x02' + Chr(13) +
           '' + Chr(13) +
           'clrf WREG ; clear W' + Chr(13) +
           '' + Chr(13) +
           'Init' + Chr(13) +
           '      clrf COUNT ; set COUNT to zero' + Chr(13) +
           '      movlw d''100'' ; move the literal decimal 100 to W' + Chr(13) +
           '      movwf NUM ; mov';
end;

initialization
  MakeIdentTable;
end.


