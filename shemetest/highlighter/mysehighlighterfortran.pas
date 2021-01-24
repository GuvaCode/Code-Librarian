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
unit MySEHighlighterFortran;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter, SynEditTypes;

type
  TtkTokenKind       = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
                        tkString, tkSymbol, tkUnknown);

  TProcTableProc     = procedure of object;

  PIdentFuncTableFunc  = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc  = function: TtkTokenKind of object;

  { TMySynFortranSyn }

  TMySynFortranSyn                 = Class(TSynCustomHighlighter)
   Private
    fLine                          : PChar;
    fLineNumber                    : Integer;
    fProcTable                     : array[#0..#255] of TProcTableProc;
    Run                            : LongInt;
    fStringLen                     : Integer;
    fToIdent                       : PChar;
    fTokenPos                      : Integer;
    FTokenID                       : TtkTokenKind;
    fIdentFuncTable                : array[0..145] of TIdentFuncTableFunc;
    fCommentAttri                  : TSynHighlighterAttributes;
    fIdentifierAttri               : TSynHighlighterAttributes;
    fKeyAttri                      : TSynHighlighterAttributes;
    fNumberAttri                   : TSynHighlighterAttributes;
    fSpaceAttri                    : TSynHighlighterAttributes;
    fStringAttri                   : TSynHighlighterAttributes;
    fSymbolAttri                   : TSynHighlighterAttributes;

    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func145: TtkTokenKind;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure ExclamationProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure CommentProc;

   Protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;

   Public
    class function GetLanguageName: string; override;
    constructor Create(AOwner: TComponent); override;

    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(Const NewValue: AnsiString; LineNumber: LongInt); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    procedure Next; override;

   Published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
  End;

implementation

Uses SynEditStrConst;

Var
   Identifiers       : array[#0..#255] of ByteBool;
   mHashTable        : array[#0..#255] of Integer;

Procedure MakeIdentTable;
 Var I, J: Char;
begin
 For I := #0 To #255 Do
  Begin
   Case I Of
     '_',
     '0'..'9',
     'a'..'z',
     'A'..'Z'       : Identifiers[I] := True;
     Else             Identifiers[I] := False;
   End;

   J := UpCase(I);

   Case I In ['_', 'A'..'Z', 'a'..'z'] Of
      True                   : mHashTable[I] := Ord(J) - 64
      Else                     mHashTable[I] := 0;
   End;
  End;
end;

{ TMySynFortranSyn }

function TMySynFortranSyn.KeyHash(ToHash: PChar): Integer;
begin
 Result := 0;

 While ToHash^ In ['_', '0'..'9', 'a'..'z', 'A'..'Z'] Do
  Begin
   Inc(Result, mHashTable[ToHash^]);

   Inc(ToHash);
  End;

 fStringLen := ToHash - fToIdent;
end;

function TMySynFortranSyn.KeyComp(const aKey: String): Boolean;
 Var I: Integer;
     Temp: PChar;
begin
 Temp := fToIdent;

 If Length(aKey) = fStringLen Then
  Begin
   Result := True;

   For I := 1 To fStringLen Do
    Begin
     If mHashTable[Temp^] <> mHashTable[aKey[I]] Then
      Begin
       Result := False;

       Break;
      End;

     Inc(Temp);
    End;
  End
 Else
  Result := False;
end;

function TMySynFortranSyn.Func15: TtkTokenKind;
begin
 If KeyComp('if') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func19: TtkTokenKind;
begin
 If KeyComp('do') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func23: TtkTokenKind;
begin
 If KeyComp('end') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func26: TtkTokenKind;
begin
 If KeyComp('data') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func28: TtkTokenKind;
begin
 If KeyComp('call') Then
  Result := tkKey
 Else
  If KeyComp('case') Then
   Result := tkKey
  Else
   If KeyComp('read') Then
    Result := tkKey
   Else
    Result := tkIdentifier;
end;

function TMySynFortranSyn.Func30: TtkTokenKind;
begin
 If KeyComp('map') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func36: TtkTokenKind;
begin
 If KeyComp('real') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func38: TtkTokenKind;
begin
 If KeyComp('endif') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func41: TtkTokenKind;
begin
 If KeyComp('else') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func42: TtkTokenKind;
begin
 If KeyComp('enddo') Then
  Result := tkKey
 Else
  If KeyComp('enddo') Then
   Result := tkKey
  Else
   Result := tkIdentifier;
end;

function TMySynFortranSyn.Func47: TtkTokenKind;
begin
 If KeyComp('then') Then
  Result := tkKey
 Else
  If KeyComp('save') Then
   Result := tkKey
  Else
   Result := tkIdentifier;
end;

function TMySynFortranSyn.Func48: TtkTokenKind;
begin
 If KeyComp('cycle') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func50: TtkTokenKind;
begin
 If KeyComp('open') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func56: TtkTokenKind;
begin
 If KeyComp('elseif') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func57: TtkTokenKind;
begin
 If KeyComp('while') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func58: TtkTokenKind;
begin
 If KeyComp('exit') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func59: TtkTokenKind;
begin
 If KeyComp('logical') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func61: TtkTokenKind;
begin
 If KeyComp('value') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func63: TtkTokenKind;
begin
 If KeyComp('record') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func64: TtkTokenKind;
begin
 If KeyComp('select') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func66: TtkTokenKind;
begin
 If KeyComp('type') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func68: TtkTokenKind;
begin
 If KeyComp('include') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func69: TtkTokenKind;
begin
 If KeyComp('allocate') Then
  Result := tkKey
 Else
  If KeyComp('default') Then
   Result := tkKey
  Else
   Result := tkIdentifier;
end;

function TMySynFortranSyn.Func70: TtkTokenKind;
begin
 If KeyComp('stop') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func73: TtkTokenKind;
begin
 If KeyComp('union') Then
  Result := tkKey
 Else
  If KeyComp('common') Then
   Result := tkKey
  Else
   If KeyComp('format') Then
    Result := tkKey
   Else
    Result := tkIdentifier;
end;

function TMySynFortranSyn.Func75: TtkTokenKind;
begin
 If KeyComp('write') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func77: TtkTokenKind;
begin
 If KeyComp('character') Then
  Result := tkKey
 Else
  If KeyComp('print') Then
   Result := tkKey
  Else
   Result := tkIdentifier;
end;

function TMySynFortranSyn.Func78: TtkTokenKind;
begin
 If KeyComp('integer') Then
  Result := tkKey
 Else
  If KeyComp('deallocate') Then
   Result := tkKey
  Else
   Result := tkIdentifier;
end;

function TMySynFortranSyn.Func81: TtkTokenKind;
begin
 If KeyComp('interface') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func82: TtkTokenKind;
begin
 If KeyComp('entry') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func84: TtkTokenKind;
begin
 If KeyComp('allocatable') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func88: TtkTokenKind;
begin
 If KeyComp('program') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func96: TtkTokenKind;
begin
 If KeyComp('return') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func97: TtkTokenKind;
begin
 If KeyComp('parameter') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func99: TtkTokenKind;
begin
 If KeyComp('external') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func101: TtkTokenKind;
begin
 If KeyComp('continue') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func102: TtkTokenKind;
begin
 If KeyComp('function') Then
  Result := tkKey
 Else
  If KeyComp('dimension') Then
   Result := tkKey
  Else
   Result := tkIdentifier;
end;

function TMySynFortranSyn.Func114: TtkTokenKind;
begin
 If KeyComp('equivalence') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func127: TtkTokenKind;
begin
 If KeyComp('stucture') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func144: TtkTokenKind;
begin
 If KeyComp('subroutine') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

function TMySynFortranSyn.Func145: TtkTokenKind;
begin
 If KeyComp('structure') Then
  Result := tkKey
 Else
  Result := tkIdentifier;
end;

procedure TMySynFortranSyn.AsciiCharProc;
begin
 fTokenID := tkString;

 Repeat
  Case FLine[Run] Of
   #0, #10, #13     : Break;
  End;

  Inc(Run);

 Until FLine[Run] = #39;

 If FLine[Run] <> #0 Then
  Inc(Run);
end;

procedure TMySynFortranSyn.CRProc;
begin
 fTokenID := tkSpace;

 Inc(Run);

 If fLine[Run] = #10 Then
  Inc(Run);
end;

procedure TMySynFortranSyn.CommaProc;
begin
 Inc(Run);

 fTokenID := tkSymbol;
end;

procedure TMySynFortranSyn.EqualProc;
begin
 Case FLine[Run + 1] Of
      '='         : Begin
                     Inc(Run, 2);

                     fTokenID := tkSymbol;
                    End;

      Else          Begin
                     Inc(Run);

                     fTokenID := tkSymbol;
                    End;
 End;
end;

procedure TMySynFortranSyn.ExclamationProc;
begin
 Inc(Run, 1);

 fTokenID := tkComment;

 While FLine[Run] <> #0 Do
  Begin
   Case FLine[Run] Of
        #10, #13        : Break;
   End;

   Inc(Run);
  End;
end;

procedure TMySynFortranSyn.GreaterProc;
begin
 Case FLine[Run + 1] Of
      '='            : Begin
                        Inc(Run, 2);

                        fTokenID := tkSymbol;
                       End;

      '>'            : Begin
                        If FLine[Run + 2] = '=' Then
                         Inc(Run, 3)
                        Else
                         Inc(Run, 2);

                        fTokenID := tkSymbol;
                       End;

      Else             Begin
                        Inc(Run);

                        fTokenID := tkSymbol;
                       End;
 End;
end;

procedure TMySynFortranSyn.IdentProc;
begin
 If (FLine[Run] In ['C', 'c']) And (Run = 0) Then
  Begin
   Inc(Run);

   CommentProc;
  End
 Else
  Begin
   fTokenID := IdentKind((fLine + Run));

   Inc(Run, fStringLen);

   While Identifiers[fLine[Run]] Do
    Inc(Run);
  End;
end;

procedure TMySynFortranSyn.LFProc;
begin
 Inc(Run);
 fTokenID := tkSpace;
end;

procedure TMySynFortranSyn.LowerProc;
begin
 Case FLine[Run + 1] Of
      '='            : Begin
                        Inc(Run, 2);

                        fTokenID := tkSymbol;
                       End;

      '<'            : Begin
                        If FLine[Run + 2] = '=' Then
                         Inc(Run, 3)
                        Else
                         Inc(Run, 2);

                        fTokenID := tkSymbol;
                       End;

      Else             Begin
                        Inc(Run);

                        fTokenID := tkSymbol;
                       End;
 End;
end;

procedure TMySynFortranSyn.MinusProc;
begin
 Inc(Run);
 fTokenID := tkSymbol;
end;

procedure TMySynFortranSyn.ModSymbolProc;
begin
 Inc(Run);
 fTokenID := tkSymbol;
end;

procedure TMySynFortranSyn.NullProc;
begin
 fTokenID := tkNull;
end;

procedure TMySynFortranSyn.NumberProc;
begin
 Inc(Run);
 fTokenID := tkNumber;

 While FLine[Run] In ['0'..'9', '.', 'x', 'X', 'e', 'E', 'f', 'F'] Do
  Begin
   Case FLine[Run] Of
        '.'        : If FLine[Run + 1] = '.' Then
                      Break;
   End;

   Inc(Run);
  End;
end;

procedure TMySynFortranSyn.PlusProc;
begin
 Inc(Run);
 fTokenID := tkSymbol;
end;

procedure TMySynFortranSyn.PointProc;
begin
 If (((UpCase(FLine[Run + 1]) = 'G') And (UpCase(FLine[Run + 2]) In ['E','T'])) Or
     ((UpCase(FLine[Run + 1]) = 'L') And (UpCase(FLine[Run + 2]) In ['E','T'])) Or
     ((UpCase(FLine[Run + 1]) = 'N') And (UpCase(FLine[Run + 2]) = 'E')) Or
     ((UpCase(FLine[Run + 1]) = 'E') And (UpCase(FLine[Run + 2]) = 'Q')) Or
     ((UpCase(FLine[Run + 1]) = 'O') And (UpCase(FLine[Run + 2]) = 'R'))) And
     (FLine[Run + 3] = '.') Then
  Begin
   Inc(Run, 4);

   fTokenID := tkSymbol;
  End
 Else
  If (((UpCase(FLine[Run + 1]) = 'A') And (UpCase(FLine[Run + 2]) = 'N') And
       (UpCase(FLine[Run + 3]) = 'D')) Or ((UpCase(FLine[Run + 1]) = 'N') And
       (UpCase(FLine[Run + 2]) = 'O') And (UpCase(FLine[Run + 3]) = 'T'))) And
       (FLine[Run + 4] = '.') Then
   Begin
    Inc(Run, 5);

    fTokenID := tkSymbol;
   End
  Else
   If (UpCase(FLine[Run + 1]) = 'T') And (UpCase(FLine[Run + 2]) = 'R') And
      (UpCase(FLine[Run + 3]) = 'U') And (UpCase(FLine[Run + 4]) = 'E') And
      (FLine[Run + 5] = '.') Then
    Begin
     Inc(Run, 6);

     fTokenID := tkSymbol;
    End
   Else
    If (UpCase(FLine[Run + 1]) = 'F') And (UpCase(FLine[Run + 2]) = 'A') And
       (UpCase(FLine[Run + 3]) = 'L') And (UpCase(FLine[Run + 4]) = 'S') And
       (UpCase(FLine[Run + 5]) = 'E') And (FLine[Run + 6] = '.') Then
     Begin
      Inc(Run, 7);

      fTokenID := tkSymbol;
     End
    Else
     Begin
      Inc(Run);

      fTokenID := tkSymbol;
     End;
end;

procedure TMySynFortranSyn.RoundCloseProc;
begin
 Inc(Run);

 fTokenID := tkSymbol;
end;

procedure TMySynFortranSyn.RoundOpenProc;
begin
 Inc(Run);

 fTokenID := tkSymbol;
end;

procedure TMySynFortranSyn.SemiColonProc;
begin
 Inc(Run);

 fTokenID := tkSymbol;
end;

procedure TMySynFortranSyn.SlashProc;
begin
 Inc(Run);

 fTokenID := tkSymbol;
end;

procedure TMySynFortranSyn.SpaceProc;
begin
 Inc(Run);

 fTokenID := tkSpace;

 While FLine[Run] In [#1..#9, #11, #12, #14..#32] Do
  Inc(Run);
end;

procedure TMySynFortranSyn.StarProc;
begin
 If Run = 0 Then
  Begin
   Inc(Run);

   CommentProc;
  End
 Else
  Begin
   Inc(Run);

   fTokenID := tkSymbol;
  End;
end;

procedure TMySynFortranSyn.StringProc;
begin
 fTokenID := tkString;

 If (FLine[Run + 1] = #34) And (FLine[Run + 2] = #34) Then
  Inc(Run, 2);

 Repeat
  Case FLine[Run] Of
       #0, #10, #13   : Break;
       #92            : If FLine[Run + 1] = #10 Then
                         Inc(Run);
  End;

  Inc(Run);
 Until FLine[Run] = #34;

 If FLine[Run] <> #0 Then
  Inc(Run);
end;

procedure TMySynFortranSyn.UnknownProc;
begin
 Inc(Run);
 fTokenID := tkUnknown;
end;

function TMySynFortranSyn.AltFunc: TtkTokenKind;
begin
 Result := tkIdentifier;
end;

procedure TMySynFortranSyn.InitIdent;
 Var I: Integer;
     pF: PIdentFuncTableFunc;
begin
 pF := PIdentFuncTableFunc(@fIdentFuncTable);

 For I := Low(fIdentFuncTable) To High(fIdentFuncTable) Do
  Begin
   pF^ := @AltFunc;

   Inc(pF);
  End;

 fIdentFuncTable[15] := @Func15;
 fIdentFuncTable[19] := @Func19;
 fIdentFuncTable[23] := @Func23;
 fIdentFuncTable[26] := @Func26;
 fIdentFuncTable[28] := @Func28;
 fIdentFuncTable[30] := @Func30;
 fIdentFuncTable[36] := @Func36;
 fIdentFuncTable[38] := @Func38;
 fIdentFuncTable[41] := @Func41;
 fIdentFuncTable[42] := @Func42;
 fIdentFuncTable[47] := @Func47;
 fIdentFuncTable[48] := @Func48;
 fIdentFuncTable[50] := @Func50;
 fIdentFuncTable[56] := @Func56;
 fIdentFuncTable[57] := @Func57;
 fIdentFuncTable[58] := @Func58;
 fIdentFuncTable[59] := @Func59;
 fIdentFuncTable[61] := @Func61;
 fIdentFuncTable[63] := @Func63;
 fIdentFuncTable[64] := @Func64;
 fIdentFuncTable[66] := @Func66;
 fIdentFuncTable[68] := @Func68;
 fIdentFuncTable[69] := @Func69;
 fIdentFuncTable[70] := @Func70;
 fIdentFuncTable[73] := @Func73;
 fIdentFuncTable[75] := @Func75;
 fIdentFuncTable[77] := @Func77;
 fIdentFuncTable[78] := @Func78;
 fIdentFuncTable[81] := @Func81;
 fIdentFuncTable[82] := @Func82;
 fIdentFuncTable[84] := @Func84;
 fIdentFuncTable[88] := @Func88;
 fIdentFuncTable[96] := @Func96;
 fIdentFuncTable[97] := @Func97;
 fIdentFuncTable[99] := @Func99;
 fIdentFuncTable[101] := @Func101;
 fIdentFuncTable[102] := @Func102;
 fIdentFuncTable[114] := @Func114;
 fIdentFuncTable[127] := @Func127;
 fIdentFuncTable[144] := @Func144;
 fIdentFuncTable[145] := @Func145;
end;

function TMySynFortranSyn.IdentKind(MayBe: PChar): TtkTokenKind;
 Var HashKey: Integer;
begin
 fToIdent := MayBe;

 HashKey := KeyHash(MayBe);

 If HashKey < 146 Then
  Result := fIdentFuncTable[HashKey]()
 Else
  Result := tkIdentifier;
end;

procedure TMySynFortranSyn.MakeMethodTables;
 Var I : Char;
begin
 For I := #0 To #255 Do
  Case I Of
         #39                     : fProcTable[I] := @AsciiCharProc;
         #13                     : fProcTable[I] := @CRProc;
         ','                     : fProcTable[I] := @CommaProc;
         '='                     : fProcTable[I] := @EqualProc;
         '!'                     : fProcTable[I] := @ExclamationProc;
         '>'                     : fProcTable[I] := @GreaterProc;
         'A'..'Z',
         'a'..'z',
         '_'                     : fProcTable[I] := @IdentProc;
         #10                     : fProcTable[I] := @LFProc;
         '<'                     : fProcTable[I] := @LowerProc;
         '-'                     : fProcTable[I] := @MinusProc;
         '%'                     : fProcTable[I] := @ModSymbolProc;
         #0                      : fProcTable[I] := @NullProc;
         '0'..'9'                : fProcTable[I] := @NumberProc;
         '+'                     : fProcTable[I] := @PlusProc;
         '.'                     : fProcTable[I] := @PointProc;
         ')'                     : fProcTable[I] := @RoundCloseProc;
         '('                     : fProcTable[I] := @RoundOpenProc;
         ';'                     : fProcTable[I] := @SemiColonProc;
         '/'                     : fProcTable[I] := @SlashProc;
         #1..#9,
         #11,
         #12,
         #14..#32                : fProcTable[I] := @SpaceProc;
         '*'                     : fProcTable[I] := @StarProc;
         #34                     : fProcTable[I] := @StringProc;
         Else                      fProcTable[I] := @UnknownProc;
  End;
end;

procedure TMySynFortranSyn.CommentProc;
begin
 fTokenID := tkComment;

 While FLine[Run] <> #0 Do
  Begin
   Case FLine[Run] Of
        #10, #13        : Break;
   End;

   Inc(Run);
  End;
end;

function TMySynFortranSyn.GetIdentChars: TSynIdentChars;
begin
 Result := TSynValidStringChars;
end;

function TMySynFortranSyn.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterFortran;
end;

class function TMySynFortranSyn.GetLanguageName: string;
begin
 Result := SYNS_LangFortran;
end;

constructor TMySynFortranSyn.Create(AOwner: TComponent);
begin
 Inherited Create(AOwner);

 fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
 fCommentAttri.Style := [fsItalic];
 AddAttribute(fCommentAttri);
 fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
 AddAttribute(fIdentifierAttri);
 fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
 fKeyAttri.Style := [fsBold];
 AddAttribute(fKeyAttri);
 fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
 AddAttribute(fNumberAttri);
 fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
 AddAttribute(fSpaceAttri);
 fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
 AddAttribute(fStringAttri);
 fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
 AddAttribute(fSymbolAttri);

 SetAttributesOnChange(@DefHighlightChange);
 InitIdent;
 MakeMethodTables;
 fDefaultFilter := SYNS_FilterFortran;

 StringAttri.Foreground := clRed;
 NumberAttri.Foreground := clFuchsia;
 SymbolAttri.Foreground := clOlive;
 IdentifierAttri.Foreground := clNavy;
 CommentAttri.Foreground := clTeal;
end;

function TMySynFortranSyn.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
 Case Index Of
    SYN_ATTR_COMMENT           : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER        : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD           : Result := fKeyAttri;
    SYN_ATTR_STRING            : Result := fStringAttri;
    SYN_ATTR_WHITESPACE        : Result := fSpaceAttri;
    SYN_ATTR_SYMBOL            : Result := fSymbolAttri;
    Else                         Result := Nil;
 End;
end;

function TMySynFortranSyn.GetEol: Boolean;
begin
 Result := fTokenID = tkNull;
end;

function TMySynFortranSyn.GetTokenID: TtkTokenKind;
begin
 Result := fTokenId;
end;

procedure TMySynFortranSyn.SetLine(Const NewValue: AnsiString; LineNumber: LongInt);
begin
 fLine := PChar(NewValue);
 Run := 0;
 fLineNumber := LineNumber;

 Next;
end;

function TMySynFortranSyn.GetToken: String;
 Var Len: LongInt;
begin
 Len := Run - fTokenPos;

 SetString(Result, (FLine + fTokenPos), Len);
end;

function TMySynFortranSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
 Case GetTokenID Of
    tkComment                  : Result := fCommentAttri;
    tkIdentifier               : Result := fIdentifierAttri;
    tkKey                      : Result := fKeyAttri;
    tkNumber                   : Result := fNumberAttri;
    tkSpace                    : Result := fSpaceAttri;
    tkString                   : Result := fStringAttri;
    tkSymbol                   : Result := fSymbolAttri;
    tkUnknown                  : Result := fIdentifierAttri;
    Else                         Result := Nil;
 End;
end;

function TMySynFortranSyn.GetTokenKind: integer;
begin
 Result := Ord(fTokenId);
end;

function TMySynFortranSyn.GetTokenPos: Integer;
begin
 Result := fTokenPos;
end;

procedure TMySynFortranSyn.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
 TokenLength := Run - fTokenPos;
 TokenStart := FLine + fTokenPos;
end;

procedure TMySynFortranSyn.Next;
begin
 fTokenPos := Run;

 fProcTable[fLine[Run]];
end;

initialization
 MakeIdentTable;

end.

