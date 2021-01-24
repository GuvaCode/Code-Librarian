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
unit uPiNoteOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Fgl, Variants, Dialogs;

Const
     SectionSyntaxScheme       = 'SyntaxSchemeInfo';
     SectionStartUpPiNote      = 'StartUpPiNote';
     SectionUsefulThings       = 'UsefulThings';
     SectionDocument           = 'Document';
     SectionRecentFiles        = 'RecentFiles';
     SectionWindowPos          = 'WindowPos';

     KeySyntaxSchemeCount      = 'SyntaxCount';
     KeySyntaxSchemeTheme      = 'SyntaxTheme';
     KeySyntaxMainFore         = 'SyntaxMainFore';
     KeySyntaxMainBack         = 'SyntaxMainBack';
     KeySyntaxEditedCount      = 'SyntaxEditedCount';
     KeySyntaxEditedName       = 'SyntaxEditedName';
     KeySyntaxLineFore         = 'SyntaxLineFore';
     KeySyntaxLineBack         = 'SyntaxLineBack';
     KeyAttrCount              = 'AttrCount';

     KeyItem                   = 'Item';
     KeyEntries                = 'Entries';

     KeyFontInUse              = 'FontInUse';
     KeyFontSize               = 'FontSize';
     KeyFontStyle              = 'FontStyle';
     KeyFontQuality            = 'FontQuality';
     KeyTabWidth               = 'TabWidth';
     KeyMaxUndo                = 'MaxUndo';
     KeyRightEdge              = 'RightEdge';
     KeyAutoCB                 = 'AutoCompleteBrackets';
     KeyUseRecentFile          = 'UseRecentFile';
     KeyCheckFile              = 'CheckFile';

Type
    TPiNoteType      = (pnString, pnInteger, pnBoolean);

    TPiNoteOption     = Class
     Private
       fSectionName       : String;
       fKeyName           : String;
       fValue             : Variant;
       fDefaultValue      : Variant;
       fType              : TPiNoteType;

     Public
       Property SectionName : String Read fSectionName Write fSectionName;
       Property KeyName : String Read fKeyName Write fKeyName;
       Property Value : Variant Read fValue Write fValue;
       Property DefaultValue : Variant Read fDefaultValue Write fDefaultValue;
       Property OptionType : TPiNoteType Read fType Write fType Default pnString;
    end;

    TPiNoteOptionList            = Specialize TFPGObjectList <TPiNoteOption>;

    { TPiNoteOptions }

    TPiNoteOptions               = Class
     Private
       fIniFileName              : String;
       fIniFile                  : TIniFile;
       fOptionList               : TPiNoteOptionList;
       fSectionList              : TStringList;

     Public
       Constructor Create(Const IniFileName : String);
       Destructor Destroy; Override;

       Procedure AddOption(Const SectionName, KeyName : String; Value, DefaultValue : Variant);
       Function GetOption(Const SectionName, KeyName : String) : Variant;
       Function GetOptionBoolean(Const SectionName, KeyName : String) : Boolean;
       Procedure SetOption(Const SectionName, KeyName : String; Value : Variant);
       Procedure Clear;
       Procedure ClearSection(Const SectionName : String);

       Procedure SaveToFile;

       Property SectionList : TStringList Read fSectionList Write fSectionList;
    end;

implementation

{ TPiNoteOptions }

constructor TPiNoteOptions.Create(const IniFileName: String);
 Var tSec, sList : TStringList;
     Ind, IndK, iVal : Integer;
     OrigKey, sVal : String;
     bVal : Boolean;
begin
 fIniFileName := IniFileName;

 fIniFile := TIniFile.Create(fIniFileName);

 fOptionList := TPiNoteOptionList.Create();
 fSectionList := TStringList.Create;
 fSectionList.Sorted := True;
 fSectionList.Duplicates := dupIgnore;

 If FileExists(fIniFileName) Then
  Begin
   sList := TStringList.Create;

   fIniFile.ReadSections(sList);

   tSec := TStringList.Create;

   For Ind := 0 To sList.Count - 1 Do
    Begin
     tSec.Clear;

     fIniFile.ReadSection(sList[Ind], tSec);

     For IndK := 0 To tSec.Count - 1 Do
      Begin
       If pos('.s', tSec[IndK]) > 0 Then
        Begin
         OrigKey := StringReplace(tSec[IndK], '.s', '', [rfReplaceAll]);
         sVal := fIniFile.ReadString(sList[Ind], tSec[IndK], '');

         AddOption(sList[Ind], OrigKey, sVal, '');
        end;

       If pos('.i', tSec[IndK]) > 0 Then
        Begin
         OrigKey := StringReplace(tSec[IndK], '.i', '', [rfReplaceAll]);
         iVal := fIniFile.ReadInteger(sList[Ind], tSec[IndK], -1);

         AddOption(sList[Ind], OrigKey, iVal, -1);
        end;

       If pos('.b', tSec[IndK]) > 0 Then
        Begin
         OrigKey := StringReplace(tSec[IndK], '.b', '', [rfReplaceAll]);
         bVal := fIniFile.ReadBool(sList[Ind], tSec[IndK], False);

         AddOption(sList[Ind], OrigKey, bVal, False);
        end;
      end;
    end;

   tSec.Free;
   sList.Free;
  end;
end;

destructor TPiNoteOptions.Destroy;
begin
 fSectionList.Free;
 fOptionList.Free;
 fIniFile.Free;

 Inherited Destroy;
end;

procedure TPiNoteOptions.AddOption(const SectionName, KeyName: String; Value,
  DefaultValue: Variant);
 Var Opt : TPiNoteOption;
begin
 fSectionList.Add(SectionName);

 Opt := TPiNoteOption.Create;
 Opt.SectionName := SectionName;
 Opt.KeyName := KeyName;
 Opt.Value := Value;
 Opt.DefaultValue := DefaultValue;

 Case varType(Value) Of
      varString      : Opt.OptionType := pnString;
      varInteger     : Opt.OptionType := pnInteger;
      varBoolean     : Opt.OptionType := pnBoolean;
 end;

 fOptionList.Add(Opt);
end;

function TPiNoteOptions.GetOption(const SectionName, KeyName: String): Variant;
 Var I : Integer;
begin
 Result := -1;

 If Not fSectionList.Find(SectionName, I) Then
  Exit;

 For I := 0 To fOptionList.Count - 1 Do
  Begin
   If fOptionList[I].SectionName = SectionName Then
    If fOptionList[I].KeyName = KeyName Then
     Begin
      Result := fOptionList[I].Value;

      Break;
     end;
  end;
end;

function TPiNoteOptions.GetOptionBoolean(const SectionName, KeyName: String
  ): Boolean;
Var I : Integer;
begin
Result := False;

If Not fSectionList.Find(SectionName, I) Then
 Exit;

For I := 0 To fOptionList.Count - 1 Do
 Begin
  If fOptionList[I].SectionName = SectionName Then
   If fOptionList[I].KeyName = KeyName Then
    Begin
     Result := fOptionList[I].Value;

     Break;
    end;
 end;
end;

procedure TPiNoteOptions.SetOption(const SectionName, KeyName: String;
  Value: Variant);
 Var I : Integer;
     Fnd : Boolean;
begin
 Fnd := False;

 For I := 0 To fOptionList.Count - 1 Do
  If fOptionList[I].SectionName = SectionName Then
   If fOptionList[I].KeyName = KeyName Then
    Begin
     Fnd := True;

     fOptionList[I].Value := Value;

      Case varType(Value) Of
            varString      : fOptionList[I].OptionType := pnString;
            varInteger     : fOptionList[I].OptionType := pnInteger;
            varBoolean     : fOptionList[I].OptionType := pnBoolean;
      end;

      Break;
    end;

 If Not Fnd Then
  AddOption(SectionName, KeyName, Value, Value);
end;

procedure TPiNoteOptions.Clear;
begin
 fOptionList.Clear;
 fSectionList.Clear;
end;

procedure TPiNoteOptions.ClearSection(const SectionName: String);
 Var I : Integer;
begin
 For I := fOptionList.Count - 1 DownTo 0 Do
  If fOptionList[I].SectionName = SectionName Then
   fOptionList.Delete(I);
end;

procedure TPiNoteOptions.SaveToFile;
 Var I : Integer;
begin
 For I := 0 To fOptionList.Count - 1 Do
  Begin
   Case fOptionList[I].OptionType Of
        pnString       : fIniFile.WriteString(fOptionList[I].SectionName, fOptionList[I].KeyName + '.s',
                                              VarToStr(fOptionList[I].Value));
        pnInteger      : fIniFile.WriteInteger(fOptionList[I].SectionName, fOptionList[I].KeyName + '.i',
                                               Integer(fOptionList[I].Value));
        pnBoolean      : fIniFile.WriteBool(fOptionList[I].SectionName, fOptionList[I].KeyName + '.b',
                                            Boolean(fOptionList[I].Value));
   end;
  end;

 fIniFile.UpdateFile;
end;

end.

