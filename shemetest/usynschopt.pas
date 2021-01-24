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
unit uSynSchOpt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ColorBox, Menus, Buttons, SynEditHighlighter, SynEdit,
  SynHighlighterAny, uSyntaxList;

type

  { TfSynSchOpt }

  TfSynSchOpt = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    cbfBold: TCheckBox;
    cbfItalic: TCheckBox;
    cbLanguages: TComboBox;
    cbfUnderLine: TCheckBox;
    clBMainBack: TColorBox;
    clBFore: TColorBox;
    clBBack: TColorBox;
    clBLineBack: TColorBox;
    clBMainFore: TColorBox;
    cbThemes: TComboBox;
    clBLineFore: TColorBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ThemeLabel: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbSections: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    LoadTheme: TOpenDialog;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SaveTheme: TSaveDialog;
    SynTest: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cbfBoldChange(Sender: TObject);
    procedure cbfItalicChange(Sender: TObject);
    procedure cbfUnderLineChange(Sender: TObject);
    procedure cbLanguagesChange(Sender: TObject);
    procedure cbThemesSelect(Sender: TObject);
    procedure clBBackSelect(Sender: TObject);
    procedure clBForeSelect(Sender: TObject);
    procedure clBLineBackSelect(Sender: TObject);
    procedure clBLineForeSelect(Sender: TObject);
    procedure clBMainBackSelect(Sender: TObject);
    procedure clBMainForeSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbSectionsClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure SynTestDblClick(Sender: TObject);
  private
    { private declarations }
    NeedToReset : Boolean;

    WorkingSynSch : TSynCustomHighlighter;

    Procedure BuildSections(SynSc : TSynCustomHighlighter);

    Procedure SaveValuesIntoOptions;

    Procedure ResetToDefault;

    Procedure LocalSelectTheme(ThemeIdx: Integer; SetDefaultValue : Boolean = False);

    Procedure RefreshTheme;
  public
    { public declarations }
    UsedSyntaxSchemeID : Integer;
  end;

var
  fSynSchOpt: TfSynSchOpt;

implementation

Uses uSyntaxDefault, uThemesDefault, uMain, {, uTabForm, uEditor,} uPiNoteOptions,
     SynEditStrConst, IniFiles;

{$R *.lfm}

{ TfSynSchOpt }

procedure TfSynSchOpt.FormShow(Sender: TObject);
 Var Ind : Integer;
begin
{ OldMainForeground := MainForeground;
 OldMainBackground := MainBackground;
 OldLineForeground := LineForeground;
 OldLineBackground := LineBackground;
 }
 StoreSyntaxSchemeToMemory;

 For Ind := 0 To SyntaxSchemeNameList.Count - 1 Do
  cbLanguages.Items.Add(SyntaxSchemeNameList[Ind]);

{ clBMainFore.Selected := MainForeground;
 clBMainBack.Selected := MainBackground;
 clBLineFore.Selected := LineForeground;
 clBLineBack.Selected := LineBackground;}

 If UsedSyntaxSchemeID <> -1 Then
  Begin
   cbLanguages.Text := SyntaxSchemeList[UsedSyntaxSchemeID].LanguageName;

   cbLanguagesChange(Sender);
  end
 Else
  Begin
   cbLanguages.ItemIndex := 0;
   cbLanguagesChange(Sender);
  end;
end;

procedure TfSynSchOpt.lbSectionsClick(Sender: TObject);
begin
 If lbSections.Count > 0 Then
  If (lbSections.ItemIndex > -1) And (WorkingSynSch <> Nil) Then
   Begin
    clBFore.Selected := WorkingSynSch.Attribute[lbSections.ItemIndex].Foreground;
    clBBack.Selected := WorkingSynSch.Attribute[lbSections.ItemIndex].Background;

    cbfUnderLine.Checked := fsUnderline In WorkingSynSch.Attribute[lbSections.ItemIndex].Style;
    cbfBold.Checked := fsBold In WorkingSynSch.Attribute[lbSections.ItemIndex].Style;
    cbfItalic.Checked := fsItalic In WorkingSynSch.Attribute[lbSections.ItemIndex].Style;
   end;
end;

procedure TfSynSchOpt.MenuItem1Click(Sender: TObject);
 Var fI : TIniFile;
     I : Integer;
begin
 If WorkingSynSch = Nil Then
  Exit;

 If SaveTheme.Execute Then
  Begin
   fI := TIniFile.Create(SaveTheme.FileName);

   fI.WriteString(SectionSyntaxScheme, KeySyntaxEditedName, cbThemes.Text);
   fI.WriteInteger(SectionSyntaxScheme, KeySyntaxMainFore, SynTest.Font.Color);
   fI.WriteInteger(SectionSyntaxScheme, KeySyntaxMainBack, SynTest.Color);
   fI.WriteInteger(SectionSyntaxScheme, KeySyntaxLineFore, SynTest.LineHighlightColor.Foreground);
   fI.WriteInteger(SectionSyntaxScheme, KeySyntaxLineBack, SynTest.LineHighlightColor.Background);

   fI.WriteInteger(SectionSyntaxScheme, KeyAttrCount, WorkingSynSch.AttrCount);

   For I := 0 To WorkingSynSch.AttrCount -1 Do
    Begin
     fI.WriteInteger(SectionSyntaxScheme, KeySyntaxLineFore + IntToStr(I), WorkingSynSch.Attribute[I].Foreground);
     fI.WriteInteger(SectionSyntaxScheme, KeySyntaxLineBack + IntToStr(I), WorkingSynSch.Attribute[I].Background);

     fI.WriteBool(SectionSyntaxScheme, 'StyleU' + IntToStr(I), fsUnderLine In WorkingSynSch.Attribute[I].Style);
     fI.WriteBool(SectionSyntaxScheme, 'StyleB' + IntToStr(I), fsBold In WorkingSynSch.Attribute[I].Style);
     fI.WriteBool(SectionSyntaxScheme, 'StyleI' + IntToStr(I), fsItalic In WorkingSynSch.Attribute[I].Style);
    end;

   fI.UpdateFile;

   fI.Free;
  end;
end;

procedure TfSynSchOpt.MenuItem2Click(Sender: TObject);
 Var fI : TIniFile;
     sTmp : String;
     I, aCnt : Integer;
begin
 If WorkingSynSch = Nil Then
  Exit;

 If LoadTheme.Execute Then
  Begin
   fI := TIniFile.Create(LoadTheme.FileName);

   sTmp := fI.ReadString(SectionSyntaxScheme, KeySyntaxEditedName, '');

   If sTmp <> '' Then
    Begin
     I := cbThemes.Items.IndexOf(sTmp);

     If I > -1 Then
      Begin
       cbThemes.ItemIndex := I;

       cbThemesSelect(Sender);

       SynTest.Font.Color := fI.ReadInteger(SectionSyntaxScheme, KeySyntaxMainFore, 0);
       SynTest.Color := fI.ReadInteger(SectionSyntaxScheme, KeySyntaxMainBack, 0);
       SynTest.LineHighlightColor.Foreground := fI.ReadInteger(SectionSyntaxScheme, KeySyntaxLineFore, 0);
       SynTest.LineHighlightColor.Background := fI.ReadInteger(SectionSyntaxScheme, KeySyntaxLineBack, 0);

       aCnt := fI.ReadInteger(SectionSyntaxScheme, KeyAttrCount, 0);

       If aCnt > 0 Then
        For I := 0 To aCnt - 1 Do
         Begin
          WorkingSynSch.Attribute[I].Foreground := fI.ReadInteger(SectionSyntaxScheme, KeySyntaxLineFore + IntToStr(I), 0);
          WorkingSynSch.Attribute[I].Background := fI.ReadInteger(SectionSyntaxScheme, KeySyntaxLineBack + IntToStr(I), 0);

          WorkingSynSch.Attribute[I].Style := [];

          If fI.ReadBool(SectionSyntaxScheme, 'StyleU' + IntToStr(I), False) Then
           WorkingSynSch.Attribute[I].Style := WorkingSynSch.Attribute[I].Style + [fsUnderLine];

          If fI.ReadBool(SectionSyntaxScheme, 'StyleB' + IntToStr(I), False) Then
           WorkingSynSch.Attribute[I].Style := WorkingSynSch.Attribute[I].Style + [fsBold];

          If fI.ReadBool(SectionSyntaxScheme, 'StyleI' + IntToStr(I), False) Then
           WorkingSynSch.Attribute[I].Style := WorkingSynSch.Attribute[I].Style + [fsItalic];

          WorkingSynSch.Tag := 1;
         end;
      end;
    end;

   fI.Free;
  end;
end;

procedure TfSynSchOpt.SynTestDblClick(Sender: TObject);
begin
{ If Main.OpenFile.Execute Then
  Begin
   SynTest.Clear;
   SynTest.Lines.LoadFromFile(Main.OpenFile.FileName);
  end;  }
end;

procedure TfSynSchOpt.cbLanguagesChange(Sender: TObject);
begin
 BuildSections(OldSyntaxSchemeList[GetSyntaxSchemeIndex(SyntaxSchemeNameList[cbLanguages.ItemIndex])]);

 RefreshTheme;
end;

procedure TfSynSchOpt.cbThemesSelect(Sender: TObject);
begin
 LocalSelectTheme(cbThemes.ItemIndex);
end;

procedure TfSynSchOpt.cbfUnderLineChange(Sender: TObject);
begin
 If cbfUnderLine.Checked Then
  WorkingSynSch.Attribute[lbSections.ItemIndex].Style := WorkingSynSch.Attribute[lbSections.ItemIndex].Style + [fsUnderLine]
 Else
  WorkingSynSch.Attribute[lbSections.ItemIndex].Style := WorkingSynSch.Attribute[lbSections.ItemIndex].Style - [fsUnderLine];

 WorkingSynSch.Tag := 1;
end;

procedure TfSynSchOpt.cbfBoldChange(Sender: TObject);
begin
 If cbfBold.Checked Then
  WorkingSynSch.Attribute[lbSections.ItemIndex].Style := WorkingSynSch.Attribute[lbSections.ItemIndex].Style + [fsBold]
 Else
  WorkingSynSch.Attribute[lbSections.ItemIndex].Style := WorkingSynSch.Attribute[lbSections.ItemIndex].Style - [fsBold];

 WorkingSynSch.Tag := 1;
end;

procedure TfSynSchOpt.Button1Click(Sender: TObject);
begin
 Close;
end;

procedure TfSynSchOpt.Button2Click(Sender: TObject);
 Var I : Integer;
     Tmp : TForm;
begin
 Screen.Cursor := crHourGlass;

 ThemeInUseIdx := cbThemes.ItemIndex;

 MainForeground := clBMainFore.Selected;
 MainBackground := clBMainBack.Selected;
 LineForeground := clBLineFore.Selected;
 LineBackground := clBLineBack.Selected;

 GetSyntaxSchemeFromMemory;

 SaveValuesIntoOptions;

 Main.PiNoteOptions.SaveToFile;

 Main.ApplySyntaxOptions;

 Main.UpdateEditor;

 Screen.Cursor := crDefault;
 NeedToReset := False;

 Close;
end;

procedure TfSynSchOpt.Button3Click(Sender: TObject);
begin
 If cbLanguages.ItemIndex < 0 Then
  Exit;

 If cbThemes.ItemIndex > -1 Then
  LocalSelectTheme(cbThemes.ItemIndex, True);
end;

procedure TfSynSchOpt.Button4Click(Sender: TObject);
 Var Pt, Pt2 : TPoint;
begin
 Pt.x := Button4.Left;
 Pt.y := Button4.Top + Button4.Height;

 Pt2 := ClientToScreen(Pt);

 PopUpMenu1.PopUp(Pt2.x, Pt2.y);
end;

procedure TfSynSchOpt.cbfItalicChange(Sender: TObject);
begin
 If cbfItalic.Checked Then
  WorkingSynSch.Attribute[lbSections.ItemIndex].Style := WorkingSynSch.Attribute[lbSections.ItemIndex].Style + [fsItalic]
 Else
  WorkingSynSch.Attribute[lbSections.ItemIndex].Style := WorkingSynSch.Attribute[lbSections.ItemIndex].Style - [fsItalic];

 WorkingSynSch.Tag := 1;
end;

procedure TfSynSchOpt.clBBackSelect(Sender: TObject);
begin
 WorkingSynSch.Attribute[lbSections.ItemIndex].Background := clBBack.Selected;
 WorkingSynSch.Tag := 1;
end;

procedure TfSynSchOpt.clBForeSelect(Sender: TObject);
begin
 WorkingSynSch.Attribute[lbSections.ItemIndex].Foreground := clBFore.Selected;
 WorkingSynSch.Tag := 1;
end;

procedure TfSynSchOpt.clBLineBackSelect(Sender: TObject);
begin
 SynTest.LineHighlightColor.Background := clBLineBack.Selected;
end;

procedure TfSynSchOpt.clBLineForeSelect(Sender: TObject);
begin
 SynTest.LineHighlightColor.Foreground := clBLineFore.Selected;
end;

procedure TfSynSchOpt.clBMainBackSelect(Sender: TObject);
begin
 SynTest.Color := clBMainBack.Selected;
end;

procedure TfSynSchOpt.clBMainForeSelect(Sender: TObject);
 Var I : Integer;
begin
 SynTest.Font.Color := clBMainFore.Selected;

 If WorkingSynSch <> Nil Then
  For I := 0 To WorkingSynSch.AttrCount - 1 Do
   If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrIdentifier Then
    WorkingSynSch.Attribute[I].Foreground := clBMainFore.Selected;
end;

procedure TfSynSchOpt.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 If NeedToReset Then
  ResetToDefault;
end;

procedure TfSynSchOpt.FormCreate(Sender: TObject);
 Var I : Integer;
begin
 UsedSyntaxSchemeID := -1;
 NeedToReset := True;
 For I := 0 To Length(PiNoteThemes) - 1 Do
  cbThemes.Items.Add(PiNoteThemes[I].Name);

 cbThemes.ItemIndex := ThemeInUseIdx;

 ThemeLabel.Caption := ThemeLabel.Caption + ' (' + IntToStr(Length(PiNoteThemes)) +
                   ' items on the list)';
 clBFore.NoneColorColor := clNone;
 clBBack.NoneColorColor := clNone;
 clBMainFore.NoneColorColor := clNone;
 clBMainBack.NoneColorColor := clNone;
 clBLineFore.NoneColorColor := clNone;
 clBLineBack.NoneColorColor := clNone;
 WorkingSynSch := Nil;

// SynTest.HighlightAllColor:=Main.SynEdit1.HighlightAllColor;
 //SynEdit1.SelectedColor.Background := InvertColor((*LineBackground*)MainBackground);
 //SynEdit1.SelectedColor.Foreground := (SynEdit1.SelectedColor.Background(*LineForeground*));

 //cbThemesSelect(Nil);


end;

procedure TfSynSchOpt.BuildSections(SynSc: TSynCustomHighlighter);
 Var Ind : Integer;
begin
 WorkingSynSch := SynSc;

 lbSections.Clear;

 For Ind := 0 To SynSc.AttrCount - 1 Do
  lbSections.Items.Add(IntToStr(Ind) + '- ' + SynSc.Attribute[Ind].Name);

 If SynSc.SampleSource <> '' Then
  SynTest.Text := SynSc.SampleSource
 Else
  SynTest.Text := UndefinedSampleSource[GetSyntaxSchemeIndex(SyntaxSchemeNameList[cbLanguages.ItemIndex])];

 SynTest.Highlighter := WorkingSynSch;
 SynTest.Highlighter.Enabled := True;
end;

procedure TfSynSchOpt.SaveValuesIntoOptions;
 Var I, A, CntEdited : Integer;
     LnSection : String;
begin
 CntEdited := 0;
 Main.PiNoteOptions.ClearSection(SectionSyntaxScheme);
 Main.PiNoteOptions.AddOption(SectionSyntaxScheme, KeySyntaxSchemeCount, SyntaxSchemeList.Count, 0);
 Main.PiNoteOptions.AddOption(SectionSyntaxScheme, KeySyntaxSchemeTheme, cbThemes.ItemIndex, 0);
 Main.PiNoteOptions.AddOption(SectionSyntaxScheme, KeySyntaxMainFore, MainForeground, 0);
 Main.PiNoteOptions.AddOption(SectionSyntaxScheme, KeySyntaxMainBack, MainBackground, 0);
 Main.PiNoteOptions.AddOption(SectionSyntaxScheme, KeySyntaxLineFore, LineForeground, 0);
 Main.PiNoteOptions.AddOption(SectionSyntaxScheme, KeySyntaxLineBack, LineBackground, 0);

 For I := 0 To SyntaxSchemeList.Count - 1 Do
  If SyntaxSchemeList[I].Tag <> 0 Then  //Solo gli highliter modificati
   Begin
    LnSection := SyntaxSchemeList[I].GetLanguageName;
    Main.PiNoteOptions.AddOption(SectionSyntaxScheme, IntToStr(CntEdited) + KeySyntaxEditedName, LnSection, '');
    Inc(CntEdited);
    Main.PiNoteOptions.ClearSection(LnSection);
    Main.PiNoteOptions.AddOption(LnSection, KeyAttrCount, SyntaxSchemeList[I].AttrCount, 0);

    For A := 0 To SyntaxSchemeList[I].AttrCount - 1 Do
     Begin
      Main.PiNoteOptions.AddOption(LnSection, IntToStr(A) + 'Fore',
                                   SyntaxSchemeList[I].Attribute[A].Foreground, 0);
      Main.PiNoteOptions.AddOption(LnSection, IntToStr(A) + 'Back',
                                   SyntaxSchemeList[I].Attribute[A].Background, 0);
      Main.PiNoteOptions.AddOption(LnSection, IntToStr(A) + 'StyleU',
                                   fsUnderline In SyntaxSchemeList[I].Attribute[A].Style, False);
      Main.PiNoteOptions.AddOption(LnSection, IntToStr(A) + 'StyleB',
                                   fsBold In SyntaxSchemeList[I].Attribute[A].Style, False);
      Main.PiNoteOptions.AddOption(LnSection, IntToStr(A) + 'StyleI',
                                   fsItalic In SyntaxSchemeList[I].Attribute[A].Style, False);
     end;
   end;

 Main.PiNoteOptions.AddOption(SectionSyntaxScheme, KeySyntaxEditedCount, CntEdited, 0);
end;

procedure TfSynSchOpt.ResetToDefault;
begin
 MainForeground := OldMainForeground;
 MainBackground := OldMainBackground;
end;

procedure TfSynSchOpt.LocalSelectTheme(ThemeIdx: Integer; SetDefaultValue : Boolean = False);
 Var I, SynIdx : Integer;
begin
 clBMainFore.Selected := PiNoteThemes[ThemeIdx].MainForeground;
 clBMainBack.Selected := PiNoteThemes[ThemeIdx].MainBackground;
 clBLineFore.Selected := PiNoteThemes[ThemeIdx].LineForeground;
 clBLineBack.Selected := PiNoteThemes[ThemeIdx].LineBackground;

 clBLineBackSelect(Nil);
 clBLineForeSelect(Nil);
 clBMainForeSelect(Nil);
 clBMainBackSelect(Nil);

 MainForeground := PiNoteThemes[ThemeIdx].MainForeground;
 MainBackground := PiNoteThemes[ThemeIdx].MainBackground;
 LineForeground := PiNoteThemes[ThemeIdx].LineForeground;
 LineBackground := PiNoteThemes[ThemeIdx].LineBackground;

 ColorComment := PiNoteThemes[ThemeIdx].ColorComment;
 ColorNumber := PiNoteThemes[ThemeIdx].ColorNumber;
 ColorPreProcessor := PiNoteThemes[ThemeIdx].ColorPreProcessor;
 ColorReservedWord := PiNoteThemes[ThemeIdx].ColorReservedWord;
 ColorString := PiNoteThemes[ThemeIdx].ColorString;
 ColorSymbol := PiNoteThemes[ThemeIdx].ColorSymbol;

 //BuildDefaultAttribs;

 If WorkingSynSch <> Nil Then
  Begin
   For I := 0 To WorkingSynSch.AttrCount - 1 Do
    Begin
     If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrComment Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorComment;

     If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrNumber Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorNumber;

     If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrPreprocessor Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorPreProcessor;

     If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrReservedWord Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorReservedWord;

     If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrString Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorString;

     If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrSymbol Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorSymbol;

     If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrDirective Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorPreProcessor;

     If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrProcedureHeaderName Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].MainForeground;

     If WorkingSynSch.Attribute[I].StoredName = SYNS_XML_AttrCaseLabel Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].MainForeground;

     If (WorkingSynSch.Attribute[I].Foreground = clNone) Or
        (WorkingSynSch.Attribute[I].Foreground = clBlack) Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].MainForeground;
   end;

   If SetDefaultValue Then
    Exit;

   If ThemeInUseIdx <> ThemeIdx Then
    Exit;

   If Main.PiNoteOptions.SectionList.IndexOf(cbLanguages.Text) < 0 Then
    Exit;

   SynIdx := GetSyntaxSchemeIndex(cbLanguages.Text);

   For I := 0 To OldSyntaxSchemeList[SynIdx].AttrCount - 1 Do
    Begin
     OldSyntaxSchemeList[SynIdx].Attribute[I].Foreground := Main.PiNoteOptions.GetOption(cbLanguages.Text, IntToStr(I)  + 'Fore');
     OldSyntaxSchemeList[SynIdx].Attribute[I].Background := Main.PiNoteOptions.GetOption(cbLanguages.Text, IntToStr(I)  + 'Back');

     OldSyntaxSchemeList[SynIdx].Attribute[I].Style := [];



     If OldSyntaxSchemeList[SynIdx].Attribute[I].StoredName = SYNS_XML_AttrComment Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorComment;

     If OldSyntaxSchemeList[SynIdx].Attribute[I].StoredName = SYNS_XML_AttrNumber Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorNumber;

     If OldSyntaxSchemeList[SynIdx].Attribute[I].StoredName = SYNS_XML_AttrPreprocessor Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorPreProcessor;

     If OldSyntaxSchemeList[SynIdx].Attribute[I].StoredName = SYNS_XML_AttrReservedWord Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorReservedWord;

     If OldSyntaxSchemeList[SynIdx].Attribute[I].StoredName = SYNS_XML_AttrString Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorString;

     If OldSyntaxSchemeList[SynIdx].Attribute[I].StoredName = SYNS_XML_AttrSymbol Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorSymbol;

     If OldSyntaxSchemeList[SynIdx].Attribute[I].StoredName = SYNS_XML_AttrDirective Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].ColorPreProcessor;

     If OldSyntaxSchemeList[SynIdx].Attribute[I].StoredName = SYNS_XML_AttrProcedureHeaderName Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].MainForeground;

     If OldSyntaxSchemeList[SynIdx].Attribute[I].StoredName = SYNS_XML_AttrCaseLabel Then
      WorkingSynSch.Attribute[I].Foreground := PiNoteThemes[ThemeIdx].MainForeground;

     If Boolean(Main.PiNoteOptions.GetOption(cbLanguages.Text, IntToStr(I)  + 'StyleU')) Then
      OldSyntaxSchemeList[SynIdx].Attribute[I].Style := OldSyntaxSchemeList[SynIdx].Attribute[I].Style + [fsUnderLine];

     If Boolean(Main.PiNoteOptions.GetOption(cbLanguages.Text, IntToStr(I)  + 'StyleB')) Then
      OldSyntaxSchemeList[SynIdx].Attribute[I].Style := OldSyntaxSchemeList[SynIdx].Attribute[I].Style + [fsBold];

     If Boolean(Main.PiNoteOptions.GetOption(cbLanguages.Text, IntToStr(I)  + 'StyleI')) Then
      OldSyntaxSchemeList[SynIdx].Attribute[I].Style := OldSyntaxSchemeList[SynIdx].Attribute[I].Style + [fsItalic];

     SynTest.Color := main.SynEdit1.Color;


    end;
  end;
end;

procedure TfSynSchOpt.RefreshTheme;
begin
 clBMainFore.Selected := MainForeground;
 clBMainBack.Selected := MainBackground;
 clBLineFore.Selected := LineForeground;
 clBLineBack.Selected := LineBackground;

 clBLineBackSelect(Nil);
 clBLineForeSelect(Nil);
 clBMainForeSelect(Nil);
 clBMainBackSelect(Nil);

 LocalSelectTheme(cbThemes.ItemIndex);
end;

end.
