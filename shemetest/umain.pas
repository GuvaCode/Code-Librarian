unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit, SynEditHighlighter,
  uPiNoteOptions, uSynSchOpt,
  SynEditStrConst;

type

  { Tmain }

  Tmain = class(TForm)
    Button1: TButton;
    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);

    procedure FormCreate(Sender: TObject);
  private
    WorkingSynSch : TSynCustomHighlighter;
    OldCaretPos : TPoint;
  public
    ShowLineHighlight:boolean;
    PiNoteOptions : TPiNoteOptions;
    SyntaxSchemeID : Integer;
    PiNoteDefaultSyntaxScheme : Integer;
    Procedure ApplySyntaxOptions;
    procedure ApplyStartUpOptions;
    procedure UpdateEditor;

  end;

var
  main: Tmain;

implementation
 uses Variants, uThemesDefault, uSyntaxList, uSyntaxDefault;
{$R *.lfm}

 { Tmain }

 procedure Tmain.FormCreate(Sender: TObject);
 var i:integer;
 begin
  WorkingSynSch := Nil;
  CreateSyntaxSchemeList;
  CreateDefaultThemes;
  PiNoteOptions := TPiNoteOptions.Create('codelibrarian.ini'); //patch to ide
  SyntaxSchemeID:=5;
  ApplySyntaxOptions;

  ApplyStartUpOptions;
///  SynEdit1.Highlighter:=WorkingSynSch;
  ShowLineHighlight:=true;
  UpdateEditor;
 end;

 procedure Tmain.ApplySyntaxOptions;
  Var oVal, LanName : Variant;
     I, SynIdx, A : Integer;
 begin
    oVal := PiNoteOptions.GetOption(SectionSyntaxScheme, KeySyntaxSchemeTheme);

 If oVal <> -1 Then
  ThemeInUseIdx := Integer(oVal);
 SelectTheme(ThemeInUseIdx, SyntaxSchemeList);
 oVal := PiNoteOptions.GetOption(SectionSyntaxScheme, KeySyntaxMainFore);
 If oVal <> -1 Then  MainForeground := Integer(oVal);
 oVal := PiNoteOptions.GetOption(SectionSyntaxScheme, KeySyntaxMainBack);
 If oVal <> -1 Then  MainBackground := Integer(oVal);
 oVal := PiNoteOptions.GetOption(SectionSyntaxScheme, KeySyntaxLineFore);
 If oVal <> -1 Then  LineForeground := Integer(oVal);
 oVal := PiNoteOptions.GetOption(SectionSyntaxScheme, KeySyntaxLineBack);
 If oVal <> -1 Then  LineBackground := Integer(oVal);

 oVal := PiNoteOptions.GetOption(SectionSyntaxScheme, KeySyntaxEditedCount);
 If Integer(oVal) > 0 Then
  For I := 0 To Integer(oVal) - 1 Do
   Begin
    LanName := PiNoteOptions.GetOption(SectionSyntaxScheme, IntToStr(I) + KeySyntaxEditedName);
    SynIdx := GetSyntaxSchemeIndex(LanName);
    For A := 0 To SyntaxSchemeList[SynIdx].AttrCount - 1 Do
     Begin
      SyntaxSchemeList[SynIdx].Attribute[A].Foreground := PiNoteOptions.GetOption(LanName, IntToStr(A)  + 'Fore');
      SyntaxSchemeList[SynIdx].Attribute[A].Background := PiNoteOptions.GetOption(LanName, IntToStr(A)  + 'Back');
      SyntaxSchemeList[SynIdx].Attribute[A].Style := [];
      If Boolean(PiNoteOptions.GetOption(LanName, IntToStr(A)  + 'StyleU')) Then
       SyntaxSchemeList[SynIdx].Attribute[A].Style := SyntaxSchemeList[SynIdx].Attribute[A].Style + [fsUnderLine];
      If Boolean(PiNoteOptions.GetOption(LanName, IntToStr(A)  + 'StyleB')) Then
       SyntaxSchemeList[SynIdx].Attribute[A].Style := SyntaxSchemeList[SynIdx].Attribute[A].Style + [fsBold];
      If Boolean(PiNoteOptions.GetOption(LanName, IntToStr(A)  + 'StyleI')) Then
       SyntaxSchemeList[SynIdx].Attribute[A].Style := SyntaxSchemeList[SynIdx].Attribute[A].Style + [fsItalic];
     end;
   end;
 end;

 procedure Tmain.ApplyStartUpOptions;
  Var oVal : Variant;
     I : Integer;
 begin
    oVal := PiNoteOptions.GetOption(SectionStartUpPiNote, KeyAttrCount);
     If oVal > 0 Then
  Begin
     oVal := PiNoteOptions.GetOption(SectionDocument, KeySyntaxSchemeTheme);

 If varType(oVal) = varString Then
  PiNoteDefaultSyntaxScheme := GetSyntaxSchemeIndex(oVal);
  end;

 end;

 procedure Tmain.UpdateEditor;
 Var TmpC : TPoint;
begin
{ SynEdit1.Font.Size := UserFontSize;
 SynEdit1.Font.Style := UserFontStyle;
 SynEdit1.Gutter.Visible := ShowLineNumber;
 SynEdit1.TabWidth := UserTabWidth;
 SynEdit1.MaxUndo := UserMaxUndo;
 sEdit.Font.Quality := UserFontQuality; }

 SynEdit1.Gutter.Color := MainBackground;

 SynEdit1.Gutter.Parts[2].MarkupInfo.Background := MainBackground;
 SynEdit1.Gutter.Parts[2].MarkupInfo.Foreground := MainForeground;

 SynEdit1.Font.Color := MainForeground;
 SynEdit1.Color := MainBackground;

 If ShowLineHighlight Then
  Begin
   SynEdit1.LineHighlightColor.Background := LineBackground;
   SynEdit1.LineHighlightColor.Foreground := LineForeground;
  end
 Else
  Begin
   SynEdit1.LineHighlightColor.Background := clNone;
   SynEdit1.LineHighlightColor.Foreground := clNone;
  end;

 SynEdit1.SelectedColor.Background := InvertColor((*LineBackground*)MainBackground);
 SynEdit1.SelectedColor.Foreground := (SynEdit1.SelectedColor.Background(*LineForeground*));

 If SyntaxSchemeID = -1 Then SynEdit1.Highlighter := Nil
 Else
  Begin
   SynEdit1.Highlighter := SyntaxSchemeList[SyntaxSchemeID];
   SynEdit1.Highlighter.Enabled := True;
  end;
 Self.Invalidate;
end;

procedure Tmain.Button1Click(Sender: TObject);
begin
   fSynSchOpt := TfSynSchOpt.Create(Self);
   fSynSchOpt.UsedSyntaxSchemeID := SyntaxSchemeID;

   fSynSchOpt.ShowModal;
   fSynSchOpt.Free;
end;

end.

