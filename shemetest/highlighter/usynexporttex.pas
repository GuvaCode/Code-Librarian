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
unit uSynExportTeX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditExport, SynEditHighlighter, Graphics;

Type

    { TSynExporterTeX }

    TSynExporterTeX = class(TSynCustomExporter)
      Private
        FCreateTeXFragment: Boolean;
       FMargin        : Integer;
       FLastAttri     : TSynHighlighterAttributes;
       FPageStyleEmpty: Boolean;
       FTabWidth: Integer;

       Function AttriToCommand(Attri: TSynHighlighterAttributes;
                               UniqueAttriName: string): string;
       Function AttriToCommandCallback(lHighlighter: TSynCustomHighlighter;
                                       Attri: TSynHighlighterAttributes; UniqueAttriName: string;
                                       Params: array of Pointer): Boolean;
       Function CommandNameCallback(lHighlighter: TSynCustomHighlighter;
                                    Attri: TSynHighlighterAttributes; UniqueAttriName: string;
                                    Params: array of Pointer): Boolean;
       Function GetCommandName(lHighlighter: TSynCustomHighlighter;
                               Attri: TSynHighlighterAttributes): string;
       Function GetNewCommands: string;
       Function MakeValidName(lName: string): string;

      Protected
       procedure FormatAfterLastAttribute; override;
       procedure FormatAttributeDone(BackgroundChanged: Boolean;
                                     ForegroundChanged: Boolean; FontStylesChanged: TFontStyles); override;
       procedure FormatAttributeInit(BackgroundChanged: Boolean;
                                     ForegroundChanged: Boolean; FontStylesChanged: TFontStyles); override;
       procedure FormatBeforeFirstAttribute(BackgroundChanged: Boolean;
                                            ForegroundChanged: Boolean; FontStylesChanged: TFontStyles); override;
       procedure FormatNewLine; override;
       procedure FormatToken(Token: UnicodeString); override;
       function GetFooter: UnicodeString; override;
       function GetFormatName: string; override;
       function GetHeader: UnicodeString; override;
       function ReplaceReservedChar(AChar: WideChar): UnicodeString; override;
       procedure SetTokenAttribute(Attri: TSynHighlighterAttributes); override;
       function UseBom: Boolean; override;

      Public
       constructor Create(AOwner: TComponent); override;
       //function SupportedEncodings: TSynEncodings; override;

      Published
       property Margin: Integer read FMargin write FMargin default 2;
       property TabWidth: Integer read FTabWidth write FTabWidth default 2;
       property Color;
       property CreateTeXFragment: Boolean read FCreateTeXFragment
          write FCreateTeXFragment default false;
       property PageStyleEmpty: Boolean read FPageStyleEmpty write FPageStyleEmpty
          default false;
       property DefaultFilter;
       //property Encoding;
       property Font;
       property Highlighter;
       property Title;
       property UseBackground;
    end;

implementation

{ TSynExporterTeX }

function TSynExporterTeX.AttriToCommand(Attri: TSynHighlighterAttributes;
  UniqueAttriName: string): string;
 Const
       NewCommand    = '\newcommand{\%s}[1]{%s#1%s}';
       SBold         = '\textbf{';
       SItalic       = '\textit{';
       SUnderline    = '\uln{';
       SColor        = '\textcolor[rgb]{%s}{';
       SBackColor    = '\colorbox[rgb]{%s}{';

 Var Formatting: string;
     BracketCount: Integer;
begin
 BracketCount := 0;

 With Attri Do
  Begin
   If fsBold In Style Then
    Begin
     Formatting := Formatting + SBold;

     Inc(BracketCount);
    end;

   If fsItalic In Style Then
    Begin
     Formatting := Formatting + fsItalic;

     Inc(BracketCount);
    end;

   If fsUnderline In Style Then
    Begin
     Formatting := Formatting + fsUnderline;

     Inc(BracketCount);
    end;

   If (Foreground <> clBlack) And (Foreground <> clNone) Then
    Begin
     Formatting := Formatting + Format(SColor, [ColorToTeX(Foreground)]);

     Inc(BracketCount);
    end;

   If fUseBackground And (Background <> clNone) Then
    Begin
     Formatting := Formatting + Format(SBackColor, [ColorToTeX(Background)]);

     Inc(BracketCount);
    end;

   Result := Format(NewCommand, [MakeValidName(UniqueAttriName), Formatting,
                    StringOfChar('}', BracketCount)])
  end;
end;

function TSynExporterTeX.AttriToCommandCallback(
  lHighlighter: TSynCustomHighlighter; Attri: TSynHighlighterAttributes;
  UniqueAttriName: string; Params: array of Pointer): Boolean;
begin

end;

function TSynExporterTeX.CommandNameCallback(
  lHighlighter: TSynCustomHighlighter; Attri: TSynHighlighterAttributes;
  UniqueAttriName: string; Params: array of Pointer): Boolean;
begin

end;

function TSynExporterTeX.GetCommandName(lHighlighter: TSynCustomHighlighter;
  Attri: TSynHighlighterAttributes): string;
begin

end;

function TSynExporterTeX.GetNewCommands: string;
begin

end;

function TSynExporterTeX.MakeValidName(lName: string): string;
begin

end;

procedure TSynExporterTeX.FormatAfterLastAttribute;
begin

end;

procedure TSynExporterTeX.FormatAttributeDone(BackgroundChanged: Boolean;
  ForegroundChanged: Boolean; FontStylesChanged: TFontStyles);
begin

end;

procedure TSynExporterTeX.FormatAttributeInit(BackgroundChanged: Boolean;
  ForegroundChanged: Boolean; FontStylesChanged: TFontStyles);
begin

end;

procedure TSynExporterTeX.FormatBeforeFirstAttribute(
  BackgroundChanged: Boolean; ForegroundChanged: Boolean;
  FontStylesChanged: TFontStyles);
begin

end;

procedure TSynExporterTeX.FormatNewLine;
begin

end;

procedure TSynExporterTeX.FormatToken(Token: UnicodeString);
begin
  inherited FormatToken(Token);
end;

function TSynExporterTeX.GetFooter: UnicodeString;
begin

end;

function TSynExporterTeX.GetFormatName: string;
begin
  Result:=inherited GetFormatName;
end;

function TSynExporterTeX.GetHeader: UnicodeString;
begin

end;

function TSynExporterTeX.ReplaceReservedChar(AChar: WideChar): UnicodeString;
begin

end;

procedure TSynExporterTeX.SetTokenAttribute(Attri: TSynHighlighterAttributes);
begin
  inherited SetTokenAttribute(Attri);
end;

function TSynExporterTeX.UseBom: Boolean;
begin

end;

constructor TSynExporterTeX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

(*
function TSynExporterTeX.SupportedEncodings: TSynEncodings;
begin

end;
*)

end.

