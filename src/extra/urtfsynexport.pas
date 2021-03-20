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
unit uRTFSynExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditExport, Graphics;

Type

    { TRtfSynExport }

    TRtfSynExport               = Class(TSynCustomExporter)
     Private
      fAttributesChanged           : Boolean;
      fListColors                  : TList;

      Function ColorToRTF(AColor: TColor) : String;
      Function GetColorIndex(AColor: TColor) : integer;

     Protected
      Procedure FormatAfterLastAttribute; Override;
      Procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: boolean;
                                    FontStylesChanged: TFontStyles); Override;
      Procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: boolean;
                                    FontStylesChanged: TFontStyles); Override;
      Procedure FormatBeforeFirstAttribute(BackgroundChanged,
                                           ForegroundChanged: boolean; FontStylesChanged: TFontStyles); Override;
      Procedure FormatNewLine; override;
      Function GetFooter: string; override;
      Function GetFormatName: string; override;
      Function GetHeader: string; override;
      //Function ReplaceReservedChar(AChar: WideChar): UnicodeString; override;
      //Function UseBom: Boolean; override;

     Public
      Constructor Create(AOwner: TComponent); override;
      Destructor Destroy; override;
      Procedure Clear; override;
      //Function SupportedEncodings: TSynEncodings; override;

     Published
      Property Color;
      Property DefaultFilter;
      //Property Encoding;
      Property Font;
      Property Highlighter;
      Property Title;
      Property UseBackground;
    End;

implementation

Const
     SYNS_ExporterFormatRTF                 =  'RTF';
     SYNS_FilterRTF                         =  'Rich Text Format Documents (*.rtf)|*.rtf';

{ TRtfSynExport }

function TRtfSynExport.ColorToRTF(AColor: TColor): String;
 Var Col : Integer;
begin
 Col := ColorToRGB(AColor);

 Result := Format('\red%d\green%d\blue%d;', [Red(Col), Green(Col), Blue(Col)]);
end;

function TRtfSynExport.GetColorIndex(AColor: TColor): integer;
begin
 Result := fListColors.IndexOf(Pointer(AColor));

 If Result = -1 Then
  Result := fListColors.Add(Pointer(AColor));
end;

procedure TRtfSynExport.FormatAfterLastAttribute;
begin
 // no need to reset the font style here...
end;

procedure TRtfSynExport.FormatAttributeDone(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
Const
     FontTags : Array[TFontStyle] Of String = ('\b0', '\i0', '\ul0', '\strike0');

 Var AStyle : TFontStyle;
begin
 For AStyle := Low(TFontStyle) To High(TFontStyle) Do
  Begin
   If AStyle In FontStylesChanged Then
    Begin
     fAttributesChanged := True;

     AddData(FontTags[AStyle]);
    End;
  End;
end;

procedure TRtfSynExport.FormatAttributeInit(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
Const
     FontTags : Array[TFontStyle] Of String = ('\b', '\i', '\ul', '\strike');

 Var AStyle: TFontStyle;
begin
 If BackgroundChanged Then
  Begin
   AddData(Format('\cb%d', [GetColorIndex(fLastBG)]));

   fAttributesChanged := True;
  End;

 If ForegroundChanged Then
  Begin
   AddData(Format('\cf%d', [GetColorIndex(fLastFG)]));

   fAttributesChanged := True;
  End;

 For AStyle := Low(TFontStyle) To High(TFontStyle) Do
  If AStyle In FontStylesChanged Then
   Begin
    AddData(FontTags[AStyle]);

    fAttributesChanged := True;
   End;

 If fAttributesChanged Then
  Begin
   AddData(' ');

   fAttributesChanged := False;
  End;
end;

procedure TRtfSynExport.FormatBeforeFirstAttribute(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
begin
 FormatAttributeInit(BackgroundChanged, ForegroundChanged, FontStylesChanged);
end;

procedure TRtfSynExport.FormatNewLine;
begin
 AddData(#13#10'\par ');
end;

function TRtfSynExport.GetFooter: string;
begin
 Result := '}';
end;

function TRtfSynExport.GetFormatName: string;
begin
 Result := SYNS_ExporterFormatRTF;
end;

function TRtfSynExport.GetHeader: string;
 Var I : Integer;

 Function GetFontTable : String;
 Begin
  Result := '{\fonttbl{\f0\fmodern ' + Font.Name;
  Result := Result + ';}}'#13#10;
 End;

begin
 Result := '{\rtf1\ansi\ansicpg1252\uc1\deff0\deftab720' + GetFontTable;
 Result := Result + '{\colortbl';

 For I := 0 To fListColors.Count - 1 Do
  Result := Result + ColorToRTF(TColor(fListColors[I]));

 Result := Result + '}'#13#10;
 //Result := Result + '{\info{\comment Generated by Fed-It RTF ' + 'exporter}'#13#10;
 Result := Result + '{\title ' + fTitle + '}}'#13#10;

 If fUseBackground Then
  Result := Result + { TODO } #13#10;

 Result := Result + Format('\deflang1033\pard\plain\f0\fs%d ', [2 * Font.Size]);
end;

constructor TRtfSynExport.Create(AOwner: TComponent);
begin
 Inherited Create(AOwner);

 fListColors := TList.Create;
 fDefaultFilter := SYNS_FilterRTF;

 fReplaceReserved['\'] := '\\';
 fReplaceReserved['{'] := '\{';
 fReplaceReserved['}'] := '\}';
end;

destructor TRtfSynExport.Destroy;
begin
 fListColors.Free;
 fListColors := Nil;

 Inherited Destroy;
end;

procedure TRtfSynExport.Clear;
begin
 Inherited Clear;

 If Assigned(fListColors) Then
  fListColors.Clear;
end;

end.


