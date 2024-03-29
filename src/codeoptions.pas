unit codeoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Buttons, codelib;

type

  { TFrmOptions }

  TFrmOptions = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    savesizeChb: TCheckBox;
    bgcolorBtn: TColorButton;
    DirectoryEdit: TDirectoryEdit;
    bgcolorLbl: TLabel;
    optLbl: TLabel;
    procedure BitBtn2Click(Sender: TObject);
    procedure DirectoryEditAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FrmOptions: TFrmOptions;

implementation

uses codelibconst;
{$R *.lfm}

{ TFrmOptions }

procedure TFrmOptions.DirectoryEditAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  DirectoryEdit.Directory:=Value;
end;

procedure TFrmOptions.BitBtn2Click(Sender: TObject);
begin
  codelib.CodeFrm.DatabasePath := DirectoryEdit.Directory;
  CodeFrm.BackgroundColor := bgcolorBtn.ButtonColor;
  CodeFrm.SaveSizePosition := savesizeChb.Checked;
  Close;
end;

procedure TFrmOptions.FormCreate(Sender: TObject);
begin
  caption:=rs_optcaption;
  optlbl.Caption:=rs_optlbl;
  DirectoryEdit.Directory:=CodeFrm.DatabasePath;
  bgcolorLbl.Caption := rs_optbackgoundcolor;
  savesizeChb.Caption := rs_savesizeposition;
  bgcolorBtn.ButtonColor := CodeFrm.BackgroundColor;
  savesizeChb.Checked := CodeFrm.SaveSizePosition;
end;

end.

