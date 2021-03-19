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
    DirectoryEdit: TDirectoryEdit;
    optLbl: TLabel;
    procedure BitBtn2Click(Sender: TObject);
    procedure DirectoryEditAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FrmOptions: TFrmOptions;


resourcestring
  rs_optlbl='Select data base path ..';
  rs_optcaption='Options ...';

implementation

{$R *.lfm}

{ TFrmOptions }

procedure TFrmOptions.DirectoryEditAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  DirectoryEdit.Directory:=Value;
end;

procedure TFrmOptions.BitBtn2Click(Sender: TObject);
begin
 codelib.CodeFrm.DatabasePath:=DirectoryEdit.Directory;
 close;
end;

procedure TFrmOptions.FormCreate(Sender: TObject);
begin
  caption:=rs_optcaption;
  optlbl.Caption:=rs_optlbl;
  DirectoryEdit.Directory:=CodeFrm.DatabasePath;
end;

end.

