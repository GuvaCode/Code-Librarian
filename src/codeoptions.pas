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
    Label1: TLabel;
    procedure DirectoryEditAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FrmOptions: TFrmOptions;

implementation

{$R *.lfm}

{ TFrmOptions }

procedure TFrmOptions.DirectoryEditAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  codelib.CodeFrm.DatabasePath:=value;
  DirectoryEdit.Directory:=Value;
end;

procedure TFrmOptions.FormCreate(Sender: TObject);
begin
  DirectoryEdit.Directory:=CodeFrm.DatabasePath;
end;

end.

