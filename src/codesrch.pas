unit codesrch;

{$mode objfpc}{$H+}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type

  { TfmCodeSearch }

  TfmCodeSearch = class(TForm)
    edSearch: TEdit;
    gbxOptions: TGroupBox;
    cbCaseSensitive: TCheckBox;
    cbWholeWord: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

resourcestring
  rs_caption='Search';
  rs_gbxOptions ='Options';
  rs_cbCaseSensitive = 'Case sensitive';
  rs_cbWholeWord = 'Whole words only';
  rs_btnOk='OK';
  rs_btnCancel='Cancel';

implementation

{$R *.lfm}

{ TfmCodeSearch }

procedure TfmCodeSearch.FormCreate(Sender: TObject);
begin
  caption:=rs_Caption;
  gbxOptions.Caption:=rs_gbxOptions;
  cbCaseSensitive.Caption:=rs_cbCaseSensitive;
  cbWholeWord.Caption:=rs_cbWholeWord;
  btnOk.Caption:=rs_btnOk;
  btnCancel.Caption:=rs_btnCancel;
end;

end.
