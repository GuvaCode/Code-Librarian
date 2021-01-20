unit codesrch;

{$mode objfpc}{$H+}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfmCodeSearch = class(TForm)
    lblFind: TLabel;
    edSearch: TEdit;
    gbxOptions: TGroupBox;
    cbCaseSensitive: TCheckBox;
    cbWholeWord: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.lfm}

end.
