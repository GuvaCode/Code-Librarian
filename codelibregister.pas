unit CodeLibRegister;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, LazIDEIntf, ProjectIntf, MenuIntf, CodeLib;

 procedure Register;

implementation

procedure ShowCodeLib(Sender: TObject);
begin
   if CodeFrm=nil then CodeFrm:=TCodeFrm.Create(Application);
   CodeFrm.ShowOnTop;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(mnuTools, 'CodeLibrarianItem', SMenuName,
    nil, @ShowCodeLib, nil, 'ce_interface');
end;

end.

