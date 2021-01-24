unit CodeLibRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  LCLType,
  MenuIntf, IDECommands, ToolBarIntf, PackageLinkIntf;

 procedure Register;

implementation
uses CodeLib;

procedure ShowCodeLib(Sender: TObject);
begin
   if CodeFrm=nil then CodeFrm:=TCodeFrm.Create(nil);
   CodeFrm.ShowOnTop;
end;

procedure Register;
var
  IDEShortCutX: TIDEShortCut;
  IDECommandCategory: TIDECommandCategory;
  IDECommand: TIDECommand;
begin
  IDEShortCutX := IDEShortCut(VK_C, [ssCtrl, ssAlt], VK_UNKNOWN, []);
  IDECommandCategory := IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  if IDECommandCategory <> nil then
  begin
    IDECommand := RegisterIDECommand(IDECommandCategory, 'Code Librarian',
    rsMenuName, IDEShortCutX, nil, @ShowCodeLib);
    if IDECommand <> nil then
      RegisterIDEButtonCommand(IDECommand);
  end;
  RegisterIDEMenuCommand(itmOptionsDialogs, 'Code Librarian', rsMenuName+' ...',
    nil, @ShowCodeLib, IDECommand, 'ce_interface');
end;

end.

