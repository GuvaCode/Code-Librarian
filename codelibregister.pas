unit CodeLibRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  LCLType,  CodeLib, LCLTranslator,
  MenuIntf, IDECommands, ToolBarIntf;

 procedure Register;

implementation
uses CodeLibConst;

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
  SectionEditorMnu: TIDEMenuSection;
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

  SectionEditorMnu:=RegisterIDEMenuSection(SrcEditMenuSectionFirstStatic,'CodeLib');
   RegisterIDEMenuCommand(SectionEditorMnu, 'CodeLib', rsMenuName+' ...',
    nil, @ShowCodeLib, IDECommand, 'ce_interface');
end;

end.

