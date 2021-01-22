unit codelib;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable}

interface

uses
   Clipbrd, Forms, Db, BufDataset, ImgList, Controls, StdActns, Classes,
   LazFileUtils, SysUtils, LCLType,
   ActnList, Dialogs, Menus, ComCtrls, ExtCtrls, SynHighlighterPas,
   SynEdit, SynHighlighterCpp, SynHighlighterHTML, SynHighlighterSQL;

type
  TSearchRecord = record
    Text: string;
    CaseSensitive: Boolean;
    WholeWord: Boolean;
  end;

  { TCodeFrm }
 type
  TCodeFrm = class(TForm)
    BufDataset1: TBufDataset;
    DataSource1: TDataSource;
    StatusBar: TStatusBar;
    Splitter: TSplitter;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileNew: TMenuItem;
    mitFilePrinterSetup: TMenuItem;
    mitFilePrint: TMenuItem;
    mitFileSep3: TMenuItem;
    mitFileExit: TMenuItem;
    mitEdit: TMenuItem;
    mitEditPaste: TMenuItem;
    mitEditCopy: TMenuItem;
    mitEditCut: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpContents: TMenuItem;
    mitHelpSep1: TMenuItem;
    mitHelpAbout: TMenuItem;
    mitFileDelete: TMenuItem;

    pnlView: TPanel;
    pmTopics: TPopupMenu;
    mitTreeNew: TMenuItem;
    mitTreeDelete: TMenuItem;
    pmCode: TPopupMenu;
    mitEditorCut: TMenuItem;
    mitEditorCopy: TMenuItem;
    mitEditorPaste: TMenuItem;
    mitEditorSep2: TMenuItem;
    mitEditorHighlighting: TMenuItem;
    mitPascal: TMenuItem;
    mitCPP: TMenuItem;
    mitEditSep1: TMenuItem;
    mitEditCopyFromIde: TMenuItem;
    mitEditPasteFromIde: TMenuItem;
    mitEditorSep1: TMenuItem;
    mitEditorCopyFromDelphi: TMenuItem;
    mitEditorPasteIntoDelphi: TMenuItem;
    mitEditSep2: TMenuItem;
    mitEditFind: TMenuItem;
    mitEditFindNext: TMenuItem;
    mitFileSep1: TMenuItem;
    mitNone: TMenuItem;
    mitFileNewRootFolder: TMenuItem;
    mitFileNewFolder: TMenuItem;
    mitFileNewSnippet: TMenuItem;
    mitTreeNewRootFolder: TMenuItem;
    mitTreeNewFolder: TMenuItem;
    mitTreeNewSnippet: TMenuItem;
    mitOptions: TMenuItem;
    mitOptionsOptions: TMenuItem;
    mitTreeMakeRoot: TMenuItem;
    mitEditSep3: TMenuItem;
    mitEditExpandAll: TMenuItem;
    mitEditContractAll: TMenuItem;
    mitHTML: TMenuItem;
    mitSQL: TMenuItem;
    SynCpp: TSynCppSyn;
    SynHTML: TSynHTMLSyn;
    SynPas: TSynPasSyn;
    SynSQL: TSynSQLSyn;
    tvTopics: TTreeView;
    mActions: TActionList;
    actDelete: TAction;
    actNewRootFolder: TAction;
    actNewFolder: TAction;
    actNewSnippet: TAction;
    actMakeRoot: TAction;
    actPrinterSetup: TAction;
    actPrint: TAction;
    actExit: TAction;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditCopyFromIde: TAction;
    actEditPasteToIde: TAction;
    actEditFind: TAction;
    actEditFindNext: TAction;
    actExpandAll: TAction;
    actContractAll: TAction;
    ilActions: TImageList;
    Toolbar: TToolBar;
    tbnNewFolder: TToolButton;
    tbnNewSnippet: TToolButton;
    tbnDelete: TToolButton;
    tbnCut: TToolButton;
    tbnSep1: TToolButton;
    tbnCopy: TToolButton;
    tbnPaste: TToolButton;
    tbnSep2: TToolButton;
    tbnCopyIde: TToolButton;
    tbnPasteIde: TToolButton;
    tbnSep3: TToolButton;
    tbnExpandAll: TToolButton;
    tbnContractAll: TToolButton;
    tbnSep4: TToolButton;
    tbnFind: TToolButton;
    actOptions: TAction;
    actHelpAbout: TAction;
    actHelpContents: TAction;
    actHelpHelp: TAction;
    tbnFindNext: TToolButton;
    actSyntaxNone: TAction;
    actSyntaxPascal: TAction;
    actSyntaxCpp: TAction;
    actSyntaxHtml: TAction;
    actSyntaxSql: TAction;
    actEditRename: TAction;
    mitTreeRename: TMenuItem;

    procedure CodeTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure pmCodePopup(Sender: TObject);
    procedure tvTopicsChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure tvTopicsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvTopicsChange(Sender: TObject; Node: TTreeNode);
    procedure DeleteExecute(Sender: TObject);
    procedure PrinterSetupExecute(Sender: TObject);
    procedure PrintExecute(Sender: TObject);
    procedure ExitExecute(Sender: TObject);
    procedure CutExecute(Sender: TObject);
    procedure CopyExecute(Sender: TObject);
    procedure PasteExecute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure CopyFromIdeExecute(Sender: TObject);
    procedure PasteToIdeExecute(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure FindNextExecute(Sender: TObject);
    procedure tvTopicsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvTopicsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ExpandAllExecute(Sender: TObject);
    procedure ContractAllExecute(Sender: TObject);
    procedure HelpExecute(Sender: TObject);
    procedure HelpContentsExecute(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure NewSnippetExecute(Sender: TObject);
    procedure NewRootFolderExecute(Sender: TObject);
    procedure NewFolderExecute(Sender: TObject);
    procedure tvTopicsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OptionsExecute(Sender: TObject);
    procedure tvTopicsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tvTopicsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure MakeRootExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GenericSyntaxHighlightingExecute(Sender: TObject);
    procedure tvTopicsDblClick(Sender: TObject);
    procedure actEditRenameExecute(Sender: TObject);
    procedure tvTopicsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FModified: Boolean;
    FSearch: TSearchRecord;
    FDatabasePath: string;
    FCodeText: TSynEdit;
   // FCurrentSyntaxMode: TSynCustomHighliter;
    CodeDB: TDataSet;
    function CreateNewDB(const DatabaseFile: string): TBufDataset;
    function OpenDB(const DatabaseFile: string): TBufDataset;
    procedure CloseDB(ClearFileName: Boolean = False);
    procedure InitializeTreeView;
    procedure SaveRecord;
    procedure DoSearch(First: Boolean);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure SetModified(New: Boolean);

    procedure SortNodes;
    procedure AddDefaultIndexes(DataSet: TBufDataset);
    procedure SetupSyntaxHighlightingControl;
  public
    //constructor Create(AOwner: TComponent); override;
   // destructor Destroy; override;
    function AddFolder(Node: TTreeNode; const Desc: string): TTreeNode;
    function AddCode(const Desc: string): TTreeNode;
    property Modified: Boolean read FModified write SetModified;
    property DatabasePath: string read FDatabasePath write FDatabasePath;
  end;

resourcestring
  SMenuName = 'Code Librarian';
  SModified = 'Modified';
  SSnippet = 'snippet';
  SFolder = 'folder';
  SConfirmDelete = 'Delete this %s?';
  SNotForFormFiles = 'Copy/Paste is not allowed in form files.';
  SCannotAttach = 'Subitems cannot be attached to a code snippet, only folders.';
  SNewCode = 'New Code';
  SCouldNotCreateDatabase = 'Could not create database.';

  const
  ConfigurationSection = 'CodeLib';
  ClosedFolderImageIndex = 0;
  OpenFolderImageIndex   = 17;
  CodeSnippetImageIndex = 18;
  DefaultDBFileName = 'CodeDB.db';

  var
   // CodeFrm: TCodeFrm;
    CodeFrm: TCodeFrm = nil;

implementation
{$R *.lfm}

uses
   LazIDEIntf, SrcEditorIntf;

function TCodeFrm.CreateNewDB(const DatabaseFile: string): TBufDataset;
begin
 {$Warnings Off} Result := TBufDataset.Create(self) {$Warnings On};
  try
    with Result do
    begin
     Filename := DatabaseFile;
     FieldDefs.Add('Key', ftAutoInc, 0, False);
     FieldDefs.Add('Type', ftString, 1, True);
     FieldDefs.Add('Parent', ftInteger, 0, False);
     FieldDefs.Add('System', ftInteger, 0, False);
     FieldDefs.Add('Topic', ftString, 250, False);
     FieldDefs.Add('Code', ftMemo, 0, False);
     FieldDefs.Add('Language', ftString, 4, False);
     IndexDefs.Add('Main', 'Key', [ixPrimary]);
     IndexDefs.Add('Parent', 'Parent', []);
     IndexDefs.Add('System', 'System', []);
     CreateDataset;
     IndexName := 'Parent';
     SaveToFile;
     Open;
     IndexFieldNames := 'Parent';
    end;
     except
    on E: Exception do
    begin
      Result.Free;
      Result := nil; // We swallow the exception, hence need a defined function result
     ShowMessage(E.Message);
    end;
  end;
end;

function TCodeFrm.OpenDB(const DatabaseFile: string): TBufDataset;
begin
  Result := nil;
  if not FileExists(DatabaseFile) then Exit;
  {$Warnings Off}
  Result := TBufDataset.Create(Self);
  {$Warnings On}
  with Result do
  begin
    Filename:=DatabaseFile;
    try
      Open;
      IndexFieldNames := 'Parent';
    except
      on E: Exception do
      begin
        Close;
        Result.Free;
        Result := nil;
      end;
    end;
  end;
end;

procedure TCodeFrm.AddDefaultIndexes(DataSet: TBufDataset);
begin
  with DataSet as TBufDataset do
  begin
    IndexDefs.Add('Main', 'Key', [ixPrimary]);
    IndexDefs.Add('Parent', 'Parent', []);
    IndexName := 'Parent';
    IndexFieldNames := 'Parent';
  end;

end;

procedure TCodeFrm.InitializeTreeView;

  procedure LoadTreeView(Node: TTreeNode; Parent: Integer);
  var
    BookMark: TBookMark;
    RNode: TTreeNode;
  begin
    BookMark := CodeDB.GetBookMark;
    try
      if CodeDB.Locate('Parent', Parent, []) then // Do not localize.
        while (not CodeDB.Eof) and (CodeDB.FieldByName('Parent').AsInteger = Parent) do // Do not localize.
        begin
          RNode := tvTopics.Items.AddChildObject(Node, CodeDB.FieldByName('Topic').AsString, // Do not localize.
            Pointer(PtrInt(CodeDB.FieldByName('Key').AsInteger))); // Do not localize.
          if CodeDB.FieldByName('Type').AsString = 'F' then // Do not localize.
          begin
            RNode.ImageIndex := ClosedFolderImageIndex;
            RNode.SelectedIndex := OpenFolderImageIndex;
            LoadTreeView(RNode, CodeDB.FieldByName('Key').AsInteger); // Do not localize.
          end
          else
          begin
            RNode.ImageIndex := CodeSnippetImageIndex;
            RNode.SelectedIndex := CodeSnippetImageIndex;
          end;
          CodeDB.Next;
        end;
    finally
      CodeDB.GotoBookMark(BookMark);
      CodeDB.FreeBookMark(BookMark);
    end;
  end;

var
  Node: TTreeNode;
begin
  tvTopics.SortType := stNone;
  tvTopics.Items.BeginUpdate;
  try
    CodeDB.First;
    while (not CodeDB.Eof) and (CodeDB.FieldByName('Parent').AsInteger = 0) do // Do not localize.
    begin
      Node := tvTopics.Items.AddObject(nil, CodeDB.FieldByName('Topic').AsString, // Do not localize.
        Pointer(PtrInt(CodeDB.FieldByName('Key').AsInteger))); // Do not localize.
      Node.ImageIndex := ClosedFolderImageIndex;
      Node.SelectedIndex := OpenFolderImageIndex;
      LoadTreeView(Node, CodeDB.FieldByName('Key').AsInteger); // Do not localize.
      CodeDB.Next;
    end;
  finally
    tvTopics.SortType := stText;
    //tvTopics.SortType := stNone;
    tvTopics.Items.EndUpdate;
  end;

end;

function TCodeFrm.AddFolder(Node: TTreeNode; const Desc: string): TTreeNode;
var
  NNode: TTreeNode;
begin
  Result := nil;
  with CodeDB do
  begin
    Insert;
    if Node <> nil then
      FieldByName('Parent').AsInteger := PtrInt(Node.Data)  // Do not localize.
    else
      FieldByName('Parent').AsInteger := 0;  // Do not localize.
    FieldByName('Topic').AsString := Desc; // Do not localize.
    FieldByname('Type').AsString := 'F';  // Do not localize.
    Post;

  end;
  NNode := tvTopics.Items.AddChildObject(Node, Desc,
    Pointer(PtrInt(CodeDB.FieldByName('Key').AsInteger))); // Do not localize.
  NNode.ImageIndex := ClosedFolderImageIndex;
  NNode.SelectedIndex := OpenFolderImageIndex;
  Result := NNode;

  SortNodes;
end;

procedure TCodeFrm.SetModified(New: Boolean);

begin
  FModified := New;
  if FModified then
    StatusBar.Panels[1].Text := SModified
  else
    StatusBar.Panels[1].Text := ''; // No need to localize.
end;

function TCodeFrm.AddCode(const Desc: string): TTreeNode;
var
  Node: TTreeNode;
begin
  Result := nil;

  if tvTopics.Selected = nil then Exit;
  with CodeDB do
  begin
    Insert;

    FieldByName('Parent').AsInteger := PtrInt(tvTopics.Selected.Data);  // Do not localize.
    FieldByName('Topic').AsString := Desc;  // Do not localize.
    FieldByName('Type').AsString := 'C';  // Do not localize.
    if mitPascal.Checked then
      FieldByName('Language').AsString := 'P'  // Do not localize.
    else
      FieldByName('Language').AsString := 'C';  // Do not localize.
    Post;

  end;
  Node := tvTopics.Items.AddChildObject(tvTopics.Selected, Desc,
    Pointer(PtrInt(CodeDB.FieldByName('Key').AsInteger)));  // Do not localize.
  Node.ImageIndex := CodeSnippetImageIndex;
  Node.SelectedIndex := CodeSnippetImageIndex;
  Result := Node;
  SortNodes;
end;

procedure TCodeFrm.CodeTextChange(Sender: TObject);
begin
  Modified := FCodeText.Modified;
end;

procedure TCodeFrm.FormCreate(Sender: TObject);
var dbFPath:string;
begin
  SetupSyntaxHighlightingControl;
{  Screen.Cursor := crHourglass;
  try
    CodeDB := nil;
    dbFPath:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+DefaultDBFileName;
    CodeDB := OpenDB(DefaultDBFileName); // do not localize
    if CodeDB = nil then CodeDB := createNewDb(dbFPath);
    if CodeDB = nil then
    begin
      MessageDlg(SCouldNotCreateDatabase, mtError, [mbOK], 0);
      Exit;
    end;
    InitializeTreeView;
    mitPascal.Checked := True;
    FModified := False;
  finally
    Screen.Cursor := crDefault;
  end;}
caption:=SMenuName;
 dbFPath:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+'CodeLibrarian.dat';
 CodeDB := OpenDB(dbFPath); // do not localize
 if CodeDB = nil then CodeDB := createNewDb(dbFPath);
 InitializeTreeView;
end;

procedure TCodeFrm.mActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
var
  HaveEditorSelection: Boolean;
  HaveSelectedNode: Boolean;
  SnippetIsSelected: Boolean;
  Editor: TSourceEditorInterface;
begin
  HaveEditorSelection := Length(FCodeText.SelText) > 0;
  actEditCut.Enabled := HaveEditorSelection;
  actEditCopy.Enabled := HaveEditorSelection;
  // bug on linux on menu
  // actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));
  HaveSelectedNode  := tvTopics.Selected <> nil;
  SnippetIsSelected := HaveSelectedNode and (tvTopics.Selected.ImageIndex = CodeSnippetImageIndex);

  Editor:=SourceEditorManagerIntf.ActiveEditor;
  if Editor <> nil then
  begin
   actEditCopyFromIde.Enabled := (SnippetIsSelected and (Editor.SelectionAvailable));
    actEditPasteToIde.Enabled := (SnippetIsSelected and (FCodeText.SelAvail))
  end;

  actDelete.Enabled := HaveSelectedNode;
  actEditRename.Enabled := HaveSelectedNode;

  if not HaveSelectedNode then
  begin
    actMakeRoot.Enabled := False;
    actNewSnippet.Enabled := False;
  end
  else
  begin
    actMakeRoot.Enabled := (not (tvTopics.Selected.Level = 0)) and (not SnippetIsSelected);
    actNewSnippet.Enabled := not SnippetIsSelected;
    actNewFolder.Enabled := not SnippetIsSelected;
  end;

  Handled := True;
end;

procedure TCodeFrm.pmCodePopup(Sender: TObject);
begin
  actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));
end;

procedure TCodeFrm.tvTopicsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if (tvTopics.Selected <> nil) and Modified then
    SaveRecord;
  // Do not alter value of AllowChange.
end;

procedure TCodeFrm.SaveRecord;
begin
  // This is called from the destructor where
  // we may be in a forced clean-up state due
  // to an exception in the constructor.
  if ExceptObject <> nil then
    Exit;

  // Do not localize any of the following lines.
  Modified := False;
  if not CodeDB.Active then Exit;
  if tvTopics.Selected = nil then Exit;
  if CodeDB.Locate('Key', PtrInt(tvTopics.Selected.Data), []) then
  begin
    CodeDB.Edit;
    CodeDB.FieldByName('Topic').AsString := tvTopics.Selected.Text;

    if tvTopics.Selected.ImageIndex = CodeSnippetImageIndex then
    begin
      CodeDB.FieldByName('Code').AsString:=FCodeText.Text;

      if mitPascal.Checked then
        CodeDB.FieldByName('Language').AsString := 'P'
      else
      if mitCPP.Checked then
        CodeDB.FieldByName('Language').AsString := 'C'
      else
      if mitHTML.Checked then
        CodeDB.FieldByName('Language').AsString := 'H'
      else
      if mitSQL.Checked then
        CodeDB.FieldByName('Language').AsString := 'S'
      else
        CodeDB.FieldByName('Language').AsString := 'N'
    end;
    CodeDB.Post;

  end;
end;

procedure TCodeFrm.tvTopicsEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  Modified := True;
  SortNodes;
end;

procedure TCodeFrm.tvTopicsChange(Sender: TObject; Node: TTreeNode);
begin
  try
    if (Node <> nil) and (Node.ImageIndex = CodeSnippetImageIndex) then
    begin
      if CodeDB.Locate('Key', PtrInt(Node.Data), []) then  // Do not localize.
      begin
        FCodeText.Lines.BeginUpdate;
        try
          if CodeDB.FieldByName('Language').AsString = 'N' then  // Do not localize.
          begin
            // This is raw text
            mitNone.Checked := True;
            FCodeText.HighLighter := nil;
          end
          else
          if CodeDB.FieldByName('Language').AsString = 'C' then  // Do not localize.
          begin
            // This is CPP source code
            mitCPP.Checked := True;
            FCodeText.HighLighter := SynCPP;
          end
          else
          if CodeDB.FieldByName('Language').AsString = 'H' then  // Do not localize.
          begin
            // This is HTML source code
            mitHTML.Checked := True;
            FCodeText.HighLighter := SynHTML;
          end
          else
          if CodeDB.FieldByName('Language').AsString = 'S' then  // Do not localize.
          begin
            // This is SQL code
            mitSQL.Checked := True;
            FCodeText.HighLighter := SynSQL;
          end
          else
          begin
            // This is Object Pascal source code.
            mitPascal.Checked := True;
            FCodeText.HighLighter := SynPAS;
          end;
          FCodeText.Text:=CodeDB.FieldByName('Code').AsString;
        finally
          FCodeText.Lines.EndUpdate;
        end;
      end;
      FCodeText.ReadOnly := False;
    end
    else
    begin
      FCodeText.Lines.Clear;
      FCodeText.ReadOnly := True;
    end;
    Modified := False;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));
end;

procedure TCodeFrm.DeleteExecute(Sender: TObject);

  procedure DeleteRecords(Node: TTreeNode);
  var
    RNode: TTreeNode;
  begin
    RNode := Node.GetNext;
    while (RNode <> nil) and (RNode.Level > Node.Level) do
    begin
      if CodeDB.Locate('Key', PtrInt(RNode.Data), []) then  // Do not localize.
        CodeDB.Delete;
      RNode := RNode.GetNext;
    end;
  end;


var
  NodeType: string;
begin
  try
    if tvTopics.Selected = nil then Exit;
    if CodeDB.Locate('Key', PtrInt(tvTopics.Selected.Data), []) then  // Do not localize.
    begin
      if tvTopics.Selected.ImageIndex = CodeSnippetImageIndex then
        NodeType := SSnippet
      else
        NodeType := SFolder;
      if MessageDlg(Format(SConfirmDelete, [NodeType]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        CodeDB.Delete;
        DeleteRecords(tvTopics.Selected);
        tvTopics.Selected.Delete;
      end;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TCodeFrm.PrinterSetupExecute(Sender: TObject);
begin
 // dlgPrinterSetup.Execute;
end;

procedure TCodeFrm.PrintExecute(Sender: TObject);
//resourcestring
//  RS_PRINTTITLE = 'GExperts';
begin
 { if tvTopics.Selected <> nil then
    FCodeText.Print(tvTopics.Selected.Text)
  else
    FCodeText.Print(RS_PRINTTITLE); }

end;

procedure TCodeFrm.ExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TCodeFrm.CutExecute(Sender: TObject);
begin
  FCodeText.CutToClipboard;
  actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));
end;

procedure TCodeFrm.CopyExecute(Sender: TObject);
begin
  FCodeText.CopyToClipboard;
  actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));
end;

procedure TCodeFrm.PasteExecute(Sender: TObject);
begin
  if not FCodeText.ReadOnly then
    FCodeText.PasteFromClipBoard
end;

procedure TCodeFrm.HelpAboutExecute(Sender: TObject);
begin
 // ShowGXAboutForm;
end;

procedure TCodeFrm.CopyFromIdeExecute(Sender: TObject);
var
    Editor: TSourceEditorInterface;
begin
   Editor:=SourceEditorManagerIntf.ActiveEditor;
   if Editor=nil then exit;
   FCodeText.InsertTextAtCaret(Editor.GetText(true));
end;

procedure TCodeFrm.PasteToIdeExecute(Sender: TObject);
var
    Editor: TSourceEditorInterface;
begin
  Editor:=SourceEditorManagerIntf.ActiveEditor;
   if Editor=nil then exit;
    //editor.CurrentLineText:=FCodeText.SelText;
     //editor.CursorTextXY;
    editor.InsertLine(editor.CursorTextXY.Y,FCodeText.SelText,true);
    //  EDitor.InsertLine(Editor.LinesInWindow,'test');
end;

procedure TCodeFrm.FindExecute(Sender: TObject);
begin
  //try
    {with TfmCodeSearch.Create(nil) do
    try
      if ShowModal = mrOK then
      begin
        FSearch.Text := edSearch.Text;
        FSearch.CaseSensitive := cbCaseSensitive.Checked;
        FSearch.WholeWord := cbWholeWord.Checked;
        DoSearch(True);
      end;
    finally
      Free;
    end;
  except
    on E: Exception do
      showmessage(E.Message);
  end;  }
end;

procedure TCodeFrm.DoSearch(First: Boolean);
  function CaseInsensitivePos(const SubString, S: string): Integer;
begin
 Result := AnsiPos(AnsiUpperCase(SubString), AnsiUpperCase(S));
end;
  function CharIsAlpha(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['a'..'z','A'..'Z']);
end;
  function DoMatch(const Text: string): Integer;
  var
    MatchPos: Integer;
  begin
    Result := -1;

    if FSearch.CaseSensitive then
      MatchPos := AnsiPos(FSearch.Text, Text)
    else
      MatchPos := CaseInsensitivePos(FSearch.Text, Text);

    if (MatchPos > 0) and FSearch.WholeWord then
    begin
      // If the previous character is alphabetic, there isn't a match
      if MatchPos > 1 then
        if CharIsAlpha(Text[MatchPos - 1]) then
          Exit;
      // If the next character is alphabetic, we didn't find a word match
      if MatchPos + Length(FSearch.Text) <= Length(Text) then
        if CharIsAlpha(Text[MatchPos + Length(FSearch.Text)]) then
          Exit;
    end;
    Result := MatchPos;
  end;

var
  Node: TTreeNode;
  Match: Integer;
  InTopic: Boolean;
  FirstLoop: Boolean;
begin
  try
    Screen.Cursor := crHourGlass;
    try
      Node := nil;
      if not First then
      begin
        //if ActiveControl = FCodeText then
          Node := tvTopics.Selected
        //else
        //  Node := tvTopics.Selected.GetNext;
      end;
      if First or (Node = nil) then
        Node := tvTopics.Items.GetFirstNode;
      Match := 0;
      InTopic := False;
      FirstLoop := True;
      while Node <> nil do
      begin
        if CodeDB.Locate('Key', PtrInt(Node.Data), []) then
        begin
          if FirstLoop and (FCodeText.Focused) and (Length(FCodeText.SelText) > 0) then
          begin
            InTopic := False;
            Match := DoMatch(Copy(CodeDB.FieldByName('Code').AsString, FCodeText.SelStart + Length(FCodeText.SelText) , 999999));
            if Match > 0 then
            begin
             Match := Match + FCodeText.SelStart + Length(FCodeText.SelText) - 1;
           end;
          end
          else
          begin
            if not FirstLoop then
            begin
              InTopic := True;
              Match := DoMatch(CodeDB.FieldByName('Topic').AsString);
            end;
            if Match = 0 then
            begin
              Match := DoMatch(CodeDB.FieldByName('Code').AsString);
              InTopic := False;
           end;
          end;
          if Match > 0 then Break;
        end;
        Node := Node.GetNext;
        FirstLoop := False;
      end;
      if Node = nil then
        SysUtils.Beep;
      if Match > 0 then
      begin
        tvTopics.Selected := Node;
        if InTopic then
          tvTopics.SetFocus
        else
        begin
          FCodeText.Enabled:=true;
          FCodeText.SetFocus;
          Dec(Match);
           FCodeText.SelStart := Match + 1;
          FCodeText.SelEnd := Match + Length(FSearch.Text) + 1;
        end;
      end;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCodeFrm.FindNextExecute(Sender: TObject);
begin
  if FSearch.Text <> '' then
    DoSearch(False)
  else
    actEditFind.Execute;
end;

procedure TCodeFrm.tvTopicsDragDrop(Sender, Source: TObject; X, Y: Integer);

var
  Node: TTreeNode;

begin
  try
    if tvTopics.Selected = nil then
      Exit;
    Node := tvTopics.GetNodeAt(X, Y);
    if Node = nil then
      Exit;
    if Node.ImageIndex = CodeSnippetImageIndex then
    begin
      MessageDlg(SCannotAttach, mtError, [mbOK], 0);
      Exit;
    end;
    if CodeDB.Locate('Key', PtrInt(tvTopics.Selected.Data), []) then  // Do not localize.
    begin
      CodeDB.Edit;
      CodeDB.FieldByName('Parent').AsInteger := PtrInt(Node.Data);  // Do not localize.
      CodeDB.Post;

      tvTopics.Selected.MoveTo(Node, naAddChild);
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TCodeFrm.tvTopicsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Sender);
end;

procedure TCodeFrm.ExpandAllExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := tvTopics.Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Expand(True);
    Node := Node.GetNextSibling;
  end;
end;

procedure TCodeFrm.ContractAllExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  // OnChanging doesn't fire under Delphi 5 when calling Collapse below
  if (tvTopics.Selected <> nil) and Modified then
    SaveRecord;

  Node := tvTopics.Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Collapse(True);
    Node := Node.GetNextSibling;
  end;

  // OnChange doesn't fire under Delphi 5 when calling Collapse above
  tvTopicsChange(tvTopics, tvTopics.Selected);
end;

procedure TCodeFrm.SaveSettings;

begin
  // Do not localize any of the following lines.
 { BaseKey := ConfigInfo.GExpertsIdeRootRegistryKey;

  RegIni := TGExpertsSettings.Create(BaseKey);
  try
    RegIni.WriteInteger(ConfigurationSection, 'Left', Left);
    RegIni.WriteInteger(ConfigurationSection, 'Top', Top);
    RegIni.WriteInteger(ConfigurationSection, 'Width', Width);
    RegIni.WriteInteger(ConfigurationSection, 'Height', Height);
    RegIni.WriteInteger(ConfigurationSection, 'Layout', Ord(Layout));
    if Layout = clSide then
      RegIni.WriteInteger(ConfigurationSection, 'Splitter', tvTopics.Width)
    else
      RegIni.WriteInteger(ConfigurationSection, 'Splitter', tvTopics.Height);
    RegIni.WriteString(ConfigurationSection, 'DatabasePath', DatabasePath);
  finally
    RegIni.Free;
  end;

  BaseKey := AddTrailingPathDelimiter(BaseKey) + ConfigurationSection;
  RegIni := TGExpertsSettings.Create(BaseKey);
  try
    RegSaveFont(RegIni, 'Editor', FCodeText.Font);
    RegSaveFont(RegIni, 'TreeView', tvTopics.Font);
  finally
    RegIni.Free;
  end; }
end;

procedure TCodeFrm.LoadSettings;
//var
 // RegIni: TGExpertsSettings;
 // BaseKey: string;
begin
  // Do not localize any of the following lines.
  // Do not localize any of the following lines.
  {BaseKey := ConfigInfo.GExpertsIdeRootRegistryKey;

  RegIni := TGExpertsSettings.Create(BaseKey);
  try
    Left := RegIni.ReadInteger(ConfigurationSection, 'Left', Left);
    Top := RegIni.ReadInteger(ConfigurationSection, 'Top', Top);
    Width := RegIni.ReadInteger(ConfigurationSection, 'Width', Width);
    Height := RegIni.ReadInteger(ConfigurationSection, 'Height', Height);
    Layout := TCodeLayout(RegIni.ReadInteger(ConfigurationSection, 'Layout', 0));
    if Layout = clSide then
      tvTopics.Width := RegIni.ReadInteger(ConfigurationSection, 'Splitter', tvTopics.Width)
    else
      tvTopics.Height := RegIni.ReadInteger(ConfigurationSection, 'Splitter', tvTopics.Height);
    DatabasePath := RegIni.ReadString(ConfigurationSection, 'DatabasePath', DatabasePath);
  finally
    RegIni.Free;
  end;

  BaseKey := AddTrailingPathDelimiter(BaseKey) + ConfigurationSection;
  RegIni := TGExpertsSettings.Create(BaseKey);
  try
    RegLoadFont(RegIni, 'Editor', FCodeText.Font);
    RegLoadFont(RegIni, 'TreeView', tvTopics.Font);
  finally
    RegIni.Free;
  end;  }
end;

procedure TCodeFrm.HelpExecute(Sender: TObject);
begin

 // GxContextHelp(Self, 17);

end;

procedure TCodeFrm.HelpContentsExecute(Sender: TObject);
begin

 // GxContextHelpContents(Self);

end;

procedure TCodeFrm.StatusBarResize(Sender: TObject);
begin
  with StatusBar do
    Panels[0].Width := Width - Panels[1].Width;
end;

procedure TCodeFrm.NewSnippetExecute(Sender: TObject);

var
  Node: TTreeNode;
begin
  Screen.Cursor := crHourglass;
  try
    Node := AddCode(SNewCode);
    if Node <> nil then
    begin
      tvTopics.Selected := Node;
      tvTopics.Selected.EditText;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

resourcestring
  SNewFolder = 'New Folder';

procedure TCodeFrm.NewRootFolderExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := AddFolder(nil, SNewFolder);
  if Node <> nil then
  begin
    tvTopics.Selected := Node;
    tvTopics.Selected.EditText;
  end;
end;

procedure TCodeFrm.NewFolderExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  Screen.Cursor := crHourglass;
  try
    Node := AddFolder(tvTopics.Selected, SNewFolder);
    if Node <> nil then
    begin
      tvTopics.Selected := Node;
      tvTopics.Selected.EditText;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCodeFrm.tvTopicsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F2) and (tvTopics.Selected <> nil) then
    tvTopics.Selected.EditText;
end;

procedure TCodeFrm.OptionsExecute(Sender: TObject);
//var
 // Dlg: TfmCodeOptions;
begin
 { Dlg := TfmCodeOptions.Create(nil);
  try
    with Dlg do
    begin
      edPath.Text := DatabasePath;
      if Layout = clSide then
        rbSide.Checked := True
      else
        rbTop.Checked := True;
      fcTreeView.ItemIndex := fcTreeView.Items.IndexOf(tvTopics.Font.Name);
      udTreeView.Position := tvTopics.Font.Size;
      fcEditor.ItemIndex := fcEditor.Items.IndexOf(FCodeText.Font.Name);
      udEditor.Position := FCodeText.Font.Size;
    end;

    if Dlg.ShowModal = mrOK then
    begin
      if (DatabasePath <> Dlg.edPath.Text) then
      begin
        if CodeDB <> nil then
          CloseDB(True);

        FreeAndNil(CodeDB);
        tvTopics.Items.Clear;
        DatabasePath := AddTrailingPathDelimiter(Dlg.edPath.Text);
        CodeDB := OpenDB(DatabasePath + DefaultDBFileName);
        if CodeDB = nil then
          CodeDB := CreateNewDB(DatabasePath + DefaultDBFileName);
        if CodeDB = nil then
        begin
          MessageDlg(SCouldNotCreateDatabase, mtError, [mbOK], 0);
          Exit;
        end;
        InitializeTreeView;
      end;
      if Dlg.rbSide.Checked then
        Layout := clSide
      else
        Layout := clTop;

      with tvTopics.Font do
      begin
        Name := Dlg.fcTreeView.Text;
        Size := Trunc(StrToInt(Dlg.eTreeView.Text));
      end;
      with FCodeText.Font do
      begin
        Name := Dlg.fcEditor.Text;
        Size := Trunc(StrToInt(Dlg.eEditor.Text));
      end;
    end;
  finally
    Dlg.Free;
  end;  }
end;

procedure TCodeFrm.tvTopicsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  AutoScroll := True;
end;

procedure TCodeFrm.tvTopicsEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  AutoScroll := False;
end;

procedure TCodeFrm.MakeRootExecute(Sender: TObject);
begin
  with tvTopics do
  if ((Selected <> nil) and (Selected.Level > 0) and (not (Selected.ImageIndex = CodeSnippetImageIndex))) then
  begin
    CodeDB.Edit;
    CodeDB.FieldByName('Parent').AsInteger := 0;  // Do not localize.
    tvTopics.Selected.MoveTo(nil, naAdd);
    Modified := True;
    SortNodes;
  end;
end;

procedure TCodeFrm.FormHide(Sender: TObject);
begin
  if FModified then SaveRecord;
   CloseDB;
end;

procedure TCodeFrm.FormShow(Sender: TObject);
begin
  if (CodeDB <> nil) and not CodeDB.Active then CodeDB.Open;
end;

procedure TCodeFrm.SortNodes;
begin
  tvTopics.AlphaSort;
  //tvTopics.SortType := stNone;
end;

procedure TCodeFrm.GenericSyntaxHighlightingExecute(Sender: TObject);
begin
  Modified := True;

  if Sender = actSyntaxNone then
    FCodeText.HighLighter :=  nil
  else
  if Sender = actSyntaxPascal then
    FCodeText.HighLighter :=  SynPAS
  else
  if Sender = actSyntaxCpp then
    FCodeText.HighLighter := SynCPP
  else
  if Sender = actSyntaxHtml then
    FCodeText.HighLighter := SynHTML
  else
  if Sender = actSyntaxSql then
    FCodeText.HighLighter := SynSQL
  else
    raise Exception.Create('Internal error selecting language');
end;

procedure TCodeFrm.SetupSyntaxHighlightingControl;
begin
  FCodeText := TSynEdit.Create(Self);
  FCodeText.HighLighter := SynPas;
  FCodeText.Align := alClient;
  FCodeText.PopupMenu := pmCode;
  FCodeText.OnChange := @CodeTextChange;
  FCodeText.Parent := pnlView;
  FCodeText.ReadOnly := True;
  FCodeText.Gutter.Parts[0].Visible:=false;
  FCodeText.Gutter.Parts[1].Visible:=false;
  FCodeText.BorderStyle:=bsNone;
  FCodeText.RightEdge:=-1;
  actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));
end;
{
constructor TCodeFrm.Create(AOwner: TComponent);
var dbFPath:string;
begin
 { inherited Create(AOwner);

  SetupSyntaxHighlightingControl;

  Screen.Cursor := crHourglass;
  try
    CodeDB := nil;
    dbFPath:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+DefaultDBFileName;
    CodeDB := OpenDB(DefaultDBFileName); // do not localize
    if CodeDB = nil then CodeDB := createNewDb(dbFPath);
    if CodeDB = nil then
    begin
      MessageDlg(SCouldNotCreateDatabase, mtError, [mbOK], 0);
      Exit;
    end;
    InitializeTreeView;
    mitPascal.Checked := True;
    FModified := False;
  finally
    Screen.Cursor := crDefault;
  end; }
end;

destructor TCodeFrm.Destroy;
begin
  if FModified then
    SaveRecord;
{  SaveSettings;
  CloseDB(True);
  FreeAndNil(CodeDB);

  inherited Destroy;}
end; }

procedure TCodeFrm.tvTopicsDblClick(Sender: TObject);
begin
  if tvTopics.Selected <> nil then
  begin
    if tvTopics.Selected.ImageIndex = CodeSnippetImageIndex then
      actEditPasteToIde.Execute;
  end;
end;

procedure TCodeFrm.actEditRenameExecute(Sender: TObject);
begin
  if tvTopics.Selected <> nil then
    tvTopics.Selected.EditText
end;

procedure TCodeFrm.tvTopicsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if Button = mbRight then
  begin
    Node := tvTopics.GetNodeAt(X, Y);
    if Node <> nil then
      tvTopics.Selected := Node;
  end;
end;

procedure TCodeFrm.CloseDB(ClearFileName: Boolean);
begin
  if CodeDB <> nil then
  begin
   CodeDB.Close;
  end;
end;

procedure TCodeFrm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    Key := #0;
    actExit.Execute;
  end;
end;

end.

