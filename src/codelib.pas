unit codelib;

{$mode objfpc}{$H+}

{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$WARN 4046 off : Constructing a class "$1" with abstract method "$2"}
{$HINTS OFF}

interface

uses
  Clipbrd, Forms, DB, BufDataset, ImgList, Controls, StdActns, Classes,
  LazFileUtils, SysUtils, LCLType, ActnList, Dialogs, Menus, ComCtrls, ExtCtrls,
  LCLVersion, XMLPropStorage, SynEdit, SynHighlighterCpp, SynHighlighterHTML,
  SynHighlighterSQL, SynHighlighterPas, SynEditHighlighter, SynHighlighterJava,
  SynExportHTML, SynHighlighterJScript, SynHighlighterPerl, SynHighlighterPHP,
  SynHighlighterPython, synhighlighterunixshellscript, SynHighlighterBat,
  SynHighlighterAny, PrintersDlgs, codelibConst, Graphics;

type
  TSearchRecord = record
    Text: string;
    CaseSensitive: boolean;
    WholeWord: boolean;
  end;

  TLanguageRecord = record
    ImageIndex: Integer;            
    Highlighter: TSynCustomHighlighter;
    MenuItem: TMenuItem;
    Code: String;
  end;

{ TCodeFrm }
type
  TCodeFrm = class(TForm)
    actCopyAsHtml: TAction;
    actEditUndo: TAction;
    actEditRedo: TAction;
    actImportOneFileTxt: TAction;
    actImportSeveralFilesTxt: TAction;
    actSetBackgoundColor: TAction;
    actSaveAsTxt: TAction;
    actReadOnly: TAction;
    actSyntaxBat: TAction;
    actSyntaxUNIXShell: TAction;
    actSyntaxPython: TAction;
    actSyntaxPHP: TAction;
    actSyntaxPerl: TAction;
    actSyntaxJavaScript: TAction;
    actSaveAsHtml: TAction;
    actSyntaxJava: TAction;
    CodeDB: TBufDataset;
    dlgColor: TColorDialog;
    mitImportSeveralFilesTXT: TMenuItem;
    mitImportOneFileTXT: TMenuItem;
    mitEditorSep6: TMenuItem;
    mitSetColor: TMenuItem;
    mitImport: TMenuItem;
    mitExportHtml: TMenuItem;
    mitExportTXT: TMenuItem;
    mitExport: TMenuItem;
    mitEditorRedo: TMenuItem;
    mitEditorUndo: TMenuItem;
    mitEditorSep5: TMenuItem;
    mitReadOnly: TMenuItem;
    mitEditorSep4: TMenuItem;
    mitBat: TMenuItem;
    mitUNIXShell: TMenuItem;
    mitSeparatorHighliter: TMenuItem;
    mitPython: TMenuItem;
    mitPHP: TMenuItem;
    mitPerl: TMenuItem;
    mitJavaScript: TMenuItem;
    mitEditorSep3: TMenuItem;
    mitEditorCopyHtml: TMenuItem;
    mitExpSep: TMenuItem;
    mitJAVA: TMenuItem;
    dlgPrinterSetup: TPrinterSetupDialog;
    dlgPrint: TPrintDialog;
    dlgSave: TSaveDialog;
    dlgOpen: TOpenDialog;
    dlgDirectory: TSelectDirectoryDialog;
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
    StatusBar: TStatusBar;
    SynAny: TSynAnySyn;
    SynBat: TSynBatSyn;
    SynCpp: TSynCppSyn;
    SynExportHTML: TSynExporterHTML;
    SynHTML: TSynHTMLSyn;
    SynJava: TSynJavaSyn;
    SynJavaScript: TSynJScriptSyn;
    {$IF LCL_FULLVERSION >= 2010000}
    SynPas: TSynCustomHighlighter;
    {$ELSE}
    SynPas: TSynPasSyn;
    {$ENDIF}
    SynPerl: TSynPerlSyn;
    SynPHP: TSynPHPSyn;
    SynPython: TSynPythonSyn;
    SynSQL: TSynSQLSyn;
    SynUNIXShell: TSynUNIXShellScriptSyn;
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
    tbnFindNext: TToolButton;
    actSyntaxNone: TAction;
    actSyntaxPascal: TAction;
    actSyntaxCpp: TAction;
    actSyntaxHtml: TAction;
    actSyntaxSql: TAction;
    actEditRename: TAction;
    mitTreeRename: TMenuItem;
    procedure actCopyAsHtmlExecute(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actImportOneFileTxtExecute(Sender: TObject);
    procedure actImportSeveralFilesTxtExecute(Sender: TObject);
    procedure actReadOnlyExecute(Sender: TObject);
    procedure actSaveAsHtmlExecute(Sender: TObject);
    procedure actSaveAsTxtExecute(Sender: TObject);
    procedure actSetBackgoundColorExecute(Sender: TObject);
    procedure CodeTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure mActionsUpdate(AAction: TBasicAction; var Handled: boolean);
    procedure tvTopicsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: boolean);
    procedure tvTopicsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvTopicsChange(Sender: TObject; Node: TTreeNode);
    procedure DeleteExecute(Sender: TObject);
    procedure PrinterSetupExecute(Sender: TObject);
    procedure PrintExecute(Sender: TObject);
    procedure ExitExecute(Sender: TObject);
    procedure CutExecute(Sender: TObject);
    procedure CopyExecute(Sender: TObject);
    procedure PasteExecute(Sender: TObject);
    procedure CopyFromIdeExecute(Sender: TObject);
    procedure PasteToIdeExecute(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure FindNextExecute(Sender: TObject);
    procedure tvTopicsDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure tvTopicsDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure ExpandAllExecute(Sender: TObject);
    procedure ContractAllExecute(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure NewSnippetExecute(Sender: TObject);
    procedure NewRootFolderExecute(Sender: TObject);
    procedure NewFolderExecute(Sender: TObject);
    procedure tvTopicsKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OptionsExecute(Sender: TObject);
    procedure tvTopicsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tvTopicsEndDrag(Sender, Target: TObject; X, Y: integer);
    procedure MakeRootExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GenericSyntaxHighlightingExecute(Sender: TObject);
    procedure tvTopicsDblClick(Sender: TObject);
    procedure actEditRenameExecute(Sender: TObject);
    procedure tvTopicsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure UpdateActionsCP(Sender: TObject);
  private
    FModified: boolean;
    FSearch: TSearchRecord;
    FDatabasePath: string;
    FBackgroundColor: TColor;
    FSaveSizePosition: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FTop: Integer;
    FLeft: Integer;
    FWindowState: Integer;
    FCodeText: TSynEdit;
    function CreateNewDB(const DatabaseFile: string): TBufDataset;
    function OpenDB(const DatabaseFile: string): TBufDataset;
    procedure CloseDB(ClearFileName: boolean = False);
    procedure InitializeTreeView;
    procedure SaveRecord;
    procedure DoSearch(First: boolean);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure SetModified(New: boolean);
    procedure SortNodes;
    procedure AddDefaultIndexes(DataSet: TBufDataset);
    procedure SetupSyntaxHighlightingControl;
    procedure SetBackgroundColor(AValue: TColor);
    procedure CheckDBVersion;
    procedure tvTopicsExpand(Sender: TObject; Node: TTreeNode);
    function IsSnippet(Key: Integer): Boolean;
    function GetImageIndex(Key: Integer): Integer; 
  public
    function AddFolder(Node: TTreeNode; const Desc: string): TTreeNode;
    function AddCode(const Desc: string; FullpathFilenameToImport: string; Import: boolean = False): TTreeNode;
    property Modified: boolean read FModified write SetModified;
    property DatabasePath: string read FDatabasePath write FDatabasePath;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property SaveSizePosition: Boolean read FSaveSizePosition write FSaveSizePosition;
  end;

var
  CodeFrm: TCodeFrm;

implementation

{$R *.lfm}
uses
  LazIDEIntf, SrcEditorIntf, MySEPrint, LazConfigStorage, BaseIDEIntf,
  LCLProc,
  IDEOptEditorIntf, EditorSyntaxHighlighterDef,
  codesrch, codeoptions;

const
  windowstateArr: array [0..3] of TWindowState = (wsNormal, wsMinimized, wsMaximized, wsFullScreen);

var
  languageArr: array [0..11] of TLanguageRecord = (
    (ImageIndex: 18; Highlighter: nil; MenuItem: nil; Code: 'NONE'),
    (ImageIndex: 23; Highlighter: nil; MenuItem: nil; Code: 'PASCAL'),
    (ImageIndex: 24; Highlighter: nil; MenuItem: nil; Code: 'CPP'),
    (ImageIndex: 25; Highlighter: nil; MenuItem: nil; Code: 'HTML'),
    (ImageIndex: 26; Highlighter: nil; MenuItem: nil; Code: 'SQL'),
    (ImageIndex: 27; Highlighter: nil; MenuItem: nil; Code: 'JAVA'),
    (ImageIndex: 28; Highlighter: nil; MenuItem: nil; Code: 'JAVASCRIPT'),
    (ImageIndex: 29; Highlighter: nil; MenuItem: nil; Code: 'PERL'),
    (ImageIndex: 30; Highlighter: nil; MenuItem: nil; Code: 'PHP'),
    (ImageIndex: 31; Highlighter: nil; MenuItem: nil; Code: 'PYTHON'),
    (ImageIndex: 32; Highlighter: nil; MenuItem: nil; Code: 'UNIXSHELL'),
    (ImageIndex: 33; Highlighter: nil; MenuItem: nil; Code: 'MSDOSBAT'));

function TCodeFrm.CreateNewDB(const DatabaseFile: string): TBufDataset;
begin
 {$Warnings Off}
  Result := TBufDataset.Create(self)
{$Warnings On}
  ;
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
      FieldDefs.Add('Protect', ftBoolean, 0, False);
      FieldDefs.Add('Language', ftString, 10, False);
      FieldDefs.Add('Expand', ftBoolean, 0, False);
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
  if not FileExists(DatabaseFile) then
    Exit;
  {$Warnings Off}
  Result := TBufDataset.Create(Self);
  {$Warnings On}
  with Result do
  begin
    Filename := DatabaseFile;
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

  procedure LoadTreeView(Node: TTreeNode; Parent: integer);
  var
    BookMark: TBookMark;
    RNode: TTreeNode;
  begin
    BookMark := CodeDB.GetBookMark;
    try
      if CodeDB.Locate('Parent', Parent, []) then // Do not localize.
        while (not CodeDB.EOF) and (CodeDB.FieldByName('Parent').AsInteger = Parent) do
          // Do not localize.
        begin
          RNode := tvTopics.Items.AddChildObject(Node,
            CodeDB.FieldByName('Topic').AsString, // Do not localize.
            Pointer(PtrInt(CodeDB.FieldByName('Key').AsInteger))); // Do not localize.
          RNode.ImageIndex := GetImageIndex(CodeDB.FieldByName('Key').AsInteger);
          RNode.SelectedIndex := GetImageIndex(CodeDB.FieldByName('Key').AsInteger);
          if not IsSnippet(CodeDB.FieldByName('Key').AsInteger) then // Do not localize.
          begin
            LoadTreeView(RNode, CodeDB.FieldByName('Key').AsInteger); // Do not localize.
            RNode.Expanded := CodeDB.FieldByName('Expand').AsBoolean;
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
  tvTopics.OnExpanded := nil;
  tvTopics.OnCollapsed := nil;
  tvTopics.SortType := stNone;
  tvTopics.Items.BeginUpdate;
  try
    CodeDB.First;
    while (not CodeDB.EOF) and (CodeDB.FieldByName('Parent').AsInteger = 0) do
      // Do not localize.
    begin
      Node := tvTopics.Items.AddObject(nil, CodeDB.FieldByName('Topic').AsString,
        // Do not localize.
        Pointer(PtrInt(CodeDB.FieldByName('Key').AsInteger))); // Do not localize.
      Node.ImageIndex := GetImageIndex(CodeDB.FieldByName('Key').AsInteger);
      Node.SelectedIndex := GetImageIndex(CodeDB.FieldByName('Key').AsInteger);
      LoadTreeView(Node, CodeDB.FieldByName('Key').AsInteger); // Do not localize.
      Node.Expanded := CodeDB.FieldByName('Expand').AsBoolean;
      CodeDB.Next;
    end;
  finally
    tvTopics.SortType := stNone;
    tvTopics.Items.EndUpdate;
  end;
  tvTopics.OnExpanded := @tvTopicsExpand;
  tvTopics.OnCollapsed := @tvTopicsExpand;
end;

function TCodeFrm.AddFolder(Node: TTreeNode; const Desc: string): TTreeNode;
var
  NNode: TTreeNode;
  ParentId: Integer;
  Language: String;
begin
  Result := nil;
  ParentId := 0;
  Language := 'PASCAL';
  if (Node <> nil) then
  begin
    ParentId := PtrInt(Node.Data);
    if IsSnippet(PtrInt(Node.Data)) then
      exit
    else
      Language := CodeDB.FieldByName('Language').AsString;
  end;
  with CodeDB do
  begin
    Insert;
    FieldByName('Parent').AsInteger := ParentId;  // Do not localize.
    FieldByName('Language').AsString := Language;
    FieldByName('Topic').AsString := Desc; // Do not localize.
    FieldByName('Type').AsString := 'F';  // Do not localize.
    FieldByName('Expand').AsBoolean := True;
    Post;
  end;
  NNode := tvTopics.Items.AddChildObject(Node, Desc,
    Pointer(PtrInt(CodeDB.FieldByName('Key').AsInteger))); // Do not localize.
  NNode.ImageIndex := GetImageIndex(CodeDB.FieldByName('Key').AsInteger);
  NNode.SelectedIndex := GetImageIndex(CodeDB.FieldByName('Key').AsInteger);
  Result := NNode;
  SortNodes;
end;

procedure TCodeFrm.SetModified(New: boolean);
begin
  FModified := New;
  if FModified then
    StatusBar.Panels[1].Text := rsModified
  else
    StatusBar.Panels[1].Text := ''; // No need to localize.
end;

function TCodeFrm.AddCode(const Desc: string; FullpathFilenameToImport: string; Import: boolean = False): TTreeNode;
var
  Node: TTreeNode;
  T: TStringList;
  Language: String;
begin
  Result := nil;
  if tvTopics.Selected = nil then begin
    Dialogs.MessageDlg(rs_error, rs_noFolderSelected, mtError, [mbOK], 0);
    Exit;
  end
  else if IsSnippet(PtrInt(tvTopics.Selected.Data)) then
    exit;
  Language := CodeDB.FieldByName('Language').AsString;
  with CodeDB do
  begin
    Insert;
    FieldByName('Parent').AsInteger := PtrInt(tvTopics.Selected.Data);
    // Do not localize.
    FieldByName('Topic').AsString := Desc;  // Do not localize.
    FieldByName('Type').AsString := 'C';  // Do not localize.
    FieldByName('Language').AsString := Language;

    if Import then
    begin
      T := TStringList.Create;
     	T.LoadFromFile(FullpathFilenameToImport);
      FieldByName('Code').AsString := T.Text;
      T.Free;

      case ExtractFileExt(FullpathFilenameToImport) of
        '.pas': FieldByName('Language').AsString := 'PASCAL';      // Do not localize.
        '.pp': FieldByName('Language').AsString := 'PASCAL';      // Do not localize.
        '.c': FieldByName('Language').AsString := 'CPP';         // Do not localize.
        '.cpp': FieldByName('Language').AsString := 'CPP';         // Do not localize.
        '.h': FieldByName('Language').AsString := 'CPP';         // Do not localize.
        '.htm': FieldByName('Language').AsString := 'HTML';        // Do not localize.
        '.html': FieldByName('Language').AsString := 'HTML';        // Do not localize.
        '.sql': FieldByName('Language').AsString := 'SQL';         // Do not localize.
        '.java': FieldByName('Language').AsString := 'JAVA';        // Do not localize.
        '.js': FieldByName('Language').AsString := 'JAVASCRIPT';  // Do not localize.
        '.pl': FieldByName('Language').AsString := 'PERL';        // Do not localize.
        '.pm': FieldByName('Language').AsString := 'PERL';        // Do not localize.
        '.t': FieldByName('Language').AsString := 'PERL';        // Do not localize.
        '.php': FieldByName('Language').AsString := 'PHP';         // Do not localize.
        '.py': FieldByName('Language').AsString := 'PYTHON';      // Do not localize.
        '.py3': FieldByName('Language').AsString := 'PYTHON';      // Do not localize.
        '.sh': FieldByName('Language').AsString := 'UNIXSHELL';   // Do not localize.
        '.bash': FieldByName('Language').AsString := 'UNIXSHELL';   // Do not localize.
        '.bat': FieldByName('Language').AsString := 'MSDOSBAT';   // Do not localize.
      end;

    end;

    Post;

  end;
  Node := tvTopics.Items.AddChildObject(tvTopics.Selected, Desc,
    Pointer(PtrInt(CodeDB.FieldByName('Key').AsInteger)));  // Do not localize.
  Node.ImageIndex := GetImageIndex(CodeDB.FieldByName('Key').AsInteger);
  Node.SelectedIndex := GetImageIndex(CodeDB.FieldByName('Key').AsInteger);
  Result := Node;
  SortNodes;
end;

procedure TCodeFrm.CodeTextChange(Sender: TObject);
begin
  Modified := FCodeText.Modified;
end;

procedure TCodeFrm.actSaveAsHtmlExecute(Sender: TObject);
var
  T: TStringList;
begin
  dlgSave.Filter := SynExportHTML.DefaultFilter;
  dlgSave.DefaultExt := '.html';

  if dlgSave.Execute then
  begin
    SynExportHTML.Highlighter := FCodeText.Highlighter;
    SynExportHTML.Title := '';
    SynExportHTML.Color := FCodeText.Color;
    SynExportHTML.ExportAsText := True;

    if FCodeText.SelText = '' then
      SynExportHTML.ExportAll(FCodeText.Lines)
    else
    begin
      T := TStringList.Create;
      T.Add(FCodeText.SelText);
      SynExportHTML.ExportAll(T);
      T.Free;
    end;
    SynExportHTML.SaveToFile(dlgSave.FileName);
  end;
end;

procedure TCodeFrm.actSaveAsTxtExecute(Sender: TObject);
var
  T: TStringList;
begin
  dlgSave.Filter := ExportTextFilter;
  dlgSave.DefaultExt := '.txt';
  if dlgSave.Execute then
  begin
    if FCodeText.SelText = '' then
      FCodeText.Lines.SaveToFile(dlgSave.FileName)
    else
    begin
      T := TStringList.Create;
      T.Add(FCodeText.SelText);
      T.SaveToFile(dlgSave.FileName);
      T.Free;
    end;
  end;
end;

procedure TCodeFrm.actSetBackgoundColorExecute(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  if dlgColor.Execute then
    try
      BackgroundColor := dlgColor.Color;
    finally
      Screen.Cursor := crDefault;
    end;
end;

procedure TCodeFrm.actCopyAsHtmlExecute(Sender: TObject);
begin
  if FCodeText.Highlighter <> nil then
    SynExportHTML.Highlighter := FCodeText.Highlighter;
  SynExportHTML.Color := FCodeText.Color;
  SynExportHTML.ExportAsText := True;
  SynExportHTML.Options := [heoFragmentOnly];
  SynExportHTML.UseBackground := True;
  SynExportHTML.ExportRange(FCodeText.Lines, FCodeText.BlockBegin, FCodeText.BlockEnd);
  SynExportHTML.CopyToClipboard;
end;

procedure TCodeFrm.actEditRedoExecute(Sender: TObject);
begin
  FCodeText.Redo;
end;

procedure TCodeFrm.actEditUndoExecute(Sender: TObject);
begin
  FCodeText.Undo;
end;

procedure TCodeFrm.actImportOneFileTxtExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  if dlgOpen.Execute then begin
    try
      Screen.Cursor := crHourglass;
      Node := AddCode(ExtractFileName(dlgOpen.FileName), dlgOpen.FileName, True);
      if Node <> nil then
        tvTopics.Selected := Node;
    finally
        Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TCodeFrm.actImportSeveralFilesTxtExecute(Sender: TObject);


          function GetStrlistOfTextFilesToImport(const sImportingDir: string): TStringList;
          var
           recDirSR: TSearchRec;
           o_List: TStringList;
          const
            sMaskGlobex: string = '*.txt';
          begin
            o_List := TStringList.Create;
            o_List.Sorted := False;
            o_List.Duplicates := dupIgnore;
            o_List.CaseSensitive := False;
            if FindFirst(sImportingDir + DirectorySeparator + sMaskGlobex, faAnyFile, recDirSR) = 0 then begin
             repeat
               o_List.Add(sImportingDir + DirectorySeparator + recDirSR.Name);
             until FindNext(recDirSR) <> 0;
            findclose(recDirSR);
            end;
	          Result := o_List;
          end;


var
	oStrlstTextFilesToImport: TStringList;
  sTextFile: string;
  i, iNbrFiles: integer;
begin
  if dlgDirectory.Execute then begin
    try
      Screen.Cursor := crHourglass;
      oStrlstTextFilesToImport:= GetStrlistOfTextFilesToImport(dlgDirectory.FileName);
      iNbrFiles := oStrlstTextFilesToImport.Count;
      if iNbrFiles > 0 then begin
			  for i := 0 to Pred(iNbrFiles) do begin
          sTextFile := oStrlstTextFilesToImport.Strings[i];
          AddCode(ExtractFileName(sTextFile), sTextFile, True);
        end;
      end
      else
        Dialogs.MessageDlg(rs_error, rs_noFileFoundInImportingFolder, mtError, [mbOK], 0);
    finally
      if Assigned(oStrlstTextFilesToImport) then FreeAndNil(oStrlstTextFilesToImport);
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TCodeFrm.actReadOnlyExecute(Sender: TObject);
begin
  FCodeText.ReadOnly := mitReadOnly.Checked;
  SaveRecord;
end;

procedure TCodeFrm.FormCreate(Sender: TObject);
begin
  LoadSettings;
end;

procedure TCodeFrm.FormDestroy(Sender: TObject);
begin
  if FModified then
    SaveRecord;
  SaveSettings;
  CloseDB(True);
  FreeAndNil(CodeDB);
end;

procedure TCodeFrm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_F3 then
    actEditFindNext.Execute;
  if (Key = VK_F) and (ssCtrl in Shift) then
    actEditFind.Execute;
end;

procedure TCodeFrm.mActionsUpdate(AAction: TBasicAction; var Handled: boolean);
var
  HaveEditorSelection: boolean;
  HaveSelectedNode: boolean;
  SnippetIsSelected: boolean;
  Editor: TSourceEditorInterface;
begin
  HaveEditorSelection := Length(FCodeText.SelText) > 0;
  if not FCodeText.ReadOnly then
    actEditCut.Enabled := HaveEditorSelection;

  actEditCopy.Enabled := HaveEditorSelection;
  actCopyAsHtml.Enabled := actEditCopy.Enabled;
  actEditUndo.Enabled := FCodeText.CanUndo;
  actEditRedo.Enabled := FCodeText.CanRedo;

  // bug on linux on menu
  // actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));
  HaveSelectedNode := tvTopics.Selected <> nil;
  SnippetIsSelected := HaveSelectedNode and IsSnippet(PtrInt(tvTopics.Selected.Data));

  Editor := SourceEditorManagerIntf.ActiveEditor;
  if Editor <> nil then
  begin
    if not FCodeText.ReadOnly then
      actEditCopyFromIde.Enabled := (SnippetIsSelected and (Editor.SelectionAvailable));
    actEditPasteToIde.Enabled := (SnippetIsSelected and (FCodeText.SelAvail));
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
    actMakeRoot.Enabled := (not (tvTopics.Selected.Level = 0)) and
      (not SnippetIsSelected);
    actNewSnippet.Enabled := not SnippetIsSelected;
    actNewFolder.Enabled := not SnippetIsSelected;
  end;
  actPrint.Enabled := SnippetIsSelected;

  actImportOneFileTxt.Enabled := not SnippetIsSelected  and HaveSelectedNode;
  actImportSeveralFilesTxt.Enabled := not SnippetIsSelected  and HaveSelectedNode;

  actSaveAsHtml.Enabled := SnippetIsSelected;
  actSaveAsTxt.Enabled := SnippetIsSelected;
  mitExport.Enabled := SnippetIsSelected;

  Handled := True;
end;

procedure TCodeFrm.tvTopicsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: boolean);
begin
  if (tvTopics.Selected <> nil) and Modified then
    SaveRecord; // Do not alter value of AllowChange.
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
  if not CodeDB.Active then
    Exit;
  if tvTopics.Selected = nil then
    Exit;
  if CodeDB.Locate('Key', PtrInt(tvTopics.Selected.Data), []) then
  begin
    CodeDB.Edit;
    CodeDB.FieldByName('Topic').AsString := tvTopics.Selected.Text;

    if IsSnippet(PtrInt(tvTopics.Selected.Data)) then
    begin
      CodeDB.FieldByName('Code').AsString := FCodeText.Text;
      CodeDB.FieldByName('Protect').AsBoolean := FcodeText.ReadOnly;
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
var
  i: Integer;
begin
  try
    if (Node <> nil) and IsSnippet(PtrInt(Node.Data)) then
    begin
      if CodeDB.Locate('Key', PtrInt(Node.Data), []) then  // Do not localize.
      begin
        FCodeText.Lines.BeginUpdate;
        try
          FCodeText.HighLighter := languageArr[Low(languageArr)].Highlighter;
          languageArr[Low(languageArr)].MenuItem.Checked := True;
          for i := Low(languageArr) to High(languageArr) do
          begin
            if SameText(languageArr[i].Code, CodeDB.FieldByName('Language').AsString) then
            begin
              FCodeText.HighLighter := languageArr[i].Highlighter;
              languageArr[i].MenuItem.Checked := True;
              break;
            end;
          end;
          FCodeText.Text := CodeDB.FieldByName('Code').AsString;
        finally
          FCodeText.Lines.EndUpdate;
        end;
      end;
      actReadOnly.Enabled := True;
      FCodeText.ReadOnly := CodeDB.FieldByName('Protect').AsBoolean;
      actReadOnly.Checked := FCodeText.ReadOnly;
    end
    else
    begin
      FCodeText.Lines.Clear;
      FCodeText.ReadOnly := True;
      actReadOnly.Enabled := False;
    end;
    Modified := False;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  UpdateActionsCP(Sender);
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
    if tvTopics.Selected = nil then
      Exit;
    if CodeDB.Locate('Key', PtrInt(tvTopics.Selected.Data), []) then  // Do not localize.
    begin
      if IsSnippet(PtrInt(tvTopics.Selected.Data)) then
        NodeType := rsSnippet
      else
        NodeType := rsFolder;
      if MessageDlg(Format(rsConfirmDelete, [NodeType]), mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
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
  dlgPrinterSetup.Execute;
end;

procedure TCodeFrm.PrintExecute(Sender: TObject);
var
  Print: TMySEPrint;
begin
  if dlgPrint.Execute then
  begin
    Print := TMySEPrint.Create(Self);
    Print.SynEdit := FCodeText;
    Print.Highlighter := FCodeText.Highlighter;
    Print.Colors := True;
    Print.Title := '';/// ExtractFileName((Tmp As TEditor).FileInEditing);
    Print.Color := FCodeText.Color; ///
    Print.Font := FCodeText.Font;
    Print.LineNumbers := False;
    if Assigned(Print.Highlighter) then
      Print.Highlight := True;
    Print.Print;
    Print.Free;
  end;
end;

procedure TCodeFrm.ExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TCodeFrm.CutExecute(Sender: TObject);
begin
  UpdateActionsCP(Sender);
  FCodeText.CutToClipboard;
end;

procedure TCodeFrm.CopyExecute(Sender: TObject);
begin
  UpdateActionsCP(Sender);
  FCodeText.CopyToClipboard;
end;

procedure TCodeFrm.PasteExecute(Sender: TObject);
begin
  if not FCodeText.ReadOnly then
    FCodeText.PasteFromClipBoard;
end;


procedure TCodeFrm.CopyFromIdeExecute(Sender: TObject);
var
  Editor: TSourceEditorInterface;
begin
  Editor := SourceEditorManagerIntf.ActiveEditor;
  if Editor = nil then
    exit;
  FCodeText.InsertTextAtCaret(Editor.GetText(True));
end;

procedure TCodeFrm.PasteToIdeExecute(Sender: TObject);
var
  Editor: TSourceEditorInterface;
begin
  Editor := SourceEditorManagerIntf.ActiveEditor;
  if Editor = nil then
    exit;
  editor.InsertLine(editor.CursorTextXY.Y, FCodeText.SelText, True);
end;

procedure TCodeFrm.FindExecute(Sender: TObject);
begin
  try
    with TfmCodeSearch.Create(nil) do
      try
        if ShowModal = mrOk then
        begin
          FSearch.Text := edSearch.Text;
          FSearch.CaseSensitive := cbCaseSensitive.Checked;
          FSearch.WholeWord := cbWholeWord.Checked;
          FCodeText.SetFocus;
          DoSearch(True);
        end;
      finally
        Free;
      end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TCodeFrm.DoSearch(First: boolean);

  function CaseInsensitivePos(const SubString, S: string): integer;
  begin
    Result := AnsiPos(AnsiUpperCase(SubString), AnsiUpperCase(S));
  end;

  function CharIsAlpha(const C: char): boolean;
  begin
    Result := CharInSet(C, ['a'..'z', 'A'..'Z']);
  end;

  function DoMatch(const Text: string): integer;
  var
    MatchPos: integer;
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
  Match: integer;
  InTopic: boolean;
  FirstLoop: boolean;
begin
  try
    Screen.Cursor := crHourGlass;
    try
      Node := nil;
      if not First then
      begin
        Node := tvTopics.Selected;
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
            Match := DoMatch(Copy(CodeDB.FieldByName('Code').AsString,
              FCodeText.SelStart + Length(FCodeText.SelText), 999999));
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
          if Match > 0 then
            Break;
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
          Dec(Match);
          tvTopics.OnChange(Self, tvTopics.Selected);
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

procedure TCodeFrm.tvTopicsDragDrop(Sender, Source: TObject; X, Y: integer);
var
  Node: TTreeNode;
begin
  try
    if tvTopics.Selected = nil then
      Exit;
    Node := tvTopics.GetNodeAt(X, Y);
    if Node = nil then
      Exit;
    if IsSnippet(PtrInt(Node.Data)) then
    begin
      MessageDlg(rsCannotAttach, mtError, [mbOK], 0);
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

procedure TCodeFrm.tvTopicsDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
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
  if (tvTopics.Selected <> nil) and Modified then
    SaveRecord;
  Node := tvTopics.Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Collapse(True);
    Node := Node.GetNextSibling;
  end;
  tvTopicsChange(tvTopics, tvTopics.Selected);
end;

procedure TCodeFrm.SaveSettings;
var
  Config: TConfigStorage;
  i: Integer;
begin
  FWidth := ClientWidth;
  FHeight := ClientHeight;
  FTop := Top;
  FLeft := Left;
  for i := Low(windowstateArr) to High(windowstateArr) do
    if windowstateArr[i] = WindowState then
    begin
      FWindowState := i;
      break;
    end;
  try
    Config := GetIDEConfigStorage('codelib.xml', False);
    try
      Config.SetDeleteValue('CodeLib/DBPath/Value', DatabasePath, LazarusIDE.GetPrimaryConfigPath);
      Config.SetDeleteValue('Color/BackgroundColor/Value', FBackgroundColor, Graphics.clWhite);
      Config.SetDeleteValue('SizePosition/Save/Value', FSaveSizePosition, True);
      if FSaveSizePosition then
      begin
        Config.SetValue('SizePosition/Width/Value', FWidth);
        Config.SetValue('SizePosition/Height/Value', FHeight);
        Config.SetValue('SizePosition/Top/Value', FTop);
        Config.SetValue('SizePosition/Left/Value', FLeft);
        Config.SetValue('SizePosition/WindowState/Value', FWindowState);
        Config.SetValue('SizePosition/TreeViewWidth/Value', tvTopics.Width);
      end;
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      DebugLn(['Saving codelib.xml failed: ', E.Message]);
    end;
  end;
end;

procedure TCodeFrm.LoadSettings;
var
  Config: TConfigStorage;
begin  // loadresource string;

  actDelete.Caption := rs_actDelete;
  actNewRootFolder.Caption := rs_actNewRootFolder;
  actNewFolder.Caption := rs_actNewFolder;
  actNewSnippet.Caption := rs_actNewSnippet;
  actMakeRoot.Caption := rs_actMakeRoot;
  actPrinterSetup.Caption := rs_actPrinterSetup;
  actPrint.Caption := rs_actPrint;
  actExit.Caption := rs_actExit;
  actEditCut.Caption := rs_actEditCut;
  actEditCopy.Caption := rs_actEditCopy;
  actEditPaste.Caption := rs_actEditPaste;
  actEditCopyFromIde.Caption := rs_actEditCopyFromIde;
  actEditPasteToIde.Caption := rs_actEditPasteToIde;
  actEditFind.Caption := rs_actEditFind;
  actEditFindNext.Caption := rs_actEditFindNext;
  actEditUndo.Caption := rs_actEditUndo;
  actEditRedo.Caption := rs_actEditRedo;
  actEditRename.Caption := rs_actEditRename;
  actExpandAll.Caption := rs_actExpandAll;
  actContractAll.Caption := rs_actContractAll;
  actOptions.Caption := rs_actOptions;
  actSyntaxNone.Caption := rs_actSyntaxNone;
  actReadOnly.Caption := rs_actReadOnly;
  actSetBackgoundColor.Caption := rs_actSetBackgoundColor;

  actSaveAsHtml.Caption := rs_actSaveAsHtml;
  actSaveAsTxt.Caption := rs_actSaveAsTXT;
  actImportOneFileTxt.Caption := rs_actImportOneFileTxt;
  actImportSeveralFilesTxt.Caption := rs_actImportSeveralFilesTxt;

  mitFile.Caption := rs_mitFile;
  mitFileNew.Caption := rs_mitFileNew;

  mitExport.Caption := rs_mitExport;
  mitImport.Caption := rs_mitImport;

  mitEdit.Caption := rs_mitEdit;
  mitOptions.Caption := rs_mitOptions;
  mitEditorHighlighting.Caption := rs_mitEditorHighlighting;
  mitTreeNew.Caption := rs_mitFileNew;

  SetupSyntaxHighlightingControl;
  Caption := rsMenuName;

  try
    Config := GetIDEConfigStorage('codelib.xml', True);
    try
      FDatabasePath := Config.GetValue('CodeLib/DBPath/Value', LazarusIDE.GetPrimaryConfigPath);
      FBackgroundColor := Config.GetValue('Color/BackgroundColor/Value', Graphics.clWhite);
      FSaveSizePosition := Config.GetValue('SizePosition/Save/Value', True);
      if FSaveSizePosition then
      begin
        FWidth := Config.GetValue('SizePosition/Width/Value', ClientWidth);
        FHeight := Config.GetValue('SizePosition/Height/Value', ClientHeight);
        FTop := Config.GetValue('SizePosition/Top/Value', Top);
        FLeft := Config.GetValue('SizePosition/Left/Value', Left);
        FWindowState := Config.GetValue('SizePosition/WindowState/Value', Low(windowstateArr));
        if (FWindowState < Low(windowstateArr)) or (FWindowState > Low(windowstateArr)) then
          FWindowState := Low(windowstateArr);
        tvTopics.Width := Config.GetValue('SizePosition/TreeViewWidth/Value', tvTopics.Width);
      end;
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      DebugLn(['Loading codelib.xml failed: ', E.Message]);
    end;
  end;

  CodeDB := OpenDB(AppendPathDelim(FDatabasePath) + DefaultDBFileName); // do not localize

  if CodeDB = nil then
    CodeDB := CreateNewDb(AppendPathDelim(FDatabasePath) + DefaultDBFileName)
  else
    CheckDBVersion;

  InitializeTreeView;
  mitPascal.Checked := True;
  FModified := False;
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
    Node := AddCode(rsNewCode, '', False);
    if Node <> nil then
    begin
      tvTopics.Selected := Node;
      tvTopics.Selected.EditText;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCodeFrm.NewRootFolderExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := AddFolder(nil, rs_actNewFolder);
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
    Node := AddFolder(tvTopics.Selected, rs_actNewFolder);
    if Node <> nil then
    begin
      tvTopics.Selected := Node;
      tvTopics.Selected.EditText;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCodeFrm.tvTopicsKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_F2) and (tvTopics.Selected <> nil) then
    tvTopics.Selected.EditText;
end;

procedure TCodeFrm.OptionsExecute(Sender: TObject);
begin
  try
    with TFrmOptions.Create(nil) do
      try
        ShowModal;
      finally
        SaveSettings;
        Free;
        if CodeDB <> nil then
          CodeDB.Close;
        tvTopics.Items.Clear;
        LoadSettings;
      end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TCodeFrm.tvTopicsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  AutoScroll := True;
end;

procedure TCodeFrm.tvTopicsEndDrag(Sender, Target: TObject; X, Y: integer);
begin
  AutoScroll := False;
end;

procedure TCodeFrm.MakeRootExecute(Sender: TObject);
begin
  with tvTopics do
    if (Selected <> nil) and (Selected.Level > 0) and
      not IsSnippet(PtrInt(Selected.Data)) then
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
  if FModified then
    SaveRecord;
  CloseDB;
  SaveSettings;
end;

procedure TCodeFrm.FormShow(Sender: TObject);
begin
  if (CodeDB <> nil) and not CodeDB.Active then
    CodeDB.Open;
  SetupSyntaxHighlightingControl;
  if FSaveSizePosition then
  begin
    ClientWidth := FWidth;
    ClientHeight := FHeight;
    Top := FTop;
    Left := FLeft;
    WindowState := windowstateArr[FWindowState];
  end;
end;

procedure TCodeFrm.SortNodes;
begin
  tvTopics.SortType := stNone;
end;

procedure TCodeFrm.GenericSyntaxHighlightingExecute(Sender: TObject);
var
  i: Integer;
begin
  if CodeDB.Locate('Key', PtrInt(tvTopics.Selected.Data), []) then
  begin
    for i := Low(languageArr) to High(languageArr) do
    begin
      if languageArr[i].ImageIndex = (Sender as TAction).ImageIndex then
      begin
        CodeDB.Edit;
        CodeDB.FieldByName('Language').AsString := languageArr[i].Code;
        CodeDB.Post;
        FCodeText.Highlighter := languageArr[i].Highlighter;
        if IsSnippet(PtrInt(tvTopics.Selected.Data)) then
        begin
          tvTopics.Selected.ImageIndex := languageArr[i].ImageIndex;
          tvTopics.Selected.SelectedIndex := languageArr[i].ImageIndex;
        end;
        break;
      end;
    end;
  end;
end;

procedure TCodeFrm.SetupSyntaxHighlightingControl;
begin
  if not Assigned(FCodeText) then
  begin
    FCodeText := TSynEdit.Create(Self);
 {$IF LCL_FULLVERSION >= 2010000}
    SynPas := TSynCustomHighlighter(IDEEditorOptions.CreateSynHighlighter(lshFreePascal));
 {$ELSE}
    SynPas := TSynPasSyn.Create(Self);
  {$ENDIF}
  end;
 {$IFDEF LCL_FULLVERSION >= 2010000}
  FCodeText.HighLighter := SynPas;
 {$ELSE}
  FCodeText.HighLighter := SynCPP;
 {$ENDIF}
  with  SourceEditorManagerIntf do
  begin
    GetEditorControlSettings(FCodeText);
    GetHighlighterSettings(SynCPP);
    GetHighlighterSettings(SynSQL);
    GetHighlighterSettings(SynHTML);
    GetHighlighterSettings(SynJava);
    GetHighlighterSettings(SynJavaScript);
    GetHighlighterSettings(SynPerl);
    GetHighlighterSettings(SynPHP);
    GetHighlighterSettings(SynPython);
    GetHighlighterSettings(SynUnixShell);
    GetHighlighterSettings(SynBat);
    GetHighlighterSettings(SynPas);
  end;

 {$IF LCL_FULLVERSION < 2010000}
  SynPas.AsmAttri := SynCpp.AsmAttri;
  SynPas.CommentAttri := SynCpp.CommentAttri;
  SynPas.DirectiveAttri := SynCpp.DirecAttri;
  SynPas.IdentifierAttri := SynSQL.IdentifierAttri;
  SynPas.KeyAttri := SynCPP.KeyAttri;
  SynPAs.NumberAttri := SynCPP.NumberAttri;
  SynPas.SpaceAttri := SynCpp.SpaceAttri;
  SynPas.SymbolAttri := SynCpp.SymbolAttri;
 {$ENDIF}

  languageArr[0].Highlighter := SynAny;
  languageArr[1].Highlighter := SynPas;
  languageArr[2].Highlighter := SynCpp;
  languageArr[3].Highlighter := SynHTML;
  languageArr[4].Highlighter := SynSQL;
  languageArr[5].Highlighter := SynJava;
  languageArr[6].Highlighter := SynJavaScript;
  languageArr[7].Highlighter := SynPerl;
  languageArr[8].Highlighter := SynPHP;
  languageArr[9].Highlighter := SynPython;
  languageArr[10].Highlighter := SynUNIXShell;
  languageArr[11].Highlighter := SynBat;

  languageArr[0].MenuItem := mitNone;
  languageArr[1].MenuItem := mitPascal;
  languageArr[2].MenuItem := mitCPP;
  languageArr[3].MenuItem := mitHTML;
  languageArr[4].MenuItem := mitSQL;
  languageArr[5].MenuItem := mitJAVA;
  languageArr[6].MenuItem := mitJavaScript;
  languageArr[7].MenuItem := mitPerl;
  languageArr[8].MenuItem := mitPHP;
  languageArr[9].MenuItem := mitPython;
  languageArr[10].MenuItem := mitUNIXShell;
  languageArr[11].MenuItem := mitBat;

  with FCodeText do
  begin
    BeginUpdate();
    Align := alClient;
    PopupMenu := pmCode;
    OnChange := @CodeTextChange;
    Parent := pnlView;
    Gutter.Parts[0].Visible := False;
    Gutter.Parts[1].Visible := False;
    Gutter.Parts[3].Visible := False;
    BorderStyle := bsNone;
    RightEdge := -1;
    Keystrokes[88].ShortCut := (0); //remove Ctrl+Alt+C from synedit
    EndUpdate;
  end;
  UpdateActionsCP(Self);

  FCodeText.HighLighter := nil;

  FCodeText.Color := FBackgroundColor;
end;

procedure TCodeFrm.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    FCodeText.Color := FBackgroundColor;
  end;
end;

procedure TCodeFrm.CheckDBVersion;
var
  i: Integer;
  tableOK: Boolean = False;
  TempDB: TBufDataSet;
begin
  for i := 0 to CodeDB.FieldDefs.Count - 1 do
  begin
    tableOK := SameText(CodeDB.FieldDefs[i].Name, 'Expand');
    if tableOK then
      break;
  end;
  if not tableOK then
  begin
    CodeDB.Close;
    FreeAndNil(CodeDB);
    TempDB := TBufDataset.Create(nil);
    TempDB.LoadFromFile(AppendPathDelim(FDatabasePath) + DefaultDBFileName);
    TempDB.Open;
    if TempDB.RecordCount > 0 then
    begin
      CodeDB := CreateNewDb(AppendPathDelim(FDatabasePath) + DefaultDBFileName);
      TempDB.First;
      while not TempDB.EOF do
      begin
        CodeDB.Insert;
        for i := 0 to TempDB.Fields.Count - 1 do
          CodeDB.Fields[i].Value := TempDB.Fields[i].Value;
        CodeDB.Append;
        TempDB.Next;
      end;
    end;
    TempDB.Close;
    FreeAndNil(TempDB);
  end;
end;

procedure TCodeFrm.tvTopicsExpand(Sender: TObject; Node: TTreeNode);
begin
  if not CodeDB.Active then
    Exit;
  if CodeDB.Locate('Key', PtrInt(Node.Data), []) then
  begin
    CodeDB.Edit;
    CodeDB.FieldByName('Expand').AsBoolean := Node.Expanded;
    CodeDB.Post;
  end;
end;    

function TCodeFrm.IsSnippet(Key: Integer): Boolean;
begin
  Result := False;
  if (CodeDB.FieldByName('Key').AsInteger = Key) or CodeDB.Locate('Key', Key, []) then
    Result := not SameText(CodeDB.FieldByName('Type').AsString, 'F');
end;

function TCodeFrm.GetImageIndex(Key: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if not IsSnippet(Key) then
    Result := FolderImageIndex
  else
  begin
    Result := 18;
    for i := Low(languageArr) to High(languageArr) do
    begin
      if SameText(languageArr[i].Code, CodeDB.FieldByName('Language').AsString) then
      begin
        Result := languageArr[i].ImageIndex;
        break;
      end;
    end;
  end;
end;

procedure TCodeFrm.UpdateActionsCP(Sender: TObject);
begin
  actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));
  actEditCut.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));
end;

procedure TCodeFrm.tvTopicsDblClick(Sender: TObject);
begin
  if tvTopics.Selected <> nil then
  begin
    tvTopics.SetFocus;
    if IsSnippet(PtrInt(tvTopics.Selected.Data)) then
      actEditPasteToIde.Execute;
  end;
end;

procedure TCodeFrm.actEditRenameExecute(Sender: TObject);
begin
  if tvTopics.Selected <> nil then
    tvTopics.Selected.EditText;
end;

procedure TCodeFrm.tvTopicsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
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

procedure TCodeFrm.CloseDB(ClearFileName: boolean);
begin
  if CodeDB <> nil then
  begin
    CodeDB.Close;
  end;
end;

procedure TCodeFrm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    actExit.Execute;
  end;
end;

end.
