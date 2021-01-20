unit codelib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, Forms, Controls, Graphics, Dialogs,
  DBGrids, ComCtrls, StdCtrls, ExtCtrls, Menus, DBCtrls, ActnList,  LCLintf,
  LazFileUtils,
  SynEdit,
  SynHighlighterCpp,
  SynHighlighterHTML,
  SynHighlighterSQL,
  SynEditTypes, SynHighlighterPas;

type
  SearchRecord = record
    Text: string;
    CaseSensitive: Boolean;
    WholeWord: Boolean;
  end;

  { TCodeFrm }

  TCodeFrm = class(TForm)
    About1: TMenuItem;
    CodeDb: TBufDataset;
    Contents1: TMenuItem;
    Copy1: TMenuItem;
    CopyfromDelphi1: TMenuItem;
    Cut1: TMenuItem;
    DataSource1: TDataSource;
    Delete1: TMenuItem;
    Edit1: TMenuItem;
    Exit1: TMenuItem;
    File1: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    Folder1: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    ImgBar: TImageList;
    ImgTree: TImageList;
    MainMenu1: TMainMenu;
    mitContractAll: TMenuItem;
    mitCopy: TMenuItem;
    mitCopyfromDelphi: TMenuItem;
    mitCPP: TMenuItem;
    mitCut: TMenuItem;
    mitDelete: TMenuItem;
    mitExpandAll: TMenuItem;
    mitHighlighting: TMenuItem;
    mitHTML: TMenuItem;
    mitMakeRoot: TMenuItem;
    mitNew: TMenuItem;
    mitNewFolder: TMenuItem;
    mitNewRootFolder: TMenuItem;
    mitNewSnippet: TMenuItem;
    mitNone: TMenuItem;
    mitPascal: TMenuItem;
    mitPaste: TMenuItem;
    mitPasteintoDelphi: TMenuItem;
    mitSep: TMenuItem;
    mitSQL: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    New1: TMenuItem;
    Options1: TMenuItem;
    Options2: TMenuItem;
    PackDatabase1: TMenuItem;
    Paste1: TMenuItem;
    PasteintoDelphi1: TMenuItem;
    pmCode: TPopupMenu;
    pmTopics: TPopupMenu;
    pnlView: TPanel;
    CodeText: TSynEdit;
    Print1: TMenuItem;
    PrinterSetup1: TMenuItem;
    RootFolder1: TMenuItem;
    Snippet1: TMenuItem;
    Splitter1: TSplitter;
    SynCpp: TSynCppSyn;
    SynHTML: TSynHTMLSyn;
    SynPas: TSynPasSyn;
    SynSQL: TSynSQLSyn;
    ToolBar: TToolBar;
    tbFolder: TToolButton;
    tbOpen: TToolButton;
    tbDelete: TToolButton;
    sep1: TToolButton;
    sep2: TToolButton;
    tbCut: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    sep3: TToolButton;
    tbCopyFromLazarus: TToolButton;
    tbPasteToLazarus: TToolButton;
    sep4: TToolButton;
    tbExpand: TToolButton;
    tbContract: TToolButton;
    sep5: TToolButton;
    tbFind: TToolButton;
    tvTopics: TTreeView;
    Updatedatabase1: TMenuItem;

    procedure CodeTextStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure Delete1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FindNext1Click(Sender: TObject);
    procedure Folder1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mitMakeRootClick(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure pmCodePopup(Sender: TObject);
    procedure RootFolder1Click(Sender: TObject);
    procedure Snippet1Click(Sender: TObject);
    procedure tbContractClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
    procedure tbCutClick(Sender: TObject);
    procedure tbExpandClick(Sender: TObject);
    procedure tbFindClick(Sender: TObject);
    procedure tbPasteClick(Sender: TObject);
    procedure tvTopicsChange(Sender: TObject; Node: TTreeNode);
    procedure tvTopicsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure SetLanguage(Sender: TObject);
    procedure tvTopicsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvTopicsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvTopicsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvTopicsEdited(Sender: TObject; Node: TTreeNode; var S: string);
  private

     FModified: Boolean;
     FSearch: SearchRecord;
     function CreateNewDB(DBFileName: string): TBufDataset;
     function OpenDB(DBFileName: string): TBufDataset;
     procedure InitializeTreeView;
     procedure SetModified(AValue: Boolean);
     procedure SortNodes;
     procedure SaveRecord;
     procedure DoSearch(First: Boolean);
  public
     function AddFolder(Node: TTreeNode; Desc: string): TTreeNode;
     function AddCode(Desc: string): TTreeNode;
     property Modified: Boolean read FModified write SetModified;
  end;

var
  CodeFrm: TCodeFrm = nil;

resourcestring
  SMenuName = 'Code Librarian';
  SNewCode = 'New Code';
  SNewFolder = 'New Folder';
  SSnippet = 'snippet';
  SFolder = 'folder';
  SConfirmDelete = 'Delete this %s?';
  SCannotAttach = 'Subitems cannot be attached to a code snippet, only folders.';


implementation
uses Clipbrd, codesrch, LazIDEIntf;
{$R *.lfm}

{ TCodeFrm }
procedure TCodeFrm.FormCreate(Sender: TObject);
var dbPath:string;
begin
 caption:=SMenuName;
 dbPath:=LazarusIDE.GetPrimaryConfigPath;
 CodeDB := OpenDB(dbPath+'/CodeLibrarian.dat'); // do not localize
 if CodeDB = nil then CodeDB := createNewDb(dbPath+'/CodeLibrarian.dat');
 InitializeTreeView;
 //
end;

procedure TCodeFrm.Folder1Click(Sender: TObject);
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

procedure TCodeFrm.Delete1Click(Sender: TObject);
procedure DeleteRecords(Node: TTreeNode);
  var
    RNode: TTreeNode;
  begin
    RNode := Node.GetNext;
    while (RNode <> nil) and (RNode.Level > Node.Level) do
    begin
     {$Warnings Off}
     {$Notes Off}
      if CodeDB.Locate('Key', Integer(RNode.Data), []) then  // do not localize
        CodeDB.Delete;
     {$Notes On}
     {$Warnings On}
      RNode := RNode.GetNext;
    end;
  end;

  var
    NodeType: string;
begin
  try
    if tvTopics.Selected = nil then Exit;
    {$Warnings Off}
    {$Notes Off}
    if CodeDB.Locate('Key', Integer(tvTopics.Selected.Data), []) then  // do not localize
    {$Notes On}
    {$Warnings On}
    begin
      if tvTopics.Selected.ImageIndex = 2 then // Code snippet
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

procedure TCodeFrm.CodeTextStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  tbCut.Enabled := CodeText.SelAvail;
  tbCopy.Enabled := CodeText.SelAvail;
  tbPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not CodeText.ReadOnly));
  Modified := CodeText.Modified;
end;

procedure TCodeFrm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TCodeFrm.FindNext1Click(Sender: TObject);
begin
  if FSearch.Text <> '' then
    DoSearch(False);
end;

procedure TCodeFrm.FormHide(Sender: TObject);
begin
  if FModified then SaveRecord;
  CodeDB.Close;
end;

procedure TCodeFrm.FormShow(Sender: TObject);
begin
  if (CodeDB <> nil) and not CodeDB.Active then CodeDB.Open;
end;

procedure TCodeFrm.mitMakeRootClick(Sender: TObject);
begin
 with tvTopics do
  if ((Selected <> nil) and (Selected.Level > 0) and (not (Selected.ImageIndex = 2))) then
  begin
    CodeDB.Edit;
    CodeDB.FieldByName('Parent').AsInteger := 0;  // do not localize
    tvTopics.Selected.MoveTo(nil, naAdd);
    Modified := True;
    SortNodes;
  end;
end;

procedure TCodeFrm.New1Click(Sender: TObject);
begin
  if tvTopics.Selected = nil then
  begin
    Snippet1.Enabled := False;
    mitNewSnippet.Enabled := False;
  end
  else
  if tvTopics.Selected.ImageIndex = 2 then // Code snippet
  begin
    Snippet1.Enabled := False;
    mitNewSnippet.Enabled := False;
    Folder1.Enabled := False;
    mitNewFolder.Enabled := False;
  end
  else // in every other case...
  begin
    Snippet1.Enabled := True;
    mitNewSnippet.Enabled := True;
    Folder1.Enabled := True;
    mitNewFolder.Enabled := True;
  end;
end;

procedure TCodeFrm.pmCodePopup(Sender: TObject);
begin
    mitCut.Enabled := CodeText.SelAvail;
    mitCopy.Enabled := CodeText.SelAvail;
    mitPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not CodeText.ReadOnly));
end;

procedure TCodeFrm.RootFolder1Click(Sender: TObject);
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

procedure TCodeFrm.Snippet1Click(Sender: TObject);
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

procedure TCodeFrm.tbContractClick(Sender: TObject);
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


procedure TCodeFrm.tbCopyClick(Sender: TObject);
begin
    try
    CodeText.CopyToClipboard;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TCodeFrm.tbCutClick(Sender: TObject);
begin
  try
    CodeText.CutToClipboard;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TCodeFrm.tbExpandClick(Sender: TObject);
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

procedure TCodeFrm.tbFindClick(Sender: TObject);
begin
   try
    with TfmCodeSearch.Create(nil) do
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
      ShowMessage(E.Message);
  end;
end;

procedure TCodeFrm.tbPasteClick(Sender: TObject);
begin
  try
    if not CodeText.ReadOnly then
      CodeText.PasteFromClipBoard
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TCodeFrm.tvTopicsChange(Sender: TObject; Node: TTreeNode);
begin
  try
    if (Node <> nil) and (Node.ImageIndex = 2) then // Code snippet
    begin
      tbFolder.Enabled := False;
      tbOpen.Enabled := False;
      {$Warnings Off}
      {$Notes Off}
      if CodeDB.Locate('Key', Integer(Node.Data), []) then  // do not localize
      {$Warnings On}
      {$Notes On}
      begin
        CodeText.Lines.BeginUpdate;
        try
          case CodeDB.FieldByName('Language').AsString of
          'NONE': begin
               CodeText.Highlighter:=nil;
               mitNone.Checked:=true;
               end;
          'PAS': begin
               CodeText.Highlighter:=SynPas;
               mitPascal.Checked:=true;
               end;
          'CPP': begin
               CodeText.Highlighter:=SynCpp;
               mitCPP.Checked:=true;
               end;
          'HTML': begin
               CodeText.Highlighter:=SynHTML;
               mitHTML.Checked:=true;
               end;
          'SQL': begin
               CodeText.Highlighter:=SynSQL;
               mitSQL.Checked:=true;
               end;
          end;
          CodeText.Text:=CodeDB.FieldByName('Code').AsString;
        finally
          CodeText.Lines.EndUpdate;
       end;
      end;
     CodeText.ReadOnly := False;
     CodeText.Enabled:=True;
    end
    else
    begin
      tbFolder.Enabled := True;
      tbOpen.Enabled := True;
      CodeText.Lines.Clear;
      CodeText.ReadOnly := True;
      CodeText.Enabled:=False;
    end;
    Modified := False;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TCodeFrm.tvTopicsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
   if (tvTopics.Selected <> nil) and Modified then
    SaveRecord;// Do not alter value of AllowChange
end;

procedure TCodeFrm.SetLanguage(Sender: TObject);
begin
  Modified := True;
   if Sender = mitNone then CodeText.Highlighter:=nil;
   if Sender = mitPascal then  CodeText.Highlighter:=SynPas;
   if Sender = mitCPP then CodeText.Highlighter:=SynCpp;
   if Sender = mitHTML then CodeText.Highlighter:=SynHTML;
   if Sender = mitSQL then CodeText.Highlighter:=SynSQL;
end;

procedure TCodeFrm.tvTopicsCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
   if Node.Selected then
    Sender.Canvas.Font.Style := [fsBold]
  else
    Sender.Canvas.Font.Style := [];
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
    if Node.ImageIndex = 2 then // Code snippet
    begin
      MessageDlg(SCannotAttach, mtError, [mbOK], 0);
      Exit;
    end;
    {$Warnings Off}
    {$Notes Off}
    if CodeDB.Locate('Key', Integer(tvTopics.Selected.Data), []) then  // do not localize
    begin
      CodeDB.Edit;
      CodeDB.FieldByName('Parent').AsInteger := Integer(Node.Data);  // do not localize
      CodeDB.Post;
      tvTopics.Selected.MoveTo(Node, naAddChild);
    end;
    {$Notes on}
    {$Warnings On}
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

procedure TCodeFrm.tvTopicsEdited(Sender: TObject; Node: TTreeNode; var S: string
  );
begin
   Modified := True;
  SortNodes;
end;

procedure TCodeFrm.SetModified(AValue: Boolean);
begin
  if FModified=AValue then Exit;
  FModified:=AValue;
end;

function TCodeFrm.CreateNewDB(DBFileName: string): TBufDataset;
begin
 {$Warnings Off} Result := TBufDataset.Create(self) {$Warnings On};
  try
    with Result do
    begin
     Filename := DBFileName;
     FieldDefs.Add('Key', ftAutoInc, 0, False);
     FieldDefs.Add('Type', ftString, 1, True);
     FieldDefs.Add('Parent', ftInteger, 0, False);
     FieldDefs.Add('System', ftInteger, 0, False);
     FieldDefs.Add('Topic', ftString, 50, False);
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

function TCodeFrm.OpenDB(DBFileName: string): TBufDataset;
begin
  {$Warnings Off}
  Result := TBufDataset.Create(Self);
  {$Warnings On}
  with Result do
  begin
    Filename:=DBFileName;
    try
      Open;
      IndexFieldNames := 'Parent';
     //  IndexName := 'Parent';  // do not localize
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

procedure TCodeFrm.InitializeTreeView;
 procedure LoadTreeView(Node: TTreeNode; Parent: Integer);
  var
    BookMark: TBookMark;
    RNode: TTreeNode;
  begin
    BookMark := CodeDB.GetBookMark;
    try
     {$Notes Off}
     if CodeDB.Locate('Parent', Parent, []) then
     {$Notes On} // do not localize
        while (not CodeDB.EOF) and (CodeDB.FieldByName('Parent').AsInteger = Parent) do
        begin
        {$Warnings Off}
          RNode := tvTopics.Items.AddChildObject(Node, CodeDB.FieldByName('Topic').AsString,
         Pointer(CodeDB.FieldByName('Key').AsInteger));
        {$Warnings On}
          if CodeDB.FieldByName('Type').AsString = 'F' then // do not localize
          begin
            RNode.ImageIndex := 0; // Closed folder
            RNode.SelectedIndex := 1; // Open folder
            LoadTreeView(RNode, CodeDB.FieldByName('Key').AsInteger); // do not localize
          end
          else
          begin
            RNode.ImageIndex := 2; // Code snippet
            RNode.SelectedIndex := 2;
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
    while (not CodeDB.EOF) and (CodeDB.FieldByName('Parent').AsInteger = 0) do
    begin
      {$Warnings Off}
      Node := tvTopics.Items.AddObject(nil, CodeDB.FieldByName('Topic').AsString,
      Pointer(CodeDB.FieldByName('Key').AsInteger));
      {$Warnings On}
      Node.ImageIndex := 0; // Closed folder
      Node.SelectedIndex := 1;
      LoadTreeView(Node, CodeDB.FieldByName('Key').AsInteger);
      CodeDB.Next;
    end;
  finally
    tvTopics.SortType := stText;
    tvTopics.Items.EndUpdate;
  end;

end;

procedure TCodeFrm.SortNodes;
begin
   // Is there a better way?
  tvTopics.SortType := stNone;
  tvTopics.SortType := stText;
end;

procedure TCodeFrm.SaveRecord;
// Do not localize any of the following lines
begin
 try
   Modified := False;
   if not CodeDB.Active then Exit;
   if tvTopics.Selected = nil then Exit;
  {$Warnings off}
  {$Notes Off}
  if CodeDB.Locate('Key', Integer(tvTopics.Selected.Data), [])
  {$Notes On}
  {$Warnings on} then
   begin
     CodeDB.Edit;
     CodeDB.FieldByName('Topic').AsString := tvTopics.Selected.Text;
     if tvTopics.Selected.ImageIndex = 2 then  // Code snippet
     begin
       CodeDB.FieldByName('Code').AsString:=CodeText.Text;
       if mitPascal.Checked then
         CodeDB.FieldByName('Language').AsString := 'PAS'
       else
       if mitCPP.Checked then
         CodeDB.FieldByName('Language').AsString := 'CPP'
       else
       if mitHTML.Checked then
         CodeDB.FieldByName('Language').AsString := 'HTML'
       else
       if mitSQL.Checked then
         CodeDB.FieldByName('Language').AsString := 'SQL'
       else
         CodeDB.FieldByName('Language').AsString := 'NONE'
     end;
     CodeDB.Post;
   end;
 except
   on E: Exception do
     ShowMessage(E.Message);
 end;
end;

procedure TCodeFrm.DoSearch(First: Boolean);
function CaseInsensitivePos(const SubString, S: string): Integer;
begin
  Result := Pos(UpperCase(SubString), UpperCase(S));
end;
function DoMatch(const Text: string): Integer;
var
  MatchPos: Integer;
begin
  Result := -1;
  if FSearch.CaseSensitive then
    MatchPos := Pos(FSearch.Text, Text)
  else
    MatchPos := CaseInsensitivePos(FSearch.Text, Text);
  if (MatchPos > 0) and FSearch.WholeWord then
  begin
    // If the previous character is alphabetic, there isn't a match
    if MatchPos > 1 then
      if UpCase(Text[MatchPos - 1]) in ['A'..'Z'] then
        Exit;
    // If the next character is alphabetic, we didn't find a word match
    if MatchPos + Length(FSearch.Text) <= Length(Text) then
      if UpCase(Text[MatchPos + Length(FSearch.Text)]) in ['A'..'Z'] then
        Exit;
    Result := MatchPos;
  end
  else
    Result := MatchPos;
end;

function GetByteSelStart: Integer;
begin
  Result := CodeText.SelStart;
end;

var
Node: TTreeNode;
Match: Integer;
InTopic: Boolean;
FirstLoop: Boolean;
opts:TSynSearchOptions;
begin
try
  Screen.Cursor := crHourGlass;
  try
    Node := nil;
    if not First then
    begin
        Node := tvTopics.Selected
    end;
    if First or (Node = nil) then
      Node := tvTopics.Items.GetFirstNode;
    Match := 0;
    InTopic := False;
    FirstLoop := True;
    while Node <> nil do
    begin
      {$Warnings Off}
      {$Notes Off}
      if CodeDB.Locate('Key', Integer(Node.Data), []) then
      {$Notes On}
      {$Warnings On}
      begin
        if FirstLoop and (ActiveControl = CodeText) and (CodeText.SelAvail) then
        begin
          InTopic := False;
          Match := DoMatch(Copy(CodeDB.FieldByName('Code').AsString, GetByteSelStart + (CodeText.SelEnd - CodeText.SelStart) , 999999));
          if Match > 0 then
          begin
            Match := Match + GetByteSelStart + (CodeText.SelEnd - CodeText.SelStart) - 1;
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
    if Node = nil then SysUtils.Beep;
    if Match > 0 then
    begin
      tvTopics.Selected := Node;
      if InTopic then tvTopics.SetFocus
      else
      begin
        CodeText.Enabled:=true;
        CodeText.SetFocus;
        opts:=[ssoFindContinue];
        CodeText.SearchReplace(FSearch.Text,FSearch.Text,opts);
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

function TCodeFrm.AddFolder(Node: TTreeNode; Desc: string): TTreeNode;
var
  NNode: TTreeNode;
begin
  Result := nil;
  try
    with CodeDB do
    begin
      Insert;
      {$Warnings Off}
      if Node <> nil then
       FieldByName('Parent').AsInteger := Integer(Node.Data)// do not localize
      else
      {$Warnings On}
      FieldByName('Parent').AsInteger := 0;  // do not localize
      FieldByName('Topic').AsString := Desc; // do not localize
      FieldByname('Type').AsString := 'F';  // do not localize
      Post;
    end;
    NNode := tvTopics.Items.AddChildObject(Node, Desc,
    {$Warnings Off}
    Pointer(CodeDB.FieldByName('Key').AsInteger));
    {$Warnings On}
    NNode.ImageIndex := 0; // Closed folder
    NNode.SelectedIndex := 1; // Open Folder
    Result := NNode;
    SortNodes;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

function TCodeFrm.AddCode(Desc: string): TTreeNode;
var
  Node: TTreeNode;
begin
  Result := nil;
  try
    if tvTopics.Selected = nil then Exit;
    with CodeDB do
    begin
      Insert;
      {$Warnings Off}
      FieldByName('Parent').AsInteger := Integer(tvTopics.Selected.Data);
      {$Warnings On}
      FieldByName('Topic').AsString := Desc;  // do not localize
      FieldByName('Type').AsString := 'C';  // do not localize
      if mitPascal.Checked then
        FieldByName('Language').AsString := 'PAS'  // do not localize
      else
        FieldByName('Language').AsString := 'NONE';  // do not localize
      Post;
    end;
    {$Warnings Off}
    Node := tvTopics.Items.AddChildObject(tvTopics.Selected,Desc,Pointer(CodeDB.FieldByName('Key').AsInteger));
    {$Warnings On}
    Node.ImageIndex := 2; // Code snippet
    Node.SelectedIndex := 2;
    Result := Node;
    SortNodes;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

end.

