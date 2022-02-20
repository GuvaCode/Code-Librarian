unit codelibconst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
ConfigurationSection = 'CodeLib';
ClosedFolderImageIndex = 0;
OpenFolderImageIndex   = 17;
CodeSnippetImageIndex = 18;
DefaultDBFileName = 'codelibrarian.dat';
ExportTextFilter  =  'Text Files (*.txt)|*.txt';

resourcestring
 rsMenuName = 'Code Librarian';
 rsModified = 'Modified';
 rsSnippet = 'snippet';
 rsFolder = 'folder';
 rsConfirmDelete = 'Delete this %s ?';
 rsNotForFormFiles = 'Copy/Paste is not allowed in form files.';
 rsCannotAttach = 'Subitems cannot be attached to a code snippet, only folders.';
 rsNewCode = 'New code';
 rsCouldNotCreateDatabase = 'Could not create database.';

 rs_actDelete = 'Delete';
 rs_actNewRootFolder = 'New root folder';
 rs_actNewFolder = 'New folder';
 rs_actNewSnippet = 'New snippet';
 rs_actMakeRoot = 'Move folder to root';
 rs_actPrinterSetup = 'Printer setup...';
 rs_actPrint = 'Print snippet';
 rs_actExit = 'Exit';
 rs_actEditCut = 'Cu&t';
 rs_actEditCopy = '&Copy';
 rs_actEditPaste = '&Paste';
 rs_actEditCopyFromIde = 'Copy select text from IDE editor';
 rs_actEditPasteToIde = 'Paste select text to IDE editor';
 rs_actEditFind = 'Find...';
 rs_actEditFindNext = 'Find next';
 rs_actEditUndo ='Undo';
 rs_actEditRedo ='Redo';
 rs_actEditRename='Rename';
 rs_actExpandAll = 'Expand all';
 rs_actContractAll = 'Сollapse all';
 rs_actOptions='Options';
 rs_actSyntaxNone='&None';
 rs_actReadOnly='Read only';
 rs_actSetBackgoundColor='Set editor backgound color';

 rs_actSaveAsHtml='Export snippet as html page';
 rs_actSaveAsTXT ='Export snippet as text file';

 rs_actImportOneFileTxt ='Import one Text file';
 rs_actImportSeveralFilesTxt = 'Import several Text files';

 rs_mitFile='File';
 rs_mitFileNew='&New';
 rs_mitExport='Export';
 rs_mitImport='Import';

 rs_mitEdit='&Edit';
 rs_mitOptions='&Options';

 rs_mitEditorHighlighting='Syntax &highlighting';

 // Find
  rs_caption='Find ...';

  rs_cbCaseSensitive = 'Case sensitive';
  rs_cbWholeWord = 'Whole words only';
  rs_btnOk='Find';
  rs_btnCancel='Cancel';

// Options
  rs_optlbl='Select data base path ...';
  rs_optcaption='Options ...';
  rs_optbackgoundcolor = 'Editor backgound color';
  rs_savesizeposition = 'Save window size and position';

// Add\import code
  rs_noFolderSelected='Please, select an importing folder.';
  rs_noFileFoundInImportingFolder='No *.txt file found in the importing folder.';
  rs_error='Error';
implementation

end.
