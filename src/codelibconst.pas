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

resourcestring
 rsMenuName = 'Code Librarian';
 rsModified = 'Modified';
 rsSnippet = 'snippet';
 rsFolder = 'folder';
 rsConfirmDelete = 'Delete this %s?';
 rsNotForFormFiles = 'Copy/Paste is not allowed in form files.';
 rsCannotAttach = 'Subitems cannot be attached to a code snippet, only folders.';
 rsNewCode = 'New Code';
 rsCouldNotCreateDatabase = 'Could not create database.';
 rsNewFolder = 'New Folder';

 rs_actDelete = 'Delete';
 rs_actNewRootFolder = 'New Root Folder';
 rs_actNewFolder = 'New Folder';
 rs_actNewSnippet = 'New Snippet';
 rs_actMakeRoot = 'Move Folder to Root';
 rs_actPrinterSetup = 'Printer Setup...';
 rs_actPrint = 'Print Snippet';
 rs_actExit = 'Exit';
 rs_actEditCut = 'Cu&t';
 rs_actEditCopy = '&Copy';
 rs_actEditPaste = '&Paste';
 rs_actEditCopyFromIde = 'Copy select text from IDE Editor';
 rs_actEditPasteToIde = 'Paste select text to IDE Editor';
 rs_actEditFind = 'Find...';
 rs_actEditFindNext = 'Find Next';
 rs_actExpandAll = 'Expand All';
 rs_actContractAll = 'Contract All';
 rs_actOptions='Options';
 rs_actSyntaxNone='&None';
 rs_actReadOnly='Read Only';




implementation

end.

