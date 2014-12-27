(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jmakutils_vars.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/22  JDuenow   - launched EditMkdirModule
2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in
2005/09/09  BSchranz  - translated to english
2005/09/10  BSchranz  - Added interal batch (instead of external file)

-----------------------------------------------------------------------------*)

unit utils_Vars;

{$I jedi.inc}

interface

uses
  makestudio_TLB;

var
  MakeStudio: IJApplication;
  Canceled: Boolean = False;


resourcestring
  struPluginName = 'Utility Functions';
  struPluginAuthor = 'Jeremy Dünow (jeremy.duenow@optimeas.de)';
  struPluginHint = 'Utility Plugin for MakeStudio';

resourcestring
  stdMkdirCaption     = 'Create directories';
  stdDirectories      = 'Directories';
  stdFiles            = 'Files';

  stdErrNoProjects    = 'No directories available';
  stdForceDirectories = 'Creating directories...';
  stdMore             = '<more...>';

//  stdCopyFile = '
  stdCopyFilesCaption = 'Copying files';
  stdBatchFileCaption = 'External Batch file';
  stdBatchFileInternalCaption = 'Internal Batch command list';
  stdStartingBatch    = 'Starting batch file';
  stdErrorCopying     = 'Error copying!';
  stderrRunningBatch  = 'Error executing batch file!';
  stdCategoryFile     = 'File Operations';
  stdCategorySystem   = 'System';

const
  stdcProjectCount = 'DirectoryCount';
  stdcFileCount = 'FileCount';
  stdcBatchfile = 'BatchFile';
  stdcBatchLine = 'BatchLine';
  stdcBatchLineCount = 'BatchLineCount';
  stdcReplaceVars = 'ReplaceVars';
  stdcProjects = 'Directory%d';
  stdcSource = 'Source%d';
  stdcTarget = 'Target%d';
  stExtBat = '.bat';
  stdcInternalBatchFilename = '$$Interal.bat';


implementation

end.
