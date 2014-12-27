(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EasyBackup_Vars.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/05  BSchranz  - Easybackup Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit EasyBackup_Vars;

{$I jedi.inc}

interface

uses
  makestudio_TLB, Windows;

var
  MakeStudio: IJApplication;
  Canceled: Boolean = False;


resourcestring
  struPluginName = 'EasyBackup Installer Plugin';
  struPluginAuthor = 'Burkhard Schranz (burkhard.schranz@optimeas.de)';
  struPluginHint = 'Plugin for Easy Backup';
  stdCategory = 'File operations';

resourcestring
  stdBreak = '****************************************************************';

resourcestring
  stdEasyBackupCaption      = 'Create Mirror Directory';
  stdTargesDir              = 'Target directory:';
  stdSourceDirs             = 'Source directory';
  stdStartingBatch          = 'Start backup...';
  stdMovingFiles            = 'Copying files...';

const
  stdcTargetDir             = 'TargetDir';
  stdcSourceCount           = 'SourceCount';
  stdcSourceDir             = 'SourceDir%d';

  stdcBackupDelete          = 'BackupDelete';
  stdcBackupCopy            = 'BackupCopy';
  stdcBackupIgnore          = 'BackupIgnore';
  stdcBackupCopyAge         = 'BackupCopyAge';
  stdcBackupShowSelection   = 'BackupShowSelection';
  stdcCopyOption            = 'CopyOption';
  stdcLogbookOption         = 'LogbookOption';
  stdcBackupCopyAgeValue    = 'BackupCopyAgeValue';

  iDefaultIndent            = 2;

resourcestring
  strDelete = 'Delete';
  strCopy = 'Copy';
  strIgnore = 'Ignore';
  strError = 'Error';
  strUnknown = '???';
  strErrorCopyingFile = '%s cannot be copied';
  strErrorDeletingFile = '%s cannot be deleted';
  strErrorCreatingDirectory = 'Directory %s cannot be created';
  strFileCopied = '%s copied';
  strFileDeleted = '%s deleted';
  strFileIgnored = '%s ignored';
  strCR = '-----------------------------------------------------------';

  strBackup = 'Backup';
  strSync = 'Synchronization';
  strDeleteSync = 'Missing files';
  strBackupStartet = 'Backup started';
  strSyncStarted = 'Synchronization started';
  strDeleteSyncStart = 'Searching missing files...';
  strShowLoogbook = 'There where errors during the execution - do you want to open the logbook?';
  strExecEnd = 'Execution finished'#10+
               '%d files found in the source directory'#10+
               '%d files copied to the backup directory'#10+
               '%d files found in backup'#10+
               '%d files copied to the source directory'#10+
               '%d missing files found in the source directory'#10+
               '%d created files in the source directory'#10+
               '%d files deleted in backup'#10+
               '%d files ignored in backup'#10+
               '%d errors in total';

implementation

end.
