(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: resources_MakeStudioPlugin.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/08/13  BSchranz  - Plugin created
2005/08/16  USchuster - fixed compilation over makefile
2005/09/09  BSchranz  - translation bug fixed
2005/11/12  USchuster - reintegrated changes from revision 0.2(2005/08/16)
2006/04/11  BSchranz  - Version also set to "FileVersion" Key
2006/08/05  FBrams    - SetToVersion included;
                        created command to handle multiple Files 

-----------------------------------------------------------------------------*)
unit resources_Vars;

interface

uses
  msTLB;

var
  MakeStudio: IJApplication;
  FCanceled: Boolean = False;

resourcestring
  struPluginName = 'Edit Resources Plugin';
  struPluginAuthor = 'Burkhard Schranz (burkhard.schranz@optimeas.de)';
  struPluginHint = 'Plugin for editing Version resources';
  stCategory = 'Compiler\Resources';
  strVersionCommandCaption = 'Edit Version Resource';
  strMultipleVersionCommandCaption = 'Edit Multiple Version Resource';

  stMainVersion = 'Major version';
  stReleaseVersion = 'Minor version';
  stIssueVersion = 'Release';
  stBuildVersion = 'Build';

  stSetTo = 'Set to:';
  stWrongFiletype = 'Unknown Resource type - only .res or .dcr are possible!';
  stErrorLoadingFile = 'Error loading resource file "%s"';
  stErrorSavingFile = 'Error saving resource file "%s"';
  stVersionResourceNotFound = 'No version information found in "%s"!';
  stProceedingWin32VersionResource = 'Proceeding WIN32 Versionsresource';
  stProceedingFile = 'File: %s';
  stLanguage = 'Language: %s';
  stVersionModified = 'Version changed from %d.%d.%d.%d to %d.%d.%d.%d..';
  stLanguageNutral = 'Language Nutral';
  stVersionKeyNotFound = 'Error: Version Key "%s" not found!';

  stIncreaseVersion = 'Increase Version (%s)';
  stSetVersion = 'Set Version to %d.%d.%d.%d';
  stModuleName = 'in module: %s';
  stMenuPath = 'Extra\Version\';

const
  stdcFilename = 'Filename';
  stdcIncreaseType = 'IncreaseType';
  stdcFileVersionKey = 'FileVersionKey';
  stdcIncreaseSet = 'IncreaseSet';
  stdcNewVersionValue = 'NewVersionValue';
  stdcRecursive = 'Recursive';

  iDefaultIndent = 2;

implementation

end.
