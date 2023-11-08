(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsplugintemplate_Vars.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/08  BSchranz  - Plugin template created
2005/02/15  USchuster - preparations for check in and modified for Wizard

-----------------------------------------------------------------------------*)
unit innosetup_Vars;

interface

uses
  makestudio_TLB;

var
  MakeStudio: IJApplication;
  FCanceled: Boolean = False;

resourcestring
  struPluginName = 'Inno Setup Plugin';
  struPluginAuthor = 'Burkhard Schranz (burkhard.schranz@optimeas.de)';
  struPluginHint = 'Plugin for Inno Setup Projects';
  stCategory = 'Installation';

resourcestring
  strInnoSetupProject = 'Inno Setup <%s>';
  strProjectFile='Project file';
  strOutputfile='Output file';
  strOptions='Options';
  strPreviewText = 'Output file: "%s"' + #10#13 +
                   'Options: "%s"';
  strExecutingProject = 'Executing Inno Setup Project...';
  strErrorCompilerNotFound = 'Inno Setup Compiler not found!';

const
  sProjectFile='ProjectFile';
  sOutputfile='OutputFile';
  sOptions='Options';

  // stdcRegKey = 'Software\JEDI\MakeStudio';
  stdcRegISCCExe = 'Iscc.exe';
  stdcPathIscc = 'Inno Setup 6';

implementation

end.
 
