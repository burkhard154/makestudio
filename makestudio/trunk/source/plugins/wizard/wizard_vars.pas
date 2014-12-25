(* -----------------------------------------------------------------------------
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

  2011/09/25  BSchranz  - created

  ----------------------------------------------------------------------------- *)
unit wizard_vars;

interface

uses
  msTLB;

var
  MakeStudio: IJApplication;
  FCanceled: Boolean = False;

resourcestring
  struPluginName = 'Code Wizard';
  struPluginAuthor = 'Burkhard Schranz (burkhard.schranz@optimeas.de)';
  struPluginHint = 'Wizard to create Plugins';
  stCategory = 'System';
  strMenuPath = '&Extra\Code &Wizard';

const
  // Filenames of the templates

  // Delphi
  sTemplatePrefix = 'template_';
  sPluginDpr = 'plugin.dpr';
  sActionTestPas = 'actiontest.pas';
  sActionTestDfm = 'actiontest.dfm';
  sActionsPas = 'actions.pas';
  sActionsDfm = 'actions.dfm';
  sEditPas = 'edit.pas';
  sEditDfm = 'edit.dfm';
  sModulePas = 'module.pas';
  sVarsPas = 'vars.pas';

  // C#
  sTemplatePrefixCs = 'template_cs_';
  sPluginCsproj = 'plugin.csproj';
  sPluginCs = 'plugin.cs';
  sActionTestCs = 'actiontest.cs';
  sActionTestresx = 'actiontest.resx';
  sActionsCs = 'actions.cs';
  sActionsresx = 'actions.resx';
  sEditCs = 'edit.cs';
  sEditresx = 'edit.resx';
  sModuleCs = 'command.cs';

  // ressource ID's
  sRESID_CS_ASSEMBLYINFO = 'CS_ASSEMBLYINFO';
  sRESID_CS_PUGIN = 'CS_PUGIN';
  sRESID_CSPRPOJ_PUGIN = 'CSPROJ_PUGIN';
  sRESID_CS_COMMAND = 'CS_COMMAND';
  sRESID_CS_EDIT = 'CS_EDIT';
  sRESID_RESX_EDIT = 'RESX_EDIT';
  sRESID_CS_ACTIONS = 'CS_ACTIONS';
  sRESID_RESX_ACTIONS = 'RESX_ACTIONS';
  sRESID_CS_ACTIONTEST = 'CS_ACTIONTEST';
  sRESID_RESX_ACTIONTEST = 'RESX_ACTIONTEST';

  sRESID_DPR_PLUGIN = 'DPR_PLUGIN';
  sRESID_PAS_ACTIONS = 'PAS_ACTIONS';
  sRESID_DFM_ACTIONS = 'DFM_ACTIONS';
  sRESID_PAS_ACTIONTEST = 'PAS_ACTIONTEST';
  sRESID_DFM_ACTIONTEST = 'DFM_ACTIONTEST';
  sRESID_PAS_EDIT = 'PAS_EDIT';
  sRESID_DFM_EDIT = 'DFM_EDIT';
  sRESID_PAS_MODULE = 'PAS_MODULE';
  sRESID_PAS_VARS = 'PAS_VARS';

implementation

end.
