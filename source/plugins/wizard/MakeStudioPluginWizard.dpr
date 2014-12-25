(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MakeStudioPluginWizard.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/02/15  USchuster - new project
2005/03/09  USchuster - changes for C# Builder wizard

-----------------------------------------------------------------------------*)

library MakeStudioPluginWizard;

{$I jedi.inc}
{$I compopt.inc}

uses
  ShareMem,
  ToolsAPI,
  msPluginWizardOptions in 'msPluginWizardOptions.pas' {msPluginWizardOptionsForm},
  msPluginWizardCommon in 'msPluginWizardCommon.pas',
  MakeStudioPluginWizardMain in 'MakeStudioPluginWizardMain.pas',
  msPluginWizardNewCommandOptions in 'msPluginWizardNewCommandOptions.pas' {msPluginWizardNewCommandForm};

var
  {$IFDEF IDE_SUPPORTS_DELPHI}
  MakeStudioDelphiWin32VCLWizardIndex: Integer = -1;
  {$ENDIF IDE_SUPPORTS_DELPHI}
  {$IFDEF IDE_SUPPORTS_CSHARP}
  MakeStudioCSharpWizardIndex: Integer = -1;
  {$ENDIF IDE_SUPPORTS_CSHARP}

procedure MakeStudioWizardTerminate;
var
  WizardServices: IOTAWizardServices;
begin
  WizardServices := BorlandIDEServices as IOTAWizardServices;
  {$IFDEF IDE_SUPPORTS_DELPHI}
  WizardServices.RemoveWizard(MakeStudioDelphiWin32VCLWizardIndex);
  {$ENDIF IDE_SUPPORTS_DELPHI}
  {$IFDEF IDE_SUPPORTS_CSHARP}
  WizardServices.RemoveWizard(MakeStudioCSharpWizardIndex);
  {$ENDIF IDE_SUPPORTS_CSHARP}
end;

function MakeStudioWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
begin
  WizardServices := BorlandIDEServices as IOTAWizardServices;
  {$IFDEF IDE_SUPPORTS_DELPHI}
  MakeStudioDelphiWin32VCLWizardIndex := WizardServices.AddWizard(TMakeStudioDxWin32VCLPluginWizard.Create);
  {$ENDIF IDE_SUPPORTS_DELPHI}
  {$IFDEF IDE_SUPPORTS_CSHARP}
  MakeStudioCSharpWizardIndex := WizardServices.AddWizard(TMakeStudioCSharpPluginWizard.Create);
  {$ENDIF IDE_SUPPORTS_CSHARP}
  Terminate := MakeStudioWizardTerminate;
  Result := True;
end;

exports
  MakeStudioWizardInit name WizardEntryPoint;

end.
