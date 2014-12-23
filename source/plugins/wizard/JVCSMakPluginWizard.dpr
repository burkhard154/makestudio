(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSMakPluginWizard.dpr

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

library JVCSMakPluginWizard;

{$I jedi.inc}
{$I compopt.inc}

uses
  ShareMem,
  ToolsAPI,
  JVCSMakPluginWizardOptions in 'JVCSMakPluginWizardOptions.pas' {JVCSMakePluginWizardOptionsForm},
  JVCSMakPluginWizardCommon in 'JVCSMakPluginWizardCommon.pas',
  JVCSMakPluginWizardMain in 'JVCSMakPluginWizardMain.pas',
  JVCSMakPluginWizardNewCommandOptions in 'JVCSMakPluginWizardNewCommandOptions.pas' {JVCSMakePluginWizardNewCommandForm};

var
  {$IFDEF IDE_SUPPORTS_DELPHI}
  JVCSMakDelphiWin32VCLWizardIndex: Integer = -1;
  {$ENDIF IDE_SUPPORTS_DELPHI}
  {$IFDEF IDE_SUPPORTS_CSHARP}
  JVCSMakCSharpWizardIndex: Integer = -1;
  {$ENDIF IDE_SUPPORTS_CSHARP}

procedure JVCSMakWizardTerminate;
var
  WizardServices: IOTAWizardServices;
begin
  WizardServices := BorlandIDEServices as IOTAWizardServices;
  {$IFDEF IDE_SUPPORTS_DELPHI}
  WizardServices.RemoveWizard(JVCSMakDelphiWin32VCLWizardIndex);
  {$ENDIF IDE_SUPPORTS_DELPHI}
  {$IFDEF IDE_SUPPORTS_CSHARP}
  WizardServices.RemoveWizard(JVCSMakCSharpWizardIndex);
  {$ENDIF IDE_SUPPORTS_CSHARP}
end;

function JVCSMakWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
begin
  WizardServices := BorlandIDEServices as IOTAWizardServices;
  {$IFDEF IDE_SUPPORTS_DELPHI}
  JVCSMakDelphiWin32VCLWizardIndex := WizardServices.AddWizard(TJVCSMakDxWin32VCLPluginWizard.Create);
  {$ENDIF IDE_SUPPORTS_DELPHI}  
  {$IFDEF IDE_SUPPORTS_CSHARP}
  JVCSMakCSharpWizardIndex := WizardServices.AddWizard(TJVCSMakCSharpPluginWizard.Create);
  {$ENDIF IDE_SUPPORTS_CSHARP}
  Terminate := JVCSMakWizardTerminate;
  Result := True;
end;

exports
  JVCSMakWizardInit name WizardEntryPoint;

end.
