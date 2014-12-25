(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: delphi32.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

library CodeWizard;

{$I jedi.inc}

{$R 'plugintemplate.res' 'resources\plugintemplate.rc'}
{$R 'plugintemplate_csharp.res' 'resources\plugintemplate_csharp.rc'}
{$R 'plugintemplate_delphi.res' 'resources\plugintemplate_delphi.rc'}

uses
  SysUtils,
  Classes,
  msTLB in '..\..\framework\msTLB.pas',
  ComServ,
  ActiveX,
  Forms,
  wizard_actions in 'wizard_actions.pas' {Form3},
  wizard_vars in 'wizard_vars.pas',
  wizard_parser in 'wizard_parser.pas',
  msPluginWizardCommon in 'msPluginWizardCommon.pas',
  msPluginWizardNewCommandOptions in 'msPluginWizardNewCommandOptions.pas' {msPluginWizardNewCommandForm},
  msPluginWizardOptions in 'msPluginWizardOptions.pas' {msPluginWizardOptionsForm};

{$E jpl}

{$R *.res}

procedure AfterAllPluginsLoaded;
begin
end;

//:Indentifies this DLL-Version
procedure MakeStudioPlugin; stdcall;
begin
end;

//:Get name of Plugin
procedure GetName(aName: PChar); stdcall;
begin
  StrCopy(aName, PChar(struPluginName));
end;

//:Get author of Plugin
procedure GetAuthor(aName: PChar); stdcall;
begin
  StrCopy(aName, PChar(struPluginAuthor));
end;

//:Get description of Plugin
procedure GetDescription(aName: PChar); stdcall;
begin
  StrCopy(aName, PChar(struPluginHint));
end;

//:List of Required plugins separeted by ";"
procedure GetRequiredPlugins(aName: PChar); stdcall;
begin
  StrCopy(aName, '');
end;

//:Register an initialize Plugin
function RegisterPlugin(aMakeStudioApp: IJApplication): Integer; stdcall;
var
  P: Picture;
begin
  Result := 0;
  MakeStudio := aMakeStudioApp;
  with MakeStudio do
  begin
    try
      //Create form with actions and ModuleCallback
      Form3 := TForm3.Create(nil);
      Form3.RegisterActions;
    except
      on E:Exception do
      begin
        MakeStudio.LogMessage('Plugin: ' + Application.ExeName);
        MakeStudio.LogMessage('Exception: ' + E.Message);
      end;
    end;
  end;
end;

//:UnRegister an finalize Plugin
function UnregisterPlugin: Integer; stdcall;
begin
  Result := 0;
  try
    Form3.Free;
  except
  end;
end;

//:Version of plugin
function GetMinorVersion: Integer; stdcall;
begin
  Result := 0;
end;

//:Version of plugin
function GetMajorVersion: Integer; stdcall;
begin
  Result := 1;
end;

//:Return the GUID of the Plugins Options-DLG
function GetOptionsPageGUID: TGUID; stdcall;
begin
  Result := GUID_NULL;
end;

exports
  GetName,
  GetAuthor,
  GetDescription,
  GetRequiredPlugins,
  RegisterPlugin,
  UnregisterPlugin,
  GetMinorVersion,
  GetMajorVersion,
  AfterAllPluginsLoaded,
  GetOptionsPageGUID,
  MakeStudioPlugin;

begin
end.
