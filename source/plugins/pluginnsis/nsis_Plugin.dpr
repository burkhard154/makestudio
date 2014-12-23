(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: NSIS_JMakPlugin.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/09/10  BSchranz  - Plugin created
2005/11/12  USchuster - fixed compilation over makefile

-----------------------------------------------------------------------------*)
library nsis_Plugin;

{$I jedi.inc}

{%Bmp 'nsis.bmp'}

uses
  SysUtils,
  Classes,
  msTLB in '..\..\Framework\msTLB.pas',
  ComServ,
  Forms,
  ActiveX,
  nsis_Actions in 'nsis_Actions.pas' {FormActions},
  nsis_nsisProjectEdit in 'nsis_nsisProjectEdit.pas' {FormEditNSISProjectParams},
  nsis_nsisProjectCommand in 'nsis_nsisProjectCommand.pas',
  nsis_Vars in 'nsis_Vars.pas';

{$E jpl}
{$R *.res}

//:Called after all plugins are loaded and registered
//could be used for initialization purpose
procedure AfterAllPluginsLoaded;
begin
end;

//:Indentifies this DLL-Version
procedure JVCSMAKPlugin; stdcall;
begin
end;

//:Get name of Plugin
procedure GetName(AName: PChar); stdcall;
begin
  StrCopy(AName, PChar(struPluginName));
end;

//:Get author of Plugin
procedure GetAuthor(AName: PChar); stdcall;
begin
  StrCopy(AName, PChar(struPluginAuthor));
end;

//:Get description of Plugin
procedure GetDescription(AName: PChar); stdcall;
begin
  StrCopy(AName, PChar(struPluginHint));
end;

//:List of Required plugins separated by ";"
procedure GetRequiredPlugins(AName: PChar); stdcall;
begin
  StrCopy(AName, '');
end;

//:Register an initialize Plugin
function RegisterPlugin(AJVCSMAKApp: IJApplication): Integer; stdcall;
var
 P: Picture;
begin
  Result := 0;
  jvcsmak := AJVCSMAKApp;
  with jvcsmak do
  begin
    try
      //Create form with actions and ModuleCallback
      FormActions := TFormActions.Create(nil);


      //--- add modules --------------------------------------------------------
      GetPictureFromImageList(FormActions.ImageList1, 2, P);
      //Name=Inno Setup Project; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      //Callback for the Moduletype
      jvcsmak.LogMessage(Application.Exename);
      PluginNSISProjectCallback := TPluginNSISProjectCallback.Create(nil);
      jvcsmak.AddCommandType( struNSISCommandName, '', stCategory, P, '.nsi', -1,
        ICommandCallback(PluginNSISProjectCallback));

      //Credits
      jvcsmak.AddCreditInfo(struPluginName + ' by ' + struPluginAuthor);

      //Additional Info
      jvcsmak.AddAdditionalInfo(struPluginHint);
    except
    end;
  end;
end;

//:UnRegister an finalize Plugin
function UnregisterPlugin:Integer; stdcall;
begin
  Result := 0;
  try
    FormActions.Free;
    //Remember to Destroy your Callbacks here!
    PluginNSISProjectCallback.Free;
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
  //not used yet
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
  JVCSMAKPlugin;

begin
end.
