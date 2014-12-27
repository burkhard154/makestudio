(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MakeStudioplugintemplate.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2006/04/21  BSchranz  - Plugin ZIP created

-----------------------------------------------------------------------------*)
library zip_Plugin;

{$R 'zip_Resource.res' 'zip_Resource.rc'}
{$R *.dres}

uses
  SysUtils,
  Classes,
  ComServ,
  Forms,
  ZipMstr,
  ActiveX,
  makestudio_TLB in '..\..\framework\makestudio_TLB.pas',
  zip_Actions in 'zip_Actions.pas' {FormActions},
  zip_CommandEdit in 'zip_CommandEdit.pas' {FormEditZipCommand},
  zip_Command in 'zip_Command.pas',
  zip_Vars in 'zip_Vars.pas',
  zip_Utils in 'zip_Utils.pas';

{$E jpl}
{$R *.res}

//:Called after all plugins are loaded and registered
//could be used for initialization purpose
procedure AfterAllPluginsLoaded;
begin
end;

//:Indentifies this DLL-Version
procedure MakeStudioPlugin; stdcall;
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
function RegisterPlugin(AMakeStudioApp: IJApplication): Integer; stdcall;
var
 P: Picture;
begin
  Result := 0;
  MakeStudio := AMakeStudioApp;
  with MakeStudio do
  begin
    try
      //Create form with actions and ModuleCallback
      FormActions := TFormActions.Create(nil);


      //--- add modules --------------------------------------------------------
      GetPictureFromImageList(FormActions.ImageList1, 3, P);
      //Name=WinZipCommand; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      //Callback for the Moduletype
      MakeStudio.LogMessage(Application.Exename);
      PluginWinZipCommandCallback := TPluginZipCommandCallback.Create(nil);
      MakeStudio.AddCommandType( stCommandname, '', stCategory, P, 'txt', -1,
        ICommandCallback(PluginWinZipCommandCallback));

      //Credits
      MakeStudio.AddCreditInfo(struPluginName + ' by ' + struPluginAuthor);
      MakeStudio.AddCreditInfo( strZIPCredits);

      //Additional Info
      MakeStudio.AddAdditionalInfo(struPluginHint);

      //Create ZIPMaster
      ZipMaster := TZipMaster.Create( nil);
      ZipMaster.Unattended := true;
      MakeStudio.AddAdditionalInfo( Format( strZIPMasterVersion, [ZipMaster.Dll_Version]));
      MakeStudio.AddAdditionalInfo( Format( strZIPMasterPath, [ZipMaster.Dll_Path]));
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
    PluginWinZipCommandCallback.Free;

    //Destroy ZipMaster
    ZipMaster.Free;
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
  MakeStudioPlugin;

begin
end.
