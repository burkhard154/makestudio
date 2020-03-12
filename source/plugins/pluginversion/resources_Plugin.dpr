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

-----------------------------------------------------------------------------*)
library resources_Plugin;

{$I jedi.inc}

uses
  SysUtils,
  Classes,
  makestudio_TLB in '..\..\framework\makestudio_TLB.pas',
  ComServ,
  Forms,
  ActiveX,
  resources_Actions in 'resources_Actions.pas' {FormActions},
  resources_Edit in 'resources_Edit.pas' {FormEditParams},
  resources_Module in 'resources_Module.pas',
  resources_Vars in 'resources_Vars.pas',
  unitResourceVersionInfo in 'unitResourceVersionInfo.pas',
  unitNTModule in 'unitNTModule.pas',
  unitPEFile in 'unitPEFile.pas',
  unitResFile in 'unitResFile.pas',
  unitResourceDetails in 'unitResourceDetails.pas',
  unitResourceExaminer in 'unitResourceExaminer.pas',
  resourcesEditMultipleVersionResourceEdit in 'resourcesEditMultipleVersionResourceEdit.pas' {FormEditEditMultipleVersionResourceParams},
  resourcesEditMultipleVersionResourceCommand in 'resourcesEditMultipleVersionResourceCommand.pas',
  unitRCFile in 'unitRCFile.pas',
  SimpleRCVersionInfo in 'SimpleRCVersionInfo.pas';

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
      GetPictureFromImageList(FormActions.ImageList1, 1, P);
      MakeStudio.AddMenuAction( FormActions.acCreateRCVersionResource.Name,
       stMenuPath+FormActions.acCreateRCVersionResource.Caption,
       FormActions.acCreateRCVersionResource.Hint,
       P, IActionCallback( FormActions)  );

      //--- add modules --------------------------------------------------------
      GetPictureFromImageList(FormActions.ImageList1, 0, P);
      //Name=Versionsresourcen ndern; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      //Callback for the Moduletype
      MakeStudio.LogMessage(Application.Exename);
      PluginVersionsresourcenndernCallback := TPluginVersionsresourcenndernCallback.Create(nil);
      MakeStudio.AddCommandType(strVersionCommandCaption, '', stCategory, P, '.res;.dcr;.rc', -1,
        ICommandCallback(PluginVersionsresourcenndernCallback));

{      PluginEditMultipleVersionResourceCallback := TPluginEditMultipleVersionResourceCallback.Create(nil);
      MakeStudio.AddCommandType(strMultipleVersionCommandCaption, '', stCategory, P, '.res;.dcr', -1,
        ICommandCallback(PluginEditMultipleVersionResourceCallback));}

      //Credits
      MakeStudio.AddCreditInfo(struPluginName + ' by ' + struPluginAuthor);

      //Additional Info
      MakeStudio.AddAdditionalInfo(struPluginHint);
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
    PluginVersionsresourcenndernCallback.Free;
    //PluginEditMultipleVersionResourceCallback.Free;
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
