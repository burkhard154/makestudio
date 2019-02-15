library msbuild_plugin;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  SysUtils,
  Classes,
  ComServ,
  Forms,
  PluginMsBuildvars in 'PluginMsBuildvars.pas',
  PluginMsBuildmodule in 'PluginMsBuildmodule.pas',
  PluginMsBuildedit in 'PluginMsBuildedit.pas' {FormEditParams},
  PluginMsBuildactions in 'PluginMsBuildactions.pas' {FormActions},
  PluginMsBuildactiontest in 'PluginMsBuildactiontest.pas' {FormActionTest},
  ActiveX,
  makestudio_TLB in '..\..\framework\makestudio_TLB.pas';

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
      //Create form with actions
      FormActions := TFormActions.Create(nil);

      //--- add actions --------------------------------------------------------
      GetPictureFromImageList(FormActions.ImageList1, FormActions.acSettings.ImageIndex, P);
      //if the Caption has "\" - the action is assigned to this main menu path!
      //e.g. 'Testmenu\test\'+FormActions.acTestaction1.Caption...
      //if not, the action is assigned to the "extras" menu item
      MakeStudio.AddMenuAction(FormActions.acSettings.Name,
                             'Extra\.NET\' + FormActions.acSettings.Caption,
                             FormActions.acSettings.Hint,
                             P,
                             IActionCallback(FormActions));

      //--- add modules --------------------------------------------------------
      GetPictureFromImageList(FormActions.ImageList1, 2, P);
      //Name=Testcommand; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      //Callback for the Moduletype
      MakeStudio.LogMessage(Application.Exename);
      PluginTestcommandCallback := TPluginTestcommandCallback.Create(nil);
      MakeStudio.AddCommandType(struPluginName, struPluginHint, stCategory, P, 'sln', -1,
        ICommandCallback(PluginTestcommandCallback));

      //Credits
      MakeStudio.AddCreditInfo(struPluginName + ' by ' + struPluginAuthor);

      //Additional Info
      MakeStudio.AddAdditionalInfo(struPluginHint);
      LoadFromRegistry;
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
    PluginTestcommandCallback.Free;
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
