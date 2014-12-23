{ -----------------------------------------------------------------------------
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

  ----------------------------------------------------------------------------- }

library delphi32_Plugin;

{$I jedi.inc}

uses
  SysUtils,
  Classes,
  ComServ,
  ActiveX,
  Forms,
  delphi32_Vars in 'delphi32_Vars.pas',
  delphi32_Compile in 'delphi32_Compile.pas',
  delphi32_Actions in 'delphi32_Actions.pas' {Form3},
  delphi32_EditDelphi32Module in 'delphi32_EditDelphi32Module.pas' {FormEditModule},
  delphi32_DelphiPath in 'delphi32_DelphiPath.pas' {FormSearchPathDelphi},
  delphi32_SelectDelphiVersion in 'delphi32_SelectDelphiVersion.pas' {FormSelectDelphiVersion},
  delphi32_Utils in 'delphi32_Utils.pas',
  delphi32_SetDelphiVersionModule in 'delphi32_SetDelphiVersionModule.pas',
  delphi32_BDSEnvironment in 'delphi32_BDSEnvironment.pas' {FormSelectBDSEnvironment},
  delphi32_CompilerSwitch in 'delphi32_CompilerSwitch.pas' {FormCompilerSwitches},
  delphi32_Properties in 'delphi32_Properties.pas' {FormEditDelphi32Globals},
  delphi32_CheckVersionEdit in 'delphi32_CheckVersionEdit.pas' {FormEditDelphi32CheckVersionParams},
  delphi32_CheckVersionCommand in 'delphi32_CheckVersionCommand.pas',
  delphi32_SpecialSettingsEdit in 'delphi32_SpecialSettingsEdit.pas' {FormEditSpecialSettingsParams},
  delphi32_SpecialSettingsCommand in 'delphi32_SpecialSettingsCommand.pas',
  delphi32_brcc32Command in 'delphi32_brcc32Command.pas',
  delphi32_brcc32CommandEdit in 'delphi32_brcc32CommandEdit.pas' {FormEditbrcc32Command},
  delphi_EditSelectDelphiPlatform in 'delphi_EditSelectDelphiPlatform.pas' {FormEditSelectDelphiPlatform},
  delphi_SelectDelphiPlatformCommand in 'delphi_SelectDelphiPlatformCommand.pas',
  delphi_EditEditNamespaces in 'delphi_EditEditNamespaces.pas' {FormEditEditNamespaces},
  delphi_EditNamespaces in 'delphi_EditNamespaces.pas',
  delphi_AddComponentFolder in 'delphi_AddComponentFolder.pas' {FormAddComponentFolder},
  msTLB in '..\..\framework\msTLB.pas';

{$E jpl}
{$R *.res}

procedure AfterAllPluginsLoaded;
begin
end;

// :Indentifies this DLL-Version
procedure JVCSMAKPlugin; stdcall;
begin
end;

// :Get name of Plugin
procedure GetName(aName: PChar); stdcall;
begin
  StrCopy(aName, PChar(struPluginName));
end;

// :Get author of Plugin
procedure GetAuthor(aName: PChar); stdcall;
begin
  StrCopy(aName, PChar(struPluginAuthor));
end;

// :Get description of Plugin
procedure GetDescription(aName: PChar); stdcall;
begin
  StrCopy(aName, PChar(struPluginHint));
end;

// :List of Required plugins separeted by ";"
procedure GetRequiredPlugins(aName: PChar); stdcall;
begin
  StrCopy(aName, '');
end;

// :Register an initialize Plugin
function RegisterPlugin(aJVCSMAKApp: IJApplication): Integer; stdcall;
var
  P: Picture;
begin
  Result := 0;
  jvcsmak := aJVCSMAKApp;
  with jvcsmak do
  begin
    try
      // Create form with actions and ModuleCallback
      Form3 := TForm3.Create(nil);

      // add actions
      jvcsmak.AddMenuAction(Form3.acDelphiSearchPath.Name,
        stdmaMenuPath + Form3.acDelphiSearchPath.Caption, Form3.acDelphiSearchPath.Hint, nil,
        IActionCallback(Form3));

      GetPictureFromImageList(Form3.ImageList1, Form3.acSelectDelphiVersion.ImageIndex, P);

      jvcsmak.AddMenuAction(Form3.acSelectDelphiVersion.Name,
        stdmaMenuPath + Form3.acSelectDelphiVersion.Caption, Form3.acSelectDelphiVersion.Hint, P,
        IActionCallback(Form3));

      GetPictureFromImageList(Form3.ImageList1, Form3.acBDSProjectDir.ImageIndex, P);
      jvcsmak.AddMenuAction(Form3.acBDSProjectDir.Name,
        stdmaMenuPath + Form3.acBDSProjectDir.Caption, Form3.acBDSProjectDir.Hint, P,
        IActionCallback(Form3));
      jvcsmak.AddMenuAction( Form3.acAddComponentFolder.Name,
        stdmaMenuPath + Form3.acAddComponentFolder.Caption, Form3.acAddComponentFolder.Hint, nil,
        IActionCallback(Form3));

      // add modules
      { compatibility
        TDMakModuleType = (dmmtNone=0, dmmtNormalFile(Delphi32)=1, dmmtJVCS=2,
        dmmtJVCSSync=3, dmmtPassolo=4, dmmtWise=5, dmmtMkdir=6);
      }
      GetPictureFromImageList(Form3.ImageList1, 1, P);
      // Name=testmodule;GUID=Implementation of Module, Image comes from Form3,
      // Extension=txt (could be more than one extension - spepareted by ;)
      // compatibility 1
      // Callback for the Moduletype
      PluginDelphi32Callback := TDelphi32ModuleCallback.Create(nil);
      jvcsmak.AddCommandType(stdStandardModuleCaption, '', stdCategoryCompiler, P,
        '.dpr;.dpk;.bpl;.dcp', 1, ICommandCallback(PluginDelphi32Callback));

      // SetVersion
      GetPictureFromImageList(Form3.ImageList1, 4, P);
      PluginSetDVersionCallback := TSetDVersionCommandCallback.Create(nil);
      jvcsmak.AddCommandType(stdSetDVersionCaption, '', stdCategoryCompiler, P, '', -1,
        ICommandCallback(PluginSetDVersionCallback));

      // Special Settings
      GetPictureFromImageList(Form3.ImageList1, 5, P);
      PluginSpecialSettingsCallback := TPluginSpecialSettingsCallback.Create(nil);
      jvcsmak.AddCommandType('Delphi Special Settings', '', stdCategoryCompiler, P, '', -1,
        ICommandCallback(PluginSpecialSettingsCallback));

      // CheckDelphiVersion
      GetPictureFromImageList(Form3.ImageList1, 7, P);
      PluginDelphi32CheckVersionCallback := TPluginDelphi32CheckVersionCallback.Create(nil);
      jvcsmak.AddCommandType('Delphi CheckVersion', '', stdCategoryCompiler, P, '', -1,
        ICommandCallback(PluginDelphi32CheckVersionCallback));

      // BRCC32 Resource Compiler
      GetPictureFromImageList(Form3.ImageList1, 8, P);
      Brcc32CommandCallback := TBrcc32CommandCallback.Create(nil);
      jvcsmak.AddCommandType('Mircosoft Resource Compiler (RC)', '', stdCategoryResource, P, '.rc',
        -1, ICommandCallback(Brcc32CommandCallback));

      // Select Delphi Platform
      GetPictureFromImageList(Form3.ImageList1, 9, P);
      PluginSelectDelphiPlatformCallback := TPluginSelectDelphiPlatformCallback.Create(nil);
      jvcsmak.AddCommandType('Select Delphi Platform', '', stdCategoryCompiler, P, '', -1,
        ICommandCallback(PluginSelectDelphiPlatformCallback));

      //Edit Namespaces
      GetPictureFromImageList(Form3.ImageList1, 10, P);
      PluginEditNamespacesCallback := TPluginEditNamespacesCallback.Create(nil);
      jvcsmak.AddCommandType('Edit Namespaces', '', stdCategoryCompiler, P, '', -1,
        ICommandCallback(PluginEditNamespacesCallback));


      // Credits
      jvcsmak.AddCreditInfo(struPluginName + ' by ' + struPluginAuthor);

      // Additional Info
      // jvcsmak.AddAdditionalInfo(struPluginHint);

      ReadDelphiVersionReg;

      while not CheckAllDelphiVersions do
        CheckAllDelphiVersions;

    except
      on E: Exception do
      begin
        jvcsmak.LogMessage('Plugin: ' + Application.ExeName);
        jvcsmak.LogMessage('Exception: ' + E.Message);
      end;
    end;
  end;
end;

// :UnRegister an finalize Plugin
function UnregisterPlugin: Integer; stdcall;
begin
  Result := 0;
  try
    Form3.Free;
    PluginDelphi32Callback.Free;
    PluginSetDVersionCallback.Free;
  except
  end;
end;

// :Version of plugin
function GetMinorVersion: Integer; stdcall;
begin
  Result := 0;
end;

// :Version of plugin
function GetMajorVersion: Integer; stdcall;
begin
  Result := 1;
end;

// :Return the GUID of the Plugins Options-DLG
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
  JVCSMAKPlugin;

begin

end.
