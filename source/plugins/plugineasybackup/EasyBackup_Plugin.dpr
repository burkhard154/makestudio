(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Easybackup.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/05  BSchranz  - Easybackup Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

library EasyBackup_Plugin;

{$I jedi.inc}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  SysUtils,
  Classes,
  ComServ,
  ActiveX,
  EasyBackup_Actions in 'EasyBackup_Actions.pas' {Form3},
  EasyBackup_Actiontest in 'EasyBackup_Actiontest.pas' {Form2},
  EasyBackup_Module in 'EasyBackup_Module.pas',
  EasyBackup_Vars in 'EasyBackup_Vars.pas',
  EasyBackup_EditModule in 'EasyBackup_EditModule.pas' {FormEditEasyBakModule},
  EasyBackup_FormCopy in 'EasyBackup_FormCopy.pas' {FormDoCopy},
  EasyBackup_SelectFilesForm in 'EasyBackup_SelectFilesForm.pas' {FormSelectFiles},
  EasyBackup_Utils in 'EasyBackup_Utils.pas',
  makestudio_TLB in '..\..\framework\makestudio_TLB.pas';

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
procedure GetName(AName: PChar); stdcall;
begin
  SysUtils.StrCopy(AName, PChar(struPluginName));
end;

//:Get author of Plugin
procedure GetAuthor(AName: PChar); stdcall;
begin
  SysUtils.StrCopy(AName, PChar(struPluginAuthor));
end;

//:Get description of Plugin
procedure GetDescription(AName: PChar); stdcall;
begin
  SysUtils.StrCopy(AName, PChar(struPluginHint));
end;

//:List of Required plugins separeted by ";"
procedure GetRequiredPlugins(AName: PChar); stdcall;
begin
  SysUtils.StrCopy(AName, '');
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
      Form3 := TForm3.Create(nil);


      //add actions
      {GetPictureFromImageList(Form3.ImageList1, Form3.acTestaction1.ImageIndex, p);
      MakeStudio.AddMenuAction(Form3.acTestaction1.Name,
                             Form3.acTestaction1.Caption,
                             Form3.acTestaction1.Hint,
                             p,
                             IActionCallback(Form3));}

      //add modules
      GetPictureFromImageList(Form3.ImageList1, 2, P);
      //Name=testmodule;GUID=Implementation of Module, Image comes from Form3,
      //Extension=txt (could be more than one extension - spepareted by ;)
      //no compatibility - module did not exist before
      //Callback for the Moduletype
      EasyBackupCommandCallback := TEasyBackupCommandCallback.Create(nil);
      MakeStudio.AddCommandType(stdEasyBackupCaption, '', stdCategory, P, '.wse',
                             -1, ICommandCallback(EasyBackupCommandCallback));


      //Credits
      MakeStudio.AddCreditInfo(struPluginName + ' by ' + struPluginAuthor);

      //Additional Info
      //MakeStudio.AddAdditionalInfo(struPluginHint);

    except
    end;
  end;
end;

//:UnRegister an finalize Plugin
function UnregisterPlugin: Integer; stdcall;
begin
  Result := 0;
  try
    Form3.Free;
    EasyBackupCommandCallback.Free;
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
