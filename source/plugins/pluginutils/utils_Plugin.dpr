(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jmakutils.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy D�now (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/22  JDuenow   - launched EditMkdirModule
2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in
2005/09/11  BSchranz  - Internal Batch command list added

-----------------------------------------------------------------------------*)

library utils_Plugin;

{$I jedi.inc}

uses
  SysUtils,
  Classes,
  makestudio_TLB in '..\..\framework\makestudio_TLB.pas',
  ComServ,
  ActiveX,
  synDWrite,
  utils_Vars in 'utils_Vars.pas',
  utils_MkDir in 'utils_MkDir.pas',
  utils_Actionedit in 'utils_Actionedit.pas' {Form2},
  utils_Actions in 'utils_Actions.pas' {Form3},
  utils_EditMkDirModule in 'utils_EditMkDirModule.pas' {FormMkdirModuleEdit},
  utils_RunBatch in 'utils_RunBatch.pas',
  utils_EditCopyFilesModule in 'utils_EditCopyFilesModule.pas' {FormCopyFilesEdit},
  utils_FileCopy in 'utils_FileCopy.pas',
  utils_EditBatchInternal in 'utils_EditBatchInternal.pas' {FormEditBatchInternal},
  utils_EditBatchModule in 'utils_EditBatchModule.pas' {FormEditBatchModule};

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

//:List of Required plugins separeted by ";"
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
      Form3 := TForm3.Create(nil);


      //add actions
      {GetPictureFromImageList(Form3.ImageList1, Form3.acTestaction1.ImageIndex, P);
      MakeStudio.AddMenuAction(Form3.acTestaction1.Name,
                             Form3.acTestaction1.Caption,
                             Form3.acTestaction1.Hint,
                             P,
                             IActionCallback(Form3));}

      //add modules
      {compatibility
TDMakModuleType = (dmmtNone=0, dmmtNormalFile(Delphi32)=1, dmmtJVCS=2,
dmmtJVCSSync=3, dmmtPassolo=4, dmmtWise=5, dmmtMkdir=6);
      }
      GetPictureFromImageList(Form3.ImageList1, 0, P);
      //Name=testmodule;GUID=Implementation of Module, Image comes from Form3,
      //Extension=txt (could be more than one extension - spepareted by ;)
      //compatibility - 6
      //Callback for the Moduletype
      PluginMkDirCallback := TMkDirModuleCallback.Create(nil);
      MakeStudio.AddCommandType(stdMkdirCaption, '', stdCategoryFile, P, '',
                             6, ICommandCallback(PluginMkDirCallback));

      GetPictureFromImageList(Form3.ImageList1, 1, P);
      PluginCopyFilesCallback := TCopyFilesModuleCallback.Create(nil);
      MakeStudio.AddCommandType(stdCopyFilesCaption, '', stdCategoryFile, P, '',
                             -1, ICommandCallback(PluginCopyFilesCallback));


      GetPictureFromImageList(Form3.ImageList1, 2, P);
      PluginBatchCallback := TBatchModuleCallback.Create(nil);
      MakeStudio.AddCommandType(stdBatchFileCaption, '', stdCategorySystem, P, '.bat',
                             -1, ICommandCallback(PluginBatchCallback));

      GetPictureFromImageList(Form3.ImageList1, 3, P);
      PluginBatchInternalCallback := TBatchInternalModuleCallback.Create(nil);
      MakeStudio.AddCommandType(stdBatchFileInternalCaption, '', stdCategorySystem, P, '.bat',
                             -1, ICommandCallback(PluginBatchInternalCallback));

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
    TsynDWrite.Finalize;
    Form3.Free;
    PluginMkDirCallback.Free;
    PluginCopyFilesCallback.Free;
    PluginBatchCallback.Free;
    PluginBatchInternalCallback.Free;
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
