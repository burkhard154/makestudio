(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dialogs_MakeStudioPlugin.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/04  JDuenow   - launched Dialogs Module

-----------------------------------------------------------------------------*)

library dialogs_MakeStudioPlugin;

{$I jedi.inc}

uses
  SysUtils,
  Classes,
  msTLB in '..\..\msTLB.pas',
  ComServ,
  ActiveX,
  dialogs_Vars in 'dialogs_Vars.pas',
  dialogs_Actions in 'dialogs_Actions.pas' {Form3},
  dialogs_Actiontest in 'dialogs_Actiontest.pas' {Form2},
  dialogs_EditMsgBoxModule in 'dialogs_EditMsgBoxModule.pas' {FormMsgBoxModuleEdit},
  dialogs_EditInputBoxModule in 'dialogs_EditInputBoxModule.pas' {FormInputBoxModuleEdit},
  dialogs_MsgBox in 'dialogs_MsgBox.pas',
  dialogs_InputBox in 'dialogs_InputBox.pas',
  dialogs_tools in 'dialogs_tools.pas',
  dialogs_dlgInput in 'dialogs_dlgInput.pas' {FormDlgInput};

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
  with MakeStudio do begin
    try
      //Create form with actions and ModuleCallback
      Form3 := TForm3.Create(nil);

      GetPictureFromImageList(Form3.ImageList1, 2, P);
      MsgBoxCommandCallback := TMsgBoxCommandCallback.Create(nil);
      MakeStudio.AddCommandType(stdMsgBoxCaption, '', stdCategory, P, '',
                             0, ICommandCallback(MsgBoxCommandCallback));

      GetPictureFromImageList(Form3.ImageList1, 3, P);
      InputBoxCommandCallback := TInputBoxCommandCallback.Create(nil);
      MakeStudio.AddCommandType(stdInputBoxCaption, '', stdCategory, P, '',
                             0, ICommandCallback(InputBoxCommandCallback));

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
    MsgBoxCommandCallback.Free;
    InputBoxCommandCallback.Free;
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
