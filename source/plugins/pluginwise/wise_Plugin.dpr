﻿(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: wise.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy D�now (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/21  JDuenow   - launched Wise Module
2005/01/04  BSchranz  - Plugin created
2005/02/05  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

library wise_Plugin;

{$I jedi.inc}

uses
  SysUtils,
  Classes,
  makestudio_TLB in '..\..\framework\makestudio_TLB.pas',
  ComServ,
  ActiveX,
  wise_Actions in 'wise_Actions.pas' {Form3},
  wise_Actiontest in 'wise_Actiontest.pas' {Form2},
  wise_Module in 'wise_Module.pas',
  wise_Vars in 'wise_Vars.pas',
  wise_EditWiseModule in 'wise_EditWiseModule.pas' {FormWiseModuleEdit};

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
      GetPictureFromImageList(Form3.ImageList1, 2, P);
      //Name=testmodule;GUID=Implementation of Module, Image comes from Form3,
      //Extension=txt (could be more than one extension - spepareted by ;)
      //compatibility - 5
      //Callback for the Moduletype
      WiseCommandCallback := TWiseCommandCallback.Create(nil);
      MakeStudio.AddCommandType(stdWiseCaption, '', stdCategory, P, '.wse',
                             5, ICommandCallback(WiseCommandCallback));

      //Variables
      MakeStudio.Variables.AddVar('wise_fVersion');
      MakeStudio.Variables.Values['wise_fVersion'] := stdcFVersion;
      MakeStudio.Variables.AddVar('wise_lVersion');
      MakeStudio.Variables.Values['wise_lVersion'] := stdcLVersion;

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
    WiseCommandCallback.Free;
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
