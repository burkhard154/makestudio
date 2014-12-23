(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsmakplugintemplate.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/08  BSchranz  - Plugin template created
2005/02/15  USchuster - preparations for check in and modified for Wizard

-----------------------------------------------------------------------------*)
library DotNetFramework_JMakPlugin;

uses
  SysUtils,
  Classes,
  msTLB,
  ComServ,
  Forms,
  ActiveX,
  dotnetframework_Actions in 'dotnetframework_Actions.pas' {FormActions},
  dotnetframework_TlbImpCommand in 'dotnetframework_TlbImpCommand.pas',
  dotnetframework_TlbImpEdit in 'dotnetframework_TlbImpEdit.pas' {FormEditTlbImpParams},
  dotnetframework_Vars in 'dotnetframework_Vars.pas';

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
      //Name=Delphi.Net; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      //Callback for the Moduletype
      PluginTlbImpCallback := TPluginTlbImpCallback.Create(nil);
      jvcsmak.AddCommandType( stCommandCaption, stCommandHint, stCategory, P, 'txt', -1,
        ICommandCallback(PluginTlbImpCallback));

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
    PluginTlbImpCallback.Free;
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