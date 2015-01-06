{$IFDEF BLOCKHEADER}
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

2005/01/08  BSchranz  - Plugin template created
2005/02/15  USchuster - preparations for check in and modified for Wizard
2011/09/25  BSchranz  - preparations for external code wizard
2015/01/01  BSchranz  - makestudio reference fixed

-----------------------------------------------------------------------------*)
{$ENDIF BLOCKHEADER}
library %MODULEIDENT%;

uses
  SysUtils,
  Classes,
  ComServ,
  Forms,
  makestudio_TLB in '..\..\framework\makestudio_TLB',
  {$IFDEF BLOCKEXTERNALWIZARD}
  %USEVARS%,
  %USEMODULE%,
  %USEEDIT%,
  {$IFDEF BLOCKMENUACTION}
  %USEACTIONS%,
  %USEACTIONTEST%,
  {$ENDIF BLOCKMENUACTION}
  {$ENDIF BLOCKEXTERNALWIZARD}
  ActiveX;

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
      {$IFDEF BLOCKMENUACTION}
      //Create form with actions
      FormActions := TFormActions.Create(nil);

      //--- add actions --------------------------------------------------------
      GetPictureFromImageList(FormActions.ImageList1, FormActions.acTestaction1.ImageIndex, P);
      //if the Caption has "\" - the action is assigned to this main menu path!
      //e.g. 'Testmenu\test\'+FormActions.acTestaction1.Caption...
      //if not, the action is assigned to the "extras" menu item
      MakeStudio.AddMenuAction(FormActions.acTestaction1.Name,
                             '%MENUACTIONPATH%' + FormActions.acTestaction1.Caption,
                             FormActions.acTestaction1.Hint,
                             P,
                             IActionCallback(FormActions));
      {$ENDIF BLOCKMENUACTION}

      //--- add modules --------------------------------------------------------
      GetPictureFromImageList(FormActions.ImageList1, 0, P);
      //Name=%COMMANDNAME%; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      //Callback for the Moduletype
      MakeStudio.LogMessage(Application.Exename);
      Plugin%COMMANDIDENTIFIER%Callback := TPlugin%COMMANDIDENTIFIER%Callback.Create(nil);
      MakeStudio.AddCommandType('%COMMANDNAME%', '', stCategory, P, 'txt', -1,
        ICommandCallback(Plugin%COMMANDIDENTIFIER%Callback));

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
    Plugin%COMMANDIDENTIFIER%Callback.Free;
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
