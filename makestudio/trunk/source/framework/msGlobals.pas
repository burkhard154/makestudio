(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msGlobals.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:                                                                                    }

2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/02/19  USchuster - changes for commandline version
2005/06/26  USchuster - changes to support EMakeKind <> mkGUI
2005/28/09  BSchranz  - GUI dependency removed
2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
2005/04/09  BSchranz  - Translated to englisch

-----------------------------------------------------------------------------*)
//Globals and global funtions
unit msGlobals;

{$I jedi.inc}

interface

uses
  Classes, msActionHandler, msProgram, msVarHandler,
  msTLB, msPluginHandler, msResources, SysUtils, Windows,
  Graphics, JclStrings, JclSysInfo;

//Global vars
var
  PluginHandler: TMakeStudioPluginHandler; //Pluginhandler
  Actionhandler: TJVCSActionhandler;
  Programhandler: TProgram;
  Varhandler: TxVarHandler; //handles variables
  HelpFile : String = '';

//:Initialize all global vars
procedure InitGlobals(AMakeKind: EMakeKind = mkGUI);
//:Free all gobal vars
procedure FreeGlobals;
//:Initialize Application
procedure InitializeApplication;
//:Finalize Application
procedure FinalizeApplication;

implementation

uses
  msInternalCommands, msFrmRunListListbox, msutils;

procedure InitGlobals(AMakeKind: EMakeKind = mkGUI);
begin
  PluginHandler := TMakeStudioPluginHandler.Create(AMakeKind);
  Programhandler := TProgram.Create;
  Programhandler.RootProgram := Programhandler;
  Varhandler := TxVarHandler.Create(nil);
  SetVarhandlerReference( Varhandler);
  AddInternalCommands;
  Actionhandler := TJVCSActionhandler.Create(nil, nil);
end;

procedure FreeGlobals;
begin
  ReleaseInternalCommands;
  PluginHandler.Free;
  Programhandler.Free;
  Actionhandler.Free;
  Varhandler.Free;
end;

procedure InitializeApplication;
var
  S: string;
  I: Integer;
begin
  //load all Plugins
  PluginHandler.LoadAllLibs;

  PluginHandler.AfterAllPluginsLoaded;

  //Add reasonable variables

  //Personal Folder
  Varhandler.AddVar( 'PersonalFolder', stSystemCategory, GetPersonalFolder);
  //Win-Dir
  Varhandler.AddVar( 'WindowsFolder', stSystemCategory, GetWindowsFolder);
  //JApplication Folder
  Varhandler.AddVar( 'ProgramApplicationFolder', stSystemCategory, GetJAppDataFolder);
  //JApplication Folder
  Varhandler.AddVar( 'PersonalApplicationFolder', stSystemCategory, GetJPersonalAppDataFolder);
  //TempData Folder
  Varhandler.AddVar( 'TempdataFolder', stSystemCategory, GetWindowsTempFolder);
  //Help Folder
  Varhandler.AddVar( tvar_HelpPath, stSystemCategory, IncludeTrailingPathDelimiter( ExtractFilePath( Paramstr(0))) + stHelpPath);
  //ActiveScriptFile
  Varhandler.AddVar( 'ActiveScriptFile', stSystemCategory, '');


  //all Plugins as Variables?
  {Varhandler.AddVar('PluginCount', 'Plugins', '', '');
  Varhandler.SetVar('PluginCount', PluginHandler.Plugins.Count);
  for I := 0 to PluginHandler.Plugins.Count - 1 do
  begin
    Varhandler.AddVar('Plugin' + IntToStr(I), 'Plugins', '', '');
    Varhandler.SetVar('Plugin' + IntToStr(I), PluginHandler.Plugins[I].Filename);
  end;}
end;

procedure FinalizeApplication;
begin
  Programhandler.Clear;
  Actionhandler.ClearCallbacks;
  CommandTypes.Clear;
  PluginHandler.FreeAllLibs;
end;

end.

