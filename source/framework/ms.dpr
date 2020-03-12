(* -----------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: DMak.dpr

  The Initial Devoloper of the original DMAK-Code is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
  Code move to JEDI VCS:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
  Uwe Schuster (jedivcs@bitcommander.de)

  Componentes and used code which is used in this code are explictly stated to
  be copyright of the respective author(s).

  Last Modified: see History

  Known Issues:
  ToDo:
  - create forms on demand (USc: Done)
  - translate to english
  - contains still a lot Exit's which should be changed into begin end
  - merge/remove FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, ... implementations
  -----------------------------------------------------------------------------

  Unit history:

  2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to MakeStudio
  2003/11/28  USchuster - rebuild dpr from DMAK
  2003/12/05  USchuster - re-formatted
  - fixed #bf012 (create forms on demand)
  - changed Application Title
  2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
  2005/02/02  USchuster - minor style cleaning

  ----------------------------------------------------------------------------- *)

program jmak;

{$APPTYPE CONSOLE}
{$DEFINE CONSOLE}
{$R makestudio.tlb}
{$I jedi.inc}
{$I jvcl.inc}
{$IFDEF VCL}
fdsa
{$ENDIF}
  uses SysUtils, Classes, msProgram, msGlobals, msUtils, msResources, DateUtils, JclAnsiStrings, makestudio_TLB;

var
  stGlobalLog: TStringList;

  // Console Output
procedure ConsoleAddLog(S: string);
begin
  Writeln(StrAnsiToOem(S));
  stGlobalLog.Add(S)
end;

procedure ConsoleAddLogStrings(sl: TStringList);
var
  i: Integer;
begin
  stGlobalLog.AddStrings(sl);
  for i := 0 to sl.Count - 1 do
    Writeln(StrAnsiToOem(sl[i]));
end;

procedure ConsoleClearLog;
begin
  stGlobalLog.Clear;
end;

// :Stores the Logbook with date and time and filename in then Appdata folder
procedure ConsoleSaveLogAuto;
var
  S: string;
  Y, M, D, H, min, sec, msec: Word;
begin
  DecodeDateTime(Now, Y, M, D, H, min, sec, msec);

  if Programhandler.Filename <> '' then
    S := ChangeFileExt(ExtractFileName(Programhandler.Filename), '') + ' ';
  S := S + Format('%d_%d_%d (%d_%d_%d).txt', [Y, M, D, H, min, sec]);

  S := GetJAppDataFolder + S;
  try
    stGlobalLog.SaveToFile(S);
  except
    On E: Exception do
      Writeln(StrAnsiToOem(E.Message));
  end
end;

procedure ConsoleSaveLog(Filename: String);
begin
  try
    stGlobalLog.SaveToFile(Filename);
  except
    On E: Exception do
      Writeln(StrAnsiToOem(E.Message));
  end
end;

procedure ListPlugins;
var
  i: Integer;
begin
  for i := 0 to PluginHandler.Plugins.Count - 1 do
  begin
    ConsoleAddLog(PluginHandler.Plugins[i].GetName + ' (' + PluginHandler.Plugins[i].Filename + ')');
  end;
  AddLog('');
end;

procedure RunProgram(AFilename: string);
begin
  InitGlobals(mkCommandLine);
  try
    InitializeApplication;
    // ListPlugins;
    if Programhandler.LoadFromFile(AFilename) then
      Programhandler.Execute;
    FinalizeApplication;
  finally
    FreeGlobals;
  end;
end;

begin
  try
    Writeln('JEDI VCS make util (Project JEDI (www.delphi-jedi.org))');
    Writeln;
    if ParamCount > 0 then
    begin
      stGlobalLog := TStringList.Create;
      try
        _AddLog := ConsoleAddLog;
        _AddLogStrings := ConsoleAddLogStrings;
        _ClearLog := ConsoleClearLog;
        _SaveLogAuto := ConsoleSaveLogAuto;
        _SaveLog := ConsoleSaveLog;
        ProgramAddLog := ConsoleAddLog;
        ProgramClearLog := ConsoleClearLog;

        AddLog(stdCmdLineCopyright);
        AddLog('');
        AddLog(stdCmdLoadedPlugins);

        RunProgram(ParamStr(1));
      finally
        stGlobalLog.Free;
      end;
    end
    else
    begin
      Writeln('Usage:');
      Writeln('MakeStudioe makefile');
    end;
  except
    on E: Exception do
    begin
      if not(E is EAbort) then
        Writeln('[', E.ClassName, '] ', E.Message);
    end;
  end;

end.
