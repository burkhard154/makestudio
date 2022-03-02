(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Dmakutils.pas

The Initial Developer of the original DMAK-Code is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
Code move to JEDI VCS:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to MakeStudio
2003/11/28  USchuster - 2nd Migrationstep (fixed header)
2003/12/05  USchuster - re-formatted
2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/08/12  BSchranz  - command line version "jmak.exe" added
2005/02/09  BSchranz  - TCommandTypeDragObject added to drop Command on an Sequence editor
2005/02/09  BSchranz  - GetJPersonalAppDataFolder added to save and load user desktops
2005/04/09  BSchranz  - Translated to englisch
2005/04/09  USchuster - D5 fix

-----------------------------------------------------------------------------*)

unit msUtils;

{$I jedi.inc}

interface

uses
  SysUtils, Registry, Windows, Classes, Controls, JclFileUtils,
  JclSysInfo, JclShell, JclWin32, msresources, msprogram;

type
  //Drag-Object for adding Commands to the command list
  TCommandTypeDragObject = class({$IFDEF DELPHI6_UP} TDragObjectEx{$ELSE} TDragObject{$ENDIF})
  private
    FCommandType: TCommandTypeItem;
  public
    property CommandType: TCommandTypeItem read FCommandType write FCommandType;
  end;

var
  _AddLog: procedure(S: string) = nil;
  _AddLogStrings: procedure(sl: TStringList) = nil;
  _ClearLog: procedure = nil;
  _SaveLogAuto: procedure = nil;
  _SaveLog: procedure(Filename: string) = nil;

{:Logbook entry}
procedure AddLog(S: string);
procedure AddLogStrings(sl: TStringList);
{:clear logbook}
procedure ClearLog;
procedure SaveLogAuto;
procedure SaveLog(Filename: string);
procedure TrimStringListLeft(SL: TStrings);
{:Writes a String into XUTILSROOTKEY of the registry}
procedure WriteRegStringLM(const Key, Name, Value: string);
{:Reads a String from XUTILSROOTKEY of the registry}
function ReadRegStringLM(const Key, Name, Default: string): string;
{:Writes a Integer into XUTILSROOTKEY of the registry}
procedure WriteRegIntLM(const Key, Name: string; Value: Integer);
{:Reads a Integer from XUTILSROOTKEY of the registry}
function ReadRegIntLM(const Key, Name: string; Default: Integer): Integer;
{:Writes a Float into XUTILSROOTKEY of the registry}
procedure WriteFloatIntLM(const Key, Name: string; Value: Double);
{:Reads a fload from XUTILSROOTKEY of the registry}
function ReadRegFloatLM(const Key, Name: string; Default: Double): Double;
{:Returns Token of the given input string separated by separator}
function GetToken(Input: string; Token: Integer; Separator: string): string;
{:Returns a list of Tokes of the given input string separated by separator}
function GetTokenList(Input: string; Tokens: TStrings; Separator: string): Integer;

function xGetLastError: string;
function GetMakeStudioBaseRegistryKey: string;
//:Returns the application data folder <all users><application data><MakeStudio>
function GetJAppDataFolder: string;
//:Returns the application data folder <current user><application data><MakeStudio>
function GetJPersonalAppDataFolder: string;
//:Returns the Help Filename
function GetHelpFile:String;

//:Defines the root key for all registry functions
var
  XUTILSROOTKEY: HKEY = HKEY_CURRENT_USER;

implementation

uses msglobals;


function GetHelpFile:String;
begin
  Result := PathAddSeparator( Varhandler.GetVar( tvar_HelpPath)) + sCHMFilename;
end;

function GetToken(Input: string; Token: Integer; Separator: string): string;
var
  SL: TStringList;
begin
  Result := '';
  SL := TStringList.Create;
  try
    if Token < GetTokenList(Input, SL, Separator) then
      Result := SL[Token];
  finally
    SL.Free;
  end;
end;

function GetTokenList(Input: string; Tokens: TStrings; Separator: string): Integer;
var
  S: string;
  L: Integer;
begin
  Tokens.Clear;
  S := Trim(Input);
  L := Pos(Separator, S);
  while L > 0 do
  begin
    Tokens.Add(SYSTEM.Copy(S, 1, L - 1));
    SYSTEM.Delete(S, 1, L);
    L := Pos(Separator, S);
  end;
  Tokens.Add(S);
  Result := Tokens.Count;
end;

procedure AddLog(S: string);
begin
  if Assigned(_AddLog) then
    _AddLog(S);
end;

procedure AddLogStrings(sl: TStringList);
begin
  if Assigned(_AddLogStrings) then
    _AddLogStrings(sl);
end;

procedure ClearLog;
begin
  if Assigned(_ClearLog) then
    _ClearLog;
end;

procedure SaveLogAuto;
begin
  if Assigned(_SaveLogAuto) then
    _SaveLogAuto;
end;

procedure SaveLog(Filename: string);
begin
  if Assigned(_SaveLog) then
    _SaveLog(Filename);
end;

function GetJAppDataFolder: string;
begin
  Result := PathAddSeparator(GetSpecialFolderLocation(CSIDL_COMMON_DOCUMENTS)) + stcMakeStudio;

  ForceDirectories(Result);
  Result := PathAddSeparator(Result);
end;

function GetJPersonalAppDataFolder: string;
begin
  if GetAppdataFolder <> '' then
    Result := PathAddSeparator(GetAppdataFolder) + stcMakeStudio
  else
    Result := PathAddSeparator(GetPersonalFolder) + stcMakeStudio;

  ForceDirectories(Result);
  Result := PathAddSeparator(Result);
end;

procedure TrimStringListLeft(SL: TStrings);
var
  I: Integer;
begin
  for I := 0 to SL.Count - 1 do
    SL[I] := TrimLeft(SL[I]);
end;

procedure WriteRegStringLM(const Key, Name, Value: string);
begin
  with TRegistry.Create(KEY_WRITE) do
  try
    RootKey := XUTILSROOTKEY;
    if OpenKey(Key, True) then
      WriteString(Name, Value);
  finally
    Free;
  end;
end;

function ReadRegStringLM(const Key, Name, Default: string): string;
begin
  Result := Default;
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := XUTILSROOTKEY;
    if KeyExists(Key) and OpenKeyReadOnly(Key) and ValueExists(Name) then
      Result := ReadString(Name);
  finally
    Free;
  end;
end;

procedure WriteRegIntLM(const Key, Name: string; Value: Integer);
begin
  with TRegistry.Create(KEY_WRITE) do
  try
    RootKey := XUTILSROOTKEY;
    if OpenKey(Key, True) then
      WriteInteger(Name, Value);
  finally
    Free;
  end;
end;

function ReadRegIntLM(const Key, Name: string; Default: Integer): Integer;
begin
  Result := Default;
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := XUTILSROOTKEY;
    if KeyExists(Key) and OpenKeyReadOnly(Key) and ValueExists(Name) then
      Result := ReadInteger(Name);
  finally
    Free;
  end;
end;

procedure WriteFloatIntLM(const Key, Name: string; Value: Double);
begin
  with TRegistry.Create(KEY_WRITE) do
  try
    RootKey := XUTILSROOTKEY;
    if OpenKey(Key, True) then
      WriteFloat(Name, Value);
  finally
    Free;
  end;
end;

function ReadRegFloatLM(const Key, Name: string; Default: Double): Double;
begin
  Result := Default;
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := XUTILSROOTKEY;
    if KeyExists(Key) and OpenKeyReadOnly(Key) and ValueExists(Name) then
      Result := ReadFloat(Name);
  finally
    Free;
  end;
end;

function xGetLastError: string;
var
  ch: array [0..511] of Char;
begin
  FormatMessage(
    FORMAT_MESSAGE_FROM_SYSTEM,
    nil,
    GetLastError,
    LANG_NEUTRAL, // Default language
    ch,
    511,
    nil);
  Result := StrPas(ch);
end;

function GetMakeStudioBaseRegistryKey: string;
begin
  {$IFNDEF ADDITIVE}
  Result := 'Software\optimeas\makestudio';
  {$ELSE}
  Result := 'Software\Additive\Project Maker';
  {$ENDIF ~ADDITIVE}
end;


end.
