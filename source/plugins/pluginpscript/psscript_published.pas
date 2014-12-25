{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsplugintemplate_Module.pas

The Initial Developer of the original code (JEDI Make) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2006/04/21  BSchranz  - Plugin Script created
2005/05/27  BSchranz  - Released

------------------------------------------------------------------------------}
unit psscript_published;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  SysUtils, Classes, Windows, JclStrings, JclFileUtils, JclSysUtils, Dialogs,
  pscript_vars, ShellApi, JclSysInfo, JclShell,uPSComponent, uPSRuntime,
  uPSCompiler, JclRegistry, JclIniFiles, ActiveX, ComObj;


procedure Register_psscript_published( CL: TPSPascalCompiler; S: TPSExec);


//Folders
procedure SetCurrentFolder(const Folder: string);
function FileOrDirExists(const Name: String): Boolean;
function CreateShellLink(const Filename, Description, ShortcutTo, Parameters, WorkingDir, IconFilename: String; const IconIndex, ShowCmd: Integer): String;

//Jedi Make Functions
procedure Writeln( S:String);
function Readln( Question:String):String;
function VarGet( VarName:String):Variant;
function VarCount:Integer;
function VarByIdx( Idx:Integer):Variant;
procedure VarAdd( VarName:String);
function VarExists( VarName:String):Boolean;
procedure VarSet( VarName:String; Content:Variant);
function VarReplace( S:String):String;
function Exec( App, Args, Dir:String):Integer;

implementation

procedure Register_psscript_published( CL: TPSPascalCompiler; S: TPSExec);

  //addfunction
  procedure af( ProcPtr: Pointer;
    const Name: string; const Declaration:String);
  begin
    if CL<>nil then
      CL.AddDelphiFunction( Declaration);
    if S<>nil then
      S.RegisterDelphiFunction( ProcPtr, Name, cdRegister);
  end;

begin
  //Types, Consts
  if CL<>nil then begin
    CL.AddConstantN('faReadOnly','LongWord').SetUInt( $00000001);
    CL.AddConstantN('faHidden','LongWord').SetUInt( $00000002);
    CL.AddConstantN('faSysFile','LongWord').SetUInt( $00000004);
    CL.AddConstantN('faDirectory','LongWord').SetUInt( $00000010);
    CL.AddConstantN('faArchive','LongWord').SetUInt( $00000020);
    CL.AddConstantN('faSymLink','LongWord').SetUInt( $00000040);
    CL.AddConstantN('faAnyFile','LongWord').SetUInt( $0000003F);
    CL.AddConstantN('HKEY_CLASSES_ROOT', 'LongWord').SetUInt($80000000);
    CL.AddConstantN('HKEY_CURRENT_USER', 'LongWord').SetUInt($80000001);
    CL.AddConstantN('HKEY_LOCAL_MACHINE', 'LongWord').SetUInt($80000002);
    CL.AddConstantN('HKEY_USERS', 'LongWord').SetUInt($80000003);
    CL.AddConstantN('HKEY_PERFORMANCE_DATA', 'LongWord').SetUInt($80000004);
    CL.AddConstantN('HKEY_CURRENT_CONFIG', 'LongWord').SetUInt($80000005);
    CL.AddConstantN('HKEY_DYN_DATA', 'LongWord').SetUInt($80000006);
    CL.AddConstantN('SW_HIDE', 'LongWord').SetUInt( 0);
    CL.AddConstantN('SW_SHOWNORMAL', 'LongWord').SetUInt( 1);
    CL.AddConstantN('SW_NORMAL', 'LongWord').SetUInt( 1);
    CL.AddConstantN('SW_SHOWMINIMIZED', 'LongWord').SetUInt( 2);
    CL.AddConstantN('SW_SHOWMAXIMIZED', 'LongWord').SetUInt( 3);
    CL.AddConstantN('SW_MAXIMIZE', 'LongWord').SetUInt( 3);
    CL.AddConstantN('SW_SHOWNOACTIVATE', 'LongWord').SetUInt( 4);
    CL.AddConstantN('SW_SHOW', 'LongWord').SetUInt( 5);
    CL.AddConstantN('SW_MINIMIZE', 'LongWord').SetUInt( 6);
    CL.AddConstantN('SW_SHOWMINNOACTIVE', 'LongWord').SetUInt( 7);
    CL.AddConstantN('SW_SHOWNA', 'LongWord').SetUInt( 8);
    CL.AddConstantN('SW_RESTORE', 'LongWord').SetUInt( 9);

    CL.AddTypeS('TDateTime', 'Double');
    CL.AddTypeS('DelphiHKEY', 'LongWord');
  end;

  af(@DeleteDirectory, 'DeleteDirectory', 'Function DeleteDirectory( const DirectoryName : string; MoveToRecycleBin : Boolean) : Boolean');
  af(@ForceDirectories, 'ForceDirectories', 'Function ForceDirectories( Dir : string) : Boolean');
  af(@RenameFile, 'RenameFile', 'Function RenameFile( const OldName, NewName : string) : Boolean');
  af(@FileCopy, 'CopyFile', 'Function CopyFile( Source, Destination : String; ReplaceExisting : Boolean) : Boolean');
  af(@SysUtils.DeleteFile, 'DeleteFile', 'Function DeleteFile( Filename : String) : Boolean');
  af(@DirectoryExists, 'DirectoryExists', 'Function DirectoryExists( const Name : String) : Boolean');
  af(@FileExists, 'FileExists', 'Function FileExists( const Name : String) : Boolean');
  af(@FileOrDirExists, 'FileOrDirExists', 'Function FileOrDirExists( const Name : String) : Boolean');
  af(@BuildFileList, 'BuildFileList', 'Function BuildFileList( const Path : string; const Attr : Integer; const List : TStrings) : Boolean');
  af(@SetCurrentFolder, 'SetCurrentFolder', 'Procedure SetCurrentFolder( const Folder : string)');
  af(@GetCommonFilesFolder, 'GetCommonFilesFolder', 'Function GetCommonFilesFolder : string');
  af(@GetCurrentFolder, 'GetCurrentFolder', 'Function GetCurrentFolder : string');
  af(@GetProgramFilesFolder, 'GetProgramFilesFolder', 'Function GetProgramFilesFolder : string');
  af(@GetWindowsFolder, 'GetWindowsFolder', 'Function GetWindowsFolder : string');
  af(@GetWindowsSystemFolder, 'GetWindowsSystemFolder', 'Function GetWindowsSystemFolder : string');
  af(@GetWindowsTempFolder, 'GetWindowsTempFolder', 'Function GetWindowsTempFolder : string');
  af(@GetDesktopFolder, 'GetDesktopFolder', 'Function GetDesktopFolder : string');
  af(@GetProgramsFolder, 'GetProgramsFolder', 'Function GetProgramsFolder : string');
  af(@GetPersonalFolder, 'GetPersonalFolder', 'Function GetPersonalFolder : string');
  af(@GetFavoritesFolder, 'GetFavoritesFolder', 'Function GetFavoritesFolder : string');
  af(@GetStartupFolder, 'GetStartupFolder', 'Function GetStartupFolder : string');
  af(@GetRecentFolder, 'GetRecentFolder', 'Function GetRecentFolder : string');
  af(@GetSendToFolder, 'GetSendToFolder', 'Function GetSendToFolder : string');
  af(@GetStartmenuFolder, 'GetStartmenuFolder', 'Function GetStartmenuFolder : string');
  af(@GetDesktopDirectoryFolder, 'GetDesktopDirectoryFolder', 'Function GetDesktopDirectoryFolder : string');
  af(@GetNethoodFolder, 'GetNethoodFolder', 'Function GetNethoodFolder : string');
  af(@GetFontsFolder, 'GetFontsFolder', 'Function GetFontsFolder : string');
  af(@GetCommonStartmenuFolder, 'GetCommonStartmenuFolder', 'Function GetCommonStartmenuFolder : string');
  af(@GetCommonProgramsFolder, 'GetCommonProgramsFolder', 'Function GetCommonProgramsFolder : string');
  af(@GetCommonStartupFolder, 'GetCommonStartupFolder', 'Function GetCommonStartupFolder : string');
  af(@GetCommonDesktopdirectoryFolder, 'GetCommonDesktopdirectoryFolder', 'Function GetCommonDesktopdirectoryFolder : string');
  af(@GetCommonAppdataFolder, 'GetCommonAppdataFolder', 'Function GetCommonAppdataFolder : string');
  af(@GetAppdataFolder, 'GetAppdataFolder', 'Function GetAppdataFolder : string');
  af(@GetAppdataFolder, 'GetAppdataFolder', 'Function GetPrinthoodFolder : string');
  af(@GetCommonFavoritesFolder, 'GetCommonFavoritesFolder', 'Function GetCommonFavoritesFolder : string');
  af(@GetTemplatesFolder, 'GetTemplatesFolder', 'Function GetTemplatesFolder : string');
  af(@GetInternetCacheFolder, 'GetInternetCacheFolder', 'Function GetInternetCacheFolder : string');
  af(@GetCookiesFolder, 'GetCookiesFolder', 'Function GetCookiesFolder : string');
  af(@GetHistoryFolder, 'GetHistoryFolder', 'Function GetHistoryFolder : string');
  af(@GetProfileFolder, 'GetProfileFolder', 'Function GetProfileFolder : string');
  af(@CreateShellLink, 'CreateShellLink', 'Function CreateShellLink( const Filename, Description, ShortcutTo, Parameters, WorkingDir, IconFilename : String; const IconIndex, ShowCmd : Integer) : String');
  af(@PathAddSeparator, 'AddBackslash', 'Function AddBackslash( const S : String) : String');
  af(@PathRemoveSeparator, 'RemoveBackslash', 'Function RemoveBackslash( const S : String) : String');
  af(@ExtractFileExt, 'ExtractFileExt', 'Function ExtractFileExt( const FileName : string) : String');
  af(@ExtractFilePath, 'ExtractFilePath', 'Function ExtractFilePath( const FileName : string) : String');
  af(@ExtractFileName, 'ExtractFileName', 'Function ExtractFileName( const FileName : string) : String');
  af(@ExtractFileDrive, 'ExtractFileDrive', 'Function ExtractFileDrive( const FileName : string) : String');
  af(@ExpandFileName, 'ExpandFileName', 'Function ExpandFileName( const FileName : string) : String');
  af(@ChangeFileExt, 'ChangeFileExt', 'Function ChangeFileExt( const FileName, Extension : string) : String');
  af(@RegCreateKey, 'RegCreateKey', 'function RegCreateKey(const RootKey: DelphiHKEY; const Key: string): Longint;');
  af(@RegDeleteEntry, 'RegDeleteEntry', 'function RegDeleteEntry(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;');
  af(@RegDeleteKeyTree, 'RegDeleteKeyTree', 'function RegDeleteKeyTree(const RootKey: DelphiHKEY; const Key: string): Boolean;');
  af(@RegReadBoolDef, 'RegReadBoolDef', 'function RegReadBoolDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Boolean): Boolean;');
  af(@RegReadIntegerDef, 'RegReadIntegerDef', 'function RegReadIntegerDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Integer): Integer;');
  af(@RegReadDoubleDef, 'RegReadDoubleDef', 'function RegReadDoubleDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Double): Double;');
  af(@RegReadStringDef, 'RegReadStringDef', 'function RegReadStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: string): string;');
  af(@RegWriteBool, 'RegWriteBool', 'procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; Value: Boolean);');
  af(@RegWriteInteger, 'RegWriteInteger', 'procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; Value: Integer);');
  af(@RegWriteDouble, 'RegWriteDouble', 'procedure RegWriteDouble(const RootKey: DelphiHKEY; const Key, Name: string; Value: Double);');
  af(@RegWriteString, 'RegWriteString', 'procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name, Value: string);');
  af(@RegGetValueNames, 'RegGetValueNames', 'function RegGetValueNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;');
  af(@RegGetKeyNames, 'RegGetKeyNames', 'function RegGetKeyNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;');
  af(@RegHasSubKeys, 'RegHasSubKeys', 'function RegHasSubKeys(const RootKey: DelphiHKEY; const Key: string): Boolean;');
  af(@RegKeyExists, 'RegKeyExists', 'function RegKeyExists(const RootKey: DelphiHKEY; const Key: string): Boolean;');
  af(@IniReadBool, 'IniReadBool', 'function IniReadBool(const FileName, Section, Line: string): Boolean;');
  af(@IniReadInteger, 'IniReadInteger', 'function IniReadInteger(const FileName, Section, Line: string): Integer;');
  af(@IniReadString, 'IniReadString', 'function IniReadString(const FileName, Section, Line: string): string;');
  af(@IniWriteBool, 'IniWriteBool', 'procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);');
  af(@IniWriteInteger, 'IniWriteInteger', 'procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);');
  af(@IniWriteString, 'IniWriteString', 'procedure IniWriteString(const FileName, Section, Line, Value: string);');
  af(@ShowMessage, 'ShowMessage', 'Procedure ShowMessage( S : String)');
  af(@CreateOleObject, 'CreateOleObject', 'Function CreateOleObject( Classname : String) : Variant');
  af(@Now, 'Now', 'Function Now : TDateTime');
  af(@DateToStr, 'DateToStr', 'Function DateToStr( Date : TDateTime) : String');
  af(@TimeToStr, 'TimeToStr', 'Function TimeToStr( Date : TDateTime) : String');
  af(@DateTimeToStr, 'DateTimeToStr', 'Function DateTimeToStr( Date : TDateTime) : String');
  af(@StrToDateTime, 'StrToDateTime', 'Function StrToDateTime( Date : String) : TDateTime');
  af(@Writeln, 'Writeln', 'Procedure Writeln( S : String)');
  af(@Readln, 'Readln', 'Function Readln( Question : String) : String');
  af(@VarGet, 'VarGet', 'Function VarGet( VarName : String) : Variant');
  af(@VarCount, 'VarCount', 'Function VarCount : Integer');
  af(@VarByIdx, 'VarByIdx', 'Function VarByIdx( Idx : Integer) : Variant');
  af(@VarAdd, 'VarAdd', 'Procedure VarAdd( VarName : String)');
  af(@VarExists, 'VarExists', 'Function VarExists( VarName : String) : Boolean');
  af(@VarSet, 'VarSet', 'Procedure VarSet( VarName : String; Content : Variant)');
  af(@VarReplace, 'VarReplace', 'Function VarReplace( S : String) : String');
  af(@Exec, 'Exec', 'Function Exec( App, Args, Dir : String) : Integer');

  af(@Format, 'Format', 'function Format(const Format: string; const Args: array of const): string;');
  af(@SameText, 'SameText', 'function SameText(const S1, S2: string): Boolean;');
  af(@StrUpper, 'StrUpper', 'function StrUpper(const S: String):String;');
  af(@Trim, 'Trim', 'function Trim(const S: string): string;');
  af(@TrimLeft, 'TrimLeft', 'function TrimLeft(const S: string): string;');
  af(@TrimRight, 'TrimRight', 'function TrimRight(const S: string): string;');
end;

//File Functions
function FileOrDirExists(const Name: String): Boolean;
begin
  Result := JclFileUtils.FileExists( Name) or JclFileUtils.DirectoryExists( Name);
end;

//Folders
procedure SetCurrentFolder(const Folder: string);
begin
{$IFDEF UNICODE}
  Windows.SetCurrentDirectory( PChar(Folder));
{$ELSE}
  Windows.SetCurrentDirectory( PAnsiChar( Folder));
{$ENDIF}
end;

//Shell
function CreateShellLink(const Filename, Description, ShortcutTo, Parameters, WorkingDir, IconFilename: String; const IconIndex, ShowCmd: Integer): String;
var SL:TShellLink;
begin
  SL.Description := Description;
  SL.Target := ShortcutTo;
  SL.Arguments := Parameters;
  SL.WorkingDirectory := WorkingDir;
  SL.IconLocation := IconFilename;
  SL.IconIndex := IconIndex;
  SL.ShowCmd := ShowCmd;
  ShellLinkCreate( SL, Filename);
end;


//Jedi Make Functions
procedure Writeln( S:String);
begin
  MakeStudio.LogMessage( S);
end;

function Readln( Question:String):String;
begin
  Result := InputBox( Question, '', '');
end;

function VarGet( VarName:String):Variant;
begin
  if VarExists( VarName) then
    Result := MakeStudio.Variables.Values[ Varname]
  else
    Result := '';
end;

function VarCount:Integer;
begin
  Result := MakeStudio.Variables.Count;
end;

function VarByIdx( Idx:Integer):Variant;
begin
  Result := MakeStudio.Variables.ValuesByIdx[ Idx];
end;

procedure VarAdd( VarName:String);
begin
  MakeStudio.Variables.AddVar( Varname);
end;

function VarExists( VarName:String):Boolean;
begin
  Result := MakeStudio.Variables.VarExists( VarName);
end;

procedure VarSet( VarName:String; Content:Variant);
begin
  if VarExists( VarName) then
    MakeStudio.Variables.Values[ Varname] := Content
  else begin
    MakeStudio.Variables.AddVar( VarName);
    MakeStudio.Variables.Values[ Varname] := Content;
  end;
end;

function VarReplace( S:String):String;
begin
  Result := MakeStudio.Variables.ReplaceVarsInString( S);
end;

function Exec( App, Args, Dir:String):Integer;
begin
  Result := MakeStudio.ExecCmdLine( App, Args, Dir, nil);
end;


end.
