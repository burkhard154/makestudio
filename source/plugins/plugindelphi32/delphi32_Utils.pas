(* -----------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: delphi32_Utils.pas

  The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

  Componentes and used code which is used in this code are explictly stated to
  be copyright of the respective author(s).

  Last Modified: see History

  Known Issues:
  -----------------------------------------------------------------------------

  Unit history:

  2005/01/04  BSchranz  - Plugin created
  2005/02/04  USchuster - preparations for check in
  2005/02/25  BSchranz  - Using long path names for D7
  2005/07/10  USchuster - fix to detect D5
  2006/03/05  BSchranz  - Modified GetVersionText
  2006/04/11  BSchranz  - Register IDE DLL Expert support added
  2006/04/12  BSchranz  - support for borlands .dof file in compilation added
  2006/04/12  BSchranz  - support for package prefix and suffix added
  2006/04/30  USchuster - D5+D6 fix

  ----------------------------------------------------------------------------- *)

unit delphi32_Utils;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ActnList, StdCtrls, ImgList, ExtCtrls, Menus, ToolWin, Contnrs,
  ShellAPI, Registry, makestudio_TLB, ActiveX, delphi32_Vars, JclFileUtils,
  JclSysInfo, JclShell, JclWin32, JvJclUtils, System.UITypes;

Type
  // :Options to control omParseString
  TomParseOption = (omPoRegExpr // :Separator becomes a RegExp to locate the splitting characters
    , omPoTrimFields // :each field is cleaned from white space (leading, trailing)
    , omPoNoEmptyFields // :empty fields are not added
    , omPoDequote // :remove any pair of quotes "..." or '...' (extra trim before and omPoTrimFields applied after)
    , omPoAddPathDelim
    // :treating entries as path name: verify and fix that path ends on delimiter           <path>\<name>\
    , omPoRemPathDelim
    // :treating entries as path name: verify and fix that path does not end on delimiter   <path>\<name>
    , omPoUniqueOnly // :Add entry only, if not already contained
    , omPoCaseSensitive // :Handle Unique search case sensitive (default: SameText-compare)
    , omPoClear // :Clear list first (default: append elements)
    );
  TomParseOptions = set of TomParseOption;

  // :Options to control omPrefixStrings
  TomPrefixOption = (omPrSingleQuotes // :Force '...' - quotes for each entry (removing other quotes)
    , omPrDoubleQuotes // :Force "..." - quotes for each entry (removing other quotes)
    , omPrDequoteTrimFields // :each field is cleaned from quotes and white space (leading, trailing)
    , omPrNoEmptyFields // :empty fields are not added
    , omPrAddPathDelim
    // :treating entries as path name: verify and fix that path ends on delimiter           <path>\<name>\
    , omPrRemPathDelim
    // :treating entries as path name: verify and fix that path does not end on delimiter   <path>\<name>
    , omPrUniqueOnly // :Add entry only, if not already contained
    , omPrCaseSensitive // :Handle Unique search case sensitive (default: SameText-compare)
    , omPrClear // :clear AStringsOut first (default: append elements)
    );
  TomPrefixOptions = set of TomPrefixOption;

  { :like includetrailingpathdelimiter }
function CheckBackslash(const Value: string): string;
{ :like excludetrailingpathdelimiter }
function CheckNoBackslash(const Value: string): string;
{ :Gets the Delphi-Searchpath }
function GetDelphiSearchPath: string;
{ :Gets the Delphi-Library Language Path - starting with XE2 }
function GetDelphiLangPath: string;
{ :Gets the Delphi-Installation directory }
function GetDelphiRootPath: string;
{ :Returns the Registry Key for Delphi depending on the selected Delphi Version }
function GetDelphiRootKey: string;
{ :Returns then used Delphi Version }
function GetDelphiVersion: TDelphiVersion;
{ :Returns then used Delphi Version }
function GetCompilerPlatform: TCompilerPlatform;
{ :Returns the version as text }
function GetVersionText: string;
function GetVersionTextEx(AVersion: TDelphiVersion): string;
{ :Sets the used Compiler Platform - starts at Delphi XE2 }
procedure SetCompilerPlatform(APlatform: TCompilerPlatform);
{ :Sets the used Delphi Version }
procedure SetDelphiVersion(AVersion: TDelphiVersion);
{ :Replaces all variables in a Delphi path like $(Delphi) or $(BDS) }
function ReplaceDelphiPathVars(APath: string): string;
{ :Returns the selected Delphi version Default is 5.0 }
procedure ReadDelphiVersionReg;
{ :Writes the selected Delphi-Version into the registry }
procedure WriteDelphiVersionReg;
{ :Returns the last Windows Error message }
procedure LastErrorMsg;
{ :Adds a search path list to the delphi search path list }
procedure AddPathListToDelphiPath(PathList: TStringList);
{ :Removes a search path list from the delphi search path list
  if IncludeSubDirs = true then all Subdirectory Entries are removed }
procedure RemovePathListFromDelphiPath(PathList: TStringList; IncludeSubdirs: Boolean = False);
{ :Returns true if GetDelphiversion is an installed Version }
function DelphiVersionInstalled: Boolean;
function CheckDelphiVersion(AVersion: TDelphiVersion): Boolean;
function GetBDSVersion: String;
function GetDelphiCompiler: string;
function GetBRCC32Compiler: string;
function GetLibraryKey: String;
function GetDelphiRootPathLong: string;
function GetDelphiBPLPath: string;
function GetDelphiDCPPath: string;
function GetPlatformString: String;
function GetLANGDIR: String;
function GetBDSLIB: String;
function GetBDSBIN: String;
function GetPackageRunOnly(Filename: string): Boolean;
function GetPackageInfo(Filename: string; Key: string): string;
function GetPackageDescription(Filename: string): string;
function GetPackageSuffix(Filename: string): string;
function GetPackagePrefix(Filename: string): string;
function GetPackageVersion(Filename: string): string;
{ :Returns the required packages List in "Requires" }
function GetPackageDepencies(Filename: string; Requires: TStrings): Boolean;
{ :Checks if the Version is ready to work }
function CheckAllDelphiVersions: Boolean;
// :Determines if the File is a Delphi Package
function GetIsPackage(Filename: string): Boolean;
// :Determines if the file is a Delphi Project
function GetIsProject(Filename: string): Boolean;
// :Determines if the file is a .dll IDE Expert
function GetIsProjectExpert(Filename: string): Boolean;
// :Compiler Version needed for "Libsuffix=Auto" in Packages >= Delphi 10.4 / Sydney
function GetCompilerVersion: String;

function GetBDSProjectsPath: string;
function GetBDSCommonPath: string;
function GetBDSUserPath: string;

// :Publishes all Delphi Informations in the Make-Variables List
procedure PublishVars;
// :Wait
procedure Wait_a_While(ms: Cardinal);
// :Writes a Stringlist to the global log
procedure AddLogStrings(sl: TStrings);

// :Only working on W2000 and XP
procedure SetSystemEnvironmentVariable(const Name, Value: string);
function GetSystemEnvironmentVariable(const Name: string): string;

// *********** Registry functions **************************************
{ :Writes a String into XUTILSROOTKEY of the registry }
procedure WriteRegStringLM(const Key, Name, Value: string);
{ :Reads a String from XUTILSROOTKEY of the registry }
function ReadRegStringLM(const Key, Name, Default: string): string;
{ :Writes a Integer into XUTILSROOTKEY of the registry }
procedure WriteRegIntLM(const Key, Name: string; Value: Integer);
{ :Reads a Integer from XUTILSROOTKEY of the registry }
function ReadRegIntLM(const Key, Name: string; Default: Integer): Integer;
{ :Writes a Float into XUTILSROOTKEY of the registry }
procedure WriteFloatIntLM(const Key, Name: string; Value: Double);
{ :Reads a fload from XUTILSROOTKEY of the registry }
function ReadRegFloatLM(const Key, Name: string; Default: Double): Double;

// :Parse a ";" separated string and return every item in quotes "s1";"s2";...
function QuoteSeparetedList(Separated: String): String;

// functions copied from om/omUtils, an optiMEAS private framework (JAK, 2016)
procedure omParseString(const S: string; Separator: char; AStrings: TStrings); overload;
procedure omParseString(const S: string; Separator: string; AStrings: TStrings; parseOptions: TomParseOptions);
  overload;
procedure omPrefixStrings(prefix: string; AStringsIn: TStrings; prefixOptions: TomPrefixOptions;
  AStringsOut: TStrings = nil);

var
  XUTILSROOTKEY: DWORD = HKEY_CURRENT_USER;

implementation

uses
{$IFDEF DELPHI7_UP}
  StrUtils,
{$ENDIF DELPHI7_UP}
  System.RegularExpressions,
  delphi32_SelectDelphiVersion, delphi32_BDSEnvironment;

var
  // stores the selected Delphi Version
  _DelphiVersion: TDelphiVersion = dver5;
  _CompilerPlatform: TCompilerPlatform = dpWin32;

{$IFNDEF D5TODO}
{$IFDEF DELPHI5}

function GetEnvironmentVariable(const Name: string): string; overload;
begin
  if not GetEnvironmentVar(Name, Result) then
    Result := '';
end;
{$ENDIF DELPHI5}
{$ENDIF ~D5TODO}

function GetSystemEnvironmentVariable(const Name: string): string;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('SYSTEM\CurrentControlSet\Control\Session ' + 'Manager\Environment', False);
      Result := ReadString(Name);
    finally
      Free;
    end
end;

procedure SetSystemEnvironmentVariable(const Name, Value: string);
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment', False);
      WriteString(Name, Value);
      SetEnvironmentVariable(PChar(Name), PChar(Value));

      { Send Message To All Top Window for Refresh }
      SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, Integer(PChar('Environment')));
    finally
      Free;
    end
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

procedure Wait_a_While(ms: Cardinal);
var
  st: Cardinal;
begin
  st := GetTickCount;
  while GetTickCount - st < ms do
    Application.ProcessMessages;
end;

procedure AddLogStrings(sl: TStrings);
var
  I: Integer;
begin
  for I := 0 to sl.Count - 1 do
    MakeStudio.LogMessage(sl[I]);
end;

function GetDelphiVersion: TDelphiVersion;
begin
  Result := _DelphiVersion;
end;

function GetCompilerPlatform: TCompilerPlatform;
begin
  Result := _CompilerPlatform;
end;

function GetVersionText: string;
begin
  Result := GetVersionTextEx(GetDelphiVersion);
end;

function GetVersionTextEx(AVersion: TDelphiVersion): string;
begin
  case AVersion of
    dver5:
      Result := stdver5;
    dver6:
      Result := stdver6;
    dver7:
      Result := stdver7;
    dver2005:
      Result := stdver2005;
    dver2006:
      Result := stdver2006;
    dver2007:
      Result := stdver2007;
    dver2009:
      Result := stdver2009;
    dver2010:
      Result := stdver2010;
    dverXE:
      Result := stdverXE;
    dverXE2:
      Result := stdverXE2;
    dverXE3:
      Result := stdverXE3;
    dverXE4:
      Result := stdverXE4;
    dverXE5:
      Result := stdverXE5;
    dverXE6:
      Result := stdverXE6;
    dverXE7:
      Result := stdverXE7;
    dverXE8:
      Result := stdverXE8;
    dverD10S:
      Result := stdverD10S;
    dverD101B:
      Result := stdverD101B;
    dverD102T:
      Result := stdverD102T;
    dverD103R:
      Result := stdverD103R;
    dverD104S:
      Result := stdverD104S;
    dverD11A:
      Result := stdverD11A;
    dverD12A:
      Result := stdverD12A;
    dverD13:
      Result := stdverD13;
  end;
end;

function GetBDSVersion: String;
begin
  case GetDelphiVersion of
    dver2005:
      Result := PathRemoveSeparator(stDelphi9Key);
    dver2006:
      Result := PathRemoveSeparator(stDelphi10Key);
    dver2007:
      Result := PathRemoveSeparator(stDelphi11Key);
    dver2009:
      Result := PathRemoveSeparator(stDelphi12Key);
    dver2010:
      Result := PathRemoveSeparator(stDelphi14Key);
    dverXE:
      Result := PathRemoveSeparator(stDelphi15Key);
    dverXE2:
      Result := PathRemoveSeparator(stDelphi16Key);
    dverXE3:
      Result := PathRemoveSeparator(stDelphi17Key);
    dverXE4:
      Result := PathRemoveSeparator(stDelphi18Key);
    dverXE5:
      Result := PathRemoveSeparator(stDelphi19Key);
    dverXE6:
      Result := PathRemoveSeparator(stDelphi20Key);
    dverXE7:
      Result := PathRemoveSeparator(stDelphi21Key);
    dverXE8:
      Result := PathRemoveSeparator(stDelphi22Key);
    dverD10S:
      Result := PathRemoveSeparator(stDelphi23Key);
    dverD101B:
      Result := PathRemoveSeparator(stDelphi24Key);
    dverD102T:
      Result := PathRemoveSeparator(stDelphi25Key);
    dverD103R:
      Result := PathRemoveSeparator(stDelphi26Key);
    dverD104S:
      Result := PathRemoveSeparator(stDelphi27Key);
    dverD11A:
      Result := PathRemoveSeparator(stDelphi28Key);
    dverD12A:
      Result := PathRemoveSeparator(stDelphi29Key);
    dverD13:
      Result := PathRemoveSeparator(stDelphi30Key);
  else
    Result := '';
  end;
end;

procedure SetCompilerPlatform(APlatform: TCompilerPlatform);
begin
  if _DelphiVersion > dverXE then
  begin
    _CompilerPlatform := APlatform;
  end
  else
    _CompilerPlatform := dpWin32;

  MakeStudio.LogMessage('Compiler platform set to: ' + GetPlatformString);
end;

procedure SetDelphiVersion(AVersion: TDelphiVersion);
var
  V: TDelphiVersion;
begin

  // Check if the Delphi Version is valid
  if not CheckDelphiVersion(AVersion) then
  begin
    MessageDlg(stderrNoDelphi, mtInformation, [mbOk], 0);
    DlgSelectDelphiVersion(AVersion);
    if not CheckDelphiVersion(AVersion) then
      Exit
    else
    begin
      _DelphiVersion := AVersion;
      WriteDelphiVersionReg;
    end;
  end;

  V := _DelphiVersion;
  _DelphiVersion := AVersion;

  case _DelphiVersion of
    dver5, dver6:
      begin
        Var_Delphi := GetDelphiRootPath;
      end;
    dver7:
      begin
        Var_Delphi := GetDelphiRootPathLong;
      end;
    dver2005, dver2006, dver2007, dver2009, dver2010, dverXE, dverXE2, dverXE3, dverXE4, dverXE5, dverXE6, dverXE7,
      dverXE8, dverD10S, dverD101B, dverD102T, dverD103R, dverD104S, dverD11A, dverD12A, dverD13:
      begin
        Var_BDS := GetDelphiRootPathLong;

        // Check if the BDSProjectsDir is valid
        if GetBDSProjectsPath = '' then
        begin
          if not DlgBDSEnvironment(GetDelphiVersion) then
          begin
            _DelphiVersion := V;
            Var_BDS := GetDelphiRootPathLong;
            Exit;
          end;
        end;

        Var_BDSProjectsDir := GetBDSProjectsPath;
      end;
  end;

  // publish vars in jedi make
  PublishVars;
end;

function CheckDelphiVersion(AVersion: TDelphiVersion): Boolean;
var
  S: string;
  tempKey: DWORD;
begin
  Result := False;
  S := '';
  case AVersion of
    dver5:
      begin
        tempKey := XUTILSROOTKEY;
        try
          XUTILSROOTKEY := HKEY_LOCAL_MACHINE;
          S := PathAddSeparator(ReadRegStringLM(stDelphiRootKeyLMDXX + stDelphi5Key, stDelphiInstallKey, '')) + 'bin\' +
            stDelphiCompiler + '.exe';
        finally
          XUTILSROOTKEY := tempKey;
        end;
      end;
    dver6:
      S := PathAddSeparator(ReadRegStringLM(stDelphiRootKeyLMDXX + stDelphi6Key, stDelphiInstallKey, '')) + 'bin\' +
        stDelphiCompiler + '.exe';

    dver7:
      S := PathAddSeparator(ReadRegStringLM(stDelphiRootKeyLMDXX + stDelphi7Key, stDelphiInstallKey, '')) + 'bin\' +
        stDelphiCompiler + '.exe';

    dver2005:
      S := PathAddSeparator(ReadRegStringLM(stDelphiRootKeyLMBDS + stDelphi9Key, stDelphiInstallKey, '')) + 'bin\' +
        stDelphiCompiler + '.exe';

    dver2006:
      S := PathAddSeparator(ReadRegStringLM(stDelphiRootKeyLMBDS + stDelphi10Key, stDelphiInstallKey, '')) + 'bin\' +
        stDelphiCompiler + '.exe';

    dver2007:
      S := PathAddSeparator(ReadRegStringLM(stDelphiRootKeyLMBDS + stDelphi11Key, stDelphiInstallKey, '')) + 'bin\' +
        stDelphiCompiler + '.exe';

    dver2009:
      S := PathAddSeparator(ReadRegStringLM(stCodeGearDelphiRootKeyLMBDS + stDelphi12Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';

    dver2010:
      S := PathAddSeparator(ReadRegStringLM(stCodeGearDelphiRootKeyLMBDS + stDelphi14Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';

    dverXE:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi15Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverXE2:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi16Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverXE3:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi17Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverXE4:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi18Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverXE5:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi19Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverXE6:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi20Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverXE7:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi21Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverXE8:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi22Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverD10S:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi23Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverD101B:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi24Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverD102T:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi25Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverD103R:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi26Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverD104S:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi27Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverD11A:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi28Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverD12A:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi29Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
    dverD13:
      S := PathAddSeparator(ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi30Key, stDelphiInstallKey, '')) +
        'bin\' + stDelphiCompiler + '.exe';
  end;
  if S <> '' then
    Result := FileExists(S);
end;

function DelphiVersionInstalled: Boolean;
begin
  Result := CheckDelphiVersion(GetDelphiVersion);
end;

function ReplaceDelphiPathVars(APath: string): string;
begin
  case GetDelphiVersion of
    dver5, dver6, dver7:
      begin
        Result := StringReplace(APath, '$(DELPHI)', Var_Delphi, [rfReplaceAll, rfIgnoreCase]);
      end;
    dver2005, dver2006:
      begin
        Result := StringReplace(APath, '$(BDSPROJECTSDIR)', Var_BDSProjectsDir, [rfReplaceAll, rfIgnoreCase]);
        Result := StringReplace(Result, '$(BDS)', Var_BDS, [rfReplaceAll, rfIgnoreCase]);
      end;
    dver2007, dver2009, dver2010, dverXE, dverXE2 .. dverD13:
      begin
        Result := StringReplace(APath, '$(BDSPROJECTSDIR)', GetBDSProjectsPath, [rfReplaceAll, rfIgnoreCase]);
        Result := StringReplace(Result, '$(BDSCOMMONDIR)', GetBDSCommonPath, [rfReplaceAll, rfIgnoreCase]);
        Result := StringReplace(Result, '$(BDSUSERDIR)', GetBDSUserPath, [rfReplaceAll, rfIgnoreCase]);
        Result := StringReplace(Result, '$(BDS)', Var_BDS, [rfReplaceAll, rfIgnoreCase]);
        case GetDelphiVersion of
          dverXE:
            begin
              Result := StringReplace(Result, '$(BDSLIB)', GetBDSLIB, [rfReplaceAll, rfIgnoreCase]);
              // Platform, LANGDIR, BDSLIB, BDSBIN
            end;
          dverXE2 .. dverD13:
            begin
              Result := StringReplace(Result, '$(PLATFORM)', GetPlatformString, [rfReplaceAll, rfIgnoreCase]);
              Result := StringReplace(Result, '$(LANGDIR)', GetLANGDIR, [rfReplaceAll, rfIgnoreCase]);
              Result := StringReplace(Result, '$(BDSLIB)', GetBDSLIB, [rfReplaceAll, rfIgnoreCase]);
              // Platform, LANGDIR, BDSLIB, BDSBIN
            end;
        end;
      end;
  end;
  Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
end;

function GetIsPackage(Filename: string): Boolean;
begin
  Result := SameText(ExtractFileExt(Filename), stExtPackage);
end;

function GetIsProject(Filename: string): Boolean;
begin
  Result := SameText(ExtractFileExt(Filename), stExtProject);
end;

function CheckBackslash(const Value: string): string;
begin
  Result := PathAddSeparator(Value);
end;

function CheckNoBackslash(const Value: string): string;
begin
  Result := PathRemoveSeparator(Value);
end;

procedure AddPathListToDelphiPath(PathList: TStringList);
var
  S, S1: string;
  I: Integer;
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(GetDelphiRootKey + GetLibraryKey) then
    begin
      reg.OpenKey(GetDelphiRootKey + GetLibraryKey, False);
      S := GetDelphiSearchPath;
      if (S <> '') and (S[Length(S)] <> ';') then
        S := S + ';';
      for I := 0 to PathList.Count - 1 do
      begin
        S1 := MakeStudio.Variables.ReplaceVarsInString(PathList[I]);
        if Pos(UpperCase(S1 + ';'), UpperCase(S)) = 0 then
        begin
          S := S + S1 + ';';
          MakeStudio.LogMessage(Format(stdAddingSearchPath, [S1]));
        end
        else
        begin
          MakeStudio.LogMessage(Format(stdSearchPathAlreadyAdded, [S1]));
        end;
      end;
      try
        if S[Length(S)] = ';' then
          SetLength(S, Length(S) - 1);
        reg.WriteString(stdcSearchPath, S);
      except
        On E: Exception do
        begin
          MakeStudio.LogMessage(E.Message);
        end;
      end;
    end
    else
      MakeStudio.LogMessage(Format(StrRegistryKeyNotExist, [GetDelphiRootKey + GetLibraryKey]));
  finally
    reg.Free;
  end;
end;

procedure RemovePathListFromDelphiPath(PathList: TStringList; IncludeSubdirs: Boolean = False);
var
  S: string;
  sl: TStringList;
  I, K: Integer;
  reg: TRegistry;

  function Normalize(S: string): string;
  begin
    Result := MakeStudio.Variables.ReplaceVarsInString(UpperCase(PathRemoveSeparator(S)));
  end;

begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey(GetDelphiRootKey + GetLibraryKey, False);
    S := GetDelphiSearchPath;

    if (S <> '') then
      if (S[Length(S)] = ';') then
        SetLength(S, Length(S) - 1);

    sl := TStringList.Create;
    try
      sl.Text := StringReplace(S, ';', #10, [rfReplaceAll]);

      for I := 0 to PathList.Count - 1 do
      begin

        K := 0;
        while K < sl.Count do
        begin
          if IncludeSubdirs then
          begin
            if Pos(Normalize(PathList[I]), Normalize(sl[K])) > 0 then
            begin
              MakeStudio.LogMessage(Format(stdRemoveSearchPath, [sl[K]]));
              sl.Delete(K)
            end
            else
              Inc(K);
          end
          else
          begin
            if Normalize(PathList[I]) = Normalize(sl[K]) then
            begin
              MakeStudio.LogMessage(Format(stdRemoveSearchPath, [sl[K]]));
              sl.Delete(K)
            end
            else
              Inc(K);
          end;
        end;

      end;

      S := '';
      for I := 0 to sl.Count - 1 do
      begin
        S := S + sl[I];
        if I < sl.Count - 1 then
          S := S + ';';
      end;

    finally
      sl.Free;
    end;

    reg.WriteString(stdcSearchPath, S);

  finally
    reg.Free;
  end;
end;

function GetDelphiLangPath: string;
begin
  Result := '';
  if GetDelphiVersion > dverXE then
    Result := ReplaceDelphiPathVars(ReadRegStringLM(GetDelphiRootKey + GetLibraryKey, stdcLanguageLibraryPath,
      '')) + ';';
  Result := StringReplace(Result, ';;', ';', [rfReplaceAll]);
end;

function GetDelphiSearchPath: string;
begin
  Result := ReplaceDelphiPathVars(ReadRegStringLM(GetDelphiRootKey + GetLibraryKey, stdcSearchPath, ''));
  Result := StringReplace(Result, ';;', ';', [rfReplaceAll]);
end;

function GetDelphiRootKey: string;
begin
  Result := '';
  case GetDelphiVersion of
    dver5:
      Result := stDelphiRootKeyDXX + stDelphi5Key;
    dver6:
      Result := stDelphiRootKeyDXX + stDelphi6Key;
    dver7:
      Result := stDelphiRootKeyDXX + stDelphi7Key;
    dver2005:
      Result := stDelphiRootKeyBDS + stDelphi9Key;
    dver2006:
      Result := stDelphiRootKeyBDS + stDelphi10Key;
    dver2007:
      Result := stDelphiRootKeyBDS + stDelphi11Key;
    dver2009:
      Result := stCodeGearDelphiRootKeyBDS + stDelphi12Key;
    dver2010:
      Result := stCodeGearDelphiRootKeyBDS + stDelphi14Key;
    dverXE:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi15Key;
    dverXE2:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi16Key;
    dverXE3:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi17Key;
    dverXE4:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi18Key;
    dverXE5:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi19Key;
    dverXE6:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi20Key;
    dverXE7:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi21Key;
    dverXE8:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi22Key;
    dverD10S:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi23Key;
    dverD101B:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi24Key;
    dverD102T:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi25Key;
    dverD103R:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi26Key;
    dverD104S:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi27Key;
    dverD11A:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi28Key;
    dverD12A:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi29Key;
    dverD13:
      Result := stEmbarcaderoDelphiRootKeyBDS + stDelphi30Key;
  end;
end;

function GetDelphiCompiler: string;
begin
  if GetDelphiVersion > dverXE then
    case GetCompilerPlatform of
      dpOSX32:
        Result := PathAddSeparator(GetDelphiRootPath) + 'bin\dccosx.exe';
      dpWin32:
        Result := PathAddSeparator(GetDelphiRootPath) + 'bin\dcc32.exe';
      dpWin64:
        Result := PathAddSeparator(GetDelphiRootPath) + 'bin\dcc64.exe';
      dpiOSDevice:
        Result := PathAddSeparator(GetDelphiRootPath) + 'bin\dcciosarm.exe';
      dpiOSSimulator:
        Result := PathAddSeparator(GetDelphiRootPath) + 'bin\dccios32.exe';
      dpAndroid32:
        Result := PathAddSeparator(GetDelphiRootPath) + 'bin\dccaarm.exe';
    end
  else
    Result := CheckBackslash(GetDelphiRootPath) + 'Bin\' + stDelphiCompiler + '.exe';
end;

function GetBRCC32Compiler: string;
begin
  Result := CheckBackslash(GetDelphiRootPath) + 'Bin\' + stBRCC32Compiler + '.exe';
end;

procedure LastErrorMsg;
var
  ch: array [0 .. 511] of char;
begin
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, LANG_NEUTRAL,
    // Default language
    ch, 511, nil);
  MakeStudio.LogMessage(StrPas(ch));
end;

function GetLibraryKey: String;
begin
  if (GetDelphiVersion > dverXE) and (GetDelphiVersion < dverD103R) then
  begin
    case _CompilerPlatform of
      dpOSX32:
        Result := stdLibraryKey + '\' + stdcPlatformOSX32;
      dpWin32:
        Result := stdLibraryKey + '\' + stdcPlatformWIN32;
      dpWin64:
        Result := stdLibraryKey + '\' + stdcPlatformWIN64;
      dpiOSDevice:
        Result := stdLibraryKey + '\' + stdcPlatformIOSDevice;
      dpiOSSimulator:
        Result := stdLibraryKey + '\' + stdcPlatformIOSSimulator;
      dpAndroid32:
        Result := stdLibraryKey + '\' + stdcPlatformAndroid32;
    end;
  end
  else if GetDelphiVersion > dverD102T then
  begin
    case _CompilerPlatform of
      dpOSX32:
        Result := stdLibraryKey + '\' + stdcPlatformOSX32;
      dpOSX64:
        Result := stdLibraryKey + '\' + stdcPlatformOSX64;
      dpWin32:
        Result := stdLibraryKey + '\' + stdcPlatformWIN32;
      dpWin64:
        Result := stdLibraryKey + '\' + stdcPlatformWIN64;
      dpiOSDevice:
        Result := stdLibraryKey + '\' + stdcPlatformIOSDevice;
      dpiOSDevice32:
        Result := stdLibraryKey + '\' + stdcPlatformIOSDevice32;
      dpiOSDevice64:
        Result := stdLibraryKey + '\' + stdcPlatformIOSDevice64;
      dpiOSSimulator:
        Result := stdLibraryKey + '\' + stdcPlatformIOSSimulator;
      dpAndroid32:
        Result := stdLibraryKey + '\' + stdcPlatformAndroid32;
      dpAndroid64:
        Result := stdLibraryKey + '\' + stdcPlatformAndroid64;
    end;
  end
  else
    Result := stdLibraryKey;
end;

function CompilerPlatformToStr(CompilerPlatform: TCompilerPlatform): String;
begin
  case _CompilerPlatform of
    dpOSX32:
      Result := stdcPlatformOSX32;
    dpOSX64:
      Result := stdcPlatformOSX64;
    dpWin32:
      Result := stdcPlatformWIN32;
    dpWin64:
      Result := stdcPlatformWIN64;
    dpiOSDevice:
      Result := stdcPlatformIOSDevice;
    dpiOSDevice32:
      Result := stdcPlatformIOSDevice32;
    dpiOSDevice64:
      Result := stdcPlatformIOSDevice64;
    dpiOSSimulator:
      Result := stdcPlatformIOSSimulator;
    dpAndroid32:
      Result := stdcPlatformAndroid32;
    dpAndroid64:
      Result := stdcPlatformAndroid64;
  else
    Result := '';
  end;
end;

function GetDelphiRootPathLong: string;
begin
  XUTILSROOTKEY := HKEY_LOCAL_MACHINE;
  Result := '';
  case GetDelphiVersion of
    dver5:
      Result := ReadRegStringLM(stDelphiRootKeyLMDXX + stDelphi5Key, stDelphiInstallKey, '');
    dver6:
      Result := ReadRegStringLM(stDelphiRootKeyLMDXX + stDelphi6Key, stDelphiInstallKey, '');
    dver7:
      Result := ReadRegStringLM(stDelphiRootKeyLMDXX + stDelphi7Key, stDelphiInstallKey, '');
    dver2005:
      Result := ReadRegStringLM(stDelphiRootKeyLMBDS + stDelphi9Key, stDelphiInstallKey, '');
    dver2006:
      Result := ReadRegStringLM(stDelphiRootKeyLMBDS + stDelphi10Key, stDelphiInstallKey, '');
    dver2007:
      Result := ReadRegStringLM(stDelphiRootKeyLMBDS + stDelphi11Key, stDelphiInstallKey, '');
    dver2009:
      Result := ReadRegStringLM(stCodeGearDelphiRootKeyLMBDS + stDelphi12Key, stDelphiInstallKey, '');
    dver2010:
      Result := ReadRegStringLM(stCodeGearDelphiRootKeyLMBDS + stDelphi14Key, stDelphiInstallKey, '');
    dverXE:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi15Key, stDelphiInstallKey, '');
    dverXE2:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi16Key, stDelphiInstallKey, '');
    dverXE3:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi17Key, stDelphiInstallKey, '');
    dverXE4:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi18Key, stDelphiInstallKey, '');
    dverXE5:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi19Key, stDelphiInstallKey, '');
    dverXE6:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi20Key, stDelphiInstallKey, '');
    dverXE7:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi21Key, stDelphiInstallKey, '');
    dverXE8:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi22Key, stDelphiInstallKey, '');
    dverD10S:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi23Key, stDelphiInstallKey, '');
    dverD101B:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi24Key, stDelphiInstallKey, '');
    dverD102T:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi25Key, stDelphiInstallKey, '');
    dverD103R:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi26Key, stDelphiInstallKey, '');
    dverD104S:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi27Key, stDelphiInstallKey, '');
    dverD11A:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi28Key, stDelphiInstallKey, '');
    dverD12A:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi29Key, stDelphiInstallKey, '');
    dverD13:
      Result := ReadRegStringLM(stEmbarcaderoDelphiRootKeyLMBDS + stDelphi30Key, stDelphiInstallKey, '');
  end;

  if Result <> '' then
    Result := PathAddSeparator(Result);

  XUTILSROOTKEY := HKEY_CURRENT_USER;
end;

function GetDelphiRootPath: string;
begin
  if GetDelphiVersion < dver2005 then
    Result := ExtractShortPathName(GetDelphiRootPathLong)
  else
    Result := GetDelphiRootPathLong;
end;

function GetCoreIdeVersion: string;
begin
  Result := '70';
  case GetDelphiVersion of
    dver5:
      Result := '50';
    dver6:
      Result := '60';
    dver7:
      Result := '70';
    dver2005:
      Result := '90';
    dver2006:
      Result := '100';
    dver2007:
      Result := '100';
    dver2009:
      Result := '120';
    dver2010:
      Result := '140';
    dverXE:
      Result := '150';
    dverXE2:
      Result := '160';
    dverXE3:
      Result := '170';
    dverXE4:
      Result := '180';
    dverXE5:
      Result := '190';
    dverXE6:
      Result := '200';
    dverXE7:
      Result := '210';
    dverXE8:
      Result := '220';
    dverD10S:
      Result := '230';
    dverD101B:
      Result := '240';
    dverD102T:
      Result := '250';
    dverD103R:
      Result := '260';
    dverD104S:
      Result := '270';
    dverD11A:
      Result := '280';
    dverD12A:
      Result := '290';
    dverD13:
      Result := '370';
  end;
end;

function GetBDSProjectsDirResID: Integer;
begin
  Result := 0;
  case GetDelphiVersion of
    dver5:
      Result := 0;
    dver6:
      Result := 0;
    dver7:
      Result := 0;
    dver2005:
      Result := 64431;
    dver2006:
      Result := 64719;
    dver2007:
      Result := 64382;
    dver2009:
      Result := 64367;
    dver2010:
      Result := 64511;
    dverXE .. dverD13:
      Result := 0;
  end;
end;

function GetBDSProjectsBaseDirResID: Integer;
begin
  Result := 0;
  case GetDelphiVersion of
    dver5:
      Result := 0;
    dver6:
      Result := 0;
    dver7:
      Result := 0;
    dver2005:
      Result := 0;
    dver2006:
      Result := 0;
    dver2007:
      Result := 64392;
    dver2009:
      Result := 64363;
    dver2010 .. dverD13:
      Result := 0; // N.A. fixed: ..\RAD Studio\7.0\
  end;
end;

function GetBDSRessourceString(ResID: Integer): String;
var
  H: HMODULE;
  LocaleName: array [0 .. 4] of char;
  Filename: string;
begin
  if (GetDelphiVersion >= dver2005) and (ResID > 0) then
  begin
    Result := ''; // do not localize

    FillChar(LocaleName, SizeOf(LocaleName[0]), 0);
    GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
    if LocaleName[0] <> #0 then
    begin
      Filename := GetDelphiRootPath + '\Bin\coreide' + GetCoreIdeVersion + '.';
      if FileExists(Filename + LocaleName) then
        Filename := Filename + LocaleName
      else
      begin
        LocaleName[2] := #0;
        if FileExists(Filename + LocaleName) then
          Filename := Filename + LocaleName
        else
          Filename := '';
      end;

      if Filename <> '' then
      begin
        H := LoadLibraryEx(PChar(Filename), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
        if H <> 0 then
        begin
          SetLength(Result, 1024);
          SetLength(Result, LoadString(H, ResID, PChar(Result), Length(Result) - 1));
          FreeLibrary(H);
        end;
      end;
    end;
  end
  else
    Result := '';
end;

function GetBDSProjectsPathEx: string;
var
  S: String;
begin
  Result := GetBDSRessourceString(GetBDSProjectsDirResID);
  if GetDelphiVersion >= dverXE then
    Result := 'projects';
  if Result <> '' then
  begin
    S := GetBDSRessourceString(GetBDSProjectsBaseDirResID);
    if S <> '' then
      Result := PathAddSeparator(GetPersonalFolder) + S + '\' + Result
    else
    begin
      case GetDelphiVersion of
        dver2009, dver2010, dverXE, dverXE2 .. dverXE5:
          Result := PathAddSeparator(GetPersonalFolder) + 'RAD Studio\' + Result;
        dverXE6 .. dverD13:
          Result := PathAddSeparator(GetPersonalFolder) + 'Embarcadero\Studio\' + Result;
      else
        Result := PathAddSeparator(GetPersonalFolder) + Result;
      end;
    end;
  end
  else if GetDelphiVersion >= dver2005 then
    Result := 'Borland Studio Projects'; // do not localize
end;

function GetBDSProjectsPath: string;
begin
  Result := ReadRegStringLM(GetDelphiRootKey + stdEnvironmentVariablesKey, stdcBdsProjectsDir, '');
  if Result = '' then
    Result := GetBDSProjectsPathEx;
end;

function GetBDSCommonPathEx: string;
begin
  Result := GetBDSRessourceString(GetBDSProjectsBaseDirResID);
  if Result <> '' then
    Result := PathAddSeparator(GetSpecialFolderLocation(CSIDL_COMMON_DOCUMENTS)) + Result + '\' + GetBDSVersion
  else
    case GetDelphiVersion of
      dver2009, dver2010, dverXE, dverXE2 .. dverXE5:
        Result := PathAddSeparator(GetSpecialFolderLocation(CSIDL_COMMON_DOCUMENTS)) + 'RAD Studio\' + GetBDSVersion;
      dverXE6 .. dverD13:
        Result := PathAddSeparator(GetSpecialFolderLocation(CSIDL_COMMON_DOCUMENTS)) + 'Embarcadero\Studio\' +
          GetBDSVersion;
    end;
end;

function GetBDSCommonPath: string;
begin
  Result := ReadRegStringLM(GetDelphiRootKey + stdEnvironmentVariablesKey, stdcBdsCommonDir, '');
  if Result = '' then
    Result := GetBDSCommonPathEx;
end;

function GetBDSUserPathEx: string;
begin
  Result := GetBDSRessourceString(GetBDSProjectsBaseDirResID);
  if Result <> '' then
    Result := PathAddSeparator(GetPersonalFolder) + Result + '\' + GetBDSVersion
  else if GetDelphiVersion >= dver2007 then
    case GetDelphiVersion of
      dver2009, dver2010, dverXE, dverXE2 .. dverXE5:
        Result := PathAddSeparator(GetPersonalFolder) + 'RAD Studio\' + GetBDSVersion;
      dverXE6 .. dverD13:
        Result := PathAddSeparator(GetPersonalFolder) + 'Embarcadero\Studio\' + GetBDSVersion;
    end;
end;

function GetBDSUserPath: string;
begin
  Result := ReadRegStringLM(GetDelphiRootKey + stdEnvironmentVariablesKey, stdcBdsCommonDir, '');
  if Result = '' then
    Result := GetBDSUserPathEx;
end;

function GetDelphiBPLPath: string;
begin
  Result := PathAddSeparator(ReplaceDelphiPathVars(ReadRegStringLM(GetDelphiRootKey + GetLibraryKey,
    stdDPLOutValue, '')));
end;

function GetDelphiDCPPath: string;
begin
  Result := PathAddSeparator(ReplaceDelphiPathVars(ReadRegStringLM(GetDelphiRootKey + GetLibraryKey,
    stdDCPOutValue, '')));
end;

// Platform, LANGDIR, BDSLIB, BDSBIN
function GetPlatformString: String;
begin
  case GetCompilerPlatform of
    dpOSX32:
      Result := stdcPlatformOSX32;
    dpOSX64:
      Result := stdcPlatformOSX64;
    dpWin32:
      Result := stdcPlatformWIN32;
    dpWin64:
      Result := stdcPlatformWIN64;
    dpiOSDevice:
      Result := stdcPlatformIOSDevice;
    dpiOSDevice32:
      Result := stdcPlatformIOSDevice32;
    dpiOSDevice64:
      Result := stdcPlatformIOSDevice64;
    dpiOSSimulator:
      Result := stdcPlatformIOSSimulator;
    dpAndroid32:
      Result := stdcPlatformAndroid32;
    dpAndroid64:
      Result := stdcPlatformAndroid64;
  end;
end;

function GetLANGDIR: String;
var
  sl: TStringList;
  S: String;
begin
  if GetDelphiVersion >= dverXE then
  begin
    // overwritten in registry?
    Result := ReadRegStringLM(GetDelphiRootKey + GetLibraryKey, 'LANGDIR', '');

    // if not, find out which platform exists
    if Result = '' then
    begin
      sl := TStringList.Create;
      try
        S := PathAddSeparator(PathAddSeparator(GetBDSLIB) + GetPlatformString) + 'release';
        if DirectoryExists(S) then
        begin
          ReadFolders(S, sl);
          if sl.Count > 0 then
            Result := sl[0];
        end
      finally
        sl.Free;
      end;
    end;

  end
  else
    Result := '';
end;

function GetBDSLIB: String;
begin
  if GetDelphiVersion >= dverXE then
  begin
    Result := PathRemoveSeparator(ReadRegStringLM(GetDelphiRootKey + GetLibraryKey, 'BDSLIB', ''));
    if Result = '' then
      Result := PathAddSeparator(GetDelphiRootPathLong) + 'lib';
  end
  else
    Result := '';
end;

function GetBDSBIN: String;
begin
  if GetDelphiVersion >= dverXE then
    case GetCompilerPlatform of
      dpOSX32:
        Result := PathAddSeparator(GetDelphiRootPath) + 'binosx32';
      dpWin32:
        Result := PathAddSeparator(GetDelphiRootPath) + 'bin';
      dpWin64:
        Result := PathAddSeparator(GetDelphiRootPath) + 'bin64';
    end
  else
    Result := '';
end;

function GetIsProjectExpert(Filename: string): Boolean;
var
  sl: TStringList;
  I: Integer;
begin
  Result := False;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    for I := 0 to sl.Count - 1 do
      if Pos('WIZARDENTRYPOINT', UpperCase(sl[I])) <> 0 then
      begin
        Result := True;
        Break;
      end;
  finally
    sl.Free;
  end;
end;

function GetCompilerVersion: String;
begin
  Assert(_DelphiVersion > dverD103R, 'Not working for Delphi less than "Sydney"');
  case _DelphiVersion of
    dverD104S:
      Result := '270';
    dverD11A:
      Result := '280';
    dverD12A:
      Result := '290';
    dverD13:
      Result := '370';
  end;
end;

function GetPackageRunOnly(Filename: string): Boolean;
var
  sl: TStringList;
  I: Integer;
begin
  Result := False;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    for I := 0 to sl.Count - 1 do
      if Pos('$RUNONLY', sl[I]) <> 0 then
      begin
        Result := True;
        Break;
      end;
  finally
    sl.Free;
  end;
end;

function GetPackageInfo(Filename: string; Key: string): string;
var
  sl: TStringList;
  I, p1, p2: Integer;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    for I := 0 to sl.Count - 1 do
      if Pos('$' + Key, sl[I]) <> 0 then
      begin
        p1 := Pos(' ', sl[I]);
        p2 := Pos('}', sl[I]);
        Result := Copy(sl[I], p1 + 1, p2 - p1 - 1);
      end;
  finally
    sl.Free;
  end;
end;

function GetPackageDescription(Filename: string): string;
begin
  Result := GetPackageInfo(Filename, 'DESCRIPTION');
end;

function GetPackageSuffix(Filename: string): string;
begin
  Result := GetPackageInfo(Filename, 'LIBSUFFIX').Replace('''', '');

  if SameText(Result, 'auto') then
    if _DelphiVersion >= dverD11A then
      Result := GetCompilerVersion;
end;

function GetPackagePrefix(Filename: string): string;
begin
  Result := GetPackageInfo(Filename, 'LIBPREFIX');
end;

function GetPackageVersion(Filename: string): string;
begin
  Result := GetPackageInfo(Filename, 'LIBVERSION');
end;

function CheckAllDelphiVersions: Boolean;
var
  PathList: TStringList;

  procedure FillPathList;
  var
    I: Integer;
  begin
    PathList.Clear;
    PathList.Text := StringReplace(GetEnvironmentVariable('PATH'), ';', #10, [rfReplaceAll]);
    for I := 0 to PathList.Count - 1 do
      PathList[I] := PathRemoveSeparator(ExpandFilename(PathList[I]));
  end;

  function IsInPathList(S: String): Boolean;
  begin
    Result := Pos(UpperCase(S), UpperCase(PathList.Text)) > 0;
  end;

var
  lastdver, dver: TDelphiVersion;
begin
  PathList := TStringList.Create;
  lastdver := GetDelphiVersion;
  Result := True;
  try
    for dver := dver2005 to High(TDelphiVersion) do
    begin
      if CheckDelphiVersion(dver) then
      begin
        SetDelphiVersion(dver);
        FillPathList;

        // find out if the BPL Path is in the Pathlist
        if not IsInPathList(ExcludeTrailingPathDelimiter(GetDelphiBPLPath)) then
        begin
          if MessageDlg(Format(stdverBPLDirNotInPath, [GetVersionText, GetVersionText]), mtError, [mbYes, mbNo], 0) = mrYes
          then
            DlgBDSEnvironment(GetDelphiVersion);
        end;

        // Check if the directory exists
        if not DirectoryExists(GetDelphiBPLPath) then
        begin
          if MessageDlg(Format(stdBPLDirNotExist, [GetDelphiBPLPath, GetVersionText]), mtError, [mbYes, mbNo], 0) = mrYes
          then
          begin
            ForceDirectories(GetDelphiBPLPath);
          end
          else
          begin
          end;
        end;
      end;
    end;
  finally
    PathList.Free;
    SetDelphiVersion(lastdver);
  end;
end;

procedure ReadDelphiVersionReg;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKey(MakeStudio.ApplicationRegKey, True);
      if ValueExists('DelphiVersion') then
        SetDelphiVersion(TDelphiVersion(ReadInteger('DelphiVersion')));
      if ValueExists('Platform') then
        SetCompilerPlatform(TCompilerPlatform(ReadInteger('Platform')));
    finally
      Free;
    end;
end;

procedure WriteDelphiVersionReg;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKey(MakeStudio.ApplicationRegKey, True);
      WriteInteger('DelphiVersion', Ord(GetDelphiVersion));
      WriteInteger('Platform', Ord(GetCompilerPlatform));
    finally
      Free;
    end;
end;

{$IFNDEF D5TODO}
{$IFNDEF DELPHI7_UP}

function PosEx(Substr: string; S: string; AOffset: Integer = 1): Integer;
begin
  Result := 0;
  if AOffset = 1 then
    Result := Pos(Substr, S)
  else if AOffset > 0 then
    Result := Pos(Substr, Copy(S, AOffset, Length(S) - AOffset + 1));
end;
{$ENDIF ~DELPHI7_UP}
{$ENDIF ~D5TODO}

function GetPackageDepencies(Filename: string; Requires: TStrings): Boolean;
var
  sl: TStringList;
  S1, s2: string;
  p1, p2, I: Integer;
begin
  Result := False;

  if (Requires <> nil) and (FileExists(Filename)) then
  begin
    Result := GetIsPackage(Filename);
    if Result then
    begin
      sl := TStringList.Create;
      try
        sl.LoadFromFile(Filename);
        S1 := sl.Text;
        p1 := Pos('requires', LowerCase(S1));
        if p1 > 0 then
        begin
          p1 := p1 + Length('requires');
          p2 := PosEx(';', S1, p1);
          s2 := Copy(S1, p1, p2 - p1);
          S1 := '';
          for I := 1 to Length(s2) do
{$IFDEF DELPHIXE_UP}
            if CharInSet(s2[I], ['A' .. 'Z', 'a' .. 'z', '0' .. '9', ',']) then
{$ELSE}
            if s2[I] in ['A' .. 'Z', 'a' .. 'z', '0' .. '9', ','] then
{$ENDIF}
              S1 := S1 + s2[I];
          S1 := StringReplace(S1, ',', #10, [rfReplaceAll]);
          Requires.Text := S1;
        end;
      finally
        sl.Free;
      end;
    end;
  end;
end;

procedure PublishVars;
begin
  MakeStudio.Variables.Values[stvarDelphiVersion] := GetVersionText;

  if GetDelphiVersion < dver2005 then
    MakeStudio.Variables.Values[stvarDelphiRootDir] := GetDelphiRootPathLong
  else
    MakeStudio.Variables.Values[stvarDelphiRootDir] := '';

  if GetDelphiVersion > dver7 then
  begin
    MakeStudio.Variables.Values[stvarBDSProjectDir] := GetBDSProjectsPath;
    MakeStudio.Variables.Values[stvarBDSDir] := GetDelphiRootPathLong;
  end
  else
  begin
    MakeStudio.Variables.Values[stvarBDSProjectDir] := '';
    MakeStudio.Variables.Values[stvarBDSDir] := '';
  end;

  MakeStudio.Variables.Values[stvarRegDelphiRootKey] := GetDelphiRootKey;
  MakeStudio.Variables.Values[stvarDelphiSearchPath] := GetDelphiSearchPath;
  MakeStudio.Variables.Values[stvarBPLDir] := GetDelphiBPLPath;
  MakeStudio.Variables.Values[stvarDCPDir] := GetDelphiDCPPath;

  if not MakeStudio.Variables.VarExists(stvarNamespaces) then
    MakeStudio.Variables.Values[stvarNamespaces] := stNamespaces;
end;

function QuoteSeparetedList(Separated: String): String;
begin
  if Separated <> '' then
  begin
    // handle last charakter
    if Separated[Length(Separated)] = ';' then
      Separated[Length(Separated)] := '"';

    // handle first and last character
    Separated := '"' + Separated + '"';

    Result := StringReplace(Separated, ';;', ';', [rfReplaceAll]);
    Result := StringReplace(Result, ';', '";"', [rfReplaceAll]);
    Result := StringReplace(Result, '""', '"', [rfReplaceAll]);
  end;
end;

function _ChangeCharTo(FromChar, ToChar: char; const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) - 1 downto 0 do
    if Result[I + 1] = FromChar then
      Result[I + 1] := ToChar;
end;

procedure omParseString(const S: string; Separator: char; AStrings: TStrings); overload;
var
  slist: TStringList;
begin
  slist := TStringList.Create;
  with slist do
    try
      Text := _ChangeCharTo(Separator, #10, S) + #10;
      // missing empty String at the end
      AStrings.AddStrings(slist);
    finally
      Free;
    end;
end;

function _Dequote(S: string): string;
VAR
  l: Integer;
begin
  Result := S; // default: unchanged

  S := Trim(S);
  l := Length(S);
  if l < 2 then
    Exit; // can't have a pair of quotes.

  if (S[1] = '''') AND (S[l] = '''') then
  begin
    Result := Copy(S, 2, l - 2);
  end
  else if (S[1] = '"') AND (S[l] = '"') then
  begin
    Result := Copy(S, 2, l - 2);
  end
end;

function _HasQuotes(S: string): Boolean;
VAR
  I: Integer;
begin
  I := Length(S);
  if I > 1 then
    Result := CharInSet(S[I], ['''', '"']) AND (S[1] = S[I])
  else
    Result := False;
end;

function _AddPathDelimiter(S: string): string;
Var
  K, x: Integer;
begin
  Result := S; // default
  x := Length(S);
  if x > 1 then
  begin
    K := 1;
    // look inside quoted string;
    if _HasQuotes(S) then
    begin
      Inc(K);
      Dec(x);
    end;

    if (x > K) AND (S[x] <> PathDelim) then
    begin
      Insert(PathDelim, S, x + 1);
      Result := S;
    end;
  end;
end;

function _RemPathDelimiter(S: string): string;
Var
  K, x: Integer;
begin
  Result := S; // default
  x := Length(S);
  if x > 1 then
  begin
    K := 1;
    if _HasQuotes(S) then
    begin
      Inc(K);
      Dec(x);
    end;

    if (x > K) AND (S[x] = PathDelim) then
    begin
      Delete(S, x, 1);
      Result := S;
    end;
  end;
end;

procedure _DelReplicates(AStrings: TStrings; caseSens: Boolean);
var
  I, x: Integer;
begin
  if caseSens then
  begin
    I := 0; // c-Style for-loop with moving end condition
    while I < AStrings.Count - 1 do
    begin
      for x := AStrings.Count - 1 downto I + 1 do
      begin
        if AStrings[I] = AStrings[x] then
          AStrings.Delete(x);
      end;
      Inc(I);
    end;
  end
  else
  begin
    I := 0; // c-Style for-loop with moving end condition
    while I < AStrings.Count - 1 do
    begin
      for x := AStrings.Count - 1 downto I + 1 do
      begin
        if SameText(AStrings[I], AStrings[x]) then
          AStrings.Delete(x);
      end;
      Inc(I);
    end;
  end;
end;

procedure omParseString(const S: string; Separator: string; AStrings: TStrings; parseOptions: TomParseOptions);
  overload;
var
  I: Integer;
begin
  if NOT(Assigned(AStrings) AND (Length(Separator) > 0)) then
    Exit; // no output buffer, no separator

  if omPoClear in parseOptions then
    AStrings.Clear;

  if omPoRegExpr in parseOptions then
    // use regExp to split
    AStrings.AddStrings(TRegEx.Split(S, Separator))
  else
    // the classical omParseString
    omParseString(S, Separator[1], AStrings);

  if omPoDequote in parseOptions then
    for I := 0 to AStrings.Count - 1 do
      AStrings[I] := _Dequote(AStrings[I]);

  if omPoTrimFields in parseOptions then
    for I := 0 to AStrings.Count - 1 do
      AStrings[I] := Trim(AStrings[I]);

  if omPoAddPathDelim in parseOptions then
    for I := 0 to AStrings.Count - 1 do
      AStrings[I] := _AddPathDelimiter(AStrings[I])
  else if omPoRemPathDelim in parseOptions then
    for I := 0 to AStrings.Count - 1 do
      AStrings[I] := _RemPathDelimiter(AStrings[I]);

  if omPoNoEmptyFields in parseOptions then
  begin
    while AStrings.IndexOf('') >= 0 do
      AStrings.Delete(AStrings.IndexOf(''));
  end;

  if omPoUniqueOnly in parseOptions then
    _DelReplicates(AStrings, omPoCaseSensitive in parseOptions);
end;

procedure omPrefixStrings(prefix: string; AStringsIn: TStrings; prefixOptions: TomPrefixOptions; AStringsOut: TStrings);
VAR
  I, x: Integer;
  S: string;
  wr: Boolean;
begin
  if NOT Assigned(AStringsIn) then
    Exit;

  if Assigned(AStringsOut) AND (omPrClear in prefixOptions) then
    AStringsOut.Clear;

  I := 0; // c-Style for-loop with moving end condition
  while I < AStringsIn.Count do
  begin
    S := AStringsIn[I];
    if omPrDequoteTrimFields in prefixOptions then
      S := Trim(_Dequote(S));
    if omPrAddPathDelim in prefixOptions then
      S := _AddPathDelimiter(S)
    else if omPrRemPathDelim in prefixOptions then
      S := _RemPathDelimiter(S);

    if (omPrNoEmptyFields in prefixOptions) AND (Length(S) = 0) then
    begin
      if Assigned(AStringsOut) then
        Inc(I) // next index, done.
      else
        AStringsIn.Delete(I);
      continue; // same index, new choice
    end;

    if omPrDoubleQuotes in prefixOptions then
      S := '"' + _Dequote(S) + '"'
    else if omPrSingleQuotes in prefixOptions then
      S := '''' + _Dequote(S) + '''';

    S := prefix + S;

    if Assigned(AStringsOut) then
    begin
      wr := True;
      if omPrUniqueOnly in prefixOptions then
      begin
        if omPrCaseSensitive in prefixOptions then
          wr := (AStringsOut.IndexOf(S) = 0)
        else
          for x := 0 to AStringsOut.Count - 1 do
          begin
            wr := NOT SameText(S, AStringsOut[x]); // wr := not equal
            if NOT wr then
              Break;
          end;
      end;
      if wr then
        AStringsOut.Append(S);
    end
    else
      // omPrUniqueOnly handled later
      AStringsIn[I] := S;

    Inc(I);
  end;

  // last but not least:
  if (omPrUniqueOnly in prefixOptions) AND NOT Assigned(AStringsOut) then
    _DelReplicates(AStringsIn, omPrCaseSensitive in prefixOptions);
end;

end.
