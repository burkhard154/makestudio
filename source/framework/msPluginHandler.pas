(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msPluginHandler.pas

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

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to JVCSMAK
2005/01/02  BSchranz  - Migration to JVCSMak with external plugins
2005/02/04  USchuster - preparations for check in
2005/03/06  USchuster - added support for .Net plugins
                      - new property TJVCSMAKPlugin.VersionInfo (retrieve
                        version info here and not in msInfo.pas)
2005/03/16  USchuster - workaround for EOleException "Overflow or underflow in
                        the arithmetic operation"
2005/06/26  USchuster - changes to support EMakeKind <> mkGUI
2005/04/09  BSchranz  - Translated to englisch
2006/06/07  USchuster - D5 fix

-----------------------------------------------------------------------------*)
//Interface for used DLL's
unit msPluginHandler;

{$I jedi.inc}

interface

uses
  {$IFDEF DELPHI6_UP}
  Variants,
  {$ELSE}
  JvVCL5Utils,
  {$ENDIF DELPHI6_UP}
  SysUtils, Windows, Classes, Forms, ComCtrls, ActiveX, Dialogs, Controls,
  Contnrs, Psapi, Registry, msTLB, JclFileUtils, ComObj, mscorlib_TLB,
  jclStrings, msDotNetUtils;

const
  stJVCSMAKDLLGetNameProc              = 'GetName';
  stJVCSMAKDLLGetAuthorProc            = 'GetAuthor';
  stJVCSMAKDLLAfterAllPluginsLoaded    = 'AfterAllPluginsLoaded';
  stJVCSMAKDLLGetDescriptionProc       = 'GetDescription';
  stJVCSMAKDLLGetRequiredPluginsProc   = 'GetRequiredPlugins';
  stJVCSMAKDLLRegisterPluginProc       = 'RegisterPlugin';
  stJVCSMAKDLLUnregisterPluginProc     = 'UnregisterPlugin';
  stJVCSMAKDLLGetMinorVersionProc      = 'GetMinorVersion';
  stJVCSMAKDLLGetMajorVersionProc      = 'GetMajorVersion';
  stJVCSMAKDLLGetOptionsPageGUID       = 'GetOptionsPageGUID';
  stJVCSMAKDLLIdentifyProc             = 'JVCSMAKPlugin'; //Identification of that DLL is a JVCSMAKPlugin

  stNetPluginClassName                 = 'JMPlugin';
  stNetPluginPrefix                    = 'JPL';
  stNetPluginExtension                 = 'DLL';
  stcPluginSeparator                   = ';';
  stcPluginExt                         = 'JPL';

type
//Funktion prototypes for DLL calls

  TJVCSMAKDLLGetNameProc              = procedure(AName: PChar); stdcall;
  TJVCSMAKDLLGetAuthorProc            = procedure(AName: PChar); stdcall;
  TJVCSMAKDLLGetDescriptionProc       = procedure(AName: PChar); stdcall;
  TJVCSMAKDLLGetRequiredPluginsProc   = procedure(AName: PChar); stdcall;
  TJVCSMAKDLLRegisterPluginProc       = function(AJVCSMAKApp: IJApplication): Integer; stdcall;
  TJVCSMAKDLLUnregisterPluginProc     = function: Integer; stdcall;
  TJVCSMAKDLLGetMinorVersionProc      = function: Integer; stdcall;
  TJVCSMAKDLLGetMajorVersionProc      = function: Integer; stdcall;
  TJVCSMAKDLLGetOptionsPageGUIDProc   = function: TGUID; stdcall;
  TJVCSMAKDLLAfterAllPluginsLoaded    = procedure; stdcall;

  TJVCSMakPluginKind = (pkWin32DLL, pkDotNetAssembly);

//Plugin wrapper class

  TJVCSMAKPlugin = class(TObject)
  private
    NetPlugin: Variant;
    DLLHandle: THandle;
    FFilename: string;
    FRegistered: Boolean;
    FKind: TJVCSMakPluginKind;
    FAppInterface: IJApplication;

    function GetVersionInfo: string;
    function GetPluginClassName: string;
  public
    constructor Create(AApplication: IJApplication);
    destructor Destroy; override;

    //Wrapping DLL calls
    procedure AfterAllPluginsLoaded;
    function RegisterPlugin: Integer;
    function UnregisterPlugin: Integer;
    function GetName: string;
    function GetAuthor: string;
    function GetMinorVersion: Integer;
    function GetMajorVersion: Integer;
    function GetDescription: string;
    function GetRequiredPlugins: string;
    function GetOptionsGUID: TGUID;
    function HasOptionsDlg: Boolean;

    procedure RequiredPluginsList(AList: TStrings);
    procedure RequiredPluginsFilenameList(AList: TStrings);
    function IsLoaded: Boolean;

    property VersionInfo: string read GetVersionInfo;
    property PluginClassName: string read GetPluginClassName;
    property Kind: TJVCSMakPluginKind read FKind write FKind;
    property Registered: Boolean read FRegistered write FRegistered;
    property Filename: string read FFilename write FFilename;
  end;

//Plugin List
  TJVCSMAKPlugins = class(TObjectList)
  private
    function GetItems(Index: Integer): TJVCSMAKPlugin;
    procedure SetItems(Index: Integer; AJVCSMAKPlugin: TJVCSMAKPlugin);
    function GetItemsByName(ID: string): TJVCSMAKPlugin;
    procedure SetItemsByName(ID: string; AJVCSMAKPlugin: TJVCSMAKPlugin);
  public
    function IsPluginLoaded(Filename: string): Boolean;
    function Add(AJVCSMAKPlugin: TJVCSMAKPlugin): Integer;
    function Remove(AJVCSMAKPlugin: TJVCSMAKPlugin): Integer;
    function IndexOf(AJVCSMAKPlugin: TJVCSMAKPlugin): Integer;
    procedure Insert(Index: Integer; AJVCSMAKPlugin: TJVCSMAKPlugin);
    property Items[Index: Integer]: TJVCSMAKPlugin read GetItems write SetItems; default;
    property ItemsByName[ItemID: string]: TJVCSMAKPlugin read GetItemsByName write SetItemsByName;
  end;

//Plugin Handler
  TJVCSMAKPluginHandler = class(TPersistent)
  private
    FPlugins: TJVCSMAKPlugins;
    FAppInterface: IJApplication;
    FAdditionalInfo: TStringList; //Additional Infos from within the Plugins
    FCredits: TStringList;
    function GetDotNetLoaded: Boolean;
    function LoadAssembly(AFilename: string): Boolean;
    function LoadPlugin(AFilename: string): Boolean;
    function IsNetPlugin(AFilename:string): Boolean;
    procedure LoadDLLLibs;
    procedure LoadNETLibs;
    function GetLoaded: Boolean;
  protected
    property AppInterface: IJApplication read FAppInterface;// write SetAppInterface;
  public
    constructor Create(AMakeKind: EMakeKind = mkGUI); virtual;
    Destructor Destroy; override;
    procedure LoadAllLibs;
    procedure UnregisterAllLibs;
    procedure FreeAllLibs;
    procedure AfterAllPluginsLoaded;

    property Plugins: TJVCSMAKPlugins read FPlugins;
    property DLLsLoaded: Boolean read GetLoaded;
    property DotNetDLLsLoaded: Boolean read GetDotNetLoaded;
    property AdditionalInfo: TStringList read FAdditionalInfo;
    property Credits: TStringList read FCredits;
  end;

implementation

uses
  msApplication_impl, msResources, msUtils;

type
  Exception = SysUtils.Exception;  

{------------------------------------------------------------------------------}
// TJVCSMAKPlugin
constructor TJVCSMAKPlugin.Create(AApplication: IJApplication);
begin
  DLLHandle := 0;
  NetPlugin := varEmpty;
  Filename := '';
  Registered := False;
  Kind := pkWin32DLL;
  FAppInterface := AApplication; 
end;

procedure TJVCSMAKPlugin.AfterAllPluginsLoaded;
var
  proc: TJVCSMAKDLLAfterAllPluginsLoaded;
begin
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLAfterAllPluginsLoaded);
      if @proc <> nil then
        proc
      else
        AddLog(Format(tecs_MissingDLLFunction, [stJVCSMAKDLLAfterAllPluginsLoaded, Filename]));
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      if VarIsType(NetPlugin, varDispatch) then
        NetPlugin.AfterAllPluginsLoaded;
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.RegisterPlugin: Integer;
var
  proc: TJVCSMAKDLLRegisterPluginProc;
begin
  Result := -1;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLRegisterPluginProc);
      if @proc <> nil then
      begin
        proc(FAppInterface);
        Result := 0;
        Registered := True;
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stJVCSMAKDLLRegisterPluginProc, Filename]));
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      if VarIsType(NetPlugin, varDispatch) then
      begin
        try
          NetPlugin.RegisterPlugin(FAppInterface);
          Result := 0;
          Registered := True;
        except
        end;
      end;
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.UnregisterPlugin: Integer;
var
  proc: TJVCSMAKDLLUnregisterPluginProc;
begin
  Result := -1;

  if not Registered then Exit;

  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLUnregisterPluginProc);
      if @proc <> nil then
      begin
        try
          proc;
        except
  //        on E: Exception do ShowMessage('Pluginhander - '+Filename+':'+E.Message);
        end;
        Result := 0;
        Registered := False;
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stJVCSMAKDLLUnregisterPluginProc, Filename]));
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      if VarIsType(NetPlugin, varDispatch) then
      begin
        NetPlugin.UnregisterPlugin;
        Result := 0;
        Registered := False;
      end;
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.GetName: string;
var
  proc: TJVCSMAKDLLGetNameProc;
  ch: array [0..255] of Char;
begin
  Result := sttwr_StrNoFunctionReturn;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLGetNameProc);
      if @proc <> nil then
      begin
        proc(ch);
        Result := StrPas(ch);
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [Filename, stJVCSMAKDLLGetNameProc]));
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      if VarIsType(NetPlugin, varDispatch) then
        Result := NetPlugin.Name;
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.GetAuthor: string;
var
  proc: TJVCSMAKDLLGetAuthorProc;
  ch: array [0..255] of Char;
begin
  Result := sttwr_StrNoFunctionReturn;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLGetAuthorProc);
      if @proc <> nil then
      begin
        proc(ch);
        Result := StrPas(ch);
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stJVCSMAKDLLGetAuthorProc, Filename]));
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      if VarIsType(NetPlugin, varDispatch) then
        Result := NetPlugin.Author;
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.GetMinorVersion: Integer;
var
  proc: TJVCSMAKDLLGetMinorVersionProc;
begin
  Result := 0;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLGetMinorVersionProc);
      if @proc <> nil then
      begin
        Result := proc;
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stJVCSMAKDLLGetMinorVersionProc, Filename]));
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      if VarIsType(NetPlugin, varDispatch) then
        Result := NetPlugin.MinorVersion;
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.GetMajorVersion: Integer;
var
  proc: TJVCSMAKDLLGetMajorVersionProc;
begin
  Result := 0;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLGetMajorVersionProc);
      if @proc <> nil then
      begin
        Result := proc;
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stJVCSMAKDLLGetMajorVersionProc, Filename]));
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      if VarIsType(NetPlugin, varDispatch) then
        Result := NetPlugin.MajorVersion;
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.GetDescription: string;
var
  proc: TJVCSMAKDLLGetDescriptionProc;
  ch: array [0..255] of Char;
begin
  Result := sttwr_StrNoFunctionReturn;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLGetDescriptionProc);
      if @proc <> nil then
      begin
        proc(ch);
        Result := StrPas(ch);
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stJVCSMAKDLLGetDescriptionProc, Filename]));
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      if VarIsType(NetPlugin, varDispatch) then
        Result := NetPlugin.Description;
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.GetRequiredPlugins: string;
var
  proc: TJVCSMAKDLLGetRequiredPluginsProc;
  ch: array [0..2047] of Char;
begin
  Result := '';
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLGetRequiredPluginsProc);
      if @proc <> nil then
      begin
        proc(ch);
        Result := StrPas(ch);
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stJVCSMAKDLLGetRequiredPluginsProc, Filename]));
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      //Not Supported by .NET plugins yet
      {if VarIsType(NetPlugin, varDispatch) then
        Result := NetPlugin.RequiredPlugins;}
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.GetOptionsGUID: TGUID;
var
  proc: TJVCSMAKDLLGetOptionsPageGUIDProc;
begin
  Result := GUID_NULL;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stJVCSMAKDLLGetOptionsPageGUID);
      if @proc <> nil then
      begin
        Result := proc;
      end
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      //not Supported by .NET Plugins Yet
      {if VarIsType(NetPlugin, varDispatch) then
        Result := NetPlugin.OptionsPageGUID;}
    end;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

function TJVCSMAKPlugin.GetVersionInfo: string;
var
  verinfo: TJclFileVersionInfo;
  AssemblyName: string;
  obj: _Object;
begin
  try
    if Kind = pkWin32DLL then
    begin
      verinfo := TJclFileVersionInfo.Create(Filename);
      try
        Result := verinfo.FileVersion;
      finally
        verinfo.Free;
      end;
    end
    else
    if Kind = pkDotNetAssembly then
    begin
      if Supports(NetPlugin, IID__Object, obj) then
      begin
        AssemblyName := obj.GetType.Assembly.ToString;
        if Pos('Version=', AssemblyName) > 0 then
        begin
          Delete(AssemblyName, 1, Pos('Version=', AssemblyName) + 7);
          Result := Copy(AssemblyName, 1, Pos(',', AssemblyName) - 1);
        end;
      end;
    end;
  except
    Result := 'no Version Info';
  end;
end;

function TJVCSMAKPlugin.HasOptionsDlg: Boolean;
begin
  Result := IsEqualGUID(GetOptionsGUID, GUID_NULL);
end;

function TJVCSMAKPlugin.IsLoaded: Boolean;
begin
  Result := GetModuleHandle(PChar(ExtractFileName(Filename))) <> 0;
end;

procedure TJVCSMAKPlugin.RequiredPluginsList(AList: TStrings);
var
  S: string;
begin
  AList.Clear;
  S := GetRequiredPlugins;
  if S <> '' then
    AList.Text := StringReplace(S, stcPluginSeparator, #10#13, [rfReplaceAll]);
end;

procedure TJVCSMAKPlugin.RequiredPluginsFilenameList(AList: TStrings);
var
  I: Integer;
begin
  RequiredPluginsList(AList);
  for I := 0 to AList.Count - 1 do
    AList[I] := PathAddSeparator(ExtractFilePath(Forms.Application.ExeName)) +
      ChangeFileExt(AList[I], '.' + stcPluginExt);
end;

{------------------------------------------------------------------------------}
// TJVCSMAKPlugins

function TJVCSMAKPlugins.IsPluginLoaded(Filename: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := CompareText(Filename, Items[I].Filename) = 0;
    if Result then Break;
  end;
end;

function TJVCSMAKPlugins.Add(AJVCSMAKPlugin: TJVCSMAKPlugin): Integer;
begin
  Result := inherited Add(AJVCSMAKPlugin);
end;

function TJVCSMAKPlugins.GetItems(Index: Integer): TJVCSMAKPlugin;
begin
  Result := TJVCSMAKPlugin(inherited Items[Index]);
end;

function TJVCSMAKPlugins.IndexOf(AJVCSMAKPlugin: TJVCSMAKPlugin): Integer;
begin
  Result := inherited IndexOf(AJVCSMAKPlugin);
end;

procedure TJVCSMAKPlugins.Insert(Index: Integer; AJVCSMAKPlugin: TJVCSMAKPlugin);
begin
  inherited Insert(Index, AJVCSMAKPlugin);
end;

function TJVCSMAKPlugins.Remove(AJVCSMAKPlugin: TJVCSMAKPlugin): Integer;
begin
  Result := inherited Remove(AJVCSMAKPlugin);
end;

procedure TJVCSMAKPlugins.SetItems(Index: Integer; AJVCSMAKPlugin: TJVCSMAKPlugin);
begin
  inherited Items[Index] := AJVCSMAKPlugin;
end;

function TJVCSMAKPlugins.GetItemsByName(ID: string): TJVCSMAKPlugin;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if CompareText(Items[I].GetName, ID) = 0 then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TJVCSMAKPlugins.SetItemsByName(ID: string; AJVCSMAKPlugin: TJVCSMAKPlugin);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if CompareText(Items[I].GetName, ID) = 0 then
    begin
      Items[I] := AJVCSMAKPlugin;
      Break;
    end;
end;

{------------------------------------------------------------------------------}
// TExDLLHandler

constructor TJVCSMAKPluginHandler.Create(AMakeKind: EMakeKind = mkGUI);
begin
  inherited Create;
  FAppInterface := TJApplication.Create(AMakeKind);
  FPlugins := TJVCSMAKPlugins.Create;
  FAdditionalInfo := TStringList.Create;
  FCredits := TStringList.Create;
end;

destructor TJVCSMAKPluginHandler.Destroy;
begin
  FreeAllLibs;
  FAdditionalInfo.Free;
  FCredits.Free;
  FAppInterface := nil;
  FPlugins.Free;
  inherited Destroy;
end;

function TJVCSMAKPluginHandler.LoadAssembly(AFilename: string): Boolean;
var
  plgItem: TJVCSMAKPlugin;
begin
  Result := True;

  if FPlugins.IsPluginLoaded(AFileName) then Exit;

  AddLog(Format(sttwr_LoadingPlugin, [AFileName]));

  //Create and fill plugin item
  plgItem := TJVCSMAKPlugin.Create(AppInterface);
  plgItem.Kind := pkDotNetAssembly;
  plgItem.Filename := AFileName;

  //Load Assembly
  try
    plgItem.NetPlugin := CreateDotNetObject(AFilename, plgItem.PluginClassName);
  except
    on E: Exception do begin
      Result := False;
      AddLog(Format(tecs_ErrorLoadingPlugin, [AFilename]));
      AddLog(Format(tecs_ErrorLoadingPluginClass, [plgItem.PluginClassName]));
      AddLog(E.Message);
    end;
  end;

  if Result then begin

    FPlugins.Add(plgItem);

    //load required plugins - NOT YET
    {if plgItem.GetRequiredPlugins <> '' then
    begin
      sl := TStringList.Create;
      plgItem.RequiredPluginsFilenameList(sl);
      try
        for I := 0 to sl.Count - 1 do
        begin
          if not LoadPlugin(sl[I]) then
          AddLog(Format(tecs_CannotLoadRequiredPlugin, [sl[I]]));
        end;
      finally
        sl.Free;
      end;
    end;}

    //register plugin
    if plgItem.RegisterPlugin = 0 then
    begin
      Result := True;
    end
    else
    begin
      FPlugins.Remove(plgItem);
      plgItem.Free;
    end;
  end
  else
    plgItem.Free;
end;

function TJVCSMAKPluginHandler.LoadPlugin(AFilename: string): Boolean;
var
  proc: Pointer;
  lHandle: THandle;
  I: Integer;
  plgItem: TJVCSMAKPlugin;
  sl: TStringList;
begin
  Result := True;
  if FPlugins.IsPluginLoaded(AFilename) then Exit;

  AddLog(Format(sttwr_LoadingPlugin, [AFilename]));

  Result := False;
  lHandle := LoadLibrary(PChar(AFilename));

  if lHandle > 0 then
  begin
    //lookup identification proc
    proc := GetProcAddress(lHandle, stJVCSMAKDLLIdentifyProc);

    if proc <> nil then
    begin
      //AddLog('Proc war <> nil');

      //Create and fill plugin item
      plgItem := TJVCSMAKPlugin.Create(AppInterface);
      plgItem.DLLHandle := lHandle;
      plgItem.Filename := AFilename;
      FPlugins.Add(plgItem);

      //load required plugins
      if plgItem.GetRequiredPlugins <> '' then
      begin
        sl := TStringList.Create;
        plgItem.RequiredPluginsFilenameList(sl);
        try
          for I := 0 to sl.Count - 1 do
          begin
            if not LoadPlugin(sl[I]) then
            AddLog(Format(tecs_CannotLoadRequiredPlugin, [sl[I]]));
          end;
        finally
          sl.Free;
        end;
      end;

      //register plugin
      if plgItem.RegisterPlugin = 0 then
      begin
        Result := True;
      end
      else
      begin
        FPlugins.Remove(plgItem);
        plgItem.Free;
      end
    end
    else
    begin
      FreeLibrary(lHandle);
      AddLog(Format(tecs_MissingDLLFunction, [stJVCSMAKDLLIdentifyProc, AFilename]));
    end;
  end
  else
    AddLog(Format(tecs_ErrorLoadingPlugin, [AFilename]));
end;

procedure TJVCSMAKPluginHandler.LoadAllLibs;
begin
  if FAppInterface = nil then
  begin
    AddLog(tecs_NoAppInterface);
    Exit;
  end;

  try
    LoadDLLLibs;
  except
    on E: Exception do AddLog(E.Message);
  end;

  try
    LoadNETLibs;
  except
    on E: Exception do AddLog(E.Message);
  end;
end;

procedure TJVCSMAKPluginHandler.FreeAllLibs;
begin
  FPlugins.Clear;
end;

procedure TJVCSMAKPluginHandler.UnregisterAllLibs;
var
  I: Integer;
begin
  for I := 0 to FPlugins.Count - 1 do
  try
    FPlugins[I].UnregisterPlugin;
  except
    on E: Exception do ShowMessage('Pluginhander - ' + FPlugins[I].Filename + ':' + E.Message);
  end;
end;

function TJVCSMAKPluginHandler.GetDotNetLoaded: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Plugins.Count - 1 do
    if Plugins[I].FKind = pkDotNetAssembly then begin
      Result := True;
      Break;
    end;
end;

function TJVCSMAKPluginHandler.GetLoaded: Boolean;
var
  I: Integer;
begin
  Result := False; 
  for I := 0 to Plugins.Count - 1 do
    if Plugins[I].FKind = pkWin32DLL then begin
      Result := True;
      Break;
    end;
end;

procedure TJVCSMAKPluginHandler.AfterAllPluginsLoaded;
var
  I: Integer;
begin
  //AfterAllPluginsLoaded
  for I := 0 to Plugins.Count - 1 do
    Plugins[I].AfterAllPluginsLoaded;
end;

function TJVCSMAKPlugin.GetPluginClassName: string;
begin
  Result := ExtractFileName(ChangeFileExt(Filename, '')) + '.' + stNetPluginClassName;
end;

function TJVCSMAKPluginHandler.IsNetPlugin(AFilename: string): Boolean;
var
  sl: TStringList;
begin
  Result := False;
  sl := TStringList.Create;
  try
    StrTokenToStrings(ExtractFileName(AFilename), '.', sl);
    if sl.Count>3 then begin
      Result := (SameText(sl[0], stNetPluginPrefix)) and
                (SameText(sl[sl.Count - 1], stNetPluginExtension));
    end;
  finally
    sl.Free;
  end;
end;

destructor TJVCSMAKPlugin.Destroy;
begin
  if Registered then
    UnregisterPlugin;

  //FreeLibrary
  if (Kind = pkWin32DLL) and (DLLHandle <> 0) then begin
    if not FreeLibrary(DLLHandle) then
      ShowMessage(xGetLastError);

    if IsLoaded then
      MessageDlg(
         Format('INTERNAL for Plugin Developers' + #10#13 +
                 'Plugin "%s" is still loaded after FreeLibrary' + #10#13 +
                 'Please check if you have released all Memory and' + #10#13 +
                 'Interfaces', [Filename]),
                 mtError, [mbOK], 0);
  end;

  inherited;
end;

procedure TJVCSMAKPluginHandler.LoadNETLibs;
var
  SR: TSearchRec;
  Path: string;
  Err: Integer;
begin
  if FAppInterface = nil then
  begin
    AddLog(tecs_NoAppInterface);
    Exit;
  end;

  if DotNetDLLsLoaded then
  begin
    AddLog(tecs_PluginsAlreadyRegistered);
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    Path := PathAddSeparator(ExtractFilePath(Forms.Application.ExeName));
    Err := FindFirst(Path + stNetPluginPrefix + '.*.' + stNetPluginExtension, faAnyFile, SR);

    while Err = 0 do
    begin
      if IsNetPlugin(SR.Name) then
        LoadAssembly(Path + SR.Name);
      Err := FindNext(SR);
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TJVCSMAKPluginHandler.LoadDLLLibs;
var
  SR: TSearchRec;
  Path: string;
  Err: Integer;
begin
  if FAppInterface = nil then
  begin
    AddLog(tecs_NoAppInterface);
    Exit;
  end;

  if DLLsLoaded then
  begin
    AddLog(tecs_PluginsAlreadyRegistered);
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    Path := PathAddSeparator(ExtractFilePath(Forms.Application.ExeName));
    Err := FindFirst(Path + '*.' + stcPluginExt, faAnyFile, SR);

    while Err = 0 do
    begin
      LoadPlugin(Path + SR.Name);
      Err := FindNext(SR);
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
