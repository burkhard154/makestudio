(*
 ***************************************************************************
 optiMEAS GmbH
 written by Burkhard Schranz, Jens-Achim Kessel
 copyright © 2013 -
 Email : info@optimeas.de
 Web : http://www.optimeas.de
 http://www.makestudio.de
 http://www.mobiconn.de
 The source code is given as is. The author is not responsible
 for any possible damage done due to the use of this code.
 The component can be freely used in any application.
   source code remains property of the author and may not be distributed,
 published, given or sold in any form as such. No parts of the source
 code can be included in any other component or application without
 written authorization of optiMEAS GmbH


 ***************************************************************************


Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to MakeStudio
2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/03/06  USchuster - added support for .Net plugins
                      - new property TMakeStudioPlugin.VersionInfo (retrieve
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
  Contnrs, Psapi, Registry, makestudio_TLB, JclFileUtils, ComObj, mscorlib_TLB,
  jclStrings, msDotNetUtils;

const
  stMakeStudioDLLGetNameProc              = 'GetName';
  stMakeStudioDLLGetAuthorProc            = 'GetAuthor';
  stMakeStudioDLLAfterAllPluginsLoaded    = 'AfterAllPluginsLoaded';
  stMakeStudioDLLGetDescriptionProc       = 'GetDescription';
  stMakeStudioDLLGetRequiredPluginsProc   = 'GetRequiredPlugins';
  stMakeStudioDLLRegisterPluginProc       = 'RegisterPlugin';
  stMakeStudioDLLUnregisterPluginProc     = 'UnregisterPlugin';
  stMakeStudioDLLGetMinorVersionProc      = 'GetMinorVersion';
  stMakeStudioDLLGetMajorVersionProc      = 'GetMajorVersion';
  stMakeStudioDLLGetOptionsPageGUID       = 'GetOptionsPageGUID';
  stMakeStudioDLLIdentifyProc             = 'MakeStudioPlugin'; //Identification of that DLL is a MakeStudioPlugin

  stNetPluginClassName                 = 'JMPlugin';
  stNetPluginPrefix                    = 'JPL';
  stNetPluginExtension                 = 'DLL';
  stcPluginSeparator                   = ';';
  stcPluginExt                         = 'JPL';

type
//Funktion prototypes for DLL calls

  TMakeStudioDLLGetNameProc              = procedure(AName: PChar); stdcall;
  TMakeStudioDLLGetAuthorProc            = procedure(AName: PChar); stdcall;
  TMakeStudioDLLGetDescriptionProc       = procedure(AName: PChar); stdcall;
  TMakeStudioDLLGetRequiredPluginsProc   = procedure(AName: PChar); stdcall;
  TMakeStudioDLLRegisterPluginProc       = function(AMakeStudioApp: IJApplication): Integer; stdcall;
  TMakeStudioDLLUnregisterPluginProc     = function: Integer; stdcall;
  TMakeStudioDLLGetMinorVersionProc      = function: Integer; stdcall;
  TMakeStudioDLLGetMajorVersionProc      = function: Integer; stdcall;
  TMakeStudioDLLGetOptionsPageGUIDProc   = function: TGUID; stdcall;
  TMakeStudioDLLAfterAllPluginsLoaded    = procedure; stdcall;

  TMakeStudioPluginKind = (pkWin32DLL, pkDotNetAssembly);

//Plugin wrapper class

  TMakeStudioPlugin = class(TObject)
  private
    NetPlugin: Variant;
    DLLHandle: THandle;
    FFilename: string;
    FRegistered: Boolean;
    FKind: TMakeStudioPluginKind;
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
    property Kind: TMakeStudioPluginKind read FKind write FKind;
    property Registered: Boolean read FRegistered write FRegistered;
    property Filename: string read FFilename write FFilename;
  end;

//Plugin List
  TMakeStudioPlugins = class(TObjectList)
  private
    function GetItems(Index: Integer): TMakeStudioPlugin;
    procedure SetItems(Index: Integer; AMakeStudioPlugin: TMakeStudioPlugin);
    function GetItemsByName(ID: string): TMakeStudioPlugin;
    procedure SetItemsByName(ID: string; AMakeStudioPlugin: TMakeStudioPlugin);
  public
    function IsPluginLoaded(Filename: string): Boolean;
    function Add(AMakeStudioPlugin: TMakeStudioPlugin): Integer;
    function Remove(AMakeStudioPlugin: TMakeStudioPlugin): Integer;
    function IndexOf(AMakeStudioPlugin: TMakeStudioPlugin): Integer;
    procedure Insert(Index: Integer; AMakeStudioPlugin: TMakeStudioPlugin);
    property Items[Index: Integer]: TMakeStudioPlugin read GetItems write SetItems; default;
    property ItemsByName[ItemID: string]: TMakeStudioPlugin read GetItemsByName write SetItemsByName;
  end;

//Plugin Handler
  TMakeStudioPluginHandler = class(TPersistent)
  private
    FPlugins: TMakeStudioPlugins;
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

    property Plugins: TMakeStudioPlugins read FPlugins;
    property DLLsLoaded: Boolean read GetLoaded;
    property DotNetDLLsLoaded: Boolean read GetDotNetLoaded;
    property AdditionalInfo: TStringList read FAdditionalInfo;
    property Credits: TStringList read FCredits;
  end;

implementation

uses
  msApplication_impl, msResources, msUtils, System.Types, System.UITypes;

type
  Exception = SysUtils.Exception;

{------------------------------------------------------------------------------}
// TMakeStudioPlugin
constructor TMakeStudioPlugin.Create(AApplication: IJApplication);
begin
  DLLHandle := 0;
  NetPlugin := varEmpty;
  Filename := '';
  Registered := False;
  Kind := pkWin32DLL;
  FAppInterface := AApplication;
end;

procedure TMakeStudioPlugin.AfterAllPluginsLoaded;
var
  proc: TMakeStudioDLLAfterAllPluginsLoaded;
begin
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLAfterAllPluginsLoaded);
      if @proc <> nil then
        proc
      else
        AddLog(Format(tecs_MissingDLLFunction, [stMakeStudioDLLAfterAllPluginsLoaded, Filename]));
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

function TMakeStudioPlugin.RegisterPlugin: Integer;
var
  proc: TMakeStudioDLLRegisterPluginProc;
begin
  Result := -1;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLRegisterPluginProc);
      if @proc <> nil then
      begin
        proc(FAppInterface);
        Result := 0;
        Registered := True;
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stMakeStudioDLLRegisterPluginProc, Filename]));
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

function TMakeStudioPlugin.UnregisterPlugin: Integer;
var
  proc: TMakeStudioDLLUnregisterPluginProc;
begin
  Result := -1;

  if not Registered then Exit;

  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLUnregisterPluginProc);
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
        AddLog(Format(tecs_MissingDLLFunction, [stMakeStudioDLLUnregisterPluginProc, Filename]));
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

function TMakeStudioPlugin.GetName: string;
var
  proc: TMakeStudioDLLGetNameProc;
  ch: array [0..255] of Char;
begin
  Result := sttwr_StrNoFunctionReturn;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLGetNameProc);
      if @proc <> nil then
      begin
        proc(ch);
        Result := StrPas(ch);
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [Filename, stMakeStudioDLLGetNameProc]));
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

function TMakeStudioPlugin.GetAuthor: string;
var
  proc: TMakeStudioDLLGetAuthorProc;
  ch: array [0..255] of Char;
begin
  Result := sttwr_StrNoFunctionReturn;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLGetAuthorProc);
      if @proc <> nil then
      begin
        proc(ch);
        Result := StrPas(ch);
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stMakeStudioDLLGetAuthorProc, Filename]));
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

function TMakeStudioPlugin.GetMinorVersion: Integer;
var
  proc: TMakeStudioDLLGetMinorVersionProc;
begin
  Result := 0;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLGetMinorVersionProc);
      if @proc <> nil then
      begin
        Result := proc;
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stMakeStudioDLLGetMinorVersionProc, Filename]));
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

function TMakeStudioPlugin.GetMajorVersion: Integer;
var
  proc: TMakeStudioDLLGetMajorVersionProc;
begin
  Result := 0;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLGetMajorVersionProc);
      if @proc <> nil then
      begin
        Result := proc;
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stMakeStudioDLLGetMajorVersionProc, Filename]));
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

function TMakeStudioPlugin.GetDescription: string;
var
  proc: TMakeStudioDLLGetDescriptionProc;
  ch: array [0..255] of Char;
begin
  Result := sttwr_StrNoFunctionReturn;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLGetDescriptionProc);
      if @proc <> nil then
      begin
        proc(ch);
        Result := StrPas(ch);
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stMakeStudioDLLGetDescriptionProc, Filename]));
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

function TMakeStudioPlugin.GetRequiredPlugins: string;
var
  proc: TMakeStudioDLLGetRequiredPluginsProc;
  ch: array [0..2047] of Char;
begin
  Result := '';
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLGetRequiredPluginsProc);
      if @proc <> nil then
      begin
        proc(ch);
        Result := StrPas(ch);
      end
      else
        AddLog(Format(tecs_MissingDLLFunction, [stMakeStudioDLLGetRequiredPluginsProc, Filename]));
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

function TMakeStudioPlugin.GetOptionsGUID: TGUID;
var
  proc: TMakeStudioDLLGetOptionsPageGUIDProc;
begin
  Result := GUID_NULL;
  try
    if Kind = pkWin32DLL then
    begin
      @proc := GetProcAddress(DLLHandle, stMakeStudioDLLGetOptionsPageGUID);
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

function TMakeStudioPlugin.GetVersionInfo: string;
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

function TMakeStudioPlugin.HasOptionsDlg: Boolean;
begin
  Result := IsEqualGUID(GetOptionsGUID, GUID_NULL);
end;

function TMakeStudioPlugin.IsLoaded: Boolean;
begin
  Result := GetModuleHandle(PChar(ExtractFileName(Filename))) <> 0;
end;

procedure TMakeStudioPlugin.RequiredPluginsList(AList: TStrings);
var
  S: string;
begin
  AList.Clear;
  S := GetRequiredPlugins;
  if S <> '' then
    AList.Text := StringReplace(S, stcPluginSeparator, #10#13, [rfReplaceAll]);
end;

procedure TMakeStudioPlugin.RequiredPluginsFilenameList(AList: TStrings);
var
  I: Integer;
begin
  RequiredPluginsList(AList);
  for I := 0 to AList.Count - 1 do
    AList[I] := PathAddSeparator(ExtractFilePath(Forms.Application.ExeName)) +
      ChangeFileExt(AList[I], '.' + stcPluginExt);
end;

{------------------------------------------------------------------------------}
// TMakeStudioPlugins

function TMakeStudioPlugins.IsPluginLoaded(Filename: string): Boolean;
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

function TMakeStudioPlugins.Add(AMakeStudioPlugin: TMakeStudioPlugin): Integer;
begin
  Result := inherited Add(AMakeStudioPlugin);
end;

function TMakeStudioPlugins.GetItems(Index: Integer): TMakeStudioPlugin;
begin
  Result := TMakeStudioPlugin(inherited Items[Index]);
end;

function TMakeStudioPlugins.IndexOf(AMakeStudioPlugin: TMakeStudioPlugin): Integer;
begin
  Result := inherited IndexOf(AMakeStudioPlugin);
end;

procedure TMakeStudioPlugins.Insert(Index: Integer; AMakeStudioPlugin: TMakeStudioPlugin);
begin
  inherited Insert(Index, AMakeStudioPlugin);
end;

function TMakeStudioPlugins.Remove(AMakeStudioPlugin: TMakeStudioPlugin): Integer;
begin
  Result := inherited Remove(AMakeStudioPlugin);
end;

procedure TMakeStudioPlugins.SetItems(Index: Integer; AMakeStudioPlugin: TMakeStudioPlugin);
begin
  inherited Items[Index] := AMakeStudioPlugin;
end;

function TMakeStudioPlugins.GetItemsByName(ID: string): TMakeStudioPlugin;
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

procedure TMakeStudioPlugins.SetItemsByName(ID: string; AMakeStudioPlugin: TMakeStudioPlugin);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if CompareText(Items[I].GetName, ID) = 0 then
    begin
      Items[I] := AMakeStudioPlugin;
      Break;
    end;
end;

{------------------------------------------------------------------------------}
// TExDLLHandler

constructor TMakeStudioPluginHandler.Create(AMakeKind: EMakeKind = mkGUI);
begin
  inherited Create;
  FAppInterface := TJApplication.Create(AMakeKind);
  FPlugins := TMakeStudioPlugins.Create;
  FAdditionalInfo := TStringList.Create;
  FCredits := TStringList.Create;

  FCredits.Add( 'This project was launched in 2001. With the original goal to simplify and automate the installation of many packages in the Delphi development environment. Today it is a software that manages the build and setup/deployment.');
  FCredits.Add( 'Thanks to Uwe Schuster, for the code review in the first years. Thanks to Jean-Pierre Robin and Steffen Hornig for supporting the project until today.');
  FCredits.Add('');
end;

destructor TMakeStudioPluginHandler.Destroy;
begin
  FreeAllLibs;
  FAdditionalInfo.Free;
  FCredits.Free;
  FAppInterface := nil;
  FPlugins.Free;
  inherited Destroy;
end;

function TMakeStudioPluginHandler.LoadAssembly(AFilename: string): Boolean;
var
  plgItem: TMakeStudioPlugin;
begin
  Result := True;

  if FPlugins.IsPluginLoaded(AFileName) then Exit;

  AddLog(Format(sttwr_LoadingPlugin, [AFileName]));

  //Create and fill plugin item
  plgItem := TMakeStudioPlugin.Create(AppInterface);
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

function TMakeStudioPluginHandler.LoadPlugin(AFilename: string): Boolean;
var
  proc: Pointer;
  lHandle: THandle;
  I: Integer;
  plgItem: TMakeStudioPlugin;
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
    proc := GetProcAddress(lHandle, stMakeStudioDLLIdentifyProc);

    if proc <> nil then
    begin
      //AddLog('Proc war <> nil');

      //Create and fill plugin item
      plgItem := TMakeStudioPlugin.Create(AppInterface);
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
      AddLog(Format(tecs_MissingDLLFunction, [stMakeStudioDLLIdentifyProc, AFilename]));
    end;
  end
  else
    AddLog(Format(tecs_ErrorLoadingPlugin, [AFilename]));
end;

procedure TMakeStudioPluginHandler.LoadAllLibs;
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

procedure TMakeStudioPluginHandler.FreeAllLibs;
begin
  FPlugins.Clear;
end;

procedure TMakeStudioPluginHandler.UnregisterAllLibs;
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

function TMakeStudioPluginHandler.GetDotNetLoaded: Boolean;
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

function TMakeStudioPluginHandler.GetLoaded: Boolean;
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

procedure TMakeStudioPluginHandler.AfterAllPluginsLoaded;
var
  I: Integer;
begin
  //AfterAllPluginsLoaded
  for I := 0 to Plugins.Count - 1 do
    Plugins[I].AfterAllPluginsLoaded;
end;

function TMakeStudioPlugin.GetPluginClassName: string;
begin
  Result := ExtractFileName(ChangeFileExt(Filename, '')) + '.' + stNetPluginClassName;
end;

function TMakeStudioPluginHandler.IsNetPlugin(AFilename: string): Boolean;
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

destructor TMakeStudioPlugin.Destroy;
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

procedure TMakeStudioPluginHandler.LoadNETLibs;
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

procedure TMakeStudioPluginHandler.LoadDLLLibs;
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
