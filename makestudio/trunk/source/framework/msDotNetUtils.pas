(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: DotNetUtils.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:                                                                                    }

2006/06/02  BSchranz  - Added
2006/06/07  USchuster - D5 fix

-----------------------------------------------------------------------------*)
//Globals and global funtions
unit msDotNetUtils;

{$I jedi.inc}

interface

uses
  {$IFDEF DELPHI6_UP}
  Variants,
  {$ENDIF DELPHI6_UP}
  Windows, Classes, ActiveX, ComObj, mscoree_TLB, mscorlib_TLB, SysUtils;

var
  GetCORSystemDirectory: function (pbBuffer: PWideChar; cchBuffer: DWORD;
    out dwLength: DWORD): HRESULT; stdcall = nil;
  GetCORVersion: function (pbBuffer: PWideChar; cchBuffer: DWORD;
    out dwLength: DWORD): HRESULT; stdcall = nil;
  GetCORRequiredVersion: function (pBuffer: PWideChar; cchBuffer: DWORD;
    out dwLength: DWORD): HRESULT; stdcall = nil;
  CorBindToRuntimeHost: function (pwszVersion: PWideChar; pwszBuildFlavor: PWideChar;
    pwszHostConfigFile: PWideChar; pReserver: Pointer; startupFlags: DWORD;
    const rclsid: TCLSID; const riid: TIID; out ppv: IUnknown): HRESULT; stdcall = nil;
  CorBindToRuntimeEx: function (pwszVersion: PWideChar; pwszBuildFlavor: PWideChar;
    startupFlags: DWORD; const rclsid: TCLSID; const riid: TIID; out ppv: IUnknown): HRESULT; stdcall = nil;
  CorBindToRuntimeByCfg: function (pCfgStream: IStream; reserved: DWORD; startupFlags: DWORD;
    const rclsid: TCLSID; const riid: TIID; out ppv: IUnknown): HRESULT; stdcall = nil;
  CorBindToRuntime: function (pwszVersion: PWideChar; pwszBuildFlavor: PWideChar;
    const rclsid: TCLSID; const riid: TIID; out ppv: IUnknown): HRESULT; stdcall = nil;
  CorBindToCurrentRuntime: function (pwszFileName: PWideChar; const rclsid: TCLSID;
    const riid: TIID; out ppv: IUnknown): HRESULT; stdcall = nil;
  ClrCreateManagedInstance: function (pTypeName: PWideChar; const riid: TIID;
    out ppObject: IUnknown): HRESULT; stdcall = nil;
  CorMarkThreadInThreadPool: procedure (); stdcall = nil;
  RunDll32ShimW: function (hwnd: HWND; hinst: HINST; lpszCmdLine: PWideChar;
    nCmdShow: Integer): HRESULT; stdcall = nil;
  LoadLibraryShim: function (szDllName: PWideChar; szVersion: PWideChar;
    pvReserver: Pointer; out phModDll: HMODULE): HRESULT; stdcall = nil;
  CallFunctionShim: function (szDllName: PWideChar; szFunctionName: PChar {?};
    lpvArgument1: Pointer; lpvArgument2: Pointer; szVersion: PWideChar;
    pvReserver: Pointer): HRESULT; stdcall = nil;
  GetRealProcAddress: function (pwszProcName: PChar {?}; out ppv: Pointer): HRESULT; stdcall = nil;
  CorExitProcess: procedure (exitCode: Integer); stdcall = nil;

type
  TCorStartupFlags = TOleEnum;

const
  STARTUP_CONCURRENT_GC                         = $1;      //Specifies concurrent garbage collection should be used
  STARTUP_LOADER_OPTIMIZATION_MASK              = 6;       // loader optimization mask
  STARTUP_LOADER_OPTIMIZATION_SINGLE_DOMAIN     = 2;       // no domain neutral loading
  STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN      = 4;       // all domain neutral loading
  STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN_HOST = 6;       // strong name domain neutral loading
  STARTUP_LOADER_SAFEMODE                       = $10;     // Do not apply runtime version policy to the version passed in
  STARTUP_LOADER_SETPREFERENCE                  = $100;    // Set preferred runtime. Do not actually start it

type
  COR_GC_STAT_TYPES = TOleEnum;
  TCorGCStatTypes = COR_GC_STAT_TYPES;

const
  COR_GC_COUNTS      = $00000001;
  COR_GC_MEMORYUSAGE = $00000002;

type
  COR_GC_THREAD_STATS_TYPES = TOleEnum;
  TCorGCThreadStatsTypes = COR_GC_THREAD_STATS_TYPES;

const
  COR_GC_THREAD_HAS_PROMOTED_BYTES = $00000001;

var
  libMscoree: THandle = 0;
  CorRuntimeHost: ICorRuntimeHost = nil;

//Public Routines
procedure DotNetSetFPU;
procedure DotNetRestoreFPU;
procedure DotNetCheck(H: HRESULT);
procedure StartDotNetFramework;
procedure StopDotNetFramework;
function CreateDotNetObject(const Filename, TypeName: WideString): OleVariant;

implementation

var
  FPU: Word;
  DotNetVersion: PWideChar = nil;
  DotNetBuildFlavor: PWideChar = nil;
  DotNetStartupFlags: TCorStartupFlags = 0;

const
  DotNetError = $80000000;

{$IFNDEF D5TODO}
{$IFDEF DELPHI5}
function Get8087CW: Word;
begin
  Result := $133F;
end;
{$ENDIF DELPHI5}
{$ENDIF ~D5TODO}

procedure DotNetSetFPU;
begin
  FPU := Get8087CW;
  Set8087CW($133F);
end;

procedure DotNetRestoreFPU;
begin
  Set8087CW(FPU);
end;

procedure DotNetCheck(H: HRESULT);
begin
  if H and DotNetError <> 0 then
  begin
    {$IFDEF DELPHI6_UP}
    if Assigned(SafeCallErrorProc) then
      TSafeCallErrorProc(SafeCallErrorProc)(H, nil)
    else
    {$ENDIF DELPHI6_UP}
      OleError(H);
  end;
end;

procedure StartDotNetFramework;
begin
  if Assigned(CorRuntimeHost) then Exit;

  DotNetSetFPU;
  try
    if Assigned(@CorBindToRuntimeEx) then
      DotNetCheck(CorBindToRuntimeEx(DotNetVersion, DotNetBuildFlavor, DotNetStartupFlags,
        CLASS_CorRuntimeHost, ICorRuntimeHost, IUnknown(CorRuntimeHost)))
    else
      raise Exception.Create('.Net framework is not installed');

    if not Assigned(CorRuntimeHost) then
      raise Exception.Create('.Net framework is not installed');

    DotNetCheck(CorRuntimeHost.Start);
  finally
    DotNetRestoreFPU;
  end;
end;

procedure StopDotNetFramework;
begin
  if Assigned(CorRuntimeHost) and not IsLibrary then
  begin
    DotNetSetFPU;
    try
      CorRuntimeHost.Stop;
    finally
      DotNetRestoreFPU;
    end;
  end;
  CorRuntimeHost := nil;
end;

function CreateDotNetObject(const Filename, TypeName: WideString): OleVariant;
var
  U: IUnknown;
  Domain: _AppDomain;
begin
  StartDotNetFramework;

  DotNetCheck(CorRuntimeHost.GetDefaultDomain(U));
  Domain := U as _AppDomain;

  DotNetSetFPU;
  try
    Result := Domain.CreateInstanceFrom(Filename, TypeName).Unwrap;
  finally
    DotNetRestoreFPU;
  end;
end;

initialization
  libMscoree := LoadLibrary('mscoree.dll');

  if (libMscoree = 0) or (libMscoree = INVALID_HANDLE_VALUE) then
    libMscoree := 0
  else
  begin
    @GetCORSystemDirectory     := GetProcAddress(libMscoree, 'GetCORSystemDirectory');
    @GetCORVersion             := GetProcAddress(libMscoree, 'GetCORVersion');
    @GetCORRequiredVersion     := GetProcAddress(libMscoree, 'GetCORRequiredVersion');
    @CorBindToRuntimeHost      := GetProcAddress(libMscoree, 'CorBindToRuntimeHost');
    @CorBindToRuntimeEx        := GetProcAddress(libMscoree, 'CorBindToRuntimeEx');
    @CorBindToRuntimeByCfg     := GetProcAddress(libMscoree, 'CorBindToRuntimeByCfg');
    @CorBindToRuntime          := GetProcAddress(libMscoree, 'CorBindToRuntime');
    @CorBindToCurrentRuntime   := GetProcAddress(libMscoree, 'CorBindToCurrentRuntime');
    @ClrCreateManagedInstance  := GetProcAddress(libMscoree, 'ClrCreateManagedInstance');
    @CorMarkThreadInThreadPool := GetProcAddress(libMscoree, 'CorMarkThreadInThreadPool');
    @LoadLibraryShim           := GetProcAddress(libMscoree, 'LoadLibraryShim');
    @CallFunctionShim          := GetProcAddress(libMscoree, 'GetFunctionShim');
    @GetRealProcAddress        := GetProcAddress(libMscoree, 'GetRealProcAddress');
    @CorExitProcess            := GetProcAddress(libMscoree, 'CorExitProcess');
  end;

finalization
  StopDotNetFramework;

  @GetCORSystemDirectory     := nil;
  @GetCORVersion             := nil;
  @GetCORRequiredVersion     := nil;
  @CorBindToRuntimeHost      := nil;
  @CorBindToRuntimeEx        := nil;
  @CorBindToRuntimeByCfg     := nil;
  @CorBindToRuntime          := nil;
  @CorBindToCurrentRuntime   := nil;
  @ClrCreateManagedInstance  := nil;
  @CorMarkThreadInThreadPool := nil;
  @LoadLibraryShim           := nil;
  @CallFunctionShim          := nil;
  @GetRealProcAddress        := nil;
  @CorExitProcess            := nil;

  if libMscoree <> 0 then FreeLibrary(libMscoree);
  libMscoree := 0;

end.
