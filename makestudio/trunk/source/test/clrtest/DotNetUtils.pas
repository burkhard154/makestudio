unit DotNetUtils;

interface

{$I JEDI.INC}

uses
	Windows, Classes, ActiveX, ComObj, mscoree_tlb, mscorlib_tlb, sysutils
	{$IFDEF DELPHI6_UP} ,Variants {$ENDIF};


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
		lpvArgument1: Pointer; lpvArgument2: Pointer; szVersion: PWideCHar;
		pvReserver: Pointer): HRESULT; stdcall = nil;
	GetRealProcAddress: function (pwszProcName: PChar {?}; out ppv: Pointer): HRESULT; stdcall = nil;
	CorExitProcess: procedure (exitCode: Integer); stdcall = nil;

type
	TCorStartupFlags = TOleEnum;
const
	STARTUP_CONCURRENT_GC         					= $1;      //Specifies concurrent garbage collection should be used

	STARTUP_LOADER_OPTIMIZATION_MASK 				= 6;       // loader optimization mask
	STARTUP_LOADER_OPTIMIZATION_SINGLE_DOMAIN 		= 2;       // no domain neutral loading
	STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN 		= 4;       // all domain neutral loading
	STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN_HOST 	= 6;       // strong name domain neutral loading

	STARTUP_LOADER_SAFEMODE 						= $10;     // Do not apply runtime version policy to the version passed in
	STARTUP_LOADER_SETPREFERENCE 					= $100;    // Set preferred runtime. Do not actually start it

type
	COR_GC_STAT_TYPES = TOleEnum;
	TCorGCStatTypes = COR_GC_STAT_TYPES;
const
	COR_GC_COUNTS                 = $00000001;         // Fill out count values.
	COR_GC_MEMORYUSAGE            = $00000002;         // Fill out memory usage values.

type
	COR_GC_THREAD_STATS_TYPES = TOleEnum;
	TCorGCThreadStatsTypes = COR_GC_THREAD_STATS_TYPES;
const
	COR_GC_THREAD_HAS_PROMOTED_BYTES  = $00000001;      // Thread has bytes promoted in the last GC
														// if flags set to this value.

//------------------------------------------------------------------------------

var
	//Handle to 'mscoree.dll', do not change
	libMscoree: THandle 				= 0;
	//Initialized in StartupCLR, do not change
	ClrRuntimeHost: ICorRuntimeHost 	= nil;

//Public Routines
procedure DotNetSetFPU;
procedure DotNetRestoreFPU;
procedure DotNetCheck( H: HRESULT);
procedure StartDotNetFramework;
procedure StopDotNetFramework;
function CreateDotNetObject(const Filename, TypeName: WideString): OleVariant;

implementation

var
  FPU : Word;
	DotNetVersion: PWideChar = nil;
	DotNetBuildFlavor: PWideChar = nil;
	DotNetStartupFlags: TCorStartupFlags 	= 0;

const
  DotNetError = $80000000;

procedure DotNetSetFPU;
begin
  FPU := Get8087CW;
  Set8087CW( $133F);
end;

procedure DotNetRestoreFPU;
begin
  Set8087CW( FPU);
end;

procedure DotNetCheck( H: HRESULT);
begin
	if H and DotNetError <> 0 then begin
		if Assigned(SafeCallErrorProc) then TSafeCallErrorProc(SafeCallErrorProc)(H, nil)
	  else OleError(H);
  end;
end;

procedure StartDotNetFramework;
begin
	if Assigned(ClrRuntimeHost) then Exit;

	DotNetSetFPU;
	try

    if Assigned(@CorBindToRuntimeEx) then
			DotNetCheck(CorBindToRuntimeEx(DotnetVersion, DotnetBuildFlavor, DotnetStartupFlags,
				CLASS_CorRuntimeHost, ICorRuntimeHost, IUnknown(ClrRuntimeHost)))
    else
			raise Exception.Create('.Net framework is not installed');

		if not Assigned(ClrRuntimeHost) then
			raise Exception.Create( '.Net framework is not installed');

		DotNetCheck( ClrRuntimeHost.Start);
  finally
		DotNetRestoreFPU;
  end;
end;

procedure StopDotNetFramework;
begin
	if Assigned(ClrRuntimeHost) and not IsLibrary then begin
		DotNetSetFPU;
		try
			ClrRuntimeHost.Stop;
    finally
			DotNetRestoreFPU;
	  end;
  end;
	ClrRuntimeHost := nil;
end;

function CreateDotNetObject(const Filename, TypeName: WideString): OleVariant;
var U: IUnknown;
    Domain: _AppDomain;
begin
	StartDotNetFramework;

	DotNetCheck( ClrRuntimeHost.GetDefaultDomain( U));
	Domain := U as _AppDomain;

	DotNetSetFPU;
	try
		Result := Domain.CreateInstanceFrom( Filename, TypeName).Unwrap;
  finally
  	DotNetRestoreFPU;
	end;
end;


initialization
	libMscoree := LoadLibrary('mscoree.dll');

	if (libMscoree = 0) or (libMscoree = INVALID_HANDLE_VALUE) then
		libMscoree := 0

  else begin
    @GetCORSystemDirectory		:= GetProcAddress(libMscoree, 'GetCORSystemDirectory');
    @GetCORVersion				:= GetProcAddress(libMscoree, 'GetCORVersion');
    @GetCORRequiredVersion		:= GetProcAddress(libMscoree, 'GetCORRequiredVersion');
    @CorBindToRuntimeHost		:= GetProcAddress(libMscoree, 'CorBindToRuntimeHost');
    @CorBindToRuntimeEx			:= GetProcAddress(libMscoree, 'CorBindToRuntimeEx');
    @CorBindToRuntimeByCfg		:= GetProcAddress(libMscoree, 'CorBindToRuntimeByCfg');
    @CorBindToRuntime			:= GetProcAddress(libMscoree, 'CorBindToRuntime');
    @CorBindToCurrentRuntime	:= GetProcAddress(libMscoree, 'CorBindToCurrentRuntime');
    @ClrCreateManagedInstance	:= GetProcAddress(libMscoree, 'ClrCreateManagedInstance');
    @CorMarkThreadInThreadPool	:= GetProcAddress(libMscoree, 'CorMarkThreadInThreadPool');
    @LoadLibraryShim			:= GetProcAddress(libMscoree, 'LoadLibraryShim');
    @CallFunctionShim			:= GetProcAddress(libMscoree, 'GetFunctionShim');
    @GetRealProcAddress			:= GetProcAddress(libMscoree, 'GetRealProcAddress');
    @CorExitProcess				:= GetProcAddress(libMscoree, 'CorExitProcess');
  end;

finalization

  StopDotNetFramework;

	@GetCORSystemDirectory		:= nil;
	@GetCORVersion				:= nil;
	@GetCORRequiredVersion		:= nil;
	@CorBindToRuntimeHost		:= nil;
	@CorBindToRuntimeEx			:= nil;
	@CorBindToRuntimeByCfg		:= nil;
	@CorBindToRuntime			:= nil;
	@CorBindToCurrentRuntime	:= nil;
	@ClrCreateManagedInstance	:= nil;
	@CorMarkThreadInThreadPool	:= nil;
	@LoadLibraryShim			:= nil;
	@CallFunctionShim			:= nil;
	@GetRealProcAddress			:= nil;
	@CorExitProcess				:= nil;

	if libMscoree <> 0 then FreeLibrary(libMscoree);
	libMscoree := 0;
end.
