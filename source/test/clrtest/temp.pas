{** Contains functions to get information about installed .Net framework such as
 directory, version, statistics on loaded .Net framework, function for create
 instances of .Net types.}
unit temp;

interface

uses
	Windows, Classes, ActiveX, ComObj, mscoree_tlb, mscorlib_tlb, mscoree,
	Variants;

//** Checks if mscoree.dll was succesfully loaded. It must be in %System% directory.
function IsNetFrameworkInstalled(): Boolean;
//** Raises exception if Net framework is not installed.
procedure CheckNetFrameworkInstalled();
//** Checks if Net framework was succesfully loaded.
function IsNetFrameworkLoaded(): Boolean;
//** Raises exception is Net framework is not loaded succesfully.
procedure CheckNetFrameworkLoaded();

//** Returns directory where .Net framework is installed.
function ClrGetSystemDirectory(): WideString;
//** Returns the version number of the CLR that is running in the current process.
function ClrGetVersion(): WideString;
//** @exclude
function ClrGetRequiredVersion(): WideString;
//** Returns windows path + 'Assembly', for example, C:\Windows\Assembly.
function ClrGetAssemblyPath(): WideString;
//** Returns Global assembly cache path, appends Assembly\Gac to windows directory.
function ClrGetGacPath(): WideString;
{** Returns Global assembly cache path, appends Assembly\Gac_32 to windows directory.
 .Net 2.0 or higher.}
function ClrGetGac32Path(): WideString;
{** Returns Global assembly cache path, appends Assembly\Gac_msil to windows directory.
 .Net 2.0 or higher.}
function ClrGetGacMsilPath(): WideString;
//** Returns regitered assemblies paths. Uses SOFTWARE\Microsoft\.NETFramework\AssemblyFolders registry key.
procedure ClrGetAssemblyFolders(Folders: TStrings);

{**Starts CLR if not started yet. It is safe to call this function
 multiple times.}
procedure StartupCLR();
//** Stops CLR. After calling this method CLR can not be started again.
procedure StopCLR();
//** Sets CLR version without loading it.
procedure ClrSetVersion(const Version: WideString);
//** Indicates whether or not a managed debugger is attached to this process.
function ClrDebuggerAttached(): Boolean;
//** Returns the default domain.
function ClrGetDefaultDomain(): _AppDomain;
//** Returns the threads domain.
function ClrGetCurrentDomain(): _AppDomain;
//** Returns new AppDomainSetup object.
function ClrCreateAppDomainSetup(): IAppDomainSetup;
//** Returns new _Evidence object.
function ClrCreateEvidence(): _Evidence;
{** Returns an object for configuring the runtime prior to
 it starting. If the runtime has been initialized this
 routine returns an error. See ICorConfiguration.}
function ClrGetConfiguration(): ICorConfiguration;
//** Creates instance of Assembly.Type in default domain
function ClrCreateInstance(const AssemblyName, TypeName: WideString): OleVariant; overload;
//** Creates instance of Assembly.Type in specified domain
function ClrCreateInstance(const AssemblyName, TypeName: WideString; domain: _AppDomain): OleVariant; overload;
//** Create instance of Type in AssemblyFile in default domain
function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString): OleVariant; overload;
//** Create instance of Type in AssemblyFile in specified domain
function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString; domain: _AppDomain): OleVariant; overload;
//** Creates instance of Assembly.Type in default domain
function ClrCreateInstance(const AssemblyName, TypeName: WideString;
	const Args: OleVariant): OleVariant; overload;
//** Creates instance of Assembly.Type in specified domain
function ClrCreateInstance(const AssemblyName, TypeName: WideString;
	const Args: OleVariant; domain: _AppDomain): OleVariant; overload;
//** Creates instance of Type in AssemblyFile in default domain
function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString;
	const Args: OleVariant): OleVariant; overload;
//** Creates instance of Type in AssemblyFile in specified domain
function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString;
	const Args: OleVariant; domain: _AppDomain): OleVariant; overload;
//** Creates instance of Assembly.Type in default domain
function ClrCreateInstance(const AssemblyName, TypeName: WideString;
	const Args: array of Variant): OleVariant; overload;
//** Creates instance of Type in AssemblyFile in default domain
function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString;
	const Args: array of Variant): OleVariant; overload;
//** Returns Assembly.Type in default domain
function ClrGetType(const AssemblyName, TypeName: WideString): _Type; overload;
//** Returns Assembly.Type in specified domain
function ClrGetType(const AssemblyName, TypeName: WideString; domain: _AppDomain): _Type; overload;
//** Returns Type from Assembly in default domain
function ClrGetTypeFrom(const AssemblyFile, TypeName: WideString): _Type; overload;
//** Returns Type from Assembly in specified domain
function ClrGetTypeFrom(const AssemblyFile, TypeName: WideString; domain: _AppDomain): _Type; overload;
//** Loads Assembly in default domain
function ClrLoadAssembly(const AssemblyName: WideString): _Assembly; overload;
//** Loads Assembly in specified domain
function ClrLoadAssembly(const AssemblyName: WideString; domain: _AppDomain): _Assembly; overload;
//** Loads Assembly in default domain from file in default domain
function ClrLoadAssemblyFrom(const AssemblyFile: WideString): _Assembly; overload;
//** Loads Assembly in default domain from file in specified domain
function ClrLoadAssemblyFrom(const AssemblyFile: WideString; domain: _AppDomain): _Assembly; overload;
//** Loads Assembly from partial name in default domain
function ClrLoadAssemblyWithPartialName(const AssemblyName: WideString): _Assembly; overload;
//** Loads Assembly from partial name in specified domain
function ClrLoadAssemblyWithPartialName(const AssemblyName: WideString; domain: _AppDomain): _Assembly; overload;
//** Loads Assembly from Stream
function ClrLoadAssemblyFromStream(Stream: TStream): _Assembly; overload;
//** Loads Assembly from stream in specified domain
function ClrLoadAssemblyFromStream(Stream: TStream; domain: _AppDomain): _Assembly; overload;
//** Loads Assembly from resource
function ClrLoadAssemblyFromResourceName(Instance: THandle; const ResName: string): _Assembly; overload;
//** Loads Assembly from resource
function ClrLoadAssemblyFromResourceName(Instance: THandle; const ResName: string;
	domain: _AppDomain): _Assembly; overload;
{** Returns OleVariant with VType = varUnknown and VUnknown = nil.
 Should be used to represent null when passing to .Net method.}
function ClrNullObject(): OleVariant;
{** Represents PSafeArray as OleVariant. PSafeArray is returned from many
 CLR routines, storing it as OleVariant ensures correct cleanup.}
function PSafeArrayAsOleVariant(psa: PSafeArray): OleVariant; overload;

(*
 * These procedures are used to get information about the GC system and
 * control some aspects of the GC.  This interface is for expert usage
 * only, and can severely impact the performance of an application if
 * used improperly!!
 *)
	(*
	 * Returns a set of current statistics about the state of the GC system.
	 * These values can then be used by a smart allocation system to help the
	 * GC run, by say adding more memory or forcing a collection.

		typedef struct _COR_GC_STATS
		{
			ULONG       Flags;                                  // What values to get.

			// Value when COR_GC_COUNTS is specified.
			SIZE_T       ExplicitGCCount;                        // How many times was GC forced to run by external request.
			SIZE_T       GenCollectionsTaken[3];                    // Number of collections done for each generation

			// Memory sizes, valid for COR_GC_MEMORYUSAGE.
			SIZE_T       CommittedKBytes;                        // Total committed bytes from all heaps.
			SIZE_T       ReservedKBytes;                         // Total reserved bytes from all heaps.
			SIZE_T       Gen0HeapSizeKBytes;                     // Size of gen 0 heap.
			SIZE_T       Gen1HeapSizeKBytes;                     // Size of gen 1 heap.
			SIZE_T       Gen2HeapSizeKBytes;                     // Size of gen 2 heap.
			SIZE_T       LargeObjectHeapSizeKBytes;              // Size of large object heap.
			SIZE_T       KBytesPromotedFromGen0;                 // How many bytes promoted to next generation.
			SIZE_T       KBytesPromotedFromGen1;

		} COR_GC_STATS;
	 *)
type
	//** Specify what information is needed.
	TGCHostStatTypes = set of (corGCCounts, corGCMemoryUsage);

//** Returns statistics gathered by the GC.
procedure ClrGCHostStats(StatTypes: TGCHostStatTypes; out Stats: _COR_GC_STATS);
//** This method returns the per-thread statistics gathered by the GC.
procedure ClrGCThreadStats(var pFiberCookie: DWORD; out Stats: _COR_GC_THREAD_STATS);
{** Forces a collection to occur for the given generation, regardless of
 current GC statistics.  A value of -1 means collect all generations.}
procedure ClrGCCollect(Generation: Longint = -1);
{** Sets the segment size and gen 0 maximum size.  This value may only be
 specified once and will not change if called later.}
procedure ClrGCSetGCStartupLimits(SegmentSize, MaxGen0Size: DWORD);
{** This method allows the caller to set virtual memory limit (MB) of the runtime. This limit
 can be changed dynamically.}
procedure ClrGCSetVirtualMemLimit(sztMaxVirtualMemMB: ULONG_PTR);

//** Returns type of elements in safe-array.
function SafeArrayGetVartype(psa: PSafeArray; out pvt: TVarType): HRESULT; stdcall;
{$EXTERNALSYM SafeArrayGetVartype}

{** hrows an EOleSysError exception if the result code indicates an error.
 This function is similar to OleCheck for COM. ClrCheck provides more information
 than OleCheck. OleCheck and ClrCheck could be used in place of each other.}
procedure ClrCheck(hr: HRESULT);

//** Creates new .Net domain with AppDomainSetup and Evidence
function ClrCreateAppDomain(const DomainName: WideString; const Setup: IAppDomainSetup;
	const Evidence: IUnknown): _AppDomain; overload;
//** Creates new .Net domain with default settings.
function ClrCreateAppDomain(const DomainName: WideString): _AppDomain; overload;
//** Creates new .Net domain with AppDomainSetup
function ClrCreateAppDomain(const DomainName: WideString; const Setup: IAppDomainSetup): _AppDomain; overload;
//** Unloads domain from application.
procedure ClrUnloadAppDomain(domain: _AppDomain);

//** Returns AppDomain using its ID
function ClrFindDomainByID(ID: Integer): _AppDomain;
//** Returns ID of AppDomain where .Net object is located
function ClrGetObjectDomainID(const obj: IUnknown): Integer;
//** Returns AppDomain where .Net object is located
function ClrGetDomainFromObject(const obj: IUnknown): _AppDomain; overload;
{$IFDEF DELPHI6UP}
//** Returns AppDomain where .Net object is located
function ClrGetDomainFromObject(const obj: OleVariant): _AppDomain; overload;
{$ENDIF}
//** Enumerates assemblies in current domain. Names is filled with full assembly names
procedure ClrEnumAssemblies(Domain: _AppDomain; Names: TStrings);
//** Enumerates types in assembly
procedure ClrEnumAssemblyTypes(Assembly: _Assembly; Types: TStrings);

//** Extracts assembly name from full assembly name - string up to first ','
function ClrGetAssemblyPartialName(const FullName: WideString): WideString;
//** Finds loaded assembly in default domain using its full name
function ClrFindLoadedAssembly(const FullName: WideString): _Assembly; overload;
//** Finds loaded assembly in specified domain using its full name
function ClrFindLoadedAssembly(const FullName: WideString; Domain: _AppDomain): _Assembly; overload;
//** Finds loaded assembly in default domain using its partial name
function ClrFindLoadedAssemblyByPartialName(const PartialName: WideString): _Assembly; overload;
//** Finds loaded assembly in specified domain using its partial name
function ClrFindLoadedAssemblyByPartialName(const PartialName: WideString; Domain: _AppDomain): _Assembly; overload;
//** Returns reference to utilities assembly.
function ClrGetManagedVCLUtilsAssembly(): _Assembly; overload;
//** Returns reference to utilities assembly in specified domain.
function ClrGetManagedVCLUtilsAssembly(Domain: _AppDomain): _Assembly; overload;
//** Returns intance of assembly resolver type from utils assembly.
function ClrGetManagedVCLAssemblyResolver(): IAssemblyResolver; overload;
//** Returns intance of assembly resolver type from utils assembly in specified domain.
function ClrGetManagedVCLAssemblyResolver(Domain: _AppDomain): IAssemblyResolver; overload;
{** Registered assembly will be resolved by .Net framework when it will
 ask to resolve assembly with specified name.}
procedure ClrRegisterAssemblyToResolver(const Name: WideString; Assembly: _Assembly); overload;
{** Registered assembly will be resolved by .Net framework when it will
 ask to resolve assembly with specified name in specified domain.}
procedure ClrRegisterAssemblyToResolver(const Name: WideString; Assembly: _Assembly; Domain: _AppDomain); overload;
//** Unregister assembly from resolving.
procedure ClrUnregisterAssemblyFromResolver(const Name: WideString); overload;
//** Unregister assembly from resolving in specified domain.
procedure ClrUnregisterAssemblyFromResolver(const Name: WideString; Domain: _AppDomain); overload;
{** When called with Resolve = True .Net framework will resolve loaded assemblies when
 needed. This let reference assemblies loaded from resources or from stream.}
procedure ClrResolveLoadedAssemblies(ResolveLoaded: Boolean = True;
	ResolvePartialName: Boolean = True); overload;
{** When called with Resolve = True .Net framework will resolve loaded assemblies when
 needed. This let reference assemblies loaded from resources or from stream.}
procedure ClrResolveLoadedAssemblies(ResolveLoaded, ResolvePartialName: Boolean; Domain: _AppDomain); overload;


{$IFNDEF DELPHI7UP}
const
	VAR_INVALIDARG    = HRESULT($80070057); // = Windows.E_INVALIDARG
//** Converts variant array to PSafeArray
function VarArrayAsPSafeArray(const A: Variant): PVarArray;
{$ENDIF}

var
	{** Variables that controls CLR startup. CLR starts when TDotNetAppDomain.Active=true
	 is called first for any domain.
	 CorVersion - A string describing the version of the CLR you want to load.
	 Version numbers in the .NET Framework consist of four parts:
	 major.minor.build.revision.
	 The string passed as pwszVersion must start with the character 'v'
	 followed by the first three portions of the version number .  For example,
	 "v1.0.1529".
	 Set CorVersion = nil to load lastest version of CLR. If you change
	 predefined number you may need to recreate .Net type libraries.}
	CorVersion: PWideChar				= nil;
	{** CorBuildFlavor - The server build is optimized to take advantage of
	 multiple processors when doing garbage collections while the workstation
	 build is optimized for client applications running on a single processor machine.}
	CorBuildFlavor: PWideChar			= nil;	//'wks' or 'svr', nil = 'wks'
	{** CorStartupFlags -
	 STARTUP_CONCURRENT_GC.  Specifies concurrent garbage collection should be used.
		Note: If the caller asks for the "svr" build and concurrent gc on
		a uniproc machine we'll run "wks" and non-concurrent gc instead.<BR>
	 STARTUP_LOADER_OPTIMIZATION_SINGLE_DOMAIN.  No assemblies are loaded domain-neutral.<BR>
	 STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN. All assemblies are loaded domain-neutral.<BR>
	 STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN_HOST.  All strong named assemblies are loaded domain-neutral.<BR>
	 STARTUP_LOADER_SAFEMODE.  Specifies that the exact version of the CLR passed as
		pwszVersion will be loaded.  The shim will not evaluate policy to determine
		the latest compatible version.}
	CorStartupFlags: TCorStartupFlags 	= 0;
	{** Specify configuration file used to load CLR Runtime. If this variable is set,
	 CorBindToCurrentRuntime is used and CorVersion, CorBindFlavor and CorStartupFlags
	 are ignored. If not set - CorBindToRuntimeEx is used.}
	CorConfigFile: PWideChar			= nil;

var
	//Use assembly names from .Net 1.0 - .Net will load appropriate assembly.

	//** .Net core assembly. Should correspond with CorVersion.
	ClrCoreAssembly: WideString =
		'mscorlib, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089';
//		'mscorlib, Version = 1.0.5000.0, PublicKeyToken = b77a5c561934e089, Culture = neutral';
	//**
	ClrSystemWindowsFormsAssembly: WideString =
		'System.Windows.Forms, Culture = neutral, PublicKeyToken = b77a5c561934e089, Version = 1.0.3300.0';
//		'System.Windows.Forms, Version = 1.0.5000.0, PublicKeyToken = b77a5c561934e089, Culture = neutral';

resourcestring
	//** @exclude
	SNetFrameworkNotInstalled 	= '.Net framework is not installed';
	//** @exclude
	SNetFrameworkNotLoaded		= '.Net framework is not loaded';
	{$IFNDEF DELPHI6UP}
	//** @exclude
	STooManyParams = 'Dispatch methods do not support more than 64 parameters';
	{$ENDIF}

{$IFDEF TRIAL}
	//** @exclude
	SManagedVCLUnregistered = 'Managed VCL - UNREGISTERED VERSION';
{$ENDIF}

{$IFNDEF DELPHI6UP}
type
	//** @exclude
	TVarDispProc = procedure (Dest: PVariant; const Source: Variant;
		CallDesc: PCallDesc; Params: Pointer); cdecl;

const
	//** @exclude
	NullDomain: _AppDomain = nil;

function Get8087CW(): Word;
{$ENDIF}

const
	//** @exclude
	MCW_EM: Word = $133F;

var
	{** If set to True, ClrUtils will call Set8087CW(MCW_EM) on startup.
	  This is required for compatibility with some Microsoft code. For example,
	  this call is recommended for using DirectX and OpenGL. Look also
	  implementation of CreateWindow() function in Windows unit.<BR>
	  Side effect is that floating-point operations do not raise exceptions.
	  For example: var a, b, c: Double; a := 1; b := 0; c := a / b; should
	  raise EZeroDivide exception. With MSCompatibleFPUWord c will contain
	  value INF. According .Net documentation "Floating-point operations never
	  throw an exception", however even .Net raises floating-point exceptions
	  if 8087 control word is set to $1332 - default value for Delphi.<BR>
	  Managed VCL will use MCW_EM where possible even if MSCompatibleFPUWord
	  is False.<BR>
	  This variable is used only in initialization, so it should be set
	  in initialization part of any unit.}
	MSCompatibleFPUWord: Boolean = True;

{$IFNDEF DELPHI6UP}
function WideCompareText(const S1, S2: WideString): Integer;
function WideSameText(const S1, S2: WideString): Boolean;
{$ENDIF}

implementation

uses
	Registry, SysUtils, ClrException, ComConst, tlhelp32;
	//ClrException is included to replace SafeCallErrorProc and handle .Net exceptions.

{$R ManagedVCL_Utils.res}

//C++ Builder 5 mises some interfaces declaration when
//producing mscorlib_tlb.hpp
{$IFDEF CBUILDER5}
	{$HPPEMIT '#include "ManagedVCL_BCB5_Fix.hpp"'}
{$ENDIF}

function SafeArrayGetVartype; external 'oleaut32.dll';

function GetNullUnknown(): IUnknown;
begin
	Result := nil;
end;

{$IFNDEF DELPHI6UP}
function DumbItDownFor95(const S1, S2: WideString; CmpFlags: Integer): Integer;
var
	a1, a2: AnsiString;
begin
	a1 := s1;
	a2 := s2;
	Result := CompareStringA(LOCALE_USER_DEFAULT, CmpFlags, PChar(a1), Length(a1),
		PChar(a2), Length(a2)) - 2;
end;

function WideCompareText(const S1, S2: WideString): Integer;
begin
	SetLastError(0);
	Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PWideChar(S1),
		Length(S1), PWideChar(S2), Length(S2)) - 2;
	case GetLastError of
	  0: ;
	  ERROR_CALL_NOT_IMPLEMENTED: Result := DumbItDownFor95(S1, S2, NORM_IGNORECASE);
	  else
		RaiseLastWin32Error;
	  end;
end;

function WideSameText(const S1, S2: WideString): Boolean;
begin
	Result := WideCompareText(S1, S2) = 0;
end;
{$ENDIF}

function IsNetFrameworkInstalled(): Boolean;
begin
	Result := (libMscoree <> 0);
end;

procedure CheckNetFrameworkInstalled();
begin
	if not IsNetFrameworkInstalled() then
		raise Exception.Create(SNetFrameworkNotInstalled);
end;

function IsNetFrameworkLoaded(): Boolean;
begin
	Result := Assigned(ClrRuntimeHost);
end;

procedure CheckNetFrameworkLoaded();
begin
	if not IsNetFrameworkLoaded() then
		raise Exception.Create(SNetFrameworkNotLoaded);
end;

procedure SetWideString(var str: WideString; Buffer: PWideChar; Len: Integer);
begin
	SetLength(str, len);
	Move(Buffer^, PWideChar(str)^, len * SizeOf(WideChar));
end;

function ClrGetSystemDirectory(): WideString;
var
	Buffer: array[0..MAX_PATH] of WideChar;
	len: DWORD;
begin
	if not Assigned(@GetCORSystemDirectory) then
		raise Exception.Create(SNetFrameworkNotInstalled);
	if GetCORSystemDirectory(@Buffer[0], MAX_PATH, len) = S_OK then begin
		SetWideString(Result, @Buffer[0], len-1); //len includes null-termination char
		if (Length(Result) > 0) and (Result[Length(Result)] <> '\') then
			Result := Result + '\';
		end
	  else
		Result := '';
end;

function ClrGetVersion(): WideString;
var
	Buffer: array[0..MAX_PATH] of WideChar;
	len: DWORD;
begin
	if not Assigned(@GetCORVersion) then
		raise Exception.Create(SNetFrameworkNotInstalled);
	if GetCORVersion(@Buffer[0], MAX_PATH, len) = S_OK then
		SetWideString(Result, @Buffer[0], len-1)
	  else
		Result := '';
end;

function ClrGetRequiredVersion(): WideString;
var
	Buffer: array[0..MAX_PATH] of WideChar;
	len: DWORD;
begin
	if not Assigned(@GetCORRequiredVersion) then
		raise Exception.Create(SNetFrameworkNotInstalled);
	if GetCORRequiredVersion(@Buffer[0], MAX_PATH, len) = S_OK then
		SetWideString(Result, @Buffer[0], len-1)
	  else
		Result := '';
end;

function GetWindowsPath(): string;
var
	len: Integer;
begin
	SetLength(Result, MAX_PATH);
	len := GetWindowsDirectory(PChar(Result), MAX_PATH+1);
	SetLength(Result, len);
	if (Length(Result) > 0) and (Result[Length(Result)] <> '\') then
		Result := Result + '\';
end;

function ClrGetAssemblyPath(): WideString;
begin
	Result := GetWindowsPath() + 'Assembly\';
end;

function ClrGetGACPath(): WideString;
begin
	Result := ClrGetAssemblyPath() + 'Gac\';
end;

function ClrGetGac32Path(): WideString;
begin
	Result := ClrGetAssemblyPath() + 'Gac_32\';
end;

function ClrGetGacMsilPath(): WideString;
begin
	Result := ClrGetAssemblyPath() + 'Gac_msil\';
end;

{$IFNDEF DELPHI6UP}
function Get8087CW(): Word;
asm
	PUSH    0
	FNSTCW  [ESP].Word
	POP     EAX
end;

function DirectoryExists(const Name: string): Boolean;
var
	Code: Integer;
begin
	Code := GetFileAttributes(PChar(Name));
  	Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$ENDIF}

procedure ClrGetAssemblyFolders(Folders: TStrings);
const
	AssemblyFoldersRoot = 'SOFTWARE\Microsoft\.NETFramework\AssemblyFolders';
var
	Registry: TRegistry;
	Values: TStringList;
	Path: string;
	i: Integer;
begin
	Registry := TRegistry.Create(KEY_READ);
	try
		Registry.RootKey := HKEY_LOCAL_MACHINE;
		if Registry.OpenKey(AssemblyFoldersRoot, False) then begin
			Values := TStringList.Create();
			try
				Registry.GetKeyNames(Values);
				Registry.CloseKey();
				for i:=0 to Values.Count-1 do begin
					if Registry.OpenKey(AssemblyFoldersRoot + '\' + Values[i], False) then begin
						Path := Registry.ReadString('');
						if DirectoryExists(Path) then Folders.Add(Path);
						Registry.CloseKey();
					  end;
				  end;
			  finally
				Values.Free();
			  end;
		  end;
	  finally
		Registry.Free();
	  end;
end;

{$IFDEF TRIAL}
	{$INCLUDE Shareware.inc}
{$ENDIF}

procedure StartupCLR();
var
	FPUCW: Word;
begin
	//If CLR is started, do nothing
	if Assigned(ClrRuntimeHost) then Exit;

	FPUCW := Get8087CW();
	try
		Set8087CW(MCW_EM);

		if Assigned(CorConfigFile) and Assigned(@CorBindToCurrentRuntime) then
			ClrCheck(CorBindToCurrentRuntime(CorConfigFile, CLASS_CorRuntimeHost,
				ICorRuntimeHost, IUnknown(ClrRuntimeHost)))
		  else if Assigned(@CorBindToRuntimeEx) then
			ClrCheck(CorBindToRuntimeEx(CorVersion, CorBuildFlavor, CorStartupFlags,
				CLASS_CorRuntimeHost, ICorRuntimeHost, IUnknown(ClrRuntimeHost)))
		  else
			raise Exception.Create(SNetFrameworkNotInstalled);

		if not Assigned(ClrRuntimeHost) then
			raise Exception.Create(SNetFrameworkNotInstalled);

		//Calling IClrRuntimeHost.Start() is necessary
		ClrCheck(ClrRuntimeHost.Start());
	  finally
		Set8087CW(FPUCW);
	  end;
end;

procedure StopCLR();
var
	FPUCW: Word;
begin
	if Assigned(ClrRuntimeHost) and not IsLibrary then begin
		FPUCW := Get8087CW();
		try
			Set8087CW(MCW_EM);
			ClrRuntimeHost.Stop();
		  finally
			Set8087CW(FPUCW);
		  end;
	  end;
	ClrRuntimeHost := nil;
end;

procedure ClrSetVersion(const Version: WideString);
var
	pRuntimeHost: ICorRuntimeHost;
begin
	if not IsNetFrameworkInstalled() then
		raise Exception.Create(SNetFrameworkNotInstalled);

	ClrCheck(CorBindToRuntimeEx(PWideChar(Version), nil, STARTUP_LOADER_SETPREFERENCE,
		CLASS_CorRuntimeHost, ICorRuntimeHost, IUnknown(pRuntimeHost)));
end;

procedure ClrGCHostStats(StatTypes: TGCHostStatTypes; out Stats: _COR_GC_STATS);
begin
	if not Assigned(ClrRuntimeHost) then
		raise Exception.Create(SNetFrameworkNotLoaded);

	FillChar(Stats, SizeOf(Stats), 0);
	if corGCCounts in StatTypes then
		Stats.Flags := Stats.Flags or COR_GC_COUNTS;
	if corGCMemoryUsage in StatTypes then
		Stats.Flags := Stats.Flags or COR_GC_MEMORYUSAGE;
	ClrCheck((ClrRuntimeHost as IGCHost).GetStats(Stats));
end;

procedure ClrGCThreadStats(var pFiberCookie: DWORD; out Stats: _COR_GC_THREAD_STATS);
begin
	if not Assigned(ClrRuntimeHost) then
		raise Exception.Create(SNetFrameworkNotLoaded);

	FillChar(Stats, SizeOf(Stats), 0);
	ClrCheck((ClrRuntimeHost as IGCHost).GetThreadStats(pFiberCookie, Stats));
end;

procedure ClrGCCollect(Generation: Longint = -1);
begin
	if not Assigned(ClrRuntimeHost) then
//		raise Exception.Create(SNetFrameworkNotLoaded);
		Exit;  //Treat .Net garbage collected while .Net framework is not loaded.

	ClrCheck((ClrRuntimeHost as IGCHost).Collect(Generation));
end;

procedure ClrGCSetGCStartupLimits(SegmentSize, MaxGen0Size: DWORD);
begin
	if not Assigned(ClrRuntimeHost) then
		raise Exception.Create(SNetFrameworkNotLoaded);

	ClrCheck((ClrRuntimeHost as IGCHost).SetGCStartupLimits(SegmentSize, MaxGen0Size));
end;

procedure ClrGCSetVirtualMemLimit(sztMaxVirtualMemMB: ULONG_PTR);
begin
	if not Assigned(ClrRuntimeHost) then
		raise Exception.Create(SNetFrameworkNotLoaded);

	ClrCheck((ClrRuntimeHost as IGCHost).SetVirtualMemLimit(sztMaxVirtualMemMB));
end;

function ClrDebuggerAttached(): Boolean;
var
	dbgAttached: Integer;
begin
	if not Assigned(ClrRuntimeHost) then
		raise Exception.Create(SNetFrameworkNotLoaded);

	dbgAttached := 0;
	ClrCheck((ClrRuntimeHost as IDebuggerInfo).IsDebuggerAttached(dbgAttached));
	Result := (dbgAttached = 0);
end;

function ClrGetDefaultDomain(): _AppDomain;
var
	domain: IUnknown;
begin
	StartupClr();
	ClrCheck(ClrRuntimeHost.GetDefaultDomain(domain));
	Result := domain as _AppDomain;
end;

function ClrGetCurrentDomain(): _AppDomain;
var
	domain: IUnknown;
begin
	StartupClr();
	ClrCheck(ClrRuntimeHost.CurrentDomain(domain));
	Result := domain as _AppDomain;
end;

function ClrCreateAppDomainSetup(): IAppDomainSetup;
var
	setup: IUnknown;
begin
	StartupClr();
	ClrCheck(ClrRuntimeHost.CreateDomainSetup(setup));
	Result := setup as IAppDomainSetup;
end;

function ClrCreateEvidence(): _Evidence;
var
	evidence: IUnknown;
begin
	StartupClr();
	ClrCheck(ClrRuntimeHost.CreateEvidence(evidence));
	Result := evidence as _Evidence;
end;

function ClrGetConfiguration(): ICorConfiguration;
var
	host: ICorRuntimeHost;
begin
	//If CLR is started, do nothing. This function can not be called when
	//ICorRuntimeHost.Start() was called.
	if Assigned(ClrRuntimeHost) then
		raise Exception.Create('Can not get configuration when CLR is running');

	if Assigned(@CorBindToRuntimeEx) then
		ClrCheck(CorBindToRuntimeEx(CorVersion, CorBuildFlavor, CorStartupFlags,
			CLASS_CorRuntimeHost, ICorRuntimeHost, IUnknown(host)))
	  else
		raise Exception.Create(SNetFrameworkNotInstalled);

	if not Assigned(host) then
		raise Exception.Create(SNetFrameworkNotInstalled);

	ClrCheck(host.GetConfiguration(Result));
end;

function ClrCreateInstance(const AssemblyName, TypeName: WideString): OleVariant;
begin
	Result := ClrCreateInstance(AssemblyName, TypeName,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrCreateInstance(const AssemblyName, TypeName: WideString; domain: _AppDomain): OleVariant;
var
	pUnk: IUnknown;
	FPUCW: Word;
begin
	StartupClr();

	if not Assigned(domain) then begin
		ClrCheck(ClrRuntimeHost.GetDefaultDomain(pUnk));
		domain := pUnk as _AppDomain;
	  end;

	FPUCW := Get8087CW();
	try
		Set8087CW(MCW_EM);
		Result := domain.CreateInstance(AssemblyName, TypeName).Unwrap();
	  finally
		Set8087CW(FPUCW);
	  end;
end;

function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString): OleVariant;
begin
	Result := ClrCreateInstanceFrom(AssemblyFile, TypeName,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString; domain: _AppDomain): OleVariant;
var
	pUnk: IUnknown;
	FPUCW: Word;
begin
	StartupClr();

	if not Assigned(domain) then begin
		ClrCheck(ClrRuntimeHost.GetDefaultDomain(pUnk));
		domain := pUnk as _AppDomain;
	  end;

	FPUCW := Get8087CW();
	try
		Set8087CW(MCW_EM);
		Result := domain.CreateInstanceFrom(AssemblyFile, TypeName).Unwrap();
	  finally
		Set8087CW(FPUCW);
	  end;
end;

function ClrCreateInstance(const AssemblyName, TypeName: WideString;
	const Args: OleVariant): OleVariant;
begin
	Result := ClrCreateInstance(AssemblyName, TypeName, Args,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrCreateInstance(const AssemblyName, TypeName: WideString;
	const Args: OleVariant; domain: _AppDomain): OleVariant;
var
	pUnk: IUnknown;
	FPUCW: Word;
begin
	StartupClr();

	if not Assigned(domain) then begin
		ClrCheck(ClrRuntimeHost.GetDefaultDomain(pUnk));
		domain := pUnk as _AppDomain;
	  end;

	FPUCW := Get8087CW();
	try
		Set8087CW(MCW_EM);
		Result := domain.CreateInstance_3(AssemblyName,
			TypeName, True, BindingFlags_CreateInstance,
			nil, PSafeArray(VarArrayAsPSafeArray(Args)), nil, nil, nil).Unwrap();
	  finally
		Set8087CW(FPUCW);
	  end;
end;

function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString;
	const Args: OleVariant): OleVariant;
begin
	Result := ClrCreateInstanceFrom(AssemblyFile, TypeName, Args,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString;
	const Args: OleVariant; domain: _AppDomain): OleVariant;
var
	pUnk: IUnknown;
	FPUCW: Word;
begin
	StartupClr();

	if not Assigned(domain) then begin
		ClrCheck(ClrRuntimeHost.GetDefaultDomain(pUnk));
		domain := pUnk as _AppDomain;
	  end;

	FPUCW := Get8087CW();
	try
		Set8087CW(MCW_EM);
		Result := domain.CreateInstanceFrom_3(AssemblyFile,
			TypeName, True, BindingFlags_CreateInstance,
			nil, PSafeArray(VarArrayAsPSafeArray(Args)), nil, nil, nil).Unwrap();
	  finally
		Set8087CW(FPUCW);
	  end;
end;

function ClrCreateInstance(const AssemblyName, TypeName: WideString;
	const Args: array of Variant): OleVariant;
begin
	Result := ClrCreateInstance(AssemblyName, TypeName, OleVariant(VarArrayOf(Args)));
end;

function ClrCreateInstanceFrom(const AssemblyFile, TypeName: WideString;
	const Args: array of Variant): OleVariant;
begin
	Result := ClrCreateInstanceFrom(AssemblyFile, TypeName, OleVariant(VarArrayOf(Args)));
end;

function ClrGetType(const AssemblyName, TypeName: WideString): _Type;
begin
	Result := ClrGetType(AssemblyName, TypeName,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrGetType(const AssemblyName, TypeName: WideString; domain: _AppDomain): _Type;
var
	pUnk: IUnknown;
	assembly: _Assembly;
begin
	StartupClr();

	if not Assigned(domain) then begin
		ClrCheck(ClrRuntimeHost.GetDefaultDomain(pUnk));
		domain := pUnk as _AppDomain;
	  end;

	assembly := domain.Load_2(AssemblyName);
	Result := assembly.GetType_2(TypeName);
end;

function ClrGetTypeFrom(const AssemblyFile, TypeName: WideString): _Type;
begin
	Result := ClrGetTypeFrom(AssemblyFile, TypeName,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrGetTypeFrom(const AssemblyFile, TypeName: WideString; domain: _AppDomain): _Type;
var
	typeAssembly: _Type;
	assembly: _Assembly;
begin
	typeAssembly := ClrGetType(ClrCoreAssembly, 'System.Reflection.Assembly', domain);

	assembly := IUnknown(typeAssembly.InvokeMember_3('LoadFrom',
		BindingFlags_Static or BindingFlags_InvokeMethod or BindingFlags_Public,
		nil, Null, PSafeArray(VarArrayAsPSafeArray(VarArrayOf([AssemblyFile]))))) as
		_Assembly;
	Result := assembly.GetType_2(TypeName);
end;

function ClrLoadAssembly(const AssemblyName: WideString): _Assembly;
begin
	Result := ClrLoadAssembly(AssemblyName,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrLoadAssembly(const AssemblyName: WideString; domain: _AppDomain): _Assembly;
var
	pUnk: IUnknown;
begin
	StartupClr();

	if not Assigned(domain) then begin
		ClrCheck(ClrRuntimeHost.GetDefaultDomain(pUnk));
		domain := pUnk as _AppDomain;
	  end;

	Result := domain.Load_2(AssemblyName);
	if not Assigned(Result) then
		raise Exception.Create('Assembly does not exists:'#13#10 + AssemblyName);
end;

function ClrLoadAssemblyFrom(const AssemblyFile: WideString): _Assembly;
begin
	Result := ClrLoadAssemblyFrom(AssemblyFile,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrLoadAssemblyFrom(const AssemblyFile: WideString; domain: _AppDomain): _Assembly;
var
	typeAssembly: _Type;
begin
	typeAssembly := ClrGetType(ClrCoreAssembly, 'System.Reflection.Assembly', domain);
	Result := IUnknown(typeAssembly.InvokeMember_3('LoadFrom',
		BindingFlags_Static or BindingFlags_InvokeMethod or BindingFlags_Public,
		nil, Null, PSafeArray(VarArrayAsPSafeArray(VarArrayOf([AssemblyFile]))))) as
		_Assembly;
end;

function ClrLoadAssemblyWithPartialName(const AssemblyName: WideString): _Assembly;
begin
	Result := ClrLoadAssemblyWithPartialName(AssemblyName,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrLoadAssemblyWithPartialName(const AssemblyName: WideString; domain: _AppDomain): _Assembly;
var
	typeAssembly: _Type;
begin
	typeAssembly := ClrGetType(ClrCoreAssembly, 'System.Reflection.Assembly', domain);
	Result := IUnknown(typeAssembly.InvokeMember_3('LoadWithPartialName',
		BindingFlags_Static or BindingFlags_InvokeMethod or BindingFlags_Public,
		nil, Null, PSafeArray(VarArrayAsPSafeArray(VarArrayOf([AssemblyName]))))) as
		_Assembly;
end;

function ClrLoadAssemblyFromStream(Stream: TStream): _Assembly;
begin
	Result := ClrLoadAssemblyFromStream(Stream,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrLoadAssemblyFromStream(Stream: TStream; domain: _AppDomain): _Assembly;
var
	val: OleVariant;
	Count: Integer;
	p: System.Pointer;
	pUnk: IUnknown;
begin
	StartupClr();

	with Stream do Count := Size - Position;
	val := VarArrayCreate([0, Count-1], varByte);
	p 	:= VarArrayLock(val);
	try
		Stream.ReadBuffer(p^, Count);

		if not Assigned(domain) then begin
			ClrCheck(ClrRuntimeHost.GetDefaultDomain(pUnk));
			domain := pUnk as _AppDomain;
		  end;

		Result := domain.Load_3(PSafeArray(VarArrayAsPSafeArray(val)));
		domain := nil;
	  finally
		VarArrayUnlock(val);
	  end;
end;

function ClrLoadAssemblyFromResourceName(Instance: THandle; const ResName: string): _Assembly;
begin
	Result := ClrLoadAssemblyFromResourceName(Instance, ResName,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrLoadAssemblyFromResourceName(Instance: THandle; const ResName: string;
	domain: _AppDomain): _Assembly;
var
	resStream: TResourceStream;
begin
	resStream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
	try
		Result := ClrLoadAssemblyFromStream(resStream, domain);
	  finally
		resStream.Free();
	  end;
end;

function ClrNullObject(): OleVariant;
begin
	TVarData(Result).VType := varUnknown;
	TVarData(Result).VUnknown := nil;
end;

function PSafeArrayAsOleVariant(psa: PSafeArray): OleVariant;
var
	vType: TVarType;
begin
	vType := varEmpty;
	OleCheck(SafeArrayGetVarType(psa, vType));

	TVarData(Result).VType 	:= vType or varArray;
	TVarData(Result).VArray := PVarArray(psa);
end;

procedure ClrCheck(hr: HRESULT);
begin
	if {Failed(hr)} hr and $80000000 <> 0 then begin
		if Assigned(SafeCallErrorProc) then TSafeCallErrorProc(SafeCallErrorProc)(hr, nil)
		  else OleError(hr);
	  end;
end;

function ClrCreateAppDomain(const DomainName: WideString): _AppDomain;
begin
	Result := ClrCreateAppDomain(DomainName, ClrCreateAppDomainSetup(),
		{$IFDEF DELPHI6UP} nil {$ELSE} GetNullUnknown() {$ENDIF});
end;

function ClrCreateAppDomain(const DomainName: WideString; const Setup: IAppDomainSetup): _AppDomain;
begin
	Result := ClrCreateAppDomain(DomainName, Setup,
		{$IFDEF DELPHI6UP} nil {$ELSE} GetNullUnknown() {$ENDIF});
end;

function ClrCreateAppDomain(const DomainName: WideString; const Setup: IAppDomainSetup;
	const Evidence: IUnknown): _AppDomain;
var
	pUnk: IUnknown;
begin
	StartupClr();
	ClrCheck(ClrRuntimeHost.CreateDomainEx(PWideChar(domainName), Setup, Evidence, pUnk));
	Result := pUnk as _AppDomain; pUnk := nil;
end;

procedure ClrUnloadAppDomain(domain: _AppDomain);
begin
	if Assigned(domain) and Assigned(ClrRuntimeHost) then
		ClrCheck(ClrRuntimeHost.UnloadDomain(domain));
end;

{$IFNDEF DELPHI7UP}
function GetVarDataArrayInfo(const AVarData: TVarData; out AVarType: TVarType;
  out AVarArray: PVarArray): Boolean;
begin
  // variant that points to another variant?  lets go spelunking
  if AVarData.VType = varByRef or varVariant then
	Result := GetVarDataArrayInfo(PVarData(AVarData.VPointer)^, AVarType, AVarArray)
  else
  begin

	// make sure we are pointing to an array then
	AVarType := AVarData.VType;
	Result := (AVarType and varArray) <> 0;

	// figure out the array data pointer
	if Result then
	  if (AVarType and varByRef) <> 0 then
		AVarArray := PVarArray(AVarData.VPointer^)
	  else
		AVarArray := AVarData.VArray
	else
	  AVarArray := nil;
  end;
end;

function VarArrayAsPSafeArray(const A: Variant): PVarArray;
var
  LVarType: TVarType;
begin
  if not GetVarDataArrayInfo(TVarData(A), LVarType, Result) then
	{$IFDEF DELPHI6UP}
	VarResultCheck(VAR_INVALIDARG);
	{$ELSE}
	raise Exception.Create('Invalid argument');
	{$ENDIF}
end;
{$ENDIF}

const
{ Maximum number of dispatch arguments }
	MaxDispArgs = 64; {!!!}

procedure GetIDsOfNames(const Dispatch: IDispatch; Names: PChar;
  NameCount: Integer; DispIDs: PDispIDList);

  procedure RaiseNameException;
  begin
	raise EOleError.CreateResFmt(@SNoMethod, [Names]);
  end;

type
  PNamesArray = ^TNamesArray;
  TNamesArray = array[0..0] of PWideChar;
var
  N, SrcLen, DestLen: Integer;
  Src: PChar;
  Dest: PWideChar;
  NameRefs: PNamesArray;
  StackTop: Pointer;
  Temp: Integer;
begin
  Src := Names;
  N := 0;
  asm
	MOV  StackTop, ESP
	MOV  EAX, NameCount
	INC  EAX
	SHL  EAX, 2  // sizeof pointer = 4
	SUB  ESP, EAX
	LEA  EAX, NameRefs
	MOV  [EAX], ESP
  end;
  repeat
	SrcLen := StrLen(Src);
	DestLen := MultiByteToWideChar(0, 0, Src, SrcLen, nil, 0) + 1;
	asm
	  MOV  EAX, DestLen
	  ADD  EAX, EAX
	  ADD  EAX, 3      // round up to 4 byte boundary
	  AND  EAX, not 3
	  SUB  ESP, EAX
	  LEA  EAX, Dest
	  MOV  [EAX], ESP
	end;
	if N = 0 then NameRefs[0] := Dest else NameRefs[NameCount - N] := Dest;
	MultiByteToWideChar(0, 0, Src, SrcLen, Dest, DestLen);
	Dest[DestLen-1] := #0;
	Inc(Src, SrcLen+1);
	Inc(N);
  until N = NameCount;
  Temp := Dispatch.GetIDsOfNames(GUID_NULL, NameRefs, NameCount,
	{GetThreadLocale}0, DispIDs);
  if Temp = Integer(DISP_E_UNKNOWNNAME) then RaiseNameException else OleCheck(Temp);
  asm
	MOV  ESP, StackTop
  end;
end;

procedure VarClrInvoke(Result: PVariant; const Instance: Variant;
  CallDesc: PCallDesc; Params: System.Pointer); cdecl;
  procedure RaiseException();
  begin
	raise EOleError.CreateRes(@SVarNotObject);
  end;

  procedure GetDispatch(const pUnk: IUnknown; out Result: IDispatch);
  begin
	Result := nil;
	Supports(pUnk, IDispatch, Result);
	if Assigned(Result) then Exit;
	//If .Net object is not COM visible, use _Object interface as IDispatch.
	//IManagedObject returns E_NOINTERFACE in call to GetIDsOfNames.
	Supports(pUnk, _Object, Result);
  end;

var
	Dispatch: IDispatch;
	DispIDs: array[0..MaxDispArgs - 1] of Integer;
	FPUCW: Word;
begin
	if (CallDesc^.ArgCount) > MaxDispArgs then raise EOleError.CreateRes(@STooManyParams);

	case TVarData(Instance).VType of
	  varDispatch:           	Dispatch := IDispatch(TVarData(Instance).VDispatch);
	  varDispatch or varByRef: 	Dispatch := IDispatch(TVarData(Instance).VPointer^);
	  //If OleVariant if not varDispatch, try get IDispatch from IUnknown.
	  varUnknown:				GetDispatch(IUnknown(TVarData(Instance).VUnknown), Dispatch);
	  varUnknown or varByRef:	GetDispatch(IUnknown(TVarData(Instance).VPointer^), Dispatch);
	  else 						RaiseException;
	  end;
	if not Assigned(Dispatch) then RaiseException();

	GetIDsOfNames(Dispatch, @CallDesc^.ArgTypes[CallDesc^.ArgCount],
		CallDesc^.NamedArgCount + 1, @DispIDs);
	if Result <> nil then VarClear(Result^);

	FPUCW := Get8087CW();
	try
		Set8087CW(MCW_EM);

		DispatchInvoke(Dispatch, CallDesc, @DispIDs,
			{$IFDEF DELPHI6UP} Params, {$ELSE} @Params, {$ENDIF}
			Result);
	  finally
		Set8087CW(FPUCW);
	  end;
end;

function ClrFindDomainByID(ID: Integer): _AppDomain;
var
	hr: HRESULT;
	enum: System.Pointer;
	pUnk: IUnknown;
	pManagedObj: IManagedObject;
	strGuid: WideString;
	objID, CCW: Integer;
begin
	Result := nil;
	StartupClr();

	hr := ClrRuntimeHost.EnumDomains(enum);
	ClrCheck(hr);
	try
		while ClrRuntimeHost.NextDomain(enum, pUnk) = S_OK do begin
			if Assigned(pUnk) and Supports(pUnk, IManagedObject, pManagedObj) then begin
				Finalize(strGuid);
				hr := pManagedObj.GetObjectIdentity(strGuid, objID, CCW);
				ClrCheck(hr);

				pManagedObj := nil;

				if (objID = ID) then begin
					Supports(pUnk, AppDomain, Result);
					Break;
				  end;

            	pUnk := nil;				  
			  end;
		  end;
	  finally
		hr := ClrRuntimeHost.CloseEnum(enum);
		ClrCheck(hr);
	  end;

	if not Assigned(Result) then
		raise Exception.CreateFmt('Can not find domain %d.', [ID]);
end;

function ClrGetObjectDomainID(const obj: IUnknown): Integer;
var
	hr: HRESULT;
	pManagedObj: IManagedObject;
	strGuid: WideString;
	ID, CCW: Integer;
begin
	if Assigned(obj) and Supports(obj, IManagedObject, pManagedObj) then begin
		hr := pManagedObj.GetObjectIdentity(strGuid, ID, CCW);
		ClrCheck(hr);
		Result := ID;
		end
	  else
		raise Exception.Create('.Net object required.');
end;

function ClrGetDomainFromObject(const obj: IUnknown): _AppDomain; overload;
var
	hr: HRESULT;
	pManagedObj: IManagedObject;
	strGuid: WideString;
	ID, CCW: Integer;
begin
	Result := nil;

	if Assigned(obj) and Supports(obj, IManagedObject, pManagedObj) then begin
		hr := pManagedObj.GetObjectIdentity(strGuid, ID, CCW);
		ClrCheck(hr);
		pManagedObj := nil;

		Result := ClrFindDomainByID(ID);
		end
	  else
		raise Exception.Create('.Net object required.');
end;

{$IFDEF DELPHI6UP}
function ClrGetDomainFromObject(const obj: OleVariant): _AppDomain; overload;
begin
	Result := nil;
	if VarType(obj) in [varUnknown, varDispatch] then
		Result := ClrGetDomainFromObject(IUnknown(obj))
	  else
		raise Exception.Create('Invalid object instance.');
end;
{$ENDIF}

procedure ClrEnumAssemblies(Domain: _AppDomain; Names: TStrings);
var
	p: PSafeArray;
	v: OleVariant;
	i: Integer;
begin
	if not Assigned(Domain) then Domain := ClrGetDefaultDomain();
	p := Domain.GetAssemblies();
	v := PSafeArrayAsOleVariant(p);

	Names.BeginUpdate();
	try
		for i:=VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do
			Names.Add((IUnknown(v[i]) as _Assembly).FullName);
	  finally
		Names.EndUpdate();
	  end;
end;

procedure ClrEnumAssemblyTypes(Assembly: _Assembly; Types: TStrings);
var
	ClrTypes: OleVariant;
	i: Integer;
	t: _Type;
begin
	if not Assigned(Assembly) or not Assigned(Types) then Exit;

	ClrTypes := PSafeArrayAsOleVariant(Assembly.GetTypes());
	for i:=VarArrayLowBound(ClrTypes, 1) to VarArrayHighBound(ClrTypes, 1) do begin
		t := IUnknown(ClrTypes[i]) as _Type;
		if Assigned(t) then
			Types.Add(t.FullName);
	  end;
end;

function ClrGetAssemblyPartialName(const FullName: WideString): WideString;
var
	p: Integer;
begin
	p := Pos(',', FullName);
	if p > 0 then Result := Copy(FullName, 1, p-1)
	  else Result := FullName;
end;


function ClrFindLoadedAssembly(const FullName: WideString): _Assembly;
begin
	Result := ClrFindLoadedAssembly(FullName,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrFindLoadedAssembly(const FullName: WideString; Domain: _AppDomain): _Assembly;
var
	p: PSafeArray;
	v: OleVariant;
	i: Integer;
	Assembly: _Assembly;
begin
	Result := nil;

	if not Assigned(Domain) then Domain := ClrGetDefaultDomain();
	p := Domain.GetAssemblies();
	v := PSafeArrayAsOleVariant(p);

	for i:=VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do begin
		Assembly := IUnknown(v[i]) as _Assembly;
		if WideSameText(Assembly.FullName, FullName) then begin
			Result := Assembly;
			Exit;
		  end;
	  end;
end;

function ClrFindLoadedAssemblyByPartialName(const PartialName: WideString): _Assembly;
begin
	Result := ClrFindLoadedAssemblyByPartialName(PartialName,
		{$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrFindLoadedAssemblyByPartialName(const PartialName: WideString; Domain: _AppDomain): _Assembly;
var
	p: PSafeArray;
	v: OleVariant;
	i: Integer;
	Assembly: _Assembly;
begin
	Result := nil;

	if not Assigned(Domain) then Domain := ClrGetDefaultDomain();
	p := Domain.GetAssemblies();
	v := PSafeArrayAsOleVariant(p);

	for i:=VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do begin
		Assembly := IUnknown(v[i]) as _Assembly;
		if WideSameText(ClrGetAssemblyPartialName(Assembly.FullName), PartialName) then begin
			Result := Assembly;
			Exit;
		  end;
	  end;
end;

function ClrGetManagedVCLUtilsAssembly(): _Assembly;
begin
	Result := ClrGetManagedVCLUtilsAssembly({$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrGetManagedVCLUtilsAssembly(Domain: _AppDomain): _Assembly;
var
	p: PSafeArray;
	v: OleVariant;
	i: Integer;
	Assembly: _Assembly;
begin
	Result := nil;

	if not Assigned(Domain) then Domain := ClrGetDefaultDomain();
	p := Domain.GetAssemblies();
	v := PSafeArrayAsOleVariant(p);

	for i:=VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do begin
		Assembly := IUnknown(v[i]) as _Assembly;
		if WideSameText(ClrGetAssemblyPartialName(Assembly.FullName), 'ManagedVCL.Utils') then begin
			Result := Assembly;
			Exit;
		  end;
	  end;

	//Result := ClrLoadAssembly('ManagedVCL.Utils');
	Result := ClrLoadAssemblyFromResourceName(hInstance, 'ManagedVCL_Utils');
end;

function ClrGetManagedVCLAssemblyResolver(): IAssemblyResolver;
begin
	Result := ClrGetManagedVCLAssemblyResolver({$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

function ClrGetManagedVCLAssemblyResolver(Domain: _AppDomain): IAssemblyResolver;
var
	Utils: _Assembly;
begin
	Result := nil;
	Utils := ClrGetManagedVCLUtilsAssembly(Domain);
	if not Assigned(Utils) then Exit;
	Result := IUnknown(Utils.CreateInstance('ManagedVCL.Utils.AssemblyResolver')) as IAssemblyResolver;
end;

procedure ClrRegisterAssemblyToResolver(const Name: WideString; Assembly: _Assembly);
begin
	ClrRegisterAssemblyToResolver(Name, Assembly, {$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

procedure ClrRegisterAssemblyToResolver(const Name: WideString; Assembly: _Assembly; Domain: _AppDomain);
var
	Resolver: IAssemblyResolver;
begin
	if not Assigned(Assembly) then Exit;
	if Length(Name) = 0 then raise Exception.Create('Assembly name can not be empty.');

	Resolver := ClrGetManagedVCLAssemblyResolver(Domain);
	if not Assigned(Resolver) then
		raise Exception.Create('Can not load assembly resolver.');
	Resolver.RegisterAssembly(Name, Assembly);
end;

procedure ClrUnregisterAssemblyFromResolver(const Name: WideString);
begin
	ClrUnregisterAssemblyFromResolver(Name, {$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

procedure ClrUnregisterAssemblyFromResolver(const Name: WideString; Domain: _AppDomain);
var
	Resolver: IAssemblyResolver;
begin
	if Length(Name) = 0 then raise Exception.Create('Assembly name can not be empty.');

	Resolver := ClrGetManagedVCLAssemblyResolver(Domain);
	if not Assigned(Resolver) then
		raise Exception.Create('Can not load assembly resolver.');
	Resolver.UnregisterAssembly(Name);
end;

procedure ClrResolveLoadedAssemblies(ResolveLoaded: Boolean;
	ResolvePartialName: Boolean);
begin
	ClrResolveLoadedAssemblies(ResolveLoaded, ResolvePartialname, {$IFDEF DELPHI6UP} nil {$ELSE} NullDomain {$ENDIF});
end;

procedure ClrResolveLoadedAssemblies(ResolveLoaded, ResolvePartialName: Boolean;
	Domain: _AppDomain);
var
	Resolver: IAssemblyResolver;
begin
	Resolver := ClrGetManagedVCLAssemblyResolver(Domain);
	if not Assigned(Resolver) then
		raise Exception.Create('Can not load assembly resolver.');
	Resolver.ResolveLoadedAssemblies := ResolveLoaded;
	Resolver.ResolveWithPartialName := ResolvePartialName;
end;

//------------------------------------------------------------------------------

var
	OldVarDispProc: TVarDispProc = nil;
	SaveInitProc: System.Pointer = nil;

procedure InitClrUtils();
begin
	if SaveInitProc <> nil then TProcedure(SaveInitProc);
	if MSCompatibleFPUWord then Set8087CW(MCW_EM);
end;

initialization
	{$IFDEF TRIAL}
	CheckSharewareStatus();
	{$ENDIF}

	OldVarDispProc 	:= VarDispProc;
	//VarDispProc 	:= @VarClrInvoke;

	SaveInitProc := InitProc;
	InitProc := @InitClrUtils;

finalization
	VarDispProc := @OldVarDispProc;

	StopCLR();

end.
