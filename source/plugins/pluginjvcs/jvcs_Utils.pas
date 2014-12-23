(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCS_utils.pas

The Initial Devoloper of the original DMAK-Code is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
Code move to JEDI VCS:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Original Code comes from JclSysUtils.pas...

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/08/11  BSchranz  - Migration from jvcscore.dll to jvcs.exe
2005/08/18  BSchranz  - Identities from jvcs added
2006/04/29  BSchranz  - jvcs Labels added


-----------------------------------------------------------------------------*)

unit jvcs_Utils;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, TypInfo, SysUtils, JclBase, JclSysUtils, JclAnsiStrings, JclRegistry,
  Contnrs, jvcs_Vars, JclFileUtils, JclSysInfo, Forms, Dialogs, JclMime;


type
  TJVCSIdentity = class(TObject)
    Identity,
    User,
    Hostname,
    Password : String;
    Port : Integer;
  end;

  TJVCSHelper = class( TPersistent)
  private
    SList : TStringList;

    procedure OnCaptureLineInternal(const Text: string);
    procedure OnCaptureLineExternal(const Text: string);
    function ExecCmdLineInternal(const App: WideString; const Args: WideString;
                                Callback:TTextHandler; chr0:Char):Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetProjectList( User, Host, Password:String; Port:Integer; sl:TStringList):Boolean;
    function GetModuleList( User, Host, Password, Project:String; Port:Integer; sl:TStringList):Boolean;

    function SyncProject( User, Host, Password, Project:String; Port:Integer; aLabel:String):Boolean;
    function LabelProject(User, Host, Password, Project:String; Port:Integer; aLabel:String): Boolean;
    function ProceedInputFile( User, Host, Password:String; Port: Integer; aFilename:String):Boolean;

  end;

//Work around because jvcs.exe returns #0 within an output line
function jvcsExecute(const CommandLine: string; var Output: string; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; CHR0Replace:Char=#0): Cardinal; overload;
function jvcsExecute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; CHR0Replace:Char=#0): Cardinal; overload;

//identities
function GetLastUsedIdentity:Integer;
procedure SetLastUsedIdentity( Value:Integer);
procedure GetLastUsedIdentityEx( var User, Hostname, Password:String; var Port:Integer);
procedure SetLastUsedIdentityEx( User, Hostname, Password:String; Port:Integer);
function FindIdentity( User, Hostname, Password:String; Port:Integer):Integer;
function GetIdentityCount:Integer;
function GetIdentity( Index:Integer):TJVCSIdentity;
procedure ReadIdentities; //to be called on register

//jvcs.exe
function GetJVCSExe:String;
procedure SetJVCSExe( Value:String);

//Base64
function Base64Decode( Value:String):String;
function Base64Encode( Value:String):String;

implementation

var
  FIdentityList : TObjectList = nil;


function Base64Decode( Value:String):String;
begin
  Result := MimeDecodeString( Value);
end;

function Base64Encode( Value:String):String;
begin
  Result := MimeEncodeString( Value);
end;


function GetLastUsedIdentity:Integer;
begin
  //-1 means user defined or no identities in the registry
  Result := RegReadIntegerDef( HKCU, stdcRegKey + stdcJVCSIdentity, 'LastUsedID', -2);
  if Result = -2 then //not found - get from JVCS RegKey
    Result := RegReadIntegerDef( HKCU, stdcJVCSRegKey + stdcJVCSIdentity, 'LastUsedID', -2);
end;

procedure SetLastUsedIdentity( Value:Integer);
begin
  RegWriteInteger( HKCU, stdcRegKey + stdcJVCSIdentity, 'LastUsedID', Value);
end;

procedure GetLastUsedIdentityEx( var User, Hostname, Password:String; var Port:Integer);
begin
  if GetLastUsedIdentity = -1 then begin
    //read from jvcsmak key
    Hostname := RegReadStringDef( HKCU, stdcRegKey + stdcJVCSIdentity, 'HostName', '');
    Port := RegReadIntegerDef( HKCU, stdcRegKey + stdcJVCSIdentity, 'Port', 2106);
    User := RegReadStringDef( HKCU, stdcRegKey + stdcJVCSIdentity, 'User', '');
    Password := RegReadStringDef( HKCU, stdcRegKey + stdcJVCSIdentity, 'Password', '');
    if Password <> '' then
      Password := Base64Decode( Password);
  end
  else begin
    User := GetIdentity( GetLastUsedIdentity).User;
    Password := GetIdentity( GetLastUsedIdentity).Password;
    Port := GetIdentity( GetLastUsedIdentity).Port;
    Hostname := GetIdentity( GetLastUsedIdentity).Hostname;
  end;
end;

function FindIdentity( User, Hostname, Password:String; Port:Integer):Integer;
var i:Integer;
begin
  Result := -1;

  for i:=0 to GetIdentityCount-1 do
    if (User=GetIdentity(i).User) and
       (Hostname=GetIdentity(i).Hostname) and
       (Password=GetIdentity(i).Password) and
       (Port=GetIdentity(i).Port) then begin
      Result := i;
      Break;
    end;
end;

procedure SetLastUsedIdentityEx( User, Hostname, Password:String; Port:Integer);
begin
  //store the user defines settings
  SetLastUsedIdentity( -1);
  RegWriteString( HKCU, stdcRegKey + stdcJVCSIdentity, 'HostName', Hostname);
  RegWriteInteger( HKCU, stdcRegKey + stdcJVCSIdentity, 'Port', Port);
  RegWriteString( HKCU, stdcRegKey + stdcJVCSIdentity, 'User', User);
  RegWriteString( HKCU, stdcRegKey + stdcJVCSIdentity, 'Password', Base64Encode( Password));
end;

function GetIdentityCount:Integer;
begin
  if FIdentityList<>nil then
    Result := FIdentityList.Count
  else
    Result := 0;
end;

function GetIdentity( Index:Integer):TJVCSIdentity;
begin
  if FIdentityList<>nil then
    Result := TJVCSIdentity( FIdentityList.Items[ Index])
  else
    Result := nil;
end;

procedure ReadIdentities;
var
  S: string;
  I: Integer;
  id : TJVCSIdentity;
begin
  if FIdentityList = nil then
    FIdentityList := TObjectList.Create;

  S := RegReadStringDef( HKCU, stdcJVCSRegKey + stdcJVCSIdentity, 'ID0', '');
  I := 1;
  if S <> '' then
  begin
    while S <> '' do
    begin
      id := TJVCSIdentity.Create;
      id.Identity := S;

      //Reading Identity
      id.Hostname := RegReadStringDef( HKCU, stdcJVCSRegKey + stdcJVCSIdentity + '\' + S, 'HostName', '');
      id.Port := RegReadIntegerDef( HKCU, stdcJVCSRegKey + stdcJVCSIdentity + '\' + S, 'Port', 2106);
      id.User := RegReadStringDef( HKCU, stdcJVCSRegKey + stdcJVCSIdentity + '\' + S, 'User', '');
      id.Password := RegReadStringDef( HKCU, stdcJVCSRegKey + stdcJVCSIdentity + '\' + S, 'Password', '');
      if id.Password <> '' then
        id.Password := Base64Decode( id.Password);

      FIdentityList.Add( id);

      //Read next
      S := RegReadStringDef( HKCU, stdcJVCSRegKey + stdcJVCSIdentity, 'ID' + IntToStr(I), '');
      Inc(I);
    end;
  end;
end;

function GetJvcsExe: String;
begin
  Result := RegReadStringDef( HKCU, stdcRegKey, stdcRegJvcsExe, '');
  if Result = '' then  //not in Registry - use default
    Result := PathAddSeparator( GetProgramFilesFolder) + stdcPathJVCS + stdcRegJvcsExe;
end;

procedure SetJvcsExe( Value: String);
begin
  RegWriteString( HKCU, stdcRegKey, stdcRegJvcsExe, Value);
end;


// MuteCRTerminatedLines was "outsourced" from Win32ExecAndRedirectOutput

function MuteCRTerminatedLines(const RawOutput: string): string;
const
  Delta = 1024;
var
  BufPos, OutPos, LfPos, EndPos: Integer;
  C: Char;
begin
  SetLength(Result, Length(RawOutput));
  OutPos := 1;
  LfPos := OutPos;
  EndPos := OutPos;
  for BufPos := 1 to Length(RawOutput) do
  begin
    if OutPos >= Length(Result)-2 then
      SetLength(Result, Length(Result) + Delta);
    C := RawOutput[BufPos];
    case C of
      AnsiCarriageReturn:
        OutPos := LfPos;
      AnsiLineFeed:
        begin
          OutPos := EndPos;
          Result[OutPos] := AnsiCarriageReturn;
          Inc(OutPos);
          Result[OutPos] := C;
          Inc(OutPos);
          EndPos := OutPos;
          LfPos := OutPos;
        end;
    else
      Result[OutPos] := C;
      Inc(OutPos);
      EndPos := OutPos;
    end;
  end;
  SetLength(Result, OutPos - 1);
end;

function InternalExecute(const CommandLine: string; var Output: string; OutputLineCallback: TTextHandler;
  RawOutput: Boolean; AbortPtr: PBoolean; CHR0Replace:Char): Cardinal;
const
  BufferSize = 255;
var
  Buffer: array [0..BufferSize] of Char;
  TempOutput: string;
  PipeBytesRead: Cardinal;

  procedure ProcessLine(LineEnd: Integer);
  begin
    if RawOutput or (TempOutput[LineEnd] <> AnsiCarriageReturn) then
    begin
      while (LineEnd > 0) and (TempOutput[LineEnd] in [AnsiLineFeed, AnsiCarriageReturn]) do
        Dec(LineEnd);
      OutputLineCallback(Copy(TempOutput, 1, LineEnd));
    end;
  end;

  procedure ProcessBuffer;
  var
    CR, LF, i: Integer;
  begin
    //BSchranz - replace all #0
    if CHR0Replace<>#0 then
      for i:=0 to PipeBytesRead do
        if Buffer[i]=#0 then
          Buffer[i] := CHR0Replace;

    Buffer[PipeBytesRead] := #0;
    TempOutput := TempOutput + Buffer;
    if Assigned(OutputLineCallback) then
    repeat
      CR := Pos(AnsiCarriageReturn, TempOutput);
      if CR = Length(TempOutput) then
        CR := 0;        // line feed at CR + 1 might be missing
      LF := Pos(AnsiLineFeed, TempOutput);
      if (CR > 0) and ((LF > CR + 1) or (LF = 0)) then
        LF := CR;       // accept CR as line end
      if LF > 0 then
      begin
        ProcessLine(LF);
        Delete(TempOutput, 1, LF);
      end;
    until LF = 0;
  end;

{$IFDEF MSWINDOWS}
// "outsourced" from Win32ExecAndRedirectOutput
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  PipeRead, PipeWrite: THandle;
begin
  Result := $FFFFFFFF;
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor := nil;
  SecurityAttr.bInheritHandle := True;
  if not CreatePipe(PipeRead, PipeWrite, @SecurityAttr, 0) then
  begin
    Result := GetLastError;
    Exit;
  end;
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput := PipeWrite;
  StartupInfo.hStdError := PipeWrite;
  if CreateProcess(nil, PChar(CommandLine), nil, nil, True, NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
  begin
    CloseHandle(PipeWrite);
    if AbortPtr <> nil then
      AbortPtr^ := False;
    while ((AbortPtr = nil) or not AbortPtr^) and ReadFile(PipeRead, Buffer, BufferSize, PipeBytesRead, nil) and (PipeBytesRead > 0) do
      ProcessBuffer;
    if (AbortPtr <> nil) and AbortPtr^ then
      TerminateProcess(ProcessInfo.hProcess, Cardinal(ABORT_EXIT_CODE));
    if (WaitForSingleObject(ProcessInfo.hProcess, INFINITE) = WAIT_OBJECT_0) and
      not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
        Result := $FFFFFFFF;
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    CloseHandle(PipeWrite);
  CloseHandle(PipeRead);
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  Pipe: PIOFile;
  Cmd: string;
begin
  Cmd := Format('%s 2>&1', [CommandLine]);
  Pipe := Libc.popen(PChar(Cmd), 'r');
  { TODO : handle Abort }
  repeat
    PipeBytesRead := fread_unlocked(@Buffer, 1, BufferSize, Pipe);
    if PipeBytesRead > 0 then
      ProcessBuffer;
  until PipeBytesRead = 0;
  Result := pclose(Pipe);
  wait(nil);
{$ENDIF UNIX}
  if TempOutput <> '' then
    if Assigned(OutputLineCallback) then
      // output wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      ProcessLine(Length(TempOutput))
    else
      if RawOutput then
        Output := Output + TempOutput
      else
        Output := Output + MuteCRTerminatedLines(TempOutput);
end;

{ TODO -cHelp :
RawOutput: Do not process isolated carriage returns (#13).
That is, for RawOutput = False, lines not terminated by a line feed (#10) are deleted from Output. }

function jvcsExecute(const CommandLine: string; var Output: string; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; CHR0Replace:Char=#0): Cardinal;
begin
  Result := InternalExecute(CommandLine, Output, nil, RawOutput, AbortPtr, CHR0Replace);
end;

{ TODO -cHelp :
Author: Robert Rossmair
OutputLineCallback called once per line of output. }

function jvcsExecute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; CHR0Replace:Char=#0): Cardinal; overload;
var
  Dummy: string;
begin
  Result := InternalExecute(CommandLine, Dummy, OutputLineCallback, RawOutput, AbortPtr, CHR0Replace);
end;


{ TJVCSHelper }

function TJVCSHelper.ExecCmdLineInternal(const App, Args: WideString;
  Callback: TTextHandler; chr0: Char): Integer;
begin
  jvcsmak.LogMessage( App + ' ' + Args);
  Result := jvcsExecute(App + ' ' + Args, Callback, true, @Canceled, chr0);
end;

constructor TJVCSHelper.Create;
begin
  inherited;
  SList := TStringList.Create;
end;

procedure TJVCSHelper.OnCaptureLineInternal(const Text: string);
var
  sl: TStringList;
  S: string;
  I: Integer;
begin
  if (Pos(#10, Text) > 0) or (Pos(#13, Text) > 0) then
  begin
    S := StringReplace(Text, #10#13, #3, [rfReplaceAll]);
    S := StringReplace(S, #13#10, #3, [rfReplaceAll]);
    S := StringReplace(S, #13, #3, [rfReplaceAll]);
    S := StringReplace(S, #10, #3, [rfReplaceAll]);
    S := StringReplace(S, #3, #10, [rfReplaceAll]);

    sl := TStringList.Create;
    try
      sl.Text := S;
      for I := 0 to sl.Count - 1 do
        SList.Add( sl[i]);
    finally
      sl.Free;
    end;
  end
  else
    SList.Add(Text);
end;

function TJVCSHelper.GetProjectList(User, Host, Password: String; Port: Integer;
  sl: TStringList): Boolean;

  function LineValid( Line:String):Boolean;
  begin
    //only lines starting with a number and not deleted projects (*)
    Result := (Length( Line)>0) and (Line[1] in ['1'..'9']) and (Pos( '*', Line)=0);
  end;

  function GetProjectString( Line:String):String;
  begin
    Result := TrimRight( Copy( Line, 10, 30));
  end;

  function GetDescriptionString( Line:String):String;
  begin
    Result := TrimRight( Copy( Line, 92, 50));
  end;

var r, i : Integer;
begin
  SList.Clear;
  Result := false;

  r := ExecCmdLineInternal('"'+GetJVCSExe+'"',
       Format( stdcGetProjectListArgs, [ Host, Port, User, Password]),
       OnCaptureLineInternal, #9);

  if r=0 then begin //parse Outmut in SList

    //Detect if login was successfull
    if Pos( stdcUserAccepted, SList.Text)<>0 then begin
      sl.Clear;
      for i:=0 to SList.Count-1 do
        if LineValid( SList[i]) then
          sl.Add( GetProjectString( SList[i]) + '=' + GetDescriptionString( SList[i]));
      Result := true;
    end
    else
      MessageDlg( stdNotLoggedIn, mtError, [mbOk], 0);
  end
  else
    MessageDlg( Format( stdJVCSErrorJVCSEXE, [SList.Text]), mtError, [mbOk], 0);
end;

function TJVCSHelper.SyncProject(User, Host, Password, Project: String;
  Port: Integer; aLabel:String): Boolean;
begin
  Result := False;

  if Canceled then
    Exit;

  if aLabel='' then
    Result := ExecCmdLineInternal('"'+GetJVCSExe+'"',
         Format( stdcSyncProjectArgs, [ Host, Port, User, Password, Project]),
                        OnCaptureLineExternal, #32)=0
  else
    Result := ExecCmdLineInternal('"'+GetJVCSExe+'"',
         Format( stdcSyncLabelProjectArgs, [ Host, Port, User, Password, Project, aLabel]),
                        OnCaptureLineExternal, #32)=0;

end;

function TJVCSHelper.LabelProject(User, Host, Password, Project: String;
  Port: Integer; aLabel:String): Boolean;
begin
  Result := False;

  if Canceled then
    Exit;

  Result := ExecCmdLineInternal('"'+GetJVCSExe+'"',
       Format( stdcLabelProjectArgs, [ Host, Port, User, Password, Project, aLabel]),
                      OnCaptureLineExternal, #32)=0;

end;

function TJVCSHelper.GetModuleList(User, Host, Password, Project: String;
  Port: Integer; sl: TStringList): Boolean;

  function LineValid( Line:String):Boolean;
  begin
    //only lines starting with a number and not deleted projects (*)
    Result := (Length( Line)>0) and (Line[1] in ['1'..'9']);
  end;

  function GetIDString( Line:String):String;
  begin
    Result := TrimRight( Copy( Line, 1, 9));
  end;

  function GetFilepathString( Line:String):String;
  begin
    Result := TrimRight( Copy( Line, 10, 50));
  end;

  function GetModuleString( Line:String):String;
  begin
    Result := TrimRight( Copy( Line, 61, 30));
  end;

  function GetVersionString( Line:String):String;
  begin
    Result := TrimRight( Copy( Line, 92, 7));
  end;

var r, i : Integer;
begin
  SList.Clear;
  Result := false;
  r := ExecCmdLineInternal('"'+GetJVCSExe+'"',
       Format( stdcGetModuleListArgs, [ Host, Port, User, Password, Project]),
       OnCaptureLineInternal, #9);

  if r=0 then begin //parse Output in SList

    //Detect if login was successfull
    if Pos( stdcUserAccepted, SList.Text)<>0 then begin
      sl.Clear;
      for i:=0 to SList.Count-1 do
        if LineValid( SList[i]) then
          sl.Add( GetModuleString( SList[i]) + ';' + GetFilepathString( SList[i])
                  + ';' + GetVersionString( SList[i])  + ';' + GetIDString( SList[i]));
      Result := true;
    end
    else
      MessageDlg( stdNotLoggedIn, mtError, [mbOk], 0);
  end
  else
    MessageDlg( Format( stdJVCSErrorJVCSEXE, [SList.Text]), mtError, [mbOk], 0);
end;

destructor TJVCSHelper.Destroy;
begin
  SList.Free;
  inherited;
end;

procedure TJVCSHelper.OnCaptureLineExternal(const Text: string);
var
  sl: TStringList;
  S: string;
  I: Integer;
begin
  if (Pos(#10, Text) > 0) or (Pos(#13, Text) > 0) then
  begin
    S := StringReplace(Text, #10#13, #3, [rfReplaceAll]);
    S := StringReplace(S, #13#10, #3, [rfReplaceAll]);
    S := StringReplace(S, #13, #3, [rfReplaceAll]);
    S := StringReplace(S, #10, #3, [rfReplaceAll]);
    S := StringReplace(S, #3, #10, [rfReplaceAll]);

    sl := TStringList.Create;
    try
      sl.Text := S;
      for I := 0 to sl.Count - 1 do
        jvcsmak.LogMessage (sl[i]);
    finally
      sl.Free;
    end;
  end
  else
    jvcsmak.LogMessage(Text);
end;

function TJVCSHelper.ProceedInputFile( User, Host, Password:String; Port: Integer; aFilename:String):Boolean;
begin
  Result := False;

  if Canceled then
    Exit;

  Result := ExecCmdLineInternal('"'+GetJVCSExe+'"',
       Format( stdcProceedInputFileArgs, [ Host, Port, User, Password, aFilename]),
                      OnCaptureLineExternal, #32)=0;
end;

initialization
finalization
  if FIdentityList<>nil then
    FIdentityList.Free;
end.
