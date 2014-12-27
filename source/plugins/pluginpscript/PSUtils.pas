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
2006/05/01  BSchranz  - PSTypInfo and PSUtils added for browser functionallity
2005/05/27  BSchranz  - Released

------------------------------------------------------------------------------}
unit PSUtils;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, uPSCompiler, uPSComponent,
  uPSUtils, PSTypInfo, uPSRuntime, Dialogs;

type
  TPSHandler = Class( TPersistent)
  private
    FPSScript: TPSScript;
    FBrowserInfo: TPSTypInfoList;
    function GetScriptText: TStrings;
    procedure SetScriptText(const Value: TStrings);
    function GetBrowserInfo: TPSTypInfoList;

  public
    Constructor Create;
    Destructor Destroy; override;

    function Compile:Boolean;
    function Execute:Boolean;
    procedure Stop;

    procedure OnExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure OnCompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure OnExecute(Sender: TPSScript);
    procedure OnCompile(Sender: TPSScript);
    procedure CompileGetBrowserInfo(Sender: TPSScript);

    function Readln( aQuery:String):string;
    procedure Writeln( S:String);

    property ScriptText:TStrings read GetScriptText write SetScriptText;
    property BrowserInfo:TPSTypInfoList read GetBrowserInfo;
    property PSScript:TPSScript read FPSScript;
  end;

implementation

uses
  pscript_Vars,
  psscript_published,
  uPSR_std,
  uPSC_std,
  uPSR_stdctrls,
  uPSC_stdctrls,
  uPSR_forms,
  uPSC_forms,
  uPSC_graphics,
  uPSC_controls,
  uPSC_classes,
  uPSR_graphics,
  uPSR_controls,
  uPSR_classes,
  uPSR_dateutils,
  uPSC_dateutils;


{ compile-time registration functions }
procedure SIRegister_psscript_internals(CL: TPSPascalCompiler);
begin
end;

{ run-time registration functions }
procedure RIRegister_psscript_internals(S: TPSExec);
begin
end;

{ TPSHandler }

function TPSHandler.GetScriptText: TStrings;
begin
  Result := FPSScript.Script;
end;

procedure TPSHandler.SetScriptText(const Value: TStrings);
begin
  FPSScript.Script.Assign( Value);
end;

function TPSHandler.Compile: Boolean;
var i:Integer;
begin
  Result := FPSScript.Compile;

  if not Result then
    for i := 0 to FPSScript.CompilerMessageCount -1 do
      MakeStudio.LogMessage(FPSScript.CompilerMessages[i].MessageToString)
  else
    MakeStudio.LogMessage(StrSuccesfullyCompiled);

end;

constructor TPSHandler.Create;
begin
  inherited;

  FPSScript := TPSScript.Create(nil);
  FPSScript.OnCompImport := OnCompImport;
  FPSScript.OnExecImport := OnExecImport;
  FPSScript.OnExecute := OnExecute;
  FPSScript.OnCompile := OnCompile;

  FBrowserInfo := TPSTypInfoList.Create;
end;

procedure TPSHandler.OnExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
  RIRegister_psscript_internals( se);
  Register_psscript_published( nil, se);
  RegisterDateTimeLibrary_R(se);
end;

procedure TPSHandler.OnCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, true);
  SIRegister_Graphics(x, true);
  SIRegister_Controls(x);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
  SIRegister_psscript_internals( x);
  Register_psscript_published( x, nil);
  RegisterDateTimeLibrary_C(x);
end;

procedure TPSHandler.OnExecute(Sender: TPSScript);
begin
end;

procedure TPSHandler.OnCompile(Sender: TPSScript);
begin
end;

destructor TPSHandler.Destroy;
begin
  FBrowserInfo.Clear;
  FBrowserInfo.Free;
  FPSScript.Free;
  inherited;
end;

function TPSHandler.Execute: Boolean;
begin
  Result := false;
  if Compile then begin
    Result := FPSScript.Execute;
    if not Result then
      MakeStudio.LogMessage( FPSScript.ExecErrorToString);
  end;
end;

function TPSHandler.Readln( aQuery:String): string;
begin
  Result := InputBox( aQuery, '', '');
end;

procedure TPSHandler.Writeln(S: String);
begin
  MakeStudio.LogMessage( S);
end;

function TPSHandler.GetBrowserInfo: TPSTypInfoList;
begin
  if FBrowserInfo.Count=0 then begin
    //first Retrieve Browser Information
    FPSScript.OnCompile := CompileGetBrowserInfo;
    FPSScript.Compile;
    FPSScript.OnCompile := OnCompile;
  end;
  Result := FBrowserInfo;
end;

procedure TPSHandler.CompileGetBrowserInfo(Sender: TPSScript);
var TI : TPSTypInfo;
begin
  TI := TPSTypInfo.Create;
  try
    TI.Compiler := Sender.Comp;
    TI.GetAllReferences( FBrowserInfo);
  finally
    TI.Free;
  end;
end;

procedure TPSHandler.Stop;
begin
  if FPSScript.Running then
    FPSScript.Stop;
end;

end.
