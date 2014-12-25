{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: innosetup_InnoSetupProjectCommand.pas

The Initial Developer of the original code (JEDI Make) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2011/09/28  BSchranz  - new unit


------------------------------------------------------------------------------}
unit delphi32_brcc32Command;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils, JclSysInfo,
  ShellAPI, JclRegistry, JclFileUtils;

type
  TBrcc32Command = class(TComponent, ICommand2, IExecCallback)
  private
    FRCFile: string;
    FOutputFile: string;
    FOptions: string;
  protected
    function MeasureItem(Handle: Integer; BriefView: WordBool): Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
      Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString; const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;
    procedure CaptureOutput(const Line: WideString; var Aborted: WordBool); safecall;

    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Properties: IDispatch read Get_Properties;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  //Callback to create an instance of the ICommand
  TBrcc32CommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  Brcc32CommandCallback: TBrcc32CommandCallback;

const
  IDPluginInnoSetupProject = 'delphi32.brcc32';


implementation

uses
  ComServ, delphi32_Vars, delphi32_utils, delphi32_brcc32CommandEdit, Forms;

resourcestring
  strExecutingProject = 'Executing Resource Compiler...';
  strErrorCompilerNotFound = 'Resource Compiler not found!';
  strBRCC32File = 'Resource Compiler 32-bit <%s>';
  strPreviewText = 'Options: "%s"';



{ TPluginInnoSetupProjectCallback }

function TBrcc32CommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TBrcc32Command.Create(nil));
end;

procedure TBrcc32CommandCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
end;

function TBrcc32CommandCallback.GetIdentifier: WideString;
begin
  Result := IDPluginInnoSetupProject;
end;

{ TPluginInnoSetupProject }

constructor TBrcc32Command.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRCFile := '';
  FOutputFile := '';
  FOptions := '';
end;

function TBrcc32Command.EditItem: WordBool;
begin
  Result := False;
  with TFormEditbrcc32Command.Create(nil) do
  try
    edFilename.Text := FRCFile;
    edOptions.Text := FOptions;
    if ShowModal = mrOk then
    begin
      FRCFile := StringReplace( edFilename.Text, '"', '', [ rfReplaceAll, rfIgnoreCase]);
      FOptions := edOptions.Text;
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TBrcc32Command.ExecuteItem: WordBool;
var
  S, s1, F: string;
  CallbackIntf: IExecCallback;

  function GetRC:String;
  begin
    Result := SysUtils.FileSearch( 'rc.exe', GetEnvironmentVariable( 'PATH'));
    //Result := GetBRCC32Compiler;
  end;

begin
  Canceled := False;
  Result := False;
  MakeStudio.LogMessage('*********************************************************');
//  MakeStudio.LogMessage( 'Searchpath for rc.exe is:');
//  MakeStudio.LogMessage( GetEnvironmentVariable( 'PATH'));
  MakeStudio.LogMessage(strExecutingProject);
  MakeStudio.LogMessage(GetRC);
  MakeStudio.LogMessage(FRCFile);

  //build command line
  F := MakeStudio.Variables.ReplaceVarsInString(FRCFile);
  S := '"'+ F +'"';
  if FOptions <> '' then
    S := S + ' ' + MakeStudio.Variables.ReplaceVarsInString(FOptions);

  if FileExists( GetRC) then
  begin
    Self.GetInterface(IExecCallback, CallbackIntf);
    Result := MakeStudio.ExecCmdLine(GetRC + ' /v', S,
              ExtractFilePath(F), CallbackIntf) = 0;
  end
  else
    MakeStudio.LogMessage(strErrorCompilerNotFound);
end;

function TBrcc32Command.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1; //auto
end;

function TBrcc32Command.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := True; //auto
end;

procedure TBrcc32Command.SetFilename(const Filename: WideString);
begin
  FRCFile := Filename;
end;

function TBrcc32Command.Get_Caption: WideString;
begin
  Result := Format(strBRCC32File, [FRCFile]);
end;

procedure TBrcc32Command.Set_Caption(const Value: WideString);
begin
  //nothing
end;

function TBrcc32Command.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, stdcRCFile) then
    Result := FRCFile;
  if SameText(ParamName, stdcOptions) then
    Result := FOptions;
end;

procedure TBrcc32Command.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, stdcRCFile) then
    FRCFile := Value;
  if SameText(ParamName, stdcOptions) then
    FOptions := Value;
end;

function TBrcc32Command.Get_ParamNames(Index: Integer): WideString;
begin
  case Index of
    0: Result := stdcRCFile;
    1: Result := stdcOptions;
  end;
end;

function TBrcc32Command.Get_ParamCount: Integer;
begin
  Result := 3;
end;

function TBrcc32Command.Get_OwnerDraw: WordBool;
begin
  Result := False;
end;

function TBrcc32Command.Get_PreviewText: WideString;
begin
  Result := Format( strPreviewText, [ FOptions]);
end;

function TBrcc32Command.Notify(const Notification: WideString; Parameter: OleVariant): OleVariant;
begin
  //nothing to do
  //for future purpose - e.g. active language changed
  Result := 0;
end;

function TBrcc32Command.Get_Properties: IDispatch;
begin
  //nothing to do
  //for future purpose - integration of an property inspector
  //and extended handling of command parameters/properties
  Result := nil;
end;

procedure TBrcc32Command.CaptureOutput(const Line: WideString;
  var Aborted: WordBool);
begin
  Aborted := Canceled;
  MakeStudio.LogMessage(Line);
end;

end.
