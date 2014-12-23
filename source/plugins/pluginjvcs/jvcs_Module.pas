(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSDataClass.pas

The Initial Devoloper of the original DMAK-Code is:
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
2003/11/28  USchuster - 2nd Migrationstep (fixed header)
2003/12/05  USchuster - re-formatted
2005/01/05  BSchranz  - Migration to plugin code
2005/02/04  USchuster - preparations for check in
2005/08/11  BSchranz  - Migration from jvcscore.dll to jvcs.exe
2005/08/18  BSchranz  - Indentity list from jvcs registry added
2006/04/29  BSchranz  - jvcs Labels added

-----------------------------------------------------------------------------*)

unit jvcs_Module;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, Contnrs,
  jvcs_Vars, Registry, Forms, SysUtils, JclSysInfo, JclFileUtils,
  JclSysUtils, jvcs_Utils, JclRegistry, TypInfo;

type
  TJVCSSyncOperation = ( jvcsSyncLatest, jvcsSyncLabel, jvcsSetLabel);

  TJVCSSyncCommand = class(TComponent, ICommand, IExecCallback)
  private
    FUsername: string;
    FPassword: string;
    FPort: Integer;
    FServer: string;
    FProjects: TStringList;
    FLabel: String;
    FOperation: TJVCSSyncOperation;

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

    //IExecCallback
    procedure CaptureOutput(const Line: WideString; var Aborted: WordBool); safecall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Caption:WideString read Get_Caption;

  published
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
    property Server: string read FServer write FServer;
    property Projects: TStringList read FProjects write FProjects;
    property Operation:TJVCSSyncOperation read FOperation write FOperation;
    property JLabel:String read FLabel write FLabel;
  end;

  //Callback to create an instance of the IJVCSCommand
  TJVCSSyncCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  JVCSSyncCommandCallback: TJVCSSyncCommandCallback;

const
  IDJVCSSyncCommand = 'jvcs.sync';

implementation

uses
  ComServ, jvcs_EditJVCSSyncModule;


function TJVCSSyncCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TJVCSSyncCommand.Create(nil));
end;

procedure TJVCSSyncCommandCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled;
end;

constructor TJVCSSyncCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Projects := TStringList.Create;
  Operation := jvcsSyncLatest;
  JLabel := '';

  GetLastUsedIdentityEx( FUsername, FServer, FPassword, FPort);
end;

destructor TJVCSSyncCommand.Destroy;
begin
  Projects.Free;
  inherited Destroy;
end;

function TJVCSSyncCommand.EditItem: WordBool;
begin
  Result := DlgEditJVCSSyncModule(Self);
end;

function TJVCSSyncCommand.ExecuteItem: WordBool;
var Helper:TJvcsHelper;
    i:Integer;
begin
  Result := True;

  Helper := TJvcsHelper.Create;
  try
    if not Canceled then
    begin

      Jvcsmak.LogMessage( Get_Caption);

      for I := 0 to Projects.Count - 1 do
      begin
        case Operation of
          jvcsSyncLatest:
             Result := Helper.SyncProject( Username, Server,
                         Password, Projects[i], Port, '');
          jvcsSyncLabel:
             Result := Helper.SyncProject( Username, Server,
                         Password, Projects[i], Port, jvcsmak.Variables.ReplaceVarsInString( JLabel));
          jvcsSetLabel:
             Result := Helper.LabelProject( Username, Server,
                         Password, Projects[i], Port, jvcsmak.Variables.ReplaceVarsInString( JLabel));
        end;

        if Canceled or (not Result) then
        begin
          Result := False;
          Break;
        end;
      end;

    end
    else
      Result := False;
    for i:=0 to Projects.Count-1 do
      if not Canceled then
  finally
    Helper.Free;
  end;

end;

function TJVCSSyncCommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(Caption) + 2;
    Canvas.Font.Style := [];
    Result := Result + Canvas.TextHeight(stdSyncTxt) + 2;

    if Projects.Count > 0 then
    begin
      Result := Result + Canvas.TextHeight(stdProjectsTxt) + 2;
      if Projects.Count mod 10 = 0 then
        Result := Result + (Canvas.TextHeight(Projects[0]) + 2) * ((Projects.Count div 10))
      else
        Result := Result + (Canvas.TextHeight(Projects[0]) + 2) *
          ((Projects.Count div 10) + 1);
    end;
  finally
    Canvas.Free;
  end;
end;

function TJVCSSyncCommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
  Canvas: TCanvas;
  aRect: TRect;
  I, Offset: Integer;
  S: string;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False;

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    aRect := Rect(Left, Top, Right, Bottom);
    if Selected then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(aRect);
    end
    else
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(aRect);
    end;

    Offset := 2;
    SetCanvasTextColor(clWindowText);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent, aRect.Top + Offset, Caption);
    Offset := Offset + Canvas.TextHeight(Caption) + 2;
    Canvas.Font.Style := [];
    SetCanvasTextColor(clBlue);
    Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset,
                   Format(stdSyncTxt, [Server, Port, Username]));
    Offset := Offset + Canvas.TextHeight(stdSyncTxt) + 2;

    SetCanvasTextColor(clFuchsia);
    if Projects.Count > 0 then
    begin
      Canvas.Font.Style := [fsBold];
      Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset, stdProjectsTxt);
      Offset := Offset + Canvas.TextHeight(stdProjectsTxt) + 2;
    end;
    S := '';
    Canvas.Font.Style := [];
    for I := 0 to Projects.Count - 1 do
    begin
      S := S + Projects[I];
      if I < Projects.Count - 1 then
        S := S + ',';
      if (I + 1) mod 10 = 0 then
      begin
        Canvas.TextOut(aRect.Left + iDefaultIndent + 16, aRect.Top + Offset, S);
        Offset := Offset + Canvas.TextHeight(S) + 2;
        S := '';
      end;
    end;
    if S <> '' then
    begin
      Canvas.TextOut(aRect.Left + iDefaultIndent + 16, aRect.Top + Offset, S);
      Offset := Offset + Canvas.TextHeight(S) + 2;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TJVCSSyncCommand.SetFilename(const Filename: WideString);
begin
end;

function TJVCSSyncCommand.Get_Caption: WideString;
begin
  Result := stdJVCSSyncCaption;
  case Operation of
    jvcsSyncLatest: Result := stdJVCSSyncCaption;
    jvcsSyncLabel: Result := Format( stdJVCSSyncLabelCaption, [JLabel]);
    jvcsSetLabel: Result := Format( stdJVCSLabelCaption, [JLabel]);
  end;
end;

procedure TJVCSSyncCommand.Set_Caption(const Value: WideString);
begin
  //
end;

function TJVCSSyncCommand.Get_ParamValues(const ParamName: WideString): WideString;
var
  I: Integer;
begin
  if ParamName = stdcUsername then
    Result := Username
  else
  if ParamName = stdcPassword then
    Result := ''//Password
  else
  if ParamName = stdcPasswordEncoded then
    Result := Base64Encode( Password)
  else
  if ParamName = stdcPort then
    Result := IntToStr( Port)
  else
  if ParamName = stdcServer then
    Result := Server
  else
  if ParamName = stdcOperation then
    Result := GetEnumProp( self, 'Operation')
  else
  if ParamName = stdcLabel then
    Result := JLabel
  else
  if ParamName = stdcProjectCount then
  begin
    Result := IntToStr(Projects.Count);
  end
  else
  begin
    for I := 0 to Projects.Count - 1 do
      if SameText(Format(stdcProjects, [I + 1]), ParamName) then
      begin
        Result := Projects[I];
        Break;
      end;
  end;
end;

procedure TJVCSSyncCommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var
  I: Integer;
begin
  if ParamName = stdcUsername then
    Username := Value
  else
  if ParamName = stdcPassword then
    Password := Value
  else
  if ParamName = stdcPasswordEncoded then
    Password := Base64Decode( Value)
  else
  if ParamName = stdcPort then
    Port := StrToInt( Value)
  else
  if ParamName = stdcServer then
    Server := Value
  else
  if ParamName = stdcOperation then
    SetEnumProp( self, 'Operation', Value)
  else
  if ParamName = stdcLabel then
    JLabel := Value
  else
  if ParamName = stdcProjectCount then
  begin
    Projects.Clear;
    for I := 0 to StrToInt(Value) - 1 do
      Projects.Add('');
  end
  else
  begin
    for I := 0 to Projects.Count - 1 do
      if SameText(Format(stdcProjects, [I + 1]), ParamName) then
      begin
        Projects[I] := Value;
        Break;
      end;
  end;
end;

function TJVCSSyncCommand.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '???';
  case Index of
    0: Result := stdcUsername;
    1: Result := stdcPassword;
    2: Result := stdcPort;
    3: Result := stdcServer;
    4: Result := stdcProjectCount;
    5: Result := stdcPasswordEncoded;
    6: Result := stdcOperation;
    7: Result := stdcLabel;
    else
    begin
      Result := Format(stdcProjects, [Index - 7]);
    end;
  end;
end;

function TJVCSSyncCommand.Get_ParamCount: Integer;
begin
  Result := 8 + Projects.Count;
end;

function TJVCSSyncCommandCallback.GetIdentifier: WideString;
begin
  Result := IDJVCSSyncCommand;
end;

procedure TJVCSSyncCommand.CaptureOutput(const Line: WideString;
  var Aborted: WordBool);
begin
  Aborted := Canceled;
  jvcsmak.LogMessage(Line);
end;

end.
