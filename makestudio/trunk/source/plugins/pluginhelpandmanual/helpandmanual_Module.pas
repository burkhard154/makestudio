(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: helpandmanual_Module.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/01  JDuenow   - launched Help & Manual Module
2005/09/07  BSchranz  - translated to english, Batchfile changed to ExecCmdLine...
                        HM3 and HM4 supported

-----------------------------------------------------------------------------*)

unit helpandmanual_Module;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, makestudio_TLB,
  Classes, Windows, Dialogs, Controls,
  helpandmanual_Vars, Registry, Forms, SysUtils;

type
  THelpandmanualCommand = class(TComponent, ICommand, IExecCallback)
  private
    FCaption: string;
    FProjectPath: string;
    FOutputfile: string;
    FOutputOpt: integer;
    function RunBatchfile: Boolean;
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

    property ProjectPath: string read FProjectPath write FProjectPath;
    property Outputfile: string read FOutputfile write FOutputFile;
    property OutputOpt: integer read FOutputOpt write FOutputOpt;
  end;

  //Callback to create an instance of the IJVCSCommand
  THelpandmanualCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  HelpandmanualCommandCallback: THelpandmanualCommandCallback;

  HelpManualVersion : integer;

const
  IDHelpandmanualCommand = 'helpman.compile';

implementation

uses
  {$IFDEF DELPHI5}
  FileCtrl,
  {$ENDIF DELPHI5}
  ComServ, helpandmanual_EditHelpandmanualModule;

function THelpandmanualCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(THelpandmanualCommand.Create(nil));
end;

procedure THelpandmanualCommandCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled;
end;

constructor THelpandmanualCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdHelpandmanualCaption;
  FProjectPath := '';
  FOutputfile := '';
  FOutputOpt := 0;
  HelpManualVersion := 0; // Major Version of Help & Manual
end;

destructor THelpandmanualCommand.Destroy;
begin
  inherited Destroy;
end;

function THelpandmanualCommand.EditItem: WordBool;
begin
  Result := DlgEditHelpandmanualModule(Self);
end;

function THelpandmanualCommand.ExecuteItem: WordBool;
begin
  Result := False;
  Canceled := False;
  if not Canceled then
    Result := RunBatchfile;
end;

function THelpandmanualCommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
    Canvas.Font.Style := [];
    Result := Result + Canvas.TextHeight(stdProjectPath) + 2;
    Result := Result + Canvas.TextHeight(stdOutputfile) + 2;    
  finally
    Canvas.Free;
  end;
end;

function THelpandmanualCommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
  Canvas: TCanvas;
  aRect: TRect;
  Offset: Integer;

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
    Canvas.TextOut(aRect.Left +iDefaultIndent, aRect.Top + Offset, FCaption);
    Offset := Offset + Canvas.TextHeight(FCaption) + 2;
    Canvas.Font.Style := [];
    SetCanvasTextColor(clBlue);
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdProjectPath);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdProjectPath) + 2, aRect.Top + Offset, ProjectPath);
    Offset := Offset + Canvas.TextHeight(stdProjectPath) + 2;
    Canvas.Font.Style := [];
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdOutputfile);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdOutputfile) + 2, aRect.Top + Offset, Outputfile);
  finally
    Canvas.Free;
  end;
end;

procedure THelpandmanualCommand.SetFilename(const Filename: WideString);
begin
  ProjectPath := Filename;
end;

function THelpandmanualCommand.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure THelpandmanualCommand.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function THelpandmanualCommand.RunBatchfile: Boolean;
var F,O,P, appKey:String;

  function GetExecName: string;
  var
    reg: TRegistry;
  begin
    Result := '';
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CURRENT_USER;
      if SameText( ExtractFileExt( ProjectPath), '.hm3') then
      begin
        if reg.OpenKey('\Software\EC\Help&Manual3', false) then
        begin
          Result := reg.ReadString('AppPath');
          reg.CloseKey;
          HelpManualVersion := 3;
        end;
      end;
      if SameText( ExtractFileExt( ProjectPath), '.hm4') then
      begin
        if reg.OpenKey('Software\ECSOFTWARE\Help & Manual 4', false) then
        begin
          Result := reg.ReadString('AppPath');
          reg.CloseKey;
          HelpManualVersion := 4;
        end;
      end;
      if SameText( ExtractFileExt( ProjectPath), '.hmxz') then
      begin
        appKey := MakeStudio.ApplicationRegKey+'\plugins\help_manual';
        if reg.OpenKey(appKey, false) then
        begin
          Result := reg.ReadString('CompilerPath');
          reg.CloseKey;
          HelpManualVersion := 5;
        end;
      end;
    finally
      reg.Free;
    end;
  end;

  function GetOpt: string;
  begin
    Result := '';
    case OutputOpt of
      0: Result := '/hlp';
      1: Result := '/chm';
      2: Result := '/htm';
      3: Result := '/pdf';
      4: Result := '/rtf';
      5: Result := '/exe';
    end;
  end;

begin
  Result := True;

  MakeStudio.LogMessage(stdBreak);
  MakeStudio.LogMessage(stdStartingBatch);

  if not FileExists( GetExecName) then
  begin
    MakeStudio.LogMessage('> Error: ' + stdErrNoHelpandmanualExec + ' :-(');
    Result := False;
    Exit;
  end;

  F := MakeStudio.Variables.ReplaceVarsInString( ProjectPath);
  O := MakeStudio.Variables.ReplaceVarsInString( Outputfile);
  if FileExists( F) then begin
    if Length(O) > 0 then
      P := '"' + F + '" ' + GetOpt + '="' + O + '"'
    else
      P := '"' + F + '" ' + GetOpt;

    Result := MakeStudio.ExecCmdLine( GetExecName, P, ExtractFilePath( F), IExecCallback( self))=0;
  end
  else
    MakeStudio.LogMessage( Format( stdeFileNotFound, [F]));
end;

function THelpandmanualCommand.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if ParamName = stdcProjectPath then
    Result := ProjectPath;
  if ParamName = stdcOutputfile then
    Result := Outputfile;
  if ParamName = stdcOutputOpt then
    Result := IntToStr(OutputOpt);
end;

procedure THelpandmanualCommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if ParamName = stdcProjectPath then
    ProjectPath := Value;
  if ParamName = stdcOutputfile then
    Outputfile := Value;
  if ParamName = stdcOutputOpt then
    OutputOpt := StrToInt(Value);
end;

function THelpandmanualCommand.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '???';
  case Index of
    0: Result := stdcProjectPath;
    1: Result := stdcOutputFile;
    2: Result := stdcOutputOpt;
  end;
end;

function THelpandmanualCommand.Get_ParamCount: Integer;
begin
  Result := 3;
end;

function THelpandmanualCommandCallback.GetIdentifier: WideString;
begin
  Result := IDHelpandmanualCommand;
end;

procedure THelpandmanualCommand.CaptureOutput(const Line: WideString;
  var Aborted: WordBool);
begin
  Aborted := Canceled;
  MakeStudio.LogMessage(Line);
end;

end.
