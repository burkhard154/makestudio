(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: passolo_Module.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/19  JDuenow   - launched Passolo Module
2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit passolo_Module;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls,
  passolo_Vars, Registry, Forms, SysUtils;

type
  TPassoloCommand = class(TComponent, ICommand, IExecCallback)
  private
    FCaption: string;
    FProjectPath: string;
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
  end;

  //Callback to create an instance of the IJVCSCommand
  TPassoloCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PassoloCommandCallback: TPassoloCommandCallback;

const
  IDPassoloCommand = 'passolo.translate';

implementation

uses
  {$IFDEF DELPHI5}
  FileCtrl,
  {$ENDIF DELPHI5}
  ComServ, passolo_EditPassoloModule;

function TPassoloCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TPassoloCommand.Create(nil));
end;

procedure TPassoloCommandCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled;
end;

constructor TPassoloCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdPassoloCaption;
  FProjectPath := '';
end;

destructor TPassoloCommand.Destroy;
begin
  inherited Destroy;
end;

function TPassoloCommand.EditItem: WordBool;
begin
  Result := DlgEditPassoloModule(Self);
end;

function TPassoloCommand.ExecuteItem: WordBool;
begin
  Result := False;
  Canceled := false;
  if not Canceled then
    Result := RunBatchfile;
end;

function TPassoloCommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
    if BriefView then  begin
      Canvas.Font.Style := [];
      Result := Result + Canvas.TextHeight(stdProjectPath) + 2;
    end;
  finally
    Canvas.Free;
  end;
end;

function TPassoloCommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
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
    if BriefView then  begin
      Canvas.Font.Style := [];
      SetCanvasTextColor(clBlue);
      Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdProjectPath);
      Canvas.Font.Style := [fsBold];
      Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdProjectPath) + 2, aRect.Top + Offset, ProjectPath);
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TPassoloCommand.SetFilename(const Filename: WideString);
begin
  ProjectPath := Filename;
end;

function TPassoloCommand.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TPassoloCommand.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPassoloCommand.RunBatchfile: Boolean;
var
  sl, sl1: TStringList;
  R, I: Integer;
  PassoloExec, Path: string;

  procedure Wait_a_While(ms: Cardinal);
  var
    st: Cardinal;
  begin
    st := GetTickCount;
    while GetTickCount - st < ms do
      Application.ProcessMessages;
  end;

  procedure LastErrorMsg;
  var
   ch: array [0..511] of Char;
  begin
    FormatMessage(
      FORMAT_MESSAGE_FROM_SYSTEM,
      nil,
      GetLastError,
      LANG_NEUTRAL, // Default language
      ch,
      511,
      nil);
    jvcsmak.LogMessage(StrPas(ch));
  end;

  function GetExecName(TLB: string): string;
  begin
    Result := '';
    with TRegistry.Create do
    begin
      try
        RootKey := HKEY_CLASSES_ROOT;
        OpenKey('TypeLib\' + TLB + '\1.0\0\win32', False);
        Result := ReadString('');
      finally
        Free;
      end;
    end;
  end;

var F:String;
begin
  Result := False;

  jvcsmak.LogMessage(stdBreak);
  jvcsmak.LogMessage(stdStartingBatch);

  if not FileExists( GetExecName(stPassolo5_PSLU_TLB)) then
  begin
    jvcsmak.LogMessage('> Error: ' + stdErrNoPassoloExec + ' :-(');
    Result := False;
    Exit;
  end;

  F := jvcsmak.Variables.ReplaceVarsInString( ProjectPath);
  if FileExists( F) then begin
    Result := jvcsmak.ExecCmdLine( GetExecName(stPassolo5_PSLU_TLB),
               '"' + ProjectPath + '" /BATCH', ExtractFilePath( F),
               IExecCallback( self))=0;
  end
  else
    jvcsmak.LogMessage( Format( stdeFileNotFound, [F]));
end;

function TPassoloCommand.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if ParamName = stdcProjectPath then
    Result := ProjectPath;
end;

procedure TPassoloCommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if ParamName = stdcProjectPath then
    ProjectPath := Value;
end;

function TPassoloCommand.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '???';
  case Index of
    0: Result := stdcProjectPath;
  end;
end;

function TPassoloCommand.Get_ParamCount: Integer;
begin
  Result := 1;
end;

function TPassoloCommandCallback.GetIdentifier: WideString;
begin
  Result := IDPassoloCommand;
end;

procedure TPassoloCommand.CaptureOutput(const Line: WideString;
  var Aborted: WordBool);
begin
  Aborted := Canceled;
  jvcsmak.LogMessage(Line);
end;

end.
