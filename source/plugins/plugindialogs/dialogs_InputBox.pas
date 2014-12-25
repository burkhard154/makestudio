(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dialogs_InputBox.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/04  JDuenow   - launched Dialogs Module

-----------------------------------------------------------------------------*)

unit dialogs_InputBox;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, dialogs_dlgInput,
  dialogs_Vars, Registry, Forms, SysUtils, dialogs_tools;

type
  TInputBoxCommand = class(TComponent, ICommand)
  private
    FCaption,
    FDCaption,
    FText,
    FDefault,
    FReturnValue: string;
    function RunBox: Boolean;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DCaption: string read FDCaption write FDCaption;
    property Text: string read FText write FText;
    property Default: string read FDefault write FDefault;
    property ReturnValue: string read FReturnValue write FReturnValue;
  end;

  //Callback to create an instance of the IJVCSCommand
  TInputBoxCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  InputBoxCommandCallback: TInputBoxCommandCallback;

const
  IDInputBoxCommand = 'dialogs.inputbox';

implementation

uses
  {$IFDEF DELPHI5}
  FileCtrl,
  {$ENDIF DELPHI5}
  ComServ, dialogs_EditInputBoxModule, dialogs_Actions;

function TInputBoxCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TInputBoxCommand.Create(nil));
end;

procedure TInputBoxCommandCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled;
end;

constructor TInputBoxCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdInputBoxCaption;
end;

destructor TInputBoxCommand.Destroy;
begin
  inherited Destroy;
end;

function TInputBoxCommand.EditItem: WordBool;
begin
  Result := DlgEditInputBoxModule(Self);
end;

function TInputBoxCommand.ExecuteItem: WordBool;
begin
  Result := False;
  if not Canceled then
    Result := RunBox;
end;

function TInputBoxCommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
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
    Result := Result + Canvas.TextHeight(stdCaption) + 2;
    Result := Result + Canvas.TextHeight(stdText) + 2;
    Result := Result + Canvas.TextHeight(stdDefault) + 2;
    if Length(FReturnValue) > 0 then
      Result := Result + Canvas.TextHeight(stdInputBoxReturnValue) + 2;
  finally
    Canvas.Free;
  end;
end;

function TInputBoxCommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
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

    if Selected then begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(aRect);
    end
    else begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(aRect);
    end;

    Offset := 2;
    SetCanvasTextColor(clWindowText);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent, aRect.Top + Offset, FCaption);

    SetCanvasTextColor(clBlue);
    Offset := Offset + Canvas.TextHeight(FCaption) + 2;
    Canvas.Font.Style := [];
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdCaption);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdCaption) + 8, aRect.Top + Offset, FDCaption);

    Offset := Offset + Canvas.TextHeight(stdCaption) + 2;
    Canvas.Font.Style := [];
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdText);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdText) + 8, aRect.Top + Offset, FText);

    Offset := Offset + Canvas.TextHeight(stdText) + 2;
    Canvas.Font.Style := [];
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdDefault);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdDefault) + 2, aRect.Top + Offset, FDefault);

    if Length(FReturnValue) > 0 then begin
      Offset := Offset + Canvas.TextHeight(stdDefault) + 2;
      Canvas.Font.Style := [];
      Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdInputBoxReturnValue);
      Canvas.Font.Style := [fsBold];
      Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdInputBoxReturnValue) -6, aRect.Top + Offset, FReturnValue);
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TInputBoxCommand.SetFilename(const Filename: WideString);
begin
  //ProjectPath := Filename;
end;

function TInputBoxCommand.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TInputBoxCommand.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TInputBoxCommand.RunBox: Boolean;
var
  return: string;
begin
  Result := True;

  MakeStudio.LogMessage(stdBreak);
  MakeStudio.LogMessage(stdStartingInputBox);
  MakeStudio.LogMessage('');
  MakeStudio.LogMessage(stdCaption + ' ' + FDCaption);
  MakeStudio.LogMessage(stdText + ' ' + FText);
  MakeStudio.LogMessage(stdDefault + ' ' + FDefault);
  if Length(FReturnValue) > 0 then
    MakeStudio.LogMessage(stdInputBoxReturnValue + ' ' + FReturnValue);


  return := DlgInputBox(FDCaption, MakeStudio.Variables.ReplaceVarsInString( FText),
              MakeStudio.Variables.ReplaceVarsInString( FDefault));
//  return := InputBox(FDCaption, MakeStudio.Variables.ReplaceVarsInString( FText),
//              MakeStudio.Variables.ReplaceVarsInString( FDefault));

  if Length(FReturnValue) > 0 then begin
    MakeStudio.LogMessage('');
    MakeStudio.LogMessage(Format(stdSettingVar, [FReturnValue, return]));
    if not MakeStudio.Variables.VarExists(FReturnValue) then
      MakeStudio.Variables.AddVar(FReturnValue);
    MakeStudio.Variables.Values[FReturnValue] := return;
  end;
end;

function TInputBoxCommand.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if ParamName = stdcCaption then
    Result := DCaption;
  if ParamName = stdcText then
    Result := Text;
  if ParamName = stdcDefault then
    Result := Default;
  if ParamName = stdcReturnValue then
    Result := ReturnValue;
end;

procedure TInputBoxCommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if ParamName = stdcCaption then
    DCaption := Value;
  if ParamName = stdcText then
     Text := Value;
  if ParamName = stdcDefault then
    Default := Value;
  if ParamName = stdcReturnValue then
    ReturnValue := Value;
end;

function TInputBoxCommand.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '???';
  case Index of
    0: Result := stdcCaption;
    1: Result := stdcText;
    2: Result := stdcDefault;
    3: Result := stdcReturnValue;
  end;
end;

function TInputBoxCommand.Get_ParamCount: Integer;
begin
  Result := 4;
end;

function TInputBoxCommandCallback.GetIdentifier: WideString;
begin
  Result := IDInputBoxCommand;
end;

end.
