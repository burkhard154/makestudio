(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dialogs_MsgBox.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/04  JDuenow   - launched Dialogs Module
2006/02/25  BSchranz - MessageBoxButtons Bug Fix


-----------------------------------------------------------------------------*)

unit dialogs_MsgBox;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, TypInfo,
  dialogs_Vars, Registry, Forms, SysUtils, dialogs_tools;

type
  TMsgBoxCommand = class(TComponent, ICommand)
  private
    FCaption,
    FText : String;
    FButtons :TMsgDlgButtons;
    FReturnValue: string;
    FStyle: Integer;
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

  published
    property Text: string read FText write FText;
    property Buttons: TMsgDlgButtons read FButtons write FButtons;
    property Style: Integer read FStyle write FStyle;
    property ReturnValue: string read FReturnValue write FReturnValue;
  end;

  //Callback to create an instance of the IJVCSCommand
  TMsgBoxCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  MsgBoxCommandCallback: TMsgBoxCommandCallback;

const
  IDMsgBoxCommand = 'dialogs.msgbox';

implementation

uses
  {$IFDEF DELPHI5}
  FileCtrl,
  {$ENDIF DELPHI5}
  ComServ, dialogs_EditMsgBoxModule, dialogs_Actions;

function TMsgBoxCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TMsgBoxCommand.Create(nil));
end;

procedure TMsgBoxCommandCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled;
end;

constructor TMsgBoxCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdMsgBoxCaption;
  FText := '';
  FButtons := [mbOk];
  FStyle := 0;
  FReturnValue := '';
end;

destructor TMsgBoxCommand.Destroy;
begin
  inherited Destroy;
end;

function TMsgBoxCommand.EditItem: WordBool;
begin
  Result := DlgEditMsgBoxModule(Self);
end;

function TMsgBoxCommand.ExecuteItem: WordBool;
begin
  Result := False;
  if not Canceled then
    Result := RunBox;
end;

function TMsgBoxCommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
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
    Result := Result + Canvas.TextHeight(stdText) + 2;
    Result := Result + Canvas.TextHeight(stdButtons) + 2;
    if Length(FReturnValue) > 0 then
      Result := Result + Canvas.TextHeight(stdMsgBoxReturnValue) + 2;
  finally
    Canvas.Free;
  end;
end;

function TMsgBoxCommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
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
    
    case FStyle of
      1: Canvas.Draw(aRect.Left +iDefaultIndent + Canvas.TextWidth(FCaption) + 3, aRect.Top + 1, Form3.Image1.Picture.Graphic);
      2: Canvas.Draw(aRect.Left +iDefaultIndent + Canvas.TextWidth(FCaption) + 3, aRect.Top + 1, Form3.Image2.Picture.Graphic);
      3: Canvas.Draw(aRect.Left +iDefaultIndent + Canvas.TextWidth(FCaption) + 3, aRect.Top + 1, Form3.Image3.Picture.Graphic);
      4: Canvas.Draw(aRect.Left +iDefaultIndent + Canvas.TextWidth(FCaption) + 3, aRect.Top + 1, Form3.Image4.Picture.Graphic);
    end;

    SetCanvasTextColor(clBlue);
    Offset := Offset + Canvas.TextHeight(FCaption) + 2;
    Canvas.Font.Style := [];
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdText);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdText) + 8, aRect.Top + Offset, FText);

    Offset := Offset + Canvas.TextHeight(stdText) + 2;
    Canvas.Font.Style := [];
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdButtons);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdButtons) + 2, aRect.Top + Offset, GetSetProp( self, 'Buttons', true));

    if Length(FReturnValue) > 0 then begin
      Offset := Offset + Canvas.TextHeight(stdButtons) + 2;
      Canvas.Font.Style := [];
      Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdMsgBoxReturnValue);
      Canvas.Font.Style := [fsBold];
      Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdMsgBoxReturnValue) -6, aRect.Top + Offset, FReturnValue);
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TMsgBoxCommand.SetFilename(const Filename: WideString);
begin
  //ProjectPath := Filename;
end;

function TMsgBoxCommand.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TMsgBoxCommand.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TMsgBoxCommand.RunBox: Boolean;
var
  s, return: string;
begin
  Result := True;

  s := StringReplace( FText, '|', #13#10, [rfReplaceAll]);
  jvcsmak.LogMessage(stdBreak);
  jvcsmak.LogMessage(stdStartingMsgBox);
  jvcsmak.LogMessage('');
  jvcsmak.LogMessage(stdType + ' ' + DialogNames[FStyle]);
  jvcsmak.LogMessage(stdText + ' ' + s);
  jvcsmak.LogMessage(stdButtons + ' ' + GetSetProp( self, 'Buttons', true));
  if Length(FReturnValue) > 0 then
    jvcsmak.LogMessage(stdMsgBoxReturnValue + ' ' + FReturnValue);

  return := ButtonReturnValues[
       MessageDlg(
       jvcsmak.Variables.ReplaceVarsInString( s),
       DialogTypes[FStyle], Buttons, 0) - 1];

  if Length(FReturnValue) > 0 then begin
    jvcsmak.LogMessage('');
    jvcsmak.LogMessage(Format(stdSettingVar, [FReturnValue, return]));
    if not jvcsmak.Variables.VarExists(FReturnValue) then
      jvcsmak.Variables.AddVar(FReturnValue);
    jvcsmak.Variables.Values[FReturnValue] := return;
  end;
end;

function TMsgBoxCommand.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if ParamName = stdcText then
    Result := Text;
  if ParamName = stdcButtons then begin
    Result := GetSetProp( self, 'Buttons', true);
  end;
  if ParamName = stdcStyle then
    Result := IntToStr(Style);
  if ParamName = stdcReturnValue then
    Result := ReturnValue;
end;

procedure TMsgBoxCommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if ParamName = stdcText then
    Text := Value;
  if ParamName = stdcButtons then
    SetSetProp( self, 'Buttons', Value);
  if ParamName = stdcStyle then
    Style := StrToInt(Value);
  if ParamName = stdcReturnValue then
    ReturnValue := Value;
end;

function TMsgBoxCommand.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '???';
  case Index of
    0: Result := stdcText;
    1: Result := stdcButtons;
    2: Result := stdcStyle;
    3: Result := stdcReturnValue;
  end;
end;

function TMsgBoxCommand.Get_ParamCount: Integer;
begin
  Result := 4;
end;

function TMsgBoxCommandCallback.GetIdentifier: WideString;
begin
  Result := IDMsgBoxCommand;
end;

end.
