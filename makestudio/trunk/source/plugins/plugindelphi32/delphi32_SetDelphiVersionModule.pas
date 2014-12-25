(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: delphi32_SetDelphiVersionModule.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in
2005/02/24  BSchranz - Fixed TSetDVersionCommandCallback.GetIdentifier

-----------------------------------------------------------------------------*)

unit delphi32_SetDelphiVersionModule;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB, delphi32_Vars,
  Classes, Windows, Dialogs, Controls, SysUtils, Forms;

type
  TSetDVersionModule = class(TComponent, ICommand)
  private
    FCaption: string;
    FVersion: TDelphiVersion;
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
  end;

  //Callback to create an instance of the IJVCSModule
  TSetDVersionCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginSetDVersionCallback: TSetDVersionCommandCallback;

const
  IDSetDVersionCommand = 'Delphi32.SetDelphiVersion';

implementation

uses
  ComServ, delphi32_SelectDelphiVersion, delphi32_Utils;

function TSetDVersionCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TSetDVersionModule.Create(nil));
end;

procedure TSetDVersionCommandCallback.SetCanceled(ACanceled: WordBool);
begin
  //
end;

constructor TSetDVersionModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdSetDVersionCaption;
  FVersion := GetDelphiVersion;
end;

destructor TSetDVersionModule.Destroy;
begin
  inherited Destroy;
end;

function TSetDVersionModule.EditItem: WordBool;
begin
  Result := DlgSelectDelphiVersion(FVersion);
end;

function TSetDVersionModule.ExecuteItem: WordBool;
begin
  Result := True;
  if DelphiVersionInstalled then
  begin
    SetDelphiVersion(FVersion);
    MakeStudio.LogMessage(stdverSet + GetVersionText);
  end
  else
  begin
    MakeStudio.LogMessage(Format(stdverSetErr, [GetVersionText]));
    MakeStudio.LogMessage(stdverSet + GetVersionText);
  end;
end;

function TSetDVersionModule.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TSetDVersionModule.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False; //ownerdraw

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

    Canvas.Font.Style := [fsBold];
    SetCanvasTextColor(clRed);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, FCaption + ' - ' +
      GetVersionTextEx(FVersion));
  finally
    Canvas.Free;
  end;
end;

procedure TSetDVersionModule.SetFilename(const Filename: WideString);
begin
end;

function TSetDVersionModule.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TSetDVersionModule.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TSetDVersionModule.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if ParamName = stdcDelphiversion then
    Result := IntToStr(Ord(FVersion));
end;

procedure TSetDVersionModule.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if ParamName = stdcDelphiversion then
    FVersion := TDelphiVersion(StrToInt(Value));
end;

function TSetDVersionModule.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0: Result := stdcDelphiversion;
  end;
end;

function TSetDVersionModule.Get_ParamCount: Integer;
begin
  Result := 1;
end;

function TSetDVersionCommandCallback.GetIdentifier: WideString;
begin
  Result := IDSetDVersionCommand;
end;

end.
