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

2005/01/08  BSchranz  - Plugin template created
2005/02/15  USchuster - preparations for check in and modified for Wizard

------------------------------------------------------------------------------}
unit testplugin01Testcommand2Command;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils;

{**** Sample Code to register this command *******
var
 P: Picture;
      //--- add modules --------------------------------------------------------
      GetPictureFromImageList( <ImageList1>, 0, P);

      //Name=Testcommand2; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before

      //Create and register Callback for the command type
      PluginTestcommand2Callback := TPluginTestcommand2Callback.Create(nil);
      jvcsmak.AddCommandType('Testcommand2', '', stCategory, P, 'txt', -1,
        ICommandCallback(PluginTestcommand2Callback));
**** End Sample Code  *******}

type
  TPluginTestcommand2 = class(TComponent, ICommand2)
  private
    FCaption: string;
    FTestValue: string;
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
  TPluginTestcommand2Callback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginTestcommand2Callback: TPluginTestcommand2Callback;

const
  IDPluginTestcommand2 = 'testplugin01JMakPlugin.Testcommand2';

implementation

uses
  ComServ, testplugin01Vars, testplugin01JMakPluginTestcommand2Edit;

{ TPluginTestcommand2Callback }

function TPluginTestcommand2Callback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginTestcommand2.Create(nil));
end;

procedure TPluginTestcommand2Callback.SetCanceled(aCanceled: WordBool);
begin
  FCanceled := True; //set by the server if the user press "Cancel" oder "Stop"
end;

function TPluginTestcommand2Callback.GetIdentifier: WideString;
begin
  Result := IDPluginTestcommand2;
end;

{ TPluginTestcommand2 }

constructor TPluginTestcommand2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'Testcommand2';
  FTestValue := 'TestValue';
end;

function TPluginTestcommand2.EditItem: WordBool;
begin
  Result := False;
  with TFormEditTestcommand2Params.Create(nil) do
  try
    Edit1.Text := FTestValue;
    if ShowModal = mrOk then
    begin
      FTestValue := Edit1.Text;
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TPluginTestcommand2.ExecuteItem: WordBool;
begin
  FCanceled := False;
  jvcsmak.LogMessage(FCaption + ' ' + FTestValue);
  jvcsmak.LogMessage('Executing Testcommand2...');
  Result := True;
end;

function TPluginTestcommand2.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  //----------------------------- Example ------------------------
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 2;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
    if not BriefView then
    begin
      Canvas.Font.Style := [];
      Result := Result + Canvas.TextHeight(FCaption) + 2;
    end;
  finally
    Canvas.Free;
  end;
end;

function TPluginTestcommand2.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
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
  //----------------------------- Example ------------------------
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
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, FCaption + ' ' + FTestValue);
    if not BriefView then
    begin
      Offset := Canvas.TextHeight(FCaption) + 2;
      Canvas.Font.Style := [];
      Canvas.Font.Color := clBlue;
      Canvas.TextOut(aRect.Left + 10, aRect.Top + Offset, 'only for testing');
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TPluginTestcommand2.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginTestcommand2.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TPluginTestcommand2.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginTestcommand2.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, 'TestEntry') then
    Result := FTestValue;
end;

procedure TPluginTestcommand2.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, 'TestEntry') then
    FTestValue := Value;
end;

function TPluginTestcommand2.Get_ParamNames(Index: Integer): WideString;
begin
  Result := 'TestEntry';
end;

function TPluginTestcommand2.Get_ParamCount: Integer;
begin
  Result := 1;
end;

function TPluginTestcommand2.Get_OwnerDraw: WordBool;
begin
  //Use Caption and PreviewText!
  //Otherwise, if Result = true, you can use
  //DrawItem and MeasureItem
  Result := false;
end;

function TPluginTestcommand2.Get_PreviewText: WideString;
begin
  Result := FTestValue;
end;

function TPluginTestcommand2.Notify(const Notification: WideString; Parameter: OleVariant): OleVariant;
begin
  //nothing to do
  //for future purpose - e.g. active language changed
  Result := 0;
end;

function TPluginTestcommand2.Get_Properties: IDispatch;
begin
  //nothing to do
  //for future purpose - integration of an property inspector
  //and extended handling of command parameters/properties
  Result := nil;
end;

end.
