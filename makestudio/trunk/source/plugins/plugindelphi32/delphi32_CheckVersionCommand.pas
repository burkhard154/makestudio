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
unit delphi32_CheckVersionCommand;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils, delphi32_Vars, delphi32_Utils;

{**** Sample Code to register this command *******
var
 P: Picture;
      //--- add modules --------------------------------------------------------
      GetPictureFromImageList( <ImageList1>, 0, P);

      //Name=Delphi32.CheckVersion; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before

      //Create and register Callback for the command type
      PluginDelphi32CheckVersionCallback := TPluginDelphi32CheckVersionCallback.Create(nil);
      MakeStudio.AddCommandType('Delphi32.CheckVersion', '', stCategory, P, 'txt', -1,
        ICommandCallback(PluginDelphi32CheckVersionCallback));
**** End Sample Code  *******}

type
  TPluginDelphi32CheckVersion = class(TComponent, ICommand2)
  private
    FCaption: string;
    FVersion: TDelphiVersion;
    FVariable: string;
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
    property Version:TDelphiVersion read FVersion write FVersion;
    property Variable: string read FVariable write FVariable;
  end;

  //Callback to create an instance of the ICommand
  TPluginDelphi32CheckVersionCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginDelphi32CheckVersionCallback: TPluginDelphi32CheckVersionCallback;

const
  IDPluginDelphi32CheckVersion = 'delphi32.CheckVersion';

implementation

uses
  ComServ, delphi32_CheckVersionEdit;

{ TPluginDelphi32CheckVersionCallback }

function TPluginDelphi32CheckVersionCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginDelphi32CheckVersion.Create(nil));
end;

procedure TPluginDelphi32CheckVersionCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
end;

function TPluginDelphi32CheckVersionCallback.GetIdentifier: WideString;
begin
  Result := IDPluginDelphi32CheckVersion;
end;

{ TPluginDelphi32CheckVersion }

constructor TPluginDelphi32CheckVersion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdCheckVersionCaption;
  Variable := 'DelphiVersion';
  Version := dver5;
end;

function TPluginDelphi32CheckVersion.EditItem: WordBool;
begin
  Result := DlgCheckDelphiVersion(Self);
end;

{$IFNDEF D5TODO}
{$IFDEF DELPHI5}
function BoolToStr(ABoolean: Boolean; UseBoolStrs: Boolean = False): string;
begin
  if not UseBoolStrs then
  begin
    if ABoolean then
      Result := '1'
    else
      Result := '0';
  end
  else
  begin
    if ABoolean then
      Result := 'True'
    else
      Result := 'False';
  end;
end;
{$ENDIF DELPHI5}
{$ENDIF ~D5TODO}

function TPluginDelphi32CheckVersion.ExecuteItem: WordBool;
begin
  Canceled := False;
  MakeStudio.LogMessage(stdBreak);
  MakeStudio.LogMessage(Format(stdCheckingVersion, [GetVersionTextEx(Version)]));
  if not MakeStudio.Variables.VarExists(Variable) then
    MakeStudio.Variables.AddVar(Variable);
  MakeStudio.Variables.Values[Variable] := CheckDelphiVersion(Version);

  MakeStudio.LogMessage(Format(stdVersionInstalled, [GetVersionTextEx(Version), BoolToStr(CheckDelphiVersion(Version), True)]));

  MakeStudio.LogMessage(stdBreak);
  Result := True;
end;

function TPluginDelphi32CheckVersion.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1; //auto
end;

function TPluginDelphi32CheckVersion.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := True; //auto
end;

procedure TPluginDelphi32CheckVersion.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginDelphi32CheckVersion.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TPluginDelphi32CheckVersion.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginDelphi32CheckVersion.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, 'Version') then
    Result := IntToStr(Ord(Version));
  if SameText(ParamName, 'Variable') then
    Result := Variable;
end;

procedure TPluginDelphi32CheckVersion.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, 'Version') then
    Version := TDelphiVersion(StrToInt(Value));
  if SameText(ParamName, 'Variable') then
    Variable := Value;
end;

function TPluginDelphi32CheckVersion.Get_ParamNames(Index: Integer): WideString;
begin
  case Index of
    0:Result := 'Version';
    1:Result := 'Variable';
  end;
end;

function TPluginDelphi32CheckVersion.Get_ParamCount: Integer;
begin
  Result := 2;
end;

function TPluginDelphi32CheckVersion.Get_OwnerDraw: WordBool;
begin
  //Use Caption and PreviewText!
  //Otherwise, if Result = true, you can use
  //DrawItem and MeasureItem
  Result := False;
end;

function TPluginDelphi32CheckVersion.Get_PreviewText: WideString;
begin
  Result := Format(stdCheckVersionPreview, [GetVersionTextEx(Version)]);
end;

function TPluginDelphi32CheckVersion.Notify(const Notification: WideString; Parameter: OleVariant): OleVariant;
begin
  //nothing to do
  //for future purpose - e.g. active language changed
  Result := 0;
end;

function TPluginDelphi32CheckVersion.Get_Properties: IDispatch;
begin
  //nothing to do
  //for future purpose - integration of an property inspector
  //and extended handling of command parameters/properties
  Result := nil;
end;

end.
