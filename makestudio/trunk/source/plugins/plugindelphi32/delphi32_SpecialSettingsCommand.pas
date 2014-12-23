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
2006/04/19  BSchranz - Command implemented

------------------------------------------------------------------------------}
unit delphi32_SpecialSettingsCommand;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils, TypInfo,
  delphi32_Vars, delphi32_utils;

{**** Sample Code to register this command *******
var
 P: Picture;
      //--- add modules --------------------------------------------------------
      GetPictureFromImageList( <ImageList1>, 0, P);

      //Name=SpecialSettings; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before

      //Create and register Callback for the command type
      PluginSpecialSettingsCallback := TPluginSpecialSettingsCallback.Create(nil);
      jvcsmak.AddCommandType('SpecialSettings', '', stCategory, P, 'txt', -1,
        ICommandCallback(PluginSpecialSettingsCallback));
**** End Sample Code  *******}

type
  TPluginSpecialSettings = class(TComponent, ICommand2)
  private
    FCaption: string;
    FVariable: string;
    FOperation: TGetVarOperationType;
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

  published
    property Variable:String read FVariable write FVariable;
    property Operation:TGetVarOperationType read FOperation write FOperation;
  end;

  //Callback to create an instance of the ICommand
  TPluginSpecialSettingsCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginSpecialSettingsCallback: TPluginSpecialSettingsCallback;

const
  IDPluginSpecialSettings = 'delphi32.SpecialSettings';

implementation

uses
  ComServ, delphi32_SpecialSettingsEdit;

{ TPluginSpecialSettingsCallback }

function TPluginSpecialSettingsCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginSpecialSettings.Create(nil));
end;

procedure TPluginSpecialSettingsCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
end;

function TPluginSpecialSettingsCallback.GetIdentifier: WideString;
begin
  Result := IDPluginSpecialSettings;
end;

{ TPluginSpecialSettings }

constructor TPluginSpecialSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'Get Delphi32 into Variable';
  FVariable := '';
  FOperation := otBPLDir;
end;

function TPluginSpecialSettings.EditItem: WordBool;
begin
  Result := False;
  with TFormEditSpecialSettingsParams.Create(nil) do
  try
    cbOperation.ItemIndex := Ord( Operation);
    cbVars.Text := Variable;
    if ShowModal = mrOk then
    begin
      Variable := cbVars.Text;
      Operation := TGetVarOperationType( cbOperation.ItemIndex);
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TPluginSpecialSettings.ExecuteItem: WordBool;

  procedure SetVar( Varname:String; Value:String);
  begin
    if not jvcsmak.Variables.VarExists( Varname) then
      jvcsmak.Variables.AddVar( Varname);
    jvcsmak.Variables.Values[ Varname] := Value;
    jvcsmak.LogMessage( Varname + ' = "' + Value + '"');
  end;

begin
  Canceled := False;
  jvcsmak.LogMessage( stdBreak);
  jvcsmak.LogMessage( Get_Caption);

  if Variable<>'' then begin

    case Operation of
      otBPLDir : SetVar( Variable, GetDelphiBPLPath);
      otDCPDir : SetVar( Variable, GetDelphiDCPPath);
      otRootPath : SetVar( Variable, GetDelphiRootPathLong);
      otSearchPath : SetVar( Variable, GetDelphiSearchPath);
      otBDSPROJECTSDIR :
        begin
          SetVar( Variable, GetBDSProjectsPath);
        end;
    end;
  end
  else
    jvcsmak.LogMessage( Format( stdSetSpecialSettingsVarError, [Get_Caption]));

  jvcsmak.LogMessage( '');
  Result := True;
end;

function TPluginSpecialSettings.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1; //auto
end;

function TPluginSpecialSettings.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := True; //auto
end;

procedure TPluginSpecialSettings.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginSpecialSettings.Get_Caption: WideString;
begin
  Result := Format( stdSetSpecialSettingsVarCaption, [ Variable, stGetVarActions[ Operation]]);
end;

procedure TPluginSpecialSettings.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginSpecialSettings.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, 'Variable') then
    Result := Variable
  else if SameText(ParamName, 'Operation') then
    Result := GetEnumProp( self, 'Operation');
end;

procedure TPluginSpecialSettings.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, 'Variable') then
    Variable := Value
  else if SameText(ParamName, 'Operation') then
    SetEnumProp( self, 'Operation', Value);
end;

function TPluginSpecialSettings.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0: Result := 'Variable';
    1: Result := 'Operation';
  end;
end;

function TPluginSpecialSettings.Get_ParamCount: Integer;
begin
  Result := 2;
end;

function TPluginSpecialSettings.Get_OwnerDraw: WordBool;
begin
  //Use Caption and PreviewText!
  //Otherwise, if Result = true, you can use
  //DrawItem and MeasureItem
  Result := false;
end;

function TPluginSpecialSettings.Get_PreviewText: WideString;
begin
  Result := '';
end;

function TPluginSpecialSettings.Notify(const Notification: WideString; Parameter: OleVariant): OleVariant;
begin
  //nothing to do
  //for future purpose - e.g. active language changed
  Result := 0;
end;

function TPluginSpecialSettings.Get_Properties: IDispatch;
begin
  //nothing to do
  //for future purpose - integration of an property inspector
  //and extended handling of command parameters/properties
  Result := nil;
end;

end.
