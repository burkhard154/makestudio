(*------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsplugintemplate_Module.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/08  BSchranz  - Plugin template created
2005/02/15  USchuster - preparations for check in and modified for Wizard

------------------------------------------------------------------------------*)
unit delphi_SelectDelphiPlatformCommand;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils,
  delphi32_Vars, delphi_EditSelectDelphiPlatform, delphi32_utils;

type
  TPluginSelectDelphiPlatform = class(TComponent, ICommand2)
  private
    FCaption: string;
    FPlatform: TCompilerPlatform;
  protected
    //ICommand Interface
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
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;

    //ICommand2 Interface
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Properties: IDispatch read Get_Properties;

  public
    constructor Create(AOwner: TComponent); override;
    function PlatformToStr( _p:TCompilerPlatform):String;
  end;

  //Callback to create an instance of the ICommand
  TPluginSelectDelphiPlatformCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginSelectDelphiPlatformCallback: TPluginSelectDelphiPlatformCallback;

const
  IDPluginSelectDelphiPlatform = 'delphi.SelectDelphiPlatform';

{
  Example code to register the command.
  To be used in the "RegisterPlugin" funktion of the project file.

      //--- add then command: SelectDelphiPlatform
	  // 1. Get the image from an image list
      GetPictureFromImageList(FormActions.ImageList1, 0, P);

	  // 2. Create the global command callback
      PluginSelectDelphiPlatformCallback := TPluginSelectDelphiPlatformCallback.Create(nil);

	  // 3. Register the command itsel
      //Name=SelectDelphiPlatform; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      jvcsmak.AddCommandType('SelectDelphiPlatform', 'Your Hint here!', stCategory, P, 'txt', -1,
        ICommandCallback(PluginSelectDelphiPlatformCallback));

}
implementation

uses
  ComServ;

resourcestring
  StrSelectDelphiPlatfom = 'Select Delphi Platform';
  StrFATALCannotSetTh = 'FATAL: Cannot set the requested platform - wrong De' +
  'lphi Version?';
  StrCurrentDelphiVersi = 'Current Delphi Version=';
  StrSelectedDelphiPlat = 'Delphi Platform selected: ';

function TPluginSelectDelphiPlatformCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginSelectDelphiPlatform.Create(nil));
end;

procedure TPluginSelectDelphiPlatformCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
end;

constructor TPluginSelectDelphiPlatform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := StrSelectDelphiPlatfom;
  FPlatform := dpWin32;
end;

function TPluginSelectDelphiPlatform.EditItem: WordBool;
begin
  Result := False;
  with TFormEditSelectDelphiPlatform.Create(nil) do
  try
    case FPlatform of
      dpOSX32: cbPlatform.ItemIndex := 2;
      dpWin32: cbPlatform.ItemIndex := 0;
      dpWin64: cbPlatform.ItemIndex := 1;
      dpiOSDevice: cbPlatform.ItemIndex := 3;
      dpiOSSimulator: cbPlatform.ItemIndex := 4;
      dpAndroid32: cbPlatform.ItemIndex := 5;
    end;
    if ShowModal = mrOk then
    begin
      case cbPlatform.ItemIndex of
        0: FPlatform := dpWin32;
        1: FPlatform := dpWin64;
        2: FPlatform := dpOSX32;
        3: FPlatform := dpiOSDevice;
        4: FPlatform := dpiOSSimulator;
        5: FPlatform := dpAndroid32;
      end;
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TPluginSelectDelphiPlatform.ExecuteItem: WordBool;
begin
  Canceled := False;
  SetCompilerPlatform( FPlatform);
  jvcsmak.LogMessage( StrSelectedDelphiPlat + PlatformToStr( GetCompilerPlatform));
  if FPlatform<>GetCompilerPlatform then begin
    jvcsmak.LogMessage( StrFATALCannotSetTh);
    jvcsmak.LogMessage( StrCurrentDelphiVersi+GetVersionText);
  end;

  jvcsmak.LogMessage('');
  Result := True;
end;

function TPluginSelectDelphiPlatform.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1; //auto
end;

function TPluginSelectDelphiPlatform.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := varEmpty;
end;

function TPluginSelectDelphiPlatform.PlatformToStr(
  _p: TCompilerPlatform): String;
begin
  case _p of
    dpOSX32:
      Result := stdPlatformOSX32;
    dpWin32:
      Result := stdPlatformWIN32;
    dpWin64:
      Result := stdPlatformWIN64;
    dpiOSDevice:
      Result := stdPlatformIOSDevice;
    dpiOSSimulator:
      Result := stdPlatformIOSSimulator;
    dpAndroid32:
      Result := stdPlatformAndroid32;
  end;
end;

function TPluginSelectDelphiPlatform.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := True; //auto
end;

procedure TPluginSelectDelphiPlatform.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginSelectDelphiPlatform.Get_Caption: WideString;
begin
  Result := FCaption;
end;

function TPluginSelectDelphiPlatform.Get_OwnerDraw: WordBool;
begin
  Result := false;
end;

procedure TPluginSelectDelphiPlatform.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginSelectDelphiPlatform.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, 'Platform') then
    Result := IntToStr( Ord( FPlatform));
end;

function TPluginSelectDelphiPlatform.Get_PreviewText: WideString;
begin
  Result := PlatformToStr( FPlatform);
end;

function TPluginSelectDelphiPlatform.Get_Properties: IDispatch;
begin
  Result := nil;
end;

procedure TPluginSelectDelphiPlatform.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, 'Platform') then
    FPlatform := TCompilerPlatform( StrToInt( Value));
end;

function TPluginSelectDelphiPlatform.Get_ParamNames(Index: Integer): WideString;
begin
  Result := 'Platform';
end;

function TPluginSelectDelphiPlatform.Get_ParamCount: Integer;
begin
  Result := 1;
end;

function TPluginSelectDelphiPlatformCallback.GetIdentifier: WideString;
begin
  Result := IDPluginSelectDelphiPlatform;
end;

end.
