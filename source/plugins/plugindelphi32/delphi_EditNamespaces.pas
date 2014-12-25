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
unit delphi_EditNamespaces;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils;

type
  TPluginEditNamespaces = class(TComponent, ICommand2)
  private
    FCaption: string;
    FNamespaces: string;
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
  end;

  //Callback to create an instance of the ICommand
  TPluginEditNamespacesCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginEditNamespacesCallback: TPluginEditNamespacesCallback;

const
  IDPluginEditNamespaces = 'delphi32.EditNamespaces';

{
  Example code to register the command. 
  To be used in the "RegisterPlugin" funktion of the project file.
  
      //--- add then command: EditNamespaces
	  // 1. Get the image from an image list
      GetPictureFromImageList(FormActions.ImageList1, 0, P);
	  
	  // 2. Create the global command callback
      PluginEditNamespacesCallback := TPluginEditNamespacesCallback.Create(nil);

	  // 3. Register the command itsel
      //Name=EditNamespaces; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      MakeStudio.AddCommandType('EditNamespaces', 'Your Hint here!', stCategory, P, 'txt', -1,
        ICommandCallback(PluginEditNamespacesCallback));
  
}  
implementation

uses
  ComServ, delphi32_Vars, delphi_EditEditNamespaces;

function TPluginEditNamespacesCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginEditNamespaces.Create(nil));
end;

procedure TPluginEditNamespacesCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
end;

constructor TPluginEditNamespaces.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'Preset Delphi Namespaces';
  FNamespaces := stNamespaces;
end;

function TPluginEditNamespaces.EditItem: WordBool;
begin
  Result := False;
  with TFormEditEditNamespaces.Create(nil) do
  try
    edNamespaces.Text := FNamespaces;
    if ShowModal = mrOk then
    begin
      FNamespaces := edNamespaces.Text;
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TPluginEditNamespaces.ExecuteItem: WordBool;
begin
  Canceled := False;
  MakeStudio.Variables.Values[stvarNamespaces] := FNamespaces;
  MakeStudio.LogMessage('Setting Namespaces to: '+MakeStudio.Variables.Values[stvarNamespaces]);
  Result := True;
end;

function TPluginEditNamespaces.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1; //auto
end;

function TPluginEditNamespaces.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := True; //auto
end;

procedure TPluginEditNamespaces.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginEditNamespaces.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TPluginEditNamespaces.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginEditNamespaces.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, 'Namespaces') then
    Result := FNamespaces;
end;

procedure TPluginEditNamespaces.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, 'Namespaces') then
    FNamespaces := Value;
end;

function TPluginEditNamespaces.Get_ParamNames(Index: Integer): WideString;
begin
  Result := 'Namespaces';
end;

function TPluginEditNamespaces.Get_ParamCount: Integer;
begin
  Result := 1;
end;

function TPluginEditNamespaces.Get_OwnerDraw: WordBool;
begin
  Result := false;
end;

function TPluginEditNamespaces.Get_PreviewText: WideString;
begin
  Result := 'Namespaces: '+FNamespaces;
end;

function TPluginEditNamespaces.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := varEmpty;
end;

function TPluginEditNamespaces.Get_Properties: IDispatch;
begin
  Result := nil;
end;


function TPluginEditNamespacesCallback.GetIdentifier: WideString;
begin
  Result := IDPluginEditNamespaces;
end;





end.
