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
unit testplugin01JMakPluginCreateMirrorDirectoryCommand;

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

      //Name=Create Mirror Directory; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before

      //Create and register Callback for the command type
      PluginCreateMirrorDirectoryCallback := TPluginCreateMirrorDirectoryCallback.Create(nil);
      MakeStudio.AddCommandType('Create Mirror Directory', '', stCategory, P, 'txt', -1,
        ICommandCallback(PluginCreateMirrorDirectoryCallback));
**** End Sample Code  *******}

type
  TPluginCreateMirrorDirectory = class(TComponent, ICommand2)
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
  TPluginCreateMirrorDirectoryCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginCreateMirrorDirectoryCallback: TPluginCreateMirrorDirectoryCallback;

const
  IDPluginCreateMirrorDirectory = 'testplugin01JMakPlugin.CreateMirrorDirectory';

implementation

uses
  ComServ, testplugin01Vars, testplugin01JMakPluginCreateMirrorDirectoryEdit;

{ TPluginCreateMirrorDirectoryCallback }

function TPluginCreateMirrorDirectoryCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginCreateMirrorDirectory.Create(nil));
end;

procedure TPluginCreateMirrorDirectoryCallback.SetCanceled(aCanceled: WordBool);
begin
  FCanceled := True; //set by the server if the user press "Cancel" oder "Stop"
end;

function TPluginCreateMirrorDirectoryCallback.GetIdentifier: WideString;
begin
  Result := IDPluginCreateMirrorDirectory;
end;

{ TPluginCreateMirrorDirectory }

constructor TPluginCreateMirrorDirectory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'Create Mirror Directory';
  FTestValue := 'TestValue';
end;

function TPluginCreateMirrorDirectory.EditItem: WordBool;
begin
  Result := False;
  with TFormEditCreateMirrorDirectoryParams.Create(nil) do
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

function TPluginCreateMirrorDirectory.ExecuteItem: WordBool;
begin
  FCanceled := False;
  MakeStudio.LogMessage(FCaption + ' ' + FTestValue);
  MakeStudio.LogMessage('Executing Create Mirror Directory...');
  Result := True;
end;

function TPluginCreateMirrorDirectory.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1; //auto
end;

function TPluginCreateMirrorDirectory.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := True; //auto
end;

procedure TPluginCreateMirrorDirectory.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginCreateMirrorDirectory.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TPluginCreateMirrorDirectory.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginCreateMirrorDirectory.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, 'TestEntry') then
    Result := FTestValue;
end;

procedure TPluginCreateMirrorDirectory.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, 'TestEntry') then
    FTestValue := Value;
end;

function TPluginCreateMirrorDirectory.Get_ParamNames(Index: Integer): WideString;
begin
  Result := 'TestEntry';
end;

function TPluginCreateMirrorDirectory.Get_ParamCount: Integer;
begin
  Result := 1;
end;

function TPluginCreateMirrorDirectory.Get_OwnerDraw: WordBool;
begin
  //Use Caption and PreviewText!
  //Otherwise, if Result = true, you can use
  //DrawItem and MeasureItem
  Result := false;
end;

function TPluginCreateMirrorDirectory.Get_PreviewText: WideString;
begin
  Result := FTestValue;
end;

function TPluginCreateMirrorDirectory.Notify(const Notification: WideString; Parameter: OleVariant): OleVariant;
begin
  //nothing to do
  //for future purpose - e.g. active language changed
  Result := 0;
end;

function TPluginCreateMirrorDirectory.Get_Properties: IDispatch;
begin
  //nothing to do
  //for future purpose - integration of an property inspector
  //and extended handling of command parameters/properties
  Result := nil;
end;

end.
