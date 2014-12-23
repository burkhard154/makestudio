{$IFDEF BLOCKHEADER}
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
{$ENDIF BLOCKHEADER}
unit %MODULEIDENT%;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils;

type
  TPlugin%COMMANDIDENTIFIER% = class(TComponent, ICommand2)
  private
    FCaption: string;
    {$IFDEF BLOCKSAMPLEVAR}
    FTestValue: string;
    {$ENDIF BLOCKSAMPLEVAR}
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
  TPlugin%COMMANDIDENTIFIER%Callback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  Plugin%COMMANDIDENTIFIER%Callback: TPlugin%COMMANDIDENTIFIER%Callback;

const
  IDPlugin%COMMANDIDENTIFIER% = '%PLUGINIDENTIFIER%.%COMMANDIDENTIFIER%';

{
  Example code to register the command. 
  To be used in the "RegisterPlugin" funktion of the project file.
  
      //--- add then command: %COMMANDNAME%
	  // 1. Get the image from an image list
      GetPictureFromImageList(FormActions.ImageList1, 0, P);
	  
	  // 2. Create the global command callback
      Plugin%COMMANDIDENTIFIER%Callback := TPlugin%COMMANDIDENTIFIER%Callback.Create(nil);

	  // 3. Register the command itsel
      //Name=%COMMANDNAME%; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      jvcsmak.AddCommandType('%COMMANDNAME%', 'Your Hint here!', stCategory, P, 'txt', -1,
        ICommandCallback(Plugin%COMMANDIDENTIFIER%Callback));
  
}  
implementation

uses
  ComServ, %PLUGINIDENTIFIER%Vars, %EDITUNIT%;

function TPlugin%COMMANDIDENTIFIER%Callback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPlugin%COMMANDIDENTIFIER%.Create(nil));
end;

procedure TPlugin%COMMANDIDENTIFIER%Callback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
end;

constructor TPlugin%COMMANDIDENTIFIER%.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := '%COMMANDNAME%';
  {$IFDEF BLOCKSAMPLEVAR}
  FTestValue := '%SAMPLEVARVALUE%';
  {$ENDIF BLOCKSAMPLEVAR}
end;

function TPlugin%COMMANDIDENTIFIER%.EditItem: WordBool;
begin
  Result := False;
  with TFormEdit%COMMANDCOMPONENTNAME%.Create(nil) do
  try
    {$IFDEF BLOCKSAMPLEVAR}
    Edit1.Text := FTestValue;
    {$ENDIF BLOCKSAMPLEVAR}
    if ShowModal = mrOk then
    begin
      {$IFDEF BLOCKSAMPLEVAR}
      FTestValue := Edit1.Text;
      {$ENDIF BLOCKSAMPLEVAR}
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TPlugin%COMMANDIDENTIFIER%.ExecuteItem: WordBool;
begin
  FCanceled := False;
  {$IFDEF BLOCKSAMPLEVAR}
  jvcsmak.LogMessage(FCaption + ' ' + FTestValue);
  {$ENDIF BLOCKSAMPLEVAR}
  jvcsmak.LogMessage('Executing %COMMANDNAME%...');
  Result := True;
end;

function TPlugin%COMMANDIDENTIFIER%.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
{$IFDEF BLOCKSAMPLEPAINTCODE}
var
  Canvas: TCanvas;
{$ENDIF BLOCKSAMPLEPAINTCODE}
begin
  {$IFNDEF BLOCKSAMPLEPAINTCODE}
  Result := -1; //auto
  {$ELSE}
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
  {$ENDIF ~BLOCKSAMPLEPAINTCODE}
end;

function TPlugin%COMMANDIDENTIFIER%.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
{$IFDEF BLOCKSAMPLEPAINTCODE}
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;
{$ENDIF BLOCKSAMPLEPAINTCODE}  
begin
  {$IFNDEF BLOCKSAMPLEPAINTCODE}
  Result := True; //auto
  {$ELSE}
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
    {$IFDEF BLOCKSAMPLEVAR}
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, FCaption + ' ' + FTestValue);
    {$ENDIF BLOCKSAMPLEVAR}    
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
  {$ENDIF ~BLOCKSAMPLEPAINTCODE}
end;

procedure TPlugin%COMMANDIDENTIFIER%.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPlugin%COMMANDIDENTIFIER%.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TPlugin%COMMANDIDENTIFIER%.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPlugin%COMMANDIDENTIFIER%.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  {$IFDEF BLOCKSAMPLEVAR}
  if SameText(ParamName, '%SAMPLEVARNAME%') then
    Result := FTestValue;
  {$ENDIF BLOCKSAMPLEVAR}
end;

procedure TPlugin%COMMANDIDENTIFIER%.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  {$IFDEF BLOCKSAMPLEVAR}
  if SameText(ParamName, '%SAMPLEVARNAME%') then
    FTestValue := Value;
  {$ENDIF BLOCKSAMPLEVAR}
end;

function TPlugin%COMMANDIDENTIFIER%.Get_ParamNames(Index: Integer): WideString;
begin
  {$IFDEF BLOCKSAMPLEVAR}
  Result := '%SAMPLEVARNAME%';
  {$ENDIF BLOCKSAMPLEVAR}
end;

function TPlugin%COMMANDIDENTIFIER%.Get_ParamCount: Integer;
begin
  {$IFDEF BLOCKSAMPLEVAR}
  Result := 1;
  {$ELSE}
  Result := 0;
  {$ENDIF BLOCKSAMPLEVAR}
end;

function TPlugin%COMMANDIDENTIFIER%.Get_OwnerDraw: WordBool;
begin
  Result := false;
end;

function TPlugin%COMMANDIDENTIFIER%.Get_PreviewText: WideString;
begin
  Result := '';
end;

function TPlugin%COMMANDIDENTIFIER%.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := varEmpty;
end;

function TPlugin%COMMANDIDENTIFIER%.Get_Properties: IDispatch;
begin
  Result := nil;
end;


function TPlugin%COMMANDIDENTIFIER%Callback.GetIdentifier: WideString;
begin
  Result := IDPlugin%COMMANDIDENTIFIER%;
end;





end.
