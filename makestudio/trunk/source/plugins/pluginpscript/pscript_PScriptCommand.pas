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

2006/04/21  BSchranz  - Plugin Script created
2006/05/01  BSchranz  - PSTypInfo and PSUtils added for browser functionallity
2005/05/27  BSchranz  - Released


------------------------------------------------------------------------------}
unit pscript_PScriptCommand;

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

      //Name=PScript; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before

      //Create and register Callback for the command type
      PluginPScriptCallback := TPluginPScriptCallback.Create(nil);
      MakeStudio.AddCommandType('PScript', '', stCategory, P, 'txt', -1,
        ICommandCallback(PluginPScriptCallback));
**** End Sample Code  *******}

type
  TPluginPScript = class(TComponent, ICommand2)
  private
    FCaption: string;
    FScript : TStringList;
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
    destructor Destroy; override;
  end;

  //Callback to create an instance of the ICommand
  TPluginPScriptCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginPScriptCallback: TPluginPScriptCallback;

const
  IDPluginPScript = 'Script.PScript';

implementation

uses
  ComServ, pscript_Vars, pscript_PScriptEdit;

resourcestring
  StrStartingPascalScri = 'Starting Pascal Script....';
  strBreak = '********************************************************';
  StrPascalScriptSucces = 'Pascal Script successfully executed...';
  StrErrorExecutingPasc = 'Error executing Pascal Script...';

{ TPluginPScriptCallback }

function TPluginPScriptCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginPScript.Create(nil));
end;

procedure TPluginPScriptCallback.SetCanceled(aCanceled: WordBool);
begin
  FCanceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
  if FCanceled then
    PSHandler.Stop;
end;

function TPluginPScriptCallback.GetIdentifier: WideString;
begin
  Result := IDPluginPScript;
end;

{ TPluginPScript }

constructor TPluginPScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'PScript';
  fScript := TStringList.Create;
end;

function TPluginPScript.EditItem: WordBool;
begin
  Result := False;
  with TFormEditScriptParams.Create(nil) do
  try
    ed.Lines.Assign( FScript);
    if ShowModal = mrOk then
    begin
      FScript.Assign( ed.Lines);
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TPluginPScript.ExecuteItem: WordBool;
begin
  FCanceled := False;

  MakeStudio.LogMessage( strBreak);
  MakeStudio.LogMessage( StrStartingPascalScri);
  PSHandler.ScriptText.Assign( FScript);
  Result := PSHandler.Execute;

  if FCanceled then
    Result := false;

  if Result then
    MakeStudio.LogMessage( StrPascalScriptSucces)
  else
    MakeStudio.LogMessage( StrErrorExecutingPasc);
end;

function TPluginPScript.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
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

function TPluginPScript.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
end;

procedure TPluginPScript.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginPScript.Get_Caption: WideString;
begin
  Result := stCommandCaption;
end;

procedure TPluginPScript.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginPScript.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, 'LineCount') then
    Result := IntToStr( FScript.Count)
  else
{$IFDEF UNICODE}
    if System.Pos( WideString('Line'), ParamName)>0 then begin
{$ELSE}
    if System.Pos( 'Line', ParamName)>0 then begin
{$ENDIF}
      try
        Result := FScript[ StrToInt( StringReplace( ParamName, 'Line', '', [rfReplaceAll]))-1];
      except
        On E:Exception do ShowMessage( 'PascalScript.GetParam( '''+ParamName+'''):'+E.Message);
      end;
    end;
end;

procedure TPluginPScript.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var i:Integer;
begin
  try
    if SameText(ParamName, 'LineCount') then begin
      FScript.Clear;
      for i:=0 to StrToInt( Value)-1 do
        FScript.Add( '');
    end
    else
{$IFDEF UNICODE}
      if Pos( WideString('Line'), ParamName)>0 then begin
{$ELSE}
      if Pos( 'Line', ParamName)>0 then begin
{$ENDIF}
        FScript[ StrToInt( StringReplace( ParamName, 'Line', '', [rfReplaceAll]))-1] := Value;
      end;
  except
    On E:Exception do ShowMessage( 'PascalScript.GetParam( '''+ParamName+'''):'+E.Message);
  end;
end;

function TPluginPScript.Get_ParamNames(Index: Integer): WideString;
begin
  if Index>0 then
    Result := 'Line'+IntToStr( Index)
  else
    Result := 'LineCount';
end;

function TPluginPScript.Get_ParamCount: Integer;
begin
  Result := FScript.Count + 1;
end;

function TPluginPScript.Get_OwnerDraw: WordBool;
begin
  //Use Caption and PreviewText!
  //Otherwise, if Result = true, you can use
  //DrawItem and MeasureItem
  Result := false;
end;

function TPluginPScript.Get_PreviewText: WideString;
var i:Integer;
begin
  if FScript.Count>10 then begin
    Result := '';
    for i:=0 to 9 do
      Result := Result + FScript[i] + #10;
    Result := Result + '...';
  end
  else
    Result := FScript.Text;
end;

function TPluginPScript.Notify(const Notification: WideString; Parameter: OleVariant): OleVariant;
begin
  //nothing to do
  //for future purpose - e.g. active language changed
  Result := 0;
end;

function TPluginPScript.Get_Properties: IDispatch;
begin
  //nothing to do
  //for future purpose - integration of an property inspector
  //and extended handling of command parameters/properties
  Result := nil;
end;

destructor TPluginPScript.Destroy;
begin
  FScript.Free;
  inherited;
end;

end.
