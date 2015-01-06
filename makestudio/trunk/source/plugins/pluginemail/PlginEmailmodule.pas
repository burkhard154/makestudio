unit PlginEmailmodule;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, makestudio_TLB,
  Classes, Windows, Dialogs, Controls, SysUtils;

type
  TPluginSendemail = class(TComponent, ICommand2)
  private
    FCaption: string;

    FRecipient : string;
    FSubject   : string;
    FMessage   : string;
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
  TPluginSendemailCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginSendemailCallback: TPluginSendemailCallback;

const
  IDPluginSendemail = 'PluginEmail.Sendemail';

{
  Example code to register the command. 
  To be used in the "RegisterPlugin" funktion of the project file.
  
      //--- add then command: Send email
	  // 1. Get the image from an image list
      GetPictureFromImageList(FormActions.ImageList1, 0, P);
	  
	  // 2. Create the global command callback
      PluginSendemailCallback := TPluginSendemailCallback.Create(nil);

	  // 3. Register the command itsel
      //Name=Send email; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      MakeStudio.AddCommandType('Send email', 'Your Hint here!', stCategory, P, 'txt', -1,
        ICommandCallback(PluginSendemailCallback));
  
}  
implementation

uses
  ComServ, PlginEmailVars, PlginEmailEdit, IdMessage, IdEMailAddress;

resourcestring
  StrEmailSent = 'Email successful sent to ';
  StrStartSendingEmail = 'Start sending email...';

function TPluginSendemailCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginSendemail.Create(nil));
end;

procedure TPluginSendemailCallback.SetCanceled(aCanceled: WordBool);
begin
  FCanceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
end;

constructor TPluginSendemail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'Send email';
  LoadFromRegistry;
end;

function TPluginSendemail.EditItem: WordBool;
begin
  Result := False;
  with TFormEditSendemail.Create(nil) do
  try
    tbSubject.Text := FSubject;
    tbSendTo.Text  := FRecipient;
    memoText.Lines.Text := FMessage;
    if ShowModal = mrOk then
    begin
      Result := True;
      FSubject   := tbSubject.Text;
      FRecipient := tbSendTo.Text ;
      FMessage   := memoText.Lines.Text;
    end;
  finally
    Free;
  end;
end;

function TPluginSendemail.ExecuteItem: WordBool;
var
  addressItem : TIdEMailAddressItem;
begin
  MakeStudio.LogMessage(StrStartSendingEmail);
  FCanceled := False;

  try
    SetupSmtp;
  except
    on E: Exception do MakeStudio.LogMessage('Exception: ' + E.Message);
  end;

  try
    // Test email settings
    _IdMessage1.ClearBody;
    _IdMessage1.Clear;

    _IdMessage1.Body.Clear;
    _IdMessage1.Body.Add(MakeStudio.Variables.ReplaceVarsInString(FMessage));

    _IdMessage1.Subject := Trim(MakeStudio.Variables.ReplaceVarsInString(FSubject));

    _IdMessage1.Recipients.Clear;
    addressItem         := _IdMessage1.Recipients.Add;
    addressItem.Address := Trim(FRecipient);
    addressItem.Text    := Trim(FRecipient);

    _IdMessage1.Sender.Address := gMailSenderAddress;
    _IdMessage1.Sender.Text    := gMailSenderAddress;

    _IdSMTP1.Connect;
    _IdSMTP1.Send(_IdMessage1);

    Result := True;
    MakeStudio.LogMessage(StrEmailSent + FRecipient);
  finally
    _IdSMTP1.Disconnect;
  end;
end;

function TPluginSendemail.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1; //auto
end;

function TPluginSendemail.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := True; //auto
end;

procedure TPluginSendemail.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginSendemail.Get_Caption: WideString;
begin
  Result := FCaption + ': ' + FRecipient;
end;

procedure TPluginSendemail.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginSendemail.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, ctRecipient) then
    Result := FRecipient;
  if SameText(ParamName, ctSubject) then
    Result := FSubject;
  if SameText(ParamName, ctMessage) then
    Result := FMessage;
end;

procedure TPluginSendemail.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, ctRecipient) then
    FRecipient := Value;
  if SameText(ParamName, ctSubject) then
    FSubject := Value;
  if SameText(ParamName, ctMessage) then
    FMessage := Value;
end;

function TPluginSendemail.Get_ParamNames(Index: Integer): WideString;
begin
  case Index of
    0: Result := ctRecipient;
    1: Result := ctSubject;
    2: Result := ctMessage;
  end;
end;

function TPluginSendemail.Get_ParamCount: Integer;
begin
  Result := 3;
end;

function TPluginSendemail.Get_OwnerDraw: WordBool;
begin
  Result := false;
end;

function TPluginSendemail.Get_PreviewText: WideString;
begin
  Result := '';
end;

function TPluginSendemail.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := varEmpty;
end;

function TPluginSendemail.Get_Properties: IDispatch;
begin
  Result := nil;
end;


function TPluginSendemailCallback.GetIdentifier: WideString;
begin
  Result := IDPluginSendemail;
end;





end.
