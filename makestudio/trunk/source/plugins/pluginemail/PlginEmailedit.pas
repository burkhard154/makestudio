unit PlginEmailedit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.Buttons, IdMessage, IdEMailAddress;

type
  TFormEditSendemail = class(TForm)
    Button1: TButton;
    Button2: TButton;
    tbSubject: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    memoText: TMemo;
    Label4: TLabel;
    tbSendTo: TEdit;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormEditSendemail: TFormEditSendemail;

implementation

uses
  PlginEmailvars;

resourcestring
  StrPleaseEnterEmail = 'Please enter an email address!';

{$R *.dfm}

procedure TFormEditSendemail.Button3Click(Sender: TObject);
var
  addressItem : TIdEMailAddressItem;
begin
  if length(Trim(tbSendTo.Text)) <= 0 then
  Begin
    MessageDlg(StrPleaseEnterEmail, mtWarning, [mbOK], 0);
    Exit;
  End;

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
    _IdMessage1.Body.Add(memoText.Lines.Text);

    _IdMessage1.Subject := Trim(tbSubject.Text);

    _IdMessage1.Recipients.Clear;
    addressItem         := _IdMessage1.Recipients.Add;
    addressItem.Address := Trim(tbSendTo.Text);
    addressItem.Text    := Trim(tbSendTo.Text);

    _IdMessage1.Sender.Address := gMailSenderAddress;
    _IdMessage1.Sender.Text    := gMailSenderAddress;

    _IdSMTP1.Connect;
    _IdSMTP1.Send(_IdMessage1);

    MessageDlg('Email successful sent!', mtInformation, [mbOK], 0);
  finally
    _IdSMTP1.Disconnect;
  end;
end;

end.
