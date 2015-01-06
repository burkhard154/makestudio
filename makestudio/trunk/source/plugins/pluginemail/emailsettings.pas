unit emailsettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxMaskEdit, cxDropDownEdit,
  cxTextEdit, cxLabel;

type
  TFormEmailSettings = class(TForm)
    cxLabel2: TLabel;
    cxLabel4: TLabel;
    cxLabel3: TLabel;
    cxLabel5: TLabel;
    cxLabel6: TLabel;
    cxLabel7: TLabel;
    cxLabel8: TLabel;
    edMailserver: TEdit;
    edMailuser: TEdit;
    edMailpassword: TEdit;
    edMailSSLPort: TEdit;
    edMailSenderName: TEdit;
    edMailsenderaddress: TEdit;
    Label1: TLabel;
    btOk: TButton;
    btCancel: TButton;
    cbMailSSL: TComboBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

procedure DlgEditEmailSettings;

implementation

uses PlginEmailvars;

{$R *.dfm}

procedure DlgEditEmailSettings;
begin
  with TFormEmailSettings.Create(nil) do
    try
      LoadFromRegistry;
      edMailserver.Text := gMailServer;
      edMailuser.Text := gMailUserName;
      edMailpassword.Text := gMailPassword;
      edMailSSLPort.Text := IntToStr(gMailSSLPort);
      edMailSenderName.Text := gMailSender;
      edMailsenderaddress.Text := gMailSenderAddress;
      cbMailSSL.ItemIndex := gMailSSLType;
      if ShowModal = mrOk then
      begin
        gMailServer := edMailserver.Text;
        gMailUserName := edMailuser.Text;
        gMailPassword := edMailpassword.Text;
        gMailSSLPort := StrToInt( edMailSSLPort.Text);
        gMailSender := edMailSenderName.Text;
        gMailSenderAddress := edMailsenderaddress.Text;
        gMailSSLType := cbMailSSL.ItemIndex;
        SaveToRegistry;
      end;
    finally
      Free;
    end;
end;

end.
