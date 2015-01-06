unit PlginEmailactiontest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdCmdTCPServer,
  IdExplicitTLSClientServerBase, IdSMTPServer, JvComponentBase, JvMail, IdTCPConnection, IdTCPClient, IdMessageClient,
  IdSMTPBase, IdSMTP, JvMailSlots, IdMessage, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL;

type
  TFormActionTest = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    IdSMTP1: TIdSMTP;
    IdMessage1: TIdMessage;
    Button3: TButton;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    procedure Button3Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormActionTest: TFormActionTest;

implementation

{$R *.dfm}

procedure TFormActionTest.Button3Click(Sender: TObject);
begin
  IdSMTP1.Connect;
  IdSMTP1.Send(IdMessage1);
end;

end.
