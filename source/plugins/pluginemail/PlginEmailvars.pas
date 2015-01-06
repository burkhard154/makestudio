unit PlginEmailvars;

interface

uses
   system.classes, System.Win.Registry, system.SysUtils, makestudio_TLB,
            IdSMTPServer,  IdExplicitTLSClientServerBase,
   IdSMTPBase, IdSMTP, IdMessage, IdIOHandlerStack, IdSSL, IdSSLOpenSSL;

var
  MakeStudio: IJApplication;
  FCanceled: Boolean = False;

  //Mail - Vars
  gMailServer:String='';
  gMailUserName:String='';
  gMailPassword:String='';
  gMailSSLType:Integer = 0;
  gMailSSLPort:Integer = 0;
  gMailSender:String='';
  gMailSenderAddress:String='';

  _IdSMTP1: TIdSMTP;
  _IdMessage1: TIdMessage;
  _IdSSLIOHandlerSocketOpenSSL1 : TIdSSLIOHandlerSocketOpenSSL;

resourcestring
  struPluginName   = 'Email-Plugin';
  struPluginAuthor = 'Steffen Hornig';
  struPluginHint   = 'Plugin zum verschicken von Emails.';
  stCategory       = 'Network';

procedure LoadFromRegistry;
procedure SaveToRegistry;

procedure SetupSmtp;

const
  ctRecipient = 'Recipient';
  ctSubject   = 'Subject';
  ctMessage   = 'Message';

implementation

function GetRegistryKey:string;
begin
  Result := MakeStudio.ApplicationRegKey + '\plugins\email\';
end;

procedure SetupSmtp;
begin
  _IdSMTP1.IOHandler := _IdSSLIOHandlerSocketOpenSSL1;
  _IdSMTP1.Username := gMailUserName;
  _IdSMTP1.Password := gMailPassword;
  _IdSMTP1.Host     := gMailServer;
  _IdSMTP1.Port     := gMailSSLPort;
  _IdSMTP1.HeloName := gMailSender;

  if gMailSSLType <> 0 then
    _IdSMTP1.UseTLS   := utUseExplicitTLS
  else
    _IdSMTP1.UseTLS   := utNoTLSSupport;
end;

procedure LoadFromRegistry;
var
  r : TRegistry;
begin
  r := TRegistry.Create;
  try
    if r.OpenKeyReadOnly( GetRegistryKey) then
    begin
      try
        gMailServer   := r.ReadString('mailserver');
        gMailUserName := r.ReadString('username');
        gMailPassword := r.ReadString('password');
        gMailSSLPort  := r.ReadInteger('port');
        gMailSender   := r.ReadString('sender');
        gMailSenderAddress   := r.ReadString('senderaddress');
        gMailSSLType  := r.ReadInteger('ssltype');
      except
        SaveToRegistry; // Falls ein Eintrag noch nicht in Registry vorhanden ist
      end;
    end;
  finally
    r.Free;
  end;
end;

procedure SaveToRegistry;
var
  r : TRegistry;
begin
  r := TRegistry.Create;
  try
    r.OpenKey(GetRegistryKey, true);
    r.WriteString( 'mailserver', gMailServer);
    r.WriteString( 'username', gMailUserName);
    r.WriteString( 'password', gMailPassword);
    r.WriteString( 'sender', gMailSender);
    r.WriteString( 'senderaddress', gMailSenderAddress);
    r.WriteInteger('port', gMailSSLPort);
    r.WriteInteger('ssltype', gMailSSLType);
  finally
    r.Free;
  end;
end;


initialization
  _IdSMTP1    := TIdSMTP.Create(nil);
  _IdMessage1 := TIdMessage.Create(nil);
  _IdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

finalization
  _IdSMTP1.Free;
  _IdMessage1.Free;
  _IdSSLIOHandlerSocketOpenSSL1.Free;

end.
