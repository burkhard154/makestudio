program clrwin32host;

uses
  Forms,
  HostMain in 'HostMain.pas' {Form3},
  DotNetUtils in 'DotNetUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
