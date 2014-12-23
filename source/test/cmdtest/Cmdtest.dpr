program Cmdtest;

uses
  Forms,
  Main in 'Main.pas' {FormRunCmd};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRunCmd, FormRunCmd);
  Application.Run;
end.
