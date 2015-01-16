unit PluginMsBuildactiontest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.Mask, JvExMask, JvToolEdit;

type
  TFormActionTest = class(TForm)
    Label1: TLabel;
    tbMSBuildExe: TJvFilenameEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormActionTest: TFormActionTest;

resourcestring
  StrMSBuildFileNotFound = 'Datei existiert nicht! Bitte eine MSBuild.exe Datei auswählen.';

implementation

{$R *.dfm}

procedure TFormActionTest.Button1Click(Sender: TObject);
begin
  if not FileExists(Trim(tbMSBuildExe.Text)) then
  begin
    MessageDlg(StrMSBuildFileNotFound, mtError, [mbOK], 0);
    ModalResult := mrNone;
  end
  else
  begin
    ModalResult := mrOk;
  end;
end;

end.
