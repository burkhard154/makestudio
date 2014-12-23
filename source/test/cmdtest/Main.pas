unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclSysUtils, JvComponent, JvFormAnimation;

type
  TFormRunCmd = class(TForm)
    MemoCmd: TMemo;
    btRun: TButton;
    MemoAnswer: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MemoApp: TMemo;
    JvFormAnimation1: TJvFormAnimation;
    procedure FormCreate(Sender: TObject);
    procedure btRunClick(Sender: TObject);
  private
    function GetDir: String;
    function GetApp: String;
    function GetArgs: String;
    { Private-Deklarationen }
  public
    procedure AddLog( S:String);
    procedure OnCaptureLine(const Text: string);
    property Dir:String read GetDir;
    property App:String read GetApp;
    property Args:String read GetArgs;
  end;

var
  FormRunCmd: TFormRunCmd;

implementation

{$R *.dfm}

procedure TFormRunCmd.AddLog(S: String);
begin
  MemoAnswer.Lines.Add( S);
end;

procedure TFormRunCmd.btRunClick(Sender: TObject);
begin
  if Dir<>'' then
    if not SetCurrentDir( Dir) then
      AddLog( 'Error ExecCmdLine:SetCurrentDir("'+Dir+'")');

  JclSysUtils.Execute(App + ' ' + Args, OnCaptureLine, False, nil)
end;

function TFormRunCmd.GetArgs: String;
begin
  Result := MemoCmd.Lines.Text;
end;

function TFormRunCmd.GetApp: String;
begin
  Result := MemoApp.Lines.Text;
end;

function TFormRunCmd.GetDir: String;
begin
  Result := Edit1.Text;
end;

procedure TFormRunCmd.OnCaptureLine(const Text: string);
begin
  AddLog( Text);
end;

procedure TFormRunCmd.FormCreate(Sender: TObject);
begin
  JvFormAnimation1.AppearTelevision;
end;

end.
