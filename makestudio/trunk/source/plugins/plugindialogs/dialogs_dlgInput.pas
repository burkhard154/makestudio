unit dialogs_dlgInput;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormDlgInput = class(TForm)
    lbInputText: TLabel;
    Memo1: TMemo;
    btOk: TButton;
    btCancel: TButton;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgInputBox( aCaption, aPrompt, aDefault:String):String;

implementation

{$R *.dfm}

function DlgInputBox( aCaption, aPrompt, aDefault:String):String;
begin
  Result := '';
  with TFormDlgInput.Create(nil) do try
    Caption := aCaption;
    lbInputText.Caption := aPrompt;
    Memo1.Text := aDefault;
    if ShowModal=mrOk then begin
      Result := Memo1.Text;
    end;
  finally
    Free;
  end;
end;

end.
