unit dotnetclient.test.vclform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Borland.Vcl.StdCtrls, System.ComponentModel, Borland.Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.nfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage( Edit1.Text);
end;

end.
