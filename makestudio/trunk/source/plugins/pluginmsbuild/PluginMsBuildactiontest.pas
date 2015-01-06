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
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormActionTest: TFormActionTest;

implementation

{$R *.dfm}

end.
