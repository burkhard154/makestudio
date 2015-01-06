unit PluginMsBuildedit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.Mask, JvExMask, JvToolEdit;

type
  TFormEditTestcommand = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    tbSolutionPath: TJvFilenameEdit;
    Label2: TLabel;
    tbOutputPath: TJvDirectoryEdit;
    cbCleanup: TCheckBox;
    Label3: TLabel;
    tbBuildConfig: TEdit;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormEditTestcommand: TFormEditTestcommand;

implementation

{$R *.dfm}

end.
