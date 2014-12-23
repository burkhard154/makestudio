unit Unit13;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, omutils, Mask, JvExMask, JvToolEdit;

type
  TForm13 = class(TForm)
    Button1: TButton;
    JvFilenameEdit1: TJvFilenameEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form13: TForm13;

implementation

{$R *.dfm}

procedure TForm13.Button1Click(Sender: TObject);
var h:THandle;
    p:Procedure;
begin
  try
    h := LoadLibrary( PChar( JvFilenameEdit1.FileName));
    if h>0 then begin
      ShowMessage( 'DLL loaded');
      @p := GetProcAddress( h, 'DllTestProc');
      if @p<>nil then begin
        ShowMessage( 'Proc found - calling Proc...');
        p;
      end
      else
        ShowMessage( 'Proc "DllTestProc" not found"');

    end
    else
      ShowMessage( omGetLastError);
  except
    On E:Exception do ShowMessage( E.Message);
  end;
end;

end.
