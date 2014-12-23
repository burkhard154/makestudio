unit dotnetclient.test.clientobject;

interface

uses
  System.Runtime.InteropServices;

type
  [ComVisible( true)]
  TClientClass = class( TObject)
  private
    { Private-Deklarationen }
  public
    constructor Create;

    function TestCall( S:String):String;
  end;

implementation

uses dotnetclient.test.vclform;

{$AUTOBOX ON}

constructor TClientClass.Create;
begin
  inherited Create;
  // TODO: Hier die Konstruktorlogik einfügen
end;

function TClientClass.TestCall(S: String): String;
begin
  Result := '';
  with TForm1.Create( nil) do
  try
    Edit1.Text := S;
    ShowModal;
    Result :=  Edit1.Text;
  finally
    Free;
  end;
  //Result := S + ' - .NET object answers!';
end;

end.
