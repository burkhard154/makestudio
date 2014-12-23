unit dotnetclient.clientobject;

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

{$AUTOBOX ON}

constructor TClientClass.Create;
begin
  inherited Create;
  // TODO: Hier die Konstruktorlogik einfügen
end;

function TClientClass.TestCall(S: String): String;
begin
  Result := S + ' - .NET object answers!';
end;

end.
