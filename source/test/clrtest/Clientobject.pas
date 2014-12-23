unit Clientobject;

interface

type
  TClientClass = class
  private
    { Private-Deklarationen }
  public
    constructor Create;

    function TestCall( S:String):String;
  end;

implementation

constructor TClientClass.Create;
begin
  inherited Create;
  // TODO: Hier die Konstruktorlogik einfügen
end;

function TClientClass.TestCall(S: String): String;
begin
  Result := S + ' Test';
end;

end.
