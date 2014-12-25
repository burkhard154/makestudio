unit wizard_parser;

interface

uses
  Classes, SysUtils, Types, JclStrings, JclFileUtils;

// function GetTemplateSource(AFileName: string; AParameterList: TStrings): string; overload;
// procedure GetTemplateSource(AFileName: string; AParameterList: TStrings; AResult: TStrings); overload;
// function GetTemplateSource(ATemplateFileName, AResultFilename: string; AParameterList: TStrings): string; overload;

procedure CreateTemplateSource(ASource, AResult, AParameterList: TStrings);
procedure CreateTemplateSourceFromRessource(AIdentifier, AResultFile:String; AParameterList: TStrings);
procedure GetTemplateSourceFromRessource(AIdentifier: String; ASource: TStrings);
procedure GetTemplateSourceFromFile(AFileName: String; ASource: TStrings);

// Validate Identifier
function MakeValidIdent(const s: string): string;

implementation

type
  TDefineHandler = class(TObject)
  private
    FEnabled: Boolean;
    FDefineLine: Boolean;
    FCurrentStack: TStringList;
    FDefines: TStringList;
    function GetCanWrite: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure HandleLine(const AStr: string);

    property CanWrite: Boolean read GetCanWrite;
    property Defines: TStringList read FDefines;
  end;

constructor TDefineHandler.Create;
begin
  inherited Create;
  FEnabled := True;
  FDefineLine := False;
  FCurrentStack := TStringList.Create;
  FDefines := TStringList.Create;
end;

destructor TDefineHandler.Destroy;
begin
  FDefines.Free;
  FCurrentStack.Free;
  inherited Destroy;
end;

function TDefineHandler.GetCanWrite: Boolean;
begin
  Result := FEnabled and (not FDefineLine);
end;

function IsDirectiveStr(ASubStr: string; s: string): Boolean;
begin
  Result := SameText(Copy(s, 3, Length(ASubStr)), ASubStr);
end;

procedure CheckAddDef(ADirective: string; ACurrentDefines: TStrings);
var
  mydefine: string;
begin
  if IsDirectiveStr('DEFINE ', ADirective) then
  begin
    mydefine := Copy(ADirective, 10, Length(ADirective) - 10);
    if Assigned(ACurrentDefines) and (ACurrentDefines.IndexOf(UpperCase(mydefine)) = -1) then
      ACurrentDefines.Add(UpperCase(mydefine));
  end;
end;

procedure CheckRemoveDef(ADirective: string; ACurrentDefines: TStrings);
var
  mydefine: string;
begin
  if IsDirectiveStr('UNDEF ', ADirective) then
  begin
    mydefine := Copy(ADirective, 9, Length(ADirective) - 9);
    if Assigned(ACurrentDefines) and (ACurrentDefines.IndexOf(UpperCase(mydefine)) <> -1) then
      ACurrentDefines.Delete(ACurrentDefines.IndexOf(UpperCase(mydefine)));
  end;
end;

function CheckIfXDef(ADirective: string; ACurrentDefines: TStrings): Boolean;
var
  mydefine: string;
begin
  Result := False;
  if IsDirectiveStr('IFDEF ', ADirective) then
  begin
    mydefine := Copy(ADirective, 9, Length(ADirective) - 9);
    Result := (Assigned(ACurrentDefines)) and (ACurrentDefines.IndexOf(UpperCase(mydefine)) <> -1);
  end
  else if IsDirectiveStr('IFNDEF ', ADirective) then
  begin
    mydefine := Copy(ADirective, 10, Length(ADirective) - 10);
    Result := (Assigned(ACurrentDefines)) and (ACurrentDefines.IndexOf(UpperCase(mydefine)) = -1);
  end;
end;

function ReverseIfXDef(ADirective: string): string;
begin
  Result := '';
  if IsDirectiveStr('IFDEF ', ADirective) then
    Result := Copy(ADirective, 1, 2) + 'IFNDEF ' + Copy(ADirective, 9, Length(ADirective) - 8)
  else if IsDirectiveStr('IFNDEF ', ADirective) then
    Result := Copy(ADirective, 1, 2) + 'IFDEF ' + Copy(ADirective, 10, Length(ADirective) - 9)
  else
  begin
    // ParsingError
  end;
end;

function IsIgnoreItem(AStr: string): Boolean;
begin
  Result := Pos('DELPHI', AStr) > 0;
end;

function ProcessDefsStack(ADirective: string; ADefineStack: TStrings): Boolean;
begin
  Result := False;
  if (IsDirectiveStr('ENDIF}', ADirective) or IsDirectiveStr('ENDIF ', ADirective)) and (ADefineStack.Count > 0) then
  begin
    Result := not IsIgnoreItem(ADefineStack[ADefineStack.Count - 1]);
    ADefineStack.Delete(ADefineStack.Count - 1);
  end
  else if (IsDirectiveStr('ELSE}', ADirective) or IsDirectiveStr('ELSE ', ADirective)) and (ADefineStack.Count > 0) then
  begin
    Result := not IsIgnoreItem(ADefineStack[ADefineStack.Count - 1]);
    ADefineStack[ADefineStack.Count - 1] := ReverseIfXDef(ADefineStack[ADefineStack.Count - 1]);
  end
  else if IsDirectiveStr('IFDEF ', ADirective) or IsDirectiveStr('IFNDEF ', ADirective) then
  begin
    ADefineStack.Add(ADirective);
    Result := not IsIgnoreItem(ADirective);
  end;
end;

function ProcessDef(ADirective: string; ACurrentDefines, ADefineStack: TStrings; var IsDefLine: Boolean): Boolean;
var
  I: Integer;
begin
  Result := True;
  IsDefLine := ProcessDefsStack(ADirective, ADefineStack);
  if IsDefLine then
  begin
    for I := 0 to ADefineStack.Count - 1 do
      if Result then
        Result := CheckIfXDef(ADefineStack[I], ACurrentDefines);
    if Result then
    begin
      CheckAddDef(ADirective, ACurrentDefines);
      CheckRemoveDef(ADirective, ACurrentDefines);
    end;
  end;
end;

procedure TDefineHandler.HandleLine(const AStr: string);
begin
  FDefineLine := False;
  if Pos('{$', Trim(AStr)) = 1 then
    FEnabled := ProcessDef(Trim(AStr), FDefines, FCurrentStack, FDefineLine);
end;

procedure CreateTemplateSource(ASource, AResult, AParameterList: TStrings);
var
  I, J: Integer;
  s: string;
  DefineHandler: TDefineHandler;
begin
  AResult.Clear;
  DefineHandler := TDefineHandler.Create;
  try
    for I := 0 to Pred(AParameterList.Count) do
      if (Pos('BLOCK', AParameterList.Names[I]) = 1) and (AParameterList.Values[AParameterList.Names[I]] <> '0') then
        DefineHandler.Defines.Add(AParameterList.Names[I]);
    for I := 0 to Pred(ASource.Count) do
    begin
      s := ASource[I];
      DefineHandler.HandleLine(s);
      if DefineHandler.CanWrite then
      begin
        for J := 0 to Pred(AParameterList.Count) do
          StrReplace(s, '%' + AParameterList.Names[J] + '%', AParameterList.Values[AParameterList.Names[J]],
            [rfReplaceAll, rfIgnoreCase]);
        AResult.Add(s);
      end;
    end;
  finally
    DefineHandler.Free;
  end;
end;

procedure GetTemplateSourceFromRessource(AIdentifier: String; ASource: TStrings);
var
  ResStream: TResourceStream;
  sl: TStringList;
begin
  // Load Template Source
  ResStream := TResourceStream.Create(hInstance, AIdentifier, RT_RCDATA);
  try
    ASource.Clear;
    ASource.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
end;

procedure CreateTemplateSourceFromRessource(AIdentifier, AResultFile:String; AParameterList: TStrings);
var sl1, sl2:TStringList;
begin
  sl1 := TStringList.Create;
  sl2 := TStringList.Create;
  try
    GetTemplateSourceFromRessource( AIdentifier, sl1);
    CreateTemplateSource(sl1, sl2, AParameterList);
    sl2.SaveToFile(AResultFile);
  finally
    sl1.Free;
    sl2.Free;
  end;
end;

procedure GetTemplateSourceFromFile(AFileName: String; ASource: TStrings);
begin
  ASource.LoadFromFile(AFilename);
end;

function MakeValidIdent(const s: string): string;
var
  len: Integer;
  x: Integer;
  c: Char;
begin
  SetLength(Result, Length(s));
  x := 0;
  for c in s do
    if c in ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_'] then
    begin
      Inc(x);
      Result[x] := c;
    end;
  SetLength(Result, x);
  if x = 0 then
    Result := '_'
  else if Result[1] in ['0' .. '9'] then
    Result := '_' + Result;
end;

end.
