unit resourcesEditMultipleVersionResourceCommand;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils, resources_Module;

{**** Sample Code to register this command *******
var
 P: Picture;
      //--- add modules --------------------------------------------------------
      GetPictureFromImageList( <ImageList1>, 0, P);

      //Name=Edit Multiple Version Resource; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before

      //Create and register Callback for the command type
      PluginEditMultipleVersionResourceCallback := TPluginEditMultipleVersionResourceCallback.Create(nil);
      MakeStudio.AddCommandType('Edit Multiple Version Resource', '', stCategory, P, 'txt', -1,
        ICommandCallback(PluginEditMultipleVersionResourceCallback));
**** End Sample Code  *******}

type
  TPluginEditMultipleVersionResource = class(TComponent, ICommand)
  private
    FCaption: string;

    FIncreaseSet: integer;
    FNewVersionValue: TULargeInteger;
    FIncreaseType : Integer;
    FFileVersionKey : String;
    FFilenames : TStringList;
    FRecursive: boolean;
    FSingleVersion: TPluginVersionsresourcenndern;

    procedure _setFilename(filename:string);
    function _getFilename:string;
  protected
    function proceedDir(dir: String; recursive: boolean):boolean;
    function MeasureItem(Handle: Integer; BriefView: WordBool): Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
      Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString; const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;
    function Get_OwnerDraw: WordBool; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;

    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property Properties: IDispatch read Get_Properties;
  public
    constructor Create(AOwner: TComponent); override;

    property Filename:String read _getFilename write _setFilename;
    property IncreaseType:Integer read FIncreaseType write FIncreaseType;
    property Recursive:boolean read FRecursive write FRecursive;
    property FileVersionKey:String read FFileVersionKey write FFileVersionKey;
    property IncreaseSet: Integer read FIncreaseSet write FIncreaseSet;
    property NewVersionValue: TULargeInteger read FNewVersionValue write FNewVersionValue;    
  end;

  //Callback to create an instance of the ICommand
  TPluginEditMultipleVersionResourceCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginEditMultipleVersionResourceCallback: TPluginEditMultipleVersionResourceCallback;

const
  IDPluginEditMultipleVersionResource = 'resources.EditMultipleVersionResource';

implementation

uses
  ComServ, resources_Vars, resourcesEditMultipleVersionResourceEdit,
  StdCtrls;

{ TPluginEditMultipleVersionResourceCallback }

function TPluginEditMultipleVersionResourceCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TPluginEditMultipleVersionResource.Create(nil));
end;

procedure TPluginEditMultipleVersionResourceCallback.SetCanceled(aCanceled: WordBool);
begin
  FCanceled := True; //set by the server if the user press "Cancel" oder "Stop"
end;

function TPluginEditMultipleVersionResourceCallback.GetIdentifier: WideString;
begin
  Result := IDPluginEditMultipleVersionResource;
end;

{ TPluginEditMultipleVersionResource }

constructor TPluginEditMultipleVersionResource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'Edit Multiple Version Resource';
  FFileVersionKey := 'FileVersion';
  FSingleVersion := TPluginVersionsresourcenndern.Create(AOwner);
end;

function TPluginEditMultipleVersionResource.EditItem: WordBool;
var v1, v2, v3, v4: word;
begin
  Result := False;
  with TFormEditEditMultipleVersionResourceParams.Create(nil) do
  try
    rgType.ItemIndex := IncreaseType;
    rgIncSetClick(nil);
    edVersionKey.Text := FileVersionKey;
    mmFiles.lines.Delimiter := ';';
    cbRecursive.Checked := Recursive;
    mmFiles.lines.DelimitedText := Filename;
    rgIncSet.ItemIndex := IncreaseSet;
    FSingleVersion.DecodeVersion(NewVersionValue, v1, v2, v3, v4);
    updoMain.Position := v1;
    updoMinor.Position := v2;
    updoRel.Position := v3;
    updoBuild.Position := v4;

    if ShowModal = mrOk then
    begin
      Result := True;
      IncreaseSet := rgIncSet.ItemIndex;

      v1 := updoMain.Position;
      v2 := updoMinor.Position;
      v3 := updoRel.Position;
      v4 := updoBuild.Position;
      NewVersionValue := FSingleVersion.EncodeVersion(v1, v2, v3, v4);
      IncreaseType := rgType.ItemIndex;
      Filename := mmFiles.lines.DelimitedText;
      Recursive := cbRecursive.Checked;
      FileVersionKey := edVersionKey.Text;
    end;
  finally
    Free;
  end;
end;

function TPluginEditMultipleVersionResource.ExecuteItem: WordBool;
var i:integer;
begin
  FCanceled := False;
  Result := true;
  //MakeStudio.LogMessage('Executing Edit Multiple Version Resource...');
  FSingleVersion.IncreaseType := IncreaseType;
  FSingleVersion.IncreaseSet := IncreaseSet;
  FSingleVersion.FileVersionKey := FileVersionKey;
  FSingleVersion.NewVersionValue := NewVersionValue;

  for i := 0 to FFilenames.Count - 1 do
  begin
    if (ExtractFileExt( FFilenames.Strings[i]) <> '.res') AND
       (ExtractFileExt( FFilenames.Strings[i]) <> '.rc') AND
       (ExtractFileExt( FFilenames.Strings[i]) <> '.dcr') then
      result := result AND proceedDir(FFilenames.Strings[i], Recursive)
    else
    begin
      FSingleVersion.Filename := FFilenames.Strings[i];
      Result := Result AND FSingleVersion.ProceedWin32Res;
    end;
  end;
end;

{Searches the given directory for files of type .res and .dcr and
 changes their version as defined.
 If recursive is true, it handles the subdirectories the same way. 
}
function TPluginEditMultipleVersionResource.proceedDir(dir: String; recursive: boolean):boolean;
var sr: TSearchRec;
    FileAttrs: Integer;
begin
  Result := true;
  FileAttrs := faAnyFile;
  if FindFirst(IncludeTrailingBackslash(dir)+'*', FileAttrs, sr) = 0 then
  begin
    repeat
      if (ExtractFileExt( sr.Name) = '.res') OR
         (ExtractFileExt( sr.Name) = '.dcr') OR
         (ExtractFileExt( sr.Name) = '.rc') then
      begin
        FSingleVersion.Filename := IncludeTrailingBackslash(dir) + sr.Name;
        Result := Result AND FSingleVersion.ProceedWin32Res;
      end;
      if ((sr.Attr and faDirectory) = sr.Attr) AND recursive AND
         (sr.Name <> '.') AND (sr.Name <> '..') then
        Result := Result AND proceedDir(IncludeTrailingBackslash(dir) + sr.Name, true);
    until FindNext(sr) <> 0;
  end;
end;

function TPluginEditMultipleVersionResource.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  //----------------------------- Example ------------------------
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 2;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
    if not BriefView then
    begin
      Canvas.Font.Style := [fsBold];
      Result := Result + Canvas.TextHeight(FCaption) + 2;
      Canvas.Font.Style := [];
      Result := Result + Canvas.TextHeight(FCaption) + 2;
    end;
  finally
    Canvas.Free;
  end;
end;

function TPluginEditMultipleVersionResource.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
   Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;
  v1, v2, v3, v4: word;
  tmp:string;
  
  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  //----------------------------- Example ------------------------
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    aRect := Rect(Left, Top, Right, Bottom);
    if Selected then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(aRect);
    end
    else
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(aRect);
    end;
    Offset := 2;

    Canvas.Font.Style := [fsBold];
    SetCanvasTextColor(clWindowText);
    Canvas.TextOut(aRect.Left +iDefaultIndent, aRect.Top + Offset, FCaption);
    Offset := Offset + Canvas.TextHeight(FCaption) + 2;

    if not BriefView then
    begin
      Canvas.Font.Style := [fsBold];
      SetCanvasTextColor(clBlue);
      Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, Filename);
      Offset := Offset + Canvas.TextHeight(Filename) + 2;

      Canvas.Font.Style := [];
      SetCanvasTextColor(clMaroon);
      tmp := stSetTo + ' ';
      if IncreaseSet=1 then
        case IncreaseType of
          0: tmp := tmp + stMainVersion;
          1: tmp := tmp + stReleaseVersion;
          2: tmp := tmp + stIssueVersion;
          3: tmp := tmp + stBuildVersion;
        end
      else begin
        FSingleVersion.DecodeVersion(NewVersionValue, v1, v2, v3, v4);
        tmp := tmp + inttostr(v1) + '.' + inttostr(v2) + '.' + inttostr(v3) + '.' + inttostr(v4);
        if Recursive then
          tmp := tmp + ' (Includes sub-dirs)';
      end;
      Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, tmp);
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TPluginEditMultipleVersionResource.SetFilename(const Filename: WideString);
begin
  _setFilename(Filename);
end;

function TPluginEditMultipleVersionResource.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TPluginEditMultipleVersionResource.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginEditMultipleVersionResource.Get_ParamValues(const ParamName: WideString): WideString;
var v: array[1..4] of word;
    slist: TStringList;
    I : integer;
begin
  Result := '';
  if Paramname = stdcFilename then
    Result := Filename
  else if Paramname = stdcIncreaseType then
    Result := IntToStr( IncreaseType)
  else if Paramname = stdcFileVersionKey then
    Result := FileVersionKey
  else if Paramname = stdcIncreaseSet then
    Result := IntToStr(IncreaseSet)
  else if Paramname = stdcRecursive then
    Result := BoolToStr(Recursive)
  else if Paramname = stdcNewVersionValue then
  begin
    FSingleVersion.DecodeVersion(NewVersionValue, v[1], v[2], v[3], v[4]);
    slist:= TStringList.Create;
    slist.Delimiter:='.';
    for i := 1 to 4 do
      slist.add(inttostr(v[i]));
    result := slist.DelimitedText;
  end;
end;

procedure TPluginEditMultipleVersionResource.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var v: array[1..4] of word;
    slist: TStringList;
    I : integer;
begin
  TFormEditEditMultipleVersionResourceParams.Create(nil);
  if Paramname = stdcFilename then
    Filename := Value
  else if Paramname = stdcIncreaseType then
    IncreaseType := StrToInt( Value)
  else if Paramname = stdcFileVersionKey then
    FileVersionKey := Value
  else if Paramname = stdcIncreaseSet then
    IncreaseSet := strtoint(value)
  else if Paramname = stdcRecursive then
    Recursive := StrToBool(value)
  else if Paramname = stdcNewVersionValue then
  begin
    slist:= TStringList.Create;
    slist.Delimiter:='.';
    slist.DelimitedText := value;
    for i := 1 to 4 do
      v[i] := strtoint(slist.Strings[i-1]);
    NewVersionValue := FSingleVersion.EncodeVersion(v[1], v[2], v[3], v[4]);
  end;
end;

procedure TPluginEditMultipleVersionResource._setFilename(filename: string);
begin
  if FFilenames = nil then
    FFilenames := TStringList.Create;
  FFilenames.Delimiter := ';';
  FFilenames.DelimitedText := filename;
end;

function  TPluginEditMultipleVersionResource._getFilename:string;
begin
  Result := '';
  if FFilenames <> nil then
    Result := FFilenames.DelimitedText;
end;

function TPluginEditMultipleVersionResource.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0:Result := stdcFilename;
    1:Result := stdcIncreaseType;
    2:Result := stdcFileVersionKey;
    3:Result := stdcIncreaseSet;
    4:Result := stdcNewVersionValue;
    5:Result := stdcRecursive;
  end;
end;

function TPluginEditMultipleVersionResource.Get_ParamCount: Integer;
begin
  Result := 6;
end;

function TPluginEditMultipleVersionResource.Get_OwnerDraw: WordBool;
begin
  //Use Caption and PreviewText!
  //Otherwise, if Result = true, you can use
  //DrawItem and MeasureItem
  Result := false;
end;

function TPluginEditMultipleVersionResource.Notify(const Notification: WideString; Parameter: OleVariant): OleVariant;
begin
  //nothing to do
  //for future purpose - e.g. active language changed
  Result := 0;
end;

function TPluginEditMultipleVersionResource.Get_Properties: IDispatch;
begin
  //nothing to do
  //for future purpose - integration of an property inspector
  //and extended handling of command parameters/properties
  Result := nil;
end;

end.
 
