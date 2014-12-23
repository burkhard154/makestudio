(* -----------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: resources_JVCSMakPlugin.dpr

  The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

  Componentes and used code which is used in this code are explictly stated to
  be copyright of the respective author(s).

  Last Modified: see History

  Known Issues:
  -----------------------------------------------------------------------------

  Unit history:

  2005/08/13  BSchranz  - Plugin created
  2005/08/16  USchuster - fixed compilation over makefile
  2005/09/09  BSchranz  - translation bug fixed
  2005/11/12  USchuster - reintegrated changes from revision 0.2(2005/08/16)
  2006/04/11  BSchranz  - Version also set to "FileVersion" Key

  ----------------------------------------------------------------------------- *)
unit resources_Module;

{$I jedi.inc}
{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils;

type
  TPluginVersionsresourcenndern = class(TComponent, ICommand2)
  private
    FCaption: string;
    FFilename: String;
    FIncreaseSet: integer;
    FNewVersionValue: TULargeInteger;
    FIncreaseType: integer;
    FFileVersionKey: String;

    procedure _SetFilename(Value: String);
  protected
    function MeasureItem(Handle: integer; BriefView: WordBool): integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: integer; Left: integer; Top: integer; Right: integer; Bottom: integer;
      Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString; const Value: WideString); safecall;
    function Get_ParamNames(Index: integer): WideString; safecall;
    function Get_ParamCount: integer; safecall;
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues
      write Set_ParamValues;
    property ParamNames[Index: integer]: WideString read Get_ParamNames;
    property ParamCount: integer read Get_ParamCount;

    function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;

  public
    constructor Create(AOwner: TComponent); override;
    // Function will be used by the MultpileVersionCommand
    function ProceedWin32Res: Boolean;
    function EncodeVersion(v1, v2, v3, v4: Word): TULargeInteger;
    procedure DecodeVersion(Version: TULargeInteger; var v1, v2, v3, v4: Word);

    property Filename: String read FFilename write _SetFilename;
    property IncreaseType: integer read FIncreaseType write FIncreaseType;
    property FileVersionKey: String read FFileVersionKey write FFileVersionKey;
    property IncreaseSet: integer read FIncreaseSet write FIncreaseSet;
    property NewVersionValue: TULargeInteger read FNewVersionValue write FNewVersionValue;
  end;

  // Callback to create an instance of the ICommand
  TPluginVersionsresourcenndernCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginVersionsresourcenndernCallback: TPluginVersionsresourcenndernCallback;

const
  IDPluginVersionsresourcenndern = 'resources.EditVersion';

implementation

uses
  ComServ, resources_Vars, resources_Edit,
  unitResFile, unitRCFile, unitResourceDetails,
  unitResourceVersionInfo, unitResourceExaminer,
  SimpleRCVersionInfo;

function TPluginVersionsresourcenndernCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginVersionsresourcenndern.Create(nil));
end;

procedure TPluginVersionsresourcenndernCallback.SetCanceled(aCanceled: WordBool);
begin
  FCanceled := True; // set by the server if the user press "Cancel" oder "Stop"
end;

constructor TPluginVersionsresourcenndern.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := strVersionCommandCaption;
  FFileVersionKey := 'FileVersion';
  FIncreaseType := 3;
  FIncreaseSet := 1;
end;

function TPluginVersionsresourcenndern.EditItem: WordBool;
var
  v1, v2, v3, v4: Word;
begin
  Result := False;
  with TFormEditParams.Create(nil) do
    try
      rgType.ItemIndex := IncreaseType;
      edFilename.Text := Filename;
      edVersionKey.Text := FileVersionKey;

      rgIncSet.ItemIndex := IncreaseSet;
      DecodeVersion(NewVersionValue, v1, v2, v3, v4);
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
        NewVersionValue := EncodeVersion(v1, v2, v3, v4);
        IncreaseType := rgType.ItemIndex;
        Filename := edFilename.Text;
        FileVersionKey := edVersionKey.Text;
      end;
    finally
      Free;
    end;
end;

function TPluginVersionsresourcenndern.ExecuteItem: WordBool;
begin
  FCanceled := False;
  // jvcsmak.LogMessage('Executing Versionsresourcen ändern...');
  Result := ProceedWin32Res;
end;

function TPluginVersionsresourcenndern.MeasureItem(Handle: integer; BriefView: WordBool): integer;
var
  Canvas: TCanvas;
begin
  // ----------------------------- Example ------------------------
  Result := -1;
  Exit;
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

function TPluginVersionsresourcenndern.DrawItem(Handle: integer; Left: integer; Top: integer;
  Right: integer; Bottom: integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR)
  : WordBool;
var
  Offset: integer;
  Canvas: TCanvas;
  aRect: TRect;
  v1, v2, v3, v4: Word;
  tmp: string;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  // ----------------------------- Example ------------------------
  Exit;
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
    Canvas.TextOut(aRect.Left + iDefaultIndent, aRect.Top + Offset, FCaption);
    Offset := Offset + Canvas.TextHeight(FCaption) + 2;

    if not BriefView then
    begin
      Canvas.Font.Style := [fsBold];
      SetCanvasTextColor(clBlue);
      Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset, Filename);
      Offset := Offset + Canvas.TextHeight(Filename) + 2;

      Canvas.Font.Style := [];
      SetCanvasTextColor(clMaroon);
      tmp := stSetTo + ' ';
      if IncreaseSet = 1 then
        case IncreaseType of
          0:
            tmp := tmp + stMainVersion;
          1:
            tmp := tmp + stReleaseVersion;
          2:
            tmp := tmp + stIssueVersion;
          3:
            tmp := tmp + stBuildVersion;
        end
      else
      begin
        DecodeVersion(NewVersionValue, v1, v2, v3, v4);
        tmp := tmp + inttostr(v1) + '.' + inttostr(v2) + '.' + inttostr(v3) + '.' + inttostr(v4);
      end;
      Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset, tmp);
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TPluginVersionsresourcenndern.SetFilename(const Filename: WideString);
begin
  _SetFilename(Filename);
end;

function TPluginVersionsresourcenndern.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TPluginVersionsresourcenndern.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginVersionsresourcenndern.Get_ParamValues(const ParamName: WideString): WideString;
var
  v: array [1 .. 4] of Word;
  slist: TStringList;
  I: integer;
begin
  if ParamName = stdcFilename then
    Result := Filename
  else if ParamName = stdcIncreaseType then
    Result := inttostr(IncreaseType)
  else if ParamName = stdcFileVersionKey then
    Result := FileVersionKey
  else if ParamName = stdcIncreaseSet then
    Result := inttostr(IncreaseSet)
  else if ParamName = stdcNewVersionValue then
  begin
    DecodeVersion(NewVersionValue, v[1], v[2], v[3], v[4]);
    slist := TStringList.Create;
    try
      slist.Delimiter := '.';
      for I := 1 to 4 do
        slist.add(inttostr(v[I]));
      Result := slist.DelimitedText;
    finally
      slist.Free;
    end;
  end;
end;

procedure TPluginVersionsresourcenndern.Set_ParamValues(const ParamName: WideString;
  const Value: WideString);
var
  v: array [1 .. 4] of Word;
  slist: TStringList;
  I: integer;
begin
  if ParamName = stdcFilename then
    Filename := Value
  else if ParamName = stdcIncreaseType then
    IncreaseType := StrToInt(Value)
  else if ParamName = stdcFileVersionKey then
    FileVersionKey := Value
  else if ParamName = stdcIncreaseSet then
    IncreaseSet := StrToInt(Value)
  else if ParamName = stdcNewVersionValue then
  begin
    slist := TStringList.Create;
    try
      slist.Delimiter := '.';
      slist.DelimitedText := Value;
      for I := 1 to 4 do
        v[I] := StrToInt(slist.Strings[I - 1]);
      NewVersionValue := EncodeVersion(v[1], v[2], v[3], v[4]);
    finally
      slist.Free;
    end;
  end;
end;

function TPluginVersionsresourcenndern.Get_ParamNames(Index: integer): WideString;
begin
  Result := '';
  case Index of
    0:
      Result := stdcFilename;
    1:
      Result := stdcIncreaseType;
    2:
      Result := stdcFileVersionKey;
    3:
      Result := stdcIncreaseSet;
    4:
      Result := stdcNewVersionValue;
  end;
end;

function TPluginVersionsresourcenndern.Get_ParamCount: integer;
begin
  Result := 5;
end;

function TPluginVersionsresourcenndernCallback.GetIdentifier: WideString;
begin
  Result := IDPluginVersionsresourcenndern;
end;

procedure TPluginVersionsresourcenndern.DecodeVersion(Version: TULargeInteger;
  var v1, v2, v3, v4: Word);
begin
  with Version do
  begin
    v1 := HiWord(HighPart);
    v2 := LoWord(HighPart);
    v3 := HiWord(LowPart);
    v4 := LoWord(LowPart);
  end;
end;

function TPluginVersionsresourcenndern.EncodeVersion(v1, v2, v3, v4: Word): TULargeInteger;
begin
  Result.HighPart := (v1 shl 16) + v2;
  Result.LowPart := (v3 shl 16) + v4;
end;

function TPluginVersionsresourcenndern.ProceedWin32Res: Boolean;
var
  ResourceModule: TResourceModule;
  RCVersion: TRCVersionInfo;

  function OpenFile: Boolean;
  var
    ext: string;
    f:String;
  begin
    Result := False;
    f := jvcsmak.Variables.ReplaceVarsInString(Filename);
    ext := UpperCase(ExtractFileExt(f));
    if (ext = '.RC') then
      RCVersion := TRCVersionInfo.Create
    else if (ext = '.RES') or (ext = '.DCR') then
      ResourceModule := TResModule.Create
    else
      jvcsmak.LogMessage(stWrongFiletype);

    try
      if Assigned(ResourceModule) then
        ResourceModule.LoadFromFile(f);
      if Assigned(RCVersion) then
        RCVersion.LoadFromFile(f);
      Result := True;
    except
      on E: Exception do
      begin
        if ResourceModule <> nil then
          FreeAndNil(ResourceModule);
        if Assigned(RCVersion) then
          FreeAndNil(RCVersion);
        jvcsmak.LogMessage(E.Message);
        jvcsmak.LogMessage(Format(stErrorLoadingFile, [f]));
      end;
    end;

  end;

  procedure IncreaseVersions(var v1, v2, v3, v4: Word);
  begin
    case FIncreaseType of
      0:
        inc(v1);
      1:
        inc(v2);
      2:
        inc(v3);
      3:
        inc(v4);
    end;
    // setting all values following the incremented value to zero
    if FIncreaseType <= 2 then
      v4 := 0;
    if FIncreaseType <= 1 then
      v3 := 0;
    if FIncreaseType <= 0 then
      v2 := 0;
  end;

  function DoProceed: Boolean;
  var
    details: TResourceDetails;
    v1, v2, v3, v4, _v1, _v2, _v3, _v4: Word;
    lang: String;
    j, k: integer;
  begin
    Result := False;

    // RC
    if Assigned(RCVersion) then
    begin

      with RCVersion.GetIntFileVersion do
      begin
        v1 := MainV;
        v2 := SubV;
        v3 := Release;
        v4 := Build;
        _v1 := MainV;
        _v2 := SubV;
        _v3 := Release;
        _v4 := Build;
      end;

      if IncreaseSet = 1 then
        IncreaseVersions(v1, v2, v3, v4)
      else
        DecodeVersion(NewVersionValue, v1, v2, v3, v4);

      RCVersion.SetIntFileVersion( v1, v2, v3, v4);
      RCVersion.SetIntProductVersion( v1, v2, v3, v4);
      jvcsmak.LogMessage( RCVersion.InnerText.Text);
      jvcsmak.LogMessage(Format(stVersionModified, [_v1, _v2, _v3, _v4, v1, v2, v3, v4]));
      Result := True;
    end

    // RES
    else if Assigned(ResourceModule) then
    begin
      for j := 0 to ResourceModule.ResourceCount - 1 do
      begin
        details := ResourceModule.ResourceDetails[j];

        if details is TVersionInfoResourceDetails then
        begin
          Result := True;
          DecodeVersion(TVersionInfoResourceDetails(details).ProductVersion, v1, v2, v3, v4);
          if IncreaseSet = 1 then
          begin
            _v1 := v1;
            _v2 := v2;
            _v3 := v3;
            _v4 := v4;
            IncreaseVersions(_v1, _v2, _v3, _v4);
          end
          else
            DecodeVersion(NewVersionValue, _v1, _v2, _v3, _v4);

          TVersionInfoResourceDetails(details).ProductVersion := EncodeVersion(_v1, _v2, _v3, _v4);
          TVersionInfoResourceDetails(details).FileVersion := EncodeVersion(_v1, _v2, _v3, _v4);

          if details.ResourceLanguage = 0 then
            lang := stLanguageNutral
          else
            lang := Languages.NameFromLocaleID[details.ResourceLanguage];

          k := TVersionInfoResourceDetails(details).IndexOf(FFileVersionKey);
          if k < 0 then
            jvcsmak.LogMessage(Format(stVersionKeyNotFound, [FFileVersionKey]))
          else
          begin
            TVersionInfoResourceDetails(details).SetKeyValue(FFileVersionKey,
              Format('%d.%d.%d.%d', [_v1, _v2, _v3, _v4]));
          end;

          jvcsmak.LogMessage(Format(stLanguage, [lang]));
          jvcsmak.LogMessage(Format(stVersionModified, [v1, v2, v3, v4, _v1, _v2, _v3, _v4]));

          for k := 0 to TVersionInfoResourceDetails(details).KeyCount - 1 do
            jvcsmak.LogMessage(Format('Key %s = %s',
              [TVersionInfoResourceDetails(details).Key[k].KeyName,
              TVersionInfoResourceDetails(details).Key[k].Value]));

        end;
      end;
    end;
    if not Result then
      jvcsmak.LogMessage(Format(stVersionResourceNotFound, [jvcsmak.Variables.ReplaceVarsInString(Filename)]));
  end;

var f:String;
begin
  Result := False;
  ResourceModule := nil;
  RCVersion := nil;
  f := jvcsmak.Variables.ReplaceVarsInString(Filename);
  jvcsmak.LogMessage('');
  jvcsmak.LogMessage(stProceedingWin32VersionResource);
  jvcsmak.LogMessage(Format(stProceedingFile, [f]));

  try
    if OpenFile then
      if DoProceed then
        try
          if Assigned(ResourceModule) then
            ResourceModule.SaveToFile(f);
          if Assigned(RCVersion) then
            RCVersion.SaveToFile(f);
          Result := True;
        except
          jvcsmak.LogMessage(Format(stErrorSavingFile, [f]));
        end;
  finally
    if Assigned(ResourceModule) then
      ResourceModule.Free;
    if Assigned(RCVersion) then
      RCVersion.Free;
  end;
  jvcsmak.LogMessage('');
end;

procedure TPluginVersionsresourcenndern._SetFilename(Value: String);
var
  s: String;
begin
  s := StringReplace(Value, '"', '', [rfReplaceAll]);
  if SameText(ExtractFileExt(s), '.res') or SameText(ExtractFileExt(s), '.dcr') or
    SameText(ExtractFileExt(s), '.rc') then
    FFilename := s;
end;

function TPluginVersionsresourcenndern.Get_Properties: IDispatch;
begin
  Result := nil;
end;

function TPluginVersionsresourcenndern.Notify(const Notification: WideString; Parameter: OleVariant)
  : OleVariant;
begin
  Result := '';
end;

function TPluginVersionsresourcenndern.Get_PreviewText: WideString;
var
  s: String;
  v1, v2, v3, v4: Word;
begin
  Result := '';
  if IncreaseSet = 0 then
  begin
    DecodeVersion(NewVersionValue, v1, v2, v3, v4);
    s := Format(stSetVersion, [v1, v2, v3, v4]);
  end
  else
  begin
    case FIncreaseType of
      0:
        s := stMainVersion;
      1:
        s := stReleaseVersion;
      2:
        s := stIssueVersion;
      3:
        s := stBuildVersion;
    end;
    s := Format(stIncreaseVersion, [s]);
  end;

  Result := s + #10#13 + Format(stModuleName, [Filename]);
end;

function TPluginVersionsresourcenndern.Get_OwnerDraw: WordBool;
begin
  Result := True;
end;

end.
