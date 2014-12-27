{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsplugintemplate_Module.pas

The Initial Developer of the original code (JEDI Make) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2006/04/21  BSchranz  - Plugin ZIP created

------------------------------------------------------------------------------}
unit zip_Command;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, makestudio_TLB,
  Classes, Windows, Dialogs, Controls, SysUtils, zip_Vars, zip_utils,
  TypInfo, JclStrings, JclFileUtils, zipmstr;


type
  TPluginZipCommand = class(TComponent, ICommand2)
  private
    FZIPAction: TZIPAction;
    FParams: TStringList;
    FZIPFilename: String;
    FVerbose: Boolean;
    FTrace: Boolean;
    FUpdateZip : Boolean;

    function UnzipToFolder:Integer;
    function ZipFolder:Integer;
    function ZipWildcardRecourse:Integer;
    function ZipWildcard:Integer;
    function CheckRootDir(Dirs: TStrings; ContainsWildcard: Boolean): String;
    procedure CheckDeleteZipFile;
  protected
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
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;

    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Properties: IDispatch read Get_Properties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //ZipMaster Events
    procedure OnZIPMessage( Sender: TObject; ErrCode: Integer; const ErrMsg: String);
  published
    property ZIPAction:TZIPAction read FZIPAction write FZIPAction;
    property ZIPFilename:String read FZIPFilename write FZIPFilename;
    property Params:TStringList read FParams write FParams;
    property Verbose:Boolean read FVerbose write FVerbose;
    property Trace:Boolean read FTrace write FTrace;
    property UpdateZip:Boolean read FUpdateZip write FUpdateZip;
  end;

  //Callback to create an instance of the ICommand
  TPluginZipCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginWinZipCommandCallback: TPluginZipCommandCallback;

const
  IDPluginWinZipCommand = 'Zip.Command';

implementation

uses
  ComServ, zip_CommandEdit;

resourcestring
  StrExecutingZipComman = 'Executing Zip Command...';
  StrBreak = '****************************************************';
  StrNoFolderToExtract = 'No folder to extract...';
  StrExtractingTo = 'Extracting to: ';
  StrNoFolderToZip = 'ERROR: No Folder to ZIP spezified!';
  StrAddingFolderRecourse = 'Adding Folder (recourse): ';
  StrAddingFolder = 'Adding Folder: ';
  StrErrorDeletingZipFile = 'Error deleting ZIP file: ';
  StrZipFileDeleted = 'ZIP file deleted: ';

{ TPluginWinZipCommandCallback }

function TPluginZipCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginZipCommand.Create(nil));
end;

procedure TPluginZipCommandCallback.SetCanceled(aCanceled: WordBool);
begin
  FCanceled := True; //set by the server if the user press "Cancel" oder "Stop"
end;

function TPluginZipCommandCallback.GetIdentifier: WideString;
begin
  Result := IDPluginWinZipCommand;
end;

{ TPluginWinZipCommand }

constructor TPluginZipCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FZIPAction := tzaUnzipToFolder;
  FParams := TStringList.Create;
  FZIPFilename := '';
  FTrace := False;
  FVerbose := True;
end;

function TPluginZipCommand.ZipWildcard:Integer;
var i:Integer;
begin
  ZipMaster.FSpecArgsExcl.Clear;
  ZipMaster.FSpecArgs.Clear;

  ZipMaster.ZipFilename := MakeStudio.Variables.ReplaceVarsInString( ZipFilename);

  if Params.Count>0 then begin
    ZipMaster.AddOptions := [ AddDirNames];
    ZIPMaster.FSpecArgs.Text := MakeStudio.Variables.ReplaceVarsInString( Params.Text);
    ZipMaster.RootDir := CheckRootDir( ZipMaster.FSpecArgs, True);

    for i:=0 to ZipMaster.FSpecArgs.Count-1 do
      MakeStudio.LogMessage( StrAddingFolder + ZipMaster.FSpecArgs[i]);

    CheckDeleteZipFile;
    Result := ZipMaster.Add;
  end
  else begin
    MakeStudio.LogMessage( StrNoFolderToZip);
    Result := -1;
  end;
end;

function TPluginZipCommand.CheckRootDir(Dirs: TStrings;
  ContainsWildcard: Boolean): String;
var
  i: Integer;
begin

  // Check if there is a common root directory
  if Dirs.Count > 0 then
  begin

    if ContainsWildcard then
      Result := PathRemoveSeparator(ExtractFilePath(Dirs[0]))
    else
      Result := PathRemoveSeparator(Dirs[0]);

    i := 0;
    while (Result <> '') and (i < Dirs.Count) do
      if Pos(UpperCase(Result), UpperCase(Dirs[i])) = 0 then
        Result := PathRemoveSeparator(ExtractFilePath(Dirs[i]))
      else
        inc(i);
  end
  else
    Result := '';

  // if there is a common root directory, replace this folder in "Dirs"
  if Result <> '' then
  begin
    for i := 0 to Dirs.Count - 1 do
    begin
      if Pos(UpperCase(Result), UpperCase(Dirs[i])) = 1 then
      begin
        Dirs[i] := Copy(Dirs[i], Length(Result)+1, Length(Dirs[i]) - length(Result));
        if Dirs[i][1] = '\' then
          Dirs[i] := copy(Dirs[i], 2, length(Dirs[i]) -1);
      end;
    end;
  end;
end;

function TPluginZipCommand.ZipWildcardRecourse:Integer;
var i:Integer;
begin
  ZipMaster.FSpecArgsExcl.Clear;
  ZipMaster.FSpecArgs.Clear;

  ZipMaster.ZipFilename := MakeStudio.Variables.ReplaceVarsInString( ZipFilename);

  if Params.Count>0 then begin
    ZipMaster.AddOptions := [ AddDirNames, AddRecurseDirs];
    ZIPMaster.FSpecArgs.Text := MakeStudio.Variables.ReplaceVarsInString( Params.Text);
    ZipMaster.RootDir := CheckRootDir( ZipMaster.FSpecArgs, True);

    for i:=0 to ZipMaster.FSpecArgs.Count-1 do
      MakeStudio.LogMessage( StrAddingFolderRecourse + ZipMaster.FSpecArgs[i]);

    CheckDeleteZipFile;
    Result := ZipMaster.Add;
  end
  else begin
    MakeStudio.LogMessage( StrNoFolderToZip);
    Result := -1;
  end;
end;

procedure TPluginZipCommand.CheckDeleteZipFile;
begin
  if not UpdateZip then
    if FileExists( MakeStudio.Variables.ReplaceVarsInString( ZipFilename)) then begin
      FileSetAttr( MakeStudio.Variables.ReplaceVarsInString( ZipFilename), faArchive);
      if not DeleteFile( MakeStudio.Variables.ReplaceVarsInString( ZipFilename)) then
        MakeStudio.LogMessage( StrErrorDeletingZipFile + MakeStudio.Variables.ReplaceVarsInString( ZipFilename))
      else
        MakeStudio.LogMessage( StrZipFileDeleted + MakeStudio.Variables.ReplaceVarsInString( ZipFilename));
    end;
end;

function TPluginZipCommand.ZipFolder:Integer;
begin
  ZipMaster.FSpecArgsExcl.Clear;
  ZipMaster.FSpecArgs.Clear;

  ZipMaster.ZipFilename := MakeStudio.Variables.ReplaceVarsInString( ZipFilename);

  if Params.Count>0 then begin
    ZipMaster.AddOptions := [ AddDirNames, AddRecurseDirs, AddEmptyDirs];
    ZipMaster.RootDir := PathRemoveSeparator( MakeStudio.Variables.ReplaceVarsInString( Params[0]));
    ZipMaster.FSpecArgs.Add( PathAddSeparator( MakeStudio.Variables.ReplaceVarsInString( Params[0])) + '*.*');
    ZipMaster.RootDir := CheckRootDir( ZipMaster.FSpecArgs, True);
    MakeStudio.LogMessage( StrAddingFolderRecourse + ZipMaster.FSpecArgs[0]);

    CheckDeleteZipFile;
    Result := ZipMaster.Add;
  end
  else begin
    MakeStudio.LogMessage( StrNoFolderToZip);
    Result := -1;
  end;
end;

function TPluginZipCommand.UnzipToFolder:Integer;
begin
  ZipMaster.FSpecArgsExcl.Clear;
  ZipMaster.FSpecArgs.Clear;

  ZipMaster.ZipFilename := MakeStudio.Variables.ReplaceVarsInString( ZipFilename);

  if Params.Count>0 then begin
    ZipMaster.ExtrBaseDir := MakeStudio.Variables.ReplaceVarsInString( PathAddSeparator( Params[ 0]));
    ZipMaster.ExtrOptions := [ ExtrDirNames, ExtrOverWrite, ExtrForceDirs];
    MakeStudio.LogMessage( StrExtractingTo + ZipMaster.ExtrBaseDir);
    Result := ZipMaster.Extract;
  end
  else begin
    MakeStudio.LogMessage( StrNoFolderToExtract);
    Result := -1;
  end;
end;

function TPluginZipCommand.EditItem: WordBool;
var i:Integer;
begin
  Result := False;
  with TFormEditZipCommand.Create(nil) do
  try
    cbAction.ItemIndex := ord( ZIPAction);
    edZIPFilename.FileName := ZIPFilename;
    edParams.Lines.Assign( Params);
    cbVerbose.Checked := Verbose;
    cbTrace.Checked := Trace;
    if not UpdateZip then
      rgZipUpdate.ItemIndex := 0
    else
      rgZipUpdate.ItemIndex := 1;

    if ShowModal = mrOk then
    begin
      ZIPAction := TZIPAction( cbAction.ItemIndex);
      ZIPFilename := edZIPFilename.FileName;
      Params.Assign( edParams.Lines);

      //Clean up Params
      i := 0; while i<Params.Count do if Params[i] = '' then Params.Delete( i) else inc( i);

      Verbose := cbVerbose.Checked;
      Trace := cbTrace.Checked;
      UpdateZip := rgZipUpdate.ItemIndex = 1;
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TPluginZipCommand.ExecuteItem: WordBool;
begin
  FCanceled := False;
  MakeStudio.LogMessage( StrBreak);
  MakeStudio.LogMessage( StrExecutingZipComman);
  MakeStudio.LogMessage( MakeStudio.Variables.ReplaceVarsInString( Caption));

  //Setup ZipMaster
  ZipMaster.OnMessage := OnZIPMessage;

  case ZipAction of
    tzaUnzipToFolder : UnzipToFolder;
    tzaZipFolder : ZipFolder;
    tzaZipWildcardRecurse : ZipWildcardRecourse;
    tzaZipWildcard : ZipWildcard;
  end;

  ZipMaster.OnMessage := nil;
  Result := (ZipMaster.ErrCode=0) and not(ZipMaster.Cancel);
end;

function TPluginZipCommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1;
end;

function TPluginZipCommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := False;
end;

procedure TPluginZipCommand.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginZipCommand.Get_Caption: WideString;
begin
  Result := ZipActionStrings[ ZIPAction] + ' <' + ZipFilename + '>';
end;

procedure TPluginZipCommand.Set_Caption(const Value: WideString);
begin
  //
end;

function TPluginZipCommand.Get_ParamValues(const ParamName: WideString): WideString;
var
  I: Integer;
begin
  if ParamName = stdFilename then
    Result := ZIPFilename
  else
  if ParamName = stdAction then
    Result := GetEnumProp( self, 'ZIPAction')
  else
  if ParamName = stdVerbose then
    Result := BoolToStr( Verbose, true)
  else
  if ParamName = stdTrace then
    Result := BoolToStr( Trace, true)
  else
  if ParamName = stdUpdate then
    Result := BoolToStr( UpdateZIP, true)
  else
  if ParamName = stdCount then
  begin
    Result := IntToStr( Params.Count);
  end
  else
  begin
    for I := 0 to Params.Count-1 do
      if SameText( Format(stdParam, [I + 1]), ParamName) then
      begin
        Result := Params[I];
        Break;
      end;
  end;
end;

procedure TPluginZipCommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var
  I: Integer;
begin
  if ParamName = stdFilename then
    ZIPFilename := Value
  else
  if ParamName = stdAction then
    SetEnumProp( self, 'ZIPAction', Value)
  else
  if ParamName = stdVerbose then
    Verbose := StrToBoolDef( Value, True)
  else
  if ParamName = stdTrace then
    Trace := StrToBoolDef( Value, True)
  else
  if ParamName = stdUpdate then
    UpdateZip := StrToBoolDef( Value, True)
  else
  if ParamName = stdCount then
  begin
    Params.Clear;
    for I:=0 to StrToInt(Value)-1 do
      Params.Add('');
  end
  else
  begin
    for I := 0 to Params.Count - 1 do
      if SameText( Format( stdParam, [I + 1]), ParamName) then
      begin
        Params[I] := Value;
        Break;
      end;
  end;
end;

function TPluginZipCommand.Get_ParamNames(Index: Integer): WideString;
begin
  case Index of
    0: Result := stdFilename;
    1: Result := stdAction;
    2: Result := stdVerbose;
    3: Result := stdTrace;
    4: Result := stdCount;
    5: Result := stdUpdate;
    else
    begin
      Result := Format( stdParam, [Index - 5]);
    end;
  end;
end;

function TPluginZipCommand.Get_ParamCount: Integer;
begin
  Result := 6 + Params.Count;
end;

function TPluginZipCommand.Get_OwnerDraw: WordBool;
begin
  //Use Caption and PreviewText!
  //Otherwise, if Result = true, you can use
  //DrawItem and MeasureItem
  Result := false;
end;

function TPluginZipCommand.Get_PreviewText: WideString;
begin
  Result := Params.Text;
end;

function TPluginZipCommand.Notify(const Notification: WideString; Parameter: OleVariant): OleVariant;
begin
  //nothing to do
  //for future purpose - e.g. active language changed
  Result := 0;
end;

function TPluginZipCommand.Get_Properties: IDispatch;
begin
  //nothing to do
  //for future purpose - integration of an property inspector
  //and extended handling of command parameters/properties
  Result := nil;
end;

destructor TPluginZipCommand.Destroy;
begin
  FParams.Free;

  inherited;
end;

procedure TPluginZipCommand.OnZIPMessage(Sender: TObject; ErrCode: Integer;
  const ErrMsg: String);
begin
  if ErrCode>0 then
    MakeStudio.LogMessage( strError);

  MakeStudio.LogMessage( ErrMsg);

  if FCanceled then
    ZipMaster.Cancel := true;
end;

end.
