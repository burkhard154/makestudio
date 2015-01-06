unit PluginMsBuildmodule;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, makestudio_TLB,
  Classes, Windows, Dialogs, Controls, SysUtils;

type
  TPluginTestcommand = class(TComponent, ICommand2, IExecCallback)
  private
    FCaption      : string;
    FOutputPath   : string;
    FSolutionPath : string;
    FBuildConfig  : string;
    //FCleanUpBeforeBuild : boolean;
  protected
    //ICommand Interface
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
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;

    procedure CaptureOutput(const Line: WideString; var Aborted: WordBool); safecall;

    //ICommand2 Interface
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Properties: IDispatch read Get_Properties;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  //Callback to create an instance of the ICommand
  TPluginTestcommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginTestcommandCallback: TPluginTestcommandCallback;

const
  IDPluginTestcommand = 'PluginMsBuild.BuildSolution';

{
  Example code to register the command. 
  To be used in the "RegisterPlugin" funktion of the project file.
  
      //--- add then command: Testcommand
	  // 1. Get the image from an image list
      GetPictureFromImageList(FormActions.ImageList1, 0, P);
	  
	  // 2. Create the global command callback
      PluginTestcommandCallback := TPluginTestcommandCallback.Create(nil);

	  // 3. Register the command itsel
      //Name=Testcommand; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before
      MakeStudio.AddCommandType('Testcommand', 'Your Hint here!', stCategory, P, 'txt', -1,
        ICommandCallback(PluginTestcommandCallback));
  
}  
implementation

uses
  ComServ, PluginMsBuildVars, PluginMsBuildEdit;

function TPluginTestcommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginTestcommand.Create(nil));
end;

procedure TPluginTestcommandCallback.SetCanceled(aCanceled: WordBool);
begin
  FCanceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
end;

constructor TPluginTestcommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption      := 'MSBuild';
  FOutputPath   := '';
  FSolutionPath := '';
  FBuildConfig  := '';
  //FCleanUpBeforeBuild := true;

  // Load global settings from registry
  LoadFromRegistry;
end;

function TPluginTestcommand.EditItem: WordBool;
begin
  Result := False;
  with TFormEditTestcommand.Create(nil) do
  try
    //cbCleanup.Checked   := FCleanUpBeforeBuild;
    tbSolutionPath.Text := FSolutionPath;
    tbOutputPath.Text   := FOutputPath;
    tbBuildConfig.Text  := FBuildConfig;
    if ShowModal = mrOk then
    begin
      //FCleanUpBeforeBuild := cbCleanup.Checked ;
      FSolutionPath       := tbSolutionPath.Text ;
      FOutputPath         := tbOutputPath.Text;
      FBuildConfig        := tbBuildConfig.Text;
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TPluginTestcommand.ExecuteItem: WordBool;
var
  args         : string;
  CallbackIntf : IExecCallback;
begin
  Result    := False;
  FCanceled := False;
  MakeStudio.LogMessage('Executing ' + struPluginName);
  try

    if FileExists(gMSBuildExe) then
    begin
      Self.GetInterface(IExecCallback, CallbackIntf);

      // build solution
      if length(FOutputPath) <= 0 then
      begin
        args := ' /p:Configuration=' + FBuildConfig + ' '+ (FSolutionPath);
      end
      else
      begin
        if not DirectoryExists(FOutputPath) then ForceDirectories(FOutputPath);
        args := ' /p:Configuration=' + FBuildConfig + ' /p:OutputPath='+FOutputPath + ' ' + (FSolutionPath);
      end;

      MakeStudio.LogMessage('Build solution ' + FSolutionPath);
      Result := MakeStudio.ExecCmdLine(gMSBuildExe, args, ExtractFilePath(gMSBuildExe), CallbackIntf) = 0;
    end
    else
      MakeStudio.LogMessage(strErrorCompilerNotFound);
  except
    on E : Exception do MakeStudio.LogMessage('Exception: ' + E.Message);
  end;
end;

function TPluginTestcommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1; //auto
end;

function TPluginTestcommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := True; //auto
end;

procedure TPluginTestcommand.SetFilename(const Filename: WideString);
begin
  //Setting the Filename - used by the host at drag&drop
  //enter your code here
end;

function TPluginTestcommand.Get_Caption: WideString;
begin
  Result := FCaption;
//  if Length(FSolutionPath) > 0 then
//    Result := FCaption + ' - Solution: ' + FSolutionPath;
end;

procedure TPluginTestcommand.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TPluginTestcommand.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, ctOutputPath) then
    Result := FOutputPath;
  if SameText(ParamName, ctSolutionPath) then
    Result := FSolutionPath;
  if SameText(ParamName, ctBuildConfigName) then
    Result := FBuildConfig;
//  if SameText(ParamName, ctCleanUpBeforeBuild) then
//    Result := BoolToStr(FCleanUpBeforeBuild);
end;

procedure TPluginTestcommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, ctOutputPath) then
    FOutputPath := Value;
  if SameText(ParamName, ctSolutionPath) then
    FSolutionPath := Value;
  if SameText(ParamName, ctBuildConfigName) then
    FBuildConfig := Value;
//  if SameText(ParamName, ctCleanUpBeforeBuild) then
//    FCleanUpBeforeBuild := StrToBoolDef(Value, true);
end;

function TPluginTestcommand.Get_ParamNames(Index: Integer): WideString;
begin
  case Index of
    0: Result := ctOutputPath;
    1: Result := ctSolutionPath;
    2: Result := ctBuildConfigName;
    //3: Result := ctCleanUpBeforeBuild
  end;
end;

function TPluginTestcommand.Get_ParamCount: Integer;
begin
  Result := 4;
end;

function TPluginTestcommand.Get_OwnerDraw: WordBool;
begin
  Result := false;
end;

function TPluginTestcommand.Get_PreviewText: WideString;
begin
  Result := 'Solution: '+FSolutionPath + #10#13 + 'Build config: ' +FBuildConfig;
end;

function TPluginTestcommand.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := varEmpty;
end;

function TPluginTestcommand.Get_Properties: IDispatch;
begin
  Result := nil;
end;


function TPluginTestcommandCallback.GetIdentifier: WideString;
begin
  Result := IDPluginTestcommand;
end;

procedure TPluginTestcommand.CaptureOutput(const Line: WideString;
  var Aborted: WordBool);
begin
  Aborted := FCanceled;
  MakeStudio.LogMessage(Line);
end;



end.
