{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: NSIS_NSISProjectCommand.pas

The Initial Developer of the original code (JEDI Make) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/09/10  BSchranz  - new unit
2005/09/12  USchuster - D5 fix and minor style cleaning

------------------------------------------------------------------------------}
unit nsis_nsisProjectCommand;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils, JclSysInfo,
  ShellAPI, JclRegistry, JclFileUtils;

{**** Sample Code to register this command *******
var
 P: Picture;
      //--- add modules --------------------------------------------------------
      GetPictureFromImageList( <ImageList1>, 0, P);

      //Name=Inno Setup Project; Hint, Category
      //Extension=txt (could be more than one extension - separated by ;)
      //no compatibility - module did not exist before

      //Create and register Callback for the command type
      PluginNSISProjectCallback := TPluginNSISProjectCallback.Create(nil);
      MakeStudio.AddCommandType('Inno Setup Project', '', stCategory, P, 'txt', -1,
        ICommandCallback(PluginNSISProjectCallback));
**** End Sample Code  *******}

type
  TPluginNSISProject = class(TComponent, ICommand2, IExecCallback)
  private
    FProjectFile: string;
    FOutputFile: string;
    FOptions: string;
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
    procedure CaptureOutput(const Line: WideString; var Aborted: WordBool); safecall;

    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Properties: IDispatch read Get_Properties;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  //Callback to create an instance of the ICommand
  TPluginNSISProjectCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginNSISProjectCallback: TPluginNSISProjectCallback;

const
  IDPluginNSISProject = 'NSIS.NSIS2.project';

function GetNSISCompiler: string;
procedure LoadNSISScript(aFilename: string);
procedure SetNSISCompiler(Compiler: string);

implementation

uses
  ComServ, NSIS_Vars, NSIS_NSISProjectEdit, Forms;

function GetNSISCompiler: string;
begin
  Result := RegReadStringDef(HKCU, stdcRegKey, stdcRegNSISExe, '');
  if Result = '' then  //not in Registry - use default
    Result := PathAddSeparator(PathAddSeparator(GetProgramFilesFolder)
              + stdcPathNSIS) + stdcRegNSISExe;
end;

procedure SetNSISCompiler(Compiler: string);
begin
  RegWriteString( HKCU, stdcRegKey, stdcRegNSISExe, Compiler);
end;


procedure LoadNSISScript(aFilename: string);
begin
  ShellExecute(Application.Handle, 'OPEN', PChar(aFilename), '',
       PChar(ExtractFilePath(aFilename)), sw_ShowNormal);
end;

{ TPluginNSISProjectCallback }

function TPluginNSISProjectCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TPluginNSISProject.Create(nil));
end;

procedure TPluginNSISProjectCallback.SetCanceled(aCanceled: WordBool);
begin
  FCanceled := aCanceled; //set by the server if the user press "Cancel" oder "Stop"
end;

function TPluginNSISProjectCallback.GetIdentifier: WideString;
begin
  Result := IDPluginNSISProject;
end;

{ TPluginNSISProject }

constructor TPluginNSISProject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProjectFile := '';
  FOutputFile := '';
  FOptions := '';
end;

function TPluginNSISProject.EditItem: WordBool;
begin
  Result := False;
  with TFormEditNSISProjectParams.Create(nil) do
  try
    edFilename.Text := FProjectFile;
    edOutputfilename.Text := FOutputFile;
    if ShowModal = mrOk then
    begin
      FProjectFile := StringReplace( edFilename.Text, '"', '', [ rfReplaceAll, rfIgnoreCase]);
      FOutputFile := StringReplace( edOutputfilename.Text, '"', '', [ rfReplaceAll, rfIgnoreCase]);
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TPluginNSISProject.ExecuteItem: WordBool;
var
  S, s1, s2: string;
  CallbackIntf: IExecCallback;
begin
  FCanceled := False;
  Result := False;
  MakeStudio.LogMessage('*********************************************************');
  MakeStudio.LogMessage(strExecutingProject);
  MakeStudio.LogMessage(FProjectFile);
  MakeStudio.LogMessage('');

  //build command line
  S := '"'+ MakeStudio.Variables.ReplaceVarsInString(FProjectFile) +'"';
{  if FOutputFile <> '' then
  begin
    s1 := MakeStudio.Variables.ReplaceVarsInString(FOutputFile);
    S := S + ' "/f' + ExtractFileName(s1) + '"' + ' "/o' +
         ExtractFilePath(s1) + '"';
    ForceDirectories(ExtractFilePath(s1));
  end;
  if FOptions <> '' then
    S := S + ' ' + MakeStudio.Variables.ReplaceVarsInString(FOptions);}

  if FileExists(GetNSISCompiler) then
  begin
    Self.GetInterface(IExecCallback, CallbackIntf);
    Result := MakeStudio.ExecCmdLine(GetNSISCompiler, S,
              ExtractFilePath(GetNSISCompiler), CallbackIntf) = 0;


    if Result then begin
      if FOutputFile <> '' then begin
        s1 := ChangeFileExt( MakeStudio.Variables.ReplaceVarsInString(FOutputFile), '.exe');
        s2 := ChangeFileExt( MakeStudio.Variables.ReplaceVarsInString(FProjectFile), '.exe');
        if not SameText( s1, s2) then begin
          if FileExists( s1) then begin
            FileSetAttr( s1, faArchive);
            DeleteFile( s1);
          end
          else
            ForceDirectories(ExtractFilePath(s1));
          FileCopy( s2, s1, true);
        end;
      end;
    end;
  end
  else
    MakeStudio.LogMessage(strErrorCompilerNotFound);
end;

function TPluginNSISProject.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1; //auto
end;

function TPluginNSISProject.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := True; //auto
end;

procedure TPluginNSISProject.SetFilename(const Filename: WideString);
begin
  FProjectFile := Filename;
end;

function TPluginNSISProject.Get_Caption: WideString;
begin
  Result := Format(strNSISProject, [FProjectFile]);
end;

procedure TPluginNSISProject.Set_Caption(const Value: WideString);
begin
  //nothing
end;

function TPluginNSISProject.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
  if SameText(ParamName, sProjectFile) then
    Result := FProjectFile;
  if SameText(ParamName, sOutputFile) then
    Result := FOutputFile;
  if SameText(ParamName, sOptions) then
    Result := FOptions;
end;

procedure TPluginNSISProject.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if SameText(ParamName, sProjectFile) then
    FProjectFile := Value;
  if SameText(ParamName, sOutputFile) then
    FOutputFile := Value;
  if SameText(ParamName, sOptions) then
    FOptions := Value;
end;

function TPluginNSISProject.Get_ParamNames(Index: Integer): WideString;
begin
  case Index of
    0: Result := sProjectFile;
    1: Result := sOutputFile;
    2: Result := sOptions;
  end;
end;

function TPluginNSISProject.Get_ParamCount: Integer;
begin
  Result := 3;
end;

function TPluginNSISProject.Get_OwnerDraw: WordBool;
begin
  Result := False;
end;

function TPluginNSISProject.Get_PreviewText: WideString;
begin
  Result := Format(strPreviewText, [FOutputFile, FOptions]);
end;

function TPluginNSISProject.Notify(const Notification: WideString; Parameter: OleVariant): OleVariant;
begin
  //nothing to do
  //for future purpose - e.g. active language changed
  Result := 0;
end;

function TPluginNSISProject.Get_Properties: IDispatch;
begin
  //nothing to do
  //for future purpose - integration of an property inspector
  //and extended handling of command parameters/properties
  Result := nil;
end;

procedure TPluginNSISProject.CaptureOutput(const Line: WideString;
  var Aborted: WordBool);
begin
  Aborted := FCanceled;
  MakeStudio.LogMessage(Line);
end;

end.
