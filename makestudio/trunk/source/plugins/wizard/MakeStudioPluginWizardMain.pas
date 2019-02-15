(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MakeStudioPluginWizardMain.pas

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
Mar/05
- USc: C# Builder wizard not yet finished
-----------------------------------------------------------------------------

Unit history:

2005/02/15  USchuster - new unit
2005/03/09  USchuster - changes for C# Builder wizard
2005/03/12  USchuster - now use MakeStudioPluginWizardCommon.pas for common stuff

-----------------------------------------------------------------------------*)

unit MakeStudioPluginWizardMain;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, Classes, Windows, Controls, Forms, ToolsAPI, ComObj, msPluginWizardOptions,
  ActnList, Graphics, msPluginWizardCommon;

type
  TMakeStudioCustomPluginWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
    IOTAProjectWizard)
  private
    FParameterList: TStringList;
    FCommandoBitmap: TBitmap;
    FActionBitmap: TBitmap;
    procedure HandleActionFormCreated(const FormEditor: IOTAFormEditor);
  protected
    FWizardKind: TMakeStudioPluginWizardKind;
    FIDPostFix: string;
    constructor Create;
  public
    destructor Destroy; override;
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: {$IFDEF DELPHI6_UP}Cardinal {$ELSE}HICON {$ENDIF};
  end;

  {$IFDEF IDE_SUPPORTS_DELPHI}
  TMakeStudioDxWin32VCLPluginWizard = class(TMakeStudioCustomPluginWizard)
  public
    constructor Create;
  end;
  {$ENDIF IDE_SUPPORTS_DELPHI}

  {$IFDEF IDE_SUPPORTS_CSHARP}
  TMakeStudioCSharpPluginWizard = class(TMakeStudioCustomPluginWizard, IOTARepositoryWizard60,
    IOTARepositoryWizard80)
  public
    constructor Create;
    //IOTARepositoryWizard60
    function GetDesigner: string;
    //IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
  end;
  {$ENDIF IDE_SUPPORTS_CSHARP}

implementation

uses
  JclStrings, JclFileUtils;

{$R plugintemplate.res}
{$IFDEF IDE_SUPPORTS_DELPHI}
{$R plugintemplate_delphi.res}
{$ENDIF IDE_SUPPORTS_DELPHI}
{$IFDEF IDE_SUPPORTS_CSHARP}
{$R plugintemplate_csharp.res}
{$ENDIF IDE_SUPPORTS_CSHARP}

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

function IsDirectiveStr(ASubStr: string; S: string): Boolean;
begin
  Result := SameText(Copy(S, 3, Length(ASubStr)), ASubStr);
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
  else
  if IsDirectiveStr('IFNDEF ', ADirective) then
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
  else
  if IsDirectiveStr('IFNDEF ', ADirective) then
    Result := Copy(ADirective, 1, 2) + 'IFDEF ' + Copy(ADirective, 10, Length(ADirective) - 9)
  else
  begin
  //ParsingError
  end;
end;

function IsIgnoreItem(AStr: string): Boolean;
begin
  Result := Pos('DELPHI', AStr) > 0;
end;

function ProcessDefsStack(ADirective: string; ADefineStack: TStrings): Boolean;
begin
  Result := False;
  if (IsDirectiveStr('ENDIF}', ADirective) or IsDirectiveStr('ENDIF ', ADirective)) and
    (ADefineStack.Count > 0) then
  begin
    Result := not IsIgnoreItem(ADefineStack[ADefineStack.Count - 1]);
    ADefineStack.Delete(ADefineStack.Count - 1);
  end
  else
  if (IsDirectiveStr('ELSE}', ADirective) or IsDirectiveStr('ELSE ', ADirective)) and
    (ADefineStack.Count > 0) then
  begin
    Result := not IsIgnoreItem(ADefineStack[ADefineStack.Count - 1]);
    ADefineStack[ADefineStack.Count - 1] := ReverseIfXDef(ADefineStack[ADefineStack.Count-1]);
  end
  else
  if IsDirectiveStr('IFDEF ', ADirective) or IsDirectiveStr('IFNDEF ', ADirective) then
  begin
    ADefineStack.Add(ADirective);
    Result := not IsIgnoreItem(ADirective);
  end;
end;

function ProcessDef(ADirective: string; ACurrentDefines, ADefineStack: TStrings;
  var IsDefLine: Boolean): Boolean;
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

function GetTemplateSource(AResourceName: string; AParameterList: TStrings): string;
var
  RS: TResourceStream;
  TemplateStrings, SourceStrings: TStringList;
  I, J: Integer;
  S: string;
  DefineHandler: TDefineHandler;
begin
  TemplateStrings := TStringList.Create;
  try
    RS := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
    try
      TemplateStrings.LoadFromStream(RS);
    finally
      RS.Free;
    end;
    SourceStrings := TStringList.Create;
    try
      DefineHandler := TDefineHandler.Create;
      try
        for I := 0 to Pred(AParameterList.Count) do
          if (Pos('BLOCK', AParameterList.Names[I]) = 1) and
            (AParameterList.Values[AParameterList.Names[I]] <> '0') then
            DefineHandler.Defines.Add(AParameterList.Names[I]);
        for I := 0 to Pred(TemplateStrings.Count) do
        begin
          S := TemplateStrings[I];
          DefineHandler.HandleLine(S);
          if DefineHandler.CanWrite then
          begin
            for J := 0 to Pred(AParameterList.Count) do
              StrReplace(S, '%' + AParameterList.Names[J] + '%', AParameterList.Values[AParameterList.Names[J]],
                [rfReplaceAll, rfIgnoreCase]);
            SourceStrings.Add(S);
          end;
        end;
      finally
        DefineHandler.Free;
      end;
      Result := SourceStrings.Text;
    finally
      SourceStrings.Free;
    end;
  finally
    TemplateStrings.Free;
  end;
end;

type
  TMakeStudioPluginProjectCreator = class(TInterfacedObject,
    IOTACreator, IOTAProjectCreator)
  private
    FFileName: string;
    FResourceName: string;
    FParameterList: TStrings;
  public
    constructor Create(const AResourceName, AFileName: string; AParameterList: TStrings);
    // IOTACreator
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
  end;

  TMakeStudioPluginProjectSource = class(TInterfacedObject, IOTAFile)
  private
    FProjectName: string;
    FResourceName: string;
    FParameterList: TStrings;
  public
    constructor Create(const ProjectName, AResourceName: string;
      AParameterList: TStrings);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TOnFormCreatedEvent = procedure(const FormEditor: IOTAFormEditor) of object;

  TMakeStudioCustomPluginModuleCreator = class(TInterfacedObject,
    IOTACreator, IOTAModuleCreator)
  private
    FUnitResourceName: string;
    FDFMResourceName: string;
    FFileName: string;
    FParameterList: TStrings;
    FOnFormCreatedEvent: TOnFormCreatedEvent;
  public
    constructor Create(const AUnitResourceName, ADFMResourceName,
      AFileName: string; AParameterList: TStrings; AOnFormCreatedEvent: TOnFormCreatedEvent = nil);
    // IOTACreator
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TMakeStudioPluginModuleSource = class(TInterfacedObject, IOTAFile)
  private
    FResourceName: string;
    FParameterList: TStrings;
    FModuleIdent: string;
    FFormIdent: string;
    FAncestorIdent: string;
  public
    constructor Create(const AResourceName: string; AParameterList: TStrings;
      const ModuleIdent, FormIdent, AncestorIdent: string);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

var
  ProjectModule: IOTAModule;

constructor TMakeStudioPluginProjectCreator.Create(const AResourceName, AFileName: string;
  AParameterList: TStrings);
begin
  inherited Create;
  FResourceName := AResourceName;
  FFileName := AFileName;
  FParameterList := AParameterList;
end;  

function TMakeStudioPluginProjectCreator.GetCreatorType: string;
begin
  Result := '';
end;

function TMakeStudioPluginProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TMakeStudioPluginProjectCreator.GetFileName: string;
begin
  //does *NOT* work without path
  Result := PathAddSeparator(GetCurrentDir) + FFileName;
end;

function TMakeStudioPluginProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TMakeStudioPluginProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TMakeStudioPluginProjectCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TMakeStudioPluginProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TMakeStudioPluginProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TMakeStudioPluginProjectCreator.NewDefaultModule;
begin
end;

function TMakeStudioPluginProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TMakeStudioPluginProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

function TMakeStudioPluginProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
  if FResourceName <> '' then
    Result := TMakeStudioPluginProjectSource.Create(ProjectName, FResourceName, FParameterList);
end;

constructor TMakeStudioPluginProjectSource.Create(const ProjectName, AResourceName: string;
  AParameterList: TStrings);
begin
  inherited Create;
  FProjectName := ProjectName;
  FResourceName := AResourceName;
  FParameterList := AParameterList;
end;

function TMakeStudioPluginProjectSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TMakeStudioPluginProjectSource.GetSource: string;
var
  TempParameterList: TStringList;
begin
  TempParameterList := TStringList.Create;
  try
    TempParameterList.Assign(FParameterList);
    TempParameterList.Add(Format('MODULEIDENT=%s', [FProjectName]));
    Result := GetTemplateSource(FResourceName, TempParameterList);
  finally
    TempParameterList.Free;
  end;
end;

constructor TMakeStudioCustomPluginModuleCreator.Create(const AUnitResourceName, ADFMResourceName,
  AFileName: string; AParameterList: TStrings; AOnFormCreatedEvent: TOnFormCreatedEvent = nil);
begin
  inherited Create;
  FUnitResourceName := AUnitResourceName;
  FDFMResourceName := ADFMResourceName;
  FFileName := AFileName;
  FParameterList := AParameterList;
  FOnFormCreatedEvent := AOnFormCreatedEvent;
end;

procedure TMakeStudioCustomPluginModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  if Assigned(FOnFormCreatedEvent) then
    FOnFormCreatedEvent(FormEditor);
end;

function TMakeStudioCustomPluginModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TMakeStudioCustomPluginModuleCreator.GetCreatorType: string;
begin
  if FDFMResourceName = '' then
    Result := sUnit
  else
    Result := sForm;
end;

function TMakeStudioCustomPluginModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TMakeStudioCustomPluginModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TMakeStudioCustomPluginModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TMakeStudioCustomPluginModuleCreator.GetImplFileName: string;
begin
  //does work without path as well
  Result := PathAddSeparator(GetCurrentDir) + FFileName;
end;

function TMakeStudioCustomPluginModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TMakeStudioCustomPluginModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TMakeStudioCustomPluginModuleCreator.GetOwner: IOTAModule;
begin
  Result := ProjectModule;
end;

function TMakeStudioCustomPluginModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TMakeStudioCustomPluginModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TMakeStudioCustomPluginModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TMakeStudioCustomPluginModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
  if FDFMResourceName <> '' then
    Result := TMakeStudioPluginModuleSource.Create(FDFMResourceName, FParameterList,
      '', FormIdent, AncestorIdent);
end;

function TMakeStudioCustomPluginModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TMakeStudioPluginModuleSource.Create(FUnitResourceName, FParameterList,
    ModuleIdent, FormIdent, AncestorIdent);
end;

function TMakeStudioCustomPluginModuleCreator.NewIntfSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{ TMakeStudioPluginModuleSource }

constructor TMakeStudioPluginModuleSource.Create(const AResourceName: string; AParameterList: TStrings;
  const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FResourceName := AResourceName;
  FParameterList := AParameterList;
  FModuleIdent := ModuleIdent;
  FFormIdent := FormIdent;
  FAncestorIdent := AncestorIdent;
end;

function TMakeStudioPluginModuleSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TMakeStudioPluginModuleSource.GetSource: string;
var
  TempParameterList: TStringList;
begin
  TempParameterList := TStringList.Create;
  try
    TempParameterList.Assign(FParameterList);
    TempParameterList.Add(Format('MODULEIDENT=%s', [FModuleIdent]));
    Result := GetTemplateSource(FResourceName, TempParameterList);
    {$IFDEF COMPILER8_UP}
    Result := UTF8Encode(Result);
    {$ENDIF COMPILER8_UP}
  finally
    TempParameterList.Free;
  end;
end;

{$IFDEF IDE_SUPPORTS_DELPHI}
type
  TMakeStudioDxWin32VCLPluginModuleCreator = class(TMakeStudioCustomPluginModuleCreator);
{$ENDIF IDE_SUPPORTS_DELPHI}  

{$IFDEF IDE_SUPPORTS_CSHARP}
type
  TMakeStudioCSharpPluginProjectCreator = class(TMakeStudioPluginProjectCreator,
    IOTAProjectCreator50, IOTAProjectCreator80)
  public
    function GetCreatorType: string; override;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
  end;

  TMakeStudioCSharpPluginModuleCreator = class(TMakeStudioCustomPluginModuleCreator)
  public
    function GetCreatorType: string; override;
  end;

function TMakeStudioCSharpPluginProjectCreator.GetCreatorType: string;
begin
  Result := sAssembly;
end;

procedure TMakeStudioCSharpPluginProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
end;

function TMakeStudioCSharpPluginProjectCreator.GetProjectPersonality: string;
begin
  Result := sCSharpPersonality;
end;

function TMakeStudioCSharpPluginModuleCreator.GetCreatorType: string;
begin
  if FDFMResourceName = '' then
    Result := sClass
  else
    Result := sWinForm;
end;
{$ENDIF IDE_SUPPORTS_CSHARP}

{ TMakeStudioCustomPluginWizard }

constructor TMakeStudioCustomPluginWizard.Create;
begin
  inherited Create;
  FParameterList := TStringList.Create;
  FCommandoBitmap := TBitmap.Create;
  FActionBitmap := TBitmap.Create;
  FWizardKind := wkDelphiWin32VCL;
  FIDPostFix := '';
end;

destructor TMakeStudioCustomPluginWizard.Destroy;
begin
  FParameterList.Free;
  FCommandoBitmap.Free;
  FActionBitmap.Free;
  inherited Destroy;
end;

procedure TMakeStudioCustomPluginWizard.Execute;
var
  FilesPrefix: string;
begin
  FParameterList.Clear;
  if GetPluginOptions(FWizardKind, FParameterList, FCommandoBitmap, FActionBitmap) then
  begin
    FilesPrefix := FParameterList.Values[FilesPrefixParameterName];
    with BorlandIDEServices as IOTAModuleServices do
    begin
      if FWizardKind = wkDelphiWin32VCL then
      begin
        {$IFDEF IDE_SUPPORTS_DELPHI}
        ProjectModule := CreateModule(TMakeStudioPluginProjectCreator.Create('MakeStudioPLUGINSRC_DPR',
          FilesPrefix + 'Plugin.dpr', FParameterList));
        CreateModule(TMakeStudioDxWin32VCLPluginModuleCreator.Create('MakeStudioPLUGINUNITSRC_ACTIONS',
          'MakeStudioPLUGINFORMSRC_ACTIONS', FilesPrefix + 'Actions.pas', FParameterList, HandleActionFormCreated));
        if FParameterList.Values['BLOCKMENUACTION'] <> '0' then
        begin
          CreateModule(TMakeStudioDxWin32VCLPluginModuleCreator.Create('MakeStudioPLUGINUNITSRC_ACTIONTEST',
            'MakeStudioPLUGINFORMSRC_ACTIONTEST', FilesPrefix + 'Actiontest.pas', FParameterList));
        end;
        CreateModule(TMakeStudioDxWin32VCLPluginModuleCreator.Create('MakeStudioPLUGINUNITSRC_EDIT',
          'MakeStudioPLUGINFORMSRC_EDIT', FilesPrefix + 'Edit.pas', FParameterList));
        CreateModule(TMakeStudioDxWin32VCLPluginModuleCreator.Create('MakeStudioPLUGINSRC_MODULE', '',
          FilesPrefix + 'Module.pas', FParameterList));
        CreateModule(TMakeStudioDxWin32VCLPluginModuleCreator.Create('MakeStudioPLUGINSRC_VARS', '',
          FilesPrefix + 'Vars.pas', FParameterList));
        CreateModule(TMakeStudioDxWin32VCLPluginModuleCreator.Create('', '',
          'makestudio_tlb.pas', nil));
        {$ENDIF IDE_SUPPORTS_DELPHI}
      end
      else
      if FWizardKind = wkCSharp then
      begin
        {$IFDEF IDE_SUPPORTS_CSHARP}
        ProjectModule := CreateModule(TMakeStudioCSharpPluginProjectCreator.Create('',
          FilesPrefix + 'Plugin.bdsproj', FParameterList));
        (ProjectModule as IOTAProject).AddFile('MakeStudio.dll', False);
        (ProjectModule as IOTAProject).AddFile('stdole.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.Data.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.Drawing.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.Windows.Forms.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.XML.dll', False);
        CreateModule(TMakeStudioCSharpPluginModuleCreator.Create('MakeStudioPLUGIN_CS_SRC_ASSEMBLYINFO',
          '', FilesPrefix + 'AssemblyInfo.cs', FParameterList));
        CreateModule(TMakeStudioCSharpPluginModuleCreator.Create('MakeStudioPLUGIN_CS_SRC',
          '', FilesPrefix + 'Plugin.cs', FParameterList));
        CreateModule(TMakeStudioCSharpPluginModuleCreator.Create('MakeStudioPLUGIN_CS_UNITSRC_EDIT',
          'MakeStudioPLUGIN_CS_FORMSRC_EDIT', FilesPrefix + 'Edit.cs', FParameterList));
        CreateModule(TMakeStudioCSharpPluginModuleCreator.Create('MakeStudioPLUGIN_CS_UNITSRC_ACTIONTEST',
          'MakeStudioPLUGIN_CS_FORMSRC_ACTIONTEST', FilesPrefix + 'ActionTest.cs', FParameterList));
        {$ENDIF IDE_SUPPORTS_CSHARP}
      end;
    end;
  end;
end;

function TMakeStudioCustomPluginWizard.GetAuthor: string;
begin
  Result := 'Uwe Schuster(jedivcs@bitcommander.de)';
end;

function TMakeStudioCustomPluginWizard.GetComment: string;
begin
  Result := 'Creates a JEDI VCS Make Plugin Project.';
end;

function TMakeStudioCustomPluginWizard.GetGlyph: {$IFDEF DELPHI6_UP}Cardinal {$ELSE}HICON {$ENDIF};
begin
  Result := LoadIcon(HInstance, 'WIZARDICON');
end;

function TMakeStudioCustomPluginWizard.GetIDString: string;
begin
  Result := 'PROJECT JEDI.JEDI VCS Make Plugin Wizard' + FIDPostFix;
end;

function TMakeStudioCustomPluginWizard.GetName: string;
begin
  Result := 'JEDI VCS Make Plugin Wizard';
end;

function TMakeStudioCustomPluginWizard.GetPage: string;
begin
  Result := 'JEDI VCS';
end;

function TMakeStudioCustomPluginWizard.GetState: TWizardState;
begin
  Result := [];
end;

procedure TMakeStudioCustomPluginWizard.HandleActionFormCreated(const FormEditor: IOTAFormEditor);
var
  NFormEditor: INTAFormEditor;
  ActionForm: TComponent;
  tempComponent: TComponent;
begin
  if (FormEditor.QueryInterface(INTAFormEditor, NFormEditor) = S_OK) and
     Assigned(NFormEditor.FormDesigner) then
  begin
    ActionForm := NFormEditor.FormDesigner.GetRoot;
    if Assigned(ActionForm) then
    begin
      tempComponent := ActionForm.FindComponent('acTestaction1');
      if Assigned(tempComponent) and (tempComponent is TAction) then
        TAction(tempComponent).Caption := FParameterList.Values[TestActionCaptionParameterName];
      tempComponent := ActionForm.FindComponent('ImageList1');
      if Assigned(tempComponent) and (tempComponent is TImageList) then
      begin
        if (FCommandoBitmap.Width = 16) and (FCommandoBitmap.Height = 16) then
          TImageList(tempComponent).ReplaceMasked(0, FCommandoBitmap, FCommandoBitmap.TransparentColor);
        if (FActionBitmap.Width = 16) and (FActionBitmap.Height = 16) then
          TImageList(tempComponent).ReplaceMasked(1, FActionBitmap, FActionBitmap.TransparentColor);
      end;
    end;
  end;
end;

{$IFDEF IDE_SUPPORTS_DELPHI}
constructor TMakeStudioDxWin32VCLPluginWizard.Create;
begin
  inherited Create;
  FWizardKind := wkDelphiWin32VCL;
  FIDPostFix := ' (Delphi Win32 VCL)';
end;
{$ENDIF IDE_SUPPORTS_DELPHI}

{$IFDEF IDE_SUPPORTS_CSHARP}
constructor TMakeStudioCSharpPluginWizard.Create;
begin
  inherited Create;
  FWizardKind := wkCSharp;
  FIDPostFix := ' (C#)';
end;

function TMakeStudioCSharpPluginWizard.GetDesigner: string;
begin
  Result := dDotNet;
end;

function TMakeStudioCSharpPluginWizard.GetGalleryCategory: IOTAGalleryCategory;
var
  GalleryCategoryManager: IOTAGalleryCategoryManager;
  CSharpGalleryItem: IOTAGalleryCategory;
begin
  Result := nil;
  if Supports(BorlandIDEServices, IOTAGalleryCategoryManager, GalleryCategoryManager) then
  begin
    CSharpGalleryItem := GalleryCategoryManager.FindCategory(sCategoryCSharpNew);
    if Assigned(CSharpGalleryItem) then
      Result := GalleryCategoryManager.AddCategory(CSharpGalleryItem, 'PROJECT JEDI.JEDI VCS Make.CSharp', 'JEDI VCS')
    else
      Result := GalleryCategoryManager.AddCategory('PROJECT JEDI.JEDI VCS Make.CSharp', 'JEDI VCS C#');
  end;
end;

function TMakeStudioCSharpPluginWizard.GetPersonality: string;
begin
  Result := sCSharpPersonality;
end;
{$ENDIF IDE_SUPPORTS_CSHARP}

end.
