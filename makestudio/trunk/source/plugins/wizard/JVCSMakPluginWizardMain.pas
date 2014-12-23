(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSMakPluginWizardMain.pas

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
2005/03/12  USchuster - now use JVCSMakPluginWizardCommon.pas for common stuff

-----------------------------------------------------------------------------*)

unit JVCSMakPluginWizardMain;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, Classes, Windows, Controls, Forms, ToolsAPI, ComObj, JVCSMakPluginWizardOptions,
  ActnList, Graphics, JVCSMakPluginWizardCommon;

type
  TJVCSMakCustomPluginWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
    IOTAProjectWizard)
  private
    FParameterList: TStringList;
    FCommandoBitmap: TBitmap;
    FActionBitmap: TBitmap;
    procedure HandleActionFormCreated(const FormEditor: IOTAFormEditor);
  protected
    FWizardKind: TJVCSMakPluginWizardKind;
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
  TJVCSMakDxWin32VCLPluginWizard = class(TJVCSMakCustomPluginWizard)
  public
    constructor Create;
  end;
  {$ENDIF IDE_SUPPORTS_DELPHI}

  {$IFDEF IDE_SUPPORTS_CSHARP}
  TJVCSMakCSharpPluginWizard = class(TJVCSMakCustomPluginWizard, IOTARepositoryWizard60,
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
  TJVCSMakPluginProjectCreator = class(TInterfacedObject,
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

  TJVCSMakPluginProjectSource = class(TInterfacedObject, IOTAFile)
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

  TJVCSMakCustomPluginModuleCreator = class(TInterfacedObject,
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

  TJVCSMakPluginModuleSource = class(TInterfacedObject, IOTAFile)
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

constructor TJVCSMakPluginProjectCreator.Create(const AResourceName, AFileName: string;
  AParameterList: TStrings);
begin
  inherited Create;
  FResourceName := AResourceName;
  FFileName := AFileName;
  FParameterList := AParameterList;
end;  

function TJVCSMakPluginProjectCreator.GetCreatorType: string;
begin
  Result := '';
end;

function TJVCSMakPluginProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TJVCSMakPluginProjectCreator.GetFileName: string;
begin
  //does *NOT* work without path
  Result := PathAddSeparator(GetCurrentDir) + FFileName;
end;

function TJVCSMakPluginProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TJVCSMakPluginProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TJVCSMakPluginProjectCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TJVCSMakPluginProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TJVCSMakPluginProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TJVCSMakPluginProjectCreator.NewDefaultModule;
begin
end;

function TJVCSMakPluginProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TJVCSMakPluginProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

function TJVCSMakPluginProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
  if FResourceName <> '' then
    Result := TJVCSMakPluginProjectSource.Create(ProjectName, FResourceName, FParameterList);
end;

constructor TJVCSMakPluginProjectSource.Create(const ProjectName, AResourceName: string;
  AParameterList: TStrings);
begin
  inherited Create;
  FProjectName := ProjectName;
  FResourceName := AResourceName;
  FParameterList := AParameterList;
end;

function TJVCSMakPluginProjectSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TJVCSMakPluginProjectSource.GetSource: string;
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

constructor TJVCSMakCustomPluginModuleCreator.Create(const AUnitResourceName, ADFMResourceName,
  AFileName: string; AParameterList: TStrings; AOnFormCreatedEvent: TOnFormCreatedEvent = nil);
begin
  inherited Create;
  FUnitResourceName := AUnitResourceName;
  FDFMResourceName := ADFMResourceName;
  FFileName := AFileName;
  FParameterList := AParameterList;
  FOnFormCreatedEvent := AOnFormCreatedEvent;
end;

procedure TJVCSMakCustomPluginModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  if Assigned(FOnFormCreatedEvent) then
    FOnFormCreatedEvent(FormEditor);
end;

function TJVCSMakCustomPluginModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TJVCSMakCustomPluginModuleCreator.GetCreatorType: string;
begin
  if FDFMResourceName = '' then
    Result := sUnit
  else
    Result := sForm;
end;

function TJVCSMakCustomPluginModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TJVCSMakCustomPluginModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TJVCSMakCustomPluginModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TJVCSMakCustomPluginModuleCreator.GetImplFileName: string;
begin
  //does work without path as well
  Result := PathAddSeparator(GetCurrentDir) + FFileName;
end;

function TJVCSMakCustomPluginModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TJVCSMakCustomPluginModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TJVCSMakCustomPluginModuleCreator.GetOwner: IOTAModule;
begin
  Result := ProjectModule;
end;

function TJVCSMakCustomPluginModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TJVCSMakCustomPluginModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TJVCSMakCustomPluginModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TJVCSMakCustomPluginModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
  if FDFMResourceName <> '' then
    Result := TJVCSMakPluginModuleSource.Create(FDFMResourceName, FParameterList,
      '', FormIdent, AncestorIdent);
end;

function TJVCSMakCustomPluginModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TJVCSMakPluginModuleSource.Create(FUnitResourceName, FParameterList,
    ModuleIdent, FormIdent, AncestorIdent);
end;

function TJVCSMakCustomPluginModuleCreator.NewIntfSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{ TJVCSMakPluginModuleSource }

constructor TJVCSMakPluginModuleSource.Create(const AResourceName: string; AParameterList: TStrings;
  const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FResourceName := AResourceName;
  FParameterList := AParameterList;
  FModuleIdent := ModuleIdent;
  FFormIdent := FormIdent;
  FAncestorIdent := AncestorIdent;
end;

function TJVCSMakPluginModuleSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TJVCSMakPluginModuleSource.GetSource: string;
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
  TJVCSMakDxWin32VCLPluginModuleCreator = class(TJVCSMakCustomPluginModuleCreator);
{$ENDIF IDE_SUPPORTS_DELPHI}  

{$IFDEF IDE_SUPPORTS_CSHARP}
type
  TJVCSMakCSharpPluginProjectCreator = class(TJVCSMakPluginProjectCreator,
    IOTAProjectCreator50, IOTAProjectCreator80)
  public
    function GetCreatorType: string; override;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
  end;

  TJVCSMakCSharpPluginModuleCreator = class(TJVCSMakCustomPluginModuleCreator)
  public
    function GetCreatorType: string; override;
  end;

function TJVCSMakCSharpPluginProjectCreator.GetCreatorType: string;
begin
  Result := sAssembly;
end;

procedure TJVCSMakCSharpPluginProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
end;

function TJVCSMakCSharpPluginProjectCreator.GetProjectPersonality: string;
begin
  Result := sCSharpPersonality;
end;

function TJVCSMakCSharpPluginModuleCreator.GetCreatorType: string;
begin
  if FDFMResourceName = '' then
    Result := sClass
  else
    Result := sWinForm;
end;
{$ENDIF IDE_SUPPORTS_CSHARP}

{ TJVCSMakCustomPluginWizard }

constructor TJVCSMakCustomPluginWizard.Create;
begin
  inherited Create;
  FParameterList := TStringList.Create;
  FCommandoBitmap := TBitmap.Create;
  FActionBitmap := TBitmap.Create;
  FWizardKind := wkDelphiWin32VCL;
  FIDPostFix := '';
end;

destructor TJVCSMakCustomPluginWizard.Destroy;
begin
  FParameterList.Free;
  FCommandoBitmap.Free;
  FActionBitmap.Free;
  inherited Destroy;
end;

procedure TJVCSMakCustomPluginWizard.Execute;
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
        ProjectModule := CreateModule(TJVCSMakPluginProjectCreator.Create('JVCSMAKPLUGINSRC_DPR',
          FilesPrefix + 'Plugin.dpr', FParameterList));
        CreateModule(TJVCSMakDxWin32VCLPluginModuleCreator.Create('JVCSMAKPLUGINUNITSRC_ACTIONS',
          'JVCSMAKPLUGINFORMSRC_ACTIONS', FilesPrefix + 'Actions.pas', FParameterList, HandleActionFormCreated));
        if FParameterList.Values['BLOCKMENUACTION'] <> '0' then
        begin
          CreateModule(TJVCSMakDxWin32VCLPluginModuleCreator.Create('JVCSMAKPLUGINUNITSRC_ACTIONTEST',
            'JVCSMAKPLUGINFORMSRC_ACTIONTEST', FilesPrefix + 'Actiontest.pas', FParameterList));
        end;
        CreateModule(TJVCSMakDxWin32VCLPluginModuleCreator.Create('JVCSMAKPLUGINUNITSRC_EDIT',
          'JVCSMAKPLUGINFORMSRC_EDIT', FilesPrefix + 'Edit.pas', FParameterList));
        CreateModule(TJVCSMakDxWin32VCLPluginModuleCreator.Create('JVCSMAKPLUGINSRC_MODULE', '',
          FilesPrefix + 'Module.pas', FParameterList));
        CreateModule(TJVCSMakDxWin32VCLPluginModuleCreator.Create('JVCSMAKPLUGINSRC_VARS', '',
          FilesPrefix + 'Vars.pas', FParameterList));
        {$ENDIF IDE_SUPPORTS_DELPHI}
      end
      else
      if FWizardKind = wkCSharp then
      begin
        {$IFDEF IDE_SUPPORTS_CSHARP}
        ProjectModule := CreateModule(TJVCSMakCSharpPluginProjectCreator.Create('',
          FilesPrefix + 'Plugin.bdsproj', FParameterList));
        (ProjectModule as IOTAProject).AddFile('jvcsmak.dll', False);
        (ProjectModule as IOTAProject).AddFile('stdole.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.Data.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.Drawing.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.Windows.Forms.dll', False);
        (ProjectModule as IOTAProject).AddFile('System.XML.dll', False);
        CreateModule(TJVCSMakCSharpPluginModuleCreator.Create('JVCSMAKPLUGIN_CS_SRC_ASSEMBLYINFO',
          '', FilesPrefix + 'AssemblyInfo.cs', FParameterList));
        CreateModule(TJVCSMakCSharpPluginModuleCreator.Create('JVCSMAKPLUGIN_CS_SRC',
          '', FilesPrefix + 'Plugin.cs', FParameterList));
        CreateModule(TJVCSMakCSharpPluginModuleCreator.Create('JVCSMAKPLUGIN_CS_UNITSRC_EDIT',
          'JVCSMAKPLUGIN_CS_FORMSRC_EDIT', FilesPrefix + 'Edit.cs', FParameterList));
        CreateModule(TJVCSMakCSharpPluginModuleCreator.Create('JVCSMAKPLUGIN_CS_UNITSRC_ACTIONTEST',
          'JVCSMAKPLUGIN_CS_FORMSRC_ACTIONTEST', FilesPrefix + 'ActionTest.cs', FParameterList));
        {$ENDIF IDE_SUPPORTS_CSHARP}
      end;
    end;
  end;
end;

function TJVCSMakCustomPluginWizard.GetAuthor: string;
begin
  Result := 'Uwe Schuster(jedivcs@bitcommander.de)';
end;

function TJVCSMakCustomPluginWizard.GetComment: string;
begin
  Result := 'Creates a JEDI VCS Make Plugin Project.';
end;

function TJVCSMakCustomPluginWizard.GetGlyph: {$IFDEF DELPHI6_UP}Cardinal {$ELSE}HICON {$ENDIF};
begin
  Result := LoadIcon(HInstance, 'WIZARDICON');
end;

function TJVCSMakCustomPluginWizard.GetIDString: string;
begin
  Result := 'PROJECT JEDI.JEDI VCS Make Plugin Wizard' + FIDPostFix;
end;

function TJVCSMakCustomPluginWizard.GetName: string;
begin
  Result := 'JEDI VCS Make Plugin Wizard';
end;

function TJVCSMakCustomPluginWizard.GetPage: string;
begin
  Result := 'JEDI VCS';
end;

function TJVCSMakCustomPluginWizard.GetState: TWizardState;
begin
  Result := [];
end;

procedure TJVCSMakCustomPluginWizard.HandleActionFormCreated(const FormEditor: IOTAFormEditor);
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
constructor TJVCSMakDxWin32VCLPluginWizard.Create;
begin
  inherited Create;
  FWizardKind := wkDelphiWin32VCL;
  FIDPostFix := ' (Delphi Win32 VCL)';
end;
{$ENDIF IDE_SUPPORTS_DELPHI}

{$IFDEF IDE_SUPPORTS_CSHARP}
constructor TJVCSMakCSharpPluginWizard.Create;
begin
  inherited Create;
  FWizardKind := wkCSharp;
  FIDPostFix := ' (C#)';
end;

function TJVCSMakCSharpPluginWizard.GetDesigner: string;
begin
  Result := dDotNet;
end;

function TJVCSMakCSharpPluginWizard.GetGalleryCategory: IOTAGalleryCategory;
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

function TJVCSMakCSharpPluginWizard.GetPersonality: string;
begin
  Result := sCSharpPersonality;
end;
{$ENDIF IDE_SUPPORTS_CSHARP}

end.
