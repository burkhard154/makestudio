(*
  ***************************************************************************
  optiMEAS GmbH
  written by Burkhard Schranz, Jens-Achim Kessel
  copyright © 2013 -
  Email : info@optimeas.de
  Web : http://www.optimeas.de
  http://www.makestudio.de
  http://www.mobiconn.de
  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  The component can be freely used in any application.
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of optiMEAS GmbH


  Known Issues:
  -----------------------------------------------------------------------------

  Unit history:

  2005/01/04  BSchranz  - Plugin created
  2005/02/04  USchuster - preparations for check in
  2005/02/25  BSchranz - Log message added for deleting package from registry
  2006/04/11  BSchranz - Register IDE DLL Expert support added
  2006/04/12  BSchranz - support for borlands .dof file in compilation added
  2006/04/12  BSchranz - support for package prefix and suffix added

  ----------------------------------------------------------------------------- *)

unit delphi32_Compile;

{$I jedi.inc}
{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, makestudio_TLB, delphi32_Vars,
  Classes, Windows, Dialogs, Controls, SysUtils, Forms, Registry, JclFileUtils,
  IniFiles;

type
  TDelphi32Module = class(TComponent, ICommand, IExecCallback)
  private
    FCaption: string;
    FFilename: string;
    FDMakAction: TDMakAction;
    FCompilerSwitch: string;
    FOutputDir: string;
    FSearchDirs: TStringList;

    FLastProcessOutput: string;
    FAllPlatforms: Boolean;
    procedure SetDMakAction(Value: TDMakAction);
    function GetFilename: string;
    function GetOutputDir: string;
    procedure aSetFilename(const Value: string);
    function GetRealDelphiFilename: String;
    function GetRealOutputDir: String;
    procedure SetAllPlatforms(const Value: Boolean);

  protected
    // ICommand
    function MeasureItem(Handle: Integer; BriefView: WordBool): Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer; Bottom: Integer; Selected: WordBool;
      BriefView: WordBool; BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString; const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;

    // IExecCallback
    procedure CaptureOutput(const Line: WideString; var Aborted: WordBool); safecall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function RunCompiler: Boolean;
    procedure RegisterPackage;
    procedure UnRegisterAndDeleteBPL;
    procedure ResetSearchPath;
    procedure DeleteDCP;
    procedure RemoveSearchPath;
    procedure RemoveAllUserPackages;
    procedure RegisterDllExpert;
    procedure UnRegisterPackage;
    procedure UnRegisterCOMDLL;
    procedure RegisterCOMDLL;
    procedure RegisterCOMExe;
    procedure UnRegisterCOMExe;
    procedure SetSearchPath;
    procedure RunBatchfile;

    property Caption: string read FCaption write FCaption;
    property DelphiFilename: string read GetFilename write aSetFilename;
    property DMakAction: TDMakAction read FDMakAction write SetDMakAction;
    property CompilerSwitch: string read FCompilerSwitch write FCompilerSwitch;
    property OutputDir: string read GetOutputDir write FOutputDir;
    property SearchDirs: TStringList read FSearchDirs write FSearchDirs;
    property AllPlatforms: Boolean read FAllPlatforms write SetAllPlatforms;
    property _DelphiFilename: String read GetRealDelphiFilename;
    property _OutputDir: String read GetRealOutputDir;

  end;

  // Callback to create an instance of the IJVCSModule
  TDelphi32ModuleCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginDelphi32Callback: TDelphi32ModuleCallback;

const
  IDDelphi32Module = 'Delphi32.Module';

implementation

uses
  ComServ, delphi32_EditDelphi32Module, delphi32_Utils, system.IOUtils;

resourcestring
  StrForAllPlatforms = 'For all Platforms';

function TDelphi32ModuleCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TDelphi32Module.Create(nil));
end;

procedure TDelphi32ModuleCallback.SetCanceled(ACanceled: WordBool);
begin
  Canceled := ACanceled;
end;

constructor TDelphi32Module.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdStandardModuleCaption;
  FFilename := '';
  DMakAction := dmaNone;
  CompilerSwitch := '';
  OutputDir := '';
  SearchDirs := TStringList.Create;
  FLastProcessOutput := '';
  FAllPlatforms := false;
end;

destructor TDelphi32Module.Destroy;
begin
  SearchDirs.Free;
  inherited Destroy;
end;

function TDelphi32Module.EditItem: WordBool;
begin
  Result := EditDMakModule(Self);
end;

function TDelphi32Module.ExecuteItem: WordBool;
begin
  MakeStudio.LogMessage(stdBreak);
  MakeStudio.LogMessage(DelphiFilename);
  MakeStudio.LogMessage(stdDMakAction + stActions[DMakAction]);
  MakeStudio.LogMessage('');

  Result := True;

  if (DMakAction in [dmaRegDPK, dmaCompile, dmaRegisterDllExpert]) then
  begin
    Result := RunCompiler;

    if Result and (SearchDirs.Count > 0) then
      AddPathListToDelphiPath(SearchDirs);
  end;

  if Result then
  begin
    case DMakAction of
      dmaNone:
        ;
      dmaRegDPK:
        RegisterPackage;
      dmaUnRegDPK:
        UnRegisterPackage;
      dmaRegCOMDll:
        RegisterCOMDLL;
      dmaUnRegCOMDll:
        UnRegisterCOMDLL;
      dmaRegCOMExe:
        RegisterCOMExe;
      dmaUnRegCOMExe:
        UnRegisterCOMExe;
      dmaSetSearchPath:
        SetSearchPath;
      dmaRunBat:
        RunBatchfile;
      dmaRegisterDllExpert:
        RegisterDllExpert;
      dmaDeleteBPL:
        UnRegisterAndDeleteBPL;
      dmaDeleteDCP:
        DeleteDCP;
      dmaRemoveSearchPath, dmaRemoveSearchPathIncludingSubdir:
        RemoveSearchPath;
      dmaRemoveAllUserPackages:
        RemoveAllUserPackages;
      dmaResetSearchPath:
        ResetSearchPath;
    end;
  end;
  MakeStudio.LogMessage(stdBreak);
  MakeStudio.LogMessage('');
end;

function TDelphi32Module.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
  I: Integer;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(Caption) + 2;

    if BriefView then
      Exit;

    Result := Result + Canvas.TextHeight(DelphiFilename) + 2;
    Canvas.Font.Style := [];

    if AllPlatforms then
      Result := Result + Canvas.TextHeight(StrForAllPlatforms) + 2;
    if CompilerSwitch <> '' then
      Result := Result + Canvas.TextHeight(CompilerSwitch) + 2;
    if OutputDir <> '' then
      Result := Result + Canvas.TextHeight(OutputDir) + 2;
    if SearchDirs.Count > 0 then
      Result := Result + Canvas.TextHeight(stdSearchDirsTxt) + 2;
    for I := 0 to SearchDirs.Count - 1 do
    begin
      if I < 5 then
      begin
        Result := Result + Canvas.TextHeight(SearchDirs[I]) + 2;
      end
      else if I = 5 then
      begin
        Result := Result + Canvas.TextHeight(SearchDirs[I]) + 2;
      end
      else;
    end;
  finally
    Canvas.Free;
  end;
end;

function TDelphi32Module.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer; Bottom: Integer; Selected: WordBool;
  BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;
  I: Integer;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := false; // ownerdraw

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
    SetCanvasTextColor(clWindowText);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left + iDefaultIndent, aRect.Top + Offset, Caption);
    Offset := Offset + Canvas.TextHeight(Caption) + 2;
    if not BriefView then
    begin
      if AllPlatforms then
      begin
        SetCanvasTextColor(clBlue);
        Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset, StrForAllPlatforms);
        Offset := Offset + Canvas.TextHeight(StrForAllPlatforms) + 2;
        Canvas.Font.Style := [];
      end;

      SetCanvasTextColor(clBlue);
      Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset, DelphiFilename);
      Offset := Offset + Canvas.TextHeight(DelphiFilename) + 2;
      Canvas.Font.Style := [];

      if CompilerSwitch <> '' then
      begin
        SetCanvasTextColor(clRed);
        Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset, stdCompilerSwitch + CompilerSwitch);
        Offset := Offset + Canvas.TextHeight(CompilerSwitch) + 2;
      end;
      if OutputDir <> '' then
      begin
        SetCanvasTextColor(clBlue);
        Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset, stdOutputDir + OutputDir);
        Offset := Offset + Canvas.TextHeight(OutputDir) + 2;
      end;
      if SearchDirs.Count > 0 then
      begin
        Canvas.Font.Color := clMaroon;
        Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset, stdSearchDirsTxt);
        Offset := Offset + Canvas.TextHeight(stdSearchDirsTxt) + 2;
      end;
      for I := 0 to SearchDirs.Count - 1 do
      begin
        if I < 5 then
        begin
          Canvas.TextOut(aRect.Left + iDefaultIndent + 30, aRect.Top + Offset, SearchDirs[I]);
          Offset := Offset + Canvas.TextHeight(SearchDirs[I]) + 2;
        end
        else if I = 5 then
        begin
          Canvas.TextOut(aRect.Left + iDefaultIndent + 30, aRect.Top + Offset, '...');
          Offset := Offset + Canvas.TextHeight(SearchDirs[I]) + 2;
        end
        else;
      end;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TDelphi32Module.SetAllPlatforms(const Value: Boolean);
begin
  FAllPlatforms := Value;
end;

procedure TDelphi32Module.SetDMakAction(Value: TDMakAction);
begin
  if DelphiFilename <> '' then
    Caption := stActions[Value] + ' (' + ExtractFilename(DelphiFilename) + ')'
  else
    Caption := stActions[Value];

  FDMakAction := Value;
end;

procedure TDelphi32Module.SetFilename(const Filename: WideString);
begin
  aSetFilename(Filename);
  if CompareText(ExtractFileExt(Filename), stExtPackage) = 0 then
  begin
    SearchDirs.Add(CheckNoBackslash(ExtractFilePath(Filename)));
    if GetPackageRunOnly(Filename) then
      DMakAction := dmaCompile
    else
      DMakAction := dmaRegDPK;
  end
  else if CompareText(ExtractFileExt(Filename), stExtProject) = 0 then
  begin
    if GetIsProjectExpert(Filename) then
      DMakAction := dmaRegisterDllExpert
    else
      DMakAction := dmaCompile;
  end
  else if CompareText(ExtractFileExt(Filename), stExtBpl) = 0 then
  begin
    DMakAction := dmaDeleteBPL;
  end
  else if CompareText(ExtractFileExt(Filename), stExtDCP) = 0 then
  begin
    DMakAction := dmaDeleteDCP;
  end
end;

function TDelphi32Module.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TDelphi32Module.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TDelphi32Module.RunCompiler: Boolean;
var
  sl1: TStringList;
  UPath, RPath, NPath, OPath, IPath, NOPath, SrcPath, Path, LULibs: string;

  procedure CheckCfgFile(asl: TStringList);
  var
    I: Integer;
    S: string;
  begin
    I := 0;
    while I < asl.Count do
      if Length(asl[I]) > 2 then
      begin
        S := Copy(asl[I], 1, 3);
        if (S = '-LE') or (S = '-LN') then
          asl.Delete(I)
        else if CompareText('-NO', Copy(asl[I], 1, 3)) = 0 then
        begin
          NOPath := Copy(asl[I], 5, Length(asl[I]) - 5);
          asl.Delete(I);
        end
        else if CompareText('-LU', Copy(asl[I], 1, 3)) = 0 then
        begin
          LULibs := Copy(asl[I], 5, Length(asl[I]) - 5);
          asl.Delete(I);
        end
        else if CompareText('-U', Copy(asl[I], 1, 2)) = 0 then
        begin
          UPath := ';' + Copy(asl[I], 4, Length(asl[I]) - 4);
          asl.Delete(I);
        end
        else if CompareText('-R', Copy(asl[I], 1, 2)) = 0 then
        begin
          RPath := ';' + Copy(asl[I], 4, Length(asl[I]) - 4);
          asl.Delete(I);
        end
        else if CompareText('-I', Copy(asl[I], 1, 2)) = 0 then
        begin
          IPath := ';' + Copy(asl[I], 4, Length(asl[I]) - 4);
          asl.Delete(I);
        end
        else if CompareText('-O', Copy(asl[I], 1, 2)) = 0 then
        begin
          OPath := ';' + Copy(asl[I], 4, Length(asl[I]) - 4);
          asl.Delete(I);
        end
        else if CompareText('-E', Copy(asl[I], 1, 2)) = 0 then
        begin
          if OutputDir <> '' then
            asl.Delete(I)
          else
            Inc(I);
        end
        else
          Inc(I);
      end
      else
        Inc(I);
  end;

  procedure CheckDofFile;
  begin
    if FileExists(ChangeFileExt(DelphiFilename, '.dof')) then
      with TIniFile.Create(ChangeFileExt(DelphiFilename, '.dof')) do
        try
          NOPath := ReadString(stdofDirectories, stdofUnitOutputDir, '');
          LULibs := ReadString(stdofLinker, stdofPackages, '');
          UPath := ';' + ReadString(stdofDirectories, stdofSearchPath, '');
          RPath := UPath;
          IPath := UPath;
          OPath := UPath;
          OutputDir := ReadString(stdofDirectories, stdofEXEOutputDir, '');
        finally
          Free;
        end;
  end;

  procedure BuildDCCCfg01;
  VAR
    pList: TStringList;
  begin
    // sl1.Add('-U"' + GetDelphiDCPPath + ';' + GetDelphiSearchPath + UPath + '"');
    // sl1.Add('-R"' + GetDelphiDCPPath + ';' + GetDelphiSearchPath + RPath + '"');
    // sl1.Add('-I"' + GetDelphiDCPPath + ';' + GetDelphiSearchPath + IPath + '"');
    // sl1.Add('-O"' + GetDelphiDCPPath + ';' + GetDelphiSearchPath + OPath + '"');
    //
    pList := TStringList.Create;
    try
      omParseString(GetDelphiDCPPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoDequote, omPoRemPathDelim, omPoUniqueOnly]);
      omParseString(GetDelphiSearchPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoDequote, omPoRemPathDelim, omPoUniqueOnly]);
      omParseString(UPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoDequote, omPoRemPathDelim, omPoUniqueOnly]);

      omPrefixStrings('-I', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
      omPrefixStrings('-R', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
      omPrefixStrings('-U', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
      omPrefixStrings('-O', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
    finally
      FreeAndNil(pList);
    end;

    // UnitOutputDir
    if NOPath <> '' then
      sl1.Add('-NO"' + NOPath + '"');

    if LULibs <> '' then
      sl1.Add('-LU"' + LULibs + '"');

    // set output directory
    if GetIsProject(_DelphiFilename) then
    begin
      if OutputDir <> '' then
      begin
        sl1.Add('-E"' + ReplaceDelphiPathVars(_OutputDir) + '"');
      end;
    end
    else if GetIsPackage(_DelphiFilename) then
    begin
      if _OutputDir <> '' then
      begin
        sl1.Add('/LE"' + ReplaceDelphiPathVars(_OutputDir) + '"');
        if not DirectoryExists(ReplaceDelphiPathVars(_OutputDir)) then
          ForceDirectories(ReplaceDelphiPathVars(_OutputDir));
      end
      else
      begin
        sl1.Add('/LE"' + GetDelphiBPLPath + '"');
        if not DirectoryExists(GetDelphiBPLPath) then
          ForceDirectories(GetDelphiBPLPath);
      end;
      sl1.Add('/LN"' + GetDelphiDCPPath + '"');
      if not DirectoryExists(GetDelphiDCPPath) then
        ForceDirectories(GetDelphiDCPPath);
    end;

    // Compiler switch
    if CompilerSwitch <> '' then
      sl1.Add(StringReplace(MakeStudio.Variables.ReplaceVarsInString(CompilerSwitch), ' ', #10#13, [rfReplaceAll, rfIgnoreCase]));
    sl1.SaveToFile(SrcPath + 'dcc32.cfg');
  end;

  procedure BuildDCCCfg02;
  VAR
    pList: TStringList;
  begin
    // sl1.Add('-I' + QuoteSeparetedList(GetDelphiLangPath + ';' + GetDelphiSearchPath + ';' + UPath + ';' +
    // GetDelphiDCPPath));
    // sl1.Add('-R' + QuoteSeparetedList(GetDelphiLangPath + ';' + GetDelphiSearchPath + ';' + UPath + ';' +
    // GetDelphiDCPPath));
    // sl1.Add('-U' + QuoteSeparetedList(GetDelphiLangPath + ';' + GetDelphiSearchPath + ';' + UPath + ';' +
    // GetDelphiDCPPath));
    // sl1.Add('-O' + QuoteSeparetedList(GetDelphiLangPath + ';' + GetDelphiSearchPath + ';' + UPath + ';' +
    // GetDelphiDCPPath));

    pList := TStringList.Create;
    try
      omParseString(GetDelphiLangPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoRemPathDelim, omPoDequote, omPoUniqueOnly]);
      omParseString(GetDelphiDCPPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoRemPathDelim, omPoDequote, omPoUniqueOnly]);
      omParseString(GetDelphiSearchPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoRemPathDelim, omPoDequote, omPoUniqueOnly]);
      omParseString(UPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoRemPathDelim, omPoDequote, omPoUniqueOnly]);

      omPrefixStrings('-I', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
      omPrefixStrings('-R', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
      omPrefixStrings('-U', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
      omPrefixStrings('-O', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
    finally
      FreeAndNil(pList);
    end;

    // Namespace
    if MakeStudio.Variables.VarExists(stvarNamespaces) then
      if MakeStudio.Variables.Values[stvarNamespaces] <> '' then
        sl1.Add('-NS' + MakeStudio.Variables.Values[stvarNamespaces]);

    // UnitOutputDir
    if NOPath <> '' then
      sl1.Add('-NO' + QuoteSeparetedList(NOPath));

    if LULibs <> '' then
      sl1.Add('-LU"' + LULibs + '"');

    // set output directory
    if GetIsProject(_DelphiFilename) then
    begin
      if OutputDir <> '' then
      begin
        sl1.Add('-E"' + ReplaceDelphiPathVars(_OutputDir) + '"');
      end;
    end
    else if GetIsPackage(_DelphiFilename) then
    begin
      if _OutputDir <> '' then
      begin
        sl1.Add('/LE"' + ReplaceDelphiPathVars(_OutputDir) + '"');
        if not DirectoryExists(ReplaceDelphiPathVars(_OutputDir)) then
          ForceDirectories(ReplaceDelphiPathVars(_OutputDir));
      end
      else
      begin
        sl1.Add('/LE"' + GetDelphiBPLPath + '"');
        if not DirectoryExists(GetDelphiBPLPath) then
          ForceDirectories(GetDelphiBPLPath);
      end;
      sl1.Add('/LN"' + GetDelphiDCPPath + '"');
      if not DirectoryExists(GetDelphiDCPPath) then
        ForceDirectories(GetDelphiDCPPath);
    end;

    // Compiler switch
    if CompilerSwitch <> '' then
      sl1.Add(StringReplace(MakeStudio.Variables.ReplaceVarsInString(CompilerSwitch), ' ', #10#13, [rfReplaceAll, rfIgnoreCase]));

    // FileDelete( SrcPath + 'dcc32.cfg');
    case GetCompilerPlatform of
      dpOSX32:
        sl1.SaveToFile(SrcPath + 'dccosx.cfg');
      dpWin32:
        sl1.SaveToFile(SrcPath + 'dcc32.cfg');
      dpWin64:
        sl1.SaveToFile(SrcPath + 'dcc64.cfg');
    end;

  end;

  procedure BuildDCCCfg03;
  VAR
    pList: TStringList;
  begin
    // if GetLANGDIR <> '' then
    // begin
    // sl1.Add('-I' + QuoteSeparetedList(GetDelphiLangPath + ';' + GetDelphiSearchPath + ';' + UPath + ';' +
    // GetDelphiDCPPath));
    // sl1.Add('-R' + QuoteSeparetedList(GetDelphiLangPath + ';' + GetDelphiSearchPath + ';' + UPath + ';' +
    // GetDelphiDCPPath));
    // sl1.Add('-U' + QuoteSeparetedList(GetDelphiLangPath + ';' + GetDelphiSearchPath + ';' + UPath + ';' +
    // GetDelphiDCPPath));
    // sl1.Add('-O' + QuoteSeparetedList(GetDelphiLangPath + ';' + GetDelphiSearchPath + ';' + UPath + ';' +
    // GetDelphiDCPPath));
    // end
    // else
    // begin
    // sl1.Add('-I' + QuoteSeparetedList(GetDelphiSearchPath + ';' + UPath + ';' + GetDelphiDCPPath));
    // sl1.Add('-R' + QuoteSeparetedList(GetDelphiSearchPath + ';' + UPath + ';' + GetDelphiDCPPath));
    // sl1.Add('-U' + QuoteSeparetedList(GetDelphiSearchPath + ';' + UPath + ';' + GetDelphiDCPPath));
    // sl1.Add('-O' + QuoteSeparetedList(GetDelphiSearchPath + ';' + UPath + ';' + GetDelphiDCPPath));
    // end;
    //

    pList := TStringList.Create;
    try
      if GetLANGDIR <> '' then
        omParseString(GetDelphiLangPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoRemPathDelim, omPoDequote, omPoUniqueOnly]);
      omParseString(GetDelphiDCPPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoRemPathDelim, omPoDequote, omPoUniqueOnly]);
      omParseString(GetDelphiSearchPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoRemPathDelim, omPoDequote, omPoUniqueOnly]);
      omParseString(UPath, ';', pList, [omPoTrimFields, omPoNoEmptyFields, omPoRemPathDelim, omPoDequote, omPoUniqueOnly]);

      omPrefixStrings('-I', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
      omPrefixStrings('-R', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
      omPrefixStrings('-U', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
      omPrefixStrings('-O', pList, [omPrDoubleQuotes, omPrNoEmptyFields], sl1);
    finally
      FreeAndNil(pList);
    end;

    // Namespace
    if MakeStudio.Variables.VarExists(stvarNamespaces) then
      if MakeStudio.Variables.Values[stvarNamespaces] <> '' then
        sl1.Add('-NS' + MakeStudio.Variables.Values[stvarNamespaces]);

    // UnitOutputDir
    if NOPath <> '' then
      sl1.Add('-NO' + QuoteSeparetedList(NOPath));

    if LULibs <> '' then
      sl1.Add('-LU"' + LULibs + '"');

    // set output directory
    if GetIsProject(_DelphiFilename) then
    begin
      if OutputDir <> '' then
      begin
        sl1.Add('-E"' + ReplaceDelphiPathVars(_OutputDir) + '"');
      end;
    end
    else if GetIsPackage(_DelphiFilename) then
    begin
      if _OutputDir <> '' then
      begin
        sl1.Add('/LE"' + ReplaceDelphiPathVars(_OutputDir) + '"');
        if not DirectoryExists(ReplaceDelphiPathVars(_OutputDir)) then
          ForceDirectories(ReplaceDelphiPathVars(_OutputDir));
      end
      else
      begin
        sl1.Add('/LE"' + GetDelphiBPLPath);
        if not DirectoryExists(GetDelphiBPLPath) then
          ForceDirectories(GetDelphiBPLPath);
      end;
      sl1.Add('/LN"' + GetDelphiDCPPath);
      if not DirectoryExists(GetDelphiDCPPath) then
        ForceDirectories(GetDelphiDCPPath);
    end;

    // Compiler switch
    if CompilerSwitch <> '' then
      sl1.Add(StringReplace(MakeStudio.Variables.ReplaceVarsInString(CompilerSwitch), ' ', #10#13, [rfReplaceAll, rfIgnoreCase]));

    // FileDelete( SrcPath + 'dcc32.cfg');
    case GetCompilerPlatform of
      dpOSX32:
        sl1.SaveToFile(SrcPath + 'dccosx.cfg');
      dpWin32:
        sl1.SaveToFile(SrcPath + 'dcc32.cfg');
      dpWin64:
        sl1.SaveToFile(SrcPath + 'dcc64.cfg');
    end;

  end;

  function CommandLineFromStringList(_sl: TStringList): String;
  begin
    Result := StringReplace(_sl.Text, #10, ' ', [rfReplaceAll]);
    Result := StringReplace(Result, #13, '', [rfReplaceAll]);
  end;

begin
  Result := false;

  UPath := '';
  RPath := '';
  OPath := '';
  IPath := '';
  NPath := '';
  NOPath := '';

  if GetDelphiBPLPath = '' then
    Exit;

  // Get source directories
  Path := PathAddSeparator(MakeStudio.ApplicationDataFolder);
  SrcPath := ExtractFilePath(_DelphiFilename);

  MakeStudio.LogMessage(stdStartingCompiler);

  // create directory for the compiler results
  ForceDirectories(Path + 'Compile');

  sl1 := TStringList.Create;
  // test
  try

    // if internal makefile exists, just copy to dcc32.cfg
    if FileExists(SrcPath + 'make.cfg') then
      FileCopy(SrcPath + 'make.cfg', SrcPath + 'dcc32.cfg', True)

    else
    begin
      // create dcc32.cfg
      CheckDofFile;

      if FileExists(ChangeFileExt(_DelphiFilename, '.cfg')) then
      begin
        sl1.LoadFromFile(ChangeFileExt(_DelphiFilename, '.cfg'));
        CheckCfgFile(sl1);
      end;

      if GetDelphiVersion < dverXE2 then
        BuildDCCCfg01
      else if GetDelphiVersion < dverXE4 then
        BuildDCCCfg02
      else
        BuildDCCCfg03;

    end;

    if FileExists(ChangeFileExt(_DelphiFilename, '.cfg')) then
      RenameFile(ChangeFileExt(_DelphiFilename, '.cfg'), ChangeFileExt(_DelphiFilename, '.cf1'));

    // Executing Compiler
    MakeStudio.LogMessage(GetDelphiCompiler + ' ' + '"' + _DelphiFilename + '"');

    Result := MakeStudio.ExecCmdLine(GetDelphiCompiler, '"' + _DelphiFilename + '"', ExtractFilePath(_DelphiFilename),
      IExecCallback(Self)) = 0;

    Wait_a_While(1000);

    if not Result then
      MakeStudio.LogMessage(stderrCompile);

  finally
    sl1.Free;
    if FileExists(ChangeFileExt(_DelphiFilename, '.cf1')) then
      RenameFile(ChangeFileExt(_DelphiFilename, '.cf1'), ChangeFileExt(_DelphiFilename, '.cfg'));
  end;
end;

type
  dllproc = function: HResult;

procedure TDelphi32Module.RegisterPackage;

  function BplName: String;
  begin
    Result := GetPackagePrefix(_DelphiFilename) + ExtractFilename(ChangeFileExt(_DelphiFilename, '')) +
      GetPackageSuffix(_DelphiFilename) + '.bpl';
  end;

var
  reg: TRegistry;
  aFilename, S: string;
  sl: TStringList;
  Found: Boolean;
  I: Integer;
begin
  aFilename := '';

  if OutputDir <> '' then
    aFilename := CheckBackslash(_OutputDir) + BplName
  else
    aFilename := CheckBackslash(GetDelphiBPLPath) + BplName;

  reg := TRegistry.Create;
  sl := TStringList.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(GetDelphiRootKey + stdPackagesKey, false) then
      try
        reg.GetValueNames(sl);
        Found := false;
        for I := 0 to sl.Count - 1 do
        begin
          S := ReplaceDelphiPathVars(sl[I]);
          if CompareText(S, aFilename) = 0 then
          begin
            Found := True;
            Break;
          end;
        end;
        // lookup if the package already is installed in another directory
        if not Found then
        begin
          for I := 0 to sl.Count - 1 do
          begin
            S := ReplaceDelphiPathVars(sl[I]);

            if SameText(ExtractFilename(aFilename), ExtractFilename(S)) then
            begin
              MakeStudio.LogMessage('Filename = ' + aFilename);
              MakeStudio.LogMessage('RegisteredPackage = ' + S);
              reg.DeleteValue(sl[I]); // delete if exists
              MakeStudio.LogMessage(Format(stdRegisteredPackageDeleted, [sl[I]]));
              Break;
            end;
          end;
        end;

        if not Found then
        begin
          reg.WriteString(aFilename, GetPackageDescription(_DelphiFilename));
          MakeStudio.LogMessage(Format(stdPackageRegistered, [aFilename]));
        end
        else
          MakeStudio.LogMessage(Format(stdPackageAlreadyRegistered, [aFilename]));

      except
      end;
  finally
    reg.Free;
  end;
end;

procedure TDelphi32Module.UnRegisterPackage;
var
  reg: TRegistry;
  aFilename, S: string;
  sl: TStringList;
  I: Integer;
begin
  aFilename := '';
  if _OutputDir <> '' then
    aFilename := CheckBackslash(_OutputDir) + ChangeFileExt(ExtractFilename(_DelphiFilename), '.bpl')
  else
    aFilename := ChangeFileExt(_DelphiFilename, '.bpl');

  reg := TRegistry.Create;
  sl := TStringList.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey(GetDelphiRootKey + stdPackagesKey, false);
    try
      reg.GetValueNames(sl);
      for I := 0 to sl.Count - 1 do
      begin
        S := ReplaceDelphiPathVars(sl[I]);
        if CompareText(S, aFilename) = 0 then
        begin
          reg.DeleteValue(sl[I]);
          MakeStudio.LogMessage(Format(stdPackageUnRegistered, [aFilename]));
          Break;
        end;
      end;
    except
    end;
  finally
    reg.Free;
  end;
end;

procedure TDelphi32Module.UnRegisterCOMDLL;
var
  H: THandle;
  h1: HResult;
  P: dllproc;
  err: Integer;
  errm: array [0 .. 255] of Char;
  aFilename: string;
begin
  aFilename := '';
  if _OutputDir <> '' then
    aFilename := CheckBackslash(_OutputDir) + ExtractFilename(_DelphiFilename)
  else
    aFilename := _DelphiFilename;

  MakeStudio.LogMessage('-----------------------------------------');
  MakeStudio.LogMessage('Loading DLL: ' + aFilename);
  H := LoadLibrary(PChar(aFilename));
  if H > 0 then
  begin
    MakeStudio.LogMessage('DLL "' + aFilename + '" loaded"');
    MakeStudio.LogMessage('Getting DLLUnregisterServer Proc');
    @P := GetProcAddress(H, 'DllUnregisterServer');
    if @P <> nil then
    begin
      MakeStudio.LogMessage('Calling DLLUnregisterServer Proc');
      h1 := P;
      MakeStudio.LogMessage('Result=' + IntToStr(h1));
    end
    else
      MakeStudio.LogMessage('DLLRegisterServer not Exported');
    MakeStudio.LogMessage('FreeLibrary: "' + aFilename + '"');
    FreeLibrary(H);
  end
  else
  begin
    err := GetLastError;
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, err, 0, errm, 255, nil);
    MakeStudio.LogMessage(StrPas(errm));
    MakeStudio.LogMessage('ERROR Loading DLL: ' + aFilename);
  end;
  MakeStudio.LogMessage('Ready...');
end;

// todo - merge with UnRegister ?
procedure TDelphi32Module.RegisterCOMDLL;
var
  H: THandle;
  h1: HResult;
  P: dllproc;
  err: Integer;
  errm: array [0 .. 255] of Char;
  aFilename: string;
begin
  aFilename := '';
  if _OutputDir <> '' then
    aFilename := CheckBackslash(_OutputDir) + ExtractFilename(_DelphiFilename)
  else
    aFilename := _DelphiFilename;

  MakeStudio.LogMessage('-----------------------------------------');
  MakeStudio.LogMessage('Loading DLL: ' + aFilename);
  H := LoadLibrary(PChar(aFilename));
  if H > 0 then
  begin
    MakeStudio.LogMessage('DLL "' + aFilename + '" loaded"');
    MakeStudio.LogMessage('Getting DLLRegisterServer Proc');
    @P := GetProcAddress(H, 'DllRegisterServer');
    if @P <> nil then
    begin
      MakeStudio.LogMessage('Calling DLLRegisterServer Proc');
      h1 := P;
      MakeStudio.LogMessage('Result=' + IntToStr(h1));
    end
    else
      MakeStudio.LogMessage('DLLRegisterServer not Exported');
    MakeStudio.LogMessage('FreeLibrary: "' + aFilename + '"');
    FreeLibrary(H);
  end
  else
  begin
    err := GetLastError;
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, err, 0, errm, 255, nil);
    MakeStudio.LogMessage(StrPas(errm));
    MakeStudio.LogMessage('ERROR Loading DLL: ' + aFilename);
  end;
  MakeStudio.LogMessage('Ready...');
end;

procedure TDelphi32Module.RegisterCOMExe;
var
  S: string;
  R: Boolean;
begin
  S := '';
  if _OutputDir <> '' then
    S := CheckBackslash(_OutputDir) + ChangeFileExt(ExtractFilename(_DelphiFilename), '.exe')
  else
    S := ChangeFileExt(_DelphiFilename, '.exe');

  S := S; // ???
  MakeStudio.LogMessage(Format(stdRegisteringServer, [S]));

  // Executing EXE
  R := MakeStudio.ExecCmdLine(S, '/REGSERVER', ExtractFilePath(S), IExecCallback(Self)) = 0;

  if not R then
    MakeStudio.LogMessage(stderrRegister)
  else
    MakeStudio.LogMessage(Format(stdServerRegistered, [S]));
end;

// todo - merge with RegisterCOMExe ?
procedure TDelphi32Module.UnRegisterCOMExe;
var
  S: string;
begin
  S := '';
  if _OutputDir <> '' then
    S := CheckBackslash(_OutputDir) + ChangeFileExt(ExtractFilename(_DelphiFilename), '.exe')
  else
    S := ChangeFileExt(_DelphiFilename, '.exe');

  S := S; // ???
  MakeStudio.LogMessage(Format(stdUNRegisteringServer, [S]));

  // Executing EXE
  if MakeStudio.ExecCmdLine(S, '/UNREGSERVER', ExtractFilePath(S), IExecCallback(Self)) < 0 then
    MakeStudio.LogMessage(stderrRegister)
  else
    MakeStudio.LogMessage(Format(stdServerRegistered, [S]));
end;

procedure TDelphi32Module.SetSearchPath;
var
  pf, pfStored: TCompilerPlatform;
begin
  MakeStudio.LogMessage('');
  MakeStudio.LogMessage(stdBreak);

  if SearchDirs.Count > 0 then
  begin
    if AllPlatforms then
    begin

      pfStored := GetCompilerPlatform;

      for pf := Low(TCompilerPlatform) to High(TCompilerPlatform) do
      begin
        SetCompilerPlatform(pf);
        AddPathListToDelphiPath(SearchDirs);
      end;

      SetCompilerPlatform(pfStored);
    end
    else
      AddPathListToDelphiPath(SearchDirs);
  end;
end;

procedure TDelphi32Module.RunBatchfile;
begin
  MakeStudio.LogMessage(stdStartingBatch);
  MakeStudio.LogMessage(_DelphiFilename + ' ' + CompilerSwitch);

  // Executing Batch
  if MakeStudio.ExecCmdLine(_DelphiFilename, CompilerSwitch, ExtractFilePath(_DelphiFilename), IExecCallback(Self)) < 0 then
    MakeStudio.LogMessage(stderrRunningBatch);
end;

procedure TDelphi32Module.CaptureOutput(const Line: WideString; var Aborted: WordBool);

  function HasText(Text: string; const Values: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    Text := AnsiLowerCase(Text);
    for I := 0 to High(Values) do
      if Pos(Values[I], Text) > 0 then
        Exit;
    Result := false;
  end;

  function IsCompileFileLine(const AText: string): Boolean;

    function PosLast(Ch: Char; const S: string): Integer;
    begin
      for Result := Length(S) downto 1 do
        if S[Result] = Ch then
          Exit;
      Result := 0;
    end;

  var
    ps, psEnd: Integer;
  begin
    Result := false;
    ps := PosLast('(', AText);
    if (ps > 0) and (Pos(': ', AText) = 0) and (Pos('.', AText) > 0) then
    begin
      psEnd := PosLast(')', AText);
      Result := psEnd > ps;
    end;
  end;

  procedure OutputText(AText: string);
  var
    _s: string;
  begin
    _s := '';
    if IsCompileFileLine(AText) then
    begin
      if HasText(AText, ['hint: ', 'hinweis: ', 'suggestion: ']) then
        // do not localize
        _s := AText
      else if HasText(AText, ['warning: ', 'warnung: ', 'avertissement: ']) then
        // do not localize
        _s := AText
      else if HasText(AText, ['error: ', 'fehler: ', 'erreur: ']) then
        // do not localize
        _s := AText
      else if HasText(AText, ['fatal: ', 'schwerwiegend']) then
        // do not localize
        _s := AText;
    end
    else
      _s := AText;

    if _s <> '' then
      if _s <> FLastProcessOutput then
      begin
        MakeStudio.LogMessage(_s);
        FLastProcessOutput := _s;
      end
      else
    else
      MakeStudio.SetStatus(AText);
  end;

begin
  Aborted := Canceled;
  OutputText(Line);
end;

function TDelphi32Module.Get_ParamValues(const ParamName: WideString): WideString;
var
  I: Integer;
begin
  if ParamName = stdcFilename then
    Result := DelphiFilename
  else if ParamName = stdcDMakAction then
    Result := IntToStr(Ord(DMakAction))
  else if ParamName = stdcCompilerSwitch then
    Result := CompilerSwitch
  else if ParamName = stdcAllPlatforms then
    Result := BoolToStr(AllPlatforms, True)
  else if ParamName = stdcOutputDir then
    Result := OutputDir
  else if ParamName = stdcSearchCount then
  begin
    Result := IntToStr(SearchDirs.Count);
  end
  else
  begin
    for I := 0 to SearchDirs.Count - 1 do
      if SameText(Format(stdcSearchDirs, [I + 1]), ParamName) then
      begin
        Result := SearchDirs[I];
        Break;
      end;
  end;
end;

procedure TDelphi32Module.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var
  I: Integer;
begin
  if ParamName = stdcFilename then
    DelphiFilename := Value
  else if ParamName = stdcDMakAction then
    DMakAction := TDMakAction(StrToInt(Value))
  else if ParamName = stdcCompilerSwitch then
    CompilerSwitch := Value
  else if ParamName = stdcAllPlatforms then
    AllPlatforms := StrToBool(Value)
  else if ParamName = stdcOutputDir then
    OutputDir := Value
  else if ParamName = stdcSearchCount then
  begin
    SearchDirs.Clear;
    for I := 0 to StrToInt(Value) - 1 do
      SearchDirs.Add('');
  end
  else
  begin
    for I := 0 to SearchDirs.Count - 1 do
      if SameText(Format(stdcSearchDirs, [I + 1]), ParamName) then
      begin
        SearchDirs[I] := Value;
        Break;
      end;
  end;
end;

function TDelphi32Module.Get_ParamNames(Index: Integer): WideString;
begin
  case Index of
    0:
      Result := stdcFilename;
    1:
      Result := stdcDMakAction;
    2:
      Result := stdcCompilerSwitch;
    3:
      Result := stdcOutputDir;
    4:
      Result := stdcSearchCount;
    5:
      Result := stdcAllPlatforms;
  else
    begin
      Result := Format(stdcSearchDirs, [Index - 5]);
    end;
  end;
end;

function TDelphi32Module.Get_ParamCount: Integer;
begin
  Result := 6 + SearchDirs.Count;
end;

function TDelphi32ModuleCallback.GetIdentifier: WideString;
begin
  Result := IDDelphi32Module;
end;

procedure TDelphi32Module.RegisterDllExpert;
var
  reg: TRegistry;
  aFilename, S: string;
  sl: TStringList;
  Found: Boolean;
  I: Integer;
begin
  aFilename := '';

  if _OutputDir <> '' then
    aFilename := CheckBackslash(_OutputDir) + ChangeFileExt(ExtractFilename(_DelphiFilename), '.dll');

  if not FileExists(aFilename) then
    aFilename := ChangeFileExt(_DelphiFilename, '.dll');

  reg := TRegistry.Create;
  sl := TStringList.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(GetDelphiRootKey + stdExpertsKey, True) then
      try
        reg.GetValueNames(sl);
        Found := false;
        for I := 0 to sl.Count - 1 do
        begin
          S := ReplaceDelphiPathVars(sl[I]);
          if CompareText(S, aFilename) = 0 then
          begin
            Found := True;
            Break;
          end;
        end;

        // lookup if the package already is installed in another directory
        if not Found then
        begin
          for I := 0 to sl.Count - 1 do
          begin
            S := ReplaceDelphiPathVars(sl[I]);

            if SameText(ExtractFilename(aFilename), ExtractFilename(S)) then
            begin
              MakeStudio.LogMessage('Filename = ' + aFilename);
              MakeStudio.LogMessage('RegisteredExpert = ' + S);
              reg.DeleteValue(sl[I]); // delete if exists
              MakeStudio.LogMessage(Format(stdRegisteredExpertDeleted, [sl[I]]));
              Break;
            end;
          end;
        end;

        if not Found then
        begin
          reg.WriteString(ChangeFileExt(ExtractFilename(aFilename), ''), aFilename);
          MakeStudio.LogMessage(Format(stdExpertRegistered, [aFilename]));
        end
        else
          MakeStudio.LogMessage(Format(stdExpertAlreadyRegistered, [aFilename]));

      except
      end;
  finally
    reg.Free;
  end;
end;

procedure TDelphi32Module.UnRegisterAndDeleteBPL;
begin
  if DeleteFile(_DelphiFilename) then
  begin
    MakeStudio.LogMessage(Format(stdRegisteredPackageFileDeleted, [_DelphiFilename]));
    UnRegisterPackage;
  end
  else
    MakeStudio.LogMessage(Format(stdRegisteredPackageFileNotDeleted, [_DelphiFilename]));
end;

procedure TDelphi32Module.DeleteDCP;
begin
  if DeleteFile(_DelphiFilename) then
  begin
    MakeStudio.LogMessage(Format(stdRegisteredPackageFileDeleted, [_DelphiFilename]));
  end
  else
    MakeStudio.LogMessage(Format(stdRegisteredPackageFileNotDeleted, [_DelphiFilename]));
end;

procedure TDelphi32Module.RemoveAllUserPackages;

// returns all registered delphi packages in capitals
// and full path
  procedure GetDelphiPackages(sl: TStringList);
  var
    reg: TRegistry;
    I: Integer;
  begin
    sl.Clear;
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CURRENT_USER;
      if reg.OpenKey(GetDelphiRootKey + stdPackagesKey, false) then
        try
          reg.GetValueNames(sl);
          for I := 0 to sl.Count - 1 do
            sl[I] := ReplaceDelphiPathVars(sl[I]);
        except
        end;
    finally
      reg.Free;
    end;
  end;

  function IsUserPackage(aFilename: String): Boolean;
  var
    s1, s2: String;
    I: Integer;
  begin
    // Delete only packages in the given search path
    Result := false;
    for I := 0 to SearchDirs.Count - 1 do
    begin
      s1 := UpperCase(IncludeTrailingPathDelimiter(SearchDirs[I]));
      s2 := UpperCase(IncludeTrailingPathDelimiter(ExtractFilePath(aFilename)));
      Result := SameText(s1, s2);
      if Result then
        Break;
    end;
    // // Check if D7 or less
    // if GetDelphiVersion < dver2005 then
    // s1 := UpperCase(GetDelphiRootPathLong + 'bin')
    // else
    // s1 := UpperCase( GetDelphiRootPathLong);
    //
    // s2 := UpperCase( ExtractShortPathName(s1));
    // s3 := UpperCase( aFilename);
    // Result := not((Pos(s1, s3) <> 0) or (Pos(s2, s3) <> 0));
  end;

  function GetDCPFile(aBPLFilename: String): String;
  var
    s1, s2: String;
  begin
    s1 := ChangeFileExt(aBPLFilename, stExtDCP);
    s2 := PathAddSeparator(GetDelphiDCPPath) + ExtractFilename(s1);
    if FileExists(s1) then
      Result := s1
    else if FileExists(s2) then
      Result := s2
    else
      Result := SysUtils.FileSearch(ExtractFilename(s1), GetDelphiSearchPath);
  end;

  procedure DeletePackage(aFilename: String);
  var
    reg: TRegistry;
    sl: TStringList;
    I: Integer;
    S, s1: String;
  begin
    reg := TRegistry.Create;
    sl := TStringList.Create;
    try
      reg.RootKey := HKEY_CURRENT_USER;
      reg.OpenKey(GetDelphiRootKey + stdPackagesKey, false);
      try
        reg.GetValueNames(sl);
        for I := 0 to sl.Count - 1 do
        begin
          S := ReplaceDelphiPathVars(sl[I]);
          if SameText(ExtractFilename(S), ExtractFilename(aFilename)) then
          begin

            // remove from registry
            if reg.DeleteValue(sl[I]) then
              MakeStudio.LogMessage(Format(stdPackageUnRegistered, [aFilename]))
            else
              MakeStudio.LogMessage(Format(stdPackageNotUnRegistered, [aFilename]));

            // delete bpl
            if DeleteFile(S) then
              MakeStudio.LogMessage(Format(stdPackageBPLDeleted, [aFilename]))
            else
              MakeStudio.LogMessage(Format(stdPackageBPLNotDeleted, [aFilename]));

            // delete dcp if exists
            s1 := GetDCPFile(S);
            if s1 <> '' then
              if DeleteFile(GetDCPFile(S)) then
                MakeStudio.LogMessage(Format(stdPackageDCPDeleted, [GetDCPFile(S)]))
              else
                MakeStudio.LogMessage(Format(stdPackageDCPNotDeleted, [GetDCPFile(S)]))
            else
              MakeStudio.LogMessage(Format(stdDependingDCPNotDeleted, [S]));

            Break;
          end;
        end;
      except
      end;
    finally
      reg.Free;
    end;
  end;

var
  RegisteredBPLs: TStringList;
  I: Integer;
begin
  RegisteredBPLs := TStringList.Create;
  try
    GetDelphiPackages(RegisteredBPLs);
    for I := 0 to RegisteredBPLs.Count - 1 do
      if IsUserPackage(RegisteredBPLs[I]) then
        DeletePackage(RegisteredBPLs[I]);
  finally
    RegisteredBPLs.Free;
  end;
end;

procedure TDelphi32Module.RemoveSearchPath;
begin
  if SearchDirs.Count > 0 then
    if DMakAction = dmaRemoveSearchPath then
      RemovePathListFromDelphiPath(SearchDirs, false)
    else
      RemovePathListFromDelphiPath(SearchDirs, True);
end;

procedure TDelphi32Module.ResetSearchPath;
var
  S: String;
begin
  MakeStudio.LogMessage('ResetSearchPath:');
  XUTILSROOTKEY := HKEY_LOCAL_MACHINE;
  MakeStudio.LogMessage('Key=HKEY_LOCAL_MACHINE');

  // read search path from HKLM
  S := ReadRegStringLM(GetDelphiRootKey + GetLibraryKey, stdcSearchPath, '');
  MakeStudio.LogMessage('Lese Path=' + GetDelphiRootKey + GetLibraryKey + '; Value=' + stdcSearchPath);
  MakeStudio.LogMessage('Result=' + S);

  XUTILSROOTKEY := HKEY_CURRENT_USER;

  MakeStudio.LogMessage('Key=HKEY_CURRENT_USER');
  MakeStudio.LogMessage('Schreibe Path=' + GetDelphiRootKey + GetLibraryKey + '; Value=' + stdcSearchPath);
  MakeStudio.LogMessage('Result=' + S);

  // write search path to HKCU
  WriteRegStringLM(GetDelphiRootKey + GetLibraryKey, stdcSearchPath, S);

  MakeStudio.LogMessage(Format(stdResetSearchPath, [S]));
end;

function TDelphi32Module.GetFilename: string;
begin
  Result := FFilename;
end;

function TDelphi32Module.GetOutputDir: string;
begin
  Result := FOutputDir;
end;

procedure TDelphi32Module.aSetFilename(const Value: string);
begin
  Caption := stActions[DMakAction] + ' (' + ExtractFilename(Value) + ')';
  FFilename := Value;
end;

function TDelphi32Module.GetRealDelphiFilename: String;
begin
  Result := MakeStudio.Variables.ReplaceVarsInString(DelphiFilename);
end;

function TDelphi32Module.GetRealOutputDir: String;
begin
  Result := ExcludeTrailingPathDelimiter(MakeStudio.Variables.ReplaceVarsInString(OutputDir));
end;

end.
