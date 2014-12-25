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


 ***************************************************************************

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:                                                                                    }

2006/05/28  BSchranz  - Help Merger Added
2006/06/07  USchuster - D5 fix

-----------------------------------------------------------------------------*)

unit mshelpmerger;

{$I jedi.inc}

interface

uses
  Classes, SysUtils, JclSysUtils, IniFiles, Registry, Windows, JclFileUtils;

type
  TCaptureHelpMergerLine = procedure(aLine: string; var aAbort: Boolean) of object;

  THelpMerger = class(TComponent)
  private
    FAvailabeCHMFiles: TStringList;
    FUsedCHMFiles: TStringList;
    FHHPFilename: string;
    FAbortExec: Boolean;
    FOnCaptureOutput: TCaptureHelpMergerLine;
    FOutputFile: string;
    FMainTopicCaption: string;
    function GetHelpFolder: string;
    function GetHHCFilename: string;
    function GetHHKFilename: string;
    function GetHTMFilename: string;
    function GetMainTopicCaption: string;
    function GetOutputFile: string;
  protected
    procedure DoCaptureLine(const Text: string); virtual;
    procedure BuildHHPFile; virtual;
    procedure BuildHHCFile; virtual;
    procedure PrepareFiles; virtual;
    procedure SearchCHMFiles; virtual;
    procedure FillUsedCHMFileList; virtual;
    procedure DlgHHCNotInstalled; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    function NeedCompile: Boolean;
    procedure DlgExecute;

    property HelpFolder: string read GetHelpFolder;
    property HHCFilename: string read GetHHCFilename;
    property HHKFilename: string read GetHHKFilename;
    property HTMFilename: string read GetHTMFilename;

  published
    property HHPFilename: string read FHHPFilename write FHHPFilename;
    property OutputFile: string read GetOutputFile write FOutputFile;
    property MainTopicCaption: string read GetMainTopicCaption write FMainTopicCaption;
    property OnCaptureOutput: TCaptureHelpMergerLine read FOnCaptureOutput write FOnCaptureOutput;
  end;

implementation

uses
  mshelpmergerWorkshopError, mshelpmergerRun;

const
  sMergeSection = 'MERGE FILES';
  sOptionsSection = 'OPTIONS';
  sCompiledFile = 'Compiled file';
  sInfoExt = '.txt';
  sContentExt = '.hhc';
  sContent = 'ContentFile';
  sChapter = 'Chapter';

resourcestring
  strDelete = 'Delete';
  strHTML = 'HTML File';
  strIgnore = 'Ignore';
  strError = 'Error';
  strUnknown = '???';
  strHHC = 'HHC.EXE';
  strAddingChapter = 'Chapter %s created';
  strAddingMergeFile = 'Help file %s merged';
  strFinished = 'Help file successfully created...';
  strCompile = 'Compiling .chm file';
  strStartingCompiler = 'Starting Compiler';
  strPleaseWait = 'Please wait...';
  strErrorHHCNotFount = 'The help compiler HHC.EXE could not be found!';
  strCR = '-----------------------------------------------------------';

procedure GetFileListEx(const Path, Extension: string; Options: Integer; List: TStrings);
var
  sr: TSearchRec;
  P: string;

  procedure Add;
  begin
    case Options of
      0:List.Add(ChangeFileExt(sr.Name, ''));
      1:List.Add(sr.Name);
      2:List.Add(P + sr.Name);
    end;
  end;

begin
  List.Clear;

  if Path = '' then Exit;
  P := PathAddSeparator(Path);

  if FindFirst(P + '*.' + Extension, SysUtils.faArchive, sr) = 0 then begin
    Add;
    while FindNext(sr) = 0 do Add;
    SysUtils.FindClose(sr);
  end;
end;

{ THelpMerger }

constructor THelpMerger.Create(AOwner: TComponent);
begin
  inherited;
  FAvailabeCHMFiles := TStringList.Create;
  FUsedCHMFiles := TStringList.Create;
end;

procedure THelpMerger.DoCaptureLine(const Text: string);
var
  sl: TStringList;
  S: string;
  I: Integer;
begin
  if (Pos(#10, Text) > 0) or (Pos(#13, Text) > 0) then
  begin
    S := StringReplace(Text, #10#13, #3, [rfReplaceAll]);
    S := StringReplace(S, #13#10, #3, [rfReplaceAll]);
    S := StringReplace(S, #13, #3, [rfReplaceAll]);
    S := StringReplace(S, #10, #3, [rfReplaceAll]);
    S := StringReplace(S, #3, #10, [rfReplaceAll]);

    sl := TStringList.Create;
    try
      sl.Text := S;
      for I := 0 to sl.Count - 1 do
        if Assigned(FOnCaptureOutput) then
          if S[I] <> '' then
            OnCaptureOutput(S[I], FAbortExec);
    finally
      sl.Free;
    end;
  end
  else
  if Assigned(FOnCaptureOutput) then
    OnCaptureOutput(Text, FAbortExec);
end;

destructor THelpMerger.Destroy;
begin
  FAvailabeCHMFiles.Free;
  FUsedCHMFiles.Free;
  inherited;
end;

function THelpMerger.Execute: Boolean;
var
  reg: TRegistry;
  HHCPath: string;
begin
  FAbortExec := False;
  Result := False;

  try
    reg := TRegistry.Create;
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists('Software\Microsoft\HTML Help Workshop') then begin
      reg.OpenKey('Software\Microsoft\HTML Help Workshop', False);
      if reg.ValueExists('InstallDir') then begin
        HHCPath := reg.ReadString('InstallDir');
        HHCPath := PathAddSeparator(HHCPath) + 'hhc.exe';
        if not FileExists(HHCPath) then begin
          DoCaptureLine(strErrorHHCNotFount);
          Exit;
        end;
      end
      else begin
        DlgHelpWorkshopNotInstalled;
        DoCaptureLine(strErrorHHCNotFount);
        Exit;
      end;
    end
    else begin
      DlgHelpWorkshopNotInstalled;
      DoCaptureLine(strErrorHHCNotFount);
      Exit;
    end;

    PrepareFiles;

    DoCaptureLine(strCR);
    DoCaptureLine(strStartingCompiler);
    DoCaptureLine(strPleaseWait);

    JclSysUtils.Execute('"' + HHCPath + '"' + ' "' + HHPFilename + '"',
      DoCaptureLine, True, @FAbortExec);

    DoCaptureLine('');
    DoCaptureLine(strCR);
    DoCaptureLine(strFinished);
    Result := True;
  except
    Result := false;
  end;
end;

function THelpMerger.NeedCompile: Boolean;
var
  I: Integer;

  function IsUsed(aFile: string): Boolean;
  var
    K: Integer;
  begin
    Result := False;
    for K := 0 to FUsedCHMFiles.Count - 1 do
      if SameText(aFile, FUsedCHMFiles[K]) then begin
        Result := True;
        Break;
      end;
  end;

begin
  SearchCHMFiles;

  Result := False;

  for I := 0 to FAvailabeCHMFiles.Count - 1 do
    if not IsUsed(FAvailabeCHMFiles[I]) then begin
      Result := True;
      Break;
    end;

  if not FileExists(OutputFile) then
    Result := True;
end;

procedure THelpMerger.PrepareFiles;
begin
  SearchCHMFiles;
  BuildHHPFile;
  BuildHHCFile;
end;

procedure THelpMerger.SearchCHMFiles;
var
  I: Integer;
begin
  GetFileListEx(GetHelpFolder, 'chm', 1, FAvailabeCHMFiles);

  //Falls die Ausgabedatei gefunden wurde, dann aus der Liste löschen
  for I := 0 to FAvailabeCHMFiles.Count - 1 do
    if SameText(ExtractFileName(OutputFile), FAvailabeCHMFiles[I]) then begin
      FAvailabeCHMFiles.Delete(I);
      Break;
    end;

  FillUsedCHMFileList;
end;

function THelpMerger.GetHelpFolder: string;
begin
  Result := PathAddSeparator(ExtractFilePath(HHPFilename));
end;

procedure THelpMerger.FillUsedCHMFileList;
var
  sl: TStringList;
  I: Integer;
  InSection: Boolean;
begin
  FUsedCHMFiles.Clear;

  if FileExists(HHPFilename) then begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(HHPFilename);
      InSection := False;
      for I := 0 to sl.Count - 1 do begin

        if sl[I] <> '' then begin
          if sl[I][1] = '[' then begin
            if InSection then begin
              InSection := False;
            end
            else
            if SameText(sl[I], '[' + sMergeSection + ']') then
              InSection := True;
          end
          else
          if InSection then
            FUsedCHMFiles.Add(sl[I]);
        end;

      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure THelpMerger.BuildHHCFile;
var
  sl: TStringList;
  mf: TStringList;

  procedure AddChapter(aChapter: string);
  begin
    sl.Add('<LI> <OBJECT type="text/sitemap">');
    sl.Add('<param name="Name" value="' + aChapter + '">');
    sl.Add('</OBJECT>');

    DoCaptureLine(Format(strAddingChapter, [aChapter]));

  end;

  function ContentFromMergeFile(aMergeFile: string): string;
  var
    sl1: TStringList;
    S: string;
  begin
    //liest die Datei aMergeFile.txt als Infodatei
    //und interpretiert die erste Zeile als Kapitel
    Result := ChangeFileExt(aMergeFile, sInfoExt);
    S := ChangeFileExt(HelpFolder + aMergeFile, sInfoExt);
    sl1 := TStringList.Create;
    try
      if FileExists(S) then begin
        sl1.LoadFromFile(S);
        if sl1.Values[sContent] <> '' then
          Result := sl1.Values[sContent];
      end;
    finally
      sl1.Free;
    end;
  end;

  procedure AddMergeFile(aMergeFile: string);
  var
    S, s1: string;
    K: Integer;
  begin
    S := ChangeFileExt(aMergeFile, '');
    s1:= ChangeFileExt(ContentFromMergeFile(aMergeFile), '');
    sl.Add('<OBJECT type="text/sitemap">');
    sl.Add('<param name="Merge" value="' + S + '.chm::\' + s1 + '.hhc">');
    sl.Add('</OBJECT>');

    DoCaptureLine(Format(strAddingMergeFile, [aMergeFile]));

    //Mergefile jetzt aus der mf-Liste löschen
    for K := 0 to mf.Count - 1 do
      if SameText(mf[K], aMergeFile) then begin
        mf.Delete(K);
        Break;
      end;
  end;

  function ChapterFromMergeFile(aMergeFile: string): string;
  var
    sl1: TStringList;
    S: string;
  begin
    //liest die Datei aMergeFile.txt als Infodatei
    //und interpretiert die erste Zeile als Kapitel
    Result := '';
    S := ChangeFileExt(HelpFolder + aMergeFile, sInfoExt);
    sl1 := TStringList.Create;
    try
      if FileExists(S) then begin
        sl1.LoadFromFile(S);
        if sl1.Values[sChapter] <> '' then
          Result := sl1.Values[sChapter];
      end;
    finally
      sl1.Free;
    end;
  end;

  function NextMergeFileWithChapter(aChapter: string): string;
  var
    K: Integer;
  begin
    Result := '';
    for K := 0 to mf.Count - 1 do
      if SameText(aChapter, ChapterFromMergeFile(mf[K])) then begin
        Result := mf[K];
        Break;
      end;
  end;

var
  Chapter: string;
begin
  if FileExists(HHCFilename) then begin
    if (FileGetAttr(HHCFilename) and faReadOnly) > 0 then
      FileSetAttr(HHCFilename, 0);
    SysUtils.DeleteFile(HHCFilename);
  end;

  if FAvailabeCHMFiles.Count=0 then
    SearchCHMFiles;

  sl := TStringList.Create;
  mf := TStringList.Create;
  try
    mf.Assign(FAvailabeCHMFiles);

    //Rumpf schreiben
    sl.Add('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
    sl.Add('<HTML>');
    sl.Add('<BODY>');
    sl.Add('<OBJECT type="text/site properties">');
    sl.Add('	<param name="FrameName" value="">');
    sl.Add('</OBJECT>');
    sl.Add('<UL>');
    sl.Add('<LI><OBJECT type="text/sitemap">');
    sl.Add('	<param name="Name" value="' + MainTopicCaption + '"> ');
    sl.Add('	<param name="Local" value="' + ExtractFileName(HTMFilename) + '">');
    sl.Add('</OBJECT>');

    // Dateien und Kapitel mergen...
    //erst alle ohne Kapitelangaben mergen
    while NextMergeFileWithChapter('') <> '' do
      AddMergeFile(NextMergeFileWithChapter(''));

    //Jetzt mit dem ersten Eintrag in der verbleibenden Liste weiter machen
    while mf.Count > 0 do begin
      Chapter := ChapterFromMergeFile(mf[0]);
      AddChapter(Chapter);
      AddMergeFile(mf[0]);
      while NextMergeFileWithChapter(Chapter) <> '' do
        AddMergeFile(NextMergeFileWithChapter(Chapter));
    end;

    sl.Add('</UL>');
    sl.Add('</BODY></HTML>');

    sl.SaveToFile(HHCFilename);

  finally
    sl.Free;
    mf.Free;
  end;
end;

procedure THelpMerger.BuildHHPFile;
var
  ini: TIniFile;
  sl: TStringList;
begin
  ForceDirectories( ExtractFilePath(HHPFilename));

  if (FileGetAttr(HHPFilename) and faReadOnly) > 0 then
    FileSetAttr(HHPFilename, 0);

  //Ini-File Parameter schreiben
  ini := TIniFile.Create(HHPFilename);
  try
    ini.WriteString(sOptionsSection, sCompiledFile, OutputFile);
    ini.EraseSection(sMergeSection);
  finally
    ini.Free;
  end;

  //Merge-Files schreiben
  sl := TStringList.Create;
  try
    sl.LoadFromFile(HHPFilename);
    sl.Add('[' + sMergeSection + ']');
    sl.AddStrings(FAvailabeCHMFiles);
    sl.SaveToFile(HHPFilename);
  finally
    sl.Free;
  end;
end;

function THelpMerger.GetHHCFilename: string;
begin
  Result := ChangeFileExt(HHPFilename, '.HHC');
end;

function THelpMerger.GetHHKFilename: string;
begin
  Result := ChangeFileExt(HHPFilename, '.HHK');
end;

function THelpMerger.GetHTMFilename: string;
begin
  Result := ChangeFileExt(HHPFilename, '.HTM');
end;

function THelpMerger.GetMainTopicCaption: string;
begin
  if FMainTopicCaption <> '' then
    Result := FMainTopicCaption
  else
  if HHPFilename <> '' then
    Result := ExtractFileName(ChangeFileExt(HHPFilename, ''));
end;

function THelpMerger.GetOutputFile: string;
begin
  if FOutputFile <> '' then
    Result := FOutputFile
  else
  if HHPFilename <> '' then
    Result := ExtractFileName(ChangeFileExt(HHPFilename, '.chm'));
end;

procedure THelpMerger.DlgHHCNotInstalled;
begin
  DlgHelpWorkshopNotInstalled;
end;

procedure THelpMerger.DlgExecute;
begin
  DlgHelpMergerRun(Self);
end;

end.
