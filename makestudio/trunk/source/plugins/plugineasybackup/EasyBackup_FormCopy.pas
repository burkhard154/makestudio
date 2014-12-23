(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EasyBackup_FormCopy.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/05  BSchranz  - Easybackup Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit EasyBackup_FormCopy;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Contnrs, msTLB, EasyBackup_Vars,
  EasyBackup_Utils, JclFileUtils;

type
  TSyncronizeOption = (soDelete, soCopy, soIgnore, soCopyAge);

  TMissingFile = class(TObject)
    SourceFile: string;
    TargetFile: string;
    SourceAge: TDateTime;
    CopyOption: TSyncronizeOption;
  end;

  TFormDoCopy = class(TForm)
    Animate1: TAnimate;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbFound: TLabel;
    lbCopied: TLabel;
    Button1: TButton;
    lbFile: TLabel;
    Label7: TLabel;
    lbDir: TLabel;
    Timer1: TTimer;
    Label2: TLabel;
    lbTarget: TLabel;
    Label5: TLabel;
    lbErrors: TLabel;
    Label8: TLabel;
    lbAction: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FFilesFound, FFilesCopied, FCountErrors: Integer;
    RootDir: string;
    statSourceFilesFound, statSourceFilesCopied,
    statBackupFilesFound, statBackupFilesCopied,
    statMissingFilesFound, statMissingFilesCopied,
    statMissingFilesDeleted, statMissingFilesIgnored: Integer;

    procedure DoCopyDir(SourceDir, TargetDir: string; MustExist: Boolean);
    procedure DoCopy;
    procedure DoCopyFile(SourceDir, TargetDir, Filename: string; MustExist: Boolean);

    procedure DoFindMissingDir(SourceDir, TargetDir: string);
    procedure DoFindMissing;
    procedure DoShowMissingDlg;
    procedure DoCopyMissing;
    procedure DoFindMissingFile(SourceDir, TargetDir, Filename: string);

    procedure SetFilesFound(Value: Integer);
    procedure SetFilesCopied(Value: Integer);
    procedure SetCountErrors(Value: Integer);
  public
    Source: TStringList;
    Target: string;
    MissingList: TObjectList;
    DoSynchronize: Boolean;
    SyncronizeOption: TSyncronizeOption;
    ShowSelectDlg: Boolean;
    SyncAge: Double;
    Cancel: Boolean;
    LogbookShowAll: Boolean;

    property FilesFound: Integer read FFilesFound write SetFilesFound;
    property FilesCopied: Integer read FFilesCopied write SetFilesCopied;
    property CountErrors: Integer read FCountErrors write SetCountErrors;
  end;

function GetSyncOptionName(aSyncOption: TSyncronizeOption): string;

implementation

uses
  EasyBackup_SelectFilesForm;

{$R *.dfm}

function GetSyncOptionName(aSyncOption: TSyncronizeOption): string;
begin
  Result := strUnknown;
  case aSyncOption of
    soDelete: Result := strDelete;
    soCopy: Result := strCopy;
    soIgnore: Result := strIgnore;
    soCopyAge: Result := strError;
  end;
end;

procedure GetFileListDirEx(const Path: string; List: TStrings);
var
  sr: TSearchRec;
  P: string;
begin
  if Path[Length(Path)] = '\' then
    P := Path
  else
    P := Path+'\';

  if FindFirst(P + '*.*', faDirectory, sr) = 0 then
  begin
    if (sr.Name <> '.') and (sr.Name<>'..') and (sr.Attr and faDirectory=faDirectory) then
      List.Add(P + sr.Name);
    while FindNext(sr) = 0 do
      if (sr.Name<>'.') and (sr.Name<>'..') and (sr.Attr and faDirectory=faDirectory) then
        List.Add(P+sr.Name);
    SysUtils.FindClose(sr);
  end;
end;

procedure TFormDoCopy.DoCopyFile(SourceDir, TargetDir, Filename: string; MustExist: Boolean);
var
  s1, s2, s3, s4: string;
  cpy: Boolean;
begin
  s1 := PathAddSeparator(ExtractFilePath(PathRemoveSeparator(RootDir)));
  s2 := Filename;
  s3 := Copy(s2, Length(s1)+1, Length(s2)-Length(s1));
  s4 := PathAddSeparator(TargetDir)+s3;

  try
    ForceDirectories(ExtractFilePath(s4));
  except
    jvcsmak.LogMessage(Format(strErrorCreatingDirectory, [ExtractFilePath(s4)]));
  end;

  cpy := False;

  Application.ProcessMessages;

  if not MustExist then
  begin
    cpy := (not FileExists(s4));
    if not cpy then
      cpy := FileDateToDateTime(FileAge(s2)) > FileDateToDateTime(FileAge(s4));
  end
  else
  begin
    if FileExists(s4) then
      cpy := FileDateToDateTime(FileAge(s2)) > FileDateToDateTime(FileAge(s4));
  end;

  if cpy then
  begin
    lbFile.Caption := s2;
    lbTarget.Caption := s4;

    if FileExists(s4) then
      SetFileAttributes(PChar(s4), FILE_ATTRIBUTE_ARCHIVE);
    if CopyFile(PChar(s2), PChar(s4), False) then
    begin
      if LogbookShowAll then
        jvcsmak.LogMessage(Format(strFileCopied, [s2]));
      FilesCopied := FilesCopied + 1;
    end
    else
    begin
      jvcsmak.LogMessage(Format(strErrorCopyingFile, [s2]));
      CountErrors := CountErrors + 1;
    end;

  end;

  FilesFound := FilesFound + 1;
  Application.ProcessMessages;
end;

procedure TFormDoCopy.DoCopyDir(SourceDir, TargetDir: string; MustExist: Boolean);
var
  sld, slf: TStringList;
  I: Integer;
begin
  sld := TStringList.Create;
  slf := TStringList.Create;
  try
    GetFileListDirEx(SourceDir, sld);
    GetFileListEx(SourceDir, '*', 2, slf);

    if Cancel then Exit;

    //Dateien kopieren
    for I:=0 to slf.Count-1 do
    begin
      DoCopyFile(SourceDir, TargetDir, slf[I], MustExist);
      if Cancel then Break;
    end;


    //Verzeichnisse suchen
    for I:=0 to sld.Count-1 do
    begin
      DoCopyDir(sld[I], TargetDir, MustExist);
      if Cancel then Break;
    end;
  finally
    sld.Free;
    slf.Free;
  end;
end;

procedure TFormDoCopy.DoCopy;
var
  I: Integer;
  st: string;
  sl: TStringList;
begin
  FilesFound := 0;
  FilesCopied := 0;
  lbAction.Caption := strBackup;
  jvcsmak.LogMessage(strCR);
  jvcsmak.LogMessage(strBackupStartet);
  for I:=0 to Source.Count-1 do
  begin
    RootDir := Source[I];
    if (RootDir<>'') and (Target<>'') then
    begin
      lbDir.Caption := RootDir;
      Application.ProcessMessages;
      DoCopyDir(Source[I], Target, False);
    end;
    if Cancel then Break;
  end;
  statSourceFilesFound := FilesFound;
  statSourceFilesCopied := FilesCopied;

  FilesFound := 0;
  FilesCopied := 0;
  lbAction.Caption := strSync;
  jvcsmak.LogMessage(strCR);
  jvcsmak.LogMessage(strSyncStarted);
  if DoSynchronize then
  begin
    for I:=0 to Source.Count-1 do
    begin
      RootDir := PathAddSeparator(Target)+
                 ExtractFileName(PathRemoveSeparator(Source[I]));
      st := ExtractFilePath(PathRemoveSeparator(Source[I]));
      if (RootDir<>'') and (st<>'') then
      begin
        lbDir.Caption := RootDir;
        Application.ProcessMessages;
        DoCopyDir(RootDir, st, True);
      end;
      if Cancel then Break;
    end;
  end;
  statBackupFilesFound := FilesFound;
  statBackupFilesCopied := FilesCopied;

  FilesFound := 0;
  FilesCopied := 0;
  lbAction.Caption := strDeleteSync;
  jvcsmak.LogMessage(strCR);
  jvcsmak.LogMessage(strDeleteSyncStart);
  DoFindMissing;
  if Cancel then Exit;
  DoShowMissingDlg;
  if Cancel then Exit;
  DoCopyMissing;
  statMissingFilesFound := FilesFound;
  statMissingFilesCopied := FilesCopied;

  sl := TStringList.Create;
  try
    sl.Text := Format(strExecEnd,
            [statSourceFilesFound, statSourceFilesCopied,
              statBackupFilesFound, statBackupFilesCopied,
              statMissingFilesFound, statMissingFilesCopied,
              statMissingFilesDeleted, statMissingFilesIgnored,
              CountErrors]);
    for I:=0 to sl.Count-1 do
      jvcsmak.LogMessage(sl[I]);
  finally
    sl.Free;
  end;
end;

procedure TFormDoCopy.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  DoCopy;
  Close;
end;

procedure TFormDoCopy.FormCreate(Sender: TObject);
begin
  Source := TStringList.Create;
  MissingList := TObjectList.Create;
  Cancel := False;
  CountErrors := 0;
  FilesFound := 0;
  FilesCopied := 0;
  lbTarget.Caption := '';
  lbFound.Caption := '';
  lbFile.Caption := '';
  lbDir.Caption :='';

  statSourceFilesFound := 0;
  statSourceFilesCopied := 0;
  statBackupFilesFound := 0;
  statBackupFilesCopied := 0;
  statMissingFilesFound := 0;
  statMissingFilesCopied := 0;
  statMissingFilesDeleted := 0;
  statMissingFilesIgnored := 0;
end;

procedure TFormDoCopy.FormDestroy(Sender: TObject);
begin
  Source.Free;
  MissingList.Free;
end;

procedure TFormDoCopy.Button1Click(Sender: TObject);
begin
  Cancel := True;
end;

procedure TFormDoCopy.DoFindMissing;
var
  I: Integer;
  st: string;
begin
  MissingList.Clear;
  if DoSynchronize then
  begin
    for I:=0 to Source.Count-1 do
    begin
      RootDir := PathAddSeparator(Target)+
                 ExtractFileName(PathRemoveSeparator(Source[I]));
      st := ExtractFilePath(PathRemoveSeparator(Source[I]));
      if (RootDir<>'') and (st<>'') then
      begin
        lbDir.Caption := RootDir;
        Application.ProcessMessages;
        DoFindMissingDir(RootDir, st);
      end;
      if Cancel then Break;
    end;
  end;
end;

procedure TFormDoCopy.DoFindMissingDir(SourceDir, TargetDir: string);
var
  sld, slf: TStringList;
  I: Integer;
begin
  sld := TStringList.Create;
  slf := TStringList.Create;
  try
    GetFileListDirEx(SourceDir, sld);
    GetFileListEx(SourceDir, '*', 2, slf);

    if Cancel then Exit;

    //Dateien kopieren
    for I:=0 to slf.Count-1 do
    begin
      DoFindMissingFile(SourceDir, TargetDir, slf[I]);
      if Cancel then Break;
    end;


    //Verzeichnisse suchen
    for I:=0 to sld.Count-1 do
    begin
      DoFindMissingDir(sld[I], TargetDir);
      if Cancel then Break;
    end;
  finally
    sld.Free;
    slf.Free;
  end;
end;

procedure TFormDoCopy.DoFindMissingFile(SourceDir, TargetDir, Filename: string);
var
  s1, s2, s3, s4: string;
  ob: TMissingFile;
begin
  s1 := PathAddSeparator(ExtractFilePath(PathRemoveSeparator(RootDir)));
  s2 := Filename;
  s3 := Copy(s2, Length(s1)+1, Length(s2)-Length(s1));
  s4 := PathAddSeparator(TargetDir)+s3;

  lbFile.Caption := s2;
  lbTarget.Caption := s4;

  if not FileExists(s4) then
  begin
    ob := TMissingFile.Create;
    ob.SourceFile := s2;
    ob.TargetFile := s4;
    ob.SourceAge := FileDateToDateTime(FileAge(s2));

    if SyncronizeOption = soCopyAge then
    begin
      if (Now-ob.SourceAge)<SyncAge then
        ob.CopyOption := soCopy
      else
        ob.CopyOption := soDelete;
    end
    else
      ob.CopyOption := SyncronizeOption;

    MissingList.Add(ob);

    FilesFound := FilesFound + 1;
  end;

  Application.ProcessMessages;
end;

procedure TFormDoCopy.DoShowMissingDlg;
begin
  if ShowSelectDlg then
    if MissingList.Count>0 then
      if not SelectMissingFiles(MissingList) then
        Cancel := True;
end;

procedure TFormDoCopy.DoCopyMissing;
var
  I: Integer;
  ob: TMissingFile;
begin
  for I := 0 to MissingList.Count - 1 do
  begin
    ob := TMissingFile(MissingList[I]);
    lbFile.Caption := ob.SourceFile;
    lbTarget.Caption := ob.TargetFile;
    Application.ProcessMessages;
    case ob.CopyOption of
      soDelete:
        begin
          try
            if DeleteFile(ob.SourceFile) then
            begin
              jvcsmak.LogMessage(Format(strFileDeleted, [ob.SourceFile]));
              Inc(statMissingFilesDeleted);
            end
            else
            begin
              jvcsmak.LogMessage(Format(strErrorDeletingFile, [ob.SourceFile]));
              CountErrors := CountErrors + 1;
            end;
          except
          end;
        end;
      soCopy:
        begin
          try
            if FileExists(ob.TargetFile) then
              SetFileAttributes(PChar(ob.TargetFile), FILE_ATTRIBUTE_ARCHIVE);
            if CopyFile(PChar(ob.SourceFile), PChar(ob.TargetFile), True) then
            begin
              if LogbookShowAll then
                jvcsmak.LogMessage(Format(strFileCopied, [ob.SourceFile]));
              FilesCopied := FilesCopied + 1;
            end
            else
            begin
              jvcsmak.LogMessage(Format(strErrorCopyingFile, [ob.SourceFile]));
              CountErrors := CountErrors + 1;
            end;
          except
          end;
        end;
      soIgnore:
        begin
          jvcsmak.LogMessage(Format(strFileIgnored, [ob.SourceFile]));
          Inc(statMissingFilesIgnored);
        end;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TFormDoCopy.SetFilesFound(Value: Integer);
begin
  FFilesFound := Value;
  lbFound.Caption := IntToStr(FilesFound);
end;

procedure TFormDoCopy.SetFilesCopied(Value: Integer);
begin
  FFilesCopied := Value;
  lbCopied.Caption := IntToStr(FilesCopied);
end;

procedure TFormDoCopy.SetCountErrors(Value: Integer);
begin
  FCountErrors := Value;
  lbErrors.Caption := IntToStr(CountErrors);
end;

end.
