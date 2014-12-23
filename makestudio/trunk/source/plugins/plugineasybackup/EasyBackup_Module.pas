(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EasyBackup_Module.pas

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

unit EasyBackup_Module;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls,
  EasyBackup_Vars, Registry, Forms, SysUtils;

type
  TEasyBackupCommand = class(TComponent, ICommand)
  private
    FCaption: string;

    FBackupDelete: Boolean; //rbbkDeleteAll
    FBackupCopy: Boolean;   //rbbkCopyAll
    FBackupIgnore: Boolean; //rbbkIgnoreAll
    FBackupCopyAge: Boolean; //rbbkCopyAge
    FBackupShowSelection: Boolean;  //cbbkShowSelection
    FCopyOption: Integer;           //rgCopy.ItemIndex
    FLogbookOption: Integer;        //rgLogbook.ItemIndex
    FBackupCopyAgeValue: Integer;   //edbkAge.Value
    FTargetDir: string;              //edTarget.Selection
    FSourceDirs: TStringList;        //lstSource.Items
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

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BackupDelete: Boolean read FBackupDelete write FBackupDelete;
    property BackupCopy: Boolean read FBackupCopy write FBackupCopy;
    property BackupIgnore: Boolean read FBackupIgnore write FBackupIgnore;
    property BackupCopyAge: Boolean read FBackupCopyAge write FBackupCopyAge;
    property BackupShowSelection: Boolean read FBackupShowSelection write FBackupShowSelection;
    property CopyOption: Integer read FCopyOption write FCopyOption;
    property LogbookOption: Integer read FLogbookOption write FLogbookOption;
    property BackupCopyAgeValue: Integer read FBackupCopyAgeValue write FBackupCopyAgeValue;
    property TargetDir: string read FTargetDir write FTargetDir;
    property SourceDirs: TStringList read FSourceDirs write FSourceDirs;
  end;

  //Callback to create an instance of the IJVCSCommand
  TEasyBackupCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  EasyBackupCommandCallback: TEasyBackupCommandCallback;

const
  IDEasyBackupCommand = 'easybackup.backup';


implementation

uses
  ComServ, EasyBackup_EditModule, EasyBackup_FormCopy;

function TEasyBackupCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TEasyBackupCommand.Create(nil));
end;

procedure TEasyBackupCommandCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled;
end;

constructor TEasyBackupCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdEasyBackupCaption;
  SourceDirs := TStringList.Create;
  FBackupDelete := False;
  FBackupCopy := False;
  FBackupIgnore := False;
  FBackupCopyAge := True;
  FBackupShowSelection := False;
  FCopyOption := 0;
  FBackupCopyAgeValue := 30;
  FTargetDir:='';
  FLogbookOption := 0;
end;

destructor TEasyBackupCommand.Destroy;
begin
  SourceDirs.Free;
  inherited Destroy;
end;

function TEasyBackupCommand.EditItem: WordBool;
begin
  Result := DlgEditEasyBakModule(Self);
end;

function TEasyBackupCommand.ExecuteItem: WordBool;
var
  F: TFormDoCopy;
begin
  Result := False;
  F := TFormDoCopy.Create(nil);
  try
    F.Source.Assign(SourceDirs);
    F.Target := jvcsmak.Variables.ReplaceVarsInString( TargetDir);
    F.DoSynchronize := CopyOption = 1;
    F.ShowSelectDlg := BackupShowSelection;
    F.LogbookShowAll := LogbookOption = 1;

    if BackupCopy then
      F.SyncronizeOption := soCopy
    else
    if BackupDelete then
      F.SyncronizeOption := soDelete
    else
    if BackupIgnore then
      F.SyncronizeOption := soIgnore
    else
    if BackupCopyAge then
      F.SyncronizeOption := soCopyAge;
    F.SyncAge := BackupCopyAgeValue;

    F.Timer1.Enabled := True;
    F.ShowModal;
    Result := F.CountErrors = 0;
  finally
    F.Free;
  end;
end;

function TEasyBackupCommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
  I: Integer;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
    Canvas.Font.Style := [];
    Result := Result + Canvas.TextHeight(stdTargesDir) + 2;
    Result := Result + Canvas.TextHeight(stdSourceDirs) + 2;

    for I := 0 to SourceDirs.Count - 1 do
      Result := Result + Canvas.TextHeight('file') + 2;
  finally
    Canvas.Free;
  end;
end;

function TEasyBackupCommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
  Canvas: TCanvas;
  aRect: TRect;
  I, Offset: Integer;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False;

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

    Canvas.Font.Style := [];
    SetCanvasTextColor(clBlue);
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdTargesDir);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdTargesDir) + 2, aRect.Top + Offset, TargetDir);
    Offset := Offset + Canvas.TextHeight(stdSourceDirs) + 2;

    Canvas.Font.Style := [];
    SetCanvasTextColor(clMaroon);
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdSourceDirs);
    Offset := Offset + Canvas.TextHeight(stdSourceDirs) + 2;

    Canvas.Font.Style := [];
    SetCanvasTextColor(clMaroon);
    for I := 0 to SourceDirs.Count - 1 do
    begin
      Canvas.TextOut(aRect.Left +iDefaultIndent + 20, aRect.Top + Offset, SourceDirs[I]);
      Offset := Offset + Canvas.TextHeight('file') + 2;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TEasyBackupCommand.SetFilename(const Filename: WideString);
begin
end;

function TEasyBackupCommand.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TEasyBackupCommand.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

{
function TEasyBackupCommand.GetItemText: WideString;
var
  sl: TStringList;
  I: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Add('  ' + stdcBackupDelete + '=' + BoolToStr(BackupDelete));
    sl.Add('  ' + stdcBackupCopy + '=' + BoolToStr(BackupCopy));
    sl.Add('  ' + stdcBackupIgnore + '=' + BoolToStr(BackupIgnore));
    sl.Add('  ' + stdcBackupCopyAge + '=' + BoolToStr(BackupCopyAge));
    sl.Add('  ' + stdcBackupShowSelection + '=' + BoolToStr(BackupShowSelection));
    sl.Add('  ' + stdcCopyOption + '=' + IntToStr(CopyOption));
    sl.Add('  ' + stdcLogbookOption + '=' + IntToStr(LogbookOption));
    sl.Add('  ' + stdcBackupCopyAgeValue + '=' + IntToStr(BackupCopyAgeValue));
    sl.Add('  ' + stdcTargesDir + '=' + TargetDir);
    sl.Add('  ' + stdcSourceCount + '=' + IntToStr(SourceDirs.Count));
    for I := 0 to SourceDirs.Count - 1 do
      sl.Add('  ' + Format(stdcSourceDir, [I + 1]) + '=' + SourceDirs[I]);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;
procedure TEasyBackupCommand.SetItemText(const ItemText: WideString);

  procedure TrimStringListLeft(sl: TStrings);
  var
    I: Integer;
  begin
    for I := 0 to sl.Count - 1 do
      sl[I] := TrimLeft(sl[I]);
  end;

var
  sl: TStringList;
  C, I: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Text := ItemText;
    TrimStringListLeft(sl);

    BackupDelete := StrToBoolDef(sl.Values[stdcBackupDelete], False);
    BackupCopy := StrToBoolDef(sl.Values[stdcBackupCopy], False);
    BackupIgnore := StrToBoolDef(sl.Values[stdcBackupIgnore], False);
    BackupCopyAge := StrToBoolDef(sl.Values[stdcBackupCopyAge], True);
    BackupShowSelection := StrToBoolDef(sl.Values[stdcBackupShowSelection], False);
    CopyOption := StrToIntDef(sl.Values[stdcCopyOption], 0);
    LogbookOption := StrToIntDef(sl.Values[stdcLogbookOption], 0);
    BackupCopyAgeValue := StrToIntDef(sl.Values[stdcBackupCopyAgeValue], 30);
    TargetDir := sl.Values[stdcTargesDir];

    c := StrToIntDef(sl.Values[stdcSourceCount], 0);
    SourceDirs.Clear;
    for I := 0 to c - 1 do
      SourceDirs.Add(sl.Values[Format(stdcSourceDir, [I + 1])]);
  finally
    sl.Free;
  end;
end;
}

{$IFNDEF D5TODO}
{$IFDEF DELPHI5}
function BoolToStr(ABoolean: Boolean): string;
begin
  if ABoolean then
    Result := '1'
  else
    Result := '0';
end;

function StrToBool(AString: string): Boolean;
begin
  Result := (AString = '1') or SameText('True', AString);
end;
{$ENDIF DELPHI5}
{$ENDIF ~D5TODO}

function TEasyBackupCommand.Get_ParamValues(const ParamName: WideString): WideString;
var
  I: Integer;
begin
  if ParamName = stdcBackupDelete then
    Result := BoolToStr(BackupDelete)
  else
  if ParamName = stdcBackupCopy then
    Result := BoolToStr(BackupCopy)
  else
  if ParamName = stdcBackupIgnore then
    Result := BoolToStr(BackupIgnore)
  else
  if ParamName = stdcBackupCopyAge then
    Result := BoolToStr(BackupCopyAge)
  else
  if ParamName = stdcBackupShowSelection then
    Result := BoolToStr(BackupShowSelection)
  else
  if ParamName = stdcCopyOption then
    Result := IntToStr(CopyOption)
  else
  if ParamName = stdcLogbookOption then
    Result := IntToStr(LogbookOption)
  else
  if ParamName = stdcBackupCopyAgeValue then
    Result := IntToStr(BackupCopyAgeValue)
  else
  if ParamName = stdcTargetDir then
    Result := TargetDir
  else
  if ParamName = stdcSourceCount then
  begin
    Result := IntToStr(SourceDirs.Count);
  end
  else
  begin
    for I := 0 to SourceDirs.Count - 1 do
      if SameText(Format(stdcSourceDir, [I + 1]), ParamName) then
      begin
        Result := SourceDirs[I];
        Break;
      end;
  end;
end;

procedure TEasyBackupCommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var
  I: Integer;
begin
  if ParamName = stdcBackupDelete then
    BackupDelete := StrToBool(Value)
  else
  if ParamName = stdcBackupCopy then
    BackupCopy := StrToBool(Value)
  else
  if ParamName = stdcBackupIgnore then
    BackupIgnore := StrToBool(Value)
  else
  if ParamName = stdcBackupCopyAge then
    BackupCopyAge := StrToBool(Value)
  else
  if ParamName = stdcBackupShowSelection then
    BackupShowSelection := StrToBool(Value)
  else
  if ParamName = stdcCopyOption then
    CopyOption := StrToInt(Value)
  else
  if ParamName = stdcLogbookOption then
    LogbookOption := StrToInt(Value)
  else
  if ParamName = stdcBackupCopyAgeValue then
    BackupCopyAgeValue := StrToInt(Value)
  else
  if ParamName = stdcTargetDir then
    TargetDir := Value
  else
  if ParamName = stdcSourceCount then
  begin
    SourceDirs.Clear;
    for I:=0 to StrToInt(Value)-1 do
      SourceDirs.Add('');
  end
  else
  begin
    for I:=0 to SourceDirs.Count-1 do
      if SameText(Format(stdcSourceDir, [I + 1]), ParamName) then
      begin
        SourceDirs[I] := Value;
        Break;
      end;
  end;
end;

function TEasyBackupCommand.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '???';
  case Index of
    0: Result := stdcBackupDelete;
    1: Result := stdcBackupCopy;
    2: Result := stdcBackupIgnore;
    3: Result := stdcBackupCopyAge;
    4: Result := stdcBackupShowSelection;
    5: Result := stdcCopyOption;
    6: Result := stdcLogbookOption;
    7: Result := stdcBackupCopyAgeValue;
    8: Result := stdcTargetDir;
    9: Result := stdcSourceCount;
    else
    begin
      Result := Format(stdcSourceDir, [Index - 9]);
    end;
  end;
end;

function TEasyBackupCommand.Get_ParamCount: Integer;
begin
  Result := 10 + SourceDirs.Count;
end;

function TEasyBackupCommandCallback.GetIdentifier: WideString;
begin
  Result := IDEasyBackupCommand;
end;

end.
