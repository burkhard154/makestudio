(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: wise_Module.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/21  JDuenow   - launched Wise Module
2005/01/04  BSchranz  - Plugin created
2005/02/05  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit wise_Module;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, makestudio_TLB, Classes, Windows, Dialogs,
  Controls, wise_Vars, Registry, Forms, SysUtils, JclFileUtils;

type
  TWiseCommand = class(TComponent, ICommand)
  private
    FCaption: string;
    FDefaultPath: string;
    FExecPath: string;
    FOutputDir: string;
    FProjects: TStringList;
    function RunBatchfile: Boolean;
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

    property DefaultPath: string read FDefaultPath write FDefaultPath;
    property ExecPath: string read FExecPath write FExecPath;
    property OutputDir: string read FOutputDir write FOutputDir;
    property Projects: TStringList read FProjects write FProjects;
  end;

  //Callback to create an instance of the IJVCSCommand
  TWiseCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  WiseCommandCallback: TWiseCommandCallback;

const
  IDWiseCommand = 'wise.wiseinstaller';

implementation

uses
  ComServ, wise_EditWiseModule;

function TWiseCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TWiseCommand.Create(nil));
end;

procedure TWiseCommandCallback.SetCanceled(ACanceled: WordBool);
begin
  Canceled := ACanceled;
end;

constructor TWiseCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdWiseCaption;
  Projects := TStringList.Create;
  DefaultPath := stdDefaultPath;
end;

destructor TWiseCommand.Destroy;
begin
  Projects.Free;
  inherited Destroy; 
end;

function TWiseCommand.EditItem: WordBool;
begin
  Result := DlgEditWiseModule(Self);
end;

function TWiseCommand.ExecuteItem: WordBool;
begin
  Result := False;
  if not Canceled then
    Result := RunBatchfile;
end;


function TWiseCommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
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
    Result := Result + Canvas.TextHeight(stdOutputDir) + 2;
    Result := Result + Canvas.TextHeight(stdProjects) + 2;

    for I := 0 to Projects.Count - 1 do
      Result := Result + Canvas.TextHeight('file') + 2;
  finally
    Canvas.Free;
  end;
end;

function TWiseCommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
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
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdOutputDir);
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(aRect.Left +iDefaultIndent + Canvas.TextWidth(stdOutputDir) + 2, aRect.Top + Offset, OutputDir);
    Offset := Offset + Canvas.TextHeight(stdOutputDir) + 2;

    Canvas.Font.Style := [];
    SetCanvasTextColor(clMaroon);
    Canvas.TextOut(aRect.Left +iDefaultIndent + 10, aRect.Top + Offset, stdProjects);
    Offset := Offset + Canvas.TextHeight(stdProjects) + 2;

    Canvas.Font.Style := [];
    SetCanvasTextColor(clMaroon);
    for I := 0 to Projects.Count - 1 do
    begin
      Canvas.TextOut(aRect.Left +iDefaultIndent + 20, aRect.Top + Offset, Projects[I]);
      Offset := Offset + Canvas.TextHeight('file') + 2;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TWiseCommand.SetFilename(const Filename: WideString);
begin
  Projects.Add(Filename);
  OutputDir := PathRemoveSeparator(ExtractFilePath(Filename));
end;

function TWiseCommand.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TWiseCommand.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TWiseCommand.RunBatchfile: Boolean;
var
  sl, sl1: TStringList;
  R, I: Integer;
  WiseExec, Path: string;
  old_setup, new_setup: string;

  procedure Wait_a_While(ms: Cardinal);
  var
    st: Cardinal;
  begin
    st := GetTickCount;
    while GetTickCount - st < ms do
      Application.ProcessMessages;
  end;

  procedure LastErrorMsg;
  var
   ch: array [0..511] of Char;
  begin
    FormatMessage(
      FORMAT_MESSAGE_FROM_SYSTEM,
      nil,
      GetLastError,
      LANG_NEUTRAL, // Default language
      ch,
      511,
      nil);
    MakeStudio.LogMessage(StrPas(ch));
  end;


begin
  Path := ExtractFilePath(Application.Exename);
  WiseExec := ExecPath;

  Result := True;

  if not FileExists(WiseExec) then
  begin
    MakeStudio.LogMessage('> Error: ' + stdErrNoWiseExec + ' :-(');
    Result := False;
    Exit;
  end;

  if Projects.Count = 0 then
  begin
    MakeStudio.LogMessage('> Error: ' + stdErrNoProjects + ' :-(');
    Result := False;
    Exit;
  end;

  MakeStudio.LogMessage(stdBreak);
  MakeStudio.LogMessage(stdStartingBatch);

  ForceDirectories(Path + 'Compile');

  sl := TStringList.Create;
  sl1 := TStringList.Create;
  try
    //build Batch-File
    //setup execution directory
    sl.Add('@ECHO ON');
    for I := 0 to Projects.Count - 1 do
    begin
      sl.Add('"' + WiseExec + '" /c "' + Projects[I] + '" > ' + Path + 'Compile\prg.txt');
      MakeStudio.LogMessage('"' + WiseExec + '" /c "' + Projects[I] + '"');
    end;
    sl.Add('dir ' + Path + ' > ' + Path + 'Compile\result.txt');
    sl.SaveToFile(Path + 'Compile\prg.bat');

    //execute
    R := MakeStudio.ExecCmdLine( Path + 'Compile\prg.bat', '', '', nil);
    if R > 32 then
    begin
      while not FileExists(Path + 'Compile\result.txt') do
      begin
        Application.ProcessMessages;
      end;
      R := 0;
      while R < 50 do
      try
        Wait_a_While(100);
        sl.LoadFromFile(PChar(Path + 'Compile\prg.txt'));
        sl1.Clear;
        R := 50;
      except
        R := R + 1;
      end;

      //copy setups to OutputDir and remove old setups
      if Length(OutputDir) > 0 then
      begin
        MakeStudio.LogMessage('');
        MakeStudio.LogMessage(stdMovingFiles);
        ForceDirectories(OutputDir);
        for I := 0 to Projects.Count - 1 do
        begin
          old_setup := ChangeFileExt(Projects[I], '.exe');
          new_setup := ExtractFilePath(OutputDir) + ChangeFileExt(ExtractFileName(Projects[I]), '.exe');
{$IFDEF UNICODE}
          CopyFile(PChar(old_setup), PChar(new_setup), False);
{$ELSE}
          CopyFile(PAnsiChar(old_setup), PAnsiChar(new_setup), False);
{$ENDIF}
          DeleteFile(old_setup);
          MakeStudio.LogMessage('Move "' + old_setup + '" to "' + new_setup + '"');
        end;
      end;

      //remove duplicate lines
      if sl.Count > 0 then
      begin
        sl1.Add(sl[0]);
        while sl.Count > 0 do
        begin
          if sl[0] <> sl1[sl1.Count - 1] then
            sl1.Add(sl[0]);
          sl.Delete(0);
        end;
      end;

      for I:=0 to sl1.Count-1 do
        MakeStudio.LogMessage(sl1[I]);
    end
    else
      LastErrorMsg;

  finally
    sl.Free;
    sl1.Free;
    Wait_a_While(500);
    DeleteFile(Path + 'Compile\prg.txt');
    DeleteFile(Path + 'Compile\result.txt');
  end;
end;

function TWiseCommand.Get_ParamValues(const ParamName: WideString): WideString;
var
  I: Integer;
begin
  if ParamName = stdcExecPath then
    Result := ExecPath
  else
  if ParamName = stdcOutputDir then
    Result := OutputDir
  else
  if ParamName = stdcProjectCount then
  begin
    Result := IntToStr(Projects.Count);
  end
  else
  begin
    for I:=0 to Projects.Count-1 do
      if SameText(Format(stdcProjects, [I + 1]), ParamName) then
      begin
        Result := Projects[I];
        Break;
      end;
  end;
end;

procedure TWiseCommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var
  I: Integer;
begin
  if ParamName = stdcExecPath then
    ExecPath := Value
  else
  if ParamName = stdcOutputDir then
    OutputDir := Value
  else
  if ParamName = stdcProjectCount then
  begin
    Projects.Clear;
    for I:=0 to StrToInt(Value)-1 do
      Projects.Add('');
  end
  else
  begin
    for I:=0 to Projects.Count-1 do
      if SameText(Format(stdcProjects, [I + 1]), ParamName) then
      begin
        Projects[I] := Value;
        Break;
      end;
  end;
end;

function TWiseCommand.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '???';
  case Index of
    0: Result := stdcExecPath;
    1: Result := stdcOutputDir;
    2: Result := stdcProjectCount;
    else
    begin
      Result := Format(stdcProjects, [Index - 2]);
    end;
  end;
end;

function TWiseCommand.Get_ParamCount: Integer;
begin
  Result := 3 + Projects.Count;
end;

function TWiseCommandCallback.GetIdentifier: WideString;
begin
  Result := IDWiseCommand;
end;

end.
