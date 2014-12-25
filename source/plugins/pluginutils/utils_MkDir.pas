(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jmakutils_mkdir.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/22  JDuenow   - launched EditMkdirModule
2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit utils_MkDir;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils, Forms;

type
  TMkDirModule = class(TComponent, ICommand)
  private
    FCaption: string;
    FProjects: TStringList;
    function MakeDirectories: Boolean;

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
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Projects: TStringList read FProjects write FProjects;
  end;

  //Callback to create an instance of the IJVCSModule
  TMkDirModuleCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginMkDirCallback: TMkDirModuleCallback;

const
  idMkDirmodule = 'utils.mkdir';

implementation

uses
  {$IFDEF DELPHI5}
  FileCtrl,
  {$ENDIF DELPHI5}
  ComServ, utils_Vars, utils_EditMkDirModule;


function TMkDirModuleCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TMkDirModule.Create(nil));
end;

procedure TMkDirModuleCallback.SetCanceled(aCanceled: WordBool);
begin
  //
end;

constructor TMkDirModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdMkdirCaption;
  Projects := TStringList.Create;
end;

destructor TMkDirModule.Destroy;
begin
  Projects.Free;
  inherited Destroy;
end;

function TMkDirModule.EditItem: WordBool;
begin
  Result := DlgEditMkdirModule(Self);
end;

function TMkDirModule.MakeDirectories: Boolean;
var
  I: Integer;
begin
  Result := True;

  if Projects.Count = 0 then
  begin
    MakeStudio.LogMessage('> Error: ' + stdErrNoProjects + ' :-(');
    Result := False;
    Exit;
  end;

  MakeStudio.LogMessage(stdForceDirectories);
  for I := 0 to Projects.Count - 1 do
  begin
    MakeStudio.LogMessage('ForceDirectories("' + Projects[I] + '")');
    ForceDirectories( MakeStudio.Variables.ReplaceVarsInString( Projects[I]));
  end;

  Application.ProcessMessages;
end;

function TMkDirModule.ExecuteItem: WordBool;
begin
  Result := MakeDirectories;
end;

function TMkDirModule.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
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
    if not BriefView then
    begin
      Canvas.Font.Style := [];
      Result := Result + Canvas.TextHeight(stdDirectories) + 2;

      for I := 0 to Projects.Count - 1 do
        Result := Result + Canvas.TextHeight('file') + 2;
    end;
  finally
    Canvas.Free;
  end;
end;

function TMkDirModule.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
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
  Result := True;
  Exit;
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
    Canvas.TextOut(aRect.Left + aRect.Left + 2, aRect.Top + Offset, FCaption);
    Offset := Offset + Canvas.TextHeight(FCaption) + 2;

    if not BriefView then
    begin
      Canvas.Font.Style := [];
      SetCanvasTextColor(clMaroon);
      Canvas.TextOut(aRect.Left + aRect.Left + 2 + 10, aRect.Top + Offset, stdDirectories);
      Offset := Offset + Canvas.TextHeight(stdDirectories) + 2;

      Canvas.Font.Style := [];
      SetCanvasTextColor(clMaroon);
      for I := 0 to Projects.Count - 1 do
      begin
        Canvas.TextOut(aRect.Left + 2 + 20, aRect.Top + Offset, Projects[I]);
        Offset := Offset + Canvas.TextHeight('file') + 2;
      end;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TMkDirModule.SetFilename(const Filename: WideString);
begin
end;

function TMkDirModule.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TMkDirModule.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TMkDirModule.Get_ParamValues(const ParamName: WideString): WideString;
var
  I: Integer;
begin
  Result := '';
  if SameText(ParamName, stdcProjectCount) then
    Result := IntToStr(Projects.Count)
  else
    for I:=0 to Projects.Count-1 do
      if SameText(ParamName, Format(stdcProjects, [I + 1])) then
      begin
        Result := Projects[I];
        Break;
      end;
end;

procedure TMkDirModule.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var
  I: Integer;
begin
  if SameText(ParamName, stdcProjectCount) then
  begin
    for I := 0 to StrToInt(Value) - 1 do
      Projects.Add('');
  end
  else
    for I:=0 to Projects.Count-1 do
      if SameText(ParamName, Format(stdcProjects, [I + 1])) then
      begin
        Projects[I] := Value;
        Break;
      end;
end;

function TMkDirModule.Get_ParamNames(Index: Integer): WideString;
begin
  case Index of
    0: Result := stdcProjectCount;
    else
      Result := Format(stdcProjects, [Index]);
  end;
end;

function TMkDirModule.Get_ParamCount: Integer;
begin
  Result := Projects.Count + 1;
end;

function TMkDirModuleCallback.GetIdentifier: WideString;
begin
  Result := idMkDirmodule;
end;

end.
