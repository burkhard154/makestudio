(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jmakutils_filecopy.pas

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

unit utils_FileCopy;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils, Forms;

type
  TCopyFilesModule = class(TComponent, ICommand)
  private
    FCaption: string;
    FSourceFiles: TStringList;
    FTargetFiles: TStringList;
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

    property SourceFiles: TStringList read FSourceFiles write FSourceFiles;
    property TargetFiles: TStringList read FTargetFiles write FTargetFiles;
  end;

  //Callback to create an instance of the IJVCSModule
  TCopyFilesModuleCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  PluginCopyFilesCallback: TCopyFilesModuleCallback;

const
  IDCopyFilesmodule = 'jvcsutils.CopyFilesModule';

implementation

uses
  ComServ, utils_Vars, utils_EditCopyFilesModule;

function TCopyFilesModuleCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TCopyFilesModule.Create(nil));
end;

procedure TCopyFilesModuleCallback.SetCanceled(aCanceled: WordBool);
begin
  //
end;

constructor TCopyFilesModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdCopyFilesCaption;
  FSourceFiles := TStringList.Create;
  FTargetFiles := TStringList.Create;
end;

destructor TCopyFilesModule.Destroy;
begin
  FSourceFiles.Free;
  FTargetFiles.Free;
  inherited Destroy;
end;

function TCopyFilesModule.EditItem: WordBool;
begin
  Result := DlgEditCopyFilesModule(Self);
end;

function TCopyFilesModule.ExecuteItem: WordBool;
var
  I: Integer;
  sf, tf : String;
begin
  Result := True;
  jvcsmak.LogMessage(stdCopyFilesCaption);
  for I := 0 to SourceFiles.Count - 1 do
  begin
    sf := jvcsmak.Variables.ReplaceVarsInString(SourceFiles[I]);
    tf := jvcsmak.Variables.ReplaceVarsInString(TargetFiles[I]);

    jvcsmak.LogMessage( sf + ' -> ' + tf);

    if FileExists( sf) and FileExists( tf) then
      SetFileAttributes( PChar(tf), FILE_ATTRIBUTE_ARCHIVE);

    Result := Result and CopyFile( PChar( sf), PChar( tf), false);

    if not Result then
    begin
      jvcsmak.LogMessage(stdErrorCopying);
      Break;
    end;
  end;
end;

function TCopyFilesModule.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
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
      Result := Result + Canvas.TextHeight(stdFiles) + 2;

      for I := 0 to SourceFiles.Count - 1 do
      begin
        Result := Result + Canvas.TextHeight('file') + 2;
      end;
    end;
  finally
    Canvas.Free;
  end;
end;

function TCopyFilesModule.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
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
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, FCaption);
    Offset := Offset + Canvas.TextHeight(FCaption) + 2;

    if not BriefView then
    begin
      Canvas.Font.Style := [];
      SetCanvasTextColor(clMaroon);
      Canvas.TextOut(aRect.Left + 2 + 10, aRect.Top + Offset, stdFiles);
      Offset := Offset + Canvas.TextHeight(stdFiles) + 2;

      Canvas.Font.Style := [];
      SetCanvasTextColor(clMaroon);
      for I := 0 to SourceFiles.Count - 1 do
      begin
        Canvas.TextOut(aRect.Left + 2 + 20, aRect.Top + Offset, SourceFiles[I] + ' -> ' + TargetFiles[I]);
        Offset := Offset + Canvas.TextHeight('File') + 2;
      end;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TCopyFilesModule.SetFilename(const Filename: WideString);
begin
end;

function TCopyFilesModule.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TCopyFilesModule.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TCopyFilesModule.Get_ParamValues(const ParamName: WideString): WideString;
var
  I: Integer;
begin
  Result := '';
  if SameText(ParamName, stdcFileCount) then
    Result := IntToStr(SourceFiles.Count)
  else
    for I:=0 to SourceFiles.Count-1 do
    begin
      if SameText(ParamName, Format(stdcSource, [I + 1])) then
      begin
        Result := SourceFiles[I];
        Break;
      end;
      if SameText(ParamName, Format(stdcTarget, [I + 1])) then
      begin
        Result := TargetFiles[I];
        Break;
      end;
    end
end;

procedure TCopyFilesModule.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var
  I: Integer;
begin
  if SameText(ParamName, stdcFileCount) then
  begin
    SourceFiles.Clear;
    TargetFiles.Clear;
    for I:=0 to StrToInt(Value)-1 do
    begin
      SourceFiles.Add('');
      TargetFiles.Add('');
    end;
  end
  else
    for I:=0 to SourceFiles.Count-1 do
    begin
      if SameText(ParamName, Format(stdcSource, [I + 1])) then
      begin
        SourceFiles[I] := Value;
        Break;
      end;
      if SameText(ParamName, Format(stdcTarget, [I + 1])) then
      begin
        TargetFiles[I] := Value;
        Break;
      end;
    end
end;

function TCopyFilesModule.Get_ParamNames(Index: Integer): WideString;
begin
  if Index = 0 then
    Result := stdcFileCount
  else
  if Index in [1..SourceFiles.Count] then
    Result := Format(stdcSource, [Index])
  else
  if Index in [SourceFiles.Count+1..TargetFiles.Count*2] then
    Result := Format(stdcTarget, [Index-SourceFiles.Count]);
end;

function TCopyFilesModule.Get_ParamCount: Integer;
begin
  Result := SourceFiles.Count*2+1;
end;

function TCopyFilesModuleCallback.GetIdentifier: WideString;
begin
  Result := IDCopyFilesmodule;
end;

end.
