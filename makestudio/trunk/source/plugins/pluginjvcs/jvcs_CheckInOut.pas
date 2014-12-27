(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSDataClass.pas

The Initial Devoloper of the original DMAK-Code is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
Code move to JEDI VCS:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to MakeStudio
2003/11/28  USchuster - 2nd Migrationstep (fixed header)
2003/12/05  USchuster - re-formatted
2005/01/05  BSchranz  - Migration to plugin code
2005/02/04  USchuster - preparations for check in
2005/08/11  BSchranz  - Migration from jvcscore.dll to jvcs.exe

-----------------------------------------------------------------------------*)

unit jvcs_CheckInOut;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, makestudio_TLB,
  Classes, Windows, Dialogs, Controls,
  jvcs_Vars, Registry, Forms, SysUtils, JclSysInfo, JclFileUtils,
  JclSysUtils, jvcs_Utils, JclStrings;

type
  TJVCSInOutCommand = class(TComponent, ICommand, IExecCallback)
  private
    FCaption: string;
    FUsername: string;
    FPassword: string;
    FPort: Integer;
    FServer: string;
    FModules : TStringList;

    procedure CreateInputfile( aFilename:String);
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

    //IExecCallback
    procedure CaptureOutput(const Line: WideString; var Aborted: WordBool); safecall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
    property Server: string read FServer write FServer;
    property Modules:TStringList read FModules write FModules;
  end;

  //Callback to create an instance of the IJVCSCommand
  TJVCSInOutCommandCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

var
  JVCSInOutCommandCallback: TJVCSInOutCommandCallback;

const
  IDJVCSInOutCommand = 'jvcs.inout';

implementation

uses
  ComServ, jvcs_EditJVCSInOutModule;


function TJVCSInOutCommandCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TJVCSInOutCommand.Create(nil));
end;

procedure TJVCSInOutCommandCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled;
end;

constructor TJVCSInOutCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdJVCSInOutCaption;
  GetLastUsedIdentityEx( FUsername, FServer, FPassword, FPort);
  FModules := TStringList.Create;
end;

destructor TJVCSInOutCommand.Destroy;
begin
  FModules.Free;
  inherited Destroy;
end;

function TJVCSInOutCommand.EditItem: WordBool;
begin
  Result := DlgEditJVCSInOutModule(Self);
end;

function TJVCSInOutCommand.ExecuteItem: WordBool;
var
  sl : TStringList;
  Helper : TJVCSHelper;
  F : String;
begin
  Result := True;

  Helper := TJVCSHelper.Create;
  sl := TStringList.Create;
  try
    if not Canceled then
    begin

      F := PathAddSeparator( GetCommonAppdataFolder) + 'jvcsinput.txt';
      F := 'c:\jvcsinput.txt';
      CreateInputfile( F);
      Result := Helper.ProceedInputFile( Username, Server, Password, Port, F);

    end
    else
      Result := False;
  finally
    Helper.Free;
    sl.Free;
  end;
end;

function TJVCSInOutCommand.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
    Canvas.Font.Style := [];
    Result := Result + Canvas.TextHeight(stdSyncTxt) + 2;

    if Modules.Count > 0 then
    begin
      Result := Result + Canvas.TextHeight(stdModulesTxt) + 2;

      if Modules.Count mod 5 = 0 then
        Result := Result + (Canvas.TextHeight(Modules[0]) + 2) * ((Modules.Count div 10))
      else
        Result := Result + (Canvas.TextHeight(Modules[0]) + 2) *
          ((Modules.Count div 5) + 1);
    end;
  finally
    Canvas.Free;
  end;
end;

function TJVCSInOutCommand.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
  Canvas: TCanvas;
  aRect: TRect;
  I, Offset: Integer;
  S: string;
  sl : TStringList;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False;

  sl := TStringList.Create;
  try
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
      Canvas.TextOut(aRect.Left +iDefaultIndent, aRect.Top + Offset, FCaption);
      Offset := Offset + Canvas.TextHeight(FCaption) + 2;
      Canvas.Font.Style := [];
      SetCanvasTextColor(clBlue);
      Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset,
                     Format(stdSyncTxt, [Server, Port, Username]));
      Offset := Offset + Canvas.TextHeight(stdSyncTxt) + 2;

      SetCanvasTextColor(clFuchsia);

      if Modules.Count > 0 then
      begin
        Canvas.Font.Style := [fsBold];
        Canvas.TextOut(aRect.Left + iDefaultIndent + 10, aRect.Top + Offset, stdModulesTxt);
        Offset := Offset + Canvas.TextHeight(stdModulesTxt) + 2;
      end;
      S := '';
      Canvas.Font.Style := [];
      for I := 0 to Modules.Count - 1 do
      begin
        sl.Clear;
        StrTokenToStrings( Modules[i], ';', sl);

        if sl.Count>2 then begin
          if sl[1] = '0' then //Out
            S := S + stdCheckout + ':'
          else
            S := S + stdCheckin + ':';
          S := S + sl[0] + '.' + sl[2];
          if I < Modules.Count - 1 then
            S := S + ',';
          if (I + 1) mod 10 = 0 then
          begin
            Canvas.TextOut(aRect.Left + iDefaultIndent + 16, aRect.Top + Offset, S);
            Offset := Offset + Canvas.TextHeight(S) + 2;
            S := '';
          end;
        end;
      end;
      if S <> '' then
      begin
        Canvas.TextOut(aRect.Left + iDefaultIndent + 16, aRect.Top + Offset, S);
        Offset := Offset + Canvas.TextHeight(S) + 2;
      end;
    finally
      Canvas.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure TJVCSInOutCommand.SetFilename(const Filename: WideString);
begin
end;

function TJVCSInOutCommand.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TJVCSInOutCommand.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TJVCSInOutCommand.Get_ParamValues(const ParamName: WideString): WideString;
var
  I: Integer;
begin
  if ParamName = stdcUsername then
    Result := Username
  else
  if ParamName = stdcPassword then
    Result := Password
  else
  if ParamName = stdcPort then
    Result := IntToStr( Port)
  else
  if ParamName = stdcServer then
    Result := Server
  else
  if ParamName = stdcProjectCount then
  begin
    Result := IntToStr(Modules.Count);
  end
  else
  begin
    for I := 0 to Modules.Count - 1 do
      if SameText(Format(stdcModules, [I + 1]), ParamName) then
      begin
        Result := Modules[I];
        Break;
      end;
  end;
end;

procedure TJVCSInOutCommand.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var
  I: Integer;
begin
  if ParamName = stdcUsername then
    Username := Value
  else
  if ParamName = stdcPassword then
    Password := Value
  else
  if ParamName = stdcPort then
    try Port := StrToInt( Value) except end
  else
  if ParamName = stdcServer then
    Server := Value
  else
  if ParamName = stdcProjectCount then
  begin
    Modules.Clear;
    for I := 0 to StrToInt(Value) - 1 do
      Modules.Add('');
  end
  else
  begin
    for I := 0 to Modules.Count - 1 do
      if SameText(Format(stdcModules, [I + 1]), ParamName) then
      begin
        Modules[I] := Value;
        Break;
      end;
  end;
end;

function TJVCSInOutCommand.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '???';
  case Index of
    0: Result := stdcUsername;
    1: Result := stdcPassword;
    2: Result := stdcPort;
    3: Result := stdcServer;
    4: Result := stdcProjectCount;
    else
    begin
      Result := Format(stdcModules, [Index - 4]);
    end;
  end;
end;

function TJVCSInOutCommand.Get_ParamCount: Integer;
begin
  Result := 5 + Modules.Count;
end;

function TJVCSInOutCommandCallback.GetIdentifier: WideString;
begin
  Result := IDJVCSInOutCommand;
end;

procedure TJVCSInOutCommand.CaptureOutput(const Line: WideString;
  var Aborted: WordBool);
begin
  Aborted := Canceled;
  MakeStudio.LogMessage(Line);
end;

procedure TJVCSInOutCommand.CreateInputfile(aFilename: String);
var sl, sl1:TStringList;
    Project : String;
    i : Integer;
begin
  sl := TStringList.Create;
  sl1 := TStringList.Create;
  try
    Modules.Sorted := true;
    Modules.Sort;
    Project := '';

    if Modules.Count>0 then
      for i:=0 to Modules.Count-1 do begin
        StrTokenToStrings( Modules[i], ';', sl1);
        if Project<>sl1[0] then begin
          Project := sl1[0];
          sl.Add( Format( stdcOpenProjectArgs, [Project]));
        end;
        if sl1[1] = '0' then //checkout
          sl.Add( Format( stdcCheckoutIDArgs, [ StrToInt(sl1[3])]))
        else begin
          sl.Add( Format( stdcCheckinIDArgs, [ StrToInt(sl1[3])]));
          sl.Add( Format( stdcUndoCheckoutIDArgs, [ StrToInt(sl1[3])]));
        end;
      end;

    sl.SaveToFile( aFilename);
  finally
    sl.Free;
    sl1.Free;
  end;
end;

end.
