(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jmakutils_runbatch.pas

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
2005/09/09  BSchranz  - translated to english
2005/09/10  BSchranz  - Added interal batch (instead of external file)

-----------------------------------------------------------------------------*)

unit utils_RunBatch;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, msTLB,
  Classes, Windows, Dialogs, Controls, SysUtils, Forms, JclFileUtils;

type
  TBatchType = ( batExternal, batInternal);

  TBatchModule = class(TComponent, ICommand2, IExecCallback)
  private
    FBatchfile: string;
    FBatchType: TBatchType;
    FBatchStrings : TStringList;
    FReplaceVars : Boolean;
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
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;

    procedure CaptureOutput(const Line: WideString; var Aborted: WordBool); safecall;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; ABatchType:TBatchType); virtual;
    destructor Destroy; override;

    property BatchFile: string read FBatchfile write FBatchfile;
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Properties: IDispatch read Get_Properties;
    property BatchType : TBatchType read FBatchType write FBatchType;
    property BatchStrings:TStringList read FBatchStrings write FBatchStrings;
    property ReplaceVars:Boolean read FReplaceVars write FReplaceVars;
  end;

  //Callback to create an instance of the IJVCSModule
  TBatchModuleCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

  //Callback to create an instance of the IJVCSModule
  TBatchInternalModuleCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;


var
  PluginBatchCallback: TBatchModuleCallback;
  PluginBatchInternalCallback: TBatchInternalModuleCallback;

const
  IDBatchFilesmodule = 'jvcsutils.batchmodule';
  IDBatchInternal = 'jvcsutils.batchinternal';

implementation

uses
  ComServ, utils_Vars, utils_EditBatchModule, utils_EditBatchInternal;

function TBatchModuleCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TBatchModule.CreateEx(nil, batExternal));
end;

procedure TBatchModuleCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled;
end;

{ TBatchModule }

constructor TBatchModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBatchfile := '';
  FBatchType := batExternal;
  FBatchStrings := TStringList.Create;
end;

constructor TBatchModule.CreateEx(AOwner: TComponent; ABatchType: TBatchType);
begin
  Create( AOwner);
  BatchType := ABatchType;
end;

destructor TBatchModule.Destroy;
begin
  FBatchStrings.Free;
  inherited Destroy;
end;

function TBatchModule.EditItem: WordBool;
begin
  case BatchType of
    batExternal : Result := DlgEditBatchModule(Self);
    batInternal : Result := DlgEditBatchInternalCommand(Self);
  end;
end;

function TBatchModule.ExecuteItem: WordBool;
var s:String;
    i:Integer;
    st:TStringList;
begin
  Canceled := False;
  Result := True;

  jvcsmak.LogMessage(stdStartingBatch);

  case BatchType of
    batExternal:
      begin
        s := jvcsmak.Variables.ReplaceVarsInString( BatchFile);
        jvcsmak.LogMessage(s);
      end;
    batInternal:
      begin
        st := TStringList.Create;
        try
          st.Assign( BatchStrings);
          if ReplaceVars then
            for i:=0 to st.Count-1 do
              st[i] := jvcsmak.Variables.ReplaceVarsInString( st[i]);
          s := PathAddSeparator( jvcsmak.ApplicationDataFolder) + stdcInternalBatchFilename;
          st.SaveToFile( s);
        finally
          st.Free;
        end;
      end;
  end;

  if jvcsmak.ExecCmdLine(s, '', ExtractFilePath(s), IExecCallback(Self)) < 0 then
  begin
    jvcsmak.LogMessage(stderrRunningBatch);
    Result := False;
  end;

  Result := Result or Canceled;
end;

function TBatchModule.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
begin
  Result := -1;
end;

function TBatchModule.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin
  Result := false;
end;

procedure TBatchModule.SetFilename(const Filename: WideString);
begin
  case BatchType of
    batExternal : BatchFile := Filename;
    batInternal :
      try
        BatchStrings.LoadFromFile( Filename);
      except
        on E:Exception do MessageDlg( E.Message, mtError, [mbOk], 0);
      end;
  end;
end;

function TBatchModule.Get_Caption: WideString;
begin
  case BatchType of
    batExternal : Result := stdBatchFileCaption + ' - ' + ExtractFilename(BatchFile);
    batInternal : Result := stdBatchFileInternalCaption;
    else
      Result := '????';
  end;
end;

procedure TBatchModule.Set_Caption(const Value: WideString);
begin
end;

function TBatchModule.Get_ParamValues(const ParamName: WideString): WideString;
var i:Integer;
begin
  case BatchType of
    batExternal : Result := BatchFile;
    batInternal :
      begin
        if SameText( ParamName, stdcReplaceVars) then
          Result := IntToStr( Ord(ReplaceVars))
        else if SameText( ParamName, stdcBatchLineCount) then
          Result := IntToStr( BatchStrings.Count)
        else begin
          for i:=0 to BatchStrings.Count-1 do
            if SameText( ParamName, stdcBatchLine + IntToStr(i+1)) then begin
               Result := BatchStrings[i];
               Break;
            end;
        end;
      end;
  end;
end;

procedure TBatchModule.Set_ParamValues(const ParamName: WideString; const Value: WideString);
var i:Integer;
begin
  case BatchType of
    batExternal :  BatchFile := Value;
    batInternal :
      begin
        if SameText( ParamName, stdcReplaceVars) then
          ReplaceVars := Boolean( StrToInt( Value))
        else if SameText( ParamName, stdcBatchLineCount) then begin
          BatchStrings.Clear;
          for i:=1 to StrToInt(Value) do
            BatchStrings.Add( '');
        end
        else begin
          for i:=0 to BatchStrings.Count-1 do
            if SameText( ParamName, stdcBatchLine + IntToStr(i+1)) then begin
               BatchStrings[i] := Value;
               Break;
            end;
        end;
      end;
  end;
end;

function TBatchModule.Get_ParamNames(Index: Integer): WideString;
begin
  case BatchType of
    batExternal : Result := stdcBatchfile;
    batInternal :
      begin
        if Index = 0 then
          Result := stdcReplaceVars
        else if Index = 1 then
          Result := stdcBatchLineCount
        else
          Result := stdcBatchLine + IntToStr( Index-1);
      end;
  end;
end;

function TBatchModule.Get_ParamCount: Integer;
begin
  case BatchType of
    batExternal : Result := 1;
    batInternal : Result := 2 + BatchStrings.Count;
  end;
end;

function TBatchModuleCallback.GetIdentifier: WideString;
begin
  Result := IDBatchFilesmodule;
end;

procedure TBatchModule.CaptureOutput(const Line: WideString; var Aborted: WordBool);
begin
  Aborted := Canceled;
  jvcsmak.LogMessage(  Line);
end;

function TBatchModule.Get_Properties: IDispatch;
begin
  Result := nil;
end;

function TBatchModule.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := 0;
end;

function TBatchModule.Get_PreviewText: WideString;
var i:Integer;
begin
  if BatchType = batExternal then
    Result := BatchFile
  else begin
    Result:='';
    for i:=0 to BatchStrings.Count-1 do begin
      if i<4 then
        if i=0 then
          Result := Result + BatchStrings[i]
        else
          Result := Result + #10#13 + BatchStrings[i]
    end;
    if BatchStrings.Count>3 then
      Result := Result + #10#13 + stdMore;
  end;
end;

function TBatchModule.Get_OwnerDraw: WordBool;
begin
  Result := false;
end;

{ TBatchInternalModuleCallback }

function TBatchInternalModuleCallback.GetIdentifier: WideString;
begin
  Result := IDBatchInternal;
end;

procedure TBatchInternalModuleCallback.SetCanceled(aCanceled: WordBool);
begin
  Canceled := aCanceled;
end;

function TBatchInternalModuleCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TBatchModule.CreateEx(nil, batInternal));
end;


end.
