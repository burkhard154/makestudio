(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msInternalCommands.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/08/12  BSchranz  - Command "SaveLog" added
2005/09/02  BSchranz  - Moved if, else etc to msprgram
2005/09/02  BSchranz  - migrated TSetVariable to ICommand2
2005/09/04  BSchranz  - Translated to englisch
2005/09/12  USchuster - D5 fix and minor style cleaning

-----------------------------------------------------------------------------*)

unit msInternalCommands;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ComObj, ActiveX, StdVCL, Graphics, makestudio_TLB,
  Classes, Windows, Dialogs, Controls, SysUtils, Forms,
  msprogram, msvarhandler, JclFileUtils, JclStrings;

type

  TSetVariableOperation = ( svoNone,
                            svoIncrement,
                            svoDecrement,
                            svoAddBackslash,
                            svoRemoveBackslash,
                            svoDateTime,
                            svoVersionInfo);


  TSetVariable = class(TComponent, ICommand2)
  private
    FVarname: string;
    FValue: OleVariant;
    FDataType: varBaseType;
    FOperation: TSetVariableOperation;
    FAppend: Boolean;
    FVersionFilename: string;
    FDateTimeFormat: string;
    FVersionFormat: string;
    FReplaceVars:Boolean;
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

    property Properties: IDispatch read Get_Properties;
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Varname: string read FVarname write FVarname;
    property VarValue: OleVariant read FValue write FValue;
    property DataType: varBaseType read FDataType write FDataType;
    property Operation: TSetVariableOperation read FOperation write FOperation;
    property Append: Boolean read FAppend write FAppend;
    property VersionFilename: string read FVersionFilename write FVersionFilename;
    property DateTimeFormat: string read FDateTimeFormat write FDateTimeFormat;
    property VersionFormat: string read FVersionFormat write FVersionFormat;
    property ReplaceVars:Boolean read FReplaceVars write FReplaceVars;
  end;

  TSaveLog = class(TComponent, ICommand)
  private
    //FCaption: string;
    FFilename: string;
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

    property Filename: string read FFilename write FFilename;
  end;

  TSetVariableCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

  TSaveLogCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;


const
  IDSETVARIABLE = 'jvcsmak.setvariable';
  IDSAVELOG = 'jvcsmak.savelog';

  stdcOperation = 'Operation';
  stdcAppend = 'Append';
  stdcVersionFilename = 'Versionfilename';
  stdcDateTimeFormat = 'DateTimeFormat';
  stdcVersionFormat = 'VersionFormat';
  stdcReplaceVars = 'ReplaceVars';

resourcestring
  strsvoNone                =  'Set variable <%s> = %s';
  strsvoIncrement           =  'Increment variable <%s>';
  strsvoDecrement           =  'Decrement variable <%s>';
  strsvoAddBackslash        =  'Add trailing backslash to variable <%s>';
  strsvoRemoveBackslash     =  'Remove trailing backslash from variable <%s>';
  strsvoDateTime            =  'Read date/time into variable <%s>';
  strsvoVersionInfo         =  'Read version information into variable <%s>';

procedure AddInternalCommands;
procedure ReleaseInternalCommands;

implementation

{$R commands.res}

uses
  {$IFNDEF D5TODO}
  {$IFDEF DELPHI5}
  JclSysUtils,
  {$ENDIF DELPHI5}
  {$ENDIF ~D5TODO}
  {$IFDEF DELPHI5}
  FileCtrl,
  {$ENDIF DELPHI5}
  {$IFDEF DELPHI6_UP}
  Variants, Types,
  {$ENDIF DELPHI6_UP}
  ComServ, msResources, msGlobals, msUtils,
  msEditSetVariable, msEditSaveLog;


var
  FSetVariableCallback: TSetVariableCallback;
  FSaveLogCallback: TSaveLogCallback;


procedure ReleaseInternalCommands;
begin
  FSetVariableCallback.Free;
  FSaveLogCallback.Free;

end;

procedure AddInternalCommands;
var
  bmp: Graphics.TBitmap;
begin
  bmp := Graphics.TBitmap.Create;
  try
    FSetVariableCallback := TSetVariableCallback.Create(nil);
    FSaveLogCallback := TSaveLogCallback.Create(nil);

    bmp.LoadFromResourceName(HInstance, UpperCase(IDSETVARIABLE));
    CommandTypes.Add(stdSetVariableCaption, '', stSystemCategory,
      bmp, '', -1, ICommandCallback(FSetVariableCallback));
    bmp.LoadFromResourceName(HInstance, UpperCase(IDSAVELOG));
    CommandTypes.Add(stdSaveLogCaption, '', stSystemCategory,
      bmp, '', -1, ICommandCallback(FSaveLogCallback));
  finally
    bmp.Free;
  end;
end;

{ TSetVariableCallback }

function TSetVariableCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TSetVariable.Create(nil));
end;

procedure TSetVariableCallback.SetCanceled(ACanceled: WordBool);
begin
end;

function TSetVariableCallback.GetIdentifier: WideString;
begin
  Result := IDSETVARIABLE;
end;

{ TSetVariable }

constructor TSetVariable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVarname  := '';
  FValue    := '';
  FDataType := varBaseString;
  FOperation := svoNone;
  FAppend := False;
  FVersionFilename := '';
  FDateTimeFormat := 'YYYY-MM-DD HH:MM:SS';
  FVersionFormat := '%V1.%V2.%V3.%V4';
  FReplaceVars := False;
end;

destructor TSetVariable.Destroy;
begin
  inherited Destroy;
end;

function TSetVariable.EditItem: WordBool;
begin
  Result := DlgEditSetVariable(Self);
end;

function TSetVariable.ExecuteItem: WordBool;
var
  I: Integer;
  B: Boolean;
  S: string;
  F: Double;

  verinfo: TJclFileVersionInfo;
  sl: TStringList;
begin
  Result := True;
  S := '';
  try
    case Operation of
      svoNone:
        case DataType of
          varBaseString:
            begin
              if Append then
                S := xVarToString(Varhandler.GetVar(Varname)) + VarValue
              else
                S := VarValue;
              if ReplaceVars then
                S :=  Varhandler.ReplaceVarsInString(S);
              Varhandler.SetVarEx(Varname, S);
            end;
          varBaseBool:
            begin
              B := VarValue;
              Varhandler.SetVarEx(Varname, B);
            end;
          varBaseInteger:
            begin
              I := VarValue;
              Varhandler.SetVarEx(Varname, I);
            end;
          varBaseFloat:
            begin
              F := VarValue;
              Varhandler.SetVarEx(Varname, F);
            end;
          else
            begin
              S := VarValue;
              Varhandler.SetVarEx(Varname, S);
            end;
        end;

      svoIncrement:
        begin
          I := xVarToInt(Varhandler.GetVar(Varname)) + 1;
          Varhandler.SetVarEx(Varname, I);
        end;
      svoDecrement:
        begin
          I := xVarToInt(Varhandler.GetVar(Varname)) - 1;
          Varhandler.SetVarEx(Varname, I);
        end;
      svoAddBackslash:
        begin
          S := PathAddSeparator(xVarToString(Varhandler.GetVar(Varname)));
          if ReplaceVars then
            Varhandler.ReplaceVarsInString(S);
          Varhandler.SetVarEx(Varname, S);
        end;
      svoRemoveBackslash:
        begin
          S := PathRemoveSeparator(xVarToString(Varhandler.GetVar(Varname)));
          if ReplaceVars then
            Varhandler.ReplaceVarsInString(S);
          Varhandler.SetVarEx(Varname, S);
        end;
      svoDateTime:
        begin
          S := FormatDateTime(DateTimeFormat, Now);
          if Append then
            S := xVarToString(Varhandler.GetVar(Varname)) + S;
          if ReplaceVars then
            Varhandler.ReplaceVarsInString(S);
          Varhandler.SetVarEx(Varname, S);
        end;
      svoVersionInfo:
        begin
          try
            verinfo := TJclFileVersionInfo.Create(Varhandler.ReplaceVarsInString(VersionFilename));
            sl := TStringList.Create;
            try
              S := verinfo.FileVersion;
              StrTokenToStrings(S, '.', sl);
              S := VersionFormat;
              if sl.Count > 0 then
                S := StringReplace(S, '%V1', sl[0], [rfReplaceAll])
              else
                S := StringReplace(S, '%V1', '0', [rfReplaceAll]);

              if sl.Count > 1 then
                S := StringReplace(S, '%V2', sl[1], [rfReplaceAll])
              else
                S := StringReplace(S, '%V2', '0', [rfReplaceAll]);

              if sl.Count > 2 then
                S := StringReplace(S, '%V3', sl[2], [rfReplaceAll])
              else
                S := StringReplace(S, '%V3', '0', [rfReplaceAll]);

              if sl.Count > 3 then
                S := StringReplace(S, '%V4', sl[3], [rfReplaceAll])
              else
                S := StringReplace(S, '%V4', '0', [rfReplaceAll]);

              if Append then
                S := xVarToString(Varhandler.GetVar(Varname)) + S;

              if ReplaceVars then
                Varhandler.ReplaceVarsInString(S);

              Varhandler.SetVarEx(Varname, S);
            finally
              sl.Free;
              verinfo.Free;
            end;
          except
            On E: Exception do
            begin
              AddLog(Format(stdErrSetVariableLog, [Varname, S]));
              AddLog(E.Message);
              Result := False;
            end;
          end;
        end;
    end;

    AddLog(Format(stdSetVariableLog, [Varname, xVarToString(Varhandler.GetVar(Varname))]));
  except
    S := VarValue;
    AddLog(Format(stdErrSetVariableLog, [Varname, S]));
    Result := False;
  end
end;

function TSetVariable.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(Caption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TSetVariable.DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;

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

    Canvas.Brush.Color := BkColor;
    Canvas.FillRect(aRect);

    Offset := 2;

    Canvas.Font.Style := [fsBold];
    SetCanvasTextColor(clWindowText);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, Caption);
    //Offset := Offset + Canvas.TextHeight(Caption) + 2;
  finally
    Canvas.Free;
  end;
end;

procedure TSetVariable.SetFilename(const Filename: WideString);
begin
end;

function TSetVariable.Get_Caption: WideString;
begin
  case Operation of
    svoNone: Result := Format(strsvoNone, [ Varname, VarValue]);
    svoIncrement: Result := Format(strsvoIncrement, [ Varname]);
    svoDecrement: Result := Format(strsvoDecrement, [ Varname]);
    svoAddBackslash: Result := Format(strsvoAddBackslash, [ Varname]);
    svoRemoveBackslash: Result := Format(strsvoRemoveBackslash, [ Varname]);
    svoDateTime: Result := Format(strsvoDateTime, [ Varname]);
    svoVersionInfo: Result := Format(strsvoVersionInfo, [ Varname]);
  end;
end;

procedure TSetVariable.Set_Caption(const Value: WideString);
begin
end;

{$IFNDEF D5TODO}
{$IFDEF DELPHI5}
function BoolToStr(ABoolean: Boolean; UseBoolStrs: Boolean = False): string;
begin
  if not UseBoolStrs then
  begin
    if ABoolean then
      Result := '1'
    else
      Result := '0';
  end
  else
  begin
    if ABoolean then
      Result := 'True'
    else
      Result := 'False';
  end;
end;

function StrToBoolDef(AStr: string; ADefValue: Boolean): Boolean;
begin
  try
    Result := StrToBoolean(AStr);
  except
    Result := ADefValue;
  end;
end;
{$ENDIF DELPHI5}
{$ENDIF ~D5TODO}

function TSetVariable.Get_ParamValues(const ParamName: WideString): WideString;
begin
  if ParamName = stdcVarname then
    Result := Varname
  else
  if ParamName = stdcDataType then
    Result := IntToStr(DataType)
  else
  if ParamName = stdcValue then
    Result := VarValue;
  if ParamName = stdcOperation then
    Result := IntToStr(Ord(Operation));
  if ParamName = stdcAppend then
    Result := BoolToStr(Append);
  if ParamName = stdcVersionFilename then
    Result := VersionFilename;
  if ParamName = stdcDateTimeFormat then
    Result := DateTimeFormat;
  if ParamName = stdcVersionFormat then
    Result := VersionFormat;
  if ParamName = stdcReplaceVars then
    Result := BoolToStr( ReplaceVars);
end;

procedure TSetVariable.Set_ParamValues(const ParamName: WideString; const Value: WideString);
begin
  if ParamName = stdcVarname then
    Varname := Value;
  if ParamName = stdcDataType then
    DataType := StrToIntDef(Value, 0);
  if ParamName = stdcValue then
    VarValue := Value;
  if ParamName = stdcOperation then
    Operation := TSetVariableOperation(StrToIntDef(Value, 0));
  if ParamName = stdcAppend then
    Append := StrToBoolDef(Value, False);
  if ParamName = stdcVersionFilename then
    VersionFilename := Value;
  if ParamName = stdcDateTimeFormat then
    DateTimeFormat := Value;
  if ParamName = stdcVersionFormat then
    VersionFormat := Value;
  if ParamName = stdcReplaceVars then
    ReplaceVars := StrToBoolDef(Value, False);
end;

function TSetVariable.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0: Result := stdcVarname;
    1: Result := stdcDataType;
    2: Result := stdcValue;
    3: Result := stdcOperation;
    4: Result := stdcAppend;
    5: Result := stdcVersionFilename;
    6: Result := stdcDateTimeFormat;
    7: Result := stdcVersionFormat;
    8: Result := stdcReplaceVars;
  end;
end;

function TSetVariable.Get_ParamCount: Integer;
begin
  Result := 9;
end;

function TSetVariable.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := False;
end;

function TSetVariable.Get_PreviewText: WideString;
begin
  case Operation of
    svoNone: Result := '';
    svoIncrement: Result := '';
    svoDecrement: Result := '';
    svoAddBackslash: Result := '';
    svoRemoveBackslash: Result := '';
    svoDateTime: Result := DateTimeFormat + ' (' + FormatDateTime(DateTimeFormat, Now) + ')';
    svoVersionInfo: Result := VersionFilename + ' (' + VersionFormat + ')';
  end;
end;

function TSetVariable.Get_OwnerDraw: WordBool;
begin
  Result := False;
end;

function TSetVariable.Get_Properties: IDispatch;
begin
  Result := nil;
end;


{ TSaveLog }

constructor TSaveLog.Create(AOwner: TComponent);
begin
  inherited;
  FFilename  := '';
end;

function TSaveLog.Get_ParamValues(const ParamName: WideString): WideString;
begin
  if ParamName = stdcFilename then
    Result := Filename
  else
    Result := '';
end;

procedure TSaveLog.SetFilename(const Filename: WideString);
begin

end;

procedure TSaveLog.Set_Caption(const Value: WideString);
begin
end;

function TSaveLog.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(Caption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TSaveLog.EditItem: WordBool;
begin
  Result := DlgEditSaveLog(Self);
end;

function TSaveLog.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0: Result := stdcFilename;
  end;
end;

procedure TSaveLog.Set_ParamValues(const ParamName, Value: WideString);
begin
  if ParamName = stdcFilename then
    Filename := Value;
end;

function TSaveLog.ExecuteItem: WordBool;
begin
  AddLog(Format(stdDoSaveLog, [Filename]));
  SaveLog(Filename);
  Result := FileExists(Filename);
end;

function TSaveLog.Get_Caption: WideString;
begin
  Result := stdSaveLogCaption + ' - <' + Filename + '>';
end;

destructor TSaveLog.Destroy;
begin
  inherited;
end;

function TSaveLog.DrawItem(Handle, Left, Top, Right, Bottom: Integer; Selected,
  BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;

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

    Canvas.Brush.Color := BkColor;
    Canvas.FillRect(aRect);

    Offset := 2;

    Canvas.Font.Style := [fsBold];
    SetCanvasTextColor(clWindowText);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, Caption);
    //Offset := Offset + Canvas.TextHeight(Caption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TSaveLog.Get_ParamCount: Integer;
begin
  Result := 1;
end;

{ TSaveLogCallback }

function TSaveLogCallback.GetIdentifier: WideString;
begin
  Result := IDSAVELOG;
end;

procedure TSaveLogCallback.SetCanceled(ACanceled: WordBool);
begin

end;

function TSaveLogCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TSaveLog.Create(nil));
end;



end.
