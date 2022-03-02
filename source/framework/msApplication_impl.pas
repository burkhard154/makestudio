(* -----------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: msApplication_impl.pas

  The Initial Developer of the original DMAK-Code is:
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
  2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
  2005/02/04  USchuster - preparations for check in
  2005/02/08  BSchranz  - Error fixed in ExecCmdLine (SetCurrentDir)
  2005/02/19  USchuster - changes for commandline version
  2005/06/21  USchuster - implemented IJApplication2
  2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
  2005/04/09  BSchranz  - Translated to englisch
  2006/06/15  BSchranz  - Bug fixed in ExecCmdLine


  ----------------------------------------------------------------------------- *)

unit msApplication_impl;

{$I jedi.inc}
{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  ActiveX, ActnList, AxCtrls, ComCtrls, ComObj, Controls, Graphics, makestudio_TLB,
  Menus, StdVCL, SysUtils, Dialogs, JclSysUtils;

type
  TJApplication = class(TAutoObject, IJApplication, IJApplication2)
  private
    FAbortExec: Boolean;
    FExecCallback: IExecCallback;
    FMakeKind: EMakeKind;
  protected
    function LoadFromFile(const Filename: WideString): WordBool; safecall;
    procedure Run; safecall;
    procedure AddCommandType(const CommandName: WideString; const CommandHint: WideString;
      const CommandCategory: WideString; const Bitmap: IPictureDisp;
      const DragDropFileExtensions: WideString; CompatibilityIndex: Integer;
      const Callback: IDispatch); safecall;
    procedure AddMenuAction(const ActionName: WideString; const Caption: WideString;
      const Hint: WideString; const Bitmap: IPictureDisp; const Callback: IDispatch); safecall;
    procedure LogMessage(const Value: WideString); safecall;
    procedure AddAdditionalInfo(const Value: WideString); safecall;
    procedure AddCreditInfo(const Value: WideString); safecall;
    function Get_ApplicationRegKey: WideString; safecall;
    function Get_ApplicationDataFolder: WideString; safecall;
    procedure AddCommandCategory(const ACaption: WideString; const APicture: IPictureDisp);
      safecall;
    function Get_Variables: IVars; safecall;
    function ExecCmdLine(const App: WideString; const Args: WideString; const Dir: WideString;
      const Callback: IExecCallback): Integer; safecall;
    function Get_ParentHandle: Integer; safecall;

    function Get_MakeKind: EMakeKind; safecall;
    function Get_ApplicationHandle: Integer; safecall;
    function Get_ApplicationLanguage: WideString; safecall;
    procedure SetStatus(const Text: WideString); safecall;
    procedure AddCommandByFile(const aFilename: WideString); safecall;
    procedure AddCommand(const CommandID: WideString); safecall;

    property ApplicationRegKey: WideString read Get_ApplicationRegKey;
    property ApplicationDataFolder: WideString read Get_ApplicationDataFolder;
    property Variables: IVars read Get_Variables;

    procedure OnCaptureLine(const Text: string);
    procedure OnIdle(Sender: TObject);
    procedure ShowHelp(const Topic: WideString); safecall;
  public
    constructor Create(AMakeKind: EMakeKind = mkGUI);
  end;

resourcestring
  stdMenuModulePrefix = 'New ';

implementation

uses
  Classes, ComServ, Forms, msGlobals, msMain, msProgram,
  msUtils, msResources, msVarHandler, mshelp,
  msFrmSelectCommandTypeByExt;

resourcestring
  StrExcutionCanceled = 'Excution canceled!';
  StrInternalErrorCall = 'Internal Error: Callback for ExecCmdLine is NULL!';

constructor TJApplication.Create(AMakeKind: EMakeKind = mkGUI);
begin
  inherited Create;
  FMakeKind := AMakeKind;
end;

function TJApplication.LoadFromFile(const Filename: WideString): WordBool;
begin
  Programhandler.LoadFromFile(Filename);
end;

procedure TJApplication.Run;
begin
  Programhandler.Execute;
end;

procedure TJApplication.AddCommandType(const CommandName: WideString; const CommandHint: WideString;
  const CommandCategory: WideString; const Bitmap: IPictureDisp;
  const DragDropFileExtensions: WideString; CompatibilityIndex: Integer; const Callback: IDispatch);
var
  bmp: TBitmap;
  pic: TPicture;
begin
  try
    if (FMakeKind = mkGUI) then
    begin
      if (Bitmap <> nil) then
      begin
        pic := TPicture.Create;
        bmp := TBitmap.Create;
        try
          SetOlePicture(pic, Bitmap);
          bmp.Width := CommandTypes.ImageWidth;
          bmp.Height := CommandTypes.ImageWidth;
          bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), pic.Graphic);
          if (bmp.Width > 0) and (bmp.Height > 0) then
            CommandTypes.Add(CommandName, CommandHint, CommandCategory, bmp, DragDropFileExtensions,
              CompatibilityIndex, Callback)
          else
            CommandTypes.Add(CommandName, CommandHint, CommandCategory, nil, DragDropFileExtensions,
              CompatibilityIndex, Callback);
        finally
          pic.Free;
          bmp.Free;
        end;
      end
      else
        CommandTypes.Add(CommandName, CommandHint, CommandCategory, nil, DragDropFileExtensions,
          CompatibilityIndex, Callback);
    end
    else
      CommandTypes.Add(CommandName, CommandHint, CommandCategory, nil, DragDropFileExtensions,
        CompatibilityIndex, Callback);
  except
    on E: Exception do
    begin
      AddLog(Format(stErrAddCommand, [CommandName]));
      AddLog(E.Message);
    end;
  end;
end;

procedure TJApplication.AddMenuAction(const ActionName: WideString; const Caption: WideString;
  const Hint: WideString; const Bitmap: IPictureDisp; const Callback: IDispatch);
var
  pic: TPicture;
  bmp: TBitmap;
begin
  if (FMakeKind = mkGUI) then
  begin
    pic := TPicture.Create;
    bmp := TBitmap.Create;
    try
      if Bitmap <> nil then
      begin
        SetOlePicture(pic, Bitmap);
        bmp.Width := FormMain.ActionList1.Images.Width;
        bmp.Height := FormMain.ActionList1.Images.Width;
        bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), pic.Graphic);
        Actionhandler.Add(ActionName, Caption, Hint, bmp, IDispatch(Callback));
      end
      else
      begin
        Actionhandler.Add(ActionName, Caption, Hint, nil, IDispatch(Callback));
      end;
    finally
      pic.Free;
      bmp.Free;
    end;
  end;
end;

procedure TJApplication.LogMessage(const Value: WideString);
begin
  AddLog(Value);
end;

procedure TJApplication.AddAdditionalInfo(const Value: WideString);
begin
  PluginHandler.AdditionalInfo.Add(Value);
end;

procedure TJApplication.AddCreditInfo(const Value: WideString);
begin
  PluginHandler.Credits.Add(Value);
end;

function TJApplication.Get_ApplicationRegKey: WideString;
begin
  Result := GetMakeStudioBaseRegistryKey;
end;

function TJApplication.Get_ApplicationDataFolder: WideString;
begin
  Result := GetJAppDataFolder;
end;

procedure TJApplication.AddCommandCategory(const ACaption: WideString;
  const APicture: IPictureDisp); safecall;
begin
  // nothing to do yet
end;

function TJApplication.Get_Variables: IVars;
begin
  Result := Varhandler;
end;

function TJApplication.ExecCmdLine(const App: WideString; const Args: WideString;
  const Dir: WideString; const Callback: IExecCallback): Integer;
begin
  Result := 0; // return exec result in the future
  FExecCallback := Callback;

  if not SetCurrentDir(Dir) then
    AddLog('Error ExecCmdLine:SetCurrentDir("' + Dir + '")');

  if FExecCallback <> nil then
    Result := JclSysUtils.Execute(App + ' ' + Args, OnCaptureLine, true, @FAbortExec)
  else
    ShowMessage(StrInternalErrorCall);

  FExecCallback := nil;
  if FAbortExec then
    AddLog(StrExcutionCanceled);
end;

function TJApplication.Get_ParentHandle: Integer;
begin
  Result := Application.Handle;
end;

function TJApplication.Get_MakeKind: EMakeKind; safecall;
begin
  Result := FMakeKind;
end;

function TJApplication.Get_ApplicationHandle: Integer; safecall;
begin
  Result := Application.Handle;
end;

function TJApplication.Get_ApplicationLanguage: WideString; safecall;
begin
  Result := 'de';
end;

procedure TJApplication.OnCaptureLine(const Text: string);
var
  sl: TStringList;
  S: string;
  I: Integer;
  B: WordBool;
begin
  B := FAbortExec;

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
        FExecCallback.CaptureOutput(sl[I], B);
    finally
      sl.Free;
    end;
  end
  else
    FExecCallback.CaptureOutput(Text, B);

  FAbortExec := B;
end;

procedure TJApplication.OnIdle(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TJApplication.ShowHelp(const Topic: WideString);
begin
  if Topic = '' then
    HelpContent(GetHelpFile)
  else
    HelpKeyword(GetHelpFile, Topic);
end;

procedure TJApplication.SetStatus(const Text: WideString);
begin
  if Assigned(Application) then
    if Assigned(FormMain) then
    begin
      FormMain.StatusBar.Panels[1].Text := Text;
      Application.ProcessMessages;
    end;
end;

procedure TJApplication.AddCommandByFile(const aFilename: WideString);
var
  M: TCommand;
  m1: TCommandTypeItem;
  li: TCommandTypeItemList;
  ext:String;
begin
  with Programhandler do
  begin
    ext := SysUtils.ExtractFileExt(aFilename);
    li := CommandTypes.GetItemsByExtension( ext);
    try
      if li.Count > 1 then
        m1 := DlgSelectCommandTypeItemByExt(li, aFilename)
      else
        m1 := CommandTypes.GetItemByExtension(ext);

      if m1 <> nil then
      begin
        M := TCommand.Create(m1, Programhandler);
        M.SetFilename(aFilename);
        Add(M);
        Programhandler.Modified := true;
      end;
    finally
      li.Free;
    end;
  end;
end;

procedure TJApplication.AddCommand(const CommandID: WideString);
var
  M: TCommand;
  m1: TCommandTypeItem;
begin
  m1 := CommandTypes.GetItemByID( CommandID);
  if m1<>nil then begin
    M := TCommand.Create(m1, Programhandler);
    Programhandler.Add(M);
    Programhandler.Modified := true;
  end;
end;

initialization

TAutoObjectFactory.Create(ComServer, TJApplication, CLASS_JApplication, ciMultiInstance,
  tmApartment);

end.
