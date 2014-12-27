(* -----------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: main.pas

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
  2003/11/28  USchuster - 2nd Migrationstep
  (- changed TActionManager, TActionMainMenuBar and TActionToolBar
  to TActionList, TMainMenu and TToolBar
  - removed unused EventHandlers procedures
  - D5 ListBox fix)
  2003/12/05  USchuster - re-formatted
  - fixed #bf006 (remove last filename from caption on new file)
  - fixed #bf008 (fixed name of logbook in menu)
  - fixed #bf010 (changed makefile filtername and extension)
  - fixed #bf012 (create forms on demand)
  - changed Caption
  - fixed selection of Delphiversion for D5
  2004/02/24  USchuster - Update to JVCL 3.0 / JCL 1.90 (mantis #1353)
  2004/05/01  USchuster - changes according to the latest version of msModuleDataClass.pas
  2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
  2005/02/04  USchuster - preparations for check in
  2005/08/12  BSchranz  - command line version "jmak.exe" added
  2005/08/25  BSchranz  - Docking added, copy, paste..., debugging
  2005/04/09  BSchranz  - Translated to englisch
  2006/02/05  BSchranz  - Include Support Added
  2006/06/07  USchuster - D5 fix
  2006/06/17  USchuster - fix for D9 and lower(removed D10 properties from .dfm)
  - fixed command menu creation(supports sub categories now)

  ----------------------------------------------------------------------------- *)

unit msMain;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ActnList, StdCtrls, ImgList, ExtCtrls, Menus, ToolWin, msProgram,
  AppEvnts, ShellAPI, JvComponent, JvDragDrop,
  makestudio_TLB, JvExComCtrls, JvProgressBar, Buttons, JvMenus, msVarHandler,
  JvComCtrls, JvDockControlForm, JvDockVIDStyle, JvDockVSNetStyle,
  JvExControls, JvWaitingGradient, JvStatusBar, JclTask, JvListView, JclFileUtils,
  JvEmbeddedForms, JvDockSupportProc, JvMRUManager, JvAppStorage,
  JvAppRegistryStorage, JvFormPlacement, JvXPCore, JvExExtCtrls, JvControlBar,
  JvDockTree, mshelp, mshelpmerger, JvToolBar, JvComponentBase,
  JclStrings, System.Actions;

type
  TDefaultDockProc = procedure of object;

  TFormMain = class(TForm, IProgramNotification)
    ImageList1: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ApplicationEvents1: TApplicationEvents;
    ActionList1: TActionList;
    acAddModule: TAction;
    acDeleteModule: TAction;
    acModuleUp: TAction;
    acModuleDown: TAction;
    acExit: TAction;
    acRun: TAction;
    acOpen: TAction;
    acSave: TAction;
    acNew: TAction;
    acEditModule: TAction;
    acLogbook: TAction;
    acToggleView: TAction;
    acExpandList: TAction;
    acCollapse: TAction;
    acPrint: TAction;
    acModulePath: TAction;
    acSeparator: TAction;
    acInfo: TAction;
    acBriefView: TAction;
    Datei1: TMenuItem;
    Neu1: TMenuItem;
    ffnen1: TMenuItem;
    Speichern1: TMenuItem;
    Beenden1: TMenuItem;
    mnView: TMenuItem;
    exteditor1: TMenuItem;
    Extra1: TMenuItem;
    Modulverzeichnissebearbeiten1: TMenuItem;
    N8: TMenuItem;
    mnHelp: TMenuItem;
    Info1: TMenuItem;
    PopupNew: TPopupMenu;
    acSchedule: TAction;
    acOpenLogbookFolder: TAction;
    Logbuchordnerffnen1: TMenuItem;
    acStop: TAction;
    acVarhandler: TAction;
    JvMainMenu1: TJvMainMenu;
    acSyntaxCheck: TAction;
    mnRun: TMenuItem;
    Start2: TMenuItem;
    Stopp1: TMenuItem;
    N9: TMenuItem;
    Syntaxberprfung1: TMenuItem;
    acToggleBreakpoint: TAction;
    Haltepunktumschalten1: TMenuItem;
    StatusBar: TJvStatusBar;
    acEditSchedule: TAction;
    mnTasks: TMenuItem;
    GelpanteTasks1: TMenuItem;
    AuftragslistealsVorgangplanen1: TMenuItem;
    acCommandList: TAction;
    Bearbeiten1: TMenuItem;
    acCopy: TAction;
    acCut: TAction;
    acPaste: TAction;
    acDeleteSelection: TAction;
    acSelectAll: TAction;
    acDebugNext: TAction;
    Kopieren1: TMenuItem;
    Ausschneiden1: TMenuItem;
    Einfgen1: TMenuItem;
    N11: TMenuItem;
    AllesAuswhlen1: TMenuItem;
    N12: TMenuItem;
    Hinzufgen3: TMenuItem;
    Lschen4: TMenuItem;
    N13: TMenuItem;
    Befehlnachunten1: TMenuItem;
    Befehlnachunten2: TMenuItem;
    Kommandobearbeiten1: TMenuItem;
    N14: TMenuItem;
    N4: TMenuItem;
    NchsteAnweisungausfhren1: TMenuItem;
    JvMRUManager: TJvMRUManager;
    N6: TMenuItem;
    JvFormStorage: TJvFormStorage;
    JvAppRegistryStorage: TJvAppRegistryStorage;
    mnCommands: TMenuItem;
    acSaveAs: TAction;
    N3: TMenuItem;
    Speichern2: TMenuItem;
    acSaveDefaultDesktop: TAction;
    acSaveRunDesktop: TAction;
    acSaveDebugDesktop: TAction;
    JvXPStyleManager1: TJvXPStyleManager;
    acDebugCurrent: TAction;
    Runfromselectedcommand1: TMenuItem;
    JvDragDrop1: TJvDragDrop;
    acHelp: TAction;
    Help1: TMenuItem;
    JvToolBar1: TJvToolBar;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton35: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    ToolButton43: TToolButton;
    ToolButton44: TToolButton;
    ToolButton45: TToolButton;
    ToolButton46: TToolButton;
    Panel1: TPanel;
    PageControl1: TPageControl;
    tabCommands: TTabSheet;
    FormPanelCommands: TJvEmbeddedFormPanel;
    tabVars: TTabSheet;
    FormpanelVars: TJvEmbeddedFormPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    PageControl2: TPageControl;
    TabSheet1: TTabSheet;
    FormPanelLogbook: TJvEmbeddedFormPanel;
    Splitter2: TSplitter;
    MainFormPanel: TJvEmbeddedFormPanel;
    acComment: TAction;
    ToolButton1: TToolButton;
    procedure acHelpExecute(Sender: TObject);
    procedure JvDragDrop1Drop(Sender: TObject; Pos: TPoint; Value: TStrings);
    procedure acDebugCurrentExecute(Sender: TObject);
    procedure acDebugNextExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acDeleteSelectionExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure JvMRUManagerClick(Sender: TObject; const RecentName, Caption: string;
      UserData: Integer);
    procedure acCommandListExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure acAddModuleExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acDeleteModuleExecute(Sender: TObject);
    procedure acEditModuleExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acLogbookExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure acToggleViewExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acEditDirsExecute(Sender: TObject);
    procedure acModuleUpExecute(Sender: TObject);
    procedure acModuleDownExecute(Sender: TObject);
    procedure ApplicationEvents1Hint(Sender: TObject);
    procedure acInfoExecute(Sender: TObject);
    procedure acBriefViewExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acScheduleExecute(Sender: TObject);
    procedure acOpenLogbookFolderExecute(Sender: TObject);
    procedure acVarhandlerExecute(Sender: TObject);
    procedure acSyntaxCheckExecute(Sender: TObject);
    procedure acToggleBreakpointExecute(Sender: TObject);
    procedure acEditScheduleExecute(Sender: TObject);
    procedure acCommentExecute(Sender: TObject);
  private
    function CheckOpenModified: Boolean;
    procedure CheckHelpSystem;
  protected

    // Docking
    procedure SetMainformPanel;
    procedure ReleaseMainformPanel;

    procedure FillMenus;

    procedure OnBeforeStartProgram; stdcall;
    procedure OnAfterStopProgram; stdcall;
    procedure OnProgramPaused; stdcall;
    procedure OnSetProgramPosition(const ItemIndex: Integer); stdcall;
    procedure OnSetProgress(const ACaption: string; const ProgressMin, ProgressMax,
      ProgressPosition: Integer); stdcall;
    procedure OnClearItems; stdcall;
    procedure OnRefresh; stdcall;
    procedure OnAddItem(const ItemIndex: Integer); stdcall;
    procedure OnInsertItem(const InsertAt: Integer); stdcall;
    procedure OnDeleteItem(const ItemIndex: Integer); stdcall;
    procedure OnItemChanged(const ItemIndex: Integer); stdcall;
    procedure OnExchangeItem(const Index1, Index2: Integer); stdcall;
    procedure OnSelectionChanged(const ItemIndex: Integer); stdcall;

    // for future purpose - not used yet
    procedure OnNotify(const Notification: Integer; const Param: OLEVariant); stdcall;

    procedure UpdateActionList;
  public
    ms: TJclTaskSchedule;
    procedure UpdateCaption;

    procedure OnCreateNewModule(Sender: TObject); // Action for Module creation
  end;

var
  FormMain: TFormMain;

implementation

uses
{$IFDEF DELPHI6_UP}
  DateUtils,
{$ENDIF DELPHI6_UP}
  msRunListEdit, msEditDirectories,
  msEditTasks,
  msInfo, msGlobals, msResources,
  msUtils, msVarListInspect,
  msfrmLogbook, msfrmCommands, msfrmRunListListbox, Contnrs,
  msFrmSelectCommandTypeByExt;

{$R *.dfm}

procedure TFormMain.ToolButton2Click(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.acAddModuleExecute(Sender: TObject);
begin
  PopupNew.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
{$IFDEF DELPHI10_UP}
  JvToolBar1.DrawingStyle := ComCtrls.dsGradient;
{$ENDIF DELPHI10_UP}
  JvAppRegistryStorage.Root := GetMakeStudioBaseRegistryKey;
  Actionhandler.ActionList := ActionList1;
  Actionhandler.PluginMenu := Extra1;
  Caption := stCaption;

  OpenDialog.Filter := rsOpenSaveFileFilter;
  SaveDialog.Filter := rsOpenSaveFileFilter;

  acModuleUp.ShortCut := ShortCut(VK_UP, [ssCtrl]);
  acModuleDown.ShortCut := ShortCut(VK_DOWN, [ssCtrl]);
  acBriefView.Checked := True;
  ms := TJclTaskSchedule.Create;

  // Add Notification to Programhandler
  Programhandler.AddNotification(Self);

  // Create Logbook
  FormLogbook := TFormLogbook.Create(Application);

  // Load Plugins
  InitializeApplication;

  // Create all other Forms
  FormCommands := TFormCommands.Create(Application);
  FormVarlistInspect := TFormVarlistInspect.Create(Application);
  FormRunlistLisbox := TFormRunlistLisbox.Create(Application);

  // Fill Menu tree with commands
  FillMenus;

  UpdateActionList;

  CheckHelpSystem;
end;

procedure TFormMain.CheckHelpSystem;
var
  path: string;
  Merger: THelpMerger;
begin
  path := Varhandler.GetVar(tvar_HelpPath);
  Merger := THelpMerger.Create(nil);
  try
    Merger.HHPFilename := PathAddSeparator(path) + sHHPFilename;
    Merger.OutputFile := PathAddSeparator(path) + sCHMFilename;
    Merger.MainTopicCaption := strhHelpWelcomePage;
    Merger.DlgExecute;
  finally
    Merger.Free;
  end;

  Helpfile := PathAddSeparator(path) + sCHMFilename;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ms.Free;
end;

procedure TFormMain.acDeleteModuleExecute(Sender: TObject);
var
  I: TCommand;
begin
  if Programhandler.SelectedCount = 0 then
    Exit;

  I := Programhandler.Selected;
  Programhandler.DeleteSelectedItems;

  if Programhandler.Count > 0 then
  begin
    if Programhandler.IndexOf(I) < Programhandler.Count then
      Programhandler.Selected := I
    else
      Programhandler.Selected := Programhandler[Programhandler.Count - 1];
  end;

  Programhandler.Modified := True;
  UpdateCaption;
end;

procedure TFormMain.acEditModuleExecute(Sender: TObject);
begin
  if Programhandler.Selected <> nil then
    Programhandler.Selected.Edit;
end;

procedure TFormMain.acOpenExecute(Sender: TObject);
begin
  if CheckOpenModified then
    if OpenDialog.Execute then
    begin
      Programhandler.LoadFromFile(OpenDialog.Filename);
      JvMRUManager.Add(OpenDialog.Filename, 0);
    end;
end;

procedure TFormMain.acSaveExecute(Sender: TObject);
begin
  if (Programhandler.Filename <> '') and FileExists(Programhandler.Filename) then
    Programhandler.SaveToFile(Programhandler.Filename)
  else
    acSaveAsExecute(Sender);
end;

procedure TFormMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  SetMainformPanel;

  // command line
  if ParamCount > 0 then
  begin
    if Programhandler.LoadFromFile(ParamStr(1)) then
    begin
      if FindCmdLineSwitch('r', ['-', '/'], True) then
      begin
        Programhandler.Execute;
        if FindCmdLineSwitch('e', ['-', '/'], True) then
          Forms.Application.Terminate;
      end;
    end;
  end;
end;

procedure TFormMain.acLogbookExecute(Sender: TObject);
begin
  ShowDockForm(FormLogbook);
end;

procedure TFormMain.acRunExecute(Sender: TObject);
begin
  if not Programhandler.IsRun then
    Programhandler.Execute
  else
    Programhandler.IsPaused := False;
end;

procedure TFormMain.acToggleViewExecute(Sender: TObject);
begin
  FormMemo := TFormMemo.Create(Forms.Application); // unit msRunListEdit
  try
    FormMemo.Memo.Lines.Clear;
    FormMemo.Memo.ScrollBars := ssBoth;
    Programhandler.SaveToStrings(FormMemo.Memo.Lines);
    if FormMemo.ShowModal = mrOk then
    begin
      Programhandler.LoadFromStrings(FormMemo.Memo.Lines);
    end;
  finally
    FormMemo.Free;
  end;
end;

procedure TFormMain.acNewExecute(Sender: TObject);
var
  CanClose: Boolean;
begin
  CanClose := True;
  if Programhandler.Modified then
  begin
    case MessageDlg(stCloseQuery, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          if Programhandler.Filename <> '' then
            Programhandler.SaveToFile(Programhandler.Filename)
          else if SaveDialog.Execute then
          begin
            Programhandler.SaveToFile(SaveDialog.Filename);
          end
          else
            CanClose := False;
        end;
      mrNo:
        CanClose := True;
      mrCancel:
        CanClose := False;
    end;
  end
  else
    CanClose := True;

  if CanClose then
    Programhandler.New;
end;

procedure TFormMain.acEditDirsExecute(Sender: TObject);
begin
  FormEditDirectories := TFormEditDirectories.Create(Forms.Application);
  // unit msEditDirectories
  try
    FormEditDirectories.FillDirList;
    if FormEditDirectories.ShowModal = mrOk then
    begin
      Programhandler.Refresh;
    end;
  finally
    FormEditDirectories.Free;
  end;
end;

procedure TFormMain.acModuleUpExecute(Sender: TObject);
var
  i1, i2: Integer;
begin
  if Programhandler.Selected <> nil then
  begin
    i1 := Programhandler.IndexOf(Programhandler.Selected);
    if i1 > 0 then
    begin
      i2 := i1 - 1;
      Programhandler.Exchange(i1, i2);
    end;
  end;
end;

procedure TFormMain.acModuleDownExecute(Sender: TObject);
var
  i1, i2: Integer;
begin
  if Programhandler.Selected <> nil then
  begin
    i1 := Programhandler.IndexOf(Programhandler.Selected);
    if i1 < Programhandler.Count - 1 then
    begin
      i2 := i1 + 1;
      Programhandler.Exchange(i1, i2);
    end;
  end;
end;

procedure TFormMain.ApplicationEvents1Hint(Sender: TObject);
begin
  StatusBar.Panels[1].Text := Forms.Application.Hint;
end;

procedure TFormMain.acInfoExecute(Sender: TObject);
begin
  AboutBox := TAboutBox.Create(Forms.Application); // unit msInfo
  try
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;

procedure TFormMain.acBriefViewExecute(Sender: TObject);
begin
  acBriefView.Checked := not acBriefView.Checked;
  Programhandler.BriefView := not acBriefView.Checked;
  Programhandler.Refresh;
end;

procedure TFormMain.UpdateCaption;
begin
  Caption := stCaption + ' [' + Programhandler.Filename + ']';
  if Programhandler.Modified then
    Caption := Caption + '*';
  UpdateActionList;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if Programhandler.Modified then
  begin
    case MessageDlg(stCloseQuery, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          if Programhandler.Filename <> '' then
            Programhandler.SaveToFile(Programhandler.Filename)
          else if SaveDialog.Execute then
          begin
            Programhandler.SaveToFile(SaveDialog.Filename);
          end
          else
            CanClose := False;
        end;
      mrNo:
        CanClose := True;
      mrCancel:
        CanClose := False;
    end;
  end;
end;

procedure TFormMain.OnCreateNewModule(Sender: TObject);
var
  M: TCommand;

  procedure AddCommand;
  begin
    if Programhandler.SelectedCount > 0 then
    begin
      if Programhandler.Selected = Programhandler[Programhandler.Count - 1] then
      begin
        Programhandler.Add(M);
      end
      else
      begin
        Programhandler.Insert(Programhandler.IndexOf(Programhandler.Selected) + 1, M);
      end;
    end
    else
    begin
      Programhandler.Add(M);
    end;
  end;

begin
  if Sender is TAction then
  begin
    M := TCommand.Create(CommandTypes[TAction(Sender).Tag], Programhandler);
    if M.Edit then
    begin
      AddCommand;
      Programhandler.Modified := True;
      UpdateCaption;
      if Programhandler.IsSystemCommand(M) then
      begin
        Programhandler.Refresh;
      end;
    end
    else
      M.Free;
  end;
end;

function TFormMain.CheckOpenModified: Boolean;
begin
  Result := True;
  if Programhandler.Modified then
  begin
    case MessageDlg(stCloseQuery, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          if Programhandler.Filename <> '' then
            Programhandler.SaveToFile(Programhandler.Filename)
          else if SaveDialog.Execute then
          begin
            Programhandler.SaveToFile(SaveDialog.Filename);
          end
          else
            Result := False;
        end;
      mrNo:
        Result := True;
      mrCancel:
        Result := False;
    end;
  end;
end;

procedure TFormMain.acScheduleExecute(Sender: TObject);

  function GetExistingTask(ATaskName: string): TJclScheduledTask;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to ms.TaskCount - 1 do
      if SameText(ms.Tasks[I].TaskName, ATaskName) then
      begin
        Result := ms.Tasks[I];
        Break;
      end;
  end;

var
  Err: Boolean;
  S: string;
  mst: TJclScheduledTask;
begin
  if (Programhandler.Filename = '') or (Programhandler.Modified) then
  begin
    MessageDlg(stSaveFile, mtError, [mbOK], 0);
    Exit;
  end;

  S := ChangeFileExt(ExtractFileName(Programhandler.Filename), '');
  ms.Refresh;
  mst := GetExistingTask(S);

  if mst = nil then
    mst := ms.Add(S);

  if mst = nil then
  begin
    MessageDlg(stErrorCreatingSchedulerTask, mtError, [mbOK], 0);
    Exit;
  end;

  Err := True;
  if mst <> nil then
  begin
    mst.ApplicationName := PathAddSeparator(ExtractFilePath(Forms.Application.ExeName)) +
      'MakeStudioe.exe';
    mst.Parameters := '"' + Programhandler.Filename + '"';
    mst.WorkingDirectory := ExtractFilePath(Forms.Application.ExeName);
    mst.Save;
    if mst.ShowPage then
      Err := False;
  end;

  if Err then
    MessageDlg(stErrorFillingSchedulerTask, mtError, [mbOK], 0)
  else
    // MessageDlg(stSchedulerTaskCreated, mtInformation, [mbOK], 0)
end;

procedure TFormMain.acOpenLogbookFolderExecute(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(GetJAppDataFolder), '', PChar(GetJAppDataFolder), SW_SHOW);
end;

procedure TFormMain.acStopExecute(Sender: TObject);
begin
  Programhandler.ActiveProgram.Canceled := True;
end;

procedure TFormMain.acVarhandlerExecute(Sender: TObject);
begin
  ShowDockForm(FormVarlistInspect);
end;

procedure TFormMain.acSyntaxCheckExecute(Sender: TObject);
begin
  if Programhandler.CheckSyntax then
    MessageDlg(stdSyntaxOK, mtInformation, [mbOK], 0);
end;

procedure TFormMain.acToggleBreakpointExecute(Sender: TObject);
begin
  if Programhandler.SelectedCount > 0 then
  begin
    Programhandler.Selected.Breakpoint := not Programhandler.Selected.Breakpoint;

    if not Programhandler.ShowLines and Programhandler.Selected.Breakpoint then
      Programhandler.ShowLines := True;
  end;
end;

procedure TFormMain.acEditScheduleExecute(Sender: TObject);
begin
  DlgEditTasks(ms);
end;

procedure TFormMain.acCommandListExecute(Sender: TObject);
begin
  ShowDockForm(FormCommands);
end;

procedure TFormMain.acCommentExecute(Sender: TObject);
var
  I: Integer;
begin
  if Programhandler.SelectedCount > 0 then begin
    for I := 0 to Programhandler.Count - 1 do
      if Programhandler.Items[I].Selected then
        Programhandler.Items[I].IsIgnored := not Programhandler.Items[I].IsIgnored;
    Programhandler.Refresh;
  end;
end;

procedure TFormMain.ReleaseMainformPanel;
begin
  FormRunlistLisbox.Visible := False;
  FormLogbook.Visible := False;
  FormCommands.Visible := False;
  FormVarlistInspect.Visible := False;
  MainFormPanel.FormLink := nil;
  FormPanelLogbook.FormLink := nil;
  FormPanelCommands.FormLink := nil;
  FormpanelVars.FormLink := nil;
  Refresh;
end;

procedure TFormMain.SetMainformPanel;
begin
  MainFormPanel.FormLink := FormRunlistLisbox.Link;
  FormPanelLogbook.FormLink := FormLogbook.Link;
  FormPanelCommands.FormLink := FormCommands.Link;
  FormCommands.lvModules.Align := alLeft;
  FormPanelCommands.Align := alTop;
  FormPanelCommands.Align := alClient;
  FormCommands.lvModules.Align := alClient;
  FormpanelVars.FormLink := FormVarlistInspect.Link;
  Refresh;
end;

procedure TFormMain.FillMenus;

  function AddMenuCategory(mn: TMenuItem; ACategory: string): TMenuItem;
  var
    I: Integer;
    sl: TStringList;
    S: string;
  begin
    Result := nil;

    sl := TStringList.Create;
    try
      StrTokenToStrings(ACategory, '\', sl);
      if sl.Count > 0 then
      begin
        for I := 0 to mn.Count - 1 do
        begin
          if SameText(StringReplace(mn.Items[I].Caption, '&', '', [rfReplaceAll]), sl[0]) then
          begin
            Result := mn.Items[I];
            Break;
          end;
        end;
      end;

      if Result = nil then // not found
      begin
        Result := TMenuItem.Create(ActionList1.Owner);
        Result.Caption := sl[0];
        mn.Insert(0, Result);
      end;

      if sl.Count > 1 then
      begin
        S := '';
        for I := 1 to sl.Count - 1 do
        begin
          S := S + sl[I];
          if I < sl.Count - 1 then
            S := S + '\';
        end;
        Result := AddMenuCategory(Result, S);
      end;
    finally
      sl.Free;
    end;
  end;

var
  ac: TAction;
  mn: TMenuItem;
  I: Integer;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  for I := 0 to CommandTypes.Count - 1 do
  begin
    if not CommandTypes.Items[I].Bitmap.Empty then
    begin
      if (CommandTypes.Items[I].Bitmap.Width = ImageList1.Width) and
        (CommandTypes.Items[I].Bitmap.Height = ImageList1.Height) then
      begin
        CommandTypes.Items[I].Bitmap.PixelFormat := pf8bit;
        ImageList1.AddMasked(CommandTypes.Items[I].Bitmap,
          CommandTypes.Items[I].Bitmap.TransparentColor);
      end;
    end;

    // Add the Moduletype to Actionlist and Menus
    ac := TAction.Create(ActionList1.Owner);
    ac.Caption := CommandTypes.Items[I].Name;
    ac.Name := 'acmod' + IntToStr(ActionList1.ActionCount);
    ac.ImageIndex := ImageList1.Count - 1;
    ac.Tag := I;
    ac.OnExecute := OnCreateNewModule;
    ac.ActionList := ActionList1;

    // MainMenu
    mn := TMenuItem.Create(ActionList1.Owner);
    mn.Action := ac;
    AddMenuCategory(mnCommands, CommandTypes.Items[I].Category).Add(mn);

    // PopupNew Menu
    mn := TMenuItem.Create(ActionList1.Owner);
    mn.Action := ac;
    AddMenuCategory(PopupNew.Items, CommandTypes.Items[I].Category).Add(mn);

    { //PopupList Menu
      mn := TMenuItem.Create(ActionList1.Owner);
      mn.Action := ac;
      AddMenuCategory(PopupContext.Items, CommandTypes.Items[I].Category).Add(mn); }
  end;

end;

procedure TFormMain.OnProgramPaused;
begin
  UpdateActionList;
end;

procedure TFormMain.OnExchangeItem(const Index1, Index2: Integer);
begin
  UpdateCaption;
end;

procedure TFormMain.OnNotify(const Notification: Integer; const Param: OLEVariant);
begin
  UpdateCaption;
end;

procedure TFormMain.OnBeforeStartProgram;
var
  I: Integer;
  debug: Boolean;
begin
  // Disable everything

  for I := 0 to ActionList1.ActionCount - 1 do
    TAction(ActionList1.Actions[I]).Enabled := False;

  UpdateActionList;

  // debugging or not?
  debug := False;
  debug := Programhandler.HaltOnNextCommand;
  if not debug then
    for I := 0 to Programhandler.Count - 1 do
      debug := debug or Programhandler.Items[I].Breakpoint;

  Forms.Application.ProcessMessages;
end;

procedure TFormMain.OnSelectionChanged(const ItemIndex: Integer);
begin
  UpdateActionList;
end;

procedure TFormMain.OnClearItems;
begin
  UpdateCaption;
end;

procedure TFormMain.OnSetProgramPosition(const ItemIndex: Integer);
begin
end;

procedure TFormMain.OnDeleteItem(const ItemIndex: Integer);
begin
  UpdateCaption;
end;

procedure TFormMain.OnItemChanged(const ItemIndex: Integer);
begin
  UpdateCaption;
end;

procedure TFormMain.OnAfterStopProgram;
var
  I: Integer;
begin
  SaveLogAuto;

  // enable evereything again
  for I := 0 to ActionList1.ActionCount - 1 do
    TAction(ActionList1.Actions[I]).Enabled := True;

  UpdateActionList;
  acModulePath.Enabled := False; // !!! does not work yet

  Forms.Application.ProcessMessages;
end;

procedure TFormMain.OnInsertItem(const InsertAt: Integer);
begin
  UpdateCaption;
end;

procedure TFormMain.OnAddItem(const ItemIndex: Integer);
begin
  UpdateCaption;
end;

procedure TFormMain.OnSetProgress(const ACaption: string;
  const ProgressMin, ProgressMax, ProgressPosition: Integer);
begin
  StatusBar.Panels[1].Text := ACaption;
  Forms.Application.ProcessMessages;
end;

procedure TFormMain.OnRefresh;
begin
  UpdateCaption;
end;

procedure TFormMain.JvMRUManagerClick(Sender: TObject; const RecentName, Caption: string;
  UserData: Integer);
begin
  if CheckOpenModified then
    Programhandler.LoadFromFile(RecentName);
end;

procedure TFormMain.acSaveAsExecute(Sender: TObject);
begin
  SaveDialog.Filename := Programhandler.Filename;
  if SaveDialog.Execute then
  begin
    Programhandler.SaveToFile(SaveDialog.Filename);
    JvMRUManager.Add(SaveDialog.Filename, 0);
    UpdateCaption;
  end;
end;

procedure TFormMain.UpdateActionList;
begin
  acStop.Enabled := Programhandler.ActiveProgram.IsRun;
  acRun.Enabled := Programhandler.ActiveProgram.ActiveProgram.IsPaused or
    not Programhandler.ActiveProgram.IsRun;
  acDebugNext.Enabled := Programhandler.ActiveProgram.IsPaused or
    not Programhandler.ActiveProgram.IsRun;
  acDebugCurrent.Enabled := not Programhandler.ActiveProgram.IsRun;
  acToggleBreakpoint.Enabled := Programhandler.ActiveProgram.IsPaused or
    not(Programhandler.ActiveProgram.IsRun);
  acEditModule.Enabled := Programhandler.ActiveProgram.SelectedCount > 0;
  acCopy.Enabled := not Programhandler.ActiveProgram.IsRun and Programhandler.ActiveProgram.CanCopy;
  acCut.Enabled := not Programhandler.ActiveProgram.IsRun and Programhandler.ActiveProgram.CanCopy;
  acPaste.Enabled := not Programhandler.ActiveProgram.IsRun and
    Programhandler.ActiveProgram.CanPaste;
  acDeleteSelection.Enabled := (not Programhandler.ActiveProgram.IsRun) and
    (Programhandler.ActiveProgram.SelectedCount > 0);
end;

procedure TFormMain.acCutExecute(Sender: TObject);
begin
  Programhandler.CutSelectionToClipboard;
  UpdateActionList;
end;

procedure TFormMain.acCopyExecute(Sender: TObject);
begin
  Programhandler.CopySelectionToClipboard;
  UpdateActionList;
end;

procedure TFormMain.acPasteExecute(Sender: TObject);
begin
  Programhandler.PasteFromClipboard;
  UpdateActionList;
end;

procedure TFormMain.acDeleteSelectionExecute(Sender: TObject);
begin
  Programhandler.DeleteSelectedItems;
end;

procedure TFormMain.acSelectAllExecute(Sender: TObject);
begin
  Programhandler.SelectAll;
end;

procedure TFormMain.acDebugNextExecute(Sender: TObject);
begin
  if Programhandler.ActiveProgram.IsRun and Programhandler.ActiveProgram.IsPaused then
  begin
    Programhandler.ActiveProgram.HaltOnNextCommand := True;
    Programhandler.ActiveProgram.IsPaused := False;
  end
  else
  begin
    Programhandler.ActiveProgram.HaltOnNextCommand := True;
    Programhandler.ActiveProgram.Execute
  end;
end;

procedure TFormMain.acDebugCurrentExecute(Sender: TObject);
begin
  if Programhandler.Selected <> nil then
    if not Programhandler.IsRun then
    begin
      Programhandler.HaltOnNextCommand := True;
      Programhandler.Execute(Programhandler.IndexOf(Programhandler.Selected));
    end;
end;

procedure TFormMain.JvDragDrop1Drop(Sender: TObject; Pos: TPoint; Value: TStrings);
var
  M: TCommand;
  m1: TCommandTypeItem;
  li: TCommandTypeItemList;
  I: Integer;
begin
  with Programhandler do
  begin
    if Value.Count > 0 then
      Application.BringToFront;

    for I := 0 to Value.Count - 1 do
    begin
      li := CommandTypes.GetItemsByExtension(ExtractFileExt(Value[I]));
      try
        if li.Count > 1 then
          m1 := DlgSelectCommandTypeItemByExt(li, Value[I])
        else
          m1 := CommandTypes.GetItemByExtension(ExtractFileExt(Value[I]));

        if m1 <> nil then
        begin
          M := TCommand.Create(m1, Programhandler);
          M.SetFilename(Value[I]);
          Add(M);
          Programhandler.Modified := True;
        end;
      finally
        li.Free;
      end;
    end;
  end;
end;

procedure TFormMain.acHelpExecute(Sender: TObject);
begin
  Application.HelpFile := GetHelpFile;
  ShellExecute( 0, 'open', PChar(Application.HelpFile), nil, nil, SW_NORMAL);
//  Application.HelpSystem.ShowTableOfContents;
//  HelpContent(GetHelpFile);
end;

end.
