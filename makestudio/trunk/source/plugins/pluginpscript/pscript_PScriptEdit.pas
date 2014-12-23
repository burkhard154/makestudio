{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsplugintemplate_Module.pas

The Initial Developer of the original code (JEDI Make) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2006/04/21  BSchranz  - Plugin Script created
2005/05/27  BSchranz  - Released

------------------------------------------------------------------------------}
unit pscript_PScriptEdit;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynEditHighlighter, SynHighlighterPas,
  Menus, uPSCompiler, uPSRuntime,
  StdCtrls, ComCtrls, uPSComponent_COM, uPSComponent_StdCtrls,
  uPSComponent_Forms, uPSComponent_Default, uPSComponent_Controls, uPSComponent,
  uPSDebugger, ExtCtrls, ImgList, JvExExtCtrls, JvNetscapeSplitter, ActnList,
  ToolWin, JvMenus, pscript_Vars, PSTypInfo;

type
  TFormEditScriptParams = class(TForm)
    pashighlighter: TSynPasSyn;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    ActionList1: TActionList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    acNew: TAction;
    acLoad: TAction;
    acSave: TAction;
    acOk: TAction;
    acCancel: TAction;
    acCompile: TAction;
    acRun: TAction;
    acStepOver: TAction;
    acStepInto: TAction;
    acReset: TAction;
    acToggleBreakPoint: TAction;
    acShowTypeInfo: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    JvMainMenu1: TJvMainMenu;
    Ok1: TMenuItem;
    Cancel1: TMenuItem;
    File1: TMenuItem;
    Run1: TMenuItem;
    New1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    Run2: TMenuItem;
    N1: TMenuItem;
    Reset1: TMenuItem;
    StepInto1: TMenuItem;
    StepOver1: TMenuItem;
    N2: TMenuItem;
    Compile1: TMenuItem;
    N3: TMenuItem;
    oggleBreakpoint1: TMenuItem;
    JvXPMenuItemPainter1: TJvXPMenuItemPainter;
    Ce: TPSScriptDebugger;
    PSDllPlugin1: TPSDllPlugin;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    InfoTree: TTreeView;
    Panel2: TPanel;
    ed: TSynEdit;
    Panel5: TPanel;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    Panel6: TPanel;
    JvNetscapeSplitter2: TJvNetscapeSplitter;
    Messages: TListBox;
    BrowserImages: TImageList;
    procedure edDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure edDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure InfoTreeDblClick(Sender: TObject);
    procedure acCompileExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acOkExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure BreakPointMenuClick(Sender: TObject);
    procedure ceLineInfo(Sender: TObject; const FileName: String; Position, Row, Col: Cardinal);
    procedure Exit1Click(Sender: TObject);
    procedure StepOver1Click(Sender: TObject);
    procedure StepInto1Click(Sender: TObject);
    procedure Reset1Click(Sender: TObject);
    procedure ceIdle(Sender: TObject);
    procedure Run2Click(Sender: TObject);
    procedure ceAfterExecute(Sender: TPSScript);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    function ceNeedFile(Sender: TObject; const OrginFileName: String;
      var FileName, Output: String): Boolean;
    procedure ceBreakpoint(Sender: TObject; const FileName: String; Position, Row, Col: Cardinal);
  private
    FActiveLine: Longint;
    FResume: Boolean;
    FActiveFile: string;
    function Compile: Boolean;
    function Execute: Boolean;

    procedure SetActiveFile(const Value: string);
    procedure FillRefTree( fTypInfo:TPSTypInfoList);

    property aFile: string read FActiveFile write SetActiveFile;
  public
    function SaveCheck: Boolean;
  end;

implementation

uses PSUtils;


{$R *.dfm}

procedure TFormEditScriptParams.edSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  if ce.HasBreakPoint(ce.MainFileName, Line) then
  begin
    Special := True;
    if Line = FActiveLine then
    begin
      BG := clWhite;
      FG := clRed;
    end else
    begin
      FG := clWhite;
      BG := clRed;
    end;
  end else
  if Line = FActiveLine then
  begin
    Special := True;
    FG := clWhite;
    bg := clBlue;
  end else Special := False;
end;

procedure TFormEditScriptParams.BreakPointMenuClick(Sender: TObject);
var
  Line: Longint;
begin
  Line := Ed.CaretY;
  if ce.HasBreakPoint(ce.MainFileName, Line) then
    ce.ClearBreakPoint(ce.MainFileName, Line)
  else
    ce.SetBreakPoint(ce.MainFileName, Line);
  ed.Refresh;
end;

procedure TFormEditScriptParams.ceLineInfo(Sender: TObject; const FileName: String; Position, Row,
  Col: Cardinal);
begin
  if ce.Exec.DebugMode <> dmRun then
  begin
    FActiveLine := Row;
    if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
    begin
      Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
    end;
    ed.CaretY := FActiveLine;
    ed.CaretX := 1;

    ed.Refresh;
  end;
end;

procedure TFormEditScriptParams.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormEditScriptParams.StepOver1Click(Sender: TObject);
begin
  if ce.Exec.Status = isRunning then
    ce.StepOver
  else
  begin
    if Compile then
    begin
      ce.StepInto;
      Execute;
    end;
  end;
end;

procedure TFormEditScriptParams.StepInto1Click(Sender: TObject);
begin
  if ce.Exec.Status = isRunning then
    ce.StepInto
  else
  begin
    if Compile then
    begin
      ce.StepInto;
      Execute;
    end;
  end;
end;

procedure TFormEditScriptParams.Reset1Click(Sender: TObject);
begin
  if ce.Exec.Status = isRunning then
    ce.Stop;
end;

function TFormEditScriptParams.Compile: Boolean;
var
  i: Longint;
begin
  ce.Script.Assign(ed.Lines);
  Result := ce.Compile;
  messages.Clear;
  for i := 0 to ce.CompilerMessageCount -1 do
  begin
    Messages.Items.Add(ce.CompilerMessages[i].MessageToString);
  end;
  if Result then
    Messages.Items.Add(StrSuccesfullyCompiled);
end;

procedure TFormEditScriptParams.ceIdle(Sender: TObject);
begin
  Application.HandleMessage;
  if FResume then
  begin
    FResume := False;
    ce.Resume;
    FActiveLine := 0;
    ed.Refresh;
  end;
end;

procedure TFormEditScriptParams.Run2Click(Sender: TObject);
begin
  if CE.Running then
  begin
    FResume := True
  end else
  begin
    if Compile then
      Execute;
  end;
end;

procedure TFormEditScriptParams.ceAfterExecute(Sender: TPSScript);
begin
  FActiveLine := 0;
  ed.Refresh;
end;

function TFormEditScriptParams.Execute: Boolean;
begin
  if CE.Execute then
  begin
    Messages.Items.Add(StrSuccesfullyExecute);
    Result := True;
  end else
  begin
    messages.Items.Add('Runtime Error: '+ce.ExecErrorToString + ' at ['+IntToStr(ce.ExecErrorRow)+':'+IntToStr(ce.ExecErrorCol)+'] bytecode pos:'+inttostr(ce.ExecErrorProcNo)+':'+inttostr(ce.ExecErrorByteCodePosition));
    Result := False;
  end;
end;

procedure TFormEditScriptParams.New1Click(Sender: TObject);
begin
  if SaveCheck then
  begin
    ed.ClearAll;
    ed.Modified := False;
    aFile := '';
  end;
end;

procedure TFormEditScriptParams.Open1Click(Sender: TObject);
begin
  if SaveCheck then
  begin
    if OpenDialog1.Execute then
    begin
      ed.ClearAll;
      ed.Lines.LoadFromFile(OpenDialog1.FileName);
      ed.Modified := False;
      aFile := OpenDialog1.FileName;
    end;
  end;
end;

procedure TFormEditScriptParams.Save1Click(Sender: TObject);
begin
  if aFile <> '' then
  begin
    ed.Lines.SaveToFile(aFile);
    ed.Modified := False;
  end else
    SaveAs1Click(nil);
end;

procedure TFormEditScriptParams.Saveas1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    aFile := SaveDialog1.FileName;
    ed.Lines.SaveToFile(aFile);
    ed.Modified := False;
  end;
end;

function TFormEditScriptParams.SaveCheck: Boolean;
begin
  if ed.Modified then
  begin
    case MessageDlg('File has not been saved, save now?', mtConfirmation, mbYesNoCancel, 0) of
      idYes:
        begin
          Save1Click(nil);
          Result := aFile <> '';
        end;
      IDNO: Result := True;
      else
        Result := False;
    end;
  end else Result := True;
end;

function TFormEditScriptParams.ceNeedFile(Sender: TObject; const OrginFileName: String;
  var FileName, Output: String): Boolean;
var
  path: string;
  f: TFileStream;
begin
  if aFile <> '' then
    Path := ExtractFilePath(aFile)
  else
    Path := ExtractFilePath(ParamStr(0));
  Path := Path + FileName;
  try
    F := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  except
    Result := false;
    exit;
  end;
  try
    SetLength(Output, f.Size);
    f.Read(Output[1], Length(Output));
  finally
    f.Free;
  end;
  Result := True;
end;

procedure TFormEditScriptParams.ceBreakpoint(Sender: TObject; const FileName: String; Position, Row,
  Col: Cardinal);
begin
  FActiveLine := Row;
  if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
  begin
    Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
  end;
  ed.CaretY := FActiveLine;
  ed.CaretX := 1;

  ed.Refresh;
end;

procedure TFormEditScriptParams.SetActiveFile(const Value: string);
begin
  FActiveFile := Value;
  ce.MainFileName := ExtractFileName(FActiveFile);
  if Ce.MainFileName = '' then
    Ce.MainFileName := 'Unnamed';
end;

procedure TFormEditScriptParams.FormCreate(Sender: TObject);
begin
  ce.OnCompile := PSHandler.OnCompile;
  ce.OnCompImport := PSHandler.OnCompImport;
  ce.OnExecute := PSHandler.OnExecute;
  ce.OnExecImport := PSHandler.OnExecImport;
end;

procedure TFormEditScriptParams.FillRefTree( fTypInfo:TPSTypInfoList);

  procedure AddItem( aItem:TPSTypInfoItem; aNode:TTreeNode);
  var it:TTreeNode;
      i:Integer;
  begin
    it := InfoTree.Items.AddChild( aNode, aItem.Text);
    it.ImageIndex := aItem.ImageIndex;
    it.SelectedIndex := aItem.ImageIndex;
    it.StateIndex := aItem.ImageIndex;
    if (aNode = nil) or (aNode.Level=1) then
      Application.ProcessMessages;
    for i:=0 to aItem.Items.Count-1 do
      AddItem( aItem.Items[i], it);
  end;

var i:Integer;
begin
  InfoTree.Items.BeginUpdate;
  try
    InfoTree.Items.Clear;
    for i:=0 to fTypInfo.Count-1 do
      AddItem( fTypInfo.Items[i], nil);

    InfoTree.SortType := stText;
  finally
    InfoTree.Items.EndUpdate;
  end;
end;

procedure TFormEditScriptParams.FormShow(Sender: TObject);
begin
  Screen.Cursor := crAppStart;
  try
    FillRefTree( PSHandler.BrowserInfo);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditScriptParams.acOkExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

procedure TFormEditScriptParams.acCancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormEditScriptParams.acCompileExecute(Sender: TObject);
begin
  Compile;
end;

procedure TFormEditScriptParams.InfoTreeDblClick(Sender: TObject);
var s:String;
begin
  if InfoTree.Selected<>nil then begin
    s := ed.Lines.Text;
    Insert( InfoTree.Selected.Text, s, ed.RowColToCharIndex( ed.CaretXY) );
    ed.Lines.Text := s;
  end;
end;

procedure TFormEditScriptParams.edDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Source = InfoTree then
    Accept := true;
end;

procedure TFormEditScriptParams.edDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var s:String;
begin
  if Source = InfoTree then
    if InfoTree.Selected<>nil then begin
      s := ed.Lines.Text;
      Insert( InfoTree.Selected.Text, s, ed.RowColToCharIndex( ed.CaretXY) );
      ed.Lines.Text := s;
    end;
end;

end.
