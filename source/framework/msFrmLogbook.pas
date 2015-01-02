(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msfrmlogbook.pas

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

2005/26/08  BSchranz  - Created
2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
2005/04/09  BSchranz  - Translated to englisch
2005/04/09  USchuster - D5 fix

-----------------------------------------------------------------------------*)

unit msFrmLogbook;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, JvMenus, ImgList, ActnList, StdCtrls, ComCtrls, ExtCtrls,
  JvComponent, JvDockControlForm, msprogram, msglobals,
  JvEmbeddedForms, JvComponentBase, System.Actions;

type
  TFormLogbook = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    ListFound: TListView;
    Panel4: TPanel;
    Logbook: TMemo;
    SaveDialog1: TSaveDialog;
    ActionList1: TActionList;
    acKeywords: TAction;
    acSaveLogbook: TAction;
    acNewLogbook: TAction;
    ImageList2: TImageList;
    JvPopupMenu1: TJvPopupMenu;
    Schlsselwrter1: TMenuItem;
    JvPopupMenu2: TJvPopupMenu;
    Logbuchleeren1: TMenuItem;
    Logbuchspeichern1: TMenuItem;
    JvDockClient: TJvDockClient;
    Link: TJvEmbeddedFormLink;
    N1: TMenuItem;
    Copyselectedtext1: TMenuItem;
    acCopyTxt: TAction;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure acKeywordsExecute(Sender: TObject);
    procedure ListFoundDblClick(Sender: TObject);
    procedure acCopyTxtExecute(Sender: TObject);
  private
    Keywords: TStringList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddLogEntry(S: string);
    procedure AddLogEntryStrings(sl: TStrings);
    procedure ClearLogEntrys;
    procedure BuildListFound;
    procedure EditKeywords;
    procedure LoadKeywords;
    procedure SaveKeywords;
  end;

var
  FormLogbook: TFormLogbook;

implementation

{$R *.dfm}

uses
  {$IFDEF DELPHI6_UP}
  DateUtils,
  {$ENDIF DELPHI6_UP}
  msEditKeywordsForm, msUtils;

procedure FAddLog(S: string);
begin
  FormLogbook.AddLogEntry(S);
  Forms.Application.ProcessMessages;
end;

procedure FAddLogStrings(sl: TStringList);
begin
  FormLogbook.AddLogEntryStrings(sl);
  Forms.Application.ProcessMessages;
end;

procedure FClearLog;
begin
  FormLogbook.ClearLogEntrys;
  Forms.Application.ProcessMessages;
end;

procedure FSaveLog( Filename:String);
begin
  try
    FormLogbook.Logbook.Lines.SaveToFile( Filename);
  except
    On E:Exception do FAddLog( E.Message);
  end
end;

//:Stores the Logbook with date and time and filename in then Appdata folder
procedure FSaveLogAuto;
var
  S: string;
  Y, M, D, H, min, sec, msec: Word;
begin
  {$IFDEF DELPHI6_UP}
  DecodeDateTime(Now, Y, M, D, H, min, sec, msec);
  {$ELSE}
  DecodeDate(Now, Y, M, D);
  DecodeTime(Now, H, min, sec, msec);
  {$ENDIF DELPHI6_UP}
  if Programhandler.Filename<>'' then
    S := ChangeFileExt(ExtractFileName(Programhandler.Filename), '') + ' ';
  S := S + Format('%d_%d_%d (%d_%d_%d).txt', [Y, M, D, H, min, sec]);
                     
  S := GetJAppDataFolder + S;
  FormLogbook.Logbook.Lines.SaveToFile(S);
end;

constructor TFormLogbook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Keywords := TStringList.Create;

  //Logbook
  _AddLog := FAddLog;
  _AddLogStrings := FAddLogStrings;
  _ClearLog := FClearLog;
  _SaveLogAuto := FSaveLogAuto;
  _SaveLog := FSaveLog;
  ProgramAddLog := FAddLog;
  ProgramClearLog := FClearLog;
end;

destructor TFormLogbook.Destroy;
begin
  //Logbook
  _AddLog := nil;
  _AddLogStrings := nil;
  _ClearLog := nil;
  _SaveLogAuto := nil;
  _SaveLog := nil;
  ProgramAddLog := nil;
  ProgramClearLog := nil;

  SaveKeywords;
  Keywords.Free;
  inherited Destroy;
end;

procedure TFormLogbook.AddLogEntryStrings(sl: TStrings);
var
  I: Integer;
begin
  for I := 0 to sl.Count - 1 do
    AddLogEntry(sl[I]);
end;

procedure TFormLogbook.ClearLogEntrys;
begin
  Logbook.Clear;
  ListFound.Items.Clear;
end;

procedure TFormLogbook.BuildListFound;
var
  I, K: Integer;
  s1: string;
  P: Integer;
begin
  ListFound.Items.Clear;
  for K := 0 to Logbook.Lines.Count - 1 do
  begin
    s1 := UpperCase(Logbook.Lines[K]);
    for I := 0 to Keywords.Count - 1 do
    begin
      P := Pos(Keywords[I], s1);
      if P > 0 then
      begin
        with ListFound.Items.Add do
        begin
          Caption := IntToStr(K + 1) + ' - ' + Keywords[I];
          SubItems.Add(Keywords[I]);
          ImageIndex := 3;
          StateIndex := ImageIndex;
          Data := Pointer(Length(Logbook.Text) + P - 1);
        end;
      end;
    end;
  end;
end;

procedure TFormLogbook.AddLogEntry(S: string);
var
  I: Integer;
  s1: string;
  P: Integer;
begin
  s1 := UpperCase(S);
  for I := 0 to Keywords.Count - 1 do
  begin
    P := Pos(Keywords[I], s1);
    if P > 0 then
    begin
      with ListFound.Items.Add do
      begin
        Caption := IntToStr(Logbook.Lines.Count + 1) + ' - ' + Keywords[I];
        SubItems.Add(Keywords[I]);
        ImageIndex := 3;
        StateIndex := ImageIndex;
        Data := Pointer(Length(Logbook.Text) + P - 1);
      end;
    end;
  end;
  Logbook.Lines.Add(S);
  Logbook.SelStart := Length(Logbook.Text);
end;

procedure TFormLogbook.EditKeywords;
begin
  DlgEditKeywords(Keywords);
end;

procedure TFormLogbook.LoadKeywords;
begin
  if FileExists(ChangeFileExt(Application.ExeName, '.kwd')) then
    Keywords.LoadFromFile(ChangeFileExt(Application.ExeName, '.kwd'));
end;

procedure TFormLogbook.SaveKeywords;
begin
  try
    Keywords.SaveToFile(ChangeFileExt(Application.ExeName, '.kwd'));
  except
  end;
end;

procedure TFormLogbook.ToolButton4Click(Sender: TObject);
begin
  Logbook.Lines.Clear;
end;

procedure TFormLogbook.ToolButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Logbook.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TFormLogbook.acCopyTxtExecute(Sender: TObject);
begin
  // Copy selected text
  Logbook.CopyToClipboard;
end;

procedure TFormLogbook.acKeywordsExecute(Sender: TObject);
begin
  EditKeywords;
  BuildListFound;
end;

procedure TFormLogbook.ListFoundDblClick(Sender: TObject);
begin
  if ListFound.Selected <> nil then
  begin
    Logbook.SetFocus;
    Logbook.SelStart := Integer(ListFound.Selected.Data);
    Logbook.SelLength := Length(ListFound.Selected.SubItems[0]);
  end;
end;

procedure TFormLogbook.FormCreate(Sender: TObject);
begin
  ImageList2.GetIcon( 32, Icon);
  LoadKeywords;
end;

end.
