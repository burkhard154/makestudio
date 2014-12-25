(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: LogbookFrame.pas

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
2003/11/28  USchuster - 2nd Migrationstep (fixed header and removed Variants)
2003/12/05  USchuster - re-formatted
2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit MakeStudio_LogbookFrame;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, ToolWin, ActnList, ExtCtrls, Menus,
  JvMenus;

type
  TFrameLogbook = class(TFrame)
    SaveDialog1: TSaveDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    ListFound: TListView;
    Logbook: TMemo;
    ActionList1: TActionList;
    ImageList2: TImageList;
    acKeywords: TAction;
    acSaveLogbook: TAction;
    acNewLogbook: TAction;
    Panel5: TPanel;
    JvPopupMenu1: TJvPopupMenu;
    JvPopupMenu2: TJvPopupMenu;
    Schlsselwrter1: TMenuItem;
    Logbuchleeren1: TMenuItem;
    Logbuchspeichern1: TMenuItem;
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure acKeywordsExecute(Sender: TObject);
    procedure ListFoundDblClick(Sender: TObject);
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

implementation

{$R *.dfm}

uses
  MakeStudio_EditKeywordsForm;

constructor TFrameLogbook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Keywords := TStringList.Create;
end;

destructor TFrameLogbook.Destroy;
begin
  Keywords.Free;
  inherited Destroy;
end;

procedure TFrameLogbook.AddLogEntryStrings(sl: TStrings);
var
  I: Integer;
begin
  for I := 0 to sl.Count - 1 do
    AddLogEntry(sl[I]);
end;

procedure TFrameLogbook.ClearLogEntrys;
begin
  Logbook.Clear;
  ListFound.Items.Clear;
end;

procedure TFrameLogbook.BuildListFound;
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

procedure TFrameLogbook.AddLogEntry(S: string);
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

procedure TFrameLogbook.EditKeywords;
begin
  DlgEditKeywords(Keywords);
end;

procedure TFrameLogbook.LoadKeywords;
begin
  if FileExists(ChangeFileExt(Application.ExeName, '.kwd')) then
    Keywords.LoadFromFile(ChangeFileExt(Application.ExeName, '.kwd'));
end;

procedure TFrameLogbook.SaveKeywords;
begin
  Keywords.SaveToFile(ChangeFileExt(Application.ExeName, '.kwd'));
end;

procedure TFrameLogbook.ToolButton4Click(Sender: TObject);
begin
  Logbook.Lines.Clear;
end;

procedure TFrameLogbook.ToolButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Logbook.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TFrameLogbook.acKeywordsExecute(Sender: TObject);
begin
  EditKeywords;
  BuildListFound;
end;

procedure TFrameLogbook.ListFoundDblClick(Sender: TObject);
begin
  if ListFound.Selected <> nil then
  begin
    Logbook.SetFocus;
    Logbook.SelStart := Integer(ListFound.Selected.Data);
    Logbook.SelLength := Length(ListFound.Selected.SubItems[0]);
  end;
end;

end.
