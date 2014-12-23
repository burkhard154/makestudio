(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EasyBackup_SelectFilesForm.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/05  BSchranz  - Easybackup Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit EasyBackup_SelectFilesForm;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, ExtCtrls, ToolWin,
  Grids, EasyBackup_FormCopy, Contnrs;

type
  TFormSelectFiles = class(TForm)
    ToolBar1: TToolBar;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    tbIgnore: TToolButton;
    tbCopy: TToolButton;  
    tbDelete: TToolButton;
    lv: TListView;
    procedure lvData(Sender: TObject; Item: TListItem);
    procedure lvSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure tbIgnoreClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
  private
    aList: TObjectList;
  public
    { Public-Deklarationen }
  end;

function SelectMissingFiles(mList: TObjectList): Boolean;

implementation

{$R *.dfm}

function SelectMissingFiles(mList: TObjectList): Boolean;
begin
  Result := False;
  with TFormSelectFiles.Create(nil) do
  try
    aList := mList;
    lv.Items.Count := aList.Count;
    Result := ShowModal = mrOk
  finally
    Free;
  end;
end;

procedure TFormSelectFiles.lvData(Sender: TObject;
  Item: TListItem);
var
  ob: TMissingFile;
begin
  if Item <> nil then
  begin
    ob := TMissingFile(aList[Item.Index]);
    Item.Caption := ExtractFileName(ob.SourceFile);
    Item.SubItems.Add(GetSyncOptionName(ob.CopyOption));
    Item.SubItems.Add(ExtractFilePath(ob.SourceFile));
    Item.SubItems.Add(DateTimeToStr(ob.SourceAge));
  end;
end;

procedure TFormSelectFiles.lvSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if (Item <> nil) and Selected then
  begin
    tbDelete.Down := False;
    tbCopy.Down := False;
    tbIgnore.Down := False;
    case TMissingFile(aList[Item.Index]).CopyOption of
      soDelete: tbDelete.Down := True;
      soCopy: tbCopy.Down := True;
      soIgnore: tbIgnore.Down := True;
    end;
  end;
end;

procedure TFormSelectFiles.Button2Click(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

procedure TFormSelectFiles.Button1Click(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

procedure TFormSelectFiles.tbIgnoreClick(Sender: TObject);
var
  I: Integer;
begin
  tbDelete.Down := False;
  tbCopy.Down := False;
  tbIgnore.Down := True;
  for I:=0 to lv.Items.Count-1 do
    if lv.Items[I].Selected then
    begin
      TMissingFile(aList[I]).CopyOption := soIgnore;
      lv.Items[I].SubItems[0] := GetSyncOptionName(soIgnore);
    end;
  lv.Refresh;
end;

procedure TFormSelectFiles.tbCopyClick(Sender: TObject);
var
  I: Integer;
begin
  tbDelete.Down := False;
  tbCopy.Down := True;
  tbIgnore.Down := False;
  for I:=0 to lv.Items.Count-1 do
    if lv.Items[I].Selected then
    begin
      TMissingFile(aList[I]).CopyOption := soCopy;
      lv.Items[I].SubItems[0] := GetSyncOptionName(soCopy);
    end;
  lv.Refresh;
end;

procedure TFormSelectFiles.tbDeleteClick(Sender: TObject);
var
  I: Integer;
begin
  tbDelete.Down := True;
  tbCopy.Down := False;
  tbIgnore.Down := False;
  for I := 0 to lv.Items.Count - 1 do
    if lv.Items[I].Selected then
    begin
      TMissingFile(aList[I]).CopyOption := soDelete;
      lv.Items[I].SubItems[0] := GetSyncOptionName(soDelete);
    end;
  lv.Refresh;
end;

end.
