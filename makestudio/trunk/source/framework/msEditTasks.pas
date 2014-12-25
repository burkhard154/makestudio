(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msEditTasks.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/23  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/04/09  BSchranz  - Translated to englisch


-----------------------------------------------------------------------------*)

unit msEditTasks;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, JclTask, ImgList;

type
  TFormEditTasks = class(TForm)
    Panel1: TPanel;
    btClose: TButton;
    btEdit: TButton;
    btDelete: TButton;
    Panel2: TPanel;
    lvTasks: TListView;
    ImageList1: TImageList;
    procedure btDeleteClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
    procedure lvTasksDblClick(Sender: TObject);
  private
    ms: TJclTaskSchedule;
    procedure FillList;
  public
    { Public-Deklarationen }
  end;

procedure DlgEditTasks(Scheduler: TJclTaskSchedule);

implementation

{$R *.dfm}

procedure DlgEditTasks(Scheduler: TJclTaskSchedule);
begin
  with TFormEditTasks.Create(nil) do
  try
    ms := Scheduler;
    FillList;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormEditTasks.FillList;
var
  I: Integer;
  li: TListItem;
begin
  lvTasks.Items.Clear;
  ms.Refresh;
  for I := 0 to ms.TaskCount - 1 do
  begin
    li := lvTasks.Items.Add;
    li.ImageIndex := 0;
    li.OverlayIndex := 0;
    li.StateIndex := 0;
    li.Caption := ms.Tasks[I].TaskName;
    li.SubItems.Add(ms.Tasks[I].ApplicationName);
    li.SubItems.Add(ms.Tasks[I].Parameters);
  end;
end;

procedure TFormEditTasks.btDeleteClick(Sender: TObject);
begin
  if lvTasks.Selected <> nil then
  begin
    ms.Delete(lvTasks.Selected.Index);
    FillList;
  end;
end;

procedure TFormEditTasks.btEditClick(Sender: TObject);
begin
  if lvTasks.Selected <> nil then
  begin
    if ms.Tasks[lvTasks.Selected.Index].ShowPage then
      FillList;
  end;
end;

procedure TFormEditTasks.lvTasksDblClick(Sender: TObject);
begin
  btEditClick(Sender);
end;

end.
