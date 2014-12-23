(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msEditCopyFilesModule.pas

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

-----------------------------------------------------------------------------*)

unit utils_EditCopyFilesModule;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, utils_FileCopy, StdCtrls, Mask, JvToolEdit, ComCtrls,
  ExtCtrls, JvExMask;

type
  TFormCopyFilesEdit = class(TForm)
    Panel1: TPanel;
    lvFiles: TListView;
    Panel2: TPanel;
    Label1: TLabel;
    edSource: TJvFilenameEdit;
    Label2: TLabel;
    edTarget: TJvFilenameEdit;
    Button1: TButton;
    Button2: TButton;
    btAdd: TButton;
    btDelete: TButton;
    btReplace: TButton;
    procedure btAddClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btReplaceClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgEditCopyFilesModule(M: TCopyFilesModule): Boolean;

implementation

{$R *.dfm}

function DlgEditCopyFilesModule(M: TCopyFilesModule): Boolean;
var
  it: TListItem;
  I: Integer;
begin
  Result := False;
  with TFormCopyFilesEdit.Create(nil) do
  try
    for I:=0 to M.SourceFiles.Count-1 do
    begin
      it := lvFiles.Items.Add;
      it.Caption := M.SourceFiles[I];
      it.SubItems.Add(M.TargetFiles[I]);
    end;

    if ShowModal = mrOk then
    begin
      M.SourceFiles.Clear;
      M.TargetFiles.Clear;
      for I:=0 to lvFiles.Items.Count-1 do
      begin
        M.SourceFiles.Add(lvFiles.Items[I].Caption);
        M.TargetFiles.Add(lvFiles.Items[I].SubItems[0]);
      end;
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TFormCopyFilesEdit.btAddClick(Sender: TObject);
var
  it: TListItem;
begin
  if (edSource.FileName <> '') and (edTarget.FileName <> '') then
  begin
    it := lvFiles.Items.Add;
    it.Caption := edSource.FileName;
    it.SubItems.Add(edTarget.FileName);
  end;
end;

procedure TFormCopyFilesEdit.btDeleteClick(Sender: TObject);
begin
  if lvFiles.Selected <> nil then
    lvFiles.Items.Delete(lvFiles.Selected.Index);
end;

procedure TFormCopyFilesEdit.btReplaceClick(Sender: TObject);
begin
  if lvFiles.Selected <> nil then
  begin
    lvFiles.Selected.Caption := edSource.FileName;
    lvFiles.Selected.SubItems[0] := edTarget.FileName;
  end;
end;

end.
