(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msEditMkdirModule.pas

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
2005/06/01  JDuenow   - bugfixes

-----------------------------------------------------------------------------*)

unit utils_EditMkDirModule;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls, JvBaseDlg, utils_MkDir,
  JvComponent, Mask, JvExMask, JvToolEdit, JclFileUtils;

type
  TFormMkdirModuleEdit = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    Label1: TLabel;
    ListDirectories: TListBox;
    Panel3: TPanel;
    btnAdd: TSpeedButton;
    btnRemove: TSpeedButton;
    edtDirectory: TJvDirectoryEdit;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgEditMkdirModule(M: TMkDirModule): Boolean;

implementation

{$R *.dfm}

function DlgEditMkdirModule(M: TMkDirModule): Boolean;
var
  I: Integer;
begin
  Result := False;
  with TFormMkdirModuleEdit.Create(nil) do
  try
    for I := 0 to M.Projects.Count - 1 do
      ListDirectories.Items.Add(M.Projects[I]);
    if ShowModal = mrOk then
    begin
      Result := True;
      M.Projects.Clear;
      for I := 0 to ListDirectories.Items.Count - 1 do
        M.Projects.Add(ListDirectories.Items[I]);
    end;
  finally
    Free;
  end;
end;

procedure TFormMkdirModuleEdit.btnAddClick(Sender: TObject);
begin
  if Length(edtDirectory.Text) > 0 then
  begin
    ListDirectories.Items.Add(PathAddSeparator(edtDirectory.Text));
    edtDirectory.Text := '';
    edtDirectory.SetFocus;
  end;
end;

procedure TFormMkdirModuleEdit.btnRemoveClick(Sender: TObject);
begin
  if ListDirectories.ItemIndex >= 0 then
    ListDirectories.Items.Delete(ListDirectories.ItemIndex);
end;

end.
