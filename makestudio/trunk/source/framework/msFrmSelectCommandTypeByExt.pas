(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msFrmSelectCommandTypeByExt.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:                                                                                    }

2005/09/11  BSchranz  - used if a file extension is associated more than once
2005/09/12  USchuster - D5 fix and minor style cleaning

-----------------------------------------------------------------------------*)

unit msFrmSelectCommandTypeByExt;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, StdCtrls, ExtCtrls, msprogram;

type
  TFormSelectCommandtypeByExt = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btOk: TButton;
    btCancel: TButton;
    Panel3: TPanel;
    MemoComment: TMemo;
    Panel4: TPanel;
    ImageList: TImageList;
    lvCommands: TListView;
    procedure lvCommandsDblClick(Sender: TObject);
  private
    procedure FillCommandList(List: TCommandTypeItemList);
  public
    { Public-Deklarationen }
  end;

//returns nil if no Item selected
function DlgSelectCommandTypeItemByExt(aList: TCommandTypeItemList; Filename: string): TCommandTypeItem;

implementation

{$R *.dfm}

function DlgSelectCommandTypeItemByExt(aList: TCommandTypeItemList; Filename: string): TCommandTypeItem;
begin
  Result := nil;
  with TFormSelectCommandtypeByExt.Create(Application) do
  try
    MemoComment.Lines.Text := Format(MemoComment.Lines.Text, [Filename]);
    FillCommandList(aList);
    if ShowModal = mrOk then
    begin
      if lvCommands.Selected <> nil then
        Result := TCommandTypeItem(lvCommands.Selected.Data);
    end;
  finally
    Free;
  end;
end;

{ TFormSelectCommandtypeByExt }

procedure TFormSelectCommandtypeByExt.FillCommandList(
  List: TCommandTypeItemList);
var
  I: Integer;
  it: TListItem;
begin
  for I := 0 to List.Count - 1 do
  begin
    it := lvCommands.Items.Add;
    it.Caption := List.Items[I].Name;
    it.SubItems.Add(List.Items[I].Hint);

    if not List.Items[I].Bitmap.Empty then
    begin
      if (List.Items[I].Bitmap.Width = ImageList.Width) and
         (List.Items[I].Bitmap.Height = ImageList.Height) then
      begin
        ImageList.AddMasked(List.Items[I].Bitmap,
              List.Items[I].Bitmap.TransparentColor);
      end;
    end;

    it.ImageIndex := ImageList.Count-1;
    it.StateIndex := ImageList.Count-1;
    it.Data := List.Items[I];
  end;

  if lvCommands.Items.Count > 0 then
    lvCommands.Selected := lvCommands.Items[0];
end;

procedure TFormSelectCommandtypeByExt.lvCommandsDblClick(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

end.
