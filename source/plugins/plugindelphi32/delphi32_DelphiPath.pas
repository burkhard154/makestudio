(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: DelPath.pas

The Initial Devoloper of the original DMAK-Code is:
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
2003/11/28  USchuster - 2nd Migrationstep (fixed header)
2003/12/05  USchuster - re-formatted
2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit delphi32_DelphiPath;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, ImgList, ComCtrls, ToolWin, StdCtrls, ExtCtrls, delphi32_Utils,
  JvBaseDlg, JvBrowseFolder, Menus, JvComponent, JvComponentBase, delphi32_vars,
  System.ImageList;

type
  TFormSearchPathDelphi = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    List: TListBox;
    ToolBar2: TToolBar;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ImageList1: TImageList;
    BrowseDir: TJvBrowseForFolderDialog;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;


function DlgSearchPathDelphi: Boolean;

implementation

{$R *.dfm}

function DlgSearchPathDelphi: Boolean;
begin
  with TFormSearchPathDelphi.Create(nil) do
  try
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

procedure TFormSearchPathDelphi.FormCreate(Sender: TObject);
var
  reg: TRegistry;
  S: string;
begin
  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;
  reg.OpenKey(GetDelphiRootKey + 'Library', False);
  try
    S := reg.ReadString( stdcSearchPath);

    //build Directorylist
    S := Trim(S);
    while Pos(';', S) > 0 do
    begin
      List.Items.Add(Copy(S, 1, Pos(';', S) - 1));
      Delete(S, 1, Pos(';', S));
    end;
    if Length(S) > 1 then
    begin
      List.Items.Add(S);
    end;
  except
  end;
  reg.Free;
end;

procedure TFormSearchPathDelphi.ToolButton9Click(Sender: TObject);
begin
  if List.ItemIndex >= 0 then
    BrowseDir.Directory := ExtractFilePath(List.Items[List.ItemIndex]);
  if BrowseDir.Execute then
  begin
    List.Items.Add(BrowseDir.Directory);
  end;
end;

procedure TFormSearchPathDelphi.ToolButton10Click(Sender: TObject);
begin
  if List.ItemIndex >= 0 then
  begin
    List.Items.Delete(List.ItemIndex);
  end;
end;

procedure TFormSearchPathDelphi.Button1Click(Sender: TObject);
var
  S: string;
  I: Integer;
  reg: TRegistry;
begin
  S := '';
  for I := 0 to List.Items.Count - 1 do
  begin
    S := S + StringReplace( StringReplace( List.Items[I], #13, '', [rfReplaceAll]), #10, '', [rfReplaceAll]);
    if I < List.Items.Count - 1 then
      S := S + ';';
  end;


  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;
  reg.OpenKey(GetDelphiRootKey + 'Library', False);
  try
    reg.WriteString(stdcSearchPath, S);
  except
  end;
  reg.Free;
end;

end.
