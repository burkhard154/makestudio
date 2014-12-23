(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ModuleMemo.pas

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

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to JVCSMAK
2003/11/28  USchuster - 2nd Migrationstep (fixed header)
2003/12/05  USchuster - re-formatted
2005/02/04  USchuster - typo in header and moved {$I jedi.inc} below unit
2005/04/09  BSchranz  - TMemo changed to TJvHlEditor, Translated to englisch

-----------------------------------------------------------------------------*)

unit msRunListEdit;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, ImgList, ExtCtrls, StdCtrls, JvExControls, JvComponent,
  JvEditorCommon, JvEditor, JvHLEditor, JvFindReplace, JvComponentBase;

type
  TFormMemo = class(TForm)
    Panel8: TPanel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    mnFind: TMenuItem;
    mnFindReplace: TMenuItem;
    Memo: TMemo;
    JvFindReplace: TJvFindReplace;
    procedure mnFindReplaceClick(Sender: TObject);
    procedure mnFindClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormMemo: TFormMemo;

implementation

{$R *.dfm}

procedure TFormMemo.mnFindClick(Sender: TObject);
begin
  JvFindReplace.Find;
end;

procedure TFormMemo.mnFindReplaceClick(Sender: TObject);
begin
  JvFindReplace.Replace;
end;

end.
