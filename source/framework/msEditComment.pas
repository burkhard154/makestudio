(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msEditSaveLog.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/08/12  BSchranz  - command savelog added
2005/08/16  USchuster - D5 fix
2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
2005/04/09  BSchranz  - Translated to englisch

-----------------------------------------------------------------------------*)
unit msEditComment;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, msProgram,
  ShellApi;

type
  TFormEditComment = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    Label3: TLabel;
    edComment: TEdit;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgEditComment(M: TCOMMENTBlock): Boolean;

implementation

{$R *.dfm}

function DlgEditComment(M: TCOMMENTBlock): Boolean;
begin
  with TFormEditComment.Create(nil) do
  try
    Result := False;
    edComment.Text := M.Comment;
    if ShowModal = mrOk then
    begin
      Result := True;
      M.Comment := edComment.Text;
    end;
  finally
    Free;
  end;
end;

end.
