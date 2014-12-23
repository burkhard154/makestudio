(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msEditBatchModule.pas

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

unit utils_EditBatchModule;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvToolEdit, utils_RunBatch, JvExMask, ExtCtrls;

type
  TFormEditBatchModule = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edFilename: TJvFilenameEdit;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgEditBatchModule(M: TBatchModule): Boolean;

implementation

{$R *.dfm}

function DlgEditBatchModule(M: TBatchModule): Boolean;
begin
  Result := False;
  with TFormEditBatchModule.Create(nil) do
  try
    edFilename.FileName := M.BatchFile;
    if ShowModal = mrOk then
    begin
      Result := True;
      M.BatchFile := edFilename.FileName;
    end;
  finally
    Free;
  end;
end;

end.
