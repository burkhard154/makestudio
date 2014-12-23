(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: passolo_EditPassoloModule.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/19  JDuenow   - launched Passolo Module
2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit passolo_EditPassoloModule;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, passolo_Module,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Mask, JvExMask, JvToolEdit;

type
  TFormPassoloModuleEdit = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    edtProjectPath: TJvFilenameEdit;
  private
  
  public
    { Public-Deklarationen }
  end;

function DlgEditPassoloModule(M: TPassoloCommand): Boolean;


implementation

{$R *.dfm}

function DlgEditPassoloModule(M: TPassoloCommand): Boolean;
begin
  Result := False;
  with TFormPassoloModuleEdit.Create(nil) do
  try
    edtProjectPath.Text := M.ProjectPath;
    if ShowModal = mrOk then
    begin
      M.ProjectPath := edtProjectPath.Text;
      if FileExists(M.ProjectPath) then
        Result := True;
    end;
  finally
    Free;
  end;
end;

end.
