(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: helpandmanual_EditHelpandmanualModule.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/01  JDuenow   - launched Help & Manual Module
2005/06/20  USchuster - D5 fix
2005/09/07  BSchranz  - translated to english, Batchfile changed to ExecCmdLine...
                        HM3 and HM4 supported


-----------------------------------------------------------------------------*)

unit helpandmanual_EditHelpandmanualModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvToolEdit, Mask, JvExMask, ExtCtrls, ImgList,
  JvExStdCtrls, JvCombobox, JvListComb, JclFileUtils, helpandmanual_Module,

  helpandmanual_Vars;

type
  TFormHelpandmanualModuleEdit = class(TForm)
    Panel3: TPanel;
    Label1: TLabel;
    edtInput: TJvFilenameEdit;
    Label2: TLabel;
    edtOutput: TJvDirectoryEdit;
    Label3: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    cmbOutput: TJvImageComboBox;
    ImageList1: TImageList;
    btnSelectCompiler: TButton;
    OpenDialog1: TOpenDialog;
    procedure cmbOutputChange(Sender: TObject);
    procedure edtInputAfterDialog(Sender: TObject; var Name: String;
      var Action: Boolean);
    procedure edtOutputAfterDialog(Sender: TObject; var Name: String;
      var Action: Boolean);
    procedure btnSelectCompilerClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormHelpandmanualModuleEdit: TFormHelpandmanualModuleEdit;

function DlgEditHelpandManualModule(M: THelpandmanualCommand): Boolean;

implementation

uses
  registry;

{$R *.dfm}

function DlgEditHelpandmanualModule(M: THelpandmanualCommand): Boolean;
begin
  Result := False;
  with TFormHelpandmanualModuleEdit.Create(nil) do
  try
    edtInput.Text := M.ProjectPath;
    edtOutput.Text := M.Outputfile;
    cmbOutput.ItemIndex := M.OutputOpt;
    if ShowModal = mrOk then
    begin
      M.ProjectPath := edtInput.Text;
      M.Outputfile := edtOutput.Text;
      M.OutputOpt := cmbOutput.ItemIndex;
      if FileExists(M.ProjectPath) then
        Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TFormHelpandmanualModuleEdit.btnSelectCompilerClick(Sender: TObject);
var
  regKey, sVal : string;
  reg : TRegistry;
begin
  regKey := jvcsmak.ApplicationRegKey+'\plugins\help_manual';
  reg    := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(regKey, true) then
    begin
      sVal := reg.ReadString(regKey);

      if OpenDialog1.Execute(self.Handle) then
      begin
        reg.WriteString('CompilerPath', OpenDialog1.FileName);
      end;
    end;

    reg.CloseKey;
  finally
    reg.Free;
  end;
end;

procedure TFormHelpandmanualModuleEdit.cmbOutputChange(Sender: TObject);
begin
  case cmbOutput.ItemIndex of
    0: edtOutput.Text := ChangeFileExt(edtOutput.Text, '.hlp');
    1: edtOutput.Text := ChangeFileExt(edtOutput.Text, '.chm');
    2: edtOutput.Text := ChangeFileExt(edtOutput.Text, '.htm');
    3: edtOutput.Text := ChangeFileExt(edtOutput.Text, '.pdf');
    4: edtOutput.Text := ChangeFileExt(edtOutput.Text, '.rtf');
    5: edtOutput.Text := ChangeFileExt(edtOutput.Text, '.exe');
  end;
end;

procedure TFormHelpandmanualModuleEdit.edtInputAfterDialog(Sender: TObject;
  var Name: String; var Action: Boolean);
begin
  if (Length(Name) > 0) and (Length(edtOutput.Text) = 0) then begin
    edtOutput.Text := Name;
    cmbOutputChange(self);
    edtOutput.InitialDir := ExtractFilePath(edtInput.Text);
  end;
end;

procedure TFormHelpandmanualModuleEdit.edtOutputAfterDialog(
  Sender: TObject; var Name: String; var Action: Boolean);
begin
  edtOutput.Text := PathAddSeparator(Name);
end;

end.
