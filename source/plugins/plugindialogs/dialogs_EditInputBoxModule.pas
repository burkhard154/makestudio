(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dialogs_EditInputBoxModule.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/04  JDuenow   - launched Dialogs Module
2005/06/20  USchuster - D5 fix

-----------------------------------------------------------------------------*)

unit dialogs_EditInputBoxModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExControls, JvComponent, JvSpeedButton, ExtCtrls, dialogs_Vars,
  dialogs_tools, dialogs_InputBox, dialogs_dlgInput;

type
  TFormInputBoxModuleEdit = class(TForm)
    Panel3: TPanel;
    Label2: TLabel;
    edtText: TMemo;
    Label8: TLabel;
    cmbVarList: TComboBox;
    btnInsertTextVar: TJvSpeedButton;
    Panel1: TPanel;
    Label5: TLabel;
    cmbReturnInput: TComboBox;
    btnBoxTest: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    edtCaption: TEdit;
    edtDefault: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    btnInsertDefaultVar: TJvSpeedButton;
    procedure btnInsertTextVarClick(Sender: TObject);
    procedure btnInsertDefaultVarClick(Sender: TObject);
    procedure btnBoxTestClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure ReadVars;
    procedure ReadValues(M: TInputBoxCommand);
    procedure WriteValues(M: TInputBoxCommand);
  end;

var
  FormInputBoxModuleEdit: TFormInputBoxModuleEdit;

function DlgEditInputBoxModule(M: TInputBoxCommand): Boolean;

implementation

uses
  JclStrings;

{$R *.dfm}

function DlgEditInputBoxModule(M: TInputBoxCommand): Boolean;
begin
  Result := False;
  with TFormInputBoxModuleEdit.Create(nil) do
  try
    ReadVars;
    ReadValues(M);
    if ShowModal = mrOk then
    begin
      WriteValues(M);
      Result := True;
    end;
  finally
    Free;
  end;
end;

{ TFormInputBoxModuleEdit }

procedure TFormInputBoxModuleEdit.ReadValues(M: TInputBoxCommand);
begin
  edtCaption.Text := M.DCaption;

  StrToStrings(M.Text, '|', edtText.Lines);

  edtDefault.Text := M.Default;

  if cmbReturnInput.Items.IndexOf(M.ReturnValue) = -1 then
    cmbReturnInput.Items.Add(M.ReturnValue);
  cmbReturnInput.ItemIndex := cmbReturnInput.Items.IndexOf(M.ReturnValue);
end;

procedure TFormInputBoxModuleEdit.WriteValues(M: TInputBoxCommand);
begin
  M.DCaption := edtCaption.Text;

  M.Text := StringsToStr(edtText.Lines, '|');

  M.Default := edtDefault.Text;

  if (cmbReturnInput.ItemIndex = -1) and (Length(M.ReturnValue) > 0) then
    M.ReturnValue := '';
  if Length(cmbReturnInput.Text) > 0 then
    M.ReturnValue := cmbReturnInput.Text;
end;

procedure TFormInputBoxModuleEdit.ReadVars;
var
  I: Integer;
begin
  for I := 0 to jvcsmak.Variables.Count - 1 do
  begin
    cmbReturnInput.Items.Add(jvcsmak.Variables.Names[I]);
    cmbVarList.Items.Add(jvcsmak.Variables.Names[I]);
  end;
end;

procedure TFormInputBoxModuleEdit.btnInsertTextVarClick(Sender: TObject);
begin
  if cmbVarList.Text<>'' then
    edtText.Text := edtText.Text + '%' + cmbVarList.Text + '%';
end;

procedure TFormInputBoxModuleEdit.btnInsertDefaultVarClick(
  Sender: TObject);
begin
  if cmbVarList.Text<>'' then
    edtDefault.Text := edtDefault.Text + '%' + cmbVarList.Text + '%';
end;

procedure TFormInputBoxModuleEdit.btnBoxTestClick(Sender: TObject);
var
  return: string;
begin
  return := DlgInputBox(edtCaption.Text, FormatText(edtText.Text), FormatDefault(edtDefault.Text));
//  return := InputBox(edtCaption.Text, FormatText(edtText.Text), FormatDefault(edtDefault.Text));

  if Length(cmbReturnInput.Text) > 0 then
    ShowMessage(stdReturnValueInput + ':' + #10#13 + cmbReturnInput.Text + ' = ' + return)
  else
    ShowMessage(stdReturnValueInput + ' = ' + return);
end;

end.
