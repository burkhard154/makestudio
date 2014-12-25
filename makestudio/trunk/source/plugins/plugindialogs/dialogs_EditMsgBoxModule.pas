(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dialogs_EditMsgBoxModule.pas

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
2006/02/25  BSchranz - MessageBoxButtons Bug Fix

-----------------------------------------------------------------------------*)

unit dialogs_EditMsgBoxModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, JvExStdCtrls, JvCombobox, JvListComb,
  JvExControls, JvComponent, JvSpeedButton, ExtCtrls, dialogs_MsgBox, Mask,
  JvExMask, JvToolEdit, dialogs_Vars, dialogs_tools, TypInfo;

type
  TFormMsgBoxModuleEdit = class(TForm)
    cmbSymbols: TJvImageComboBox;
    ImageList1: TImageList;
    cmbReturnValue: TComboBox;
    btnOk: TButton;
    btnCancel: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel3: TPanel;
    lbReturnValues: TLabel;
    Label8: TLabel;
    cmbVarList: TComboBox;
    btnInsertVar: TJvSpeedButton;
    Panel1: TPanel;
    btnBoxTest: TButton;
    cmbButtons: TJvCheckedComboBox;
    edtText: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertVarClick(Sender: TObject);
    procedure btnBoxTestClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure ReadVars;
    procedure ReadValues(M: TMsgBoxCommand);
    procedure WriteValues(M: TMsgBoxCommand);
  end;

var
  FormMsgBoxModuleEdit: TFormMsgBoxModuleEdit;

function DlgEditMsgBoxModule(M: TMsgBoxCommand): Boolean;

implementation

uses
  JclStrings;

{$R *.dfm}

function DlgEditMsgBoxModule(M: TMsgBoxCommand): Boolean;
begin
  Result := False;
  with TFormMsgBoxModuleEdit.Create(nil) do
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

procedure TFormMsgBoxModuleEdit.btnInsertVarClick(Sender: TObject);
begin
  if cmbVarList.Text<>'' then
    edtText.Text := edtText.Text + '%' + cmbVarList.Text + '%';
end;

procedure TFormMsgBoxModuleEdit.btnBoxTestClick(Sender: TObject);
var
  return: Integer;
begin
  return := MessageDlg(FormatText(edtText.Text), DialogTypes[cmbSymbols.ItemIndex], BStringToBType(cmbButtons.EditText), 0);

  if Length(cmbReturnValue.Text) > 0 then
    ShowMessage(stdReturnValueString + ':' + #10#13 + cmbReturnValue.Text + ' = ' + ButtonReturnValues[return-1])
  else
    ShowMessage(stdReturnValueString + ' = ' + ButtonReturnValues[return-1]);
end;

procedure TFormMsgBoxModuleEdit.ReadVars;
var
  I: Integer;
begin
  for I := 0 to MakeStudio.Variables.Count - 1 do
  begin
    cmbReturnValue.Items.Add(MakeStudio.Variables.Names[I]);
    cmbVarList.Items.Add(MakeStudio.Variables.Names[I]);
  end;

  lbReturnValues.Hint := stdReturnValuesString + #10#13;
  for I := 0 to cmbButtons.Items.Count - 1 do
  begin
    lbReturnValues.Hint := lbReturnValues.Hint + cmbButtons.Items.Strings[I];
    if I <> cmbButtons.Items.Count - 1 then
      lbReturnValues.Hint := lbReturnValues.Hint + #10#13;
  end;
end;

procedure TFormMsgBoxModuleEdit.ReadValues(M: TMsgBoxCommand);
var
  I: Integer;
begin
  StrToStrings(M.Text, '|', edtText.Lines);

  for I := 0 to cmbButtons.Items.Count - 1 do
  begin
    if TMsgDlgBtn(i) in M.Buttons then
      cmbButtons.Checked[I] := True;
  end;
  if M.Buttons = [] then
    cmbButtons.Checked[0] := True;

  cmbSymbols.ItemIndex := M.Style;
  
  if cmbReturnValue.Items.IndexOf(M.ReturnValue) = -1 then
    cmbReturnValue.Items.Add(M.ReturnValue);
  cmbReturnValue.ItemIndex := cmbReturnValue.Items.IndexOf(M.ReturnValue);
end;

procedure TFormMsgBoxModuleEdit.WriteValues(M: TMsgBoxCommand);
var I:Integer;
    s:TMsgDlgButtons;
begin
  M.Text := StringsToStr(edtText.Lines, '|');

  s := [];
  for I := 0 to cmbButtons.Items.Count - 1 do
  begin
    if cmbButtons.Checked[I] then
      Include( s, TMsgDlgBtn(i));
  end;
  if s = [] then
    s := [mbOk];

  M.Buttons := s;

  M.Style := cmbSymbols.ItemIndex;

  if (cmbReturnValue.ItemIndex = -1) and (Length(M.ReturnValue) > 0) then
    M.ReturnValue := '';
  if Length(cmbReturnValue.Text) > 0 then
    M.ReturnValue := cmbReturnValue.Text;
end;

procedure TFormMsgBoxModuleEdit.FormCreate(Sender: TObject);
var i:TMsgDlgBtn;
begin
  cmbButtons.Items.Clear;
  for i:=Low(TMsgDlgBtn) to High(TMsgDlgBtn) do begin
    cmbButtons.Items.Add( GetEnumName( TypeInfo( TMsgDlgBtn), ord(i)));
  end;
end;

end.
