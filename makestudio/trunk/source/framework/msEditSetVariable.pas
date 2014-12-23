(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msEditSetVariable.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/20  BSchranz  - Migration to JVCSMak with external plugins
2005/02/04  USchuster - preparations for check in
2005/08/25  BSchranz - Added operation type for "SetVariable"
2005/04/09  BSchranz  - Translated to englisch

-----------------------------------------------------------------------------*)

unit msEditSetVariable;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, msGlobals, msInternalCommands, ExtCtrls,
  ComCtrls, Mask, JvExMask, JvToolEdit;

type
  TFormEditSetVariable = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    PageControl1: TPageControl;
    tbCommon: TTabSheet;
    tbDateTime: TTabSheet;
    Label6: TLabel;
    edDateTimeFormat: TEdit;
    Label7: TLabel;
    Label4: TLabel;
    cbVars: TComboBox;
    Label1: TLabel;
    lbContent: TLabel;
    Label3: TLabel;
    edValue: TEdit;
    cbAppend: TCheckBox;
    Label2: TLabel;
    cbOperation: TComboBox;
    tbVersionInfo: TTabSheet;
    tbDataType: TTabSheet;
    rgDataType: TRadioGroup;
    cbVersionFormat: TComboBox;
    Label5: TLabel;
    Label8: TLabel;
    edVersionFilename: TJvFilenameEdit;
    Label9: TLabel;
    cbReplaceVars: TCheckBox;
    procedure cbOperationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbVarsChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgEditSetVariable(M: TSetVariable): Boolean;

implementation

{$R *.dfm}

function DlgEditSetVariable(M: TSetVariable): Boolean;
begin
  with TFormEditSetVariable.Create(nil) do
  try
    Result := False;
    cbVars.Text := M.Varname;
    rgDataType.ItemIndex := M.DataType;
    edValue.Text := M.VarValue;
    cbOperation.ItemIndex := Ord( M.Operation);
    cbAppend.Checked := M.Append;
    edDateTimeFormat.Text := M.DateTimeFormat;
    cbVersionFormat.Text := M.VersionFormat;
    edVersionFilename.FileName := M.VersionFilename;
    cbReplaceVars.Checked := M.ReplaceVars;
    cbVarsChange( nil);
    cbOperationChange( nil);
    if ShowModal = mrOk then
    begin
      Result := True;
      M.Varname := cbVars.Text;
      M.DataType := rgDataType.ItemIndex;
      M.VarValue := edValue.Text;
      M.Operation := TSetVariableOperation( cbOperation.ItemIndex);
      M.Append := cbAppend.Checked;
      M.DateTimeFormat := edDateTimeFormat.Text;
      M.VersionFormat := cbVersionFormat.Text;
      M.VersionFilename := edVersionFilename.FileName;
      M.ReplaceVars := cbReplaceVars.Checked;
    end;
  finally
    Free;
  end;
end;

procedure TFormEditSetVariable.FormCreate(Sender: TObject);
begin
  Varhandler.GetVarList(cbVars.Items);
end;

procedure TFormEditSetVariable.cbVarsChange(Sender: TObject);
begin
  if Varhandler.VarExists(cbVars.Text) then
    lbContent.Caption := Varhandler.FindXVar(cbVars.Text).AsString
  else
    lbContent.Caption := '';
end;

procedure TFormEditSetVariable.cbOperationChange(Sender: TObject);
begin
  case cbOperation.ItemIndex of
  {keine
Inkrement
Dekrement
Mit Backslash abschlieﬂen
Abschlieﬂenden Backslash entfernen
Datum, Uhrzeit
Versionsinformation lesen
}
    0: begin
         tbDateTime.TabVisible := false;
         tbVersionInfo.TabVisible := false;
         tbDataType.TabVisible := true;
         rgDataType.ItemIndex := 0;
         cbAppend.Enabled := true;
         edValue.Enabled := true;
       end;
    1,2: begin
         tbDateTime.TabVisible := false;
         tbVersionInfo.TabVisible := false;
         tbDataType.TabVisible := false;
         rgDataType.ItemIndex := 2;
         cbAppend.Checked := false;
         cbAppend.Enabled := false;
         edValue.Enabled := false;
         edValue.Text := '';
       end;
    3,4: begin
         tbDateTime.TabVisible := false;
         tbVersionInfo.TabVisible := false;
         tbDataType.TabVisible := false;
         rgDataType.ItemIndex := 0;
         cbAppend.Checked := false;
         cbAppend.Enabled := false;
         edValue.Enabled := false;
         edValue.Text := '';
       end;
    5: begin
         tbDateTime.TabVisible := true;
         tbVersionInfo.TabVisible := false;
         tbDataType.TabVisible := false;
         rgDataType.ItemIndex := 0;
         cbAppend.Enabled := true;
         edValue.Enabled := false;
         edValue.Text := '';
       end;
    6: begin
         tbDateTime.TabVisible := false;
         tbVersionInfo.TabVisible := true;
         tbDataType.TabVisible := false;
         rgDataType.ItemIndex := 0;
         cbAppend.Enabled := true;
         edValue.Enabled := false;
         edValue.Text := '';
       end;
  end;
end;

end.


