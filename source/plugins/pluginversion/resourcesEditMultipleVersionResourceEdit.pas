unit resourcesEditMultipleVersionResourceEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Mask, JvExMask, JvToolEdit;

type
  TFormEditEditMultipleVersionResourceParams = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    edFilename: TJvFilenameEdit;
    rgType: TRadioGroup;
    GroupBox1: TGroupBox;
    edVersionKey: TEdit;
    rgIncSet: TRadioGroup;
    grpSet: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edtMainVer: TEdit;
    edtMinorVer: TEdit;
    edtRel: TEdit;
    edtBuild: TEdit;
    updoMain: TUpDown;
    updoMinor: TUpDown;
    updoRel: TUpDown;
    updoBuild: TUpDown;
    btAdd: TButton;
    btRemove: TButton;
    cbRecursive: TCheckBox;
    mmFiles: TMemo;
    procedure btAddClick(Sender: TObject);
    procedure rgIncSetClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormEditEditMultipleVersionResourceParams: TFormEditEditMultipleVersionResourceParams;

implementation

{$R *.dfm}

procedure TFormEditEditMultipleVersionResourceParams.btAddClick(
  Sender: TObject);
begin
  if Trim(edFilename.Text) <> '' then
  begin
    mmFiles.Lines.Add(edFilename.Text);
    edFilename.Clear;
  end;
end;

procedure TFormEditEditMultipleVersionResourceParams.rgIncSetClick(
  Sender: TObject);
begin
  grpSet.Enabled := rgIncSet.ItemIndex = 0;
  edtMainVer.Enabled := grpSet.Enabled;
  edtMinorVer.Enabled := grpSet.Enabled;
  edtRel.Enabled := grpSet.Enabled;
  edtBuild.Enabled := grpSet.Enabled;

  rgType.Enabled := rgIncSet.ItemIndex = 1;
end;

procedure TFormEditEditMultipleVersionResourceParams.btRemoveClick(
  Sender: TObject);
begin
  mmFiles.Lines.Delete(mmFiles.CaretPos.Y);
end;

end.
