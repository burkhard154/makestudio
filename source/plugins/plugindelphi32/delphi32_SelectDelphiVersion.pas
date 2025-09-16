(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: delphi32_SelectDelphiVersion.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit delphi32_SelectDelphiVersion;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, JvListComb, delphi32_Vars, delphi32_Utils,
  JvExStdCtrls, JvCombobox, System.ImageList;

type
  TFormSelectDelphiVersion = class(TForm)
    GroupBox1: TGroupBox;
    cbVer: TJvImageComboBox;
    Button1: TButton;
    ImageList1: TImageList;
    Button2: TButton;
    d5: TCheckBox;
    d6: TCheckBox;
    d7: TCheckBox;
    d2005: TCheckBox;
    d2006: TCheckBox;
    d2007: TCheckBox;
    d2009: TCheckBox;
    d2010: TCheckBox;
    dXE: TCheckBox;
    dXE2: TCheckBox;
    dxe3: TCheckBox;
    dxe4: TCheckBox;
    dxe5: TCheckBox;
    dxe6: TCheckBox;
    dxe7: TCheckBox;
    dxe8: TCheckBox;
    dD10S: TCheckBox;
    dD101B: TCheckBox;
    dD102T: TCheckBox;
    dD103R: TCheckBox;
    dD104S: TCheckBox;
    dD11A: TCheckBox;
    dD12A: TCheckBox;
    dD13: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgSelectDelphiVersion(var dVersion: TDelphiVersion): Boolean;

implementation

{$R *.dfm}

function DlgSelectDelphiVersion(var dVersion: TDelphiVersion): Boolean;
begin
  Result := False;
  with TFormSelectDelphiVersion.Create(nil) do
  try
    case dVersion of
      dver5: cbVer.ItemIndex := 0;
      dver6: cbVer.ItemIndex := 1;
      dver7: cbVer.ItemIndex := 2;
      dver2005: cbVer.ItemIndex := 3;
      dver2006: cbVer.ItemIndex := 4;
      dver2007: cbVer.ItemIndex := 5;
      dver2009: cbVer.ItemIndex := 6;
      dver2010: cbVer.ItemIndex := 7;
      dverXE: cbVer.ItemIndex := 8;
      dverXE2: cbVer.ItemIndex := 9;
      dverXE3: cbVer.ItemIndex := 10;
      dverXE4: cbVer.ItemIndex := 11;
      dverXE5: cbVer.ItemIndex := 12;
      dverXE6: cbVer.ItemIndex := 13;
      dverXE7: cbVer.ItemIndex := 14;
      dverXE8: cbVer.ItemIndex := 15;
      dverD10S: cbVer.ItemIndex := 16;
      dverD101B: cbVer.ItemIndex := 17;
      dverD102T: cbVer.ItemIndex := 18;
      dverD103R: cbVer.ItemIndex := 19;
      dverD104S: cbVer.ItemIndex := 20;
      dverD11A: cbVer.ItemIndex := 21;
      dverD12A: cbVer.ItemIndex := 22;
      dverD13: cbVer.ItemIndex := 23;
    end;
    if ShowModal = mrOk then
    begin
      case cbVer.ItemIndex of
        0: dVersion := dver5;
        1: dVersion := dver6;
        2: dVersion := dver7;
        3: dVersion := dver2005;
        4: dVersion := dver2006;
        5: dVersion := dver2007;
        6: dVersion := dver2009;
        7: dVersion := dver2010;
        8: dVersion := dverXE;
        9: dVersion := dverXE2;
        10: dVersion := dverXE3;
        11: dVersion := dverXE4;
        12: dVersion := dverXE5;
        13: dVersion := dverXE6;
        14: dVersion := dverXE7;
        15: dVersion := dverXE8;
        16: dVersion := dverD10S;
        17: dVersion := dverD101B;
        18: dVersion := dverD102T;
        19: dVersion := dverD103R;
        20: dVersion := dverD104S;
        21: dVersion := dverD11A;
        22: dVersion := dverD12A;
        23: dVersion := dverD13;
        else
          dVersion := dver5;
      end;
      Result := cbVer.ItemIndex > -1;
    end;
  finally
    Free;
  end;
end;

procedure TFormSelectDelphiVersion.FormCreate(Sender: TObject);
begin
  d5.Checked := CheckDelphiVersion(dver5);
  d6.Checked := CheckDelphiVersion(dver6);
  d7.Checked := CheckDelphiVersion(dver7);
  d2005.Checked := CheckDelphiVersion(dver2005);
  d2006.Checked := CheckDelphiVersion(dver2006);
  d2007.Checked := CheckDelphiVersion(dver2007);
  d2009.Checked := CheckDelphiVersion(dver2009);
  d2010.Checked := CheckDelphiVersion(dver2010);
  dXE.Checked := CheckDelphiVersion(dverXE);
  dXE2.Checked := CheckDelphiVersion(dverXE2);
  dXE3.Checked := CheckDelphiVersion(dverXE3);
  dXE4.Checked := CheckDelphiVersion(dverXE4);
  dXE5.Checked := CheckDelphiVersion(dverXE5);
  dXE6.Checked := CheckDelphiVersion(dverXE6);
  dXE7.Checked := CheckDelphiVersion(dverXE7);
  dXE8.Checked := CheckDelphiVersion(dverXE8);
  dD10S.Checked := CheckDelphiVersion(dverD10S);
  dD101B.Checked := CheckDelphiVersion(dverD101B);
  dD102T.Checked := CheckDelphiVersion(dverD102T);
  dD103R.Checked := CheckDelphiVersion(dverD103R);
  dD104S.Checked := CheckDelphiVersion(dverD104S);
  dD11A.Checked :=  CheckDelphiVersion(dverD11A);
  dD12A.Checked :=  CheckDelphiVersion(dverD12A);
  dD13.Checked :=  CheckDelphiVersion(dverD13);
end;

end.
