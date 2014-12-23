(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: delphi32_Properties.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2006/02/07  BSchranz  - New commands and integration of delphi 2006 - parts of the changes 
                        are nessecary because of JCL/JVCL compatibility between D2005 and D2006
2006/02/22  BSchranz  - Switched back to JVCL 3.0
                      - Delphi 2006 Support and Command "CheckDelphiVersion" added
2006/04/30  USchuster - D5 fix

-----------------------------------------------------------------------------*)

unit delphi32_Properties;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvExComCtrls, JvComCtrls, ExtCtrls,
  StdCtrls, JvExStdCtrls, JvCombobox, JvListComb, ImgList, Mask, JvExMask,
  JvToolEdit, JvComponent, JvComponentBase;

type
  TFormEditDelphi32Globals = class(TForm)
    JvTabDefaultPainter1: TJvTabDefaultPainter;
    ImageList1: TImageList;
    Panel1: TPanel;
    JvPageControl1: TJvPageControl;
    tabCommon: TTabSheet;
    GroupBox1: TGroupBox;
    cbVer: TJvImageComboBox;
    d5: TCheckBox;
    d6: TCheckBox;
    d7: TCheckBox;
    d2005: TCheckBox;
    d2006: TCheckBox;
    tabD5: TTabSheet;
    tabD7: TTabSheet;
    Label2: TLabel;
    tabD2005: TTabSheet;
    tabD2006: TTabSheet;
    Edit1: TEdit;
    Button1: TButton;
    Panel2: TPanel;
    GroupBox2: TGroupBox;
    Panel3: TPanel;
    btOk: TButton;
    btCancel: TButton;
    Panel4: TPanel;
    Memo: TMemo;
    Label1: TLabel;
    Edit2: TEdit;
    Button2: TButton;
    Panel5: TPanel;
    GroupBox3: TGroupBox;
    Panel6: TPanel;
    Memo1: TMemo;
    Label3: TLabel;
    Edit3: TEdit;
    Button3: TButton;
    Panel7: TPanel;
    GroupBox4: TGroupBox;
    Panel8: TPanel;
    Memo2: TMemo;
    Label4: TLabel;
    Edit4: TEdit;
    Button4: TButton;
    Panel9: TPanel;
    GroupBox5: TGroupBox;
    Panel10: TPanel;
    Memo3: TMemo;
    Label5: TLabel;
    JvDirectoryEdit1: TJvDirectoryEdit;
    Label6: TLabel;
    JvDirectoryEdit2: TJvDirectoryEdit;
    Label7: TLabel;
    JvDirectoryEdit3: TJvDirectoryEdit;
    Label8: TLabel;
    JvDirectoryEdit4: TJvDirectoryEdit;
    Label9: TLabel;
    JvDirectoryEdit5: TJvDirectoryEdit;
    Label10: TLabel;
    JvDirectoryEdit6: TJvDirectoryEdit;
    tabD2007: TTabSheet;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormEditDelphi32Globals: TFormEditDelphi32Globals;

implementation

{$R *.dfm}

end.
