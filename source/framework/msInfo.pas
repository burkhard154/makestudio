(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: info.pas

The Initial Developer of the original DMAK-Code is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
Code move to JEDI VCS:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to MakeStudio
2003/11/28  USchuster - 2nd Migrationstep (fixed header)
2003/12/05  USchuster - re-formatted
2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/03/06  USchuster - changed to use TMakeStudioPlugin.VersionInfo instead
                        of retrieving the plugin version info here
2005/04/09  BSchranz  - Translated to englisch
-----------------------------------------------------------------------------*)

unit msInfo;

{$I jedi.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
  ExtCtrls, JclFileUtils, JvComponent, JvBaseDlg, JvJVCLAboutForm, Vcl.Dialogs;

type
  TAboutBox = class(TForm)
    OKButton: TButton;
    Panel1: TPanel;
    Image1: TImage;
    GroupBox1: TGroupBox;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbDatetime: TLabel;
    GroupBox2: TGroupBox;
    Panel3: TPanel;
    memoCredits: TMemo;
    Label7: TLabel;
    lbProductname: TLabel;
    lbFileVersion: TLabel;
    lbCompany: TLabel;
    lbCopyright: TLabel;
    lbDescription: TLabel;
    GroupBox3: TGroupBox;
    Panel4: TPanel;
    lbPlugins: TListBox;
    GroupBox4: TGroupBox;
    Panel5: TPanel;
    memoAdditionalInfo: TMemo;
    JvJVCLAboutComponent1: TJvJVCLAboutComponent;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.dfm}

uses
  msGlobals, msPluginHandler;

procedure TAboutBox.FormCreate(Sender: TObject);
var
  verinfo: TJclFileVersionInfo;
begin
  verinfo := TJclFileVersionInfo.Create(Application.ExeName);
  try
    lbProductname.Caption := verinfo.ProductName;
    lbFileVersion.Caption := verinfo.FileVersion;
    lbCompany.Caption := verinfo.CompanyName;
    lbCopyright.Caption := verinfo.LegalCopyright;
    lbDescription.Caption := verinfo.FileDescription;
    Label7.Caption := Application.Title;

    lbDatetime.Caption := DateTimeToStr(FileDateToDateTime(FileAge(Application.ExeName)));
  finally
    verinfo.Free;
  end;
end;

procedure TAboutBox.FormShow(Sender: TObject);
var
  I: Integer;
begin
  //Credits
  memoCredits.Lines.Clear;
  memoCredits.Lines.AddStrings(PluginHandler.Credits);

  //Additional Infos
  memoAdditionalInfo.Lines.Clear;
  memoAdditionalInfo.Lines.AddStrings(PluginHandler.AdditionalInfo);

  //Plugins
  for I := 0 to PluginHandler.Plugins.Count - 1 do
    lbPlugins.Items.Add(Format('%s (%s)', [PluginHandler.Plugins.Items[I].GetName,
      PluginHandler.Plugins.Items[I].VersionInfo]));;
end;

procedure TAboutBox.Button1Click(Sender: TObject);
begin
  JvJVCLAboutComponent1.Execute;
end;

end.

