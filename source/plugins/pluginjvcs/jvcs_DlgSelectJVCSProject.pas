(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: DlgSelectJVCSProject.pas

The Initial Devoloper of the original DMAK-Code is:
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
2003/11/28  USchuster - 2nd Migrationstep (fixed header, removed Variants, D5 Fix)
2003/12/05  USchuster - re-formatted
2005/01/05  BSchranz  - Migration to plugin code
2005/02/04  USchuster - preparations for check in
2005/08/12  BSchranz  - New Version - based on jvcs.exe
2005/08/16  USchuster - D5/D6 fix

-----------------------------------------------------------------------------*)

unit jvcs_DlgSelectJVCSProject;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, ExtCtrls, jvcs_Module, jvcs_utils;

type
  TFormSelectJVCSProject = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ImageList1: TImageList;
    lvProjects: TListView;
    btOk: TButton;
    btCancel: TButton;
    procedure btOkClick(Sender: TObject);
  private
    SelectedProjects: TStringList;
    FUser,
    FHost,
    FPassword : String;
    FPort : Integer;
    procedure FillList;
  public
    { Public-Deklarationen }
  end;


function SelectJVCSProjects( User, Host, Password:String; Port:Integer; Projects:TStringList): Boolean;

implementation

{$R *.dfm}

function SelectJVCSProjects( User, Host, Password:String; Port:Integer; Projects:TStringList): Boolean;
begin
  with TFormSelectJVCSProject.Create(nil) do
  try
    SelectedProjects := Projects;
    FUser := User;
    FHost := Host;
    FPassword := Password;
    FPort := Port;
    FillList;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

procedure TFormSelectJVCSProject.FillList;
var
  sl: TStringList;
  I, idx: Integer;
  Helper : TJVCSHelper;
begin
  sl := TStringList.Create;
  Helper := TJVCSHelper.Create;
  try
    Helper.GetProjectList( FUser, FHost, FPassword, FPort,  sl);

    sl.Sorted := True;
    sl.Sort;
    SelectedProjects.Sorted := True;
    SelectedProjects.Sort;
    {$IFDEF DELPHI6_UP}
//USc 28.11.2003 this property doesn't exist in Delphi 5 and TStringList.IndexOf
// or Find seams to be caseinsensitive
    SelectedProjects.CaseSensitive := False;
    {$ENDIF DELPHI6_UP}

    for I := 0 to sl.Count - 1 do
      with lvProjects.Items.Add do
      begin
        Caption := sl.Names[I];
        SubItems.Add(sl.Values[sl.Names[I]]);
        ImageIndex := 0;
        StateIndex := 0;
        Checked := SelectedProjects.Find(sl.Names[I], idx);
      end;
  finally
    sl.Free;
    Helper.Free;
  end;
end;

procedure TFormSelectJVCSProject.btOkClick(Sender: TObject);
var
  I: Integer;
begin
  SelectedProjects.Clear;
  for I := 0 to lvProjects.Items.Count - 1 do
  begin
    if lvProjects.Items[I].Checked then
      SelectedProjects.Add(lvProjects.Items[I].Caption);
  end;

  Close;
  ModalResult := mrOk;
end;

end.
