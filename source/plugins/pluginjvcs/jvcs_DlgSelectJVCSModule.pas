(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcs_DlgSelectJVCSModule.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/08/19  BSchranz  - check in and check out implemented
2006/04/30  USchuster - D5 fix

-----------------------------------------------------------------------------*)

unit jvcs_DlgSelectJVCSModule;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvExExtCtrls, JvNetscapeSplitter, ImgList, StdCtrls,
  ComCtrls, JclStrings, jvcs_utils;

type
  TFormSelectJVCSModule = class(TForm)
    Panel1: TPanel;
    lvModules: TListView;
    Panel2: TPanel;
    btOk: TButton;
    btCancel: TButton;
    ImageList1: TImageList;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    lvProjects: TListView;
    procedure lvProjectsClick(Sender: TObject);
  private
    FUser,
    FHost,
    FPassword: string;
    FPort: Integer;
    procedure FillProjectList;
    procedure FillModuleList(Project: string);
  public
    { Public-Deklarationen }
  end;

var
  FormSelectJVCSModule: TFormSelectJVCSModule;

function SelectJVCSModules(User, Host, Password: string; Port: Integer; Modules: TStringList): Boolean;

implementation

{$R *.dfm}

function SelectJVCSModules(User, Host, Password: string; Port: Integer; Modules: TStringList): Boolean;
var
  I: Integer;
begin
  Result := False;
  with TFormSelectJVCSModule.Create(nil) do
  try
    FUser := User;
    FHost := Host;
    FPassword := Password;
    FPort := Port;
    FillProjectList;
    Modules.Clear;
    if ShowModal = mrOk then begin
      if lvProjects.Selected <> nil then
        if lvModules.Items.Count > 0 then begin
          for I := 0 to lvModules.Items.Count - 1 do
            if lvModules.Items[I].Selected then
              Modules.Add(lvProjects.Selected.Caption + ';' +
                          lvModules.Items[I].Caption + ';' +
                          IntToStr(Integer(lvModules.Items[I].Data)));
          Result := True;
        end;
    end;
  finally
    Free;
  end;
end;

{ TFormSelectJVCSModule }

procedure TFormSelectJVCSModule.FillModuleList(Project: string);
var
  sl, sl1: TStringList;
  I: Integer;
  Helper: TJVCSHelper;
begin
  sl := TStringList.Create;
  sl1 := TStringList.Create;
  Helper := TJVCSHelper.Create;
  lvModules.Items.BeginUpdate;
  try
    lvModules.Items.Clear;
  finally
    lvModules.Items.EndUpdate;
  end;
  try
    Helper.GetModuleList(FUser, FHost, FPassword, Project, FPort,  sl);

    sl.Sorted := True;
    sl.Sort;
    {$IFDEF DELPHI6_UP}
    sl.CaseSensitive := False;
    {$ENDIF DELPHI6_UP}

    for I := 0 to sl.Count - 1 do
      with lvModules.Items.Add do
      begin
        StrTokenToStrings(sl[I], ';', sl1);

        if sl1.Count > 2 then begin
          Caption := sl1[0];
          SubItems.Add(sl1[2]);
          SubItems.Add(sl1[1]);
          ImageIndex := -1;
          StateIndex := -1;
          Data := Pointer(StrToInt(sl1[3]));
        end
        else
          Caption := 'Internal Error!!';
      end;
  finally
    sl.Free;
    sl1.Free;
    Helper.Free;
  end;
end;

procedure TFormSelectJVCSModule.FillProjectList;
var
  sl: TStringList;
  I: Integer;
  Helper: TJVCSHelper;
begin
  sl := TStringList.Create;
  Helper := TJVCSHelper.Create;
  lvProjects.Items.BeginUpdate;
  try
    lvProjects.Items.Clear;
  finally
    lvProjects.Items.EndUpdate;
  end;
  try
    Helper.GetProjectList(FUser, FHost, FPassword, FPort,  sl);

    sl.Sorted := True;
    sl.Sort;
    {$IFDEF DELPHI6_UP}
    sl.CaseSensitive := False;
    {$ENDIF DELPHI6_UP}

    for I := 0 to sl.Count - 1 do
      with lvProjects.Items.Add do
      begin
        Caption := sl.Names[I];
        SubItems.Add(sl.Values[sl.Names[I]]);
        ImageIndex := 0;
        StateIndex := 0;
      end;
  finally
    sl.Free;
    Helper.Free;
  end;
end;

procedure TFormSelectJVCSModule.lvProjectsClick(Sender: TObject);
begin
  if lvProjects.Selected <> nil then
    FillModuleList(lvProjects.Selected.Caption);
end;

end.
