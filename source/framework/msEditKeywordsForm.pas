(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EditKeywordsForm.pas

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

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to JVCSMAK
2003/11/28  USchuster - 2nd Migrationstep (fixed header and removed Variants)
2003/12/05  USchuster - re-formatted
2005/02/04  USchuster - typo in header and moved {$I jedi.inc} below unit
2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
2005/04/09  BSchranz  - Translated to englisch

-----------------------------------------------------------------------------*)

unit msEditKeywordsForm;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormKeywords = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel4: TPanel;
    Label1: TLabel;
    edKeyword: TEdit;
    Panel5: TPanel;
    lbKeywords: TListBox;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure lbKeywordsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;


function DlgEditKeywords(sl: TStringList): Boolean;

implementation

{$R *.dfm}

function DlgEditKeywords(sl: TStringList): Boolean;
var
  I: Integer;
begin
  Result := False;
  with TFormKeywords.Create(nil) do
  try
    lbKeywords.Items.Assign(sl);
    if ShowModal = mrOk then
    begin
      sl.Assign(lbKeywords.Items);
      for I := 0 to sl.Count - 1 do
        sl[I] := UpperCase(sl[I]);
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TFormKeywords.lbKeywordsClick(Sender: TObject);
begin
  if lbKeywords.ItemIndex > -1 then
    edKeyword.Text := lbKeywords.Items[lbKeywords.ItemIndex];
end;

procedure TFormKeywords.Button1Click(Sender: TObject);
begin
  if edKeyword.Text <> '' then
    lbKeywords.Items.Add(edKeyword.Text);
end;

procedure TFormKeywords.Button2Click(Sender: TObject);
var
  I: Integer;
begin
  if lbKeywords.ItemIndex > -1 then
  begin
    I := lbKeywords.ItemIndex;
    lbKeywords.Items.Delete(I);

    if I >= lbKeywords.Items.Count then
      I := lbKeywords.Items.Count - 1;
    if I > -1 then
      lbKeywords.ItemIndex := I;
  end;
end;

end.
