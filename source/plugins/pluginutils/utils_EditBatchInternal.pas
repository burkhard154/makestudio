(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msEditBatchInternal.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/09/11  BSchranz - Internal Batch command launched
                       Uses SynEdit
                       You may retrieve the latest version of this file at
                       the SynEdit home page, located at
                       http://SynEdit.SourceForge.net

-----------------------------------------------------------------------------*)

unit utils_EditBatchInternal;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvToolEdit, utils_RunBatch, JvExMask, ExtCtrls,
  SynEditHighlighter, SynEdit, SynHighlighterBat;

type
  TFormEditBatchInternal = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    btImport: TButton;
    btExport: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Memo: TSynEdit;
    SynBatSyn1: TSynBatSyn;
    cbReplaceVars: TCheckBox;
    SynBatSyn2: TSynBatSyn;
    procedure btExportClick(Sender: TObject);
    procedure btImportClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgEditBatchInternalCommand(M: TBatchModule): Boolean;

implementation

{$R *.dfm}

function DlgEditBatchInternalCommand(M: TBatchModule): Boolean;
begin
  Result := False;
  with TFormEditBatchInternal.Create(nil) do
  try
    Memo.Lines.Assign( M.BatchStrings);
    cbReplaceVars.Checked := M.ReplaceVars;
    if ShowModal = mrOk then
    begin
      Result := True;
      M.BatchStrings.Assign( Memo.Lines);
      M.ReplaceVars := cbReplaceVars.Checked;
    end;
  finally
    Free;
  end;
end;

procedure TFormEditBatchInternal.btImportClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    Memo.Lines.LoadFromFile( OpenDialog.FileName);
end;

procedure TFormEditBatchInternal.btExportClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    Memo.Lines.LoadFromFile( OpenDialog.FileName);
end;

end.
