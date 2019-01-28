unit delphi_AddComponentFolder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, JvExStdCtrls, JvHtControls,
  ExtCtrls, JvSearchFiles, JvComponentBase, delphi32_utils, delphi32_vars;

type
  TFormAddComponentFolder = class(TForm)
    Panel1: TPanel;
    JvHTLabel1: TJvHTLabel;
    edCompFolder: TJvDirectoryEdit;
    Label1: TLabel;
    edDirectoryMask: TEdit;
    edFileMask: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    lbFound: TListBox;
    Label4: TLabel;
    Label5: TLabel;
    lbMissing: TListBox;
    btOk: TButton;
    btCancel: TButton;
    btFind: TButton;
    FileSearcher: TJvSearchFiles;
    procedure btFindClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure FileSearcherFindFile(Sender: TObject; const AName: string);
  private
  public
    procedure Search;
    procedure Sort;
  end;

var
  FormAddComponentFolder: TFormAddComponentFolder;

implementation

{$R *.dfm}

procedure TFormAddComponentFolder.btFindClick(Sender: TObject);
begin
  Sort;
end;

procedure TFormAddComponentFolder.btOkClick(Sender: TObject);
var
  i: Integer;
begin
  // now add all files to the editor
  for i := 0 to lbFound.Items.Count - 1 do
    MakeStudio.AddCommandByFile(lbFound.Items[i]);

  Close;
end;

procedure TFormAddComponentFolder.FileSearcherFindFile(Sender: TObject; const AName: string);
begin
  // lbFound.Items.Add( AName);
end;

procedure TFormAddComponentFolder.Search;
begin
  FileSearcher.Files.Clear;
  FileSearcher.Directories.Clear;
  FileSearcher.RootDirectory := edCompFolder.Text;
  FileSearcher.DirParams.FileMasks.Text := edDirectoryMask.Text;
  FileSearcher.FileParams.FileMasks.Text := edFileMask.Text + '.dpk';
  FileSearcher.Search;
end;

procedure TFormAddComponentFolder.Sort;
var
  i, k, l, MovedIdx: Integer;
  RequiredList: TStringList;
  Package: String;
begin
  Screen.Cursor := crHourGlass;

  try
    lbFound.Items.Clear;
    lbMissing.Items.Clear;

    Search;

    // now have a look
    RequiredList := TStringList.Create;
    try
      i := 0;
      while i < FileSearcher.Files.Count do
      begin

        MovedIdx := -1; // first Index of a Required Package
        if SameText(ExtractFileExt(FileSearcher.Files[i]), '.dpk') then
        begin
          GetPackageDepencies(FileSearcher.Files[i], RequiredList);

          for k := 0 to RequiredList.Count - 1 do
          begin
            for l := 0 to FileSearcher.Files.Count - 1 do
            begin
              Package := ChangeFileExt(ExtractFilename(FileSearcher.Files[l]), '');

              // Required Package found
              if SameText(RequiredList[k], Package) then
              begin
                if l > i then
                begin
                  FileSearcher.Files.Move(l, i);
                  // Store the position of the first moved package
                  if MovedIdx < 0 then
                    MovedIdx := i;
                  inc(i);
                end;
                break;
              end
              // Required Package not found
              else if l = FileSearcher.Files.Count - 1 then
              begin
                if lbMissing.Items.IndexOf(RequiredList[k]) < 0 then
                  lbMissing.Items.Add(RequiredList[k]);
              end;

            end;
          end;
        end;

        // if there are moved packages, start from there
        if MovedIdx >= 0 then
          i := MovedIdx
        else
          inc(i);
      end;

    finally
      RequiredList.Free;
    end;

    lbFound.Items.Text := FileSearcher.Files.Text;

  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
