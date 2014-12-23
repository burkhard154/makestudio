unit SEFindStr;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, unitResourceDetails, ComCtrls;

type
  TFormFindRes = class(TForm)
    edFileSpec: TEdit;
    edFindText: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Memo: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fResourceModule : TResourceModule;
  public
    procedure FindInFile(Filename, SubStr: String);
  end;

var
  FormFindRes: TFormFindRes;

implementation

uses unitResFile, unitRCFile, unitNTModule, UnitPEFile, unitResourceMessages;

{$R *.dfm}

procedure TFormFindRes.FindInFile(Filename, SubStr: String);
var ext : string;
    i, k:Integer;
    s:String;
    d:TStringResourceDetails;
begin
  if fResourceModule<>nil then
    fResourceModule.Free;


  ext := UpperCase (ExtractFileExt (fileName));
  if (ext = '.RES') or (ext = '.DCR') then
    fResourceModule := TResModule.Create
  else
    if (ext = '.RC') then
      fResourceModule := TRCModule.Create
    else
      if Win32Platform = VER_PLATFORM_WIN32_NT then
        fResourceModule := TNTModule.Create
      else
        fResourceModule := TPEResourceModule.Create;

  Memo.SelStart := Length( Memo.Lines.Text);
  Memo.SelLength := 0;
  Memo.SelAttributes.Color := clBlack;
  Memo.Lines.Add( 'Loading: '+ Filename);
  fResourceModule.LoadFromFile (fileName);

  for i:=0 to fResourceModule.ResourceCount-1 do begin
    if fResourceModule.ResourceDetails[i] is TStringResourceDetails then begin

      d := TStringResourceDetails( fResourceModule.ResourceDetails[i]);

      for k:=0 to d.Count-1 do
        if Pos( UpperCase( substr), UpperCase( d.Strings[ k]))>0 then begin
          Memo.SelStart := Length( Memo.Lines.Text);
          Memo.SelLength := 0;
          Memo.SelAttributes.Color := clRed;
          Memo.Lines.Add(
            Format( 'Found: %d;%s', [ d.Ids[ k], d.Strings[ k]]));
        end;


    end;
  end;

end;

procedure TFormFindRes.FormCreate(Sender: TObject);
begin
  fResourceModule := nil;
end;

procedure TFormFindRes.FormDestroy(Sender: TObject);
begin
  if fResourceModule<>nil then
    fResourceModule.Free;
end;

procedure TFormFindRes.Button1Click(Sender: TObject);
var sr: TSearchRec;
    p : String;
begin
  Memo.Lines.Clear;

  p:= IncludeTrailingPathDelimiter( ExtractFilePath( edFileSpec.Text) );
  if p<>'' then begin

    if FindFirst( edFileSpec.Text, SysUtils.faArchive, sr) = 0 then begin
      FindInFile( p + sr.Name, edFindText.Text);

      while FindNext(sr) = 0 do
        FindInFile( p + sr.Name, edFindText.Text);

      SysUtils.FindClose(sr);
    end;
  end
  else
    Memo.Lines.Add( 'Path not valid!');
end;

procedure TFormFindRes.Button2Click(Sender: TObject);
begin
  Close;
end;

end.
