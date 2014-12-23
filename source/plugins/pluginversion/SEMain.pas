unit SEMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, unitResourceDetails, cxGraphics, cxCustomData,
  cxStyles, cxTL, cxLabel, cxMemo, cxInplaceContainer, dxStatusBar,
  cxControls, Menus, ExtCtrls, cxLookAndFeels, cxLookAndFeelPainters;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    dxStatusBar1: TdxStatusBar;
    MainMenu1: TMainMenu;
    Load1: TMenuItem;
    Load2: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Find1: TMenuItem;
    Copy1: TMenuItem;
    N2: TMenuItem;
    Panel1: TPanel;
    TreeList: TcxTreeList;
    cxTreeList1cxTreeListColumn1: TcxTreeListColumn;
    cxTreeList1cxTreeListColumn2: TcxTreeListColumn;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
  private
    fResourceModule : TResourceModule;
  public
    procedure LoadFile( Filename:String);
    procedure FillMemo;
  end;

var
  Form1: TForm1;

implementation

uses unitResFile, unitRCFile, unitNTModule, UnitPEFile, unitResourceMessages, SEFindStr;

{$R *.dfm}

resourcestring
  rstBitmap       = 'Bitmap';
  rstIcon         = 'Icon';
  rstCursor       = 'Cursor';
  rstMenu         = 'Menu';
  rstToolbar      = 'Toolbar';
  rstDialog       = 'Dialog';
  rstAccelerator  = 'Accelerator Table';
  rstString       = 'String Table';
  rstRCData       = 'RC Data';
  rstMessageTable = 'MessageTable';
  rstVersion      = 'Version';
  rstGroupCursor  = 'Cursor Group';
  rstGroupIcon    = 'Icon Group';
  rstXPManifest   = 'XP Theme Manifest';

(*----------------------------------------------------------------------*
 | function GetTypeName                                                 |
 |                                                                      |
 | Get the display name for a resource type                             |
 *----------------------------------------------------------------------*)
function GetTypeName (const tp : string) : string;
var
  i : Integer;
begin
  i := ResourceNameToInt (tp);

  case i of
    Integer (RT_BITMAP)       : result := rstBitmap;
    Integer (RT_ICON)         : result := rstIcon;
    Integer (RT_CURSOR)       : result := rstCursor;
    Integer (RT_MENU)         : result := rstMenu;
    Integer (RT_DIALOG)       : result := rstDialog;
    Integer (RT_STRING)       : result := rstString;
    Integer (RT_ACCELERATOR)  : Result := rstAccelerator;
    Integer (RT_RCDATA)       : result := rstRCData;
    Integer (RT_MESSAGETABLE) : result := rstMessageTable;
    Integer (RT_VERSION)      : result := rstVersion;
    Integer (RT_GROUP_CURSOR) : result := rstGroupCursor;
    Integer (RT_GROUP_ICON)   : result := rstGroupIcon;
//    Integer (RT_XPMANIFEST)   : result := rstXPManifest;
//    Integer (RT_TOOLBAR)      : result := rstToolbar;
    else
      result := tp
  end
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadFile( OpenDialog1.FileName);
end;

procedure TForm1.LoadFile(Filename: String);
var
  ext : string;
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

  fResourceModule.LoadFromFile (fileName);
  FillMemo;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fResourceModule := nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if fResourceModule<>nil then
    fResourceModule.Free;
end;

procedure TForm1.FillMemo;
var i, k:Integer;
    s:String;
    d:TStringResourceDetails;
begin
  TreeList.Clear;

  TreeList.BeginUpdate;
  
  try

    if fResourceModule<>nil then begin
      for i:=0 to fResourceModule.ResourceCount-1 do begin
        if fResourceModule.ResourceDetails[i] is TStringResourceDetails then begin

          d := TStringResourceDetails( fResourceModule.ResourceDetails[i]);

          for k:=0 to d.Count-1 do begin
            With TreeList.Add do begin
              Values[0] := d.Ids[ k];
              Texts[1] := d.Strings[ k];
            end;
          end;


        end;
      end;
    end;

  finally
    TreeList.EndUpdate;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Find1Click(Sender: TObject);
begin
  FormFindRes.ShowModal;
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
  TreeList.CopyAllToClipboard;
end;

end.
