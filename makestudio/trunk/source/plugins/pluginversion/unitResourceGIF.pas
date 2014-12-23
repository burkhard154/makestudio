unit unitResourceGIF;

interface

uses Windows, Classes, SysUtils, graphics, gifimage, unitResourceDetails, unitResourceGraphics;

type
//------------------------------------------------------------------------
// GIF resource details class

  TGifResourceDetails = class (TGraphicsResourceDetails)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    procedure InitNew; override;
    class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
  public
    class function GetBaseType : string; override;
    procedure GetImage (picture : TPicture); override;
  end;


implementation

{ TGifResourceDetails }

class function TGifResourceDetails.GetBaseType: string;
begin
  Result := 'GIF';
end;

function TGifResourceDetails.GetHeight: Integer;
begin
  Result := PWORD (PChar (data) + 6 + SizeOf (Word))^;
end;

procedure TGifResourceDetails.GetImage(picture: TPicture);
begin
  picture.graphic := TGifImage.Create;
  data.Seek (0, soFromBeginning);
  TGifImage (picture.graphic).LoadFromStream (data)
end;

function TGifResourceDetails.GetPixelFormat: TPixelFormat;
begin
  Result := pf8Bit;
end;

function TGifResourceDetails.GetWidth: Integer;
begin
  result := PWORD (PChar (data) + 6)^;
end;

procedure TGifResourceDetails.InitNew;
var
  img : TGIFImage;
  bmp : TBitmap;
begin
  bmp := nil;
  img := TGIFImage.Create;
  try
    bmp := TBitmap.Create;
    bmp.Width := 64;
    bmp.Height := 64;
    img.Assign(bmp);
    img.SaveToStream (data);
  finally
    img.Free;
    bmp.Free
  end
end;

class function TGifResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p : PChar;
begin
  p := PChar (data);

  Result := (StrLIComp (p, 'GIF87', 5) = 0) or (StrLIComp (p, 'GIF89', 5) = 0);
end;

initialization
  RegisterResourceDetails (TGIFResourceDetails);
finalization
  UnregisterResourceDetails (TGIFResourceDetails);
end.
