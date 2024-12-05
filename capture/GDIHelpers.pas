unit GDIHelpers;

interface

uses
  Winapi.Windows, Winapi.Dwmapi;

type
  { HDC Wrapper }
  THDCWrapper = class
  private
    FDC: HDC;
  public
    constructor Create;
    destructor Destroy; override;
    property DC: HDC read FDC write FDC;
  end;

  { HBITMAP Wrapper }
  THBITMAPWrapper = class
  private
    FBitmap: HBITMAP;
  public
    constructor Create;
    destructor Destroy; override;
    property Bitmap: HBITMAP read FBitmap write FBitmap;
  end;

  { Window Dimensions structure }
  TWindowDimensions = record
    ClientRect: TRect;
    ClientBorder: TRect;
    Placement: TWindowPlacement;
  end;

function GetWindowDimensions(hwnd: HWND): TWindowDimensions;

implementation

{ THDCWrapper }

constructor THDCWrapper.Create;
begin
  inherited;
  FDC := 0;
end;

destructor THDCWrapper.Destroy;
begin
  if FDC <> 0 then
    DeleteDC(FDC);
  inherited;
end;

{ THBITMAPWrapper }

constructor THBITMAPWrapper.Create;
begin
  inherited;
  FBitmap := 0;
end;

destructor THBITMAPWrapper.Destroy;
begin
  if FBitmap <> 0 then
    DeleteObject(FBitmap);
  inherited;
end;

function GetWindowDimensions(hwnd: HWND): TWindowDimensions;
var
  frame: TRect;
  dwmResult: HRESULT;
begin
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);

  // Get window rect
  GetWindowRect(hwnd, Result.ClientRect);

  // Get window placement
  Result.Placement.length := SizeOf(TWindowPlacement);
  GetWindowPlacement(hwnd, @Result.Placement);

  // Get DWM frame bounds
  FillChar(frame, SizeOf(frame), 0);
  dwmResult := DwmGetWindowAttribute(hwnd,
                                   DWMWA_EXTENDED_FRAME_BOUNDS,
                                   @frame,
                                   SizeOf(frame));

  if Succeeded(dwmResult) then
  begin
    // Calculate border dimensions
    Result.ClientBorder.Left := frame.Left - Result.ClientRect.Left;
    Result.ClientBorder.Top := frame.Top - Result.ClientRect.Top;
    Result.ClientBorder.Right := Result.ClientRect.Right - frame.Right;
    Result.ClientBorder.Bottom := Result.ClientRect.Bottom - frame.Bottom;
  end;

  // Adjust client rectangle
  Result.ClientRect.Bottom := Result.ClientRect.Bottom - Result.ClientBorder.Bottom;
  Result.ClientRect.Top := Result.ClientRect.Top + Result.ClientBorder.Top;
  Result.ClientRect.Left := Result.ClientRect.Left + Result.ClientBorder.Left;
  Result.ClientRect.Right := Result.ClientRect.Right - Result.ClientBorder.Right;
end;

end.
