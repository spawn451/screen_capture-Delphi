unit GDIFrameProcessor;

// GDI implementation

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, ScreenCaptureTypes;

const
  CAPTUREBLT = $40000000;

type
  TGDIFrameProcessor = class(TBaseFrameProcessor)
  private
    FMonitorDC: HDC;
    FCaptureDC: HDC;
    FCaptureBMP: HBITMAP;
    FSelectedMonitor: ScreenCaptureTypes.TMonitor;
    FNewImageBuffer: PByte;
    FData: TThreadData;
  public
    destructor Destroy; override;
    function Init(Data: TThreadData; Monitor: ScreenCaptureTypes.TMonitor): TDUPL_RETURN;
    function ProcessFrame(const CurrentMonitor: ScreenCaptureTypes.TMonitor): TDUPL_RETURN;
  end;

implementation

uses ScreenCapture;  // For ProcessCapture function

{ TGDIFrameProcessor }

destructor TGDIFrameProcessor.Destroy;
begin
  if FNewImageBuffer <> nil then
    FreeMem(FNewImageBuffer);
  if FCaptureBMP <> 0 then
    DeleteObject(FCaptureBMP);
  if FCaptureDC <> 0 then
    DeleteDC(FCaptureDC);
  if FMonitorDC <> 0 then
    DeleteDC(FMonitorDC);
  inherited;
end;

function TGDIFrameProcessor.Init(Data: TThreadData; Monitor: ScreenCaptureTypes.TMonitor): TDUPL_RETURN;
begin
  FSelectedMonitor := Monitor;
  Result := DUPL_RETURN_SUCCESS;

  // Create Device Contexts
  FMonitorDC := CreateDC('DISPLAY', nil, nil, nil);
  FCaptureDC := CreateCompatibleDC(FMonitorDC);
  FCaptureBMP := CreateCompatibleBitmap(FMonitorDC, Monitor.Width, Monitor.Height);

  if (FMonitorDC = 0) or (FCaptureDC = 0) or (FCaptureBMP = 0) then
  begin
    Result := DUPL_RETURN_ERROR_EXPECTED;
    Exit;
  end;

  // Allocate buffer for the image data
  GetMem(FNewImageBuffer, Monitor.Width * Monitor.Height * SizeOf(TFrameBGRA));
  FData := Data;
end;

function TGDIFrameProcessor.ProcessFrame(const CurrentMonitor: ScreenCaptureTypes.TMonitor): TDUPL_RETURN;
var
  OriginalBmp: HBITMAP;
  Bi: BITMAPINFOHEADER;
  ImageRect: TFrameRect;
begin
  Result := DUPL_RETURN_SUCCESS;

  // Setup the image rectangle
  ImageRect.Left := 0;
  ImageRect.Top := 0;
  ImageRect.Bottom := FSelectedMonitor.Height;
  ImageRect.Right := FSelectedMonitor.Width;

  // Select bitmap into DC
  OriginalBmp := SelectObject(FCaptureDC, FCaptureBMP);

  // Capture the screen
  if not BitBlt(FCaptureDC, 0, 0, ImageRect.Right, ImageRect.Bottom,
                FMonitorDC, 0, 0, SRCCOPY or CAPTUREBLT) then
  begin
    SelectObject(FCaptureDC, OriginalBmp);
    Result := DUPL_RETURN_ERROR_EXPECTED;
    Exit;
  end;

  // Setup bitmap info
  FillChar(Bi, SizeOf(Bi), 0);
  Bi.biSize := SizeOf(BITMAPINFOHEADER);
  Bi.biWidth := ImageRect.Right;
  Bi.biHeight := -ImageRect.Bottom;  // Negative for top-down bitmap
  Bi.biPlanes := 1;
  Bi.biBitCount := SizeOf(TFrameBGRA) * 8;
  Bi.biCompression := BI_RGB;
  Bi.biSizeImage := ((ImageRect.Right * Bi.biBitCount + 31) div 32) * 4 * ImageRect.Bottom;

  // Get the bitmap data
  GetDIBits(FMonitorDC, FCaptureBMP, 0, ImageRect.Bottom,
            FNewImageBuffer, PBITMAPINFO(@Bi)^, DIB_RGB_COLORS);

  SelectObject(FCaptureDC, OriginalBmp);

  // Process the captured image
  ProcessCapture(FData.ScreenCaptureData, Self, CurrentMonitor,
                FNewImageBuffer, FSelectedMonitor.Width * SizeOf(TFrameBGRA));
end;

end.
