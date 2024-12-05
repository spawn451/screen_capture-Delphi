unit GDIMouseProcessor;

interface

uses
  Winapi.Windows, System.Types, System.SysUtils, System.Classes,
  ScreenCaptureTypes;

type
  TMousePoint = record
    Position: TPoint;
    HotSpot: TPoint;
  end;

  TOnMouseChanged = procedure(const Frame: TFrame; const Point: TMousePoint) of object;

  TScreenCaptureData = record
    OnMouseChanged: TOnMouseChanged;
  end;

  TWindowCaptureData = record
    OnMouseChanged: TOnMouseChanged;
  end;

  TThreadData = record
    ScreenCaptureData: TScreenCaptureData;
    WindowCaptureData: TWindowCaptureData;
  end;

  TBaseMouseProcessor = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Init(const Data: TThreadData): TDUPL_RETURN; virtual; abstract;
    function ProcessFrame: TDUPL_RETURN; virtual; abstract;
  end;

  TGDIMouseProcessor = class(TBaseMouseProcessor)
  private
    FMonitorDC: HDC;
    FCaptureDC: HDC;
    FImageBuffer: TArray<Byte>;
    FNewImageBuffer: TArray<Byte>;
    FImageBufferSize: Integer;
    FLastX: Integer;
    FLastY: Integer;
    FData: TThreadData;

    procedure UpdateCursorImage(const CursorInfo: TCursorInfo;
                              var IconInfo: TIconInfoExA;
                              const MousePoint: TMousePoint);
  public
    constructor Create; override;
    destructor Destroy; override;
    function Init(const Data: TThreadData): TDUPL_RETURN; override;
    function ProcessFrame: TDUPL_RETURN; override;
  end;

implementation

{ TBaseMouseProcessor }

constructor TBaseMouseProcessor.Create;
begin
  inherited Create;
end;

destructor TBaseMouseProcessor.Destroy;
begin
  inherited;
end;

{ TGDIMouseProcessor }

constructor TGDIMouseProcessor.Create;
begin
  inherited;
  FMonitorDC := 0;
  FCaptureDC := 0;
  FImageBufferSize := 0;
  FLastX := 0;
  FLastY := 0;
end;

destructor TGDIMouseProcessor.Destroy;
begin
  if FCaptureDC <> 0 then
    DeleteDC(FCaptureDC);
  if FMonitorDC <> 0 then
    ReleaseDC(0, FMonitorDC);
  inherited;
end;

function TGDIMouseProcessor.Init(const Data: TThreadData): TDUPL_RETURN;
begin
  Result := DUPL_RETURN_SUCCESS;

  // Get DC for entire screen
  FMonitorDC := GetDC(0);
  if FMonitorDC = 0 then
    Exit(DUPL_RETURN_ERROR_EXPECTED);

  // Create compatible DC for cursor capture
  FCaptureDC := CreateCompatibleDC(FMonitorDC);
  if FCaptureDC = 0 then
  begin
    ReleaseDC(0, FMonitorDC);
    FMonitorDC := 0;
    Exit(DUPL_RETURN_ERROR_EXPECTED);
  end;

  FData := Data;
end;

function TGDIMouseProcessor.ProcessFrame: TDUPL_RETURN;
var
  CursorInfo: TCursorInfo;
  IconInfo: TIconInfoExA;
  BmpCursor: TBitmap;
  MousePoint: TMousePoint;
begin
  Result := DUPL_RETURN_SUCCESS;

  // Check if we need to process mouse changes
  if not (Assigned(FData.ScreenCaptureData.OnMouseChanged) or
          Assigned(FData.WindowCaptureData.OnMouseChanged)) then
    Exit;

  // Get cursor info
  FillChar(CursorInfo, SizeOf(CursorInfo), 0);
  CursorInfo.cbSize := SizeOf(CursorInfo);
  if not GetCursorInfo(CursorInfo) then
    Exit(DUPL_RETURN_ERROR_EXPECTED);

  // Check if cursor is visible
  if (CursorInfo.Flags and CURSOR_SHOWING) = 0 then
    Exit; // Cursor is hidden

  // Get icon info
  FillChar(IconInfo, SizeOf(IconInfo), 0);
  IconInfo.cbSize := SizeOf(IconInfo);
  if not GetIconInfoExA(CursorInfo.hCursor, @IconInfo) then // Added @ operator
    Exit(DUPL_RETURN_ERROR_EXPECTED);

  try
    // Create mouse point info
    MousePoint.Position.X := CursorInfo.ptScreenPos.x;
    MousePoint.Position.Y := CursorInfo.ptScreenPos.y;
    MousePoint.HotSpot.X := IconInfo.xHotspot;
    MousePoint.HotSpot.Y := IconInfo.yHotspot;

    // Update cursor image if needed
    UpdateCursorImage(CursorInfo, IconInfo, MousePoint);

    // Store last position
    FLastX := MousePoint.Position.X;
    FLastY := MousePoint.Position.Y;
  finally
    // Cleanup icon resources
    if IconInfo.hbmColor <> 0 then
      DeleteObject(IconInfo.hbmColor);
    if IconInfo.hbmMask <> 0 then
      DeleteObject(IconInfo.hbmMask);
  end;
end;

procedure TGDIMouseProcessor.UpdateCursorImage(const CursorInfo: TCursorInfo;
  const IconInfo: TIconInfoExA; const MousePoint: TMousePoint);
var
  BmpInfo: TBitmapInfo;
  BmpCursor: Windows.TBitmap;
  OriginalBmp, CursorBmp: HBITMAP;
  ImageSize: Integer;
  WholeImg: TFrame;
  Ptr: PCardinal;
  I: Integer;
begin
  // Get cursor bitmap info
  if GetObject(IconInfo.hbmColor, SizeOf(BmpCursor), @BmpCursor) = 0 then
    Exit;

  // Create bitmap for cursor
  CursorBmp := CreateCompatibleBitmap(FMonitorDC, BmpCursor.bmWidth, BmpCursor.bmHeight);
  if CursorBmp = 0 then
    Exit;

  try
    // Select bitmap into DC
    OriginalBmp := SelectObject(FCaptureDC, CursorBmp);

    // Draw cursor
    if not DrawIcon(FCaptureDC, 0, 0, CursorInfo.hCursor) then
      Exit;

    // Setup bitmap info
    FillChar(BmpInfo, SizeOf(BmpInfo), 0);
    with BmpInfo.bmiHeader do
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biWidth := BmpCursor.bmWidth;
      biHeight := -BmpCursor.bmHeight;  // Negative for top-down
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
      biSizeImage := ((biWidth * biBitCount + 31) div 32) * 4 * Abs(biHeight);
    end;

    // Resize buffer if needed
    ImageSize := BmpInfo.bmiHeader.biSizeImage;
    if (ImageSize > FImageBufferSize) or (Length(FImageBuffer) = 0) then
    begin
      SetLength(FNewImageBuffer, ImageSize);
      SetLength(FImageBuffer, ImageSize);
      FImageBufferSize := ImageSize;
    end;

    // Get bitmap bits
    if GetDIBits(FMonitorDC, CursorBmp, 0, Abs(BmpInfo.bmiHeader.biHeight),
                 FNewImageBuffer, BmpInfo, DIB_RGB_COLORS) = 0 then
      Exit;

    // Fix alpha channel for I-beam cursor
    if IconInfo.wResID = 32513 then
    begin
      Ptr := PCardinal(FNewImageBuffer);
      for I := 0 to (BmpCursor.bmWidth * BmpCursor.bmHeight) - 1 do
      begin
        if Ptr^ <> 0 then
          Ptr^ := $FF000000;
        Inc(Ptr);
      end;
    end;

    // Create frame from image
    WholeImg := CreateImage(
      Rect(0, 0, BmpCursor.bmWidth, BmpCursor.bmHeight),
      BmpCursor.bmWidth * SizeOf(TImageBGRA),
      PImageBGRA(FNewImageBuffer)
    );

    // Check if image changed
    if not CompareMem(FNewImageBuffer, FImageBuffer, ImageSize) then
    begin
      // Notify about cursor image change
      if Assigned(FData.WindowCaptureData.OnMouseChanged) then
        FData.WindowCaptureData.OnMouseChanged(@WholeImg, MousePoint);
      if Assigned(FData.ScreenCaptureData.OnMouseChanged) then
        FData.ScreenCaptureData.OnMouseChanged(@WholeImg, MousePoint);

      // Swap buffers
      var Temp := FImageBuffer;
      FImageBuffer := FNewImageBuffer;
      FNewImageBuffer := Temp;
    end
    else if (FLastX <> MousePoint.Position.X) or (FLastY <> MousePoint.Position.Y) then
    begin
      // Only position changed
      if Assigned(FData.WindowCaptureData.OnMouseChanged) then
        FData.WindowCaptureData.OnMouseChanged(nil, MousePoint);
      if Assigned(FData.ScreenCaptureData.OnMouseChanged) then
        FData.ScreenCaptureData.OnMouseChanged(nil, MousePoint);
    end;
  finally
    SelectObject(FCaptureDC, OriginalBmp);
    DeleteObject(CursorBmp);
  end;
end;

end.
