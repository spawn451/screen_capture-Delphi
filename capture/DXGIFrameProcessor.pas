unit DXGIFrameProcessor;

// DXGI implementation

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math,
  DX12.D3D11, DX12.D3DCommon, DX12.DXGI, DX12.DXGI1_2, ScreenCaptureTypes;

const
  DXGI_ERROR_ACCESS_LOST = HResult($887A0026);
  DXGI_ERROR_WAIT_TIMEOUT = HResult($887A0027);
  DXGI_ERROR_DEVICE_REMOVED = HResult($887A0005);
  DXGI_ERROR_DEVICE_RESET = HResult($887A0007);
  DXGI_ERROR_INVALID_CALL = HResult($887A0001);
  DXGI_ERROR_UNSUPPORTED = HResult($887A0004);
  DXGI_ERROR_SESSION_DISCONNECTED = HResult($887A0028);
  DXGI_ERROR_NOT_FOUND = HResult($887A0002);

type

  TDX_RESOURCES = record
    Device: ID3D11Device;
    DeviceContext: ID3D11DeviceContext;
  end;

  TDUPLE_RESOURCES = record
    OutputDuplication: IDXGIOutputDuplication;
    OutputDesc: TDXGI_OUTPUT_DESC;
    Output: Cardinal;
  end;

  TDirtyRegionMetadata = record
    MoveCount: Integer;
    DirtyCount: Integer;
    MetadataBuffer: array of Byte;
  end;

  TAcquireFrameRAII = class
  private
    FDuplLock: IDXGIOutputDuplication;
    FAcquiredLock: Boolean;
    procedure TryRelease;
  public
    constructor Create(ADupl: IDXGIOutputDuplication);
    destructor Destroy; override;
    function AcquireNextFrame(TimeoutInMilliseconds: Cardinal;
      var FrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
      var DesktopResource: IDXGIResource): HResult;
  end;

  TMappedSubresourceRAII = class
  private
    FContext: ID3D11DeviceContext;
    FResource: ID3D11Resource;
    FSubresource: Cardinal;
  public
    constructor Create(AContext: ID3D11DeviceContext);
    destructor Destroy; override;
    function Map(Resource: ID3D11Resource; Subresource: Cardinal;
      MapType: TD3D11_MAP; MapFlags: Cardinal;
      var MappedResource: TD3D11_MAPPED_SUBRESOURCE): HResult;
  end;

  TDXFrameProcessor = class(TBaseFrameProcessor) // Add inheritance here
  private
    FDevice: ID3D11Device;
    FDeviceContext: ID3D11DeviceContext;
    FOutputDuplication: IDXGIOutputDuplication;
    FOutputDesc: TDXGI_OUTPUT_DESC;
    FOutput: Cardinal;
    FStagingSurf: ID3D11Texture2D;
    FSelectedMonitor: ScreenCaptureTypes.TMonitor;
    FMetadata: TDirtyRegionMetadata;
    FUpdatedRegion: TRegion;
    FHasUpdatedRegions: Boolean;
    FData: TThreadData;

  public
    constructor Create;
    destructor Destroy; override;
    function Init(Data: TThreadData; Monitor: ScreenCaptureTypes.TMonitor)
      : TDUPL_RETURN;
    function ProcessFrame(const CurrentMonitorInfo: ScreenCaptureTypes.TMonitor)
      : TDUPL_RETURN;
    function ProcessDirtyRegions(const FrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
      var UpdatedRegion: TRegion): Boolean;
    function ProcessChangedFrame(const CurrentMonitor: TMonitor;
      const FrameInfo: TDXGI_OUTDUPL_FRAME_INFO): TDUPL_RETURN;

    procedure OptimizeRotatedRegions(var Region: TRegion);
    function HasUpdatedRegions: Boolean;
    function GetUpdatedRegions: TRegion;
    procedure SetUpdatedRegions(const Region: TRegion);

  end;

function Initialize(var Resources: TDX_RESOURCES): TDUPL_RETURN;
function InitializeDuple(var Resources: TDUPLE_RESOURCES; Device: ID3D11Device;
  Adapter, Output: Cardinal): TDUPL_RETURN;
function ConvertRect(Dirty: TRect; const DeskDesc: TDXGI_OUTPUT_DESC): TRect;

implementation

uses ScreenCapture;

const
  SystemTransitionsExpectedErrors: array [0 .. 3] of HResult =
    (DXGI_ERROR_DEVICE_REMOVED, DXGI_ERROR_ACCESS_LOST,
    HResult(WAIT_ABANDONED), S_OK);

  CreateDuplicationExpectedErrors: array [0 .. 4] of HResult =
    (DXGI_ERROR_DEVICE_REMOVED, HResult(E_ACCESSDENIED), DXGI_ERROR_UNSUPPORTED,
    DXGI_ERROR_SESSION_DISCONNECTED, S_OK);

  FrameInfoExpectedErrors: array [0 .. 3] of HResult =
    (DXGI_ERROR_DEVICE_REMOVED, DXGI_ERROR_ACCESS_LOST,
    DXGI_ERROR_INVALID_CALL, S_OK);

  EnumOutputsExpectedErrors: array [0 .. 1] of HResult =
    (DXGI_ERROR_NOT_FOUND, S_OK);

function ProcessFailure(Device: ID3D11Device; const Str, Title: string;
  HR: HResult; ExpectedErrors: array of HResult): TDUPL_RETURN;
var
  TranslatedHr, DeviceRemovedReason: HResult;
  i: Integer;
begin
  if Device <> nil then
  begin
    DeviceRemovedReason := Device.GetDeviceRemovedReason;
    case DeviceRemovedReason of
      DXGI_ERROR_DEVICE_REMOVED, DXGI_ERROR_DEVICE_RESET, E_OUTOFMEMORY:
        TranslatedHr := DXGI_ERROR_DEVICE_REMOVED;
      S_OK:
        TranslatedHr := HR;
    else
      TranslatedHr := DeviceRemovedReason;
    end;
  end
  else
    TranslatedHr := HR;

  if Length(ExpectedErrors) > 0 then
  begin
    for i := 0 to High(ExpectedErrors) do
    begin
      if ExpectedErrors[i] = TranslatedHr then
        Exit(DUPL_RETURN_ERROR_EXPECTED);
      if ExpectedErrors[i] = S_OK then
        Break;
    end;
  end;

  Result := DUPL_RETURN_ERROR_UNEXPECTED;
end;

function Initialize(var Resources: TDX_RESOURCES): TDUPL_RETURN;
var
  HR: HResult;
  DriverTypes: array [0 .. 3] of TD3D_DRIVER_TYPE;
  FeatureLevels: array [0 .. 3] of TD3D_FEATURE_LEVEL;
  FeatureLevel: TD3D_FEATURE_LEVEL;
  i: Integer;
begin
  DriverTypes[0] := D3D_DRIVER_TYPE_HARDWARE;
  DriverTypes[1] := D3D_DRIVER_TYPE_WARP;
  DriverTypes[2] := D3D_DRIVER_TYPE_REFERENCE;
  DriverTypes[3] := D3D_DRIVER_TYPE_UNKNOWN;

  FeatureLevels[0] := D3D_FEATURE_LEVEL_11_0;
  FeatureLevels[1] := D3D_FEATURE_LEVEL_10_1;
  FeatureLevels[2] := D3D_FEATURE_LEVEL_10_0;
  FeatureLevels[3] := D3D_FEATURE_LEVEL_9_1;

  for i := 0 to High(DriverTypes) do
  begin
    HR := D3D11CreateDevice(nil, DriverTypes[i], 0, 0, @FeatureLevels[0],
      Length(FeatureLevels), D3D11_SDK_VERSION, Resources.Device, FeatureLevel,
      Resources.DeviceContext);

    if SUCCEEDED(HR) then
      Break;
  end;

  if FAILED(HR) then
    Result := ProcessFailure(nil, 'Failed to create device in InitializeDx',
      'Error', HR, [])
  else
    Result := DUPL_RETURN_SUCCESS;
end;

function InitializeDuple(var Resources: TDUPLE_RESOURCES; Device: ID3D11Device;
  Adapter, Output: Cardinal): TDUPL_RETURN;
var
  Factory: IDXGIFactory;
  DxgiAdapter: IDXGIAdapter;
  DxgiOutput: IDXGIOutput;
  DxgiOutput1: IDXGIOutput1;
  HR: HResult;
begin
  HR := CreateDXGIFactory(IDXGIFactory, Factory);
  if FAILED(HR) then
    Exit(ProcessFailure(nil, 'Failed to construct DXGIFactory',
      'Error', HR, []));

  HR := Factory.EnumAdapters(Adapter, DxgiAdapter);
  if FAILED(HR) then
    Exit(ProcessFailure(Device, 'Failed to get DXGI Adapter', 'Error', HR,
      SystemTransitionsExpectedErrors));

  HR := DxgiAdapter.EnumOutputs(Output, DxgiOutput);
  if FAILED(HR) then
    Exit(ProcessFailure(Device,
      'Failed to get specified output in DUPLICATIONMANAGER', 'Error', HR,
      EnumOutputsExpectedErrors));

  DxgiOutput.GetDesc(Resources.OutputDesc);

  HR := DxgiOutput.QueryInterface(IDXGIOutput1, DxgiOutput1);
  if FAILED(HR) then
    Exit(ProcessFailure(nil,
      'Failed to QI for DxgiOutput1 in DUPLICATIONMANAGER', 'Error', HR, []));

  HR := DxgiOutput1.DuplicateOutput(Device, Resources.OutputDuplication);
  if FAILED(HR) then
    Exit(ProcessFailure(Device,
      'Failed to get duplicate output in DUPLICATIONMANAGER', 'Error', HR,
      CreateDuplicationExpectedErrors));

  Resources.Output := Output;
  Result := DUPL_RETURN_SUCCESS;
end;

function ConvertRect(Dirty: TRect; const DeskDesc: TDXGI_OUTPUT_DESC): TRect;
var
  Width, Height: Integer;
begin
  Result := Dirty;
  Width := DeskDesc.DesktopCoordinates.Right - DeskDesc.DesktopCoordinates.Left;
  Height := DeskDesc.DesktopCoordinates.Bottom -
    DeskDesc.DesktopCoordinates.Top;

  case DeskDesc.Rotation of
    DXGI_MODE_ROTATION_ROTATE90:
      begin
        Result.Left := Width - Dirty.Bottom;
        Result.Top := Dirty.Left;
        Result.Right := Width - Dirty.Top;
        Result.Bottom := Dirty.Right;
      end;
    DXGI_MODE_ROTATION_ROTATE180:
      begin
        Result.Left := Width - Dirty.Right;
        Result.Top := Height - Dirty.Bottom;
        Result.Right := Width - Dirty.Left;
        Result.Bottom := Height - Dirty.Top;
      end;
    DXGI_MODE_ROTATION_ROTATE270:
      begin
        Result.Left := Dirty.Top;
        Result.Top := Height - Dirty.Right;
        Result.Right := Dirty.Bottom;
        Result.Bottom := Height - Dirty.Left;
      end;
  end;
end;

{ TAcquireFrameRAII }

constructor TAcquireFrameRAII.Create(ADupl: IDXGIOutputDuplication);
begin
  inherited Create;
  FDuplLock := ADupl;
  FAcquiredLock := False;
end;

destructor TAcquireFrameRAII.Destroy;
begin
  TryRelease;
  inherited;
end;

procedure TAcquireFrameRAII.TryRelease;
var
  HR: HResult;
begin
  if FAcquiredLock then
  begin
    HR := FDuplLock.ReleaseFrame;
    if FAILED(HR) and (HR <> DXGI_ERROR_WAIT_TIMEOUT) then
      ProcessFailure(nil, 'Failed to release frame in DUPLICATIONMANAGER',
        'Error', HR, FrameInfoExpectedErrors);
  end;
  FAcquiredLock := False;
end;

function TAcquireFrameRAII.AcquireNextFrame(TimeoutInMilliseconds: Cardinal;
  var FrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
  var DesktopResource: IDXGIResource): HResult;
begin
  Result := FDuplLock.AcquireNextFrame(TimeoutInMilliseconds, FrameInfo,
    DesktopResource);
  TryRelease;
  FAcquiredLock := SUCCEEDED(Result);
end;

{ TMappedSubresourceRAII }

constructor TMappedSubresourceRAII.Create(AContext: ID3D11DeviceContext);
begin
  inherited Create;
  FContext := AContext;
  FResource := nil;
  FSubresource := 0;
end;

destructor TMappedSubresourceRAII.Destroy;
begin
  if FResource <> nil then
    FContext.Unmap(FResource, FSubresource);
  inherited;
end;

function TMappedSubresourceRAII.Map(Resource: ID3D11Resource;
  Subresource: Cardinal; MapType: TD3D11_MAP; MapFlags: Cardinal;
  var MappedResource: TD3D11_MAPPED_SUBRESOURCE): HResult;
begin
  if FResource <> nil then
    FContext.Unmap(FResource, FSubresource);

  FResource := Resource;
  FSubresource := Subresource;
  Result := FContext.Map(FResource, FSubresource, MapType, MapFlags,
    MappedResource);
end;

{ TDXFrameProcessor }

constructor TDXFrameProcessor.Create;
begin
  inherited;
  FUpdatedRegion := TRegion.Create;
  FHasUpdatedRegions := False;
end;

destructor TDXFrameProcessor.Destroy;
begin
  FSelectedMonitor.Free;
  FStagingSurf := nil;
  FOutputDuplication := nil;
  FDeviceContext := nil;
  FDevice := nil;
  FUpdatedRegion.Free;
  inherited;
end;

function TDXFrameProcessor.HasUpdatedRegions: Boolean;
begin
  Result := FHasUpdatedRegions and not FUpdatedRegion.IsEmpty;
end;

function TDXFrameProcessor.GetUpdatedRegions: TRegion;
begin
  Result := FUpdatedRegion;
end;

procedure TDXFrameProcessor.SetUpdatedRegions(const Region: TRegion);
begin
  FUpdatedRegion.Clear;
  for var Rect in Region do
    FUpdatedRegion.AddRect(Rect);
  FHasUpdatedRegions := True;
end;

function TDXFrameProcessor.Init(Data: TThreadData;
  Monitor: ScreenCaptureTypes.TMonitor): TDUPL_RETURN;
var
  DXResources: TDX_RESOURCES;
  DuplResources: TDUPLE_RESOURCES;
begin

  // Check if Monitor is valid
  if Monitor = nil then
    Exit(DUPL_RETURN_ERROR_UNEXPECTED);

  // Validate Monitor.Output
  if Monitor.Output < 0 then
    Exit(DUPL_RETURN_ERROR_UNEXPECTED);

  // Initialize selected monitor properties
  if FSelectedMonitor = nil then
    FSelectedMonitor := ScreenCaptureTypes.TMonitor.Create;

  FSelectedMonitor.Width := Monitor.Width;
  FSelectedMonitor.Height := Monitor.Height;
  FSelectedMonitor.OffsetX := Monitor.OffsetX;
  FSelectedMonitor.OffsetY := Monitor.OffsetY;
  FSelectedMonitor.Id := Monitor.Id;
  FSelectedMonitor.Adapter := Monitor.Adapter;
  FSelectedMonitor.Output := Monitor.Output;

  // Initialize D3D resources
  Result := Initialize(DXResources);
  if Result <> DUPL_RETURN_SUCCESS then
    Exit;

  // Initialize duplication resources
  Result := InitializeDuple(DuplResources, DXResources.Device, Monitor.Adapter,
    Monitor.Output);
  if Result <> DUPL_RETURN_SUCCESS then
  begin
    // Clean up on failure
    DXResources.Device := nil;
    DXResources.DeviceContext := nil;
    Exit;
  end;

  // Assign duplication resources
  FDevice := DXResources.Device;
  FDeviceContext := DXResources.DeviceContext;
  FOutputDuplication := DuplResources.OutputDuplication;
  FOutputDesc := DuplResources.OutputDesc;
  FOutput := DuplResources.Output;

  // Ensure data reference is stored
  FData := Data;

  Result := DUPL_RETURN_SUCCESS;
end;

function TDXFrameProcessor.ProcessFrame(const CurrentMonitorInfo: TMonitor)
  : TDUPL_RETURN;
var
  DesktopResource: IDXGIResource;
  FrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
  Frame: TAcquireFrameRAII;
  HR: HResult;
  AcquiredDesktopImage: ID3D11Texture2D;
  ThisDesc, StagingDesc: TD3D11_TEXTURE2D_DESC;
  SourceRegion: TD3D11_BOX;
  MappingDesc: TD3D11_MAPPED_SUBRESOURCE;
  MappedResource: TMappedSubresourceRAII;
  FrameRect: TFrameRect;
  FrameData: TFrame;
begin
  Result := DUPL_RETURN_SUCCESS;

  DesktopResource := nil;
  FillChar(FrameInfo, SizeOf(FrameInfo), 0);
  Frame := TAcquireFrameRAII.Create(FOutputDuplication);
  try
    HR := Frame.AcquireNextFrame(100, FrameInfo, DesktopResource);
    if HR = DXGI_ERROR_WAIT_TIMEOUT then
      Exit(DUPL_RETURN_SUCCESS)
    else if FAILED(HR) then
      Exit(ProcessFailure(FDevice, 'Failed to acquire next frame', 'Error', HR,
        FrameInfoExpectedErrors));

    if (FrameInfo.AccumulatedFrames = 0) or (DesktopResource = nil) then
      Exit(DUPL_RETURN_SUCCESS);

    HR := DesktopResource.QueryInterface(ID3D11Texture2D, AcquiredDesktopImage);
    if FAILED(HR) then
      Exit(DUPL_RETURN_ERROR_UNEXPECTED);

    if FStagingSurf = nil then
    begin
      FillChar(ThisDesc, SizeOf(ThisDesc), 0);
      AcquiredDesktopImage.GetDesc(ThisDesc);
      StagingDesc := ThisDesc;
      StagingDesc.BindFlags := 0;
      StagingDesc.Usage := D3D11_USAGE_STAGING;
      StagingDesc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_READ);
      StagingDesc.MiscFlags := 0;
      StagingDesc.Height := FSelectedMonitor.Height;
      StagingDesc.Width := FSelectedMonitor.Width;
      StagingDesc.ArraySize := 1;
      StagingDesc.MipLevels := 1;

      HR := FDevice.CreateTexture2D(@StagingDesc, nil, FStagingSurf);
      if FAILED(HR) then
        Exit(DUPL_RETURN_ERROR_UNEXPECTED);
    end;

    // Copy to staging
    FDeviceContext.CopyResource(FStagingSurf, AcquiredDesktopImage);

    // Handle either dirty regions or full screen
    if Assigned(FData.ScreenCaptureData.OnFrameChangedDXGIRegion) and
      (FrameInfo.TotalMetadataBufferSize > 0) then
    begin
      Result := ProcessChangedFrame(CurrentMonitorInfo, FrameInfo);
    end
    else if Assigned(FData.ScreenCaptureData.OnNewFrameDXGIFullScreen) then
    begin
      MappedResource := TMappedSubresourceRAII.Create(FDeviceContext);
      try
        HR := MappedResource.Map(FStagingSurf, 0, D3D11_MAP_READ, 0,
          MappingDesc);
        if FAILED(HR) then
          Exit(DUPL_RETURN_ERROR_UNEXPECTED);

        FrameRect.Left := 0;
        FrameRect.Top := 0;
        FrameRect.Right := CurrentMonitorInfo.Width;
        FrameRect.Bottom := CurrentMonitorInfo.Height;

        FrameData.Bounds := FrameRect;
        FrameData.Data := PImageBGRA(MappingDesc.pData);
        FrameData.RowStrideInBytes := MappingDesc.RowPitch;
        FrameData.IsContiguous := True;

        FData.ScreenCaptureData.OnNewFrameDXGIFullScreen(FrameData,
          CurrentMonitorInfo);
        Result := DUPL_RETURN_SUCCESS;
      finally
        MappedResource.Free;
      end;
    end;

  finally
    Frame.Free;
  end;
end;

// Ensure that the dirty regions are properly aligned with the rotated output
procedure TDXFrameProcessor.OptimizeRotatedRegions(var Region: TRegion);
var
  RotatedRegions: TArray<TRect>;
  i: Integer;
  Rect: TRect;
begin
  // Early exit if no rotation or empty region
  if (FOutputDesc.Rotation = DXGI_MODE_ROTATION_IDENTITY) or Region.IsEmpty then
    Exit;

  // Pre-allocate array with exact size needed
  SetLength(RotatedRegions, Region.Count);

  // Get all rectangles in one pass
  i := 0;
  for Rect in Region do
  begin
    RotatedRegions[i] := ConvertRect(Rect, FOutputDesc);
    Inc(i);
  end;

  // Clear and rebuild region in one pass
  Region.Clear;
  for Rect in RotatedRegions do
  begin
    Region.AddRect(Rect);
  end;
end;

function TDXFrameProcessor.ProcessDirtyRegions(const FrameInfo
  : TDXGI_OUTDUPL_FRAME_INFO; var UpdatedRegion: TRegion): Boolean;
var
  BufferSize: UINT;
  MoveRect: DX12.DXGI1_2.PDXGI_OUTDUPL_MOVE_RECT;
  DirtyRect: PRect;
  HR: HResult;
  i: Integer;
  CurMove: DX12.DXGI1_2.PDXGI_OUTDUPL_MOVE_RECT;
  CurDirty: PRect;
  AdjustedRect: TRect;
  Width, Height: Integer;
begin
  Result := False;

  // Performance optimization: If no metadata, nothing changed
  if FrameInfo.TotalMetadataBufferSize = 0 then
  begin
    Exit;
  end;

  // Resize buffer only when needed
  if Length(FMetadata.MetadataBuffer) < FrameInfo.TotalMetadataBufferSize then
    SetLength(FMetadata.MetadataBuffer, FrameInfo.TotalMetadataBufferSize);

  // Get move rectangles
  MoveRect := DX12.DXGI1_2.PDXGI_OUTDUPL_MOVE_RECT
    (@FMetadata.MetadataBuffer[0]);
  HR := FOutputDuplication.GetFrameMoveRects(FrameInfo.TotalMetadataBufferSize,
    MoveRect, BufferSize);

  if FAILED(HR) then
    Exit;

  FMetadata.MoveCount := BufferSize div SizeOf
    (DX12.DXGI1_2.TDXGI_OUTDUPL_MOVE_RECT);

  // Get dirty rectangles
  DirtyRect := PRect(PByte(MoveRect) + BufferSize);
  HR := FOutputDuplication.GetFrameDirtyRects(FrameInfo.TotalMetadataBufferSize
    - BufferSize, DirtyRect, BufferSize);

  if FAILED(HR) then
    Exit;

  FMetadata.DirtyCount := BufferSize div SizeOf(TRect);

  // Process moves - Skip unmoved regions
  CurMove := MoveRect;
  for i := 0 to FMetadata.MoveCount - 1 do
  begin
    if (CurMove.SourcePoint.X <> CurMove.DestinationRect.Left) or
      (CurMove.SourcePoint.Y <> CurMove.DestinationRect.Top) then
    begin
      UpdatedRegion.AddRect(ConvertRect(CurMove.DestinationRect, FOutputDesc));
    end;
    Inc(CurMove);
  end;

  // Process dirty rects
  CurDirty := DirtyRect;
  for i := 0 to FMetadata.DirtyCount - 1 do
  begin
    UpdatedRegion.AddRect(ConvertRect(CurDirty^, FOutputDesc));
    Inc(CurDirty);
  end;

  // After collecting all regions, optimize them for rotation
  // OptimizeRotatedRegions(UpdatedRegion);

  // Intersect with actual desktop rect to avoid out-of-bounds regions
  UpdatedRegion.IntersectWith(Rect(0, 0, FSelectedMonitor.Width,
    FSelectedMonitor.Height));

  Result := True;
end;

function TDXFrameProcessor.ProcessChangedFrame(const CurrentMonitor: TMonitor;
  const FrameInfo: TDXGI_OUTDUPL_FRAME_INFO): TDUPL_RETURN;
var
  UpdatedRegion: TRegion;
  MappingDesc: TD3D11_MAPPED_SUBRESOURCE;
  MappedResource: TMappedSubresourceRAII;
  HR: HResult;
  RegionRect: TRect;
  Frame: TFrame;
  RegionIndex: Integer;
  RegionCount: Integer;
  Rects: TArray<TRect>;
begin
  Result := DUPL_RETURN_SUCCESS;

  FUpdatedRegion.Clear;
  FHasUpdatedRegions := False;

  UpdatedRegion := TRegion.Create;
  try
    if not ProcessDirtyRegions(FrameInfo, UpdatedRegion) then
      Exit(DUPL_RETURN_ERROR_UNEXPECTED);

    if UpdatedRegion.IsEmpty then
      Exit(DUPL_RETURN_SUCCESS);

    // Map the staging surface
    MappedResource := TMappedSubresourceRAII.Create(FDeviceContext);
    try
      HR := MappedResource.Map(FStagingSurf, 0, D3D11_MAP_READ, 0, MappingDesc);
      if FAILED(HR) then
        Exit(DUPL_RETURN_ERROR_UNEXPECTED);

      // Get array of rectangles from region
      Rects := UpdatedRegion.GetRectangles;
      RegionCount := Length(Rects);

      // Process each dirty rectangle
      for RegionIndex := 0 to RegionCount - 1 do
      begin
        RegionRect := Rects[RegionIndex];

        // Ensure the region is within bounds
        if (RegionRect.Left < 0) or (RegionRect.Top < 0) or
          (RegionRect.Right > FSelectedMonitor.Width) or
          (RegionRect.Bottom > FSelectedMonitor.Height) then
          Continue;

        // Add to FUpdatedRegion for later use
        FUpdatedRegion.AddRect(RegionRect);

        // Create frame info for this region...
        Frame.Bounds.Left := RegionRect.Left;
        Frame.Bounds.Top := RegionRect.Top;
        Frame.Bounds.Right := RegionRect.Right;
        Frame.Bounds.Bottom := RegionRect.Bottom;

        Frame.Data := PImageBGRA(PByte(MappingDesc.pData) +
          (RegionRect.Top * MappingDesc.RowPitch) +
          (RegionRect.Left * SizeOf(TFrameBGRA)));

        Frame.RowStrideInBytes := MappingDesc.RowPitch;
        Frame.IsContiguous := RegionRect.Width * SizeOf(TFrameBGRA)
          = MappingDesc.RowPitch;

        if Assigned(FData.ScreenCaptureData.OnFrameChangedDXGIRegion) then
          FData.ScreenCaptureData.OnFrameChangedDXGIRegion(Frame,
            CurrentMonitor);
      end;

      FHasUpdatedRegions := not FUpdatedRegion.IsEmpty;

    finally
      MappedResource.Free;
    end;

  finally
    UpdatedRegion.Free;
  end;
end;

end.
