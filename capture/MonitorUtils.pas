unit MonitorUtils;

// Monitor utilities

interface

uses
  Winapi.Windows, System.SysUtils, System.Generics.Collections, System.Math,
  DX12.D3D11, DX12.DXGI, DX12.D3DCommon,ScreenCaptureTypes;

function GetMonitors: TList<ScreenCaptureTypes.TMonitor>;

implementation

function ScaleFromDPI(xdpi: Integer): Single;
begin
  case xdpi of
    96: Result := 1.0;
    120: Result := 1.25;
    144: Result := 1.5;
    192: Result := 2.0;
  else
    Result := 1.0;
  end;
end;

function GetMonitors: TList<ScreenCaptureTypes.TMonitor>;
var
  Factory: IDXGIFactory;
  Adapter: IDXGIAdapter;
  Output: IDXGIOutput;
  OutputDesc: TDXGI_OUTPUT_DESC;
  DevMode: TDeviceMode;
  MonitorDC: HDC;
  XDPI: Integer;
  Scale: Single;
  FlipSides: Boolean;
  AdapterIndex, OutputIndex: Integer;
  DeviceName: WideString;
  MonitorName: string;
  Monitor: ScreenCaptureTypes.TMonitor;
begin
  Result := TList<ScreenCaptureTypes.TMonitor>.Create;
  try
    // Create DXGI Factory
    if Succeeded(CreateDXGIFactory(IDXGIFactory, Factory)) then
    begin
      AdapterIndex := 0;
      while Factory.EnumAdapters(AdapterIndex, Adapter) <> DXGI_ERROR_NOT_FOUND do
      begin
        OutputIndex := 0;
        while Adapter.EnumOutputs(OutputIndex, Output) <> DXGI_ERROR_NOT_FOUND do
        begin
          if Succeeded(Output.GetDesc(OutputDesc)) then
          begin
            DeviceName := OutputDesc.DeviceName;
            // Simply use the device name directly without UTF-8 conversion
            MonitorName := string(DeviceName);

            // Initialize DevMode
            ZeroMemory(@DevMode, SizeOf(TDeviceMode));
            DevMode.dmSize := SizeOf(TDeviceMode);
            EnumDisplaySettingsW(PWideChar(DeviceName), DWORD(-1), DevMode);

            // Retrieve DPI scaling
            MonitorDC := CreateDCW(PWideChar(DeviceName), nil, nil, nil);
            try
              XDPI := GetDeviceCaps(MonitorDC, LOGPIXELSX);
              Scale := ScaleFromDPI(XDPI);
            finally
              DeleteDC(MonitorDC);
            end;

            // Determine rotation
            FlipSides := (OutputDesc.Rotation = DXGI_MODE_ROTATION_ROTATE90) or
                         (OutputDesc.Rotation = DXGI_MODE_ROTATION_ROTATE180) or
                         (OutputDesc.Rotation = DXGI_MODE_ROTATION_ROTATE270);

            // Create and populate TMonitor
            Monitor := ScreenCaptureTypes.TMonitor.Create;
            try
              Monitor.Id := Result.Count;
              Monitor.Output := OutputIndex;
              Monitor.Adapter := AdapterIndex;
              Monitor.Width := OutputDesc.DesktopCoordinates.Right - OutputDesc.DesktopCoordinates.Left;
              Monitor.Height := OutputDesc.DesktopCoordinates.Bottom - OutputDesc.DesktopCoordinates.Top;
              Monitor.OffsetX := OutputDesc.DesktopCoordinates.Left;
              Monitor.OffsetY := OutputDesc.DesktopCoordinates.Top;
              Monitor.Name := MonitorName;
              Monitor.Scaling := Scale;
              // Add monitor to the list
              Result.Add(Monitor);
            except
              Monitor.Free;
              raise;
            end;
          end;
          Output := nil; // Release output
          Inc(OutputIndex);
        end;
        Adapter := nil; // Release adapter
        Inc(AdapterIndex);
      end;
    end;
  except
    // Free the result list if an error occurs
    Result.Free;
    raise;
  end;
end;

end.

