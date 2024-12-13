unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  DXGIFrameProcessor, GDIFrameProcessor, MonitorUtils, ScreenCaptureTypes,
  Vcl.ExtCtrls;

type
  PImageBGRAArray = ^TImageBGRAArray;
  TImageBGRAArray = array [0 .. MaxInt div SizeOf(TFrameBGRA) - 1]
    of TFrameBGRA;

type
  TForm1 = class(TForm)
    messagesLog: TMemo;
    btnStartDXGI: TButton;
    btnStopDXGI: TButton;
    btnStartGDIDirty: TButton;
    btnStartGDI: TButton;
    btnStopGDI: TButton;
    Image1: TImage;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    btnStartDXGIDirty: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnStartDXGIClick(Sender: TObject);
    procedure btnStopDXGIClick(Sender: TObject);
    procedure btnStartGDIClick(Sender: TObject);
    procedure btnStartGDIDirtyClick(Sender: TObject);
    procedure btnStopGDIClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure btnStartDXGIDirtyClick(Sender: TObject);
  private
    DXGIProcessor: TDXFrameProcessor;
    GDIProcessor: TGDIFrameProcessor;
    ThreadData: TThreadData;
    Monitors: TList<ScreenCaptureTypes.TMonitor>;
    SelectedMonitor: ScreenCaptureTypes.TMonitor;
    Bitmap: TBitmap;
    CaptureActive: Boolean;
    FrameCount: Integer;
    FDesktopBitmap: TBitmap;
    FirstFrame: Boolean;

    procedure OnNewFrameDXGIFullScreen(const Frame: TFrame;
      const Monitor: TMonitor);
    procedure OnFrameChangedDXGIRegion(const Frame: TFrame;
      const Monitor: TMonitor);
    procedure OnNewFrameGDIFullScreen(const Frame: TFrame;
      const Monitor: TMonitor);
    procedure OnFrameChangedGDIRegion(const Frame: TFrame;
      const Monitor: TMonitor);
  public
    { Public declarations }
    procedure Display(p_sender: String; p_message: string);
    function GetNow(): String;
    function GetSelectedMonitor: ScreenCaptureTypes.TMonitor;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Display(p_sender: String; p_message: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      messagesLog.Lines.Add('[' + p_sender + '] - ' + GetNow() + ': ' +
        p_message);
    end);
end;

function TForm1.GetNow(): String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;

procedure TForm1.FormShow(Sender: TObject);
begin

  FDesktopBitmap := TBitmap.Create;

  CaptureActive := False;

  btnStopDXGI.Enabled := False;
  btnStopGDI.Enabled := False;

  messagesLog.clear;

    // Get monitors and populate ComboBox
  Monitors := GetMonitors;
  ComboBox1.Clear;

  if Monitors.Count = 0 then
    begin
      Display('Monitor', 'No monitors found.');
      btnStartDXGI.Enabled := False;
      btnStopDXGI.Enabled := False;
      btnStartGDIDirty.Enabled := False;
      btnStartGDI.Enabled := False;
      btnStopGDI.Enabled := False;
      Exit;
    end;

  // Populate ComboBox with monitor information
  for var Monitor in Monitors do
  begin
    ComboBox1.Items.AddObject(
      Format('Monitor %d (%s) - %dx%d', [
        Monitor.Id,
        Monitor.Name,
        Monitor.Width,
        Monitor.Height
      ]),
      TObject(Monitor.Id)
    );

  end;

  // Select first monitor by default if available
  if ComboBox1.Items.Count > 0 then
  begin
    ComboBox1.ItemIndex := 0;
    SelectedMonitor := Monitors[0];
  end;

    Display('Selected Monitor', Format('ID: %d', [Monitors[0].Id]));
    Display('Selected Monitor', Format('Name: %s', [Monitors[0].Name]));
    Display('Selected Monitor', Format('Resolution: %dx%d',
      [Monitors[0].Width, Monitors[0].Height]));
    Display('Selected Monitor', Format('Position: (%d,%d)',
      [Monitors[0].OffsetX, Monitors[0].OffsetY]));
    Display('Selected Monitor', Format('Output: %d', [Monitors[0].Output]));
    Display('Selected Monitor', Format('Scaling: %.2f', [Monitors[0].Scaling]));
end;


// Helper function to get the currently selected monitor
function TForm1.GetSelectedMonitor: ScreenCaptureTypes.TMonitor;
var
  SelectedMonitorId: Integer;
begin
  Result := nil; // Default return value if nothing is found

  // Check if we have a valid selection and monitors exist
  if (ComboBox1.ItemIndex >= 0) and (Monitors <> nil) then
  begin
    // Get the monitor ID we stored in the ComboBox's Objects property
    SelectedMonitorId := Integer(ComboBox1.Items.Objects[ComboBox1.ItemIndex]);

    // Look for the monitor with matching ID in our Monitors list
    for var Monitor in Monitors do
    begin
      if Monitor.Id = SelectedMonitorId then
      begin
        Result := Monitor; // Found the matching monitor
        Exit;
      end;
    end;
  end;
end;

// ComboBox change event handler
procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  // Get the selected monitor using our helper function
  SelectedMonitor := GetSelectedMonitor;

  // If we found a monitor, display its information
  if SelectedMonitor <> nil then
  begin
    messagesLog.Clear;
    Display('Selected Monitor', Format('ID: %d', [SelectedMonitor.Id]));
    Display('Selected Monitor', Format('Name: %s', [SelectedMonitor.Name]));
    Display('Selected Monitor', Format('Resolution: %dx%d',
      [SelectedMonitor.Width, SelectedMonitor.Height]));
    Display('Selected Monitor', Format('Position: (%d,%d)',
      [SelectedMonitor.OffsetX, SelectedMonitor.OffsetY]));
    Display('Selected Monitor', Format('Output: %d', [SelectedMonitor.Output]));
    Display('Selected Monitor', Format('Scaling: %.2f', [SelectedMonitor.Scaling]));
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Stop the screen capture process
  if CaptureActive then
  begin
    CaptureActive := False;

    // Wait for the capture loop to finish
    while CaptureActive do
      Application.ProcessMessages;

    // Free Processor
    if DXGIProcessor <> nil then
    begin
      DXGIProcessor.Free;
      DXGIProcessor := nil;
    end;

    if GDIProcessor <> nil then
    begin
      GDIProcessor.Free;
      GDIProcessor := nil;
    end;
  end;

  // Free other resources
  if Monitors <> nil then
  begin
    for var Monitor in Monitors do
      Monitor.Free;
    Monitors.Free;
  end;

  if ThreadData <> nil then
    ThreadData.Free;

    FreeAndNil(FDesktopBitmap);

  Action := caFree;
end;


procedure TForm1.btnStartDXGIClick(Sender: TObject);
begin
  messagesLog.clear;

  btnStopGDI.Enabled := false;
  btnStartGDI.Enabled := false;
  btnStartGDIDirty.Enabled := false;
  btnStartDXGI.Enabled := false;
  btnStartDXGIDirty.Enabled := false;
  btnStopDXGI.Enabled := true;

  if not CaptureActive then
  begin
    // Initialize the DXGIProcessor if it hasn't been done yet
    if DXGIProcessor = nil then
    begin
      FrameCount := 0;
      ThreadData := TThreadData.Create;
      ThreadData.ScreenCaptureData.OnNewFrameDXGIFullScreen :=
        OnNewFrameDXGIFullScreen;

      // Get the selected monitor
      SelectedMonitor := GetSelectedMonitor;
      if SelectedMonitor = nil then
      begin
        Display('Error', 'No monitor selected');
        Exit;
      end;

      if Monitors.Count = 0 then
      begin
        Display('Error', 'No monitors found.');
        Exit;
      end;

      DXGIProcessor := TDXFrameProcessor.Create;
      if DXGIProcessor.Init(ThreadData, SelectedMonitor) <> DUPL_RETURN_SUCCESS then
      begin
        Display('Error',
          'Failed to initialize the frame processor.');
        Exit;
      end;
    end;

    CaptureActive := True;

    // Start DXGI capture loop
    while CaptureActive do
    begin
      DXGIProcessor.ProcessFrame(SelectedMonitor);
      Application.ProcessMessages;
    end;

  end;
end;


procedure TForm1.btnStartDXGIDirtyClick(Sender: TObject);
begin
  messagesLog.Clear;

  btnStopGDI.Enabled := false;
  btnStartGDI.Enabled := false;
  btnStartGDIDirty.Enabled := false;
  btnStartDXGI.Enabled := false;
  btnStartDXGIDirty.Enabled := false;
  btnStopDXGI.Enabled := true;

  if not CaptureActive then
  begin
    // Initialize the DXGIProcessor if it hasn't been done yet
    if DXGIProcessor = nil then
    begin
      FrameCount := 0;
      ThreadData := TThreadData.Create;

      // Track if it's the first frame (initial full-screen capture)
      FirstFrame := True;

      // Use the full-screen event on the first frame
      ThreadData.ScreenCaptureData.OnNewFrameDXGIFullScreen := OnNewFrameDXGIFullScreen;
      ThreadData.ScreenCaptureData.OnFrameChangedDXGIRegion := OnFrameChangedDXGIRegion;

      // Get the selected monitor
      SelectedMonitor := GetSelectedMonitor;
      if SelectedMonitor = nil then
      begin
        Display('Error', 'No monitor selected');
        Exit;
      end;

      DXGIProcessor := TDXFrameProcessor.Create;
      if DXGIProcessor.Init(ThreadData, SelectedMonitor) <> DUPL_RETURN_SUCCESS then
      begin
        Display('Error', 'Failed to initialize the frame processor.');
        Exit;
      end;
    end;

    CaptureActive := True;

    // Start DXGI capture loop
    while CaptureActive do
    begin
      // If it's the first frame, use full screen
      if FirstFrame then
      begin
        // Call the full screen frame capture function here
        DXGIProcessor.ProcessFrame(SelectedMonitor);

        // Once the first frame is processed, switch to region update
        FirstFrame := False;
        // Switch the event handler to update only changed regions
        ThreadData.ScreenCaptureData.OnNewFrameDXGIFullScreen := nil;
        ThreadData.ScreenCaptureData.OnFrameChangedDXGIRegion := OnFrameChangedDXGIRegion;
      end
      else
      begin
        // After the first frame, only process changed regions
        DXGIProcessor.ProcessFrame(SelectedMonitor);
      end;

      Application.ProcessMessages;
    end;
  end;
end;


procedure TForm1.btnStartGDIClick(Sender: TObject);
begin
  messagesLog.clear;

  btnStopGDI.Enabled := true;
  btnStartGDI.Enabled := false;
  btnStartGDIDirty.Enabled := false;
  btnStartDXGI.Enabled := false;
  btnStartDXGIDirty.Enabled := false;
  btnStopDXGI.Enabled := false;

  if not CaptureActive then
  begin
    // Initialize the GDI processor if it hasn't been done yet
    if GDIProcessor = nil then
    begin
      FrameCount := 0;
      ThreadData := TThreadData.Create;

      // Track if it's the first frame (initial full-screen capture)
      FirstFrame := True;

      // Use the full-screen event on the first frame
      ThreadData.ScreenCaptureData.OnNewFrameGDIFullScreen := OnNewFrameGDIFullScreen;
      ThreadData.ScreenCaptureData.OnFrameChangedGDIRegion := OnFrameChangedGDIRegion;

      // Get the selected monitor
      SelectedMonitor := GetSelectedMonitor;
      if SelectedMonitor = nil then
      begin
        Display('Error', 'No monitor selected');
        Exit;
      end;

      GDIProcessor := TGDIFrameProcessor.Create;
      if GDIProcessor.Init(ThreadData, SelectedMonitor) <> DUPL_RETURN_SUCCESS then
      begin
        Display('Error', 'Failed to initialize the GDI processor.');
        Exit;
      end;
    end;

    CaptureActive := True;
    // Start GDI capture loop
    while CaptureActive do
    begin
      // If it's the first frame, use full screen
      if FirstFrame then
      begin
        GDIProcessor.ProcessFrame(SelectedMonitor);

        // Once the first frame is processed, switch to region update
        FirstFrame := False;
        // Switch the event handler to update only changed regions
        ThreadData.ScreenCaptureData.OnNewFrameGDIFullScreen := nil;
        ThreadData.ScreenCaptureData.OnFrameChangedGDIRegion := OnFrameChangedGDIRegion;
      end
      else
      begin
        // After the first frame, only process changed regions
        GDIProcessor.ProcessFrame(SelectedMonitor);
      end;

      Application.ProcessMessages;
    end;
  end;
end;

procedure TForm1.btnStartGDIDirtyClick(Sender: TObject);
begin
  messagesLog.clear;

  btnStopGDI.Enabled := true;
  btnStartGDI.Enabled := false;
  btnStartGDIDirty.Enabled := false;
  btnStartDXGI.Enabled := false;
  btnStartDXGIDirty.Enabled := false;
  btnStopDXGI.Enabled := false;

  if not CaptureActive then
  begin
    // Initialize the GDIProcessor if it hasn't been done yet
    if GDIProcessor  = nil then
    begin
      FrameCount := 0;
      ThreadData := TThreadData.Create;
      ThreadData.ScreenCaptureData.OnFrameChangedGDIRegion :=
        OnFrameChangedGDIRegion;

      // Get the selected monitor
      SelectedMonitor := GetSelectedMonitor;
      if SelectedMonitor = nil then
      begin
        Display('Error', 'No monitor selected');
        Exit;
      end;

      GDIProcessor := TGDIFrameProcessor.Create;
      if GDIProcessor.Init(ThreadData, SelectedMonitor) <> DUPL_RETURN_SUCCESS then
      begin
        Display('Error',
          'Failed to initialize the frame processor.');
        Exit;
      end;
    end;

    CaptureActive := True;

    // Start DXGI capture loop
    while CaptureActive do
    begin
      GDIProcessor.ProcessFrame(SelectedMonitor);
      Application.ProcessMessages;
    end;
  end;
end;

procedure TForm1.btnStopDXGIClick(Sender: TObject);
begin

  btnStopGDI.Enabled := false;
  btnStartGDI.Enabled := true;
  btnStartGDIDirty.Enabled := true;
  btnStartDXGI.Enabled := true;
  btnStartDXGIDirty.Enabled := true;
  btnStopDXGI.Enabled := false;

  if CaptureActive then
  begin
    CaptureActive := False;
  end;

  // Wait for the capture loop to finish
  while CaptureActive do
    Application.ProcessMessages;

  if DXGIProcessor <> nil then
  begin
    // Destroy the DXGIProcessor
    DXGIProcessor.Destroy;
    DXGIProcessor := nil;
  end;
end;

procedure TForm1.btnStopGDIClick(Sender: TObject);
begin

  btnStopGDI.Enabled := false;
  btnStartGDI.Enabled := true;
  btnStartGDIDirty.Enabled := true;
  btnStartDXGI.Enabled := true;
  btnStartDXGIDirty.Enabled := true;
  btnStopDXGI.Enabled := false;

  if CaptureActive then
    CaptureActive := False;

  // Wait for capture loop to finish
  while CaptureActive do
    Application.ProcessMessages;

  if GDIProcessor <> nil then
  begin
    GDIProcessor.Free;
    GDIProcessor := nil;
  end;
end;

procedure TForm1.OnNewFrameGDIFullScreen(const Frame: TFrame; const Monitor: TMonitor);
var
  Y: Integer;
  FrameBitmap: TBitmap;
  DstRow: PByte;
  SrcRow: PByte;
  RowWidth: Integer;
begin
  FrameBitmap := TBitmap.Create;
  try
    FrameBitmap.SetSize(Frame.Bounds.Right - Frame.Bounds.Left, Frame.Bounds.Bottom - Frame.Bounds.Top);
    FrameBitmap.PixelFormat := pf32bit;

    // Log the frame details
    Display('GDI Full Screen', Format('Left=%d Top=%d Right=%d Bottom=%d',
      [Frame.Bounds.Left, Frame.Bounds.Top, Frame.Bounds.Right, Frame.Bounds.Bottom]));

    // Fast row copy
    RowWidth := (Frame.Bounds.Right - Frame.Bounds.Left) * SizeOf(TFrameBGRA);
    for Y := 0 to FrameBitmap.Height - 1 do
    begin
      DstRow := FrameBitmap.ScanLine[Y];
      SrcRow := PByte(Frame.Data) + (Y * Frame.RowStrideInBytes);
      Move(SrcRow^, DstRow^, RowWidth);
    end;

    // Save the frame to file (optional)
    // FrameBitmap.SaveToFile(Format('D:\Capture\GDIFrame_%d_FullScreen_%dx%d.bmp', [FrameCount, Frame.Bounds.Right - Frame.Bounds.Left, Frame.Bounds.Bottom - Frame.Bounds.Top]));
    // Inc(FrameCount);

    // Draw the full frame to the screen
    Image1.Picture.Bitmap.Assign(FrameBitmap);

    // Ensure desktop bitmap is correctly sized
    if (FDesktopBitmap.Width <> SelectedMonitor.Width) or
       (FDesktopBitmap.Height <> SelectedMonitor.Height) then
    begin
      FDesktopBitmap.SetSize(SelectedMonitor.Width, SelectedMonitor.Height);
    end;

    // Log the desktop bitmap update
    Display('GDI Full Screen', Format('Drawing full frame to desktop bitmap: Width=%d, Height=%d',
      [FDesktopBitmap.Width, FDesktopBitmap.Height]));

    // Draw the entire screen to the desktop bitmap
    FDesktopBitmap.Canvas.Lock;
    try
      FDesktopBitmap.Canvas.Draw(0, 0, FrameBitmap);
    finally
      FDesktopBitmap.Canvas.Unlock;
    end;

  finally
    FrameBitmap.Free;
  end;
end;

procedure TForm1.OnFrameChangedGDIRegion(const Frame: TFrame; const Monitor: TMonitor);
var
  Y: Integer;
  FrameBitmap: TBitmap;
  DstRow: PByte;
  SrcRow: PByte;
  RowWidth: Integer;
begin
  FrameBitmap := TBitmap.Create;
  try
    FrameBitmap.SetSize(Frame.Bounds.Right - Frame.Bounds.Left, Frame.Bounds.Bottom - Frame.Bounds.Top);
    FrameBitmap.PixelFormat := pf32bit;

    // Log the frame update for changed regions
    Display('GDI Region', Format('Left=%d Top=%d Right=%d Bottom=%d',
      [Frame.Bounds.Left, Frame.Bounds.Top, Frame.Bounds.Right, Frame.Bounds.Bottom]));

    // Fast row copy
    RowWidth := (Frame.Bounds.Right - Frame.Bounds.Left) * SizeOf(TFrameBGRA);
    for Y := 0 to FrameBitmap.Height - 1 do
    begin
      DstRow := FrameBitmap.ScanLine[Y];
      SrcRow := PByte(Frame.Data) + (Y * Frame.RowStrideInBytes);
      Move(SrcRow^, DstRow^, RowWidth);
    end;

    // Save the frame to file (optional)
    // FrameBitmap.SaveToFile(Format('D:\Capture\GDIFrame_%d_Region_%dx%d_at_%d_%d.bmp',
    // [FrameCount, Frame.Bounds.Right - Frame.Bounds.Left, Frame.Bounds.Bottom - Frame.Bounds.Top, Frame.Bounds.Left, Frame.Bounds.Top]));
    // Inc(FrameCount);

    // Ensure desktop bitmap is correctly sized
    if (FDesktopBitmap.Width <> SelectedMonitor.Width) or
       (FDesktopBitmap.Height <> SelectedMonitor.Height) then
    begin
      FDesktopBitmap.SetSize(SelectedMonitor.Width, SelectedMonitor.Height);
    end;

    // Log the desktop bitmap update
    Display('GDI Region', Format('Updating dirty region on desktop bitmap: Width=%d, Height=%d',
      [FDesktopBitmap.Width, FDesktopBitmap.Height]));

    // Update only the changed region on top of the full screen
    FDesktopBitmap.Canvas.Lock;
    try
      // Copy the dirty region to the full-screen bitmap
      FDesktopBitmap.Canvas.Draw(Frame.Bounds.Left, Frame.Bounds.Top, FrameBitmap);
    finally
      FDesktopBitmap.Canvas.Unlock;
    end;

    // Assign the updated full screen with the updated region to the image control
    Image1.Picture.Bitmap.Assign(FDesktopBitmap);

  finally
    FrameBitmap.Free;
  end;
end;

procedure TForm1.OnNewFrameDXGIFullScreen(const Frame: TFrame; const Monitor: TMonitor);
var
  Y: Integer;
  FrameBitmap: TBitmap;
  DstRow: PByte;
  SrcRow: PByte;
  RowWidth: Integer;
begin
  FrameBitmap := TBitmap.Create;
  try
    FrameBitmap.SetSize(Frame.Bounds.Right - Frame.Bounds.Left, Frame.Bounds.Bottom - Frame.Bounds.Top);
    FrameBitmap.PixelFormat := pf32bit;

    // Log the frame details
    Display('DXGI Full Screen', Format('Left=%d Top=%d Right=%d Bottom=%d',
      [Frame.Bounds.Left, Frame.Bounds.Top, Frame.Bounds.Right, Frame.Bounds.Bottom]));

    // Fast row copy
    RowWidth := (Frame.Bounds.Right - Frame.Bounds.Left) * SizeOf(TFrameBGRA);
    for Y := 0 to FrameBitmap.Height - 1 do
    begin
      DstRow := FrameBitmap.ScanLine[Y];
      SrcRow := PByte(Frame.Data) + (Y * Frame.RowStrideInBytes);
      Move(SrcRow^, DstRow^, RowWidth);
    end;

    // Save the frame to file (optional)
    // FrameBitmap.SaveToFile(Format('D:\Capture\DXGIFrame_%d_FullScreen_%dx%d.bmp',
    // [FrameCount, Frame.Bounds.Right - Frame.Bounds.Left, Frame.Bounds.Bottom - Frame.Bounds.Top]));
    // Inc(FrameCount);

    // Draw the full screen frame
    Image1.Picture.Bitmap.Assign(FrameBitmap);

    // Ensure desktop bitmap is correctly sized
    if (FDesktopBitmap.Width <> SelectedMonitor.Width) or
       (FDesktopBitmap.Height <> SelectedMonitor.Height) then
    begin
      FDesktopBitmap.SetSize(SelectedMonitor.Width, SelectedMonitor.Height);
    end;

    // Log the desktop bitmap update
    Display('DXGI Full Screen', Format('Drawing full frame to desktop bitmap: Width=%d, Height=%d',
      [FDesktopBitmap.Width, FDesktopBitmap.Height]));

    // Draw the entire screen to the desktop bitmap
    FDesktopBitmap.Canvas.Lock;
    try
      FDesktopBitmap.Canvas.Draw(0, 0, FrameBitmap);
    finally
      FDesktopBitmap.Canvas.Unlock;
    end;

  finally
    FrameBitmap.Free;
  end;
end;

procedure TForm1.OnFrameChangedDXGIRegion(const Frame: TFrame; const Monitor: TMonitor);
var
  Y: Integer;
  FrameBitmap: TBitmap;
  DstRow: PByte;
  SrcRow: PByte;
  RowWidth: Integer;
begin
  FrameBitmap := TBitmap.Create;
  try
    FrameBitmap.SetSize(Frame.Bounds.Right - Frame.Bounds.Left, Frame.Bounds.Bottom - Frame.Bounds.Top);
    FrameBitmap.PixelFormat := pf32bit;

    // Log the frame update for changed regions
    Display('DXGI Region', Format('Left=%d Top=%d Right=%d Bottom=%d',
      [Frame.Bounds.Left, Frame.Bounds.Top, Frame.Bounds.Right, Frame.Bounds.Bottom]));

    // Fast row copy
    RowWidth := (Frame.Bounds.Right - Frame.Bounds.Left) * SizeOf(TFrameBGRA);
    for Y := 0 to FrameBitmap.Height - 1 do
    begin
      DstRow := FrameBitmap.ScanLine[Y];
      SrcRow := PByte(Frame.Data) + (Y * Frame.RowStrideInBytes);
      Move(SrcRow^, DstRow^, RowWidth);
    end;

    // Save the frame to file (optional)
    // FrameBitmap.SaveToFile(Format('D:\Capture\DXGIFrame_%d_Region_%dx%d_at_%d_%d.bmp',
    // [FrameCount, Frame.Bounds.Right - Frame.Bounds.Left, Frame.Bounds.Bottom - Frame.Bounds.Top, Frame.Bounds.Left, Frame.Bounds.Top]));
    // Inc(FrameCount);

    // Ensure desktop bitmap is correctly sized
    if (FDesktopBitmap.Width <> SelectedMonitor.Width) or
       (FDesktopBitmap.Height <> SelectedMonitor.Height) then
    begin
      FDesktopBitmap.SetSize(SelectedMonitor.Width, SelectedMonitor.Height);
    end;

    // Log the desktop bitmap update
    Display('DXGI Region', Format('Updating dirty region on desktop bitmap: Width=%d, Height=%d',
      [FDesktopBitmap.Width, FDesktopBitmap.Height]));

    // Update the bitmap
    FDesktopBitmap.Canvas.Lock;
    try
      FDesktopBitmap.Canvas.Draw(Frame.Bounds.Left, Frame.Bounds.Top, FrameBitmap);
    finally
      FDesktopBitmap.Canvas.Unlock;
    end;

    // Assign the updated full screen with the updated region to the image control
    Image1.Picture.Bitmap.Assign(FDesktopBitmap);

  finally
    FrameBitmap.Free;
  end;
end;


end.
