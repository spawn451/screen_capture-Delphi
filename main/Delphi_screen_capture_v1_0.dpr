program Delphi_screen_capture_v1_0;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  DX12.D3D11 in '..\DXHeaders\DX12.D3D11.pas',
  DX12.D3DCommon in '..\DXHeaders\DX12.D3DCommon.pas',
  DX12.DCommon in '..\DXHeaders\DX12.DCommon.pas',
  DX12.DXGI in '..\DXHeaders\DX12.DXGI.pas',
  DX12.DXGI1_2 in '..\DXHeaders\DX12.DXGI1_2.pas',
  Logging in '..\log\Logging.pas',
  DXGIFrameProcessor in '..\capture\DXGIFrameProcessor.pas',
  ScreenCapture in '..\capture\ScreenCapture.pas',
  ScreenCaptureUtils in '..\capture\ScreenCaptureUtils.pas',
  ScreenCaptureTypes in '..\capture\ScreenCaptureTypes.pas',
  MonitorUtils in '..\capture\MonitorUtils.pas',
  GDIFrameProcessor in '..\capture\GDIFrameProcessor.pas',
  GDIHelpers in '..\capture\GDIHelpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


