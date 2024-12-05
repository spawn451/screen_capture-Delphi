unit ScreenCapture;

// Processing logic

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math,
  ScreenCaptureTypes, ScreenCaptureUtils, DXGIFrameProcessor;

function CreateImage(const ImgRect: TFrameRect; RowStrideInBytes: Integer;
  Data: PImageBGRA): TFrame;
procedure ProcessCapture(const Data: TCaptureData; Base: TBaseFrameProcessor;
  const Monitor: TMonitor; const StartSrc: PByte; SrcRowStride: Integer);

implementation

function CreateImage(const ImgRect: TFrameRect; RowStrideInBytes: Integer;
  Data: PImageBGRA): TFrame;
begin
  Result.Bounds := ImgRect;
  Result.Data := Data;
  Result.RowStrideInBytes := RowStrideInBytes;
  Result.IsContiguous := RowStrideInBytes = SizeOf(TFrameBGRA) *
    (ImgRect.Right - ImgRect.Left);
end;

procedure ProcessCapture(const Data: TCaptureData; Base: TBaseFrameProcessor;
  const Monitor: TMonitor; const StartSrc: PByte; SrcRowStride: Integer);
var
  ImageRect: TFrameRect;
  SizeOfImgBGRA: Integer;
  StartImgSrc: PImageBGRA;
  DstRowStride: Integer;
  WholeImg, NewImg, OldImg: TFrame;
  ImgDifs: TArray<TFrameRect>;
  LeftOffset: Integer;
  ThisStartSrc: PByte;
  DifImg: TFrame;
  StartDst: PByte;
  I: Integer;
  UpdatedRegion: TRegion;
  RegionRect: TRect;
  Frame: TFrame;
  FrameRect: TFrameRect;
begin
  ImageRect.Left := 0;
  ImageRect.Top := 0;
  ImageRect.Bottom := Monitor.Height;
  ImageRect.Right := Monitor.Width;
  SizeOfImgBGRA := SizeOf(TFrameBGRA);
  StartImgSrc := PImageBGRA(StartSrc);
  DstRowStride := SizeOfImgBGRA * Monitor.Width;

  // Handle DXGI full screen
  if Assigned(Data.OnNewFrameDXGIFullScreen) then
  begin
    WholeImg := CreateImage(ImageRect, SrcRowStride, StartImgSrc);
    WholeImg.IsContiguous := DstRowStride = SrcRowStride;
    Data.OnNewFrameDXGIFullScreen(WholeImg, Monitor);
  end;

  // Handle DXGI region changes
  if Assigned(Data.OnFrameChangedDXGIRegion) then
  begin
    // If Base is TDXFrameProcessor and has updated regions
    if (Base is TDXFrameProcessor) and
      (TDXFrameProcessor(Base).HasUpdatedRegions) then
    begin
      UpdatedRegion := TDXFrameProcessor(Base).GetUpdatedRegions;
      if not UpdatedRegion.IsEmpty then
      begin
        for RegionRect in UpdatedRegion do
        begin
          // Calculate pointer to start of region data
          LeftOffset := RegionRect.Left * SizeOfImgBGRA;
          ThisStartSrc := StartSrc + LeftOffset +
            (RegionRect.Top * SrcRowStride);

          // Convert TRect to TFrameRect
          FrameRect.Left := RegionRect.Left;
          FrameRect.Top := RegionRect.Top;
          FrameRect.Right := RegionRect.Right;
          FrameRect.Bottom := RegionRect.Bottom;

          // Create frame
          Frame := CreateImage(FrameRect, SrcRowStride, StartImgSrc);

          // Notify about the changed region
          Data.OnFrameChangedDXGIRegion(Frame, Monitor);
        end;
      end;
    end;
  end;

  // Handle GDI fullscreen
  if Assigned(Data.OnNewFrameGDIFullScreen) then
  begin
    WholeImg := CreateImage(ImageRect, SrcRowStride, StartImgSrc);
    WholeImg.IsContiguous := DstRowStride = SrcRowStride;
    Data.OnNewFrameGDIFullScreen(WholeImg, Monitor);
  end;

  // Handle GDI region changes
  if Assigned(Data.OnFrameChangedGDIRegion) then
  begin
    if Base.FirstRun then
    begin
      WholeImg := CreateImage(ImageRect, SrcRowStride, StartImgSrc);
      WholeImg.IsContiguous := DstRowStride = SrcRowStride;
      Data.OnFrameChangedGDIRegion(WholeImg, Monitor);
      Base.FirstRun := False;
    end
    else
    begin
      NewImg := CreateImage(ImageRect, SrcRowStride, StartImgSrc);
      OldImg := CreateImage(ImageRect, 0, PImageBGRA(Base.ImageBuffer));
      ImgDifs := GetDifs(OldImg, NewImg);

      for var R in ImgDifs do
      begin
        LeftOffset := R.Left * SizeOfImgBGRA;
        ThisStartSrc := StartSrc + LeftOffset + (R.Top * SrcRowStride);
        DifImg := CreateImage(R, SrcRowStride, PImageBGRA(ThisStartSrc));
        DifImg.IsContiguous := False;
        Data.OnFrameChangedGDIRegion(DifImg, Monitor);
      end;
    end;

    // Buffer management for GDI change detection
    if Base.ImageBufferSize < DstRowStride * Monitor.Height then
    begin
      Base.ImageBufferSize := DstRowStride * Monitor.Height;
      SetLength(Base.ImageBuffer, Base.ImageBufferSize);
    end;

    StartDst := @Base.ImageBuffer[0];
    if DstRowStride = SrcRowStride then
      Move(StartSrc^, StartDst^, DstRowStride * Monitor.Height)
    else
      for I := 0 to Monitor.Height - 1 do
        Move((StartSrc + (I * SrcRowStride))^, (StartDst + (I * DstRowStride))^,
          DstRowStride);
  end;
end;

end.
