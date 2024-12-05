unit ScreenCaptureTypes;

// Type definitions with all callbacks

interface

uses
  Winapi.Windows, System.Math;

type
  TRegion = class
  private
    FRects: TArray<TRect>;
    FIndex: Integer;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddRect(const Rect: TRect);
    procedure IntersectWith(const Rect: TRect);
    function GetEnumerator: TRegion;
    function MoveNext: Boolean;
    function GetCurrent: TRect;
    function GetCount: Integer;
    function IsEmpty: Boolean;
    function GetRectangles: TArray<TRect>;
    property Current: TRect read GetCurrent;
    property Count: Integer read GetCount;
  end;

  TDUPL_RETURN = (
    DUPL_RETURN_SUCCESS,
    DUPL_RETURN_ERROR_EXPECTED,
    DUPL_RETURN_ERROR_UNEXPECTED
  );

  TFrameBGRA = record
    B, G, R, A: Byte;
  end;
  PImageBGRA = ^TFrameBGRA;

  TFrameRect = record
    Left, Top, Right, Bottom: Integer;
  end;

  TFrame = record
    Bounds: TFrameRect;
    Data: PImageBGRA;
    RowStrideInBytes: Integer;
    IsContiguous: Boolean;
  end;

  TMonitor = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FId: Integer;
    FAdapter: Integer;
    FName: string;
    FIndex: Integer;
    FOutput: Integer;
    FScaling: Single;
  public
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property OffsetX: Integer read FOffsetX write FOffsetX;
    property OffsetY: Integer read FOffsetY write FOffsetY;
    property Id: Integer read FId write FId;
    property Adapter: Integer read FAdapter write FAdapter;
    property Name: string read FName write FName;
    property Index: Integer read FIndex write FIndex;
    property Output: Integer read FOutput write FOutput;
    property Scaling: Single read FScaling write FScaling;
  end;

  TScreenCaptureCallback = reference to procedure(const Image: TFrame; const Monitor: TMonitor);

  TCaptureData = record
    OnNewFrameDXGIFullScreen: TScreenCaptureCallback;    // DXGI full screen
    OnFrameChangedDXGIRegion: TScreenCaptureCallback;    // DXGI dirty regions
    OnNewFrameGDIFullScreen: TScreenCaptureCallback;     // GDI full screen
    OnFrameChangedGDIRegion: TScreenCaptureCallback;     // GDI changed regions
  end;

  TThreadData = class
  public
    ScreenCaptureData: TCaptureData;
    constructor Create;
  end;

  TBaseFrameProcessor = class
  public
    ImageBuffer: TArray<Byte>;
    ImageBufferSize: Integer;
    FirstRun: Boolean;
    constructor Create;
  end;

implementation

{ TRegion }

constructor TRegion.Create;
begin
  inherited Create;
  Clear;
end;

destructor TRegion.Destroy;
begin
  Clear;
  inherited;
end;

procedure TRegion.Clear;
begin
  SetLength(FRects, 0);
  FIndex := -1;
end;


procedure TRegion.AddRect(const Rect: TRect);
begin
  if not IsRectEmpty(Rect) then
  begin
    SetLength(FRects, Length(FRects) + 1);
    FRects[High(FRects)] := Rect;

    // Optimize if we have enough rectangles
    if Length(FRects) >= 10 then // Adjust threshold as needed

  end;
end;


procedure TRegion.IntersectWith(const Rect: TRect);
var
  I: Integer;
begin
  for I := Length(FRects) - 1 downto 0 do
  begin
    if not IntersectRect(FRects[I], FRects[I], Rect) then
    begin
      Delete(FRects, I, 1);
    end;
  end;
end;

function TRegion.GetEnumerator: TRegion;
begin
  FIndex := -1;
  Result := Self;
end;

function TRegion.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < Length(FRects);
end;

function TRegion.GetCurrent: TRect;
begin
  if (FIndex >= 0) and (FIndex < Length(FRects)) then
    Result := FRects[FIndex]
  else
    Result := TRect.Empty;
end;

function TRegion.GetCount: Integer;
begin
  Result := Length(FRects);
end;

function TRegion.IsEmpty: Boolean;
begin
  Result := Length(FRects) = 0;
end;

function TRegion.GetRectangles: TArray<TRect>;
begin
  Result := Copy(FRects);
end;

{ TBaseFrameProcessor }

constructor TBaseFrameProcessor.Create;
begin
  inherited;
  FirstRun := True;
  ImageBufferSize := 0;
end;

{ TThreadData }

constructor TThreadData.Create;
begin
  inherited Create;
  FillChar(ScreenCaptureData, SizeOf(TCaptureData), 0);
end;

end.
