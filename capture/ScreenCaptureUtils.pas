unit ScreenCaptureUtils;

// Region detection utilities for GDI

interface

uses
  System.Classes, System.SysUtils, ScreenCaptureTypes;

const
  MAXDIST = 256;

type
  TBitMap = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FBlocks: TArray<UInt64>;
    function GetBitIndex(X, Y: Integer): Integer;
  public
    constructor Create(Height, Width: Integer);
    function GetBit(X, Y: Integer): Boolean;
    procedure SetBit(X, Y: Integer);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

procedure SanitizeRects(var Rects: TArray<TFrameRect>; const Img: TFrame);
procedure MergeRects(var Rects: TArray<TFrameRect>);
function GetRects(const Map: TBitMap): TArray<TFrameRect>;
function GetDifs(const OldImage, NewImage: TFrame): TArray<TFrameRect>;

implementation

const
  BITS_PER_BLOCK = SizeOf(UInt64) * 8;

{ TBitMap }

constructor TBitMap.Create(Height, Width: Integer);
var
  BlockCount: Integer;
begin
  inherited Create;
  FWidth := Width;
  FHeight := Height;
  BlockCount := (Width * Height) div BITS_PER_BLOCK + 1;
  SetLength(FBlocks, BlockCount);
end;

function TBitMap.GetBitIndex(X, Y: Integer): Integer;
begin
  Result := X * FWidth + Y;
end;

function TBitMap.GetBit(X, Y: Integer): Boolean;
var
  Index: Integer;
  BlockNum, BitNum: Integer;
begin
  Index := GetBitIndex(X, Y);
  BlockNum := Index div BITS_PER_BLOCK;
  BitNum := Index mod BITS_PER_BLOCK;
  Result := (FBlocks[BlockNum] and (UInt64(1) shl BitNum)) <> 0;
end;

procedure TBitMap.SetBit(X, Y: Integer);
var
  Index: Integer;
  BlockNum, BitNum: Integer;
begin
  Index := GetBitIndex(X, Y);
  BlockNum := Index div BITS_PER_BLOCK;
  BitNum := Index mod BITS_PER_BLOCK;
  FBlocks[BlockNum] := FBlocks[BlockNum] or (UInt64(1) shl BitNum);
end;

procedure SanitizeRects(var Rects: TArray<TFrameRect>; const Img: TFrame);
var
  R: Integer;
begin
  for R := 0 to High(Rects) do
  begin
    if Rects[R].Right > (Img.Bounds.Right - Img.Bounds.Left) then
      Rects[R].Right := Img.Bounds.Right - Img.Bounds.Left;

    if Rects[R].Bottom > (Img.Bounds.Bottom - Img.Bounds.Top) then
      Rects[R].Bottom := Img.Bounds.Bottom - Img.Bounds.Top;
  end;
end;

procedure MergeRects(var Rects: TArray<TFrameRect>);
var
  OutRects: TArray<TFrameRect>;
  I: Integer;
  Found: Boolean;
begin
  if Length(Rects) <= 2 then
    Exit;

  SetLength(OutRects, 1);
  OutRects[0] := Rects[0];

  // Horizontal scan
  for I := 1 to High(Rects) do
  begin
    if (OutRects[High(OutRects)].Right = Rects[I].Left) and
       (OutRects[High(OutRects)].Bottom = Rects[I].Bottom) then
      OutRects[High(OutRects)].Right := Rects[I].Right
    else
    begin
      SetLength(OutRects, Length(OutRects) + 1);
      OutRects[High(OutRects)] := Rects[I];
    end;
  end;

  if Length(OutRects) <= 2 then
  begin
    Rects := OutRects;
    Exit;
  end;

  SetLength(Rects, 0);

  // Vertical scan
  for I := 0 to High(OutRects) do
  begin
    Found := False;
    for var J := High(Rects) downto 0 do
    begin
      if (Rects[J].Bottom = OutRects[I].Top) and
         (Rects[J].Left = OutRects[I].Left) and
         (Rects[J].Right = OutRects[I].Right) then
      begin
        Rects[J].Bottom := OutRects[I].Bottom;
        Found := True;
        Break;
      end;
    end;

    if not Found then
    begin
      SetLength(Rects, Length(Rects) + 1);
      Rects[High(Rects)] := OutRects[I];
    end;
  end;
end;

function GetRects(const Map: TBitMap): TArray<TFrameRect>;
var
  X, Y: Integer;
begin
  SetLength(Result, 0);

  for X := 0 to Map.Height - 1 do
    for Y := 0 to Map.Width - 1 do
      if Map.GetBit(X, Y) then
      begin
        SetLength(Result, Length(Result) + 1);
        with Result[High(Result)] do
        begin
          Top := X * MAXDIST;
          Bottom := (X + 1) * MAXDIST;
          Left := Y * MAXDIST;
          Right := (Y + 1) * MAXDIST;
        end;
      end;
end;

function GetDifs(const OldImage, NewImage: TFrame): TArray<TFrameRect>;
var
  OldPtr, NewPtr: PInteger;
  Width, Height: Integer;
  WidthChunks, HeightChunks: Integer;
  LineRem, BottomRem: Integer;
  Changes: TBitMap;
  X, Y, I: Integer;
begin
  OldPtr := PInteger(OldImage.Data);
  NewPtr := PInteger(NewImage.Data);

  Width := NewImage.Bounds.Right - NewImage.Bounds.Left;
  Height := NewImage.Bounds.Bottom - NewImage.Bounds.Top;

  WidthChunks := Width div MAXDIST;
  HeightChunks := Height div MAXDIST;

  LineRem := Width mod MAXDIST;
  BottomRem := Height mod MAXDIST;

  Changes := TBitMap.Create(HeightChunks + 1, WidthChunks + 1);
  try
    // Compare function
    for X := 0 to HeightChunks - 1 do
    begin
      for I := 0 to MAXDIST - 1 do
      begin
        for Y := 0 to WidthChunks - 1 do
        begin
          if not Changes.GetBit(X, Y) then
            if not CompareMem(OldPtr, NewPtr, MAXDIST * SizeOf(Integer)) then
              Changes.SetBit(X, Y);

          Inc(OldPtr, MAXDIST);
          Inc(NewPtr, MAXDIST);
        end;

        if not Changes.GetBit(X, WidthChunks) then
          if not CompareMem(OldPtr, NewPtr, LineRem * SizeOf(Integer)) then
            Changes.SetBit(X, WidthChunks);

        Inc(OldPtr, LineRem);
        Inc(NewPtr, LineRem);
      end;
    end;

    for I := 0 to BottomRem - 1 do
    begin
      for Y := 0 to WidthChunks - 1 do
      begin
        if not Changes.GetBit(HeightChunks, Y) then
          if not CompareMem(OldPtr, NewPtr, MAXDIST * SizeOf(Integer)) then
            Changes.SetBit(HeightChunks, Y);

        Inc(OldPtr, MAXDIST);
        Inc(NewPtr, MAXDIST);
      end;

      if not Changes.GetBit(HeightChunks, WidthChunks) then
        if not CompareMem(OldPtr, NewPtr, LineRem * SizeOf(Integer)) then
          Changes.SetBit(HeightChunks, WidthChunks);

      Inc(OldPtr, LineRem);
      Inc(NewPtr, LineRem);
    end;

    Result := GetRects(Changes);
    MergeRects(Result);
    SanitizeRects(Result, NewImage);

  finally
    Changes.Free;
  end;
end;

end.
