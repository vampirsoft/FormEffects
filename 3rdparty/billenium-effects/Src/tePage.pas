unit tePage;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teTimed, Windows, Messages, Graphics, teRender;

type
  {$ifndef TE_NOHLP}
  TTERect = class;

  TTERects = class
  private
    List: TList;
  public
    OrgDir,
    TgtDir: TTEEffectDirection;

    constructor Create(OrgDirValue, TgtDirValue: TTEEffectDirection);
    destructor Destroy; override;

    function CreateRect(ClipWidth, ClipHeight, Left, Top, Right,
      Bottom: Integer): TTERect;
    function TgtBottom(Rect: TTERect): Integer;
    function TgtLeft(Rect: TTERect): Integer;
    function TgtRight(Rect: TTERect): Integer;
    function TgtTop(Rect: TTERect): Integer;
  end;

  TTERect = class
  private
    Rects: TTERects;
    function GetBottom: Integer;
    function GetLeft: Integer;
    function GetRight: Integer;
    function GetTop: Integer;
    procedure SetBottom(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
  public
    ClipWidth,
    ClipHeight: Integer;
    R: TRect;

    constructor Create(RectsValue: TTERects);
    function Height: Integer;
    function Width: Integer;
    function TgtHeight: Integer;
    function TgtWidth: Integer;

    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Right: Integer read GetRight write SetRight;
    property Bottom: Integer read GetBottom write SetBottom;
    function TgtLeft: Integer;
    function TgtTop: Integer;
    function TgtRight: Integer;
    function TgtBottom: Integer;
    function TgtR: TRect;
    function UpdR: TRect;
  end;
  {$endif TE_NOHLP}

  TPageTransition = class(TTimedTransitionEffect)
  private
    FSize: Byte;
    FUncover: Boolean;
    FUse3D: Boolean;
    procedure Apply3D(IsReversed: Boolean; Dir: TTEEffectDirection; ArcData,
      ArcRevData: PByteArray; ArcCustomData, ArcRevCustomData: PDWordArray; ArcBmp:
      TBitmap; ArcRect: TRect; OrgBmp: TBitmap; OrgRect: TRect; ArcVisPixelCount:
      Integer);
    procedure Apply3DHrz(IsReversed: Boolean; ArcData: PDWordArray; Dst:
      PByteArray; DstIndex, DstUpdRowLen: Longint; Src: PByteArray; SrcIndex:
      Longint);
    procedure Apply3DVrt(IsReversed: Boolean; ArcData, Dst: PByteArray;
      DstIndex, DstUpdRowLen, DstUpdGap: Longint; Src: PByteArray;
      SrcIndex, SrcUpdGap: Longint);
  protected
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    function GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
    function UncoverToUse: Boolean;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;
    procedure Assign(Source: TPersistent); override;
    class function GetEditor: String; override;
    function Is3D(Device: TTETransitionDevice): Boolean;
  published
    property Direction default tedLeft;
    property Pass2Options;
    property PassSetting;
    property Reversed;
    property Size: Byte read FSize write FSize default 150;
    property Uncover: Boolean read FUncover write FUncover default True;
    property Use3D: Boolean read FUse3D write FUse3D default True;
  end;

implementation

uses teMskWk, TypInfo;

type
  TPageData = class(TTECustomData)
  public
    ArcData: PByteArray;        // Array containing the pixel count for each position in the arc
    ArcCustomData: PDWordArray; // Arc data with custom calculations
    ArcLightMap: PByteArray;    // Arc lighting
    ArcPixelCount: Integer;     // Number of pixels in each arc
    ArcPreCalc: PWordArray;     // Total pixels accumulated for each item in ArcData
    ArcRevData: PByteArray;     // Array containing the pixel count for each position in the reversed arc
    ArcRevLightMap: PByteArray; // Reversed arc lighting
    ArcVisPixelCount: Integer;  // Number of visible pixels in each arc
    BmpArc: TBitmap;            // Bitmap for offscreen rendering of the arc
    BmpArcRev: TBitmap;         // Bitmap for offscreen rendering of the reversed arc
    BmpReversed: TBitmap;       // Contains the reversed org bitmap
    Bmp: TBitmap;               // The org bitmap
    Direction: TTEEffectDirection;
    Height: Integer;
    DoScroll: Boolean;          // Allow scrollin pixels
    R1Arc3D: TTERect;
    R1ArcRev3D: TTERect;
    R2Arc3D: TTERect;
    R2ArcRev3D: TTERect;
    RArc: TTERect;              // Regular arc rect
    RArcRev: TTERect;           // Reversed arc rect
    RClip: TTERect;             // Clipping rect
    Rects: TTERects;
    RMovingPg: TTERect;         // Moving page rect
    RMovingPgBak: TTERect;
    RMovingPgNew: TTERect;
    RStaticPg: TTERect;         // Static page rect
    RVisArc: TTERect;           // Visible regular arc rect
    RVisArcRev: TTERect;        // Visible reversed arc rect
    Width: Integer;

    constructor Create(AData: TTETransitionData; Size: Integer;
      Uncover: Boolean; Dir: TTEEffectDirection); reintroduce;
    destructor Destroy; override;
  end;

constructor TPageData.Create(AData: TTETransitionData; Size: Integer;
  Uncover: Boolean; Dir: TTEEffectDirection);
begin
  inherited Create(AData);

  if Uncover
  then Direction := Dir
  else
  begin
    case Dir of
      tedRight    : Direction := tedLeft;
      tedLeft     : Direction := tedRight;
      tedDown     : Direction := tedUp;
      tedUp       : Direction := tedDown;
    end;
  end;

  if Size > 0 then
  begin
    GetMem    (ArcData       , Size * 2); // Maximum of 2 * radius
    ZeroMemory(ArcData       , Size * 2);
    GetMem    (ArcRevData    , Size * 2);
    GetMem    (ArcPreCalc    , Size * 2 * 2);
    GetMem    (ArcLightMap   , Size * 2);
    ZeroMemory(ArcLightMap   , Size * 2);
    GetMem    (ArcRevLightMap, Size * 2);
    GetMem    (ArcCustomData , Size * 2 * 4);
    BmpArc    := TBitmap.Create;
    BmpArc   .Canvas.Lock;
    BmpArcRev := TBitmap.Create;
    BmpArcRev.Canvas.Lock;
  end;
  ArcPixelCount    := 0;
  ArcVisPixelCount := 0;
  BmpReversed      := TBitmap.Create;
  BmpReversed.Canvas.Lock;
  Rects            := TTERects.Create(tedDown, Direction);
end;

destructor TPageData.Destroy;
begin
  if Assigned(ArcData       ) then
    FreeMem(ArcData);
  if Assigned(ArcRevData    ) then
    FreeMem(ArcRevData);
  if Assigned(ArcPreCalc    ) then
    FreeMem(ArcPreCalc);
  if Assigned(ArcLightMap   ) then
    FreeMem(ArcLightMap);
  if Assigned(ArcRevLightMap) then
    FreeMem(ArcRevLightMap);
  if Assigned(ArcCustomData ) then
    FreeMem(ArcCustomData);

  if Assigned(BmpArc     ) then
  begin
    BmpArc     .Canvas.Unlock;
    BmpArc     .Free;
  end;
  if Assigned(BmpArcRev  ) then
  begin
    BmpArcRev  .Canvas.Unlock;
    BmpArcRev  .Free;
  end;
  if Assigned(BmpReversed) then
  begin
    BmpReversed.Canvas.Unlock;
    BmpReversed.Free;
  end;
  Rects.Free;

  inherited;
end;

{ TPageTransition }

constructor TPageTransition.Create(AOwner: TComponent);
begin
  inherited;

  AllowedDirections := [tedRight, tedLeft, tedDown, tedUp, tedRandom];
  Direction         := tedLeft;
  FSize             := 150;
  FUncover          := True;
  FUse3D            := True;
end;

procedure TPageTransition.Apply3D(IsReversed: Boolean; Dir: TTEEffectDirection;
  ArcData, ArcRevData: PByteArray; ArcCustomData, ArcRevCustomData:
  PDWordArray; ArcBmp: TBitmap; ArcRect: TRect; OrgBmp: TBitmap; OrgRect:
  TRect; ArcVisPixelCount: Integer);
var
  ArcUpdParams,
  OrgUpdParams: TTEUpdParams;
  ArcScanLineSize,
  OrgScanLineSize,
  SavePos: Integer;
  ArcPixels,
  OrgPixels: PAnsiChar;
  UnUpdateRect: TRect;
  SaveValue: DWord;
  OrgCorr: Byte;
begin
  ArcScanLineSize := GetBytesPerScanline(ArcBmp, pf32bit, 32);
  OrgScanLineSize := GetBytesPerScanline(OrgBmp, pf32bit, 32);
  ArcPixels    := PAnsiChar(ArcBmp.ScanLine[0]) + ArcScanlineSize;
  OrgPixels    := PAnsiChar(OrgBmp.ScanLine[0]) + OrgScanlineSize;
  UnUpdateRect := Rect(0, 0, 0, 0);
  GiveMeTheUpdParams(2, ArcUpdParams, ArcScanLineSize, ArcRect, UnUpdateRect,
    pf32bit);

  case Dir of
    tedDown:
    begin
      GiveMeTheUpdParams(2, OrgUpdParams, OrgScanLineSize, OrgRect,
        UnUpdateRect, pf32bit);
      {$ifdef LogTiming}
      if Assigned(Log) then
        Log.ChronoExtra.Start;
      {$endif LogTiming}
      Apply3DHrz(
        IsReversed,
        ArcCustomData,
        PByteArray(ArcPixels - ArcUpdParams.Start1),
        -ArcUpdParams.Lenght1 * 4,
        ArcUpdParams.RowLenght1 * 4,
        PByteArray(OrgPixels - OrgUpdParams.Start1),
        -OrgUpdParams.Lenght1 * 4);
      {$ifdef LogTiming}
      if Assigned(Log) then
        Log.ChronoExtra.Pause;
      {$endif LogTiming}
    end;
    tedUp:
    begin
      if OrgRect.Bottom > OrgBmp.Height
      then
      begin
        OrgCorr        := OrgRect.Bottom - OrgBmp.Height;
        OrgRect.Bottom := OrgBmp.Height;
      end
      else OrgCorr  := 0;
      if IsReversed
      then SavePos := 0
      else SavePos := ArcVisPixelCount - (ArcRect.Bottom - ArcRect.Top);
      SaveValue    := ArcRevCustomData[SavePos];
      GiveMeTheUpdParams(2, OrgUpdParams, OrgScanLineSize, OrgRect,
        UnUpdateRect, pf32bit);
      ArcRevCustomData[SavePos] :=
        ArcRevCustomData[SavePos] - DWord(OrgCorr * OrgScanLineSize);
      try
        {$ifdef LogTiming}
        if Assigned(Log) then
          Log.ChronoExtra.Start;
        {$endif LogTiming}
        Apply3DHrz(
          IsReversed,
          PDWordArray(@ArcRevCustomData[SavePos]),
          PByteArray(ArcPixels - ArcUpdParams.Start1),
          -ArcUpdParams.Lenght1 * 4,
          ArcUpdParams.RowLenght1 * 4,
          PByteArray(OrgPixels - OrgUpdParams.Start1),
          -OrgUpdParams.Lenght1 * 4);
        {$ifdef LogTiming}
        if Assigned(Log) then
          Log.ChronoExtra.Pause;
        {$endif LogTiming}
      finally
        ArcRevCustomData[SavePos] := SaveValue;
      end;
    end;
    tedRight:
    begin
      if OrgRect.Left < 0
      then
      begin
        OrgCorr      := -OrgRect.Left;
        OrgRect.Left := 0;
      end
      else OrgCorr := 0;
      if IsReversed
      then SavePos := 0
      else SavePos := ArcVisPixelCount - (ArcRect.Right - ArcRect.Left);
      SaveValue  := ArcRevCustomData[SavePos];
//      Assert(ArcRevCustomData[SavePos] >= OrgCorr);
      GiveMeTheUpdParams(2, OrgUpdParams, OrgScanLineSize, OrgRect,
        UnUpdateRect, pf32bit);
      ArcRevCustomData[SavePos] := ArcRevCustomData[SavePos] - (OrgCorr * 4);
      try
        Apply3DVrt(
          IsReversed,
          PByteArray(@ArcRevCustomData[SavePos]),
          PByteArray(ArcPixels - ArcUpdParams.Start1),
          -ArcUpdParams.Lenght1 * 4,
          ArcUpdParams.RowLenght1 * 4,
          ArcUpdParams.Gap1 * 4,
          PByteArray(OrgPixels - OrgUpdParams.Start1),
          -OrgUpdParams.Lenght1 * 4,
          OrgUpdParams.Gap1 * 4);
      finally
        ArcRevCustomData[SavePos] := SaveValue;
      end;
    end;
    tedLeft:
    begin
      GiveMeTheUpdParams(2, OrgUpdParams, OrgScanLineSize, OrgRect,
        UnUpdateRect, pf32bit);
      Apply3DVrt(
        IsReversed,
        PByteArray(ArcCustomData),
        PByteArray(ArcPixels - ArcUpdParams.Start1),
        -ArcUpdParams.Lenght1 * 4,
        ArcUpdParams.RowLenght1 * 4,
        ArcUpdParams.Gap1 * 4,
        PByteArray(OrgPixels - OrgUpdParams.Start1),
        -OrgUpdParams.Lenght1 * 4,
        OrgUpdParams.Gap1 * 4);
    end;
  end;
end;

procedure TPageTransition.Apply3DHrz(IsReversed: Boolean; ArcData: PDWordArray;
  Dst: PByteArray; DstIndex, DstUpdRowLen: Longint; Src: PByteArray;
  SrcIndex: Longint);
var
  i,
  Limit: Longint;
  Light: Byte;
begin
  // Adjusts for common index
  Inc(PAnsiChar(Src), SrcIndex - DstIndex);
  i := 0;
  if IsReversed
  then
  begin
    while DstIndex < 0 do
    begin
      Limit := DstIndex + DstUpdRowLen;
      while DstIndex < Limit do
      begin
        if Src[DstIndex  ] > Dst[DstIndex+3]
        then Dst[DstIndex  ] := Src[DstIndex  ] - Dst[DstIndex+3]
        else Dst[DstIndex  ] := 0;

        if Src[DstIndex+1] > Dst[DstIndex+3]
        then Dst[DstIndex+1] := Src[DstIndex+1] - Dst[DstIndex+3]
        else Dst[DstIndex+1] := 0;

        if Src[DstIndex+2] > Dst[DstIndex+3]
        then Dst[DstIndex+2] := Src[DstIndex+2] - Dst[DstIndex+3]
        else Dst[DstIndex+2] := 0;
        Inc(DstIndex, 4);
      end;
      Inc(PAnsiChar(Src), ArcData[i]);
      Inc(i);
    end;
  end
  else
  begin
    while DstIndex < 0 do
    begin
      Limit := DstIndex + DstUpdRowLen;
      while DstIndex < Limit do
      begin
        Light := -Dst[DstIndex+3];

        if Src[DstIndex  ] < Light
        then Dst[DstIndex  ] := Src[DstIndex  ] + Dst[DstIndex+3]
        else Dst[DstIndex  ] := 255;

        if Src[DstIndex+1] < Light
        then Dst[DstIndex+1] := Src[DstIndex+1] + Dst[DstIndex+3]
        else Dst[DstIndex+1] := 255;

        if Src[DstIndex+2] < Light
        then Dst[DstIndex+2] := Src[DstIndex+2] + Dst[DstIndex+3]
        else Dst[DstIndex+2] := 255;
        Inc(DstIndex, 4);
      end;
      Inc(PAnsiChar(Src), ArcData[i]);
      Inc(i);
    end;
  end;
end;

procedure TPageTransition.Apply3DVrt(IsReversed: Boolean; ArcData,
  Dst: PByteArray; DstIndex, DstUpdRowLen, DstUpdGap: Longint; Src: PByteArray;
  SrcIndex, SrcUpdGap: Longint);
var
  ArcDataGap,
  Limit: Longint;
  Light: Byte;
begin
  // Adjusts for common index
  ArcDataGap := DstUpdRowLen + DstUpdGap;
  Inc(PAnsiChar(Src)    , SrcIndex - DstIndex);
  Dec(SrcUpdGap     , DstUpdGap);
  Dec(PAnsiChar(ArcData), DstIndex);

  if IsReversed
  then
  begin
    while DstIndex < 0 do
    begin
      Limit := DstIndex + DstUpdRowLen;
      while DstIndex < Limit do
      begin
        if Src[DstIndex  ] > Dst[DstIndex+3]
        then Dst[DstIndex  ] := Src[DstIndex  ] - Dst[DstIndex+3]
        else Dst[DstIndex  ] := 0;

        if Src[DstIndex+1] > Dst[DstIndex+3]
        then Dst[DstIndex+1] := Src[DstIndex+1] - Dst[DstIndex+3]
        else Dst[DstIndex+1] := 0;

        if Src[DstIndex+2] > Dst[DstIndex+3]
        then Dst[DstIndex+2] := Src[DstIndex+2] - Dst[DstIndex+3]
        else Dst[DstIndex+2] := 0;
        Inc(PAnsiChar(Src), PDWord(@ArcData[DstIndex])^);
        Inc(DstIndex, 4);
      end;
      Inc(DstIndex      , DstUpdGap);
      Inc(PAnsiChar(Src)    , SrcUpdGap);
      Dec(PAnsiChar(ArcData), ArcDataGap);
    end;
  end
  else
  begin
    while DstIndex < 0 do
    begin
      Limit := DstIndex + DstUpdRowLen;
      while DstIndex < Limit do
      begin
        Light := -Dst[DstIndex+3];

        if Src[DstIndex  ] < Light
        then Dst[DstIndex  ] := Src[DstIndex  ] + Dst[DstIndex+3]
        else Dst[DstIndex  ] := 255;

        if Src[DstIndex+1] < Light
        then Dst[DstIndex+1] := Src[DstIndex+1] + Dst[DstIndex+3]
        else Dst[DstIndex+1] := 255;

        if Src[DstIndex+2] < Light
        then Dst[DstIndex+2] := Src[DstIndex+2] + Dst[DstIndex+3]
        else Dst[DstIndex+2] := 255;
        Inc(PAnsiChar(Src), PDWord(@ArcData[DstIndex])^);
        Inc(DstIndex, 4);
      end;
      Inc(DstIndex      , DstUpdGap);
      Inc(PAnsiChar(Src)    , SrcUpdGap);
      Dec(PAnsiChar(ArcData), ArcDataGap);
    end;
  end;
end;

class function TPageTransition.Description: String;
begin
  Result := 'Page';
end;

procedure TPageTransition.Assign(Source: TPersistent);
begin
  if Source is TPageTransition
  then
  begin
    inherited;

    Use3D   := TPageTransition(Source).Use3D;
    Size    := TPageTransition(Source).Size;
    Uncover := TPageTransition(Source).Uncover;
  end
  else inherited;
end;

class function TPageTransition.GetEditor: String;
begin
  Result := 'TPageTransitionEditor';
end;

procedure TPageTransition.Initialize(Data: TTETransitionData;
  var TotalFrames: Longint);
var
  PageData: TPageData;

  {$ifndef TrialLimited}
  procedure PlotPixel(Light: Array of Byte; x, y: Integer);
  begin
    if x < 0 then
      exit;

    if PageData.ArcLightMap[x] = 0
    then // This is a visible pixel
    begin
      PageData.ArcLightMap[x] := Light[y];
      Inc(PageData.ArcVisPixelCount);
    end
    else // This is an invisible pixel
    begin
      PageData.ArcData    [x] := PageData.ArcData[x] + 1;
      PageData.ArcLightMap[x] := Light[y];
    end;
    Inc(PageData.ArcPixelCount);
  end;

  procedure PlotCircle(Light: Array of Byte; x, y, x1, y1: Integer);
  var
    P1,
    P2: TPoint;
  begin
    P1 := Point(x + x1, y - y1);
    PlotPixel(Light, P1.x  , P1.y);
    P2 := Point(x + y1, y - x1);
    if(P1.x <> P2.x) or (P1.y <> P2.y) then
      PlotPixel(Light, P2.x  , P2.y);
  end;

  procedure BresenhamCircle(Radius: Integer);
  const
    BaseLight = 200;
  var
    TrimLight,
    CenterX,
    CenterY,
    x1,
    y1,
    p,
    i,
    j: Integer;
    Light: array[0..255] of Byte;
    ArcData: PByteArray;
  begin
    for i := 0 to Radius do
      Light[i] := Round((i+1) * (BaseLight / (Radius+1)));

    CenterX := 0;
    CenterY := Radius;
    x1    := 0;
    y1    := Radius;
    p     := 3 - 2 * Radius;
    while (x1 < y1) do
    begin
      PlotCircle(Light, CenterX, CenterY, x1, y1);
      if p < 0
      then p := p + 4 * x1 + 6
      else
      begin
        p  := p + 4 * (x1 - y1) + 10;
        Dec(y1);
      end;
      Inc(x1);
    end;
    if x1 = y1 then
      PlotCircle(Light, CenterX, CenterY, x1, y1);

    // Trim the initial arc's area which is almost invisible but eats CPU
    TrimLight := 0;
    j         := 0;
    while(j < PageData.ArcVisPixelCount) and (PageData.ArcLightMap[j] <= 253) do
    begin
      p := 0;
      while(j < PageData.ArcVisPixelCount) and
           (PageData.ArcData[j] = 0)       and
           (PageData.ArcLightMap[j] <= TrimLight+1) do
      begin
        Inc(p);
        Inc(j);
      end;
      if p > 2
      then Inc(TrimLight)
      else break;
    end;
    if TrimLight > 0 then
    begin
      i := 0;
      while(PageData.ArcData[i] = 0) and (PageData.ArcLightMap[i] <= TrimLight) do
        Inc(i);
      if i > 0 then
      begin
        Dec(PageData.ArcPixelCount   , i-(TrimLight*2));
        Dec(PageData.ArcVisPixelCount, i-(TrimLight*2));
        for j := 0 to TrimLight-1 do
        begin
          Assert(PageData.ArcData    [ j*2   ] = 0);
          Assert(PageData.ArcLightMap[ j*2   ] <= j+1);
          PageData.ArcLightMap[ j*2   ] := j+1;
          Assert(PageData.ArcData    [(j*2)+1] = 0);
          Assert(PageData.ArcLightMap[(j*2)+1] <= j+1);
          PageData.ArcLightMap[(j*2)+1] := j+1;
        end;
        for j := TrimLight*2 to PageData.ArcVisPixelCount-1 do
        begin
          PageData.ArcData    [j] := PageData.ArcData    [j+i-(TrimLight*2)];
          PageData.ArcLightMap[j] := PageData.ArcLightMap[j+i-(TrimLight*2)];
        end;
      end;
    end;

    // Calculate the reversed arc
    for i := 0 to PageData.ArcVisPixelCount - 1 do
    begin
      PageData.ArcRevData    [i] := PageData.ArcData    [(PageData.ArcVisPixelCount-1) - i];
      PageData.ArcRevLightMap[i] := PageData.ArcLightMap[(PageData.ArcVisPixelCount-1) - i];
    end;

    // Precalculation table
//    if UncoverToUse
//    then ArcData := PageData.ArcData
//    else ArcData := PageData.ArcRevData;
    j := 0;
    PageData.ArcPreCalc[0] := 0;
    for i := 1 to Radius+1 do
    begin
      Inc(j, PageData.ArcData[i-1] + 1); // Accumulates with previous
      PageData.ArcPreCalc[i] := j;
    end;

    // Arc custom data
    if PageData.Direction in [tedDown, tedLeft]
    then ArcData := PageData.ArcData
    else ArcData := PageData.ArcRevData;
    if PageData.Direction in [tedDown, tedUp]
    then
    begin
      j := GetBytesPerScanline(Data.SrcBmp, pf32bit, 32);
      for i := 0 to PageData.ArcVisPixelCount - 1 do
        PageData.ArcCustomData[i] := ArcData[i] * j;
    end
    else
    begin
      for i := 0 to PageData.ArcVisPixelCount - 1 do
        PageData.ArcCustomData[i] := (ArcData[i]) * 4;
    end;

  end;
  {$endif TrialLimited}

  procedure EmbedLight(Direction: TTEEffectDirection; Bmp1, Bmp2: TBitmap;
    LightMap, RevLightMap: PByteArray);
  const
    MaxLight = 50;
  var
    i,
    Index,
    Limit,
    ScanLineSize: Integer;
    Light,
    Dst1,
    Dst2: PByteArray;
    aux: Byte;
  begin
    ScanLineSize := GetBytesPerScanline(Bmp1, pf32bit, 32);
    Dst1         := PByteArray(PAnsiChar(Bmp1.ScanLine[0]) + ScanlineSize);
    Dst2         := PByteArray(PAnsiChar(Bmp2.ScanLine[0]) + ScanlineSize);
    Index        := -(ScanlineSize * Bmp1.Height);

    if Direction in [tedDown, tedLeft]
    then Light := LightMap
    else Light := RevLightMap;

    if Direction in [tedDown, tedUp]
    then
    begin
      i := 0;
      while Index < 0 do
      begin
        if Light[i] > MaxLight
        then aux := MaxLight
        else aux := Light[i];
        Limit := Index + ScanLineSize;
        while Index < Limit do
        begin
          Dst1[Index+3] := aux;
          Dst2[Index+3] := Light[i];
          Inc(Index, 4);
        end;
        Inc(i);
      end;
    end
    else
    begin
      while Index < 0 do
      begin
        i := 0;
        Limit := Index + ScanLineSize;
        while Index < Limit do
        begin
          if Light[i] > MaxLight
          then Dst1[Index+3] := MaxLight
          else Dst1[Index+3] := Light[i];
          Dst2[Index+3] := Light[i];
          Inc(Index, 4);
          Inc(i);
        end;
      end;
    end;
  end;

var
  W,
  H,
  SizeToUse: Integer;
begin
  inherited;

  if DirectionToUse in [tedDown, tedUp]
  then
  begin
    W := Data.Width;
    H := Data.Height;
  end
  else
  begin
    W := Data.Height;
    H := Data.Width;
  end;

  SizeToUse := FSize;
  if not Is3D(Data.Device)
  then SizeToUse := 0
  else
    if SizeToUse > H div 3 then
      SizeToUse := H div 3;

  PageData    := TPageData.Create(Data, SizeToUse, UncoverToUse, DirectionToUse);
  Data.Custom := PageData;
  PageData.Width  := W;
  PageData.Height := H;
  if Data.Bitmap = nil
  then PageData.DoScroll := not(Data.Device.DynamicClipping or Data.Device.Clipped)
  else PageData.DoScroll := True;

  PageData.RClip := PageData.Rects.CreateRect(PageData.Width, PageData.Height, 0, 0, W, H);
  AdjustBmpForTransition(PageData.BmpReversed, 0, Data.Width, Data.Height, pf32bit);

  {$ifndef TrialLimited}
  if SizeToUse > 0 then
    BresenhamCircle(SizeToUse-1);
  Assert(PageData.ArcPixelCount <= H);
  {$endif TrialLimited}

  if UncoverToUse
  then PageData.Bmp := Data.SrcBmp
  else PageData.Bmp := Data.DstBmp;

  TotalFrames := PageData.Height + PageData.ArcVisPixelCount - 1;

  PageData.R1Arc3D    := PageData.Rects.CreateRect(PageData.Width,
    PageData.ArcVisPixelCount, 0, 0, PageData.Width, PageData.ArcVisPixelCount);
  PageData.R2Arc3D    := PageData.Rects.CreateRect(PageData.Width,
    PageData.Height          , 0, 0, PageData.Width, PageData.Height);
  PageData.R1ArcRev3D := PageData.Rects.CreateRect(PageData.Width,
    PageData.ArcVisPixelCount, 0, 0, PageData.Width, PageData.ArcVisPixelCount);
  PageData.R2ArcRev3D := PageData.Rects.CreateRect(PageData.Width,
    PageData.Height          , 0, 0, PageData.Width, PageData.Height);
  if SizeToUse > 0 then
  begin
    AdjustBmpForTransition(PageData.BmpArc   , 0, PageData.R1Arc3D.TgtWidth,
      PageData.R1Arc3D.TgtHeight, pf32bit);
    AdjustBmpForTransition(PageData.BmpArcRev, 0, PageData.R1Arc3D.TgtWidth,
      PageData.R1Arc3D.TgtHeight, pf32bit);
    EmbedLight(PageData.Direction, PageData.BmpArc, PageData.BmpArcRev,
      PageData.ArcLightMap, PageData.ArcRevLightMap);
  end;

  if DirectionToUse in [tedDown, tedUp]
  then StretchBlt(
         PageData.BmpReversed.Canvas.Handle, 0, 0, Data.Width, Data.Height,
         PageData.Bmp.Canvas.Handle, 0, Data.Height-1, Data.Width, -Data.Height,
         cmSrcCopy)
  else StretchBlt(
         PageData.BmpReversed.Canvas.Handle, 0, 0, Data.Width, Data.Height,
         PageData.Bmp.Canvas.Handle, Data.Width-1, 0, -Data.Width, Data.Height,
         cmSrcCopy);

  PageData.RMovingPg    := PageData.Rects.CreateRect(PageData.Width, PageData.Height, 0, 0, PageData.Width, 0);
  PageData.RStaticPg    := PageData.Rects.CreateRect(PageData.Width, PageData.Height, 0, 0, PageData.Width, 0);
  PageData.RArc         := PageData.Rects.CreateRect(PageData.Width, PageData.Height, 0, 0, PageData.Width, 0);
  PageData.RArcRev      := PageData.Rects.CreateRect(PageData.Width, PageData.Height, 0, 0, PageData.Width, 0);
  PageData.RVisArc      := PageData.Rects.CreateRect(PageData.Width, PageData.Height, 0, 0,              0, 0);
  PageData.RVisArcRev   := PageData.Rects.CreateRect(PageData.Width, PageData.Height, 0, 0,              0, 0);
  PageData.RMovingPgBak := PageData.Rects.CreateRect(PageData.Width, PageData.Height, 0, 0,              0, 0);
  PageData.RMovingPgNew := PageData.Rects.CreateRect(PageData.Width, PageData.Height, 0, 0,              0, 0);
end;

procedure TPageTransition.ExecuteFrame(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Longint);

  procedure CalcArcPixels(IsReversed: Boolean; ArcData: PByteArray;
    ArcPixelCount, ArcVisPixelCount, PixelIndex: Integer;
    var CalcVisPixels, CalcTotPixels: Integer);
  begin
    if IsReversed then
      Dec(PixelIndex, ArcPixelCount);

    if PixelIndex <= 0
    then
    begin
      CalcVisPixels := 0;
      CalcTotPixels := 0;
    end
    else
    begin
      if PixelIndex >= ArcPixelCount
      then
      begin
        CalcVisPixels := ArcVisPixelCount;
        CalcTotPixels := ArcPixelCount;
      end
      else
      begin
        CalcVisPixels := 0;
        CalcTotPixels := 0;
        while CalcTotPixels < PixelIndex do
        begin
          Inc(CalcTotPixels, ArcData[CalcVisPixels] + 1);
          Inc(CalcVisPixels);
        end;
      end;
    end;
  end;

var
  PageData: TPageData;
  Aux,
  Aux2,
  xAux,
  yAux,
  xScroll,
  yScroll,
  ArcVisPixels,
  ArcRevVisPixels,
  ArcTotPixels,
  ArcRevTotPixels: Integer;
  Frame: Longint;
  RAux1,
  RAux2: TRect;
begin
  PageData := TPageData(Data.Custom);

  xScroll := 0;
  yScroll := 0;
  if UncoverToUse
  then Frame := CurrentFrame
  else Frame := Data.Frames - CurrentFrame + 1;

  CalcArcPixels(False, PageData.ArcData   , PageData.ArcPixelCount,
    PageData.ArcVisPixelCount, Frame, ArcVisPixels, ArcTotPixels);
  CalcArcPixels(True , PageData.ArcRevData, PageData.ArcPixelCount,
    PageData.ArcVisPixelCount, Frame, ArcRevVisPixels, ArcRevTotPixels);

  PageData.RArc      .Bottom := Frame;
  PageData.RArc      .Top    := PageData.RArc   .Bottom - ArcVisPixels;
  PageData.RArcRev   .Top    := PageData.RArc   .Top;
  PageData.RArcRev   .Bottom := PageData.RArcRev.Top    + ArcRevVisPixels;
  PageData.RVisArc   .R      := PageData.RArc.R;
  PageData.RVisArc   .Top    := PageData.RArc   .Top    + ArcRevVisPixels;
  PageData.RVisArcRev.R      := PageData.RArcRev.R;
  IntersectRect(PageData.RVisArc   .R, PageData.RVisArc   .R, PageData.RClip.R);
  IntersectRect(PageData.RVisArcRev.R, PageData.RVisArcRev.R, PageData.RClip.R);
  IntersectRect(PageData.RMovingPgBak.R, PageData.RMovingPg.R, PageData.RClip.R);
  Aux := PageData.RMovingPg.Bottom;
  PageData.RMovingPg.Top    := PageData.RArcRev.Bottom;
  if PageData.RMovingPg.Top = PageData.RArc.Bottom
  then PageData.RMovingPg.Bottom :=
         PageData.RMovingPg.Top + (Frame - (PageData.ArcPixelCount * 2))
  else PageData.RMovingPg.Bottom := PageData.RMovingPg.Top;
  IntersectRect(PageData.RMovingPgNew.R, PageData.RMovingPg.R, PageData.RClip.R);

  if PageData.DoScroll                         and
    (not IsRectEmpty(PageData.RMovingPgNew.R)) and
    (not IsRectEmpty(PageData.RMovingPgBak.R)) then
  begin
    Aux := PageData.RMovingPg.Bottom - Aux;
    case DirectionToUse of
      tedDown : yScroll :=  Aux;
      tedUp   : yScroll := -Aux;
      tedRight: xScroll :=  Aux;
      tedLeft : xScroll := -Aux;
    end;
    if UncoverToUse
    then PageData.RMovingPgNew.Bottom := PageData.RMovingPgNew.Top + Aux
    else
    begin
      xScroll := -xScroll;
      yScroll := -yScroll;
      PageData.Rects.TgtDir     := tedDown;
      PageData.RMovingPgNew.Top := PageData.RMovingPgNew.TgtTop + Aux;
      PageData.Rects.TgtDir     := PageData.Direction;
    end;
    IntersectRect(PageData.RMovingPgNew.R, PageData.RMovingPgNew.R, PageData.RClip.R);
  end;
  if UncoverToUse
  then
  begin
    PageData.RStaticPg.Top    := PageData.RStaticPg.Bottom;
    PageData.RStaticPg.Bottom := PageData.RArcRev.Top;
  end
  else
  begin
    PageData.Rects.TgtDir       := tedDown;
    PageData.RStaticPg.Bottom   := PageData.RStaticPg.Top;
    if PageData.RArc.Bottom = PageData.RArcRev.Bottom
    then PageData.RStaticPg.Top := PageData.RMovingPg.TgtBottom
    else PageData.RStaticPg.Top := PageData.RArc     .TgtBottom;
    PageData.Rects.TgtDir       := PageData.Direction;
  end;

  // Apply 3D effect in arc's offscreen bitmap
  if not IsRectEmpty(PageData.RVisArc.R) then
  begin
    PageData.R1Arc3D.Bottom := PageData.ArcVisPixelCount;
    PageData.R1Arc3D.Top    := PageData.R1Arc3D.Bottom - (ArcVisPixels - ArcRevVisPixels);
    PageData.R2Arc3D.Bottom := PageData.RVisArc.Bottom;
    PageData.R2Arc3D.Top    :=
      PageData.R2Arc3D.Bottom -
      PageData.ArcPreCalc[PageData.R1Arc3D.Bottom - PageData.R1Arc3D.Top];

    Apply3D(
      False,
      PageData.Direction,
      PageData.ArcData,
      PageData.ArcRevData,
      PageData.ArcCustomData,
      PageData.ArcCustomData,
      PageData.BmpArc,
      PageData.R1Arc3D.TgtR,
      PageData.Bmp,
      PageData.R2Arc3D.TgtR,
      PageData.ArcVisPixelCount);
  end;

  if not IsRectEmpty(PageData.RVisArcRev.R) then
  begin
    // Calculation for clipping the processed pixels
    Aux:=
      (PageData.Height - PageData.RMovingPg.Height) - ArcRevTotPixels;
    Aux2 :=
      PageData.ArcPreCalc[PageData.RArcRev.Bottom - PageData.RVisArcRev.Bottom];

    PageData.R1ArcRev3D.Top    := 0;
    PageData.R1ArcRev3D.Bottom := PageData.RVisArcRev.Height;
    PageData.R2ArcRev3D.Top    := Aux;
    PageData.R2ArcRev3D.Bottom := Aux + ArcRevTotPixels - Aux2;

    Apply3D(
      True,
      PageData.Direction,
      PByteArray (@PageData.ArcData[PageData.ArcVisPixelCount - (PageData.RVisArcRev.Height)]),
      PageData.ArcRevData,
      PDWordArray(@PageData.ArcCustomData[PageData.ArcVisPixelCount - (PageData.RVisArcRev.Height)]),
      PageData.ArcCustomData,
      PageData.BmpArcRev,
      PageData.R1ArcRev3D.TgtR,
      PageData.BmpReversed,
      PageData.R2ArcRev3D.TgtR,
      PageData.ArcVisPixelCount);
  end;

  // Scroll the old moving page
  if(xScroll <> 0) or (yScroll <> 0) then
  begin
    RAux1 := PageData.RMovingPgBak.TgtR;
    RAux2 := PageData.RClip.TgtR;
    ScrollDC(Data.Canvas.Handle, xScroll, yScroll, RAux1, RAux2, 0, nil);
  end;

  // Paint the new pixels of the moving page
  if not IsRectEmpty(PageData.RMovingPgNew.R) then
  begin
    xAux := 0;
    yAux := 0;
    case PageData.Direction of
      tedDown : yAux := PageData.Height - PageData.RMovingPg.Top + ArcTotPixels + ArcRevTotPixels;
      tedRight: xAux := PageData.Height - PageData.RMovingPg.Top + ArcTotPixels + ArcRevTotPixels;
      tedUp   : yAux := PageData.RMovingPg.Bottom - PageData.RMovingPgNew.Bottom + ArcTotPixels - ArcRevTotPixels;
      tedLeft : xAux := PageData.RMovingPg.Bottom - PageData.RMovingPgNew.Bottom + ArcTotPixels - ArcRevTotPixels;
    end;

    BitBlt(Data.Canvas.Handle,
      PageData.RMovingPgNew.TgtLeft,
      PageData.RMovingPgNew.TgtTop,
      PageData.RMovingPgNew.TgtWidth,
      PageData.RMovingPgNew.TgtHeight,
      PageData.BmpReversed.Canvas.Handle,
      xAux,
      yAux,
      cmSrcCopy);
  end;
  UnionRect(Data.UpdateRect, Data.UpdateRect, PageData.RMovingPg.UpdR);

  if PageData.RStaticPg.Bottom > 0 then
  begin
    // Paint the new pixels of the static page
    BitBlt(Data.Canvas.Handle,
      PageData.RStaticPg.TgtLeft,
      PageData.RStaticPg.TgtTop,
      PageData.RStaticPg.TgtWidth,
      PageData.RStaticPg.TgtHeight,
      Data.DstBmp.Canvas.Handle,
      PageData.RStaticPg.TgtLeft,
      PageData.RStaticPg.TgtTop,
      cmSrcCopy);
    UnionRect(Data.UpdateRect, Data.UpdateRect, PageData.RStaticPg.UpdR);
  end;

  // Paint the arc
  if not IsRectEmpty(PageData.RVisArc.R) then
  begin
    BitBlt(
      Data.Canvas.Handle,
      PageData.RVisArc.TgtLeft,
      PageData.RVisArc.TgtTop,
      PageData.RVisArc.TgtWidth,
      PageData.RVisArc.TgtHeight,
      PageData.BmpArc.Canvas.Handle,
      PageData.R1Arc3D.TgtLeft,
      PageData.R1Arc3D.TgtTop,
      cmSrcCopy);
    UnionRect(Data.UpdateRect, Data.UpdateRect, PageData.RVisArc.UpdR);
  end;

  // Paint the reversed arc
  if not IsRectEmpty(PageData.RVisArcRev.R) then
  begin
    BitBlt(Data.Canvas.Handle,
      PageData.RVisArcRev.TgtLeft,
      PageData.RVisArcRev.TgtTop,
      PageData.RVisArcRev.TgtWidth,
      PageData.RVisArcRev.TgtHeight,
      PageData.BmpArcRev.Canvas.Handle,
      PageData.R1ArcRev3D.TgtLeft,
      PageData.R1ArcRev3D.TgtTop,
      cmSrcCopy);
    UnionRect(Data.UpdateRect, Data.UpdateRect, PageData.RVisArcRev.UpdR);
  end;
end;

function TPageTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiOffScreenBmpCapable,
      tetiThreadSafe
    ];
end;

function TPageTransition.GetPixelFormat(Device: TTETransitionDevice):
  TPixelFormat;
begin
  {$ifndef TrialLimited}
  if(FSize > 0) and Is3D(Device) and Device.IsRGB
  then Result := pf32bit
  else Result := Device.PixelFormat;
  {$else}
  Result := Device.PixelFormat;
  {$endif TrialLimited}
end;

function TPageTransition.Is3D(Device: TTETransitionDevice): Boolean;
begin
{$ifdef TrialLimited}
  Result := False;
{$else}
  Result :=
    TEProcessorInfo.MMX and
    Use3D               and
    Device.IsRGB;
{$endif TrialLimited}
end;

function TPageTransition.UncoverToUse: Boolean;
begin
  if Reversed
  then Result := not Uncover
  else Result := Uncover;
end;

procedure TPageTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'Size'];
  if Value <> '' then
    Size := StrToInt(Value);

  Value := List.Values[Prefix + 'Uncover'];
  if Value <> '' then
    Uncover := SameText(Value, BoolToStr(True));

  Value := List.Values[Prefix + 'Use3D'];
  if Value <> '' then
    Use3D := SameText(Value, BoolToStr(True));
end;

procedure TPageTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
begin
  inherited;

  if(not OmitDefaultValues) or (Size <> 150) then
    List.Values[Prefix + 'Size'] := IntToStr(Size);

  if(not OmitDefaultValues) or
    (Uncover <> True) then
    List.Values[Prefix + 'Uncover'] := BoolToStr(Uncover);

  if(not OmitDefaultValues) or
    (Use3D <> True) then
    List.Values[Prefix + 'Use3D'] := BoolToStr(Use3D);
end;

{ TTERects }

constructor TTERects.Create(OrgDirValue, TgtDirValue: TTEEffectDirection);
begin
  Assert(OrgDirValue = tedDown); // Only tedDown implemented by now
  Assert(TgtDirValue in [tedRight, tedLeft, tedDown, tedUp]);
  OrgDir := OrgDirValue;
  TgtDir := TgtDirValue;
  List := TList.Create;
end;

function TTERects.CreateRect(ClipWidth, ClipHeight, Left, Top, Right,
  Bottom: Integer): TTERect;
begin
  Result := TTERect.Create(Self);
  if TgtDir in [tedDown, tedUp]
  then
  begin
    Result.ClipWidth  := ClipWidth;
    Result.ClipHeight := ClipHeight;
  end
  else
  begin
    Result.ClipWidth  := ClipHeight;
    Result.ClipHeight := ClipWidth;
  end;
  Result.Left       := Left;
  Result.Top        := Top;
  Result.Right      := Right;
  Result.Bottom     := Bottom;
  List.Add(Result);
end;

destructor TTERects.Destroy;
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do
    TTERect(List[i]).Free;
  List.Free;

  inherited;
end;

function TTERects.TgtBottom(Rect: TTERect): Integer;
begin
  case TgtDir of
    tedDown : Result := Rect.Bottom;
    tedUp   : Result := Rect.ClipHeight - Rect.Top;
    tedRight: Result := Rect.ClipHeight - Rect.Left;
    tedLeft : Result := Rect.ClipHeight - Rect.Left;
    else Result := 0;
  end;
end;

function TTERects.TgtLeft(Rect: TTERect): Integer;
begin
  case TgtDir of
    tedDown : Result := Rect.Left;
    tedUp   : Result := Rect.ClipWidth - Rect.Right;
    tedRight: Result := Rect.Top;
    tedLeft : Result := Rect.ClipWidth - Rect.Bottom;
    else Result := 0;
  end;
end;

function TTERects.TgtRight(Rect: TTERect): Integer;
begin
  case TgtDir of
    tedDown : Result := Rect.Right;
    tedUp   : Result := Rect.ClipWidth - Rect.Left;
    tedRight: Result := Rect.Bottom;
    tedLeft : Result := Rect.ClipWidth - Rect.Top;
    else Result := 0;
  end;
end;

function TTERects.TgtTop(Rect: TTERect): Integer;
begin
  case TgtDir of
    tedDown : Result := Rect.Top;
    tedUp   : Result := Rect.ClipHeight - Rect.Bottom;
    tedRight: Result := Rect.ClipHeight - Rect.Right;
    tedLeft : Result := Rect.ClipHeight - Rect.Right;
    else Result := 0;
  end;
end;

{ TTERect }

constructor TTERect.Create(RectsValue: TTERects);
begin
  Rects   := RectsValue;
end;

function TTERect.GetBottom: Integer;
begin
  Result := R.Bottom;
end;

function TTERect.GetLeft: Integer;
begin
  Result := R.Left;
end;

function TTERect.GetRight: Integer;
begin
  Result := R.Right;
end;

function TTERect.GetTop: Integer;
begin
  Result := R.Top;
end;

function TTERect.Height: Integer;
begin
  Result := R.Bottom - R.Top;
end;

procedure TTERect.SetBottom(const Value: Integer);
begin
  if R.Bottom <> Value then
    R.Bottom := Value;
end;

procedure TTERect.SetLeft(const Value: Integer);
begin
  if R.Left <> Value then
    R.Left := Value;
end;

procedure TTERect.SetRight(const Value: Integer);
begin
  if R.Right <> Value then
    R.Right := Value;
end;

procedure TTERect.SetTop(const Value: Integer);
begin
  if R.Top <> Value then
    R.Top := Value;
end;

function TTERect.TgtBottom: Integer;
begin
  Result := Rects.TgtBottom(Self);
end;

function TTERect.TgtHeight: Integer;
var
  aux: TRect;
begin
  aux := TgtR;
  Result := aux.Bottom - aux.Top;
end;

function TTERect.TgtLeft: Integer;
begin
  Result := Rects.TgtLeft(Self);
end;

function TTERect.TgtR: TRect;
begin
  Result := Rect(TgtLeft, TgtTop, TgtRight, TgtBottom);
end;

function TTERect.UpdR: TRect;
var
  aux: TRect;
begin
  aux := TgtR;
  if aux.Left < aux.Right
  then
  begin
    Result.Left  := aux.Left;
    Result.Right := aux.Right;
  end
  else
  begin
    Result.Left  := aux.Right;
    Result.Right := aux.Left;
  end;
  if aux.Top < aux.Bottom
  then
  begin
    Result.Top    := aux.Top;
    Result.Bottom := aux.Bottom;
  end
  else
  begin
    Result.Top    := aux.Bottom;
    Result.Bottom := aux.Top;
  end;
end;

function TTERect.TgtRight: Integer;
begin
  Result := Rects.TgtRight(Self);
end;

function TTERect.TgtTop: Integer;
begin
  Result := Rects.TgtTop(Self);
end;

function TTERect.TgtWidth: Integer;
var
  aux: TRect;
begin
  aux := TgtR;
  Result := aux.Right - aux.Left;
end;

function TTERect.Width: Integer;
begin
  Result := R.Right - R.Left;
end;

initialization

  TERegisterTransition(TPageTransition);

end.
