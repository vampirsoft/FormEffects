unit teBlur;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teTimed, teRender, Windows, Messages, Graphics,
  teMskWk;

type
  TBlurTransition = class(TTimedTransitionEffect)
  private
    FRadius: Byte;
    procedure PaintBlur(Work: PByteArray; k, RowLenght, RowGap, Pixels, BoxSize:
      Integer; BGR: PDWordArray);
    procedure SetRadius(const Value: Byte);
  protected
    procedure Initialize(Data: TTETransitionData; var Frames: Longint); override;
    function  CalcTotalFrames(Data: TTETransitionData): Longint;
    function  GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
    procedure ExecuteFrame(Data: TTETransitionData;
      CurrentFrame, Step, LastExecutedFrame: Longint); override;
    procedure DoPrecomputation(Data: TTETransitionData; Bmp: TBitmap);
    procedure BlurBmp(Bitmap: TBitmap; Data: TTETransitionData;
      BoxSize: Integer);
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    procedure Assign(Source: TPersistent); override;
    function GetDelegate(Device: TTETransitionDevice;
      const ReturnCopy: Boolean): TTransitionEffect; override;
    class function Description: String; override;
    class function GetEditor: String; override;
  published
    property Pass2Options;
    property PassSetting;
    property Radius: Byte read FRadius write SetRadius;
    property Reversed;
  end;

implementation

uses teFuse;

type
  TBlurData = class(TTECustomData)
  public
    BGRImg: PDWordArray;
    Pass1: Boolean;
    Pass2Frame: Integer;

    destructor Destroy; override;
  end;

const
  BPP = 3;
   
{ TBlurTransition }

constructor TBlurTransition.Create(AOwner: TComponent);
begin
  inherited;

  FRadius := 50;
end;

procedure TBlurTransition.Assign(Source: TPersistent);
begin
  if Source is TBlurTransition
  then
  begin
    inherited;

    Radius := TBlurTransition(Source).Radius;
  end
  else inherited;
end;

function TBlurTransition.CalcTotalFrames(Data: TTETransitionData): Longint;
begin
  Result := Radius * 2;
end;

class function TBlurTransition.Description: String;
begin
  Result := 'Blur';
end;

function TBlurTransition.GetPixelFormat(Device: TTETransitionDevice):
  TPixelFormat;
begin
  Result := pf24bit;
end;

procedure TBlurTransition.Initialize(Data: TTETransitionData;
  var Frames: Integer);
var
  BlurData: TBlurData;
begin
  inherited;

  BlurData    := TBlurData.Create(Data);
  Data.Custom := BlurData;
  GetMem(BlurData.BGRImg,
    (Data.Width+(Radius*2)+1)*(Data.Height+(Radius*2)+1) * 4 * 3);

  DoPrecomputation(Data, Data.SrcBmp);
  BlurData.Pass1 := True;

  Frames := CalcTotalFrames(Data);
  BlurData.Pass2Frame := (Frames div 2) + 1;
end;

procedure TBlurTransition.DoPrecomputation(Data: TTETransitionData;
  Bmp: TBitmap);
const
  PixelFormatGap = 1;
var
  x,
  y,
  RowSize,
  IndexWork,
  Index,
  BTot,
  GTot,
  RTot: Integer;
  Work: PByteArray;
  BlurData: TBlurData;
begin
  BlurData := TBlurData(Data.Custom);
  RowSize := (Data.Width + Radius + Radius + 1) * 3;
  Index   := 0;
	for y:=-Radius-1 to Data.Height+Radius-1 do
  begin
    if(y >= 0) and (y<Data.Height)
    then
    begin
      Work      := PByteArray(PByte(Bmp.ScanLine[Data.Height-y-1]));
      IndexWork := 0;
    end
    else
    begin
      Work      := nil;
      IndexWork := -1;
    end;
    for x:=-Radius-1 to Data.Width+Radius-1 do
    begin
      if(x >= 0) and (y >= 0) and (x < Data.Width-1) and (y < Data.Height-1)
      then
      begin
        BTot := Work[IndexWork];
        Inc(IndexWork);
        GTot := Work[IndexWork];
        Inc(IndexWork);
        RTot := Work[IndexWork];
        Inc(IndexWork, PixelFormatGap);
      end
      else
      begin
        BTot := 255;
        GTot := 255;
        RTot := 255;
      end;

      if x > -Radius-1 then
      begin
        Inc(BTot, BlurData.BGRImg[Index-3]);
        Inc(GTot, BlurData.BGRImg[Index-2]);
        Inc(RTot, BlurData.BGRImg[Index-1]);
      end;
      if y > -Radius-1 then
      begin
        Inc(BTot, BlurData.BGRImg[Index-RowSize  ]);
        Inc(GTot, BlurData.BGRImg[Index-RowSize+1]);
        Inc(RTot, BlurData.BGRImg[Index-RowSize+2]);

        if x > -Radius-1 then
        begin
          Dec(BTot, BlurData.BGRImg[Index-RowSize-3]);
          Dec(GTot, BlurData.BGRImg[Index-RowSize-2]);
          Dec(RTot, BlurData.BGRImg[Index-RowSize-1]);
        end;
      end;
      BlurData.BGRImg[Index  ] := BTot;
      BlurData.BGRImg[Index+1] := GTot;
      BlurData.BGRImg[Index+2] := RTot;
      Inc(Index, 3);
    end;
  end;
end;

procedure TBlurTransition.ExecuteFrame(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Integer);
var
  BlurData: TBlurData;
begin
  BlurData := TBlurData(Data.Custom);
  if CurrentFrame < BlurData.Pass2Frame
  then BlurBmp(Data.Bitmap, Data, CurrentFrame)
  else
  begin
    if BlurData.Pass1 then
    begin
      BlurData.Pass1 := False;
      DoPrecomputation(Data, Data.DstBmp);
    end;
    BlurBmp(Data.Bitmap, Data, Data.Frames - CurrentFrame);
  end;
  Data.UpdateRect := Rect(0, 0, Data.Width, Data.Height);
end;
{
procedure TBlurTransition.PaintBlur(Work: PByteArray; k, RowLenght, RowGap,
  Pixels, BoxSize: Integer);
var
  aux: DWord;
  Limit,
  RowSize,
  MaxRatio2,
  Gap,
  Index1,
  Index2,
  Index3,
  Index4: Integer;
begin
  MaxRatio2 := Radius * 2 + 1;
  Gap       := MaxRatio2 * 3;
  aux       := ((BoxSize * 2) + 1) * ((BoxSize * 2) + 1);
  RowSize   := Pixels + MaxRatio2;
  Index1    := (FRadius - BoxSize) * (RowSize + 1);
  Index2    := Index1 + (((BoxSize * 2) + 1) * (RowSize + 1));
  Index3    := Index2 - ((BoxSize * 2) + 1);
  Index4    := Index1 + Index2 - Index3;
  Index1    := Index1 * 3;
  Index2    := Index2 * 3;
  Index3    := Index3 * 3;
  Index4    := Index4 * 3;

  while k < 0 do
  begin
    Limit := k + RowLenght;
    while k < Limit do
    begin
      Work[k  ] :=
        (
          BGRImg[Index1] +
          BGRImg[Index2] -
          BGRImg[Index3] -
          BGRImg[Index4]
        ) div aux;
      Work[k+1] :=
        (
          GImg[Index1] +
          GImg[Index2] -
          GImg[Index3] -
          GImg[Index4]
        ) div aux;
      Work[k+2] :=
        (
          RImg[Index1] +
          RImg[Index2] -
          RImg[Index3] -
          RImg[Index4]
        ) div aux;

      Inc(k, BPP); // BPP bytes por pixel
      Inc(Index1, 3);
      Inc(Index2, 3);
      Inc(Index3, 3);
      Inc(Index4, 3);
    end;
    Inc(Index1, Gap);
    Inc(Index2, Gap);
    Inc(Index3, Gap);
    Inc(Index4, Gap);
    Inc(k, RowGap);
  end;
end;
}
procedure TBlurTransition.BlurBmp(Bitmap: TBitmap; Data: TTETransitionData;
  BoxSize: Integer);
var
  ScanLineSize: Integer;
  Work : PAnsiChar;
  UpdParams: TTEUpdParams;
  BlurData: TBlurData;
begin
  BlurData := TBlurData(Data.Custom);
  ScanLineSize := GetBytesPerScanline(Bitmap, Bitmap.PixelFormat, 32);
  GiveMeTheUpdParams(2, UpdParams, ScanLineSize,
    Rect(0, 0, Data.Width, Data.Height), Rect(0, 0, 0, 0), Bitmap.PixelFormat);
  Work         := PAnsiChar(Bitmap.ScanLine[0]) + ScanlineSize - UpdParams.Start1;

  PaintBlur(
    PByteArray(Work),
    -UpdParams.LenghtBytes1,
    UpdParams.RowLenght1 * BPP,
    UpdParams.GapBytes1,
    Data.Width,
    BoxSize,
    BlurData.BGRImg);
end;

function TBlurTransition.GetDelegate(Device: TTETransitionDevice;
  const ReturnCopy: Boolean): TTransitionEffect;
var
  Transition: TTransitionEffect;
begin
  Result := nil;
  if Device.IsRGB
  then Result := inherited GetDelegate(Device, ReturnCopy)
  else
  begin
    Transition := TFuseTransition.Create(nil);
    try
      Transition.Assign(Self);
      TFuseTransition(Transition).Style := 1;
      Result := Transition.GetDelegate(Device, False);
      if Result <> Transition then
        Transition.Free;
    finally
      if Result <> Transition then
        Transition.Free;
    end;
  end;
end;

procedure TBlurTransition.PaintBlur(Work: PByteArray; k, RowLenght, RowGap,
  Pixels, BoxSize: Integer; BGR: PDWordArray);
var
  aux: DWord;
  Limit,
  RowSize,
  MaxRatio,
  Gap,
  Index1,
  Index2,
  Index3,
  Index4: Integer;
  BGR1,
  BGR2,
  BGR3,
  BGR4: PDWordArray;
begin
  MaxRatio := Radius * 2 + 1;
  Gap      := MaxRatio * 3;
  aux      := ((BoxSize * 2) + 1) * ((BoxSize * 2) + 1);
  RowSize  := Pixels + MaxRatio;
  Index1   := (FRadius - BoxSize) * (RowSize + 1);
  Index2   := Index1 + (((BoxSize * 2) + 1) * (RowSize + 1));
  Index3   := Index2 - ((BoxSize * 2) + 1);
  Index4   := Index1 + Index2 - Index3;
  Index1   := Index1 * 3;
  Index2   := Index2 * 3;
  Index3   := Index3 * 3;
  Index4   := Index4 * 3;

  // Relative adjustment for using a single index
  Gap := (Gap - RowGap) * 4;
  BGR1   := PDWordArray(PAnsiChar(BGR) + (Index1 - k) * 4);
  BGR2   := PDWordArray(PAnsiChar(BGR) + (Index2 - k) * 4);
  BGR3   := PDWordArray(PAnsiChar(BGR) + (Index3 - k) * 4);
  BGR4   := PDWordArray(PAnsiChar(BGR) + (Index4 - k) * 4);

  while k < 0 do
  begin
    Limit := k + RowLenght;
    while k < Limit do
    begin
      Work[k  ] := (BGR1[k  ] + BGR2[k  ] - BGR3[k  ] - BGR4[k  ]) div aux;
      Work[k+1] := (BGR1[k+1] + BGR2[k+1] - BGR3[k+1] - BGR4[k+1]) div aux;
      Work[k+2] := (BGR1[k+2] + BGR2[k+2] - BGR3[k+2] - BGR4[k+2]) div aux;
      Inc(k, BPP); // BPP bytes per pixel
    end;
    BGR1 := PDWordArray(PAnsiChar(BGR1) + Gap);
    BGR2 := PDWordArray(PAnsiChar(BGR2) + Gap);
    BGR3 := PDWordArray(PAnsiChar(BGR3) + Gap);
    BGR4 := PDWordArray(PAnsiChar(BGR4) + Gap);
    Inc(k, RowGap);
  end;
end;

procedure TBlurTransition.SetRadius(const Value: Byte);
begin
  if(Value >= 1) and (Value <= 50) then
    FRadius := Value;
end;

{
procedure TBlurTransition.PaintBlur(Work: PByteArray; k, RowLenght, RowGap,
  Pixels, BoxSize: Integer; G, R, B: PDWordArray);
var
  aux: DWord;
  Limit,
  RowSize,
  MaxRatio,
  Gap,
  Index1,
  Index2,
  Index3,
  Index4: Integer;
  G1,
  G2,
  G3,
  G4,
  R1,
  R2,
  R3,
  R4,
  B1,
  B2,
  B3,
  B4: PDWordArray;
begin
  MaxRatio := Radius * 2 + 1;
  Gap      := MaxRatio * 3;
  aux      := ((BoxSize * 2) + 1) * ((BoxSize * 2) + 1);
  RowSize  := Pixels + MaxRatio;
  Index1   := (FRadius - BoxSize) * (RowSize + 1);
  Index2   := Index1 + (((BoxSize * 2) + 1) * (RowSize + 1));
  Index3   := Index2 - ((BoxSize * 2) + 1);
  Index4   := Index1 + Index2 - Index3;
  Index1   := Index1 * 3;
  Index2   := Index2 * 3;
  Index3   := Index3 * 3;
  Index4   := Index4 * 3;

  // Relative adjustment for using a single index
  Gap := (Gap * 4) - RowGap;
  B1   := PDWordArray(PByte(B) + (Index1 - k) * 4);
  G1   := PDWordArray(PByte(G) + (Index1 - k) * 4);
  R1   := PDWordArray(PByte(R) + (Index1 - k) * 4);
  B2   := PDWordArray(PByte(B) + (Index2 - k) * 4);
  G2   := PDWordArray(PByte(G) + (Index2 - k) * 4);
  R2   := PDWordArray(PByte(R) + (Index2 - k) * 4);
  B3   := PDWordArray(PByte(B) + (Index3 - k) * 4);
  G3   := PDWordArray(PByte(G) + (Index3 - k) * 4);
  R3   := PDWordArray(PByte(R) + (Index3 - k) * 4);
  B4   := PDWordArray(PByte(B) + (Index4 - k) * 4);
  G4   := PDWordArray(PByte(G) + (Index4 - k) * 4);
  R4   := PDWordArray(PByte(R) + (Index4 - k) * 4);

  while k < 0 do
  begin
    Limit := k + RowLenght;
    while k < Limit do
    begin
      Work[k  ] := (B1[k] + B2[k] - B3[k] - B4[k]) div aux;
      Work[k+1] := (G1[k] + G2[k] - G3[k] - G4[k]) div aux;
      Work[k+2] := (R1[k] + R2[k] - R3[k] - R4[k]) div aux;
      Inc(k, BPP); // BPP bytes por pixel
    end;
    B1 := PDWordArray(PByte(B1) + Gap);
    G1 := PDWordArray(PByte(G1) + Gap);
    R1 := PDWordArray(PByte(R1) + Gap);
    B2 := PDWordArray(PByte(B2) + Gap);
    G2 := PDWordArray(PByte(G2) + Gap);
    R2 := PDWordArray(PByte(R2) + Gap);
    B3 := PDWordArray(PByte(B3) + Gap);
    G3 := PDWordArray(PByte(G3) + Gap);
    R3 := PDWordArray(PByte(R3) + Gap);
    B4 := PDWordArray(PByte(B4) + Gap);
    G4 := PDWordArray(PByte(G4) + Gap);
    R4 := PDWordArray(PByte(R4) + Gap);
    Inc(k, RowGap);
  end;
end;
}
{
procedure TBlurTransition.EmulateBlurBmp(Bitmap: TBitmap;
  Data: TTETransitionData; BoxSize: Integer);
type
  TDoubleArray = array[0..32767] of Double;
  PDoubleArray = ^TDoubleArray;
var
  nx, ny, nb, i: Integer;
  xx, yy, bb: PDoubleArray;
  x,
  y,
  IndexWork: Integer;
  Work: PByteArray;
begin
  GetMem(xx, (Bitmap.Width+(Radius*2)+1)*(Data.Height+(Radius*2)+1) * 4 * 3);
  GetMem(yy, (Bitmap.Width+(Radius*2)+1)*(Data.Height+(Radius*2)+1) * 4 * 3);
  GetMem(bb, (Bitmap.Width+(Radius*2)+1)*(Data.Height+(Radius*2)+1) * 4 * 3);

  i := 1;
  for y:=1 to Bitmap.Height do
  begin
    Work      := PByteArray(PByte(Bitmap.ScanLine[y-1]));
    IndexWork := 0;
    for x:=1 to Bitmap.Width do
    begin
      xx[i] := Work[IndexWork];
      Inc(IndexWork, 4);
      Inc(i);
    end;
  end;

  nb := BoxSize;
  nx := Bitmap.Width*Bitmap.Height;
  ny := nx+nb-1;
  for i:=1 to ny do
    bb[i] := 0.;
  bb[1] := xx[1];
  for i:=2 to nx do
    bb[i] := bb[i-1] + xx[i];
  for i:=nx+1 to ny do
    bb[i] := bb[i-1];
  for i:=1 to nb do
    yy[i] := bb[i];
  for i:=nb+1 to ny do
    yy[i] := bb[i] - bb[i-nb];
  for i:=1 to ny do
    yy[i] := yy[i] / nb;

  i := 1;
  for y:=1 to Bitmap.Height do
  begin
    Work      := PByteArray(PByte(Bitmap.ScanLine[y-1]));
    IndexWork := 0;
    for x:=1 to Bitmap.Width do
    begin
      Work[IndexWork] := Trunc(yy[i]);
      Inc(IndexWork, 4);
      Inc(i);
    end;
  end;
end;
}
class function TBlurTransition.GetEditor: String;
begin
  Result := 'TBlurTransitionEditor';
end;

function TBlurTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiNeedOffScreenBmp,
      tetiOffScreenBmpCapable,
      tetiThreadSafe
    ];
end;

destructor TBlurData.Destroy;
begin
  FreeMem(BGRImg);

  inherited;
end;

procedure TBlurTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'Radius'];
  if Value <> '' then
    Radius := StrToInt(Value);
end;

procedure TBlurTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
begin
  inherited;

  List.Values[Prefix + 'Radius'] := IntToStr(Radius);
end;

initialization

  TERegisterTransition(TBlurTransition);
  RegisterClasses([TBlurTransition]);

end.
