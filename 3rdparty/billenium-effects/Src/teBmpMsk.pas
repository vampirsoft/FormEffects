unit teBmpMsk;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teTimed, Windows, Messages, Graphics, teBkgrnd;

type
  TBmpMaskTransition = class(TTimedTransitionEffect)
  private
    FMask: TBitmap;
    FMaskMode: TFCPictureMode;
    FSmoothingLevel: Word;

    procedure SetMask(Value: TBitmap);
    procedure SetSmoothingLevel(const Value: Word);
    procedure ExecuteSmooth(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Integer);
    procedure ExecuteStd(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Integer);
    procedure SetMaskMode(const Value: TFCPictureMode);
  protected
    LayeredMode: Boolean;

    function  GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
    function  GetBitmapsWidth(Data: TTETransitionData): Integer; override;
    function  Smooth(Device: TTETransitionDevice): Boolean;
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    destructor  Destroy; override;
    class function MaskModeAllowed(Mode: TFCPictureMode): Boolean;
    class function Description: String; override;
    class function GetEditor: String; override;
    function GetDelegate(Device: TTETransitionDevice;
      const ReturnCopy: Boolean): TTransitionEffect; override;
    procedure Assign(Source: TPersistent); override;
    function  MaxSmoothingLevel: Integer;
  published
    property Mask: TBitmap read FMask write SetMask;
    property MaskMode: TFCPictureMode read FMaskMode write SetMaskMode default fcpmStretch;
    property Pass2Options;
    property PassSetting;
    property Reversed;
    property SmoothingLevel: Word read FSmoothingLevel write SetSmoothingLevel default 0;
  end;

implementation

uses teRender, teMskWk;

resourcestring
  rsTEMaskNot8bit = 'Transition mask should have 8 bits per pixel';

type
  TBmpMaskData = class(TTECustomData)
  public
    Apply256BmpMaskSSubProc,
    Apply256BmpMaskSAddProc: PByteArray;
    IsSmooth: Boolean;
    MaskBmp: TBitmap;

    destructor Destroy; override;
  end;

var
  LevelsArray: array[1..5] of Integer = (8, 16, 32, 64, 128);

{ TBmpMaskTransition }

constructor TBmpMaskTransition.Create(AOwner: TComponent);
begin
  inherited;

  FMask           := TBitmap.Create;
  FSmoothingLevel := 0;
  FMaskMode       := fcpmStretch;
  LayeredMode     := False;
end;

destructor TBmpMaskTransition.Destroy;
begin
  FMask.Free;

  inherited;
end;

class function TBmpMaskTransition.Description: String;
begin
  Result := 'Bitmap mask';
end;

class function TBmpMaskTransition.GetEditor: String;
begin
  Result := 'TBmpMaskTransitionEditor';
end;

procedure TBmpMaskTransition.SetMask(Value: TBitmap);
begin
  if(Value <> nil) and (not Value.Empty) and (Value.PixelFormat <> pf8bit) then
    raise ETransitionEffectError.Create(rsTEMaskNot8bit);

  FMask.Assign(Value);
end;

procedure TBmpMaskTransition.Assign(Source: TPersistent);
begin
  if Source is TBmpMaskTransition
  then
  begin
    inherited;

    Mask.Assign((TBmpMaskTransition(Source).Mask));
    MaskMode       := TBmpMaskTransition(Source).MaskMode;
    SmoothingLevel := TBmpMaskTransition(Source).SmoothingLevel;
  end
  else inherited;
end;

function TBmpMaskTransition.GetPixelFormat(
  Device: TTETransitionDevice): TPixelFormat;
begin
  if LayeredMode or Smooth(Device)
  then Result := pf32bit
  else Result := Device.PixelFormat;
end;

function TBmpMaskTransition.GetBitmapsWidth(
  Data: TTETransitionData): Integer;
begin
  if Smooth(Data.Device)
  then Result := (((Data.Width-1) div  8) + 1) *  8
  else
  begin
    if Data.PixelFormat <> pf4bit
    then Result := (((Data.Width-1) div 4) + 1) * 4
    else Result := (((Data.Width-1) div 8) + 1) * 8;
  end;
end;

procedure TBmpMaskTransition.SetSmoothingLevel(const Value: Word);
begin
  if Value <= MaxSmoothingLevel then
    FSmoothingLevel := Value;
end;

function TBmpMaskTransition.Smooth(Device: TTETransitionDevice): Boolean;
begin
  Result :=
     TEProcessorInfo.MMX and
    (SmoothingLevel > 0) and
    (Device.PixelFormat in [pf15bit, pf16bit, pf24bit, pf32bit]);
end;

procedure TBmpMaskTransition.Initialize(Data: TTETransitionData; var
  TotalFrames: Longint);
var
  BmpMaskData: TBmpMaskData;
  R: TRect;
begin
  inherited;

  if Mask.PixelFormat <> pf8bit then
    raise ETransitionEffectError.Create(rsTEMaskNot8bit);

  TotalFrames := 254;

  BmpMaskData := TBmpMaskData.Create(Data);
  Data.Custom := BmpMaskData;
  BmpMaskData.Apply256BmpMaskSSubProc := nil;
  BmpMaskData.Apply256BmpMaskSAddProc := nil;
  BmpMaskData.IsSmooth := Smooth(Data.Device);

  if BmpMaskData.IsSmooth then
  begin
    Inc(TotalFrames, LevelsArray[SmoothingLevel]-1);
    BmpMaskData.Apply256BmpMaskSSubProc := GetApply256BmpMaskSProc(
      LevelsArray[SmoothingLevel], False);
    BmpMaskData.Apply256BmpMaskSAddProc := GetApply256BmpMaskSProc(
      LevelsArray[SmoothingLevel], True);
  end;

  BmpMaskData.MaskBmp := TBitmap.Create;
  BmpMaskData.MaskBmp.Canvas.Lock;
  AdjustBmpForTransition(BmpMaskData.MaskBmp, CopyPalette(Mask.Palette),
    Data.Bitmap.Width, Data.Height, pf8bit);
  SetStretchBltMode(Data.Canvas.Handle, COLORONCOLOR);
  if Reversed then
    BmpMaskData.MaskBmp.Canvas.CopyMode := cmNotSrcCopy;
  Mask.Canvas.Lock;
  try
    R := Rect(0, 0, Data.Width, Data.Height);
    DrawPicture(Mask, FMaskMode, clNone, BmpMaskData.MaskBmp, R, R, R, 0);
  finally
    Mask.Canvas.Unlock;
  end;
  if Reversed then
    BmpMaskData.MaskBmp.Canvas.CopyMode := cmSrcCopy;
end;

procedure TBmpMaskTransition.ExecuteFrame(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Longint);
begin
  if not TBmpMaskData(Data.Custom).IsSmooth
  then ExecuteStd   (Data, CurrentFrame, Step, LastExecutedFrame)
  else ExecuteSmooth(Data, CurrentFrame, Step, LastExecutedFrame);
end;

procedure TBmpMaskTransition.ExecuteStd(Data: TTETransitionData; CurrentFrame,
  Step, LastExecutedFrame: Integer);
var
  Work,
  Dst,
  MaskP: Pointer;
  ScanLineSize,
  MaskScanLineSize: Longint;
begin
  Data.UpdateRect   := Rect(0, 0, Data.Width, Data.Height);
  Data.UnUpdateRect := Rect(0, 0, 0, 0);
  MaskScanLineSize := GetBytesPerScanline(TBmpMaskData(Data.Custom).MaskBmp, pf8bit, 32);
  ScanLineSize     := GetBytesPerScanline(Data.Bitmap, Data.PixelFormat, 32);
  Work  := PAnsiChar(Data.Bitmap.ScanLine[0]) + ScanlineSize;
  Dst   := PAnsiChar(Data.DstBmp.ScanLine[0]) + ScanlineSize;
  MaskP := PAnsiChar(TBmpMaskData(Data.Custom).MaskBmp.ScanLine[0]) + MaskScanLineSize;

  if LayeredMode
  then ApplyLayered256Mask(Work, Dst, MaskP, MaskScanLineSize * Data.Height,
         255 - CurrentFrame, 255 - LastExecutedFrame, Data.PixelFormat)
  else Apply256Mask(Work, Dst, MaskP, MaskScanLineSize * Data.Height,
         255 - CurrentFrame, 255 - LastExecutedFrame, Data.PixelFormat);
end;

procedure TBmpMaskTransition.ExecuteSmooth(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Integer);
var
  Work,
  Dst,
  Src,
  MaskP: Pointer;
  ScanLineSize,
  MaskScanLineSize,
  Dif: Longint;
  BmpMaskData: TBmpMaskData;
begin
  BmpMaskData := TBmpMaskData(Data.Custom);
  Data.UpdateRect   := Rect(0, 0, Data.Width, Data.Height);
  Data.UnUpdateRect := Rect(0, 0, 0, 0);
  MaskScanLineSize := GetBytesPerScanline(BmpMaskData.MaskBmp, pf8bit, 32);
  ScanLineSize     := GetBytesPerScanline(Data.Bitmap, Data.PixelFormat, 32);
  Work  := PAnsiChar(Data.Bitmap.ScanLine[0]) + ScanlineSize;
  Dst   := PAnsiChar(Data.DstBmp.ScanLine[0]) + ScanlineSize;
  Src   := PAnsiChar(Data.SrcBmp.ScanLine[0]) + ScanlineSize;
  MaskP := PAnsiChar(BmpMaskData.MaskBmp.ScanLine[0]) + MaskScanLineSize;

  Dif := 255 - CurrentFrame;
  Apply256BmpMaskS(TApply256BmpMaskSProc(TBmpMaskData(Data.Custom).Apply256BmpMaskSSubProc),
    TApply256BmpMaskSProc(BmpMaskData.Apply256BmpMaskSAddProc), Work, Dst, Src,
    MaskP, ScanLineSize, MaskScanLineSize, BmpMaskData.MaskBmp.Width,
    Dif, Data.UpdateRect, Data.UnUpdateRect);
end;

function TBmpMaskTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiNeedOffScreenBmp,
      tetiOffScreenBmpCapable,
      tetiStaticSrcPixels,
      tetiThreadSafe,
      tetiLayeredCapable
    ];
end;

function TBmpMaskTransition.MaxSmoothingLevel: Integer;
begin
  Result := 5;
end;

procedure TBmpMaskTransition.SetMaskMode(const Value: TFCPictureMode);
begin
  if(Value <> FMaskMode) and MaskModeAllowed(Value) then
    FMaskMode := Value;
end;

class function TBmpMaskTransition.MaskModeAllowed(
  Mode: TFCPictureMode): Boolean;
begin
  Result := Mode in [fcpmStretch, fcpmTile, fcpmZoom];
end;

function TBmpMaskTransition.GetDelegate(Device: TTETransitionDevice;
  const ReturnCopy: Boolean): TTransitionEffect;
begin
  if not Mask.Empty
  then Result := inherited GetDelegate(Device, ReturnCopy)
  else
  begin
    Result := TFlickerFreeTransition.Create(nil);
    Result.Assign(Self);
  end;
end;

procedure TBmpMaskTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
  BinStream: TMemoryStream;
begin
  inherited;

  Value := List.Values[Prefix + 'MaskMode'];
  if Value <> '' then
    MaskMode := TEGetPictureModeFromDesc(Value);

  Value := List.Values[Prefix + 'SmoothingLevel'];
  if Value <> '' then
    SmoothingLevel := StrToInt(Value);

  Value := List.Values[Prefix + 'Mask'];
  if Value <> '' then
  begin
    BinStream := TMemoryStream.Create;
    try
      Assert(Value[1] = '0'); // Storing method id -> hexadecimal;
      BinStream.SetSize((Length(Value)-1) div 2);
      HexToBin(PChar(@Value[2]), BinStream.Memory, BinStream.Size);
      Mask.LoadFromStream(BinStream);
    finally
      BinStream.Free;
    end;
  end;
end;

procedure TBmpMaskTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
var
  BinStream: TMemoryStream;
  MaskTxt: PChar;
begin
  inherited;

  if(not OmitDefaultValues) or (MaskMode <> fcpmStretch) then
    List.Values[Prefix + 'MaskMode'] := TEGetPictureModeDesc(MaskMode);

  if(not OmitDefaultValues) or (SmoothingLevel <> 0) then
    List.Values[Prefix + 'SmoothingLevel'] := IntToStr(SmoothingLevel);

  if(not OmitDefaultValues) or (not Mask.Empty) then
  begin
    BinStream := TMemoryStream.Create;
    try
      Mask.SaveToStream(BinStream);
      GetMem(MaskTxt, 1+(BinStream.Size*2)+1);
      BinToHex(BinStream.Memory, PChar(@MaskTxt[1]), BinStream.Size);
      MaskTxt[0] := '0'; // Storing method id -> hexadecimal;
      MaskTxt[1+(BinStream.Size*2)] := Char(0); // Storing null value;
    finally
      BinStream.Free;
    end;
    List.Values[Prefix + 'Mask'] := MaskTxt;
  end;
end;

{ TBmpMaskData }

destructor TBmpMaskData.Destroy;
begin
  if Assigned(Apply256BmpMaskSSubProc) then
    VirtualFree(Apply256BmpMaskSSubProc, 0, MEM_RELEASE);
  if Assigned(Apply256BmpMaskSAddProc) then
    VirtualFree(Apply256BmpMaskSAddProc, 0, MEM_RELEASE);

  MaskBmp.Canvas.Unlock;
  MaskBmp.Free;

  inherited;
end;

initialization

  TERegisterTransition(TBmpMaskTransition);

end.
