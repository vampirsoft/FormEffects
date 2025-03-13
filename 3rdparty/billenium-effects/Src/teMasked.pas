unit teMasked;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teChrono, teTimed, Windows, Messages, Graphics,
  teRender;

type
  TMaskedTransition = class(TTimedTransitionEffect)
  private
    FStyle,
    FSubStyle,
    RandomStyle,
    RandomSubStyle,
    FSmoothingLevel: Word;
    Apply256MaskSProc: PByteArray;

    procedure SetStyle(const Value: Word);
    procedure SetSubStyle(const Value: Word);
    procedure SetSmoothingLevel(const Value: Word);
  protected
    FCountOfStyles: Word;
    Frame1bppMaskBmp,
    Frame1bppMaskBmp2,
    Frame8bppMaskBmp,
    Frame8bppMaskBmp2: TBitmap;
    IsSmooth: Boolean;
    StepLevel,
    Levels,
    MaxLevel,
    MinLevel,
    TotalFramesNoSmooth: Integer;

    function  StyleToUse: Word;
    function  SubStyleToUse: Word;
    function  CalculateReversedSubStyle(
      const StyleValue, SubStyleValue: Word): Word; virtual;
    function  GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
    function  GetBitmapsWidth(Data: TTETransitionData): Integer; override;
    function  CalcTotalFrames(Data: TTETransitionData): Longint; virtual; abstract;
    procedure MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step, LastExecutedFrame:
      Longint; Data: TTETransitionData; Draw, CalcDirtyRects: Boolean); virtual;
      abstract;
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure Finalize(Data: TTETransitionData); override;
    function ExecuteFrame1bpp(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint; CurrentMask, PreviousMask: TBitmap; SmoothValue,
      Draw, CalcDirtyRects: Boolean): Boolean;
    procedure ExecuteFrame8bpp(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint);
    procedure ExecuteFrame(Data: TTETransitionData;
      CurrentFrame, Step, LastExecutedFrame: Longint); override;
    function  OptimizeMask: Boolean; virtual;
    function  ResetMaskBmp(Device: TTETransitionDevice): Boolean; virtual;
    function  InversePaint: Boolean; virtual;
    function  AvoidPixelRepaint: Boolean; virtual;
    function  GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    function  Smooth(Device: TTETransitionDevice): Boolean; virtual;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;

    procedure Assign(Source: TPersistent); override;
    function  CountOfSubStyles(StyleValue: Word): Word; virtual;
    class function GetEditor: String; override;
    function  MaxSmoothingLevel: Integer;

    property CountOfStyles: Word read FCountOfStyles;
    property Style: Word read FStyle write SetStyle default 1;
    property SubStyle: Word read FSubStyle write SetSubStyle default 1;
    property SmoothingLevel: Word read FSmoothingLevel write SetSmoothingLevel default 0;
  published
    property Pass2Options;
    property PassSetting;
  end;

implementation

uses TypInfo, teMskWk;

var
  LevelsArray: array[1..7] of Byte = (31, 16, 8, 4, 3, 2, 1);

constructor TMaskedTransition.Create(AOwner: TComponent);
begin
  inherited;

  Frame1bppMaskBmp         := nil;
  Frame1bppMaskBmp2        := nil;
  Frame8bppMaskBmp         := nil;
  Frame8bppMaskBmp2        := nil;
  Apply256MaskSProc        := nil;
  FSmoothingLevel          := 0;
  FCountOfStyles           := 1;
  FStyle                   := 1;
  FSubStyle                := 1;
  RandomStyle              := 0;
  RandomSubStyle           := 0;
end;

function TMaskedTransition.OptimizeMask: Boolean;
begin
  Result := False;
end;

function TMaskedTransition.ResetMaskBmp(Device: TTETransitionDevice): Boolean;
begin
  Result := False;
end;

function TMaskedTransition.InversePaint: Boolean;
begin
  Result := False;
end;

function TMaskedTransition.AvoidPixelRepaint: Boolean;
begin
  Result := False;
end;

class function TMaskedTransition.GetEditor: String;
begin
  Result := 'TMaskedTransitionEditor';
end;

function TMaskedTransition.GetBitmapsWidth(Data: TTETransitionData): Integer;
begin
  if Smooth(Data.Device)
  then Result := (((Data.Width-1) div  8) + 1) *  8
  else Result := (((Data.Width-1) div 32) + 1) * 32;
end;

function TMaskedTransition.GetPixelFormat(
  Device: TTETransitionDevice): TPixelFormat;
begin
  Result := Device.PixelFormat;
  if(Result <> pf32bit) and Smooth(Device) then
    Result := pf32bit;
end;

function TMaskedTransition.Smooth(Device: TTETransitionDevice): Boolean;
begin
  Result := TEProcessorInfo.MMX and (SmoothingLevel > 0) and Device.IsRGB;
end;

procedure TMaskedTransition.Initialize(Data: TTETransitionData; var
  TotalFrames: Longint);
begin
  inherited;

  IsSmooth := Smooth(Data.Device);
  TotalFrames := CalcTotalFrames(Data);
  TotalFramesNoSmooth := TotalFrames;
  if IsSmooth
  then
  begin
    StepLevel := LevelsArray[SmoothingLevel];
    Levels    := 255 div StepLevel;
    MaxLevel  := 254 - ((255 - (((Levels-1) * StepLevel) + 1)) div 2);
    MinLevel  := MaxLevel - ((Levels - 1) * StepLevel);

    Inc(TotalFrames, Levels-1);
    Frame8bppMaskBmp  := TBitmap.Create;
    Frame8bppMaskBmp.Canvas.Lock;
    Frame8bppMaskBmp .PixelFormat := pf8bit;
    Frame8bppMaskBmp .Palette     := CreateGrayScalePalette;
    Frame8bppMaskBmp .Width       := GetBitmapsWidth(Data);
    Frame8bppMaskBmp .Height      := Data.Height;
    Frame8bppMaskBmp.Canvas.Pen  .Color := $02000000 or RGB(255, 255, 255);
    Frame8bppMaskBmp.Canvas.Brush.Color := Frame8bppMaskBmp.Canvas.Pen.Color;
    Frame8bppMaskBmp.Canvas.FillRect(Rect(0, 0, Data.Width, Data.Height));
    if AvoidPixelRepaint then
    begin
      Frame8bppMaskBmp2 := TBitmap.Create;
      Frame8bppMaskBmp2.Canvas.Lock;
      Frame8bppMaskBmp2.PixelFormat := pf8bit;
      Frame8bppMaskBmp2.Palette     := CreateGrayScalePalette;
      Frame8bppMaskBmp2.Width       := Frame8bppMaskBmp.Width;
      Frame8bppMaskBmp2.Height      := Data.Height;
      Frame8bppMaskBmp2.Canvas.Pen  .Color := Frame8bppMaskBmp.Canvas.Pen.Color;
      Frame8bppMaskBmp2.Canvas.Brush.Color := Frame8bppMaskBmp.Canvas.Pen.Color;
      Frame8bppMaskBmp2.Canvas.FillRect(Rect(0, 0, Data.Width, Data.Height));
    end;

    Apply256MaskSProc := GetApply256MaskSProc;
  end
  else
  begin
    Frame1bppMaskBmp := TBitmap.Create;
    Frame1bppMaskBmp.Canvas.Lock;
    Frame1bppMaskBmp.Width  := GetBitmapsWidth(Data);
    Frame1bppMaskBmp.Height := Data.Height;
    Frame1bppMaskBmp.PixelFormat := pf1bit;
    Frame1bppMaskBmp.Canvas.Pen  .Color := clBlack;
    Frame1bppMaskBmp.Canvas.Brush.Color := clBlack;
    FillRect(Frame1bppMaskBmp.Canvas.Handle,
      Rect(0, 0, Data.Width, Data.Height), GetStockObject(WHITE_BRUSH));

    if OptimizeMask then
    begin
      Frame1bppMaskBmp2 := TBitmap.Create;
      Frame1bppMaskBmp2.Canvas.Lock;
      Frame1bppMaskBmp2.Width  := Frame1bppMaskBmp.Width;
      Frame1bppMaskBmp2.Height := Data.Height;
      Frame1bppMaskBmp2.PixelFormat := pf1bit;
      FillRect(Frame1bppMaskBmp2.Canvas.Handle,
        Rect(0, 0, Data.Width, Data.Height), GetStockObject(WHITE_BRUSH));
    end;
  end;

  if IsSmooth and not AvoidPixelRepaint and Assigned(Data.DirtyRects) then
    Data.DirtyRects.AutoClear := False;
end;

procedure TMaskedTransition.Finalize(Data: TTETransitionData);
begin
  if Assigned(Apply256MaskSProc) then
  begin
    VirtualFree(Apply256MaskSProc, 0, MEM_RELEASE);
    Apply256MaskSProc := nil;
  end;

  if Assigned(Frame1bppMaskBmp ) then
  begin
    Frame1bppMaskBmp .Canvas.Unlock;
    FreeAndNil(Frame1bppMaskBmp );
  end;
  if Assigned(Frame1bppMaskBmp2) then
  begin
    Frame1bppMaskBmp2.Canvas.Unlock;
    FreeAndNil(Frame1bppMaskBmp2);
  end;
  if Assigned(Frame8bppMaskBmp ) then
  begin
    Frame8bppMaskBmp .Canvas.Unlock;
    FreeAndNil(Frame8bppMaskBmp );
  end;
  if Assigned(Frame8bppMaskBmp2) then
  begin
    Frame8bppMaskBmp2.Canvas.Unlock;
    FreeAndNil(Frame8bppMaskBmp2);
  end;

  if(Data.PassCount = 1) or (Data.Pass = 2) then
  begin
    RandomStyle    := 0;
    RandomSubStyle := 0;
  end;

  inherited;
end;

procedure TMaskedTransition.SetStyle(const Value: Word);
begin
  if(FStyle <> Value) and (Value <= CountOfStyles) then
  begin
    FStyle := Value;
    if FStyle > 0
    then SubStyle := 1
    else SubStyle := 0;
  end;
end;

procedure TMaskedTransition.SetSubStyle(const Value: Word);
begin
  if(FSubStyle <> Value) and (Value <= CountOfSubStyles(FStyle)) then
  begin
    FSubStyle := Value;
  end;
end;

procedure TMaskedTransition.SetSmoothingLevel(const Value: Word);
begin
  if Value <= MaxSmoothingLevel then
    FSmoothingLevel := Value;
end;

procedure TMaskedTransition.Assign(Source: TPersistent);
var
  Transition: TMaskedTransition;
begin
  if Source is TMaskedTransition
  then
  begin
    inherited;

    Transition     := TMaskedTransition(Source);
    Style          := Transition.Style;
    Substyle       := Transition.Substyle;
    SmoothingLevel := Transition.SmoothingLevel;
  end
  else inherited;
end;

function TMaskedTransition.CountOfSubStyles(StyleValue: Word): Word;
begin
  if StyleValue = 0
  then Result := 0
  else Result := 1;
end;

function TMaskedTransition.StyleToUse: Word;
begin
  if FStyle = 0
  then
  begin
    if RandomStyle = 0 then
      RandomStyle := Random(CountOfStyles) + 1;
    Result := RandomStyle;
  end
  else Result := FStyle;
end;

function TMaskedTransition.SubStyleToUse: Word;
begin
  if FSubStyle = 0
  then
  begin
    if RandomSubStyle = 0 then
      RandomSubStyle := Random(CountOfSubStyles(StyleToUse)) + 1;
    Result := RandomSubStyle;
  end
  else Result := FSubStyle;

  if Reversed then
    Result := CalculateReversedSubStyle(StyleToUse, Result);
end;

function TMaskedTransition.CalculateReversedSubStyle(
  const StyleValue, SubStyleValue: Word): Word;
begin
  Result := SubStyleValue;
end;

procedure TMaskedTransition.ExecuteFrame(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Longint);
begin
  if not IsSmooth
  then ExecuteFrame1bpp(Data, CurrentFrame, Step, LastExecutedFrame,
         Frame1bppMaskBmp, Frame1bppMaskBmp2, False, True, True)
  else ExecuteFrame8bpp(Data, CurrentFrame, Step, LastExecutedFrame);
end;

function TMaskedTransition.ExecuteFrame1bpp(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Longint; CurrentMask, PreviousMask:
  TBitmap; SmoothValue, Draw, CalcDirtyRects: Boolean): Boolean;
var
  Dst,
  Work,
  Mask: PDWordArray;
  j,
  k: Longint;
  ScanLineSize,
  MaskScanLineSize: Integer;
begin
  Assert(CurrentFrame >= 1);
  Assert(CurrentFrame <= Data.Frames);
  if not Data.AllowDeviceUpdate then
  begin
    MaskFrame(CurrentMask, CurrentFrame, Step, LastExecutedFrame, Data, Draw,
      CalcDirtyRects);
  end
  else
  begin
    MaskScanLineSize := GetBytesPerScanline(CurrentMask, pf1bit, 32);

    Data.UpdateRect   := Rect(0, 0, Data.Width, Data.Height);
    Data.UnUpdateRect := Rect(0, 0, 0, 0);
    MaskFrame(CurrentMask, CurrentFrame, Step, LastExecutedFrame, Data, Draw,
      CalcDirtyRects);
    IntersectRect(Data.UpdateRect, Data.UpdateRect, Rect(0, 0, Data.Width, Data.Height));
    IntersectRect(Data.UnUpdateRect, Data.UnUpdateRect, Data.UpdateRect);

    if not SmoothValue then
    begin
      if InversePaint then
      begin
        Mask := PDWordArray(PAnsiChar(CurrentMask.ScanLine[0]) + MaskScanLineSize);
        k    := -((CurrentMask.Width * (Data.Height)) div 32);
        InvertMask(Mask, k);
      end;

      if OptimizeMask then
      begin
        if Assigned(Data.DirtyRects) and (Data.DirtyRects.Count > 1)
        then
        begin
          for j := 0 to Data.DirtyRects.Count-1 do
            DoMaskOptimization(CurrentMask, PreviousMask, MaskScanLineSize,
              Data.DirtyRects[j], Rect(0, 0, 0, 0));
        end
        else DoMaskOptimization(CurrentMask, PreviousMask, MaskScanLineSize,
               Data.UpdateRect, Data.UnUpdateRect);
      end;

      ScanLineSize := GetBytesPerScanline(Data.DstBmp, Data.PixelFormat, 32);
      Work := PDWordArray(PAnsiChar(Data.Bitmap.ScanLine[0]) + ScanlineSize);
      Dst  := PDWordArray(PAnsiChar(Data.DstBmp.ScanLine[0]) + ScanlineSize);
      Mask := PDWordArray(PAnsiChar(CurrentMask.ScanLine[0]) + MaskScanlineSize);
      if Assigned(Data.DirtyRects) and (Data.DirtyRects.Count > 1)
      then
      begin
        for j := 0 to Data.DirtyRects.Count-1 do
          Apply1bppMask(Work, Dst, Mask, CurrentMask.Width, ScanLineSize,
            MaskScanLineSize, Data.PixelFormat, Data.DirtyRects[j],
            Rect(0, 0, 0, 0))
      end
      else
        Apply1bppMask(Work, Dst, Mask, CurrentMask.Width, ScanLineSize,
          MaskScanLineSize, Data.PixelFormat, Data.UpdateRect, Data.UnUpdateRect);
    end;
  end;
  if(not SmoothValue) and ResetMaskBmp(Data.Device) then
    FillRect(CurrentMask.Canvas.Handle, Data.UpdateRect, GetStockObject(WHITE_BRUSH));
  Result := True;
end;

procedure TMaskedTransition.ExecuteFrame8bpp(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Longint);
const
  Optimize = True;
var
  aux: TBitmap;
  Work,
  Dst,
  Src,
  Mask,
  Mask2: Pointer;
  ScanLineSize,
  MaskScanLineSize,
  i,
  Frame,
  StartLevel,
  EndLevel,
  TopFrame,
  LowFrame,
  DecValue,
  Increment,
  FillFrames: Longint;
  DirtyRect,
  EmptyRect: TRect;

  procedure DrawFrame(Color, Frame, Step: Integer);
  begin
    Frame8bppMaskBmp.Canvas.Pen  .Color := $02000000 or RGB(Color, Color, Color);
    Frame8bppMaskBmp.Canvas.Brush.Color := Frame8bppMaskBmp.Canvas.Pen.Color;
    ExecuteFrame1bpp(Data, Frame, Step, Frame-Step, Frame8bppMaskBmp, nil,
      True, True, False);
  end;

  // Get the params
  procedure GetDecValue;
  begin
    if(LastExecutedFrame <> 0) 
    then
    begin
      DecValue := Step * StepLevel;
      if DecValue > MaxLevel then
        DecValue := MaxLevel;
    end
    else DecValue := 0;

    EndLevel := MaxLevel;
    if CurrentFrame > (Data.Frames + 1) then
      Dec(EndLevel, (CurrentFrame - (Data.Frames + 1)) * StepLevel);

    StartLevel := EndLevel - ((Step - 1) * StepLevel);
    if StartLevel < MinLevel
    then
    begin
      StartLevel := MinLevel;
      FillFrames := Step - (((EndLevel - StartLevel) div StepLevel) + 1 );
    end
    else
    begin
      FillFrames := 0;
      if StartLevel > EndLevel then
        StartLevel := EndLevel;
    end;

    TopFrame := CurrentFrame + ((EndLevel - MaxLevel) div StepLevel);
    if TopFrame > Data.Frames + 1 then
      TopFrame := Data.Frames;
    LowFrame := LastExecutedFrame - Levels;
    if LowFrame < 0 then
      LowFrame := 0;
  end;

begin
  {$ifdef LogTiming}
  if Assigned(Log) then
    Log.ChronoExtra.Start;
  {$endif LogTiming}

  MaskScanLineSize := GetBytesPerScanline(Frame8bppMaskBmp, pf8bit          , 32);
  ScanLineSize     := GetBytesPerScanline(Data.Bitmap     , Data.PixelFormat, 32);
  if AvoidPixelRepaint
  then
  begin
    aux               := Frame8bppMaskBmp;
    Frame8bppMaskBmp  := Frame8bppMaskBmp2;
    Frame8bppMaskBmp2 := aux;
    Mask2 := PAnsiChar(Frame8bppMaskBmp2.ScanLine[0]) + MaskScanLineSize;
  end
  else Mask2 := nil;
  Work := PAnsiChar(Data.Bitmap     .ScanLine[0]) + ScanlineSize;
  Dst  := PAnsiChar(Data.DstBmp     .ScanLine[0]) + ScanlineSize;
  Src  := PAnsiChar(Data.SrcBmp     .ScanLine[0]) + ScanlineSize;
  Mask := PAnsiChar(Frame8bppMaskBmp.ScanLine[0]) + MaskScanLineSize;

  GetDecValue;

  if(DecValue > 0) and (not AvoidPixelRepaint) then
  begin
    if Assigned(Data.DirtyRects) and (Data.DirtyRects.Count > 1)
    then
    begin
      for i := 0 to Data.DirtyRects.Count-1 do
        DoDecMask1(Mask, MaskScanLineSize, Frame8bppMaskBmp.Width, DecValue,
          DecValue = Levels, Data.DirtyRects[i], Rect(0, 0, 0, 0));
      
    end
    else DoDecMask1(Mask, MaskScanLineSize, Frame8bppMaskBmp.Width, DecValue,
           DecValue = Levels, Data.UpdateRectBak, Data.UnUpdateRectBak);
  end;
  if Assigned(Data.DirtyRects) then
    Data.DirtyRects.Clear;

  if AvoidPixelRepaint
  then
  begin
    if InversePaint then
      DrawFrame(255, CurrentFrame + ((EndLevel - MaxLevel) div StepLevel) + 1, 1);
    i          := EndLevel;
    EndLevel   := StartLevel;
    StartLevel := i;
    Increment  := -StepLevel;
  end
  else
  begin
    if FillFrames > 0 then
    begin
      if InversePaint
      then
      begin
        Frame8bppMaskBmp.Canvas.Pen  .Color := $02000000 or RGB(0, 0, 0);
        Frame8bppMaskBmp.Canvas.Brush.Color := Frame8bppMaskBmp.Canvas.Pen.Color;
        Frame8bppMaskBmp.Canvas.FillRect(Data.UpdateRectBak);
      end
      else DrawFrame(0, LowFrame + FillFrames, FillFrames);
    end;
    Increment := StepLevel;
  end;

  i := StartLevel;
  // Draw the new levels
  repeat
    Frame := CurrentFrame + ((i - MaxLevel) div StepLevel);
    DrawFrame(i, Frame, 1);
    Inc(i, Increment);
  until i = EndLevel + Increment;

  if(not AvoidPixelRepaint) and
    InversePaint            and
    (CurrentFrame < Data.Frames)
  then DrawFrame(255, CurrentFrame + ((EndLevel - MaxLevel) div StepLevel) + 1, 1)
  else if(FillFrames > 0) and (not InversePaint) then
    DrawFrame(0, LastExecutedFrame + FillFrames, FillFrames);

  // Get the dirty rects
  ExecuteFrame1bpp(Data, TopFrame, TopFrame - LowFrame, LowFrame,
    Frame8bppMaskBmp, nil, True, False, True);

  if(DecValue > 0) and AvoidPixelRepaint then
  begin
    if Assigned(Data.DirtyRects) and (Data.DirtyRects.Count > 1)
    then
    begin
      for i := 0 to Data.DirtyRects.Count-1 do
        DoDecMask2(Mask, Mask2, MaskScanLineSize, Frame8bppMaskBmp.Width,
          DecValue, DecValue = Levels, Data.DirtyRects[i], Rect(0, 0, 0, 0));
    end
    else DoDecMask2(Mask, Mask2, MaskScanLineSize, Frame8bppMaskBmp.Width,
           DecValue, DecValue = Levels, Data.UpdateRect, Data.UnUpdateRect);
  end;

  if Assigned(Data.DirtyRects) and (Data.DirtyRects.Count > 1)
  then
  begin
    EmptyRect := Rect(0, 0, 0, 0);
    for i := 0 to Data.DirtyRects.Count-1 do
    begin
      DirtyRect := Data.DirtyRects[i];
      Apply256MaskS(TApply256MaskSProc(Apply256MaskSProc), Work,
        Dst, Src, Mask, ScanLineSize, MaskScanLineSize, Frame8bppMaskBmp.Width,
        DirtyRect, EmptyRect);
      Data.DirtyRects[i] := DirtyRect;
    end
  end
  else
    Apply256MaskS(TApply256MaskSProc(Apply256MaskSProc), Work,
      Dst, Src, Mask, ScanLineSize, MaskScanLineSize, Frame8bppMaskBmp.Width,
      Data.UpdateRect, Data.UnUpdateRect);

  {$ifdef LogTiming}
  if Assigned(Log) then
  begin
    Log.ChronoExtra.Pause;
    Log.CurrentItem^.LogExTime := Log.ChronoExtra.Milliseconds;
    Log.ChronoExtra.Reset;
  end;
  {$endif LogTiming}
end;

function TMaskedTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiNeedOffScreenBmp,
      tetiOffScreenBmpCapable,
      tetiStaticSrcPixels
    ];
end;

function TMaskedTransition.MaxSmoothingLevel: Integer;
begin
  Result := 7;
end;

procedure TMaskedTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'Style'];
  if Value <> '' then
    Style := StrToInt(Value);

  Value := List.Values[Prefix + 'SubStyle'];
  if Value <> '' then
    SubStyle := StrToInt(Value);

  Value := List.Values[Prefix + 'SmoothingLevel'];
  if Value <> '' then
    SmoothingLevel := StrToInt(Value);
end;

procedure TMaskedTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
var
  Prop: String;
begin
  inherited;

  Prop := 'Style';
  if IsPublishedProp(Self, Prop) and
    (
      (not OmitDefaultValues) or
      (Style <> 1)
    ) then
    List.Values[Prefix + Prop] := IntToStr(Style);

  Prop := 'SubStyle';
  if IsPublishedProp(Self, Prop) and
    (
      (not OmitDefaultValues) or
      (SubStyle <> 1)
    ) then
    List.Values[Prefix + Prop] := IntToStr(SubStyle);

  Prop := 'SmoothingLevel';
  if IsPublishedProp(Self, Prop) and
    (
      (not OmitDefaultValues) or
      (SmoothingLevel <> 0)
    ) then
    List.Values[Prefix + Prop] := IntToStr(SmoothingLevel);
end;

end.
