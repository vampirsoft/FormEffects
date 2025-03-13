unit teBlend;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, TransEff, teChrono, teTimed, teBlndWk,
  Graphics;

type
  TBlendTransition = class(TTimedTransitionEffect)
  protected
    EqualPerc: Integer;

    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint); override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    function GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
  public
    class function Description: String; override;
    function GetDelegate(Device: TTETransitionDevice;
      const ReturnCopy: Boolean): TTransitionEffect; override;
  published
    property Pass2Options;
    property PassSetting;
  end;

implementation

uses teFuse, teRender;

class function TBlendTransition.Description: String;
begin
  Result := 'Alpha blend';
end;

function TBlendTransition.GetPixelFormat(
  Device: TTETransitionDevice): TPixelFormat;
begin
  Result := Device.PixelFormat;
  if(Result = pf32bit) and (not TEProcessorInfo.MMX) then
    Result := pf24bit; // This is faster for slow pentiums
end;

procedure TBlendTransition.Initialize(Data: TTETransitionData; var TotalFrames:
  Longint);

  function GetEqDWords(Bmp1, Bmp2: PDWordArray; Size: Integer): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to Size-1 do
    begin
      if Bmp1[i] = Bmp2[i] then
        Inc(Result);
    end;
  end;

var
  Dst,
  Src: PDWordArray;
  Size: Longint;
  ScanLineSize: Integer;
begin
  inherited;

  if Data.PixelFormat in [pf15bit, pf16bit]
  then TotalFrames :=  31 
  else TotalFrames := 254;

  if(Data.PixelFormat in [pf15bit, pf16bit]) or (not TEProcessorInfo.MMX) then
  begin
    ScanLineSize := GetBytesPerScanline(Data.Bitmap, Data.PixelFormat, 32);
    Dst          := PDWordArray(Data.DstBmp.ScanLine[Data.Height-1]);
    Src          := PDWordArray(Data.SrcBmp.ScanLine[Data.Height-1]);
    Size         := (ScanLineSize * Data.Height) div 4;

    if Size <> 0
    then EqualPerc := (GetEqDWords(Dst, Src, Size) * 100) div Size
    else EqualPerc := 0;
  end;
end;

procedure TBlendTransition.ExecuteFrame(Data: TTETransitionData; CurrentFrame,
  Step, LastExecutedFrame: Longint);
begin
  {$ifdef LogTiming}
  if Assigned(Log) then
    Log.ChronoExtra.Start;
  {$endif LogTiming}

  BlendBmps(Data.Bitmap, Data.DstBmp, Data.SrcBmp, nil, Data.PixelFormat,
    CurrentFrame, EqualPerc);
  Data.UpdateRect := Rect(0, 0, Data.Width, Data.Height);
  {$ifdef LogTiming}
  if Assigned(Log) then
  begin
    Log.ChronoExtra.Pause;
    Log.CurrentItem^.LogExTime := Log.ChronoExtra.Milliseconds;
    Log.ChronoExtra.Reset;
  end;
  {$endif LogTiming}
end;

function TBlendTransition.GetDelegate(Device: TTETransitionDevice;
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

function TBlendTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiNeedOffScreenBmp,
      tetiOffScreenBmpCapable,
      tetiStaticSrcPixels,
      tetiThreadSafe
    ];
end;

initialization

  TERegisterTransition(TBlendTransition);
  RegisterClasses([TBlendTransition]);

end.
