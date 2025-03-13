unit teLog;

interface

uses Windows, Messages, SysUtils, Classes, TransEff;

{$INCLUDE teDefs.inc}

{$ifdef LogTiming}
type
  TTELog = class(TTELogBase)
  protected
    Items: array of TTELogItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CurrentItem: PTELogItem; override;
    function LastTransitionTime: Single; override;
    procedure NewItem; override;
    procedure SaveLog(Transition: TTransitionEffect; Data: TTETransitionData;
      ElapsedTime: Double); override;
    procedure SetSize(Size: Integer); override;
  end;
{$endif LogTiming}
implementation
{$ifdef LogTiming}

uses teChrono;

constructor TTELog.Create(AOwner: TComponent);
begin
  inherited;

  ItemCount  := 0;
  Detailed   := True;
  ChronoExtra := TTEChrono.Create;
end;

destructor TTELog.Destroy;
begin
  ChronoExtra  .Free;
end;

function TTELog.CurrentItem: PTELogItem;
begin
  Result := @Items[ItemCount-1];
end;

function TTELog.LastTransitionTime: Single;
begin
  if ItemCount > 1
  then Result := Items[ItemCount-2].LogTransitionTime
  else Result := 0;
end;

procedure TTELog.NewItem;
begin
  Assert(ItemCount < Length(Items), 'Too much log entries');
  Inc(ItemCount);
end;

procedure TTELog.SaveLog(Transition: TTransitionEffect;
  Data: TTETransitionData; ElapsedTime: Double);
var
  Fic: TextFile;
  i: Integer;
  DC: Hdc;
  FileName: String;
  Fps,
  TotUpdate,
  TotStep,
  TotStepTime,
  TotWork,
  TotInterval,
  TotExtra,
  TotSleep,
  TotPrecision,
  MaxUpdate,
  MaxStep,
  MaxStepTime,
  MaxWork,
  MaxInterval,
  MaxExtra,
  MaxSleep,
  MaxPrecision,
  MaxFps,
  MinUpdate,
  MinStep,
  MinStepTime,
  MinWork,
  MinInterval,
  MinExtra,
  MinSleep,
  MinPrecision,
  MinFps: Single;
  Item: TTELogItem;
begin
  if(ItemCount = 0) or (ElapsedTime = 0) then
    exit;

  FileName := ExtractFilePath(ParamStr(0)) + 'LogTrans.txt';
  AssignFile(Fic, FileName);
  try
    if FileExists(FileName)
    then Append (Fic)
    else Rewrite(Fic);
    DC := GetDC(0);
    try
      Writeln(Fic,
        Format('%s; %d ms; (%dx%d); %d/%d frames; %d bpp; pass %d/%d; %s',
          [Transition.ClassName,
           Transition.Milliseconds,
           Data.Width,
           Data.Height,
           ItemCount,
           Data.TotalFrames,
           GetDeviceCaps(DC, BITSPIXEL),
           Data.Pass,
           Data.PassCount,
           FormatDateTime('dd/mm/yy hh:nn', Now)]));
//      Writeln(Fic, Transition.ClassName + '; ' + IntToStr(Transition.Milliseconds) + ' ms; (' +
//        IntToStr(Data.Width) + 'x' + IntToStr(Data.Height) +
//        '); ' + IntToStr(ItemCount) + '/' + IntToStr(Data.TotalFrames-1) + ' frames; ' +
//        IntToStr(GetDeviceCaps(DC, BITSPIXEL)) + ' bpp');
    finally
      ReleaseDC(0, DC);
    end;
    TotUpdate    := 0;
    TotStep      := 0;
    TotStepTime  := 0;
    TotWork      := 0;
    TotInterval  := 0;
    TotExtra     := 0;
    TotSleep     := 0;
    TotPrecision := 0;
    MaxUpdate    := 0;
    MaxStep      := 0;
    MaxStepTime  := 0;
    MaxWork      := 0;
    MaxInterval  := 0;
    MaxExtra     := 0;
    MaxSleep     := 0;
    MaxPrecision := 0;
    MaxFps       := 0;
    MinUpdate    := 0;
    MinStep      := 0;
    MinStepTime  := 0;
    MinWork      := 0;
    MinInterval  := 0;
    MinExtra     := 0;
    MinSleep     := 0;
    MinPrecision := 0;
    MinFps       := 0;
    if Detailed then
    begin
      Writeln(Fic, 'Index CurFrame     Step TransTime StepTime  WorkTime SleepTime Preci  UpdTime   ExTime       Fps Interval Text');
    end;
    for i := 0 to ItemCount-1 do
    begin
      Item := Items[i];
      if Item.LogStepTime <> 0
      then Fps := 1000 / Item.LogStepTime
      else Fps := 99999;
      if Detailed then
        Writeln(Fic,
          Format('%5d %8d %8.2n %9.2n %8.2n %9.2n %9.2n %5.1n %8.2n %8.2n %9.2n %8.2n %s',
            [i + 1,
             Item.LogFrame,
             Item.LogStep,
             Item.LogTransitionTime,
             Item.LogStepTime,
             Item.LogWorkTime,
             Item.LogSleepTime,
             Item.LogSleepPrecision,
             Item.LogUpdateTime,
             Item.LogExTime,
             Fps,
             Item.LogInterval,
             Item.LogText]));
      TotUpdate    := TotUpdate    + Item.LogUpdateTime;
      TotStep      := TotStep      + Item.LogStep;
      TotStepTime  := TotStepTime  + Item.LogStepTime;
      TotWork      := TotWork      + Item.LogWorkTime;
      TotInterval  := TotInterval  + Item.LogInterval;
      TotExtra     := TotExtra     + Item.LogExTime;
      TotSleep     := TotSleep     + Item.LogSleepTime;
      TotPrecision := TotPrecision + Item.LogSleepPrecision;
      if i = 0
      then
      begin
        MaxUpdate    := Item.LogUpdateTime;
        MaxStep      := Item.LogStep;
        MaxStepTime  := Item.LogStepTime;
        MaxWork      := Item.LogWorkTime;
        MaxInterval  := Item.LogInterval;
        MaxExtra     := Item.LogExTime;
        MaxSleep     := Item.LogSleepTime;
        MaxPrecision := Item.LogSleepPrecision;
        MaxFps       := Fps;
        MinUpdate    := Item.LogUpdateTime;
        MinStep      := Item.LogStep;
        MinStepTime  := Item.LogStepTime;
        MinWork      := Item.LogWorkTime;
        MinInterval  := Item.LogInterval;
        MinExtra     := Item.LogExTime;
        MinSleep     := Item.LogSleepTime;
        MinPrecision := Item.LogSleepPrecision;
        MinFps       := Fps;
      end
      else
      begin
        if MaxUpdate    < Item.LogUpdateTime      then
          MaxUpdate    := Item.LogUpdateTime;
        if MaxStep      < Item.LogStep            then
          MaxStep      := Item.LogStep;
        if MaxStepTime  < Item.LogStepTime        then
          MaxStepTime  := Item.LogStepTime;
        if MaxWork      < Item.LogWorkTime        then
          MaxWork      := Item.LogWorkTime;
        if MaxInterval   < Item.LogInterval       then
          MaxInterval  := Item.LogInterval;
        if MaxExtra      < Item.LogExTime         then
          MaxExtra     := Item.LogExTime;
        if MaxSleep      < Item.LogSleepTime      then
          MaxSleep     := Item.LogSleepTime;
        if MaxPrecision  < Item.LogSleepPrecision then
          MaxPrecision := Item.LogSleepPrecision;
        if MaxFps        < Fps                    then
          MaxFps       := Fps;
        if MinUpdate   > Item.LogUpdateTime       then
          MinUpdate    := Item.LogUpdateTime;
        if MinStep      > Item.LogStep            then
          MinStep      := Item.LogStep;
        if MinStepTime  > Item.LogStepTime        then
          MinStepTime  := Item.LogStepTime;
        if MinWork      > Item.LogWorkTime        then
          MinWork      := Item.LogWorkTime;
        if MinInterval  > Item.LogInterval        then
          MinInterval  := Item.LogInterval;
        if MinExtra     > Item.LogExTime          then
          MinExtra     := Item.LogExTime;
        if MinSleep     > Item.LogSleepTime       then
          MinSleep     := Item.LogSleepTime;
        if MinPrecision > Item.LogSleepPrecision  then
          MinPrecision := Item.LogSleepPrecision;
        if MinFps       > Fps                     then
          MinFps       := Fps;
      end;
    end;
    // Totals
    Writeln(Fic, 'Index CurFrame     Step TransTime StepTime  WorkTime SleepTime Preci  UpdTime   ExTime       Fps Interval Text');
    Writeln(Fic,
      Format('Total %8d %8.2n %9.2n %8.2n %9.2n %9.2n %5.1n %8.2n %8.2n %9.2n %8.2n',
        [ItemCount,
         TotStep / ItemCount,
         ElapsedTime,
         TotStepTime / ItemCount,
         TotWork,
         TotSleep,
         TotPrecision / ItemCount,
         TotUpdate,
         TotExtra,
         (ItemCount * 1000) / ElapsedTime,
         TotInterval / ItemCount]));
    Writeln(Fic,
      Format('Max            %8.2n           %8.2n %9.2n %9.2n %5.1n %8.2n %8.2n %9.2n %8.2n',
        [MaxStep,
         MaxStepTime,
         MaxWork,
         MaxSleep,
         MaxPrecision,
         MaxUpdate,
         MaxExtra,
         MaxFps,
         MaxInterval]));
    Writeln(Fic,
      Format('Min            %8.2n           %8.2n %9.2n %9.2n %5.1n %8.2n %8.2n %9.2n %8.2n',
        [MinStep,
         MinStepTime,
         MinWork,
         MinSleep,
         MinPrecision,
         MinUpdate,
         MinExtra,
         MinFps,
         MinInterval]));
    if TotUpdate <> 0
    then Fps := (ItemCount * 1000) / TotUpdate
    else Fps := 99999;
    Writeln(Fic,
      Format('------------------------------------------------------------- %8.2n fps -----------------------',
       [Fps]));
  finally
    CloseFile(Fic);
    Items := nil;
  end;
end;

procedure TTELog.SetSize(Size: Integer);
begin
  SetLength(Items, Size);
  ItemCount := 0;
end;
{$endif LogTiming}
end.
