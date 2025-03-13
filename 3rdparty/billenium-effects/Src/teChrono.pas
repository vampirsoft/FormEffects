unit teChrono;

interface

{$INCLUDE teDefs.inc}

{$ifdef WIN32}
uses Windows;
{$endif WIN32}

type
  TTEChrono = class(TObject)
  private
    FMilliseconds: Double;
    FPasses: Longint;
    FPaused: Boolean;
    FResets: Longint;
    FIsReset,
    HighRes: Boolean;
    PerformanceFreq,
    TimeStarted: TLargeInteger;
  public
    constructor Create;
    destructor Destroy; override;

    function  Milliseconds: Double;
    procedure Pause;
    procedure Reset;
    procedure Restart;
    procedure Start;

    property IsReset: Boolean read FIsReset;
    property Passes: Longint read FPasses;
    property Paused: Boolean read FPaused;
    property Resets: Longint read FResets;
  end;

implementation

uses MMSystem;

{ TTEChrono }
constructor TTEChrono.Create;
begin
  PerformanceFreq := 0;
  TimeStarted     := 0;

  FIsReset      := True;
  FPaused       := False;
  FMilliseconds := 0;
  FPasses       := 0;
  FResets       := 0;

  HighRes := QueryPerformanceFrequency(PerformanceFreq);
  if not HighRes then
    timeBeginPeriod(1);
end;

destructor TTEChrono.Destroy;
begin
  if not HighRes then
    timeEndPeriod(1);

  inherited;
end;

function TTEChrono.Milliseconds: Double;
var
  Value: TLargeInteger;
begin
  if(not FIsReset) and (not FPaused)
  then
  begin
    if HighRes
    then
    begin
      QueryPerformanceCounter(Value);
      Result := FMilliseconds +
        (((Value - TimeStarted) * 1000.0) / PerformanceFreq);
    end
    else Result := FMilliseconds +
           (timeGetTime - TimeStarted);
  end
  else Result := FMilliseconds;
end;

procedure TTEChrono.Restart;
begin
  Reset;
  Start;
end;

procedure TTEChrono.Start;
begin
  if FIsReset or FPaused then
  begin
    if HighRes
    then QueryPerformanceCounter(TimeStarted)
    else TimeStarted := timeGetTime;

    Inc(FPasses);
    FIsReset := False;
    FPaused  := False;
  end;
end;

procedure TTEChrono.Pause;
begin
  if(not FIsReset) and (not Paused) then
  begin
    FMilliseconds := Milliseconds;

    FPaused := True;

    TimeStarted := 0;
  end;
end;

procedure TTEChrono.Reset;
begin
  Pause;

  if not FIsReset then
    Inc(FResets);

  FMilliseconds := 0;
  FPasses       := 0;
  FIsReset      := True;
end;

end.

