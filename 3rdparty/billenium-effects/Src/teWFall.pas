unit teWFall;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teMasked, Windows, Messages, Graphics;

type
{$ifndef TE_NOHLP}
  TSmallArray = array[0..65535] of Smallint;
  PSmallArray = ^TSmallArray;
{$endif TE_NOHLP}

  TWaterfallTransition = class(TMaskedTransition)
  private
    WaterfallData: PSmallArray;

    procedure CreateWaterfallData(Size: Integer);
    procedure DrawLineDown(MaskBmp: TBitmap; Data: TTETransitionData;
      Index, Value, OldValue: Integer);
    procedure DrawLineUp(MaskBmp: TBitmap; Data: TTETransitionData;
      Index, Value, OldValue: Integer);
    procedure DrawLineRight(MaskBmp: TBitmap; Data: TTETransitionData;
      Index, Value, OldValue: Integer);
    procedure DrawLineLeft(MaskBmp: TBitmap; Data: TTETransitionData;
      Index, Value, OldValue: Integer);
  protected
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure Finalize(Data: TTETransitionData); override;
    function  CalcTotalFrames(Data: TTETransitionData): Longint; override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step, LastExecutedFrame:
      Longint; Data: TTETransitionData; Draw, CalcDirtyRects: Boolean); override;
    function Smooth(Device: TTETransitionDevice): Boolean; override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;
    class function GetEditor: String; override;
  published
    property Direction default tedDown;
    property Reversed;
  end;

implementation

const
  MaxIncrement =  4;

{ TWaterfallTransition }

constructor TWaterfallTransition.Create(AOwner: TComponent);
begin
  inherited;

  AllowedDirections := [tedRight, tedLeft, tedDown, tedUp, tedRandom];
  Direction         := tedDown;
end;

class function TWaterfallTransition.Description: String;
begin
  Result := 'Waterfall';
end;

class function TWaterfallTransition.GetEditor: String;
begin
  Result := 'TTransitionEffectEditor';
end;

function TWaterfallTransition.CalcTotalFrames(Data: TTETransitionData): Longint;
begin
  if DirectionToUse in [tedRight, tedLeft]
  then Result := (Data.Width  * 80) div 100
  else Result := (Data.Height * 80) div 100;
end;

procedure TWaterfallTransition.CreateWaterfallData(Size: Integer);
var
  i: Integer;
begin
  GetMem(WaterfallData, Size * 2);
  for i := 0 to Size-1 do
    WaterfallData[i] := 0;
end;

procedure TWaterfallTransition.Initialize(Data: TTETransitionData; var
  TotalFrames: Longint);
begin
  inherited;

  if DirectionToUse in [tedRight, tedLeft]
  then CreateWaterfallData(Data.Height)
  else CreateWaterfallData(Data.Width);
end;

procedure TWaterfallTransition.Finalize(Data: TTETransitionData);
begin
  FreeMem(WaterfallData);

  inherited;
end;

procedure TWaterfallTransition.MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step,
  LastExecutedFrame: Longint; Data: TTETransitionData; Draw, CalcDirtyRects:
  Boolean);
var
  Increment,
  OldValue,
  Value,
  MinValue,
  MaxValue,
  TopValue: Integer;
  i,
  Limit: Integer;
  Direction: TTEEffectDirection;
begin
  Direction := DirectionToUse;
  if Direction in [tedRight, tedLeft]
  then
  begin
    Limit    := Data.Height - 1;
    TopValue := Data.Width;
  end
  else
  begin
    Limit    := Data.Width  - 1;
    TopValue := Data.Height;
  end;

  MinValue := TopValue;
  MaxValue := 0;
  for i := 0 to Limit do
  begin
    Increment := Random(MaxIncrement);
    if Increment > 0 then
    begin
      OldValue := WaterfallData[i];
      if OldValue < MinValue then
        MinValue := OldValue;
      if OldValue < TopValue then
      begin
        Value := OldValue + (Increment * Step);
        WaterfallData[i] := Value;
        if Value > MaxValue then
          MaxValue := Value;
        if Draw then
        begin
          case Direction of
            tedRight: DrawLineRight(MaskBmp, Data, i, Value, OldValue);
            tedLeft : DrawLineLeft (MaskBmp, Data, i, Value, OldValue);
            tedDown : DrawLineDown (MaskBmp, Data, i, Value, OldValue);
            tedUp   : DrawLineUp   (MaskBmp, Data, i, Value, OldValue);
          end;
        end;
      end;
    end;
  end;
  if MinValue > MaxValue then
  begin
    MaxValue := WaterfallData[0];
    MinValue := MinValue - 1;
  end;

  case Direction of
    tedRight: Data.UpdateRect := Rect(MinValue, 0, MaxValue, Data.Height);
    tedLeft : Data.UpdateRect := Rect(Data.Width - MaxValue, 0,
                Data.Width - MinValue, Data.Height);
    tedDown : Data.UpdateRect := Rect(0, MinValue, Data.Width, MaxValue);
    tedUp   : Data.UpdateRect := Rect(0, Data.Height - MaxValue, Data.Width,
                Data.Height - MinValue);
  end;
end;

procedure TWaterfallTransition.DrawLineDown(MaskBmp: TBitmap;
  Data: TTETransitionData; Index, Value, OldValue: Integer);
begin
  MaskBmp.Canvas.MoveTo(Index, OldValue);
  MaskBmp.Canvas.LineTo(Index, Value + 1);
end;

procedure TWaterfallTransition.DrawLineLeft(MaskBmp: TBitmap;
  Data: TTETransitionData; Index, Value, OldValue: Integer);
begin
  MaskBmp.Canvas.MoveTo(Data.Width - OldValue   , Index);
  MaskBmp.Canvas.LineTo(Data.Width - (Value + 1), Index);
end;

procedure TWaterfallTransition.DrawLineRight(MaskBmp: TBitmap;
  Data: TTETransitionData; Index, Value, OldValue: Integer);
begin
  MaskBmp.Canvas.MoveTo(OldValue , Index);
  MaskBmp.Canvas.LineTo(Value + 1, Index);
end;

procedure TWaterfallTransition.DrawLineUp(MaskBmp: TBitmap;
  Data: TTETransitionData; Index, Value, OldValue: Integer);
begin
  MaskBmp.Canvas.MoveTo(Index, Data.Height - OldValue);
  MaskBmp.Canvas.LineTo(Index, Data.Height - (Value + 1));
end;

function TWaterfallTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiThreadSafe
    ];
end;

function TWaterfallTransition.Smooth(Device: TTETransitionDevice): Boolean;
begin
  Result := False;
end;

initialization

  TERegisterTransition(TWaterfallTransition);

end.
