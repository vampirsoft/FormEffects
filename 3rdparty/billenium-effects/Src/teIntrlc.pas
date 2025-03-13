unit teIntrlc;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teMasked, Windows, Messages, Graphics;

type
  TInterlacedTransition = class(TMaskedTransition)
  private
  protected
    IndexAux,
    Pass1Limit: Longint;

    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint); override;
    function  CalcTotalFrames(Data: TTETransitionData): Longint; override;
    function  ResetMaskBmp(Device: TTETransitionDevice): Boolean; override;
    function  CalculateReversedSubStyle(
      const StyleValue, SubStyleValue: Word): Word; override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step, LastExecutedFrame:
      Longint; Data: TTETransitionData; Draw, CalcDirtyRects: Boolean); override;
    procedure Style1_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style1_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style1_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style1_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style1_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style1_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style1_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style1_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style1_9Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style1_10Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
    procedure Style2_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, LastExecutedFrame: Longint; Draw: Boolean);
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;

    function  CountOfSubStyles(StyleValue: Word): Word; override;

    property CountOfStyles;
  published
    property Reversed;
    property SmoothingLevel;
    property Style;
    property SubStyle;
  end;

implementation

constructor TInterlacedTransition.Create(AOwner: TComponent);
begin
  inherited;

  FCountOfStyles := 2;
end;

class function TInterlacedTransition.Description: String;
begin
  Result := 'Interlaced';
end;

function TInterlacedTransition.CalculateReversedSubStyle(
  const StyleValue, SubStyleValue: Word): Word;
begin
  Result := SubStyleValue;

  case StyleValue of
    1: case SubStyleValue of
          1: Result :=  1;
          2: Result :=  2;
          3: Result :=  4;
          4: Result :=  3;
          5: Result :=  6;
          6: Result :=  5;
          7: Result :=  8;
          8: Result :=  7;
          9: Result := 10;
         10: Result :=  9;
       end;
    2: Result := 1;
  end;
end;

procedure TInterlacedTransition.Initialize(Data: TTETransitionData; var
  TotalFrames: Longint);
begin
  inherited;

  case StyleToUse of
    1: case SubStyleToUse of
          1: begin
               IndexAux := Data.Height-1;
               if IndexAux mod 2 = 0 then
                 Dec(IndexAux);
             end;
          2: begin
               IndexAux := Data.Width-1;
               if IndexAux mod 2 = 0 then
                 Dec(IndexAux);
             end;
          3,
          4,
          7,
          8: begin
               IndexAux := Data.Height-1;
               if IndexAux mod 2 = 0 then
                 Dec(IndexAux);
               Pass1Limit := Data.Height div 2;
               if(Data.Height mod 2 <> 0)  and
                 (SubStyleToUse in [3, 7]) then
                 Inc(Pass1Limit);
             end;
          5,
          6,
          9,
         10: begin
               IndexAux := Data.Width-1;
               if IndexAux mod 2 = 0 then
                 Dec(IndexAux);
               Pass1Limit := Data.Width div 2;
               if(Data.Width mod 2 <> 0)      and
                 (SubStyleToUse in [5, 9]) then
                 Inc(Pass1Limit);
             end;
       end;
    2: if Data.Height < Data.Width
       then IndexAux := ((Data.Height - 1) div 4) * 2 + 2
       else IndexAux := ((Data.Width  - 1) div 4) * 2 + 2;
  end;
end;

function TInterlacedTransition.CalcTotalFrames(Data: TTETransitionData): Longint;
begin
  Result := 0;

  case StyleToUse of
    1: case SubStyleToUse of
          1: if IsSmooth
             then Result := Data.Height div 2
             else Result := Data.Height;
          2: if IsSmooth
             then Result := Data.Width div 2
             else Result := Data.Width;
          3,
          4,
          7,
          8: Result := Data.Height;
          5,
          6,
          9,
         10: Result := Data.Width;
       end;
    2: begin
         if Data.Height < Data.Width
         then
         begin
           if IsSmooth
           then Result := (Data.Height + 3) div 4
           else Result := (Data.Height + 1) div 2;
         end
         else
         begin
           if IsSmooth
           then Result := (Data.Width + 3) div 4
           else Result := (Data.Width + 1) div 2;
         end;
       end;
  end;
end;

function TInterlacedTransition.ResetMaskBmp(Device: TTETransitionDevice): Boolean;
begin
  Result := StyleToUse = 2;
end;

function TInterlacedTransition.CountOfSubStyles(StyleValue: Word): Word;
begin
  if StyleValue = 0
  then Result := 0
  else
  begin
    Result := 1;

    case StyleValue of
      1: Result := 10;
      2: Result := 1;
    end;
  end;
end;

function TInterlacedTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiThreadSafe
    ];
end;

procedure TInterlacedTransition.MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step,
  LastExecutedFrame: Longint; Data: TTETransitionData; Draw, CalcDirtyRects:
  Boolean);
begin
  Data.UpdateRect := Rect(0, 0, 0, 0);
  case StyleToUse of
    1: case SubStyleToUse of
          1: if IsSmooth
             then Style1_1Frame(MaskBmp, Data, CurrentFrame * 2, LastExecutedFrame * 2, Draw)
             else Style1_1Frame(MaskBmp, Data, CurrentFrame    , LastExecutedFrame    , Draw);
          2: if IsSmooth
             then Style1_2Frame(MaskBmp, Data, CurrentFrame * 2, LastExecutedFrame * 2, Draw)
             else Style1_2Frame(MaskBmp, Data, CurrentFrame    , LastExecutedFrame    , Draw);
          3: Style1_3Frame (MaskBmp, Data, CurrentFrame, LastExecutedFrame, Draw);
          4: Style1_4Frame (MaskBmp, Data, CurrentFrame, LastExecutedFrame, Draw);
          5: Style1_5Frame (MaskBmp, Data, CurrentFrame, LastExecutedFrame, Draw);
          6: Style1_6Frame (MaskBmp, Data, CurrentFrame, LastExecutedFrame, Draw);
          7: Style1_7Frame (MaskBmp, Data, CurrentFrame, LastExecutedFrame, Draw);
          8: Style1_8Frame (MaskBmp, Data, CurrentFrame, LastExecutedFrame, Draw);
          9: Style1_9Frame (MaskBmp, Data, CurrentFrame, LastExecutedFrame, Draw);
         10: Style1_10Frame(MaskBmp, Data, CurrentFrame, LastExecutedFrame, Draw);
       end;
    2: if IsSmooth
       then Style2_1Frame(MaskBmp, Data, CurrentFrame * 2, LastExecutedFrame * 2, Draw)
       else Style2_1Frame(MaskBmp, Data, CurrentFrame    , LastExecutedFrame    , Draw);
  end;
end;

procedure TInterlacedTransition.Style1_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Integer;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Even: Boolean;
  UnUpdateRect1,
  UnUpdateRect2: TRect;
begin
  Data.UnUpdateRect := Rect(0, 0, Data.Width, Data.Height);
  UnUpdateRect1     := Rect(0, 0, Data.Width, Data.Height);
  UnUpdateRect2     := Rect(0, 0, Data.Width, Data.Height);

  x    := Data.Width;
  Even := (LastExecutedFrame + 1) mod 2 = 0;
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    if Even
    then
    begin
      y := i - 2;
      IntersectRect(UnUpdateRect1, UnUpdateRect1, Rect(0,   0, x, y-1));
      IntersectRect(UnUpdateRect2, UnUpdateRect2, Rect(0, y+1, x, Data.Height));
    end
    else
    begin
      y := IndexAux - i + 1;
      IntersectRect(UnUpdateRect1, UnUpdateRect1, Rect(0, y+1, x, Data.Height));
      IntersectRect(UnUpdateRect2, UnUpdateRect2, Rect(0,   0, x, y-1));
    end;

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(0, y);
      MaskBmp.Canvas.LineTo(x, y);
    end;

    Windows.UnionRect(Data.UpdateRect, Rect(0, y, x, y+1), Data.UpdateRect);
    Even := not Even;
  end;
  if UnUpdateRect1.Bottom - UnUpdateRect1.Top > UnUpdateRect2.Bottom - UnUpdateRect2.Top
  then IntersectRect(Data.UnUpdateRect, Data.UnUpdateRect, UnUpdateRect1)
  else IntersectRect(Data.UnUpdateRect, Data.UnUpdateRect, UnUpdateRect2);
end;

procedure TInterlacedTransition.Style1_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Integer;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Even: Boolean;
  UnUpdateRect1,
  UnUpdateRect2: TRect;
begin
  Data.UnUpdateRect := Rect(0, 0, Data.Width, Data.Height);
  UnUpdateRect1     := Rect(0, 0, Data.Width, Data.Height);
  UnUpdateRect2     := Rect(0, 0, Data.Width, Data.Height);

  y    := Data.Height;
  Even := (LastExecutedFrame + 1) mod 2 = 0;
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    if Even
    then
    begin
      x := i - 2;
      IntersectRect(UnUpdateRect1, UnUpdateRect1, Rect(0  , 0, x-1       , y));
      IntersectRect(UnUpdateRect2, UnUpdateRect2, Rect(x+1, 0, Data.Width, y));
    end
    else
    begin
      x := IndexAux - i + 1;
      IntersectRect(UnUpdateRect1, UnUpdateRect1, Rect(x+1, 0, Data.Width, y));
      IntersectRect(UnUpdateRect2, UnUpdateRect2, Rect(0  , 0, x-1       , y));
    end;

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(x, 0);
      MaskBmp.Canvas.LineTo(x, y);
    end;

    Windows.UnionRect(Data.UpdateRect, Rect(x, 0, x+1, y), Data.UpdateRect);
    Even := not Even;
  end;
  if UnUpdateRect1.Bottom - UnUpdateRect1.Top > UnUpdateRect2.Bottom - UnUpdateRect2.Top
  then IntersectRect(Data.UnUpdateRect, Data.UnUpdateRect, UnUpdateRect1)
  else IntersectRect(Data.UnUpdateRect, Data.UnUpdateRect, UnUpdateRect2);
end;

procedure TInterlacedTransition.Style1_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Longint;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Pass1: Boolean;
begin
  x := Data.Width;
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    Pass1 := i <= Pass1Limit;
    if Pass1
    then y  := (i-1) * 2
    else y  := IndexAux - ((i - (Pass1Limit+1)) * 2);
    Windows.UnionRect(Data.UpdateRect, Rect(0, y, x, y+1), Data.UpdateRect);

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(0, y);
      MaskBmp.Canvas.LineTo(x, y);
    end;
  end;
end;

procedure TInterlacedTransition.Style1_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Integer;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Pass1: Boolean;
begin
  x := Data.Width;
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    Pass1 := i <= Pass1Limit;
    if Pass1
    then y  := IndexAux - ((i - 1) * 2)
    else y  := (i - (Pass1Limit + 1)) * 2;
    Windows.UnionRect(Data.UpdateRect, Rect(0, y, x, y+1), Data.UpdateRect);

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(0, y);
      MaskBmp.Canvas.LineTo(x, y);
    end;
  end;
end;

procedure TInterlacedTransition.Style1_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Longint;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Pass1: Boolean;
begin
  y := Data.Height;
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    Pass1 := i <= Pass1Limit;
    if Pass1
    then x  := (i - 1) * 2
    else x  := IndexAux - ((i - (Pass1Limit + 1)) * 2);
    Windows.UnionRect(Data.UpdateRect, Rect(x, 0, x+1, y), Data.UpdateRect);

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(x, 0);
      MaskBmp.Canvas.LineTo(x, y);
    end;
  end;
end;

procedure TInterlacedTransition.Style1_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Integer;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Pass1: Boolean;
begin
  y := Data.Height;
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    Pass1 := i <= Pass1Limit;
    if Pass1
    then x  := IndexAux - ((i - 1) * 2)
    else x  := (i - (Pass1Limit + 1)) * 2;
    Windows.UnionRect(Data.UpdateRect, Rect(x, 0, x+1, y), Data.UpdateRect);

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(x, 0);
      MaskBmp.Canvas.LineTo(x, y);
    end;
  end;
end;

procedure TInterlacedTransition.Style1_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Longint;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Pass1: Boolean;
begin
  x := Data.Width;
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    Pass1 := i <= Pass1Limit;
    if Pass1
    then y  := (i - 1) * 2
    else y  := ((i - 1) * 2) - IndexAux - 2;
    Windows.UnionRect(Data.UpdateRect, Rect(0, y, x, y+1), Data.UpdateRect);

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(0, y);
      MaskBmp.Canvas.LineTo(x, y);
    end;
  end;
end;

procedure TInterlacedTransition.Style1_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Integer;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Pass1: Boolean;
begin
  x := Data.Width;
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    Pass1 := i <= Pass1Limit;
    if Pass1
    then y  := IndexAux - ((i - 1) * 2)
    else y  := IndexAux - (((i - Pass1Limit) - 1) * 2) + 1;
    Windows.UnionRect(Data.UpdateRect, Rect(0, y, x, y + 1), Data.UpdateRect);

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(0, y);
      MaskBmp.Canvas.LineTo(x, y);
    end;
  end;
end;

procedure TInterlacedTransition.Style1_9Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Longint;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Pass1: Boolean;
begin
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    Pass1 := i <= Pass1Limit;
    if Pass1
    then x  := (i - 1) * 2
    else x  := ((i - 1) * 2) - IndexAux - 2;
    y := Data.Height;
    Windows.UnionRect(Data.UpdateRect, Rect(x, 0, x + 1, y + 1), Data.UpdateRect);

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(x, 0);
      MaskBmp.Canvas.LineTo(x, y);
    end;
  end;
end;

procedure TInterlacedTransition.Style1_10Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Integer;
  Draw: Boolean);
var
  i,
  x,
  y: Integer;
  Pass1: Boolean;
begin
  for i:= (LastExecutedFrame + 1) to CurrentFrame do
  begin
    Pass1 := i <= Pass1Limit;
    if Pass1
    then x  := IndexAux - ((i - 1) * 2)
    else x  := IndexAux - (((i - Pass1Limit) - 1) * 2) + 1;
    y := Data.Height;
    Windows.UnionRect(Data.UpdateRect, Rect(x, 0, x + 1, y + 1), Data.UpdateRect);

    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(x, 0);
      MaskBmp.Canvas.LineTo(x, y);
    end;
  end;
end;

procedure TInterlacedTransition.Style2_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, LastExecutedFrame: Integer;
  Draw: Boolean);
var
  i: Integer;
  R: TRect;
  Even: Boolean;
begin
  Data.UnUpdateRect := Rect(0, 0, Data.Width, Data.Height);
  Even := (LastExecutedFrame + 1) mod 2 = 0;
  for i:= LastExecutedFrame to CurrentFrame-1 do
  begin
    if Even
    then
    begin
      R.Left   := IndexAux - i;
      R.Top    := IndexAux - i;
      R.Right  := (Data.Width  - 1) - R.Left;
      R.Bottom := (Data.Height - 1) - R.Top;
    end
    else
    begin
      R.Left   := i;
      R.Top    := i;
      R.Right  := (Data.Width  - 1) - R.Left;
      R.Bottom := (Data.Height - 1) - R.Top;
    end;
    if(R.Left <= R.Right) and (R.Top <= R.Bottom) then
    begin
      Windows.UnionRect(Data.UpdateRect, Rect(R.Left, R.Top, R.Right+1, R.Bottom+1),
        Data.UpdateRect);
      IntersectRect(Data.UnUpdateRect, Data.UnUpdateRect,
        Rect(R.Left+1, R.Top+1, R.Right, R.Bottom));

      if Draw then
        MaskBmp.Canvas.Polyline([Point(R.Left, R.Top), Point(R.Right, R.Top),
          Point(R.Right, R.Bottom), Point(R.Left, R.Bottom),
          Point(R.Left, R.Top)]);
    end;
    Even := not Even;
  end;
end;

initialization

  TERegisterTransition(TInterlacedTransition);
  RegisterClasses([TInterlacedTransition]);

end.
