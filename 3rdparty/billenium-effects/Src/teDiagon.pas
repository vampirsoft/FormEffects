unit teDiagon;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teMasked, Windows, Messages, Graphics;

{$ifndef TE_NOHLP}
const
  DirtyRectSize = 16;
{$endif TE_NOHLP}

type
  TDiagonalTransition = class(TMaskedTransition)
  private
  protected
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint); override;
    function  CalcTotalFrames(Data: TTETransitionData): Longint; override;
    function  Smooth(Device: TTETransitionDevice): Boolean; override;
    function  ResetMaskBmp(Device: TTETransitionDevice): Boolean; override;
    function  CalculateReversedSubStyle(
      const StyleValue, SubStyleValue: Word): Word; override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step, LastExecutedFrame:
      Longint; Data: TTETransitionData; Draw, CalcDirtyRects: Boolean); override;
    procedure Style1_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
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

constructor TDiagonalTransition.Create(AOwner: TComponent);
begin
  inherited;

  FCountOfStyles := 2;
end;

class function TDiagonalTransition.Description: String;
begin
  Result := 'Diagonal';
end;

function TDiagonalTransition.CalculateReversedSubStyle(
  const StyleValue, SubStyleValue: Word): Word;
begin
  Result := SubStyleValue;

  case StyleValue of
    1: case SubStyleValue of
       1: Result := 2;
       2: Result := 1;
       3: Result := 4;
       4: Result := 3;
    end;
    2: case SubStyleValue of
       1: Result := 3;
       2: Result := 4;
       3: Result := 1;
       4: Result := 2;
    end;
  end;
end;

function TDiagonalTransition.CalcTotalFrames(Data: TTETransitionData): Longint;
begin
  Result := Data.Height + Data.Width - 1;
end;

function TDiagonalTransition.Smooth(Device: TTETransitionDevice): Boolean;
begin
  Result := (StyleToUse = 1) and inherited Smooth(Device);
end;

function TDiagonalTransition.ResetMaskBmp(Device: TTETransitionDevice): Boolean;
begin
  Result := not Smooth(Device);
end;

function TDiagonalTransition.CountOfSubStyles(StyleValue: Word): Word;
begin
  if StyleValue = 0
  then Result := 0
  else
  begin
    Result := 1;

    case StyleValue of
      1: Result := 4;
      2: Result := 4;
    end;
  end;
end;

function TDiagonalTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiThreadSafe,
      tetiUseDirtyRects
    ];
end;

procedure TDiagonalTransition.MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step,
  LastExecutedFrame: Longint; Data: TTETransitionData; Draw, CalcDirtyRects:
  Boolean);
begin
  Data.UpdateRect := Rect(0, 0, 0, 0);
  case StyleToUse of
    1: case SubStyleToUse of
      1: Style1_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      2: Style1_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      3: Style1_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      4: Style1_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    end;
    2: case SubStyleToUse of
      1: Style2_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      2: Style2_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      3: Style2_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      4: Style2_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    end;
  end;
end;

procedure TDiagonalTransition.Style1_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);

  procedure GetPoints(Frame: Integer; var xOrg, yOrg, xDst, yDst: Integer);
  begin
    if Frame > Data.Height
    then
    begin
      xOrg := Frame - Data.Height;
      yOrg := Data.Height - 1;
    end
    else
    begin
      xOrg := 0;
      yOrg := Frame - 1;
    end;

    if Frame > Data.Width
    then
    begin
      xDst := Data.Width - 1;
      yDst := Frame - Data.Width;
    end
    else
    begin
      xDst := Frame - 1;
      yDst := 0;
    end;
  end;

var
  i,
  xOrg,
  yOrg,
  xDst,
  yDst,
  xOrg2,
  yOrg2,
  xDst2,
  yDst2: Integer;
  DirtyRect: TRect;
  DirtyRectWidth: Integer;
begin
  if Step = 1
  then
  begin
    GetPoints(CurrentFrame, xOrg, yOrg, xDst, yDst);
    Windows.UnionRect(Data.UpdateRect, Rect(xOrg, yDst, xDst + 1, yOrg + 1),
      Data.UpdateRect);
    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(xOrg, yOrg);
      MaskBmp.Canvas.LineTo(xDst + 1, yDst - 1);
    end;
    if CalcDirtyRects then
    begin
      with DirtyRect do
      begin
        Left   := xOrg;
        Bottom := yOrg   + 1;
        Top    := Bottom - DirtyRectSize;
        Right  := Left   + DirtyRectSize;
      end;
      repeat
        Data.DirtyRects.AddRect(DirtyRect);
        OffsetRect(DirtyRect, DirtyRectSize, -DirtyRectSize);
      until(DirtyRect.Left > xDst) or (DirtyRect.Bottom <= yDst);
    end;
  end
  else
  begin
    if Draw then
    begin
      for i:= 1 to Step do
      begin
        GetPoints(CurrentFrame - Step + i, xOrg, yOrg, xDst, yDst);
        MaskBmp.Canvas.MoveTo(xOrg, yOrg);
        MaskBmp.Canvas.LineTo(xDst + 1, yDst - 1);
      end;
    end;

    GetPoints(CurrentFrame - Step + 1, xOrg, yOrg, xDst, yDst);
    GetPoints(CurrentFrame, xOrg2, yOrg2, xDst2, yDst2);
    Windows.UnionRect(Data.UpdateRect, Rect(xOrg, yDst, xDst2 + 1, yOrg2 + 1),
      Data.UpdateRect);

    if CalcDirtyRects then
    begin
      DirtyRectWidth := DirtyRectSize + Step - 1;
      with DirtyRect do
      begin
        Right  := xOrg2  + DirtyRectSize;
        Bottom := yOrg2  + 1;
        Left   := Right  - DirtyRectWidth;
        Top    := Bottom - DirtyRectSize;
      end;
      repeat
        Data.DirtyRects.AddRect(DirtyRect);
        OffsetRect(DirtyRect, DirtyRectSize, -DirtyRectSize);
      until(DirtyRect.Left > xDst2) or (DirtyRect.Bottom <= yDst);
    end;
  end;
end;

procedure TDiagonalTransition.Style1_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_1Frame(MaskBmp, Data,
    Data.Height + Data.Width - CurrentFrame + Step - 1, Step, Draw, CalcDirtyRects);
end;

procedure TDiagonalTransition.Style1_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);

  procedure GetPoints(Frame: Integer; var xOrg, yOrg, xDst, yDst: Integer);
  begin
    if Frame > Data.Height
    then
    begin
      xOrg := Frame - Data.Height;
      yOrg := 0;
    end
    else
    begin
      xOrg := 0;
      yOrg := Data.Height - Frame;
    end;

    if Frame > Data.Width
    then
    begin
      xDst := Data.Width - 1;
      yDst := Data.Height - (Frame - Data.Width) - 1;
    end
    else
    begin
      xDst := Frame - 1;
      yDst := Data.Height - 1;
    end;
  end;

var
  i,
  xOrg,
  yOrg,
  xDst,
  yDst,
  xOrg2,
  yOrg2,
  xDst2,
  yDst2: Integer;
  DirtyRect: TRect;
  DirtyRectWidth: Integer;
begin
  if Step = 1
  then
  begin
    GetPoints(CurrentFrame, xOrg, yOrg, xDst, yDst);
    Windows.UnionRect(Data.UpdateRect, Rect(xOrg, yOrg, xDst + 1, yDst + 1),
      Data.UpdateRect);
    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(xOrg, yOrg);
      MaskBmp.Canvas.LineTo(xDst + 1, yDst + 1);
    end;

    if CalcDirtyRects then
    begin
      with DirtyRect do
      begin
        Right  := xDst   + 1;
        Bottom := yDst   + 1;
        Left   := Right  - DirtyRectSize - 1;
        Top    := Bottom - DirtyRectSize - 1;
      end;
      repeat
        Data.DirtyRects.AddRect(DirtyRect);
        OffsetRect(DirtyRect, -DirtyRectSize, -DirtyRectSize);
      until(DirtyRect.Right <= xOrg) or (DirtyRect.Bottom <= yOrg);
    end;
  end
  else
  begin
    if Draw then
    begin
      for i:= 1 to Step do
      begin
        GetPoints(CurrentFrame - Step + i, xOrg, yOrg, xDst, yDst);
        MaskBmp.Canvas.MoveTo(xOrg, yOrg);
        MaskBmp.Canvas.LineTo(xDst + 1, yDst + 1);
      end;
    end;

    GetPoints(CurrentFrame - Step + 1, xOrg, yOrg, xDst, yDst);
    GetPoints(CurrentFrame, xOrg2, yOrg2, xDst2, yDst2);

    Windows.UnionRect(Data.UpdateRect, Rect(xOrg, yOrg2, xDst2 + 1, yDst + 1),
      Data.UpdateRect);

    if CalcDirtyRects then
    begin
      DirtyRectWidth := Step + DirtyRectSize - 1;
      with DirtyRect do
      begin
        Left   := xDst   - DirtyRectSize + 1;
        Right  := Left   + DirtyRectWidth;
        Bottom := yDst   + 1;
        Top    := Bottom - DirtyRectSize;
      end;
      repeat
        Data.DirtyRects.AddRect(DirtyRect);
        OffsetRect(DirtyRect, -DirtyRectSize, -DirtyRectSize);
      until(DirtyRect.Right <= xOrg) or (DirtyRect.Bottom <= yOrg2);
    end;
  end;
end;

procedure TDiagonalTransition.Style1_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_3Frame(MaskBmp, Data,
    Data.Height + Data.Width - CurrentFrame + Step - 1, Step, Draw, CalcDirtyRects);
end;

procedure TDiagonalTransition.Style2_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin

  if(CurrentFrame - Step + 1) mod 2 = 1
  then
  begin
    Style1_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2), Draw, CalcDirtyRects);
    if Step > 1 then
      Style1_2Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw, CalcDirtyRects);
  end
  else
  begin
    Style1_2Frame(MaskBmp, Data, CurrentFrame div 2, Step - (Step div 2), Draw, CalcDirtyRects);
    if Step > 1 then
      Style1_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw, CalcDirtyRects);
  end;
end;

procedure TDiagonalTransition.Style2_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
begin
  if(CurrentFrame - Step + 1) mod 2 = 1
  then
  begin
    Style1_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2), Draw, CalcDirtyRects);
    if Step > 1 then
      Style1_4Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw, CalcDirtyRects);
  end
  else
  begin
    Style1_4Frame(MaskBmp, Data, CurrentFrame div 2, Step - (Step div 2), Draw, CalcDirtyRects);
    if Step > 1 then
      Style1_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw, CalcDirtyRects);
  end;
end;

procedure TDiagonalTransition.Style2_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style2_1Frame(MaskBmp, Data,
    Data.Height + Data.Width - CurrentFrame + Step - 1, Step, Draw, CalcDirtyRects);
end;

procedure TDiagonalTransition.Style2_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style2_2Frame(MaskBmp, Data,
    Data.Height + Data.Width - CurrentFrame + Step - 1, Step, Draw, CalcDirtyRects);
end;

procedure TDiagonalTransition.Initialize(Data: TTETransitionData; var
  TotalFrames: Longint);
begin
  inherited;

  Data.DirtyRects.CheckBounds := True;
  Data.DirtyRects.Bounds := Rect(0, 0, Data.Width, Data.Height);
end;

initialization

  TERegisterTransition(TDiagonalTransition);

end.
