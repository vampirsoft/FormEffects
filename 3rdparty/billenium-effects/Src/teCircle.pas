unit teCircle;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teMasked, Windows, Messages, Graphics;

type
  TCircleTransition = class(TMaskedTransition)
  private
  protected
    LFUR,
    auxRect: TRect;

    function  CalcTotalFrames(Data: TTETransitionData): Longint; override;
    function  AvoidPixelRepaint: Boolean; override;
    function  OptimizeMask: Boolean; override;
    function  InversePaint: Boolean; override;
    function  CalculateReversedSubStyle(
      const StyleValue, SubStyleValue: Word): Word; override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step, LastExecutedFrame:
      Longint; Data: TTETransitionData; Draw, CalcDirtyRects: Boolean); override;
    procedure Style1_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style1_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style2_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style2_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style2_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style2_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style2_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style2_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style2_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style2_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style3_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style3_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style3_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style3_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style3_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style3_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style3_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
    procedure Style3_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw: Boolean);
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

{$ifdef D6UP}uses Types;{$endif D6UP}

constructor TCircleTransition.Create(AOwner: TComponent);
begin
  inherited;

  FCountOfStyles := 3;
end;

class function TCircleTransition.Description: String;
begin
  Result := 'Circle';
end;

function TCircleTransition.CalculateReversedSubStyle(
  const StyleValue, SubStyleValue: Word): Word;
begin
  Result := SubStyleValue;

  case StyleValue of
    1: case SubStyleValue of
       1: Result := 2;
       2: Result := 1;
    end;
    2: case SubStyleValue of
       1: Result := 5;
       2: Result := 6;
       3: Result := 7;
       4: Result := 8;
       5: Result := 1;
       6: Result := 2;
       7: Result := 3;
       8: Result := 4;
    end;
    3: case SubStyleValue of
       1: Result := 5;
       2: Result := 6;
       3: Result := 7;
       4: Result := 8;
       5: Result := 1;
       6: Result := 2;
       7: Result := 3;
       8: Result := 4;
    end;
  end;
end;

function TCircleTransition.AvoidPixelRepaint: Boolean;
begin
  Result := not InversePaint;
end;

function TCircleTransition.OptimizeMask: Boolean;
begin
  Result := True;
end;

function TCircleTransition.InversePaint: Boolean;
begin
  Result := False;
  case StyleToUse of
    1: case SubStyleToUse of
         2: Result := True;
       end;
    2: case SubStyleToUse of
         5,
         6,
         7,
         8: Result := True;
       end;
    3: case SubStyleToUse of
         5,
         6,
         7,
         8: Result := True;
       end;
  end;
end;

function TCircleTransition.CalcTotalFrames(Data: TTETransitionData): Longint;
begin
  Result := 0;

  case StyleToUse of
    1: Result := Round(Sqrt(Sqr(Data.Height div 2) + Sqr(Data.Width div 2))) + 1;
    2: Result := Round(Sqrt(Sqr(Data.Height) + Sqr(Data.Width))) + 1;
    3: case SubStyleToUse of
         1,
         2,
         5,
         6: Result := Round(Sqrt(Sqr(Data.Height div 2) + Sqr(Data.Width))) + 1;
         3,
         4,
         7,
         8: Result := Round(Sqrt(Sqr(Data.Height) + Sqr(Data.Width div 2))) + 1;
       end;
  end;
end;

function TCircleTransition.CountOfSubStyles(StyleValue: Word): Word;
begin
  if StyleValue = 0
  then Result := 0
  else
  begin
    Result := 1;

    case StyleValue of
      1: Result := 2;
      2: Result := 8;
      3: Result := 8;
    end;
  end;
end;

function TCircleTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiThreadSafe
    ];
end;

procedure TCircleTransition.MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step,
  LastExecutedFrame: Longint; Data: TTETransitionData; Draw, CalcDirtyRects:
  Boolean);
begin
  case StyleToUse of
    1: case SubStyleToUse of
      1: Style1_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      2: Style1_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
    end;
    2: case SubStyleToUse of
      1: Style2_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      2: Style2_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      3: Style2_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      4: Style2_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      5: Style2_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      6: Style2_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      7: Style2_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      8: Style2_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
    end;
    3: case SubStyleToUse of
      1: Style3_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      2: Style3_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      3: Style3_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      4: Style3_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      5: Style3_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      6: Style3_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      7: Style3_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
      8: Style3_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw);
    end;
  end;
end;

procedure TCircleTransition.Style1_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, H, Frame: Integer; ForUnUpdateRect: Boolean): TRect;
  var
    D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    c1 := (W div 2) - Frame;
    c2 := (H div 2) - Frame;
    c3 := (W div 2) + Frame;
    c4 := (H div 2) + Frame;

    Result := Rect(c1, c2, c3, c4);

    if ForUnUpdateRect then
    begin
      D := -(Frame - Round(Sqrt(Sqr(Frame) div 2)) + 1);
      InflateRect(Result, D, D);
    end;
  end;

begin
  Data.UpdateRect   := CalcRect(Data.Width, Data.Height, CurrentFrame, False);
  Data.UnUpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame - Step, True);

  if Draw then
    Ellipse(MaskBmp.Canvas.Handle, Data.UpdateRect.Left, Data.UpdateRect.Top,
      Data.UpdateRect.Right, Data.UpdateRect.Bottom);
end;

procedure TCircleTransition.Style1_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, H, Frame, Step: Integer;
    ForUpdateRect, ForUnUpdateRect: Boolean): TRect;
  var
    R, D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    R := Data.Frames - Frame;

    c1 := (W div 2) - R;
    c2 := (H div 2) - R;
    c3 := (W div 2) + R;
    c4 := (H div 2) + R;

    Result := Rect(c1, c2, c3, c4);

    if ForUpdateRect then
      InflateRect(Result, Step, Step);

    if ForUnUpdateRect then
    begin
      D := -(1 + R - Round(Sqrt(Sqr(R) div 2)));
      InflateRect(Result, D, D);
    end;
  end;

begin
  Data.UnUpdateRect :=
    CalcRect(Data.Width, Data.Height, CurrentFrame, Step, False, True);
  Data.UpdateRect :=
    CalcRect(Data.Width, Data.Height, CurrentFrame, Step, True, False);

  if Draw then
  begin
    if IsSmooth and not IsRectEmpty(Data.UnUpdateRect) then
      ExcludeClipRect(MaskBmp.Canvas.Handle, Data.UnUpdateRect.Left+1,
        Data.UnUpdateRect.Top+1, Data.UnUpdateRect.Right-1,
        Data.UnUpdateRect.Bottom-1);

    with CalcRect(Data.Width, Data.Height, CurrentFrame, Step, False, False) do
      Ellipse(MaskBmp.Canvas.Handle, Left, Top, Right, Bottom);

    if IsSmooth and not IsRectEmpty(Data.UnUpdateRect) then
      SelectClipRgn(MaskBmp.Canvas.Handle, 0);
  end;
end;

procedure TCircleTransition.Style2_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(Frame: Integer; ForUnUpdateRect: Boolean): TRect;
  var
    D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    c1 := -Frame;
    c2 := -Frame;
    c3 :=  Frame;
    c4 :=  Frame;

    Result := Rect(c1, c2, c3, c4);

    if ForUnUpdateRect then
    begin
      D := -(Frame - Round(Sqrt(Sqr(Frame) div 2)));
      InflateRect(Result, D-1, D-1);
    end;
  end;

begin
  Data.UpdateRect   := CalcRect(CurrentFrame, False);
  if Draw then
    MaskBmp.Canvas.Ellipse(Data.UpdateRect.Left, Data.UpdateRect.Top,
      Data.UpdateRect.Right, Data.UpdateRect.Bottom);
  Data.UnUpdateRect := CalcRect(CurrentFrame - Step, True);
end;

procedure TCircleTransition.Style2_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, H, Frame: Integer; ForUnUpdateRect: Boolean): TRect;
  var
    D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    c1 := W - Frame;
    c2 := H - Frame;
    c3 := W + Frame;
    c4 := H + Frame;

    Result := Rect(c1, c2, c3, c4);

    if ForUnUpdateRect then
    begin
      D := -(Frame - Round(Sqrt(Sqr(Frame) div 2)));
      InflateRect(Result, D-1, D-1);
    end;
  end;

begin
  Data.UpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame, False);
  if Draw then
    MaskBmp.Canvas.Ellipse(Data.UpdateRect.Left, Data.UpdateRect.Top,
      Data.UpdateRect.Right, Data.UpdateRect.Bottom);
  Data.UnUpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame - Step, True);
end;

procedure TCircleTransition.Style2_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(H, Frame: Integer; ForUnUpdateRect: Boolean): TRect;
  var
    D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    c1 := - Frame;
    c2 := H - Frame;
    c3 := Frame;
    c4 := H + Frame;

    Result := Rect(c1, c2, c3, c4);

    if ForUnUpdateRect then
    begin
      D := -(Frame - Round(Sqrt(Sqr(Frame) div 2)));
      InflateRect(Result, D-1, D-1);
    end;
  end;

begin
  Data.UpdateRect := CalcRect(Data.Height, CurrentFrame, False);
  if Draw then
    MaskBmp.Canvas.Ellipse(Data.UpdateRect.Left, Data.UpdateRect.Top,
      Data.UpdateRect.Right, Data.UpdateRect.Bottom);
  Data.UnUpdateRect := CalcRect(Data.Height, CurrentFrame - Step, True);
end;

procedure TCircleTransition.Style2_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, Frame: Integer; ForUnUpdateRect: Boolean): TRect;
  var
    D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    c1 := W - Frame;
    c2 := - Frame;
    c3 := W + Frame;
    c4 := Frame;

    Result := Rect(c1, c2, c3, c4);

    if ForUnUpdateRect then
    begin
      D := -(Frame - Round(Sqrt(Sqr(Frame) div 2)));
      InflateRect(Result, D-1, D-1);
    end;
  end;

begin
  Data.UpdateRect := CalcRect(Data.Width, CurrentFrame, False);
  if Draw then
    MaskBmp.Canvas.Ellipse(Data.UpdateRect.Left, Data.UpdateRect.Top,
      Data.UpdateRect.Right, Data.UpdateRect.Bottom);
  Data.UnUpdateRect := CalcRect(Data.Width, CurrentFrame - Step, True);
end;

procedure TCircleTransition.Style2_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(Frame: Integer;
    ForUpdateRect, ForUnUpdateRect: Boolean): TRect;
  var
    R, D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    R := Data.Frames - Frame;

    c1 := -R;
    c2 := -R;
    c3 := R;
    c4 := R;

    Result := Rect(c1, c2, c3, c4);

    if ForUpdateRect then
      InflateRect(Result, Step, Step);

    if ForUnUpdateRect then
    begin
      D := 1 + R - Round(Sqrt(Sqr(R) div 2));
      InflateRect(Result, -D, -D);
    end;
  end;

begin
  Data.UpdateRect :=
    CalcRect(CurrentFrame - Step, True, False);
  Data.UnUpdateRect := CalcRect(CurrentFrame, False, True);
  if Draw then
    with CalcRect(CurrentFrame, False, False) do
      MaskBmp.Canvas.Ellipse(Left, Top, Right, Bottom);
end;

procedure TCircleTransition.Style2_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, H, Frame: Integer;
    ForUpdateRect, ForUnUpdateRect: Boolean): TRect;
  var
    R, D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    R := Data.Frames - Frame;

    c1 := W - R;
    c2 := H - R;
    c3 := W + R;
    c4 := H + R;

    Result := Rect(c1, c2, c3, c4);

    if ForUpdateRect then
      InflateRect(Result, Step, Step);

    if ForUnUpdateRect then
    begin
      D := 1 + R - Round(Sqrt(Sqr(R) div 2));
      InflateRect(Result, -D, -D);
    end;
  end;

begin
  Data.UpdateRect :=
    CalcRect(Data.Width, Data.Height, CurrentFrame - Step, True, False);
  Data.UnUpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame, False, True);
  if Draw then
    with CalcRect(Data.Width, Data.Height, CurrentFrame, False, False) do
      MaskBmp.Canvas.Ellipse(Left, Top, Right, Bottom);
end;

procedure TCircleTransition.Style2_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(H, Frame: Integer;
    ForUpdateRect, ForUnUpdateRect: Boolean): TRect;
  var
    R, D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    R := Data.Frames - Frame;

    c1 := - R;
    c2 := H - R;
    c3 := R;
    c4 := H + R;

    Result := Rect(c1, c2, c3, c4);

    if ForUpdateRect then
      InflateRect(Result, Step, Step);

    if ForUnUpdateRect then
    begin
      D := 1 + R - Round(Sqrt(Sqr(R) div 2));
      InflateRect(Result, -D, -D);
    end;
  end;

begin
  Data.UpdateRect :=
    CalcRect(Data.Height, CurrentFrame - Step, True, False);
  Data.UnUpdateRect := CalcRect(Data.Height, CurrentFrame, False, True);
  if Draw then
    with CalcRect(Data.Height, CurrentFrame, False, False) do
      MaskBmp.Canvas.Ellipse(Left, Top, Right, Bottom);
end;

procedure TCircleTransition.Style2_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, Frame: Integer;
    ForUpdateRect, ForUnUpdateRect: Boolean): TRect;
  var
    R, D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    R := Data.Frames - Frame;

    c1 := W - R;
    c2 := - R;
    c3 := W + R;
    c4 := R;

    Result := Rect(c1, c2, c3, c4);

    if ForUpdateRect then
      InflateRect(Result, Step, Step);

    if ForUnUpdateRect then
    begin
      D := 1 + R - Round(Sqrt(Sqr(R) div 2));
      InflateRect(Result, -D, -D);
    end;
  end;

begin
  Data.UpdateRect :=
    CalcRect(Data.Width, CurrentFrame - Step, True, False);
  Data.UnUpdateRect := CalcRect(Data.Width, CurrentFrame, False, True);
  if Draw then
    with CalcRect(Data.Width, CurrentFrame, False, False) do
      MaskBmp.Canvas.Ellipse(Left, Top, Right, Bottom);
end;

procedure TCircleTransition.Style3_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(H, Frame: Integer; ForUnUpdateRect: Boolean): TRect;
  var
    D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    c1 := -Frame;
    c2 := (H div 2) - Frame;
    c3 := Frame;
    c4 := (H div 2) + Frame;

    Result := Rect(c1, c2, c3, c4);

    if ForUnUpdateRect then
    begin
      D := -(Frame - Round(Sqrt(Sqr(Frame) div 2)));
      InflateRect(Result, D-1, D-1);
    end;
  end;

begin
  Data.UpdateRect := CalcRect(Data.Height, CurrentFrame, False);
  if Draw then
    MaskBmp.Canvas.Ellipse(Data.UpdateRect.Left, Data.UpdateRect.Top,
      Data.UpdateRect.Right, Data.UpdateRect.Bottom);
  Data.UnUpdateRect := CalcRect(Data.Height, CurrentFrame - Step, True);
end;

procedure TCircleTransition.Style3_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, H, Frame: Integer; ForUnUpdateRect: Boolean): TRect;
  var
    D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    c1 := W - Frame;
    c2 := (H div 2) - Frame;
    c3 := W + Frame;
    c4 := (H div 2) + Frame;

    Result := Rect(c1, c2, c3, c4);

    if ForUnUpdateRect then
    begin
      D := -(Frame - Round(Sqrt(Sqr(Frame) div 2)));
      InflateRect(Result, D-1, D-1);
    end;
  end;

begin
  Data.UpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame, False);
  if Draw then
    MaskBmp.Canvas.Ellipse(Data.UpdateRect.Left, Data.UpdateRect.Top,
      Data.UpdateRect.Right, Data.UpdateRect.Bottom);
  Data.UnUpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame - Step, True);
end;

procedure TCircleTransition.Style3_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, Frame: Integer; ForUnUpdateRect: Boolean): TRect;
  var
    D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    c1 := (W div 2) - Frame;
    c2 := -Frame;
    c3 := (W div 2) + Frame;
    c4 := Frame;

    Result := Rect(c1, c2, c3, c4);

    if ForUnUpdateRect then
    begin
      D := -(Frame - Round(Sqrt(Sqr(Frame) div 2)));
      InflateRect(Result, D-1, D-1);
    end;
  end;

begin
  Data.UpdateRect := CalcRect(Data.Width, CurrentFrame, False);
  if Draw then
    MaskBmp.Canvas.Ellipse(Data.UpdateRect.Left, Data.UpdateRect.Top,
      Data.UpdateRect.Right, Data.UpdateRect.Bottom);
  Data.UnUpdateRect := CalcRect(Data.Width, CurrentFrame - Step, True);
end;

procedure TCircleTransition.Style3_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, H, Frame: Integer; ForUnUpdateRect: Boolean): TRect;
  var
    D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    c1 := (W div 2) - Frame;
    c2 := H - Frame;
    c3 := (W div 2) + Frame;
    c4 := H + Frame;

    Result := Rect(c1, c2, c3, c4);

    if ForUnUpdateRect then
    begin
      D := -(Frame - Round(Sqrt(Sqr(Frame) div 2)));
      InflateRect(Result, D-1, D-1);
    end;
  end;

begin
  Data.UpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame, False);
  if Draw then
    MaskBmp.Canvas.Ellipse(Data.UpdateRect.Left, Data.UpdateRect.Top,
      Data.UpdateRect.Right, Data.UpdateRect.Bottom);
  Data.UnUpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame - Step, True);
end;

procedure TCircleTransition.Style3_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(H, Frame: Integer;
    ForUpdateRect, ForUnUpdateRect: Boolean): TRect;
  var
    R, D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    R := Data.Frames - Frame;

    c1 := -R;
    c2 := (H div 2) - R;
    c3 := R;
    c4 := (H div 2) + R;

    Result := Rect(c1, c2, c3, c4);

    if ForUpdateRect then
      InflateRect(Result, Step, Step);

    if ForUnUpdateRect then
    begin
      D := 1 + R - Round(Sqrt(Sqr(R) div 2));
      InflateRect(Result, -D, -D);
    end;
  end;

begin
  Data.UpdateRect :=
    CalcRect(Data.Height, CurrentFrame - Step, True, False);
  Data.UnUpdateRect := CalcRect(Data.Height, CurrentFrame, False, True);
  if Draw then
    with CalcRect(Data.Height, CurrentFrame, False, False) do
      MaskBmp.Canvas.Ellipse(Left, Top, Right, Bottom);
end;

procedure TCircleTransition.Style3_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, H, Frame: Integer;
    ForUpdateRect, ForUnUpdateRect: Boolean): TRect;
  var
    R, D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    R := Data.Frames - Frame;

    c1 := W - R;
    c2 := (H div 2) - R;
    c3 := W + R;
    c4 := (H div 2) + R;

    Result := Rect(c1, c2, c3, c4);

    if ForUpdateRect then
      InflateRect(Result, Step, Step);

    if ForUnUpdateRect then
    begin
      D := 1 + R - Round(Sqrt(Sqr(R) div 2));
      InflateRect(Result, -D, -D);
    end;
  end;

begin
  Data.UpdateRect :=
    CalcRect(Data.Width, Data.Height, CurrentFrame - Step, True, False);
  Data.UnUpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame, False, True);
  if Draw then
    with CalcRect(Data.Width, Data.Height, CurrentFrame, False, False) do
      MaskBmp.Canvas.Ellipse(Left, Top, Right, Bottom);
end;

procedure TCircleTransition.Style3_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, Frame: Integer;
    ForUpdateRect, ForUnUpdateRect: Boolean): TRect;
  var
    R, D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    R := Data.Frames - Frame;

    c1 := (W div 2) - R;
    c2 := -R;
    c3 := (W div 2) + R;
    c4 := R;

    Result := Rect(c1, c2, c3, c4);

    if ForUpdateRect then
      InflateRect(Result, Step, Step);

    if ForUnUpdateRect then
    begin
      D := 1 + R - Round(Sqrt(Sqr(R) div 2));
      InflateRect(Result, -D, -D);
    end;
  end;

begin
  Data.UpdateRect :=
    CalcRect(Data.Width, CurrentFrame - Step, True, False);
  Data.UnUpdateRect := CalcRect(Data.Width, CurrentFrame, False, True);
  if Draw then
    with CalcRect(Data.Width, CurrentFrame, False, False) do
      MaskBmp.Canvas.Ellipse(Left, Top, Right, Bottom);
end;

procedure TCircleTransition.Style3_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint; Draw: Boolean);

  function CalcRect(W, H, Frame: Integer;
    ForUpdateRect, ForUnUpdateRect: Boolean): TRect;
  var
    R, D: Integer;
    c1, c2, c3, c4: Integer;
  begin
    R := Data.Frames - Frame;

    c1 := (W div 2) - R;
    c2 := H - R;
    c3 := (W div 2) + R;
    c4 := H + R;

    Result := Rect(c1, c2, c3, c4);

    if ForUpdateRect then
      InflateRect(Result, Step, Step);

    if ForUnUpdateRect then
    begin
      D := 1 + R - Round(Sqrt(Sqr(R) div 2));
      InflateRect(Result, -D, -D);
    end;
  end;

begin
  Data.UpdateRect :=
    CalcRect(Data.Width, Data.Height, CurrentFrame - Step, True, False);
  Data.UnUpdateRect := CalcRect(Data.Width, Data.Height, CurrentFrame, False, True);
  if Draw then
    with CalcRect(Data.Width, Data.Height, CurrentFrame, False, False) do
      MaskBmp.Canvas.Ellipse(Left, Top, Right, Bottom);
end;

initialization

  TERegisterTransition(TCircleTransition);

end.

