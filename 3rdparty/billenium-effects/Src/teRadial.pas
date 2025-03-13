unit teRadial;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teMasked, Windows, Messages, Graphics;

type
  TRadialTransition = class(TMaskedTransition)
  private
  protected
    Center: TPoint;
    ActiveCorners: Array[1..4] of Boolean;
    CornerPositions: Array[1..4] of Longint;
    CornerPoints: Array[1..4] of TPoint;
    MaxFrames: Longint;

    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    function  CalcTotalFrames(Data: TTETransitionData): Longint; override;
    function  AvoidPixelRepaint: Boolean; override;
    function  ResetMaskBmp(Device: TTETransitionDevice): Boolean; override;
    function  CalculateReversedSubStyle(
      const StyleValue, SubStyleValue: Word): Word; override;
    procedure MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step, LastExecutedFrame:
      Longint; Data: TTETransitionData; Draw, CalcDirtyRects: Boolean); override;
    procedure DrawMask(Origin: TPoint; MaskBmp: TBitmap; Data: TTETransitionData;
      Position, Step: Longint; Draw, CalcDirtyRects: Boolean);
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure Style1_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style1_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style2_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style3_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style3_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style3_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style3_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style3_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style3_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style3_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style3_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style4_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style4_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style4_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style4_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style4_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style4_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style4_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style4_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_9Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_10Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_11Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_12Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_13Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_14Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_15Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style5_16Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style6_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style6_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style6_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style6_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style6_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style6_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style6_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style6_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_1Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_2Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_3Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_4Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_5Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_6Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_7Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_8Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_9Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_10Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_11Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_12Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_13Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_14Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_15Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
    procedure Style7_16Frame(MaskBmp: TBitmap; Data: TTETransitionData;
      CurrentFrame, Step: Longint; Draw, CalcDirtyRects: Boolean);
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;

    function CountOfSubStyles(StyleValue: Word): Word; override;

    property CountOfStyles;
  published
    property Reversed;
    property SmoothingLevel;
    property Style;
    property SubStyle;
  end;

implementation

constructor TRadialTransition.Create(AOwner: TComponent);
begin
  inherited;

  FCountOfStyles := 7;
end;

class function TRadialTransition.Description: String;
begin
  Result := 'Radial';
end;

function TRadialTransition.CalculateReversedSubStyle(
  const StyleValue, SubStyleValue: Word): Word;
begin
  Result := SubStyleValue;

  case StyleValue of
    1: case SubStyleValue of
       1: Result := 2;
       2: Result := 1;
       3: Result := 4;
       4: Result := 3;
       5: Result := 6;
       6: Result := 5;
       7: Result := 8;
       8: Result := 7;
    end;
    2: case SubStyleValue of
       1: Result := 2;
       2: Result := 1;
       3: Result := 4;
       4: Result := 3;
       5: Result := 6;
       6: Result := 5;
       7: Result := 8;
       8: Result := 7;
    end;
    3: case SubStyleValue of
       1: Result := 2;
       2: Result := 1;
       3: Result := 4;
       4: Result := 3;
       5: Result := 6;
       6: Result := 5;
       7: Result := 8;
       8: Result := 7;
    end;
    4: case SubStyleValue of
       1: Result := 2;
       2: Result := 1;
       3: Result := 4;
       4: Result := 3;
       5: Result := 6;
       6: Result := 5;
       7: Result := 8;
       8: Result := 7;
    end;
    5: case SubStyleValue of
        1: Result :=  2;
        2: Result :=  1;
        3: Result :=  4;
        4: Result :=  3;
        5: Result :=  6;
        6: Result :=  5;
        7: Result :=  8;
        8: Result :=  9;
        9: Result := 10;
       10: Result :=  9;
       11: Result := 12;
       12: Result := 11;
       13: Result := 14;
       14: Result := 13;
       15: Result := 16;
       16: Result := 15;
    end;
    6: case SubStyleValue of
       1: Result := 2;
       2: Result := 1;
       3: Result := 4;
       4: Result := 3;
       5: Result := 6;
       6: Result := 5;
       7: Result := 8;
       8: Result := 7;
    end;
    7: case SubStyleValue of
        1: Result :=  2;
        2: Result :=  1;
        3: Result :=  4;
        4: Result :=  3;
        5: Result :=  6;
        6: Result :=  5;
        7: Result :=  8;
        8: Result :=  9;
        9: Result := 10;
       10: Result :=  9;
       11: Result := 12;
       12: Result := 11;
       13: Result := 14;
       14: Result := 13;
       15: Result := 16;
       16: Result := 15;
    end;
  end;
end;

function TRadialTransition.AvoidPixelRepaint: Boolean;
begin
  Result := True;
end;

function TRadialTransition.ResetMaskBmp(Device: TTETransitionDevice): Boolean;
begin
  Result := not Smooth(Device);
end;

procedure TRadialTransition.Initialize(Data: TTETransitionData; var
  TotalFrames: Longint);
var
  W,
  H: Integer;
begin
  // The corners have to be bordering to ensure the last line point is painted
  // and the with and height have to be an odd number to get straight lines at
  // middle positions
  W        := Data.Width;
  H        := Data.Height;
  if not Odd(W) then
    Inc(W);
  if not Odd(H) then
    Inc(H);

  Center.x           := W div 2;
  Center.y           := H div 2;
  CornerPositions[1] := 1;
  CornerPositions[2] := W + 2;
  CornerPositions[3] := CornerPositions[2] + H + 1;
  CornerPositions[4] := CornerPositions[3] + W + 1;
  CornerPoints   [1] := Point(-1, -1);
  CornerPoints   [2] := Point( W, -1);
  CornerPoints   [3] := Point( W,  H);
  CornerPoints   [4] := Point(-1,  H);
  ActiveCorners  [1] := True;
  ActiveCorners  [2] := True;
  ActiveCorners  [3] := True;
  ActiveCorners  [4] := True;
  case StyleToUse of
    4: case SubStyleToUse of
         1,
         2:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[4] := False;
           end;
         3,
         4:
           begin
             ActiveCorners[2] := False;
             ActiveCorners[3] := False;
           end;
         5,
         6:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[2] := False;
           end;
         7,
         8:
           begin
             ActiveCorners[3] := False;
             ActiveCorners[4] := False;
           end;
       end;
    5: case SubStyleToUse of
         1,
         2,
         3,
         4:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[4] := False;
           end;
         5,
         6,
         7,
         8:
           begin
             ActiveCorners[2] := False;
             ActiveCorners[3] := False;
           end;
         9,
         10,
         11,
         12:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[2] := False;
           end;
         13,
         14,
         15,
         16:
           begin
             ActiveCorners[3] := False;
             ActiveCorners[4] := False;
           end;
       end;
    6: case SubStyleToUse of
         1,
         2:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[2] := False;
             ActiveCorners[4] := False;
           end;
         3,
         4:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[2] := False;
             ActiveCorners[3] := False;
           end;
         5,
         6:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[3] := False;
             ActiveCorners[4] := False;
           end;
         7,
         8:
           begin
             ActiveCorners[2] := False;
             ActiveCorners[3] := False;
             ActiveCorners[4] := False;
           end;
       end;
    7: case SubStyleToUse of
         1,
         2,
         3,
         4:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[2] := False;
             ActiveCorners[4] := False;
           end;
         5,
         6,
         7,
         8:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[2] := False;
             ActiveCorners[3] := False;
           end;
         9,
         10,
         11,
         12:
           begin
             ActiveCorners[1] := False;
             ActiveCorners[3] := False;
             ActiveCorners[4] := False;
           end;
         13,
         14,
         15,
         16:
           begin
             ActiveCorners[2] := False;
             ActiveCorners[3] := False;
             ActiveCorners[4] := False;
           end;
       end;
  end;

  inherited;

  Data.DirtyRects.CheckBounds := True;
  Data.DirtyRects.Bounds := Rect(0, 0, Data.Width, Data.Height);
end;

function TRadialTransition.CalcTotalFrames(Data: TTETransitionData): Longint;
begin
  Result := 0;
  MaxFrames := ((CornerPoints[3].x + 1) * 2) + ((CornerPoints[3].y + 1) * 2);

  case StyleToUse of
    1: Result := MaxFrames;
    2: if IsSmooth
       then Result := MaxFrames div 2
       else Result := MaxFrames;
    3: begin
         Result := MaxFrames div 4;
         if IsSmooth and (SubStyleToUse in [5, 6, 7, 8]) then
           Inc(Result);
       end;
    4: case SubStyleToUse of
         1,
         2,
         3,
         4: Result := MaxFrames - CornerPoints[3].y;
         5,
         6,
         7,
         8: Result := MaxFrames - CornerPoints[3].x;
       end;
    5: case SubStyleToUse of
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8: if IsSmooth
             then Result := (MaxFrames - CornerPoints[3].y) div 2
             else Result :=  MaxFrames - CornerPoints[3].y;
          9,
         10,
         11,
         12,
         13,
         14,
         15,
         16: if IsSmooth
             then Result := (MaxFrames - CornerPoints[3].x) div 2
             else Result :=  MaxFrames - CornerPoints[3].x;
       end;
    6: Result := MaxFrames div 2 + 0;
    7: if IsSmooth
       then Result := MaxFrames div 4
       else Result := MaxFrames div 2;
  end;
end;

function TRadialTransition.CountOfSubStyles(StyleValue: Word): Word;
begin
  if StyleValue = 0
  then Result := 0
  else
  begin
    Result := 1;

    case StyleValue of
      1: Result :=  8;
      2: Result :=  8;
      3: Result :=  8;
      4: Result :=  8;
      5: Result := 16;
      6: Result :=  8;
      7: Result := 16;
    end;
  end;
end;

procedure TRadialTransition.MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step,
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
      5: Style1_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      6: Style1_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      7: Style1_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      8: Style1_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    end;
    2: case SubStyleToUse of
      1: Style2_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      2: Style2_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      3: Style2_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      4: Style2_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      5: Style2_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      6: Style2_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      7: Style2_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      8: Style2_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    end;
    3: case SubStyleToUse of
      1: Style3_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      2: Style3_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      3: Style3_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      4: Style3_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      5: Style3_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      6: Style3_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      7: Style3_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      8: Style3_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    end;
    4: case SubStyleToUse of
      1: Style4_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      2: Style4_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      3: Style4_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      4: Style4_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      5: Style4_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      6: Style4_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      7: Style4_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      8: Style4_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    end;
    5: case SubStyleToUse of
       1: Style5_1Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       2: Style5_2Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       3: Style5_3Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       4: Style5_4Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       5: Style5_5Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       6: Style5_6Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       7: Style5_7Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       8: Style5_8Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       9: Style5_9Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      10: Style5_10Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      11: Style5_11Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      12: Style5_12Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      13: Style5_13Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      14: Style5_14Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      15: Style5_15Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      16: Style5_16Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    end;
    6: case SubStyleToUse of
      1: Style6_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      2: Style6_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      3: Style6_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      4: Style6_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      5: Style6_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      6: Style6_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      7: Style6_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      8: Style6_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    end;
    7: case SubStyleToUse of
       1: Style7_1Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       2: Style7_2Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       3: Style7_3Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       4: Style7_4Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       5: Style7_5Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       6: Style7_6Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       7: Style7_7Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       8: Style7_8Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
       9: Style7_9Frame (MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      10: Style7_10Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      11: Style7_11Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      12: Style7_12Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      13: Style7_13Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      14: Style7_14Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      15: Style7_15Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
      16: Style7_16Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.DrawMask(Origin: TPoint; MaskBmp: TBitmap;
  Data: TTETransitionData; Position, Step: Longint;
  Draw, CalcDirtyRects: Boolean);

  procedure GetPoints(Position: Longint; var x, y: Integer);
  begin
    if Position < CornerPositions[2]
    then
    begin
      x := Position - 2;
      y := CornerPoints[2].y;
    end
    else if Position < CornerPositions[3]
    then
    begin
      x := CornerPoints[2].x;
      y := Position - CornerPositions[2] - 1;
    end
    else if Position < CornerPositions[4]
    then
    begin
      x := CornerPositions[4] - Position - 1;
      y := CornerPoints[3].y;
    end
    else
    begin
      x := CornerPoints[4].x;
      y := CornerPoints[4].y + CornerPositions[4] - Position;
    end;
  end;

  procedure AddPointToRect(const Point: TPoint; var UpdateRect: TRect);
  begin
    if IsRectEmpty(UpdateRect)
    then UpdateRect := Rect(Point.x, Point.y, Point.x + 1, Point.y + 1)
    else
    begin
      if Point.x <  UpdateRect.Left then
        UpdateRect.Left   := Point.x;
      if Point.x >= UpdateRect.Right then
        UpdateRect.Right  := Point.x + 1;
      if Point.y <  UpdateRect.Top then
        UpdateRect.Top    := Point.y;
      if Point.y >= UpdateRect.Bottom then
        UpdateRect.Bottom := Point.y + 1;
    end;
  end;

var
  xDst,
  yDst,
  xDst2,
  yDst2: Integer;
  PrevPosition: Longint;
  Points: array[0..5] of TPoint;
  Count: Integer;
  DirtyRect: TRect;
begin
  DirtyRect := Rect(0, 0, 0, 0);
  if Step = 1 then
  begin
    GetPoints(Position, xDst, yDst);
    AddPointToRect(Origin, DirtyRect);
    AddPointToRect(Point(xDst, yDst), DirtyRect);
    if Draw then
    begin
      MaskBmp.Canvas.MoveTo(Origin.x, Origin.y);
      MaskBmp.Canvas.LineTo(xDst, yDst);
    end;
  end
  else
  begin
    PrevPosition := Position - Step + 1;
    if PrevPosition < 1
    then Inc(PrevPosition, MaxFrames)
    else if PrevPosition > MaxFrames then
      Dec(PrevPosition, MaxFrames);

    GetPoints(PrevPosition, xDst , yDst );
    GetPoints(Position    , xDst2, yDst2);

    Points[0] := Origin;
    AddPointToRect(Points[0], DirtyRect);
    Points[1] := Point(xDst, yDst);
    AddPointToRect(Points[1], DirtyRect);
    Count := 2;
    if ActiveCorners[2] and (PrevPosition < CornerPositions[2])
    then
    begin
      if(Position < PrevPosition) or (Position > CornerPositions[2]) then
      begin
        Points[Count] := CornerPoints[2];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[3] and
        ((Position < PrevPosition) or (Position > CornerPositions[3])) then
      begin
        Points[Count] := CornerPoints[3];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[4] and
        (Position < PrevPosition) or (Position > CornerPositions[4]) then
      begin
        Points[Count] := CornerPoints[4];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[1] and
        (Position < PrevPosition) and (Position > CornerPositions[1]) then
      begin
        Points[Count] := CornerPoints[1];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
    end
    else if ActiveCorners[3] and (PrevPosition < CornerPositions[3])
    then
    begin
      if ActiveCorners[3] and
        (Position < PrevPosition) or (Position > CornerPositions[3]) then
      begin
        Points[Count] := CornerPoints[3];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[4] and
        (Position < PrevPosition) or (Position > CornerPositions[4]) then
      begin
        Points[Count] := CornerPoints[4];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[1] and
        (Position < PrevPosition) and (Position > CornerPositions[1]) then
      begin
        Points[Count] := CornerPoints[1];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[2] and
        (Position < PrevPosition) and (Position > CornerPositions[2]) then
      begin
        Points[Count] := CornerPoints[2];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
    end
    else if ActiveCorners[4] and (PrevPosition < CornerPositions[4])
    then
    begin
      if ActiveCorners[4] and
        (Position < PrevPosition) or (Position > CornerPositions[4]) then
      begin
        Points[Count] := CornerPoints[4];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[1] and
        (Position < PrevPosition) and (Position > CornerPositions[1]) then
      begin
        Points[Count] := CornerPoints[1];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[2] and
        (Position < PrevPosition) and (Position > CornerPositions[2]) then
      begin
        Points[Count] := CornerPoints[2];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[3] and
        (Position < PrevPosition) and (Position > CornerPositions[3]) then
      begin
        Points[Count] := CornerPoints[3];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
    end
    else if ActiveCorners[1] then
    begin
      if ActiveCorners[1] and
        (Position < PrevPosition) and (Position > CornerPositions[1]) then
      begin
        Points[Count] := CornerPoints[1];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[2] and
        (Position < PrevPosition) and (Position > CornerPositions[2]) then
      begin
        Points[Count] := CornerPoints[2];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[3] and
        (Position < PrevPosition) and (Position > CornerPositions[3]) then
      begin
        Points[Count] := CornerPoints[3];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
      if ActiveCorners[4] and
        (Position < PrevPosition) and (Position > CornerPositions[4]) then
      begin
        Points[Count] := CornerPoints[4];
        AddPointToRect(Points[Count], DirtyRect);
        Inc(Count);
      end;
    end;

    Points[Count] := Point(xDst2, yDst2);
    AddPointToRect(Points[Count], DirtyRect);
    if Draw then
      MaskBmp.Canvas.Polygon(Slice(Points, Count+1));
  end;

  Windows.UnionRect(Data.UpdateRect, DirtyRect, Data.UpdateRect);

  if CalcDirtyRects then
    Data.DirtyRects.AddRect(DirtyRect);
end;

function TRadialTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiThreadSafe,
      tetiUseDirtyRects
    ];
end;

procedure TRadialTransition.Style1_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := CurrentFrame + (CornerPoints[3].x div 2) + 1;
  if Position > MaxFrames then
    Dec(Position, MaxFrames);
  DrawMask(Center, MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style1_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := (MaxFrames - CurrentFrame + 1) + Step;
  if Position < 1 then
    Inc(Position, MaxFrames);
  Style1_1Frame(MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style1_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_1Frame(MaskBmp, Data, CurrentFrame + MaxFrames div 4, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style1_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := (MaxFrames - CurrentFrame + 1) + Step;
  if Position < 1 then
    Inc(Position, MaxFrames);
  Style1_3Frame(MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style1_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := CurrentFrame + CornerPoints[3].x + CornerPoints[3].y +
    ((CornerPoints[3].x - 1) div 2) + 3;
  if Position > MaxFrames then
    Dec(Position, MaxFrames);
  DrawMask(Center, MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style1_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := (MaxFrames - CurrentFrame + 1) + Step;
  if Position < 1 then
    Inc(Position, MaxFrames);
  Style1_5Frame(MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style1_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := CurrentFrame + (CornerPoints[3].x * 2 ) + CornerPoints[3].y +
    ((CornerPoints[3].y - 1) div 2) + 4;
  if Position > MaxFrames then
    Dec(Position, MaxFrames);
  DrawMask(Center, MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style1_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := (MaxFrames - CurrentFrame + 1) + Step;
  if Position < 1 then
    Inc(Position, MaxFrames);
  Style1_7Frame(MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style2_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style1_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style1_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style1_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style1_5Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw, CalcDirtyRects);
    end
    else
    begin
      Style1_5Frame(MaskBmp, Data, CurrentFrame div 2, Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style1_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw, CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style2_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  if not IsSmooth
  then
  begin
    Position := (MaxFrames - CurrentFrame + 2) + Step;
    if Position < 1 then
      Inc(Position, MaxFrames);
    Style2_1Frame(MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
  end
  else
  begin
    Style1_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style1_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  end;
end;

procedure TRadialTransition.Style2_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style1_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style1_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style1_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style1_7Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw, CalcDirtyRects);
    end
    else
    begin
      Style1_7Frame(MaskBmp, Data, CurrentFrame div 2, Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style1_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw, CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style2_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  if not IsSmooth
  then
  begin
    Position := (MaxFrames - CurrentFrame + 2) + Step;
    if Position < 1 then
      Inc(Position, MaxFrames);
    Style2_3Frame(MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
  end
  else
  begin
    Style1_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style1_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  end;
end;

procedure TRadialTransition.Style2_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style1_1Frame(MaskBmp, Data, CurrentFrame    , Step, Draw, CalcDirtyRects);
    Style1_2Frame(MaskBmp, Data, CurrentFrame + 1, Step, Draw, CalcDirtyRects);
  end
  else
  begin
    if not Odd(CurrentFrame - Step)
    then
    begin
      Style1_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style1_2Frame(MaskBmp, Data, (CurrentFrame div 2) + 1, Step div 2, Draw, CalcDirtyRects);
    end
    else
    begin
      Style1_2Frame(MaskBmp, Data, (CurrentFrame div 2) + 1, Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style1_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw, CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style2_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  if not IsSmooth
  then
  begin
    Position := (MaxFrames - CurrentFrame + 2) + Step;
    if Position < 1 then
      Inc(Position, MaxFrames);
    Style2_5Frame(MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
  end
  else
  begin
    Style1_5Frame(MaskBmp, Data, CurrentFrame    , Step, Draw, CalcDirtyRects);
    Style1_6Frame(MaskBmp, Data, CurrentFrame + 1, Step, Draw, CalcDirtyRects);
  end;
end;

procedure TRadialTransition.Style2_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style1_3Frame(MaskBmp, Data, CurrentFrame    , Step, Draw, CalcDirtyRects);
    Style1_4Frame(MaskBmp, Data, CurrentFrame + 1, Step, Draw, CalcDirtyRects);
  end
  else
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
end;

procedure TRadialTransition.Style2_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  if not IsSmooth
  then
  begin
    Position := (MaxFrames - CurrentFrame + 2) + Step;
    if Position < 1 then
      Inc(Position, MaxFrames);
    Style2_7Frame(MaskBmp, Data, Position, Step, Draw, CalcDirtyRects);
  end
  else
  begin
    Style1_7Frame(MaskBmp, Data, CurrentFrame    , Step, Draw, CalcDirtyRects);
    Style1_8Frame(MaskBmp, Data, CurrentFrame + 1, Step, Draw, CalcDirtyRects);
  end;
end;

procedure TRadialTransition.Style3_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style3_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style3_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_1Frame(MaskBmp, Data, CurrentFrame    , Step, Draw, CalcDirtyRects);
  Style1_2Frame(MaskBmp, Data, CurrentFrame + 1, Step, Draw, CalcDirtyRects);
  Style1_5Frame(MaskBmp, Data, CurrentFrame    , Step, Draw, CalcDirtyRects);
  Style1_6Frame(MaskBmp, Data, CurrentFrame + 1, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style3_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_3Frame(MaskBmp, Data, CurrentFrame    , Step, Draw, CalcDirtyRects);
  Style1_4Frame(MaskBmp, Data, CurrentFrame + 1, Step, Draw, CalcDirtyRects);
  Style1_7Frame(MaskBmp, Data, CurrentFrame    , Step, Draw, CalcDirtyRects);
  Style1_8Frame(MaskBmp, Data, CurrentFrame + 1, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style3_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style3_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style3_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_2Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_8Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style3_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style1_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
  Style1_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style4_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  DrawMask(Point(CornerPoints[1].x, Center.y), MaskBmp, Data, CurrentFrame,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style4_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style4_1Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style4_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style4_4Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style4_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := CurrentFrame + CornerPositions[3] - 1;
  if Position > MaxFrames then
    Dec(Position, MaxFrames);
  DrawMask(Point(CornerPoints[2].x, Center.y), MaskBmp, Data, Position, Step,
    Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style4_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style4_6Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style4_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := CurrentFrame + CornerPositions[2] - 1;
  if Position < 1 then
    Inc(Position, MaxFrames);
  DrawMask(Point(Center.x, CornerPoints[1].y), MaskBmp, Data, Position, Step,
    Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style4_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := CurrentFrame + CornerPositions[4] - 1;
  if Position > MaxFrames then
    Dec(Position, MaxFrames);
  DrawMask(Point(Center.x, CornerPoints[3].y), MaskBmp, Data, Position, Step,
    Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style4_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style4_7Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style5_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_1Frame(MaskBmp, Data, CurrentFrame                         ,
      Step, Draw, CalcDirtyRects);
    Style4_1Frame(MaskBmp, Data, CurrentFrame + (Data.Frames + 1),
      Step, Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style4_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_1Frame(MaskBmp, Data,
          (CurrentFrame div 2) + CornerPositions[2] + Center.y, Step div 2,
          Draw, CalcDirtyRects);
    end
    else
    begin
      Style4_1Frame(MaskBmp, Data,
        (CurrentFrame div 2) + CornerPositions[2] + Center.y, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style5_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_1Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame + 1) + Step,
      Step, Draw, CalcDirtyRects);
    Style4_1Frame(MaskBmp, Data, ((Data.Frames + 1) * 2 - CurrentFrame) + Step,
      Step, Draw, CalcDirtyRects);
  end
  else Style5_1Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
         Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style5_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style4_1Frame(MaskBmp, Data, ((Data.Frames + 1) * 2 - CurrentFrame) + Step,
      Step, Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style4_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_2Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw,
          CalcDirtyRects);
    end
    else
    begin
      Style4_2Frame(MaskBmp, Data, CurrentFrame div 2,
        Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style5_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style5_3Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style5_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_4Frame(MaskBmp, Data, ((Data.Frames + 1) * 2 - CurrentFrame) + Step,
      Step, Draw, CalcDirtyRects);
    Style4_3Frame(MaskBmp, Data, CurrentFrame - 1, Step, Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style4_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_3Frame(MaskBmp, Data,
          (CurrentFrame div 2) + CornerPositions[2] + Center.y, Step div 2,
          Draw, CalcDirtyRects);
    end
    else
    begin
      Style4_3Frame(MaskBmp, Data,
        (CurrentFrame div 2) + CornerPositions[2] + Center.y, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style5_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style5_5Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style5_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_3Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
      Step, Draw, CalcDirtyRects);
    Style4_4Frame(MaskBmp, Data, ((Data.Frames + 1) * 2 - CurrentFrame) + Step,
      Step, Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style4_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_4Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw,
          CalcDirtyRects);
    end
    else
    begin
      Style4_4Frame(MaskBmp, Data, CurrentFrame div 2,
        Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style5_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style5_7Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style5_9Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_5Frame(MaskBmp, Data, CurrentFrame - 1, Step, Draw, CalcDirtyRects);
    Style4_6Frame(MaskBmp, Data, ((Data.Frames + 1) * 2) - CurrentFrame + 1,
      Step, Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style4_5Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_5Frame(MaskBmp, Data,
          (CurrentFrame div 2) + CornerPoints[3].y + 2 + Center.x, Step div 2,
          Draw, CalcDirtyRects);
    end
    else
    begin
      Style4_5Frame(MaskBmp, Data,
        (CurrentFrame div 2) + CornerPoints[3].y + 2 + Center.x,
        Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_5Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style5_10Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style4_6Frame(MaskBmp, Data, (Data.Frames + 1) + CurrentFrame, Step,
      Draw, CalcDirtyRects);
  end
  else
    Style5_9Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
      Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style5_11Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_6Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style4_6Frame(MaskBmp, Data, ((Data.Frames + 1) * 2 - CurrentFrame) + Step,
      Step, Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style4_5Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_6Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw,
          CalcDirtyRects);
    end
    else
    begin
      Style4_6Frame(MaskBmp, Data, CurrentFrame div 2,
        Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_5Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style5_12Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style5_11Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style5_13Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style4_7Frame(MaskBmp, Data, CurrentFrame + (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style4_7Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_7Frame(MaskBmp, Data,
          (CurrentFrame div 2) + CornerPoints[3].y + 2 + Center.x, Step div 2,
          Draw, CalcDirtyRects);
    end
    else
    begin
      Style4_7Frame(MaskBmp, Data,
        (CurrentFrame div 2) + CornerPoints[3].y + 2 + Center.x,
        Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_7Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style5_14Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style5_13Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step + 1,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style5_15Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style4_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style4_8Frame(MaskBmp, Data, CurrentFrame - (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style4_7Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_8Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw,
          CalcDirtyRects);
    end
    else
    begin
      Style4_8Frame(MaskBmp, Data, CurrentFrame div 2,
        Step - (Step div 2), Draw, CalcDirtyRects);
      if Step > 1 then
        Style4_7Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style5_16Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style5_15Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style6_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  DrawMask(Point(CornerPoints[1].x, CornerPoints[1].y), MaskBmp, Data,
    CurrentFrame + CornerPositions[2] - 1, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style6_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style6_1Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style6_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style6_4Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style6_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := CurrentFrame + CornerPositions[3] - 1;
  if Position > MaxFrames then
    Dec(Position, MaxFrames);
  DrawMask(Point(CornerPoints[2].x, CornerPoints[2].y), MaskBmp, Data, Position,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style6_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  DrawMask(Point(CornerPoints[4].x, CornerPoints[4].y), MaskBmp, Data,
    CurrentFrame, Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style6_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style6_5Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style6_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
var
  Position: Longint;
begin
  Position := CurrentFrame + CornerPositions[4] - 1;
  if Position > MaxFrames then
    Dec(Position, MaxFrames);
  DrawMask(Point(CornerPoints[3].x, CornerPoints[3].y), MaskBmp, Data, Position,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style6_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style6_7Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style7_1Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style6_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style6_1Frame(MaskBmp, Data, CurrentFrame + (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style6_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_1Frame(MaskBmp, Data,
          (CurrentFrame div 2) + ((Data.Frames + 1) div 2), Step div 2,
          Draw, CalcDirtyRects);
    end
    else
    begin
      Style6_1Frame(MaskBmp, Data,
        (CurrentFrame div 2) + ((Data.Frames + 1) div 2), Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2,
        Draw, CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style7_2Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style7_1Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style7_3Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style6_1Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style6_2Frame(MaskBmp, Data, CurrentFrame - (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style6_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_2Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw,
          CalcDirtyRects);
    end
    else
    begin
      Style6_2Frame(MaskBmp, Data, CurrentFrame div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_1Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style7_4Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style7_3Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style7_5Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style6_3Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style6_3Frame(MaskBmp, Data, CurrentFrame - (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style6_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_3Frame(MaskBmp, Data,
          (CurrentFrame div 2) + ((Data.Frames + 1) div 2), Step div 2,
          Draw, CalcDirtyRects);
    end
    else
    begin
      Style6_3Frame(MaskBmp, Data,
        (CurrentFrame div 2) + ((Data.Frames + 1) div 2), Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style7_6Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style7_5Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style7_7Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style6_4Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style6_3Frame(MaskBmp, Data, CurrentFrame - (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style6_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_4Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw,
          CalcDirtyRects);
    end
    else
    begin
      Style6_4Frame(MaskBmp, Data, CurrentFrame div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_3Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style7_8Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style7_7Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style7_9Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style6_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style6_5Frame(MaskBmp, Data, CurrentFrame + (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style6_5Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_5Frame(MaskBmp, Data,
          (CurrentFrame div 2) + ((Data.Frames + 1) div 2), Step div 2,
          Draw, CalcDirtyRects);
    end
    else
    begin
      Style6_5Frame(MaskBmp, Data,
        (CurrentFrame div 2) + ((Data.Frames + 1) div 2), Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_5Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style7_10Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style7_9Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style7_11Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style6_5Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style6_6Frame(MaskBmp, Data, CurrentFrame - (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style6_5Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_6Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw,
          CalcDirtyRects);
    end
    else
    begin
      Style6_6Frame(MaskBmp, Data, CurrentFrame div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_5Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style7_12Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style7_11Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style7_13Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style6_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style6_7Frame(MaskBmp, Data, CurrentFrame + (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style6_7Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_7Frame(MaskBmp, Data,
          (CurrentFrame div 2) + ((Data.Frames + 1) div 2), Step div 2,
          Draw, CalcDirtyRects);
    end
    else
    begin
      Style6_7Frame(MaskBmp, Data,
        (CurrentFrame div 2) + ((Data.Frames + 1) div 2), Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_7Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2,
          Draw, CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style7_14Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style7_13Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

procedure TRadialTransition.Style7_15Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  if IsSmooth
  then
  begin
    Style6_7Frame(MaskBmp, Data, CurrentFrame, Step, Draw, CalcDirtyRects);
    Style6_8Frame(MaskBmp, Data, CurrentFrame - (Data.Frames + 1), Step,
      Draw, CalcDirtyRects);
  end
  else
  begin
    if(CurrentFrame - Step + 1) mod 2 = 1
    then
    begin
      Style6_7Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_8Frame(MaskBmp, Data, CurrentFrame div 2, Step div 2, Draw,
          CalcDirtyRects);
    end
    else
    begin
      Style6_8Frame(MaskBmp, Data, CurrentFrame div 2, Step - (Step div 2),
        Draw, CalcDirtyRects);
      if Step > 1 then
        Style6_7Frame(MaskBmp, Data, (CurrentFrame + 1) div 2, Step div 2, Draw,
          CalcDirtyRects);
    end;
  end;
end;

procedure TRadialTransition.Style7_16Frame(MaskBmp: TBitmap;
  Data: TTETransitionData; CurrentFrame, Step: Longint;
  Draw, CalcDirtyRects: Boolean);
begin
  Style7_15Frame(MaskBmp, Data, ((Data.Frames + 1) - CurrentFrame) + Step,
    Step, Draw, CalcDirtyRects);
end;

initialization

  TERegisterTransition(TRadialTransition);

end.
