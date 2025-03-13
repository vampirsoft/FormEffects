unit teSlide;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teTimed, Windows, Messages, Graphics;

type
  TSlideTransition = class(TTimedTransitionEffect)
  private
    FElasticSrc,
    FElasticDst: Boolean;
    FSlideOut: Boolean;
    SaveStretchBltMode: Integer;
  protected
    Mode: Integer;
    ClipR,
    VisibleR: TRect;
    DstXDesp,
    DstYDesp: Integer;
    DoScroll: Boolean;

    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure Finalize(Data: TTETransitionData); override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;

    function  GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
    function  ElasticSrcToUse: Boolean;
    function  ElasticDstToUse: Boolean;
    function  GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    function  SlideOutToUse: Boolean;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;

    procedure Assign(Source: TPersistent); override;
    class function GetEditor: String; override;
  published
    property Direction default tedDown;
    property ElasticDst: Boolean read FElasticDst write FElasticDst default False;
    property ElasticSrc: Boolean read FElasticSrc write FElasticSrc default False;
    property Pass2Options;
    property PassSetting;
    property Reversed;
    property SlideOut: Boolean read FSlideOut write FSlideOut default False;
  end;

implementation

constructor TSlideTransition.Create(AOwner: TComponent);
begin
  inherited;

  AllowedDirections := [tedRight, tedLeft, tedDown, tedUp, tedDownRight,
    tedDownLeft, tedUpRight, tedUpLeft, tedIn, tedOut, tedRandom];
  Direction   := tedDown;
  FElasticSrc := False;
  FElasticDst := False;
  FSlideOut   := False;
end;

class function TSlideTransition.Description: String;
begin
  Result := 'Slide';
end;

procedure TSlideTransition.Assign(Source: TPersistent);
var
  Transition: TSlideTransition;
begin
  if Source is TSlideTransition
  then
  begin
    inherited;

    Transition := TSlideTransition(Source);
    ElasticSrc := Transition.ElasticSrc;
    ElasticDst := Transition.ElasticDst;
    SlideOut   := Transition.SlideOut;
  end
  else inherited;
end;

class function TSlideTransition.GetEditor: String;
begin
  Result := 'TSlideTransitionEditor';
end;

function TSlideTransition.ElasticDstToUse: Boolean;
begin
  if Reversed
  then Result := ElasticSrc
  else Result := ElasticDst;
end;

function TSlideTransition.ElasticSrcToUse: Boolean;
begin
  if Reversed
  then Result := ElasticDst
  else Result := ElasticSrc;
end;

function TSlideTransition.SlideOutToUse: Boolean;
begin
  Result := SlideOut;
  if Reversed then
    Result := not Result;
end;

function TSlideTransition.GetPixelFormat(
  Device: TTETransitionDevice): TPixelFormat;
begin
  // Timing doesn't work fine with ScrollDC and DDBs
  Result := Device.PixelFormat;
end;

procedure TSlideTransition.Initialize(Data: TTETransitionData; var TotalFrames:
  Longint);

  function GetMode: Integer;
  begin
    case DirectionToUse of
      tedRight    : if SlideOutToUse
                    then if ElasticDstToUse
                         then if ElasticSrcToUse
                              then Result := 16
                              else Result := 37
                         else if ElasticSrcToUse
                              then Result := 36
                              else Result := 35
                    else if ElasticDstToUse
                         then if ElasticSrcToUse
                              then Result := 16
                              else Result := 15
                         else if ElasticSrcToUse
                              then Result := 14
                              else Result := 13;
      tedLeft     : if SlideOutToUse
                    then if ElasticDstToUse
                         then if ElasticSrcToUse
                              then Result := 12
                              else Result := 34
                         else if ElasticSrcToUse
                              then Result := 33
                              else Result := 32
                    else if ElasticDstToUse
                         then if ElasticSrcToUse
                              then Result := 12
                              else Result := 11
                         else if ElasticSrcToUse
                              then Result := 10
                              else Result :=  9;
      tedDown     : if SlideOutToUse
                    then if ElasticDstToUse
                         then if ElasticSrcToUse
                              then Result :=  4
                              else Result := 28
                         else if ElasticSrcToUse
                              then Result := 27
                              else Result := 26
                    else if ElasticDstToUse
                         then if ElasticSrcToUse
                              then Result :=  4
                              else Result :=  3
                         else if ElasticSrcToUse
                              then Result :=  2
                              else Result :=  1;
      tedUp       : if SlideOutToUse
                    then if ElasticDstToUse
                         then if ElasticSrcToUse
                              then Result :=  8
                              else Result := 31
                         else if ElasticSrcToUse
                              then Result := 30
                              else Result := 29
                    else if ElasticDstToUse
                         then if ElasticSrcToUse
                              then Result :=  8
                              else Result :=  7
                         else if ElasticSrcToUse
                              then Result :=  6
                              else Result :=  5;
      tedDownRight: if SlideOutToUse
                    then if ElasticDstToUse
                         then Result := 20
                         else if ElasticSrcToUse
                              then Result := 41
                              else Result := 40
                    else if ElasticDstToUse
                         then Result := 20
                         else if ElasticSrcToUse
                              then Result := 41
                              else Result := 19;
      tedDownLeft : if SlideOutToUse
                    then if ElasticDstToUse
                         then Result := 18
                         else if ElasticSrcToUse
                              then Result := 39
                              else Result := 38
                    else if ElasticDstToUse
                         then Result := 18
                         else if ElasticSrcToUse
                              then Result := 39
                              else Result := 17;
      tedUpRight  : if SlideOutToUse
                    then if ElasticDstToUse
                         then Result := 24
                         else if ElasticSrcToUse
                              then Result := 45
                              else Result := 44
                    else if ElasticDstToUse
                         then Result := 24
                         else if ElasticSrcToUse
                              then Result := 45
                              else Result := 23;
      tedUpLeft   : if SlideOutToUse
                    then if ElasticDstToUse
                         then Result := 22
                         else if ElasticSrcToUse
                              then Result := 43
                              else Result := 42
                    else if ElasticDstToUse
                         then Result := 22
                         else if ElasticSrcToUse
                              then Result := 43
                              else Result := 21;
      tedIn       : Result := 46;
      tedOut      : Result := 25;
      else          Result :=  0;
    end;
  end;

begin
  inherited;

  Mode  := GetMode;
  ClipR := Rect(0, 0, Data.Width, Data.Height);

  SaveStretchBltMode := SetStretchBltMode(Data.Canvas.Handle, COLORONCOLOR);

  case Mode of
     1,
     2,
     3,
     4: begin
          TotalFrames   := Data.Height-1;
          DstYDesp := Data.Height;
          VisibleR := Rect(0, 0, Data.Width, 0);
        end;
     5,
     6,
     7,
     8: begin
          TotalFrames   := Data.Height-1;
          DstYDesp := 0;
          VisibleR := Rect(0, Data.Height, Data.Width, Data.Height);
        end;
     9,
    10,
    11,
    12: begin
          TotalFrames   := Data.Width-1;
          DstXDesp := -1;
          VisibleR := Rect(Data.Width, 0, Data.Width, Data.Height);
        end;
    13,
    14,
    15,
    16: begin
          TotalFrames   := Data.Width-1;
          DstXDesp := Data.Width;
          VisibleR := Rect(0, 0, 0, Data.Height);
        end;
    17,
    18: begin
          if Data.Width > Data.Height
          then TotalFrames := Data.Width -1
          else TotalFrames := Data.Height-1;
          DstYDesp    := Data.Height;
          VisibleR    := Rect(Data.Width, 0, Data.Width, 0);
        end;
    19,
    20: begin
          if Data.Width > Data.Height
          then TotalFrames := Data.Width -1
          else TotalFrames := Data.Height-1;
          DstXDesp    := Data.Width;
          DstYDesp    := Data.Height;
          VisibleR    := Rect(0, 0, 0, 0);
        end;
    21,
    22: begin
          if Data.Width > Data.Height
          then TotalFrames := Data.Width -1
          else TotalFrames := Data.Height-1;
          DstXDesp    := 0;
          DstYDesp    := 0;
          VisibleR := Rect(Data.Width, Data.Height, Data.Width, Data.Height);
        end;
    23,
    24: begin
          if Data.Width > Data.Height
          then TotalFrames := Data.Width -1
          else TotalFrames := Data.Height-1;
          DstXDesp    := Data.Width;
          DstYDesp    := 0;
          VisibleR := Rect(0, Data.Height, 0, Data.Height);
        end;
    25: begin
          if Data.Width > Data.Height
          then TotalFrames := (Data.Width  div 2) - 1
          else TotalFrames := (Data.Height div 2) - 1;
          VisibleR := Rect(Data.Width div 2, Data.Height div 2, Data.
            Width div 2, Data.Height div 2);
        end;
    26,
    27,
    28: begin
          TotalFrames   := Data.Height-1;
          VisibleR := Rect(0, 0, Data.Width, 0);
        end;
    29,
    30,
    31: begin
          TotalFrames   := Data.Height-1;
          DstYDesp := 0;
          VisibleR := Rect(0, Data.Height, Data.Width, Data.Height);
        end;
    32,
    33,
    34: begin
          TotalFrames   := Data.Width-1;
          DstXDesp := 0;
          VisibleR := Rect(Data.Width, 0, Data.Width, Data.Height);
        end;
    35,
    36,
    37: begin
          TotalFrames   := Data.Width-1;
          VisibleR := Rect(0, 0, 0, Data.Height);
        end;
    38,
    39: begin
          if Data.Width > Data.Height
          then TotalFrames := Data.Width -1
          else TotalFrames := Data.Height-1;
          DstXDesp    := 0;
          VisibleR    := Rect(0, 0, Data.Width, Data.Height);
        end;
    40,
    41: begin
          if Data.Width > Data.Height
          then TotalFrames := Data.Width -1
          else TotalFrames := Data.Height-1;
          VisibleR    := Rect(0, 0, Data.Width, Data.Height);
        end;
    42,
    43: begin
          if Data.Width > Data.Height
          then TotalFrames := Data.Width -1
          else TotalFrames := Data.Height-1;
          DstXDesp    := 0;
          DstYDesp    := 0;
          VisibleR    := Rect(0, 0, Data.Width, Data.Height);
        end;
    44,
    45: begin
          if Data.Width > Data.Height
          then TotalFrames := Data.Width -1
          else TotalFrames := Data.Height-1;
          DstYDesp    := 0;
          VisibleR    := Rect(0, 0, Data.Width, Data.Height);
        end;
    46: begin
          if Data.Width > Data.Height
          then TotalFrames := (Data.Width  div 2) - 1
          else TotalFrames := (Data.Height div 2) - 1;
          VisibleR := Rect(0, 0, Data.Width, Data.Height);
        end;
  end;

  DoScroll := Assigned(Data.Bitmap);
end;

procedure TSlideTransition.Finalize(Data: TTETransitionData);
begin
  SetStretchBltMode(Data.Canvas.Handle, SaveStretchBltMode);

  inherited;
end;

procedure TSlideTransition.ExecuteFrame(Data: TTETransitionData; CurrentFrame,
  Step, LastExecutedFrame: Longint);
var
  VisibleOldR,
  ScrollR: TRect;
  xDesp,
  yDesp: Integer;
begin
  VisibleOldR := VisibleR;

  case Mode of
     1,
     2: begin;
          Inc(VisibleR.Bottom, Step);
          Dec(DstYDesp, Step);
          if DoScroll
          then
          begin
            ScrollDC(Data.Canvas.Handle, 0, Step, VisibleOldR, ClipR, 0, nil);
            BitBlt(Data.Canvas.Handle, 0, 0, Data.Width, Step,
              Data.DstBmp.Canvas.Handle, 0, DstYDesp, cmSrcCopy);
          end
          else
            BitBlt(Data.Canvas.Handle, 0, 0, Data.Width, VisibleR.Bottom,
              Data.DstBmp.Canvas.Handle, 0, DstYDesp, cmSrcCopy);
          Data.UpdateRect := Rect(0, 0, Data.Width, VisibleR.Bottom);
          if Mode = 2 then
          begin
            StretchBlt(Data.Canvas.Handle, 0, VisibleR.Bottom,
              Data.Width, Data.Height - VisibleR.Bottom,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, VisibleR.Bottom, Data.Width, Data.Height));
          end;
        end;
     3,
     4: begin;
          Inc(VisibleR.Bottom, Step);
          StretchBlt(Data.Canvas.Handle, 0, 0, Data.Width, VisibleR.Bottom,
            Data.DstBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
            cmSrcCopy);
          Data.UpdateRect := Rect(0, 0, Data.Width, VisibleR.Bottom);
          if Mode = 4 then
          begin
            StretchBlt(Data.Canvas.Handle, 0, VisibleR.Bottom, Data.Width,
              Data.Height - VisibleR.Bottom, Data.SrcBmp.Canvas.Handle, 0, 0,
              Data.Width, Data.Height, cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, VisibleR.Bottom, Data.Width, Data.Height));
          end;
        end;
     5,
     6: begin;
          Dec(VisibleR.Top, Step);
          if DoScroll
          then
          begin
            Inc(DstYDesp, Step);
            ScrollDC(Data.Canvas.Handle, 0, -Step, VisibleOldR, ClipR, 0, nil);
            BitBlt(Data.Canvas.Handle, 0, Data.Height - Step, Data.Width, Step,
              Data.DstBmp.Canvas.Handle, 0, DstYDesp, cmSrcCopy);
          end
          else
            BitBlt(Data.Canvas.Handle, 0, VisibleR.Top, Data.Width,
              VisibleR.Bottom - VisibleR.Top, Data.DstBmp.Canvas.Handle, 0,
              0, cmSrcCopy);
          Data.UpdateRect := Rect(0, VisibleR.Top, Data.Width, VisibleR.Bottom);
          if Mode = 6 then
          begin
            StretchBlt(Data.Canvas.Handle, 0, 0, Data.Width, VisibleR.Top,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, 0, Data.Width, VisibleR.Top));
          end;
        end;
     7,
     8: begin;
          Dec(VisibleR.Top, Step);
          StretchBlt(Data.Canvas.Handle, 0, VisibleR.Top, Data.Width,
            Data.Height - VisibleR.Top, Data.DstBmp.Canvas.Handle, 0, 0,
            Data.Width, Data.Height, cmSrcCopy);
          Data.UpdateRect := Rect(0, VisibleR.Top, Data.Width, Data.Height);
          if Mode = 8 then
          begin
            StretchBlt(Data.Canvas.Handle, 0, 0, Data.Width, VisibleR.Top,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, 0, Data.Width, VisibleR.Top));
          end;
        end;
     9,
    10: begin
          Dec(VisibleR.Left, Step);
          if DoScroll
          then
          begin
            Inc(DstXDesp, Step);
            ScrollDC(Data.Canvas.Handle, -Step, 0, VisibleOldR, ClipR, 0, nil);
            BitBlt(Data.Canvas.Handle, Data.Width - Step, 0, Step, Data.Height,
              Data.DstBmp.Canvas.Handle, DstXDesp, 0, cmSrcCopy);
          end
          else
            BitBlt(Data.Canvas.Handle, VisibleR.Left, 0,
              VisibleR.Right - VisibleR.Left, Data.Height,
              Data.DstBmp.Canvas.Handle, 0, 0, cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Left, 0, VisibleR.Right, Data.Height);
          if Mode = 10 then
          begin
            StretchBlt(Data.Canvas.Handle, 0, 0, VisibleR.Left, Data.Height,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, 0, VisibleR.Left, Data.Height));
          end;
        end;
    11,
    12: begin;
          Dec(VisibleR.Left, Step);
          StretchBlt(Data.Canvas.Handle, VisibleR.Left, 0,
            Data.Width - VisibleR.Left, Data.Height, Data.DstBmp.Canvas.Handle,
            0, 0, Data.Width, Data.Height, cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Left, 0, Data.Width, Data.Height);
          if Mode = 12 then
          begin
            StretchBlt(Data.Canvas.Handle, 0, 0, VisibleR.Left, Data.Height,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, 0, VisibleR.Left, Data.Height));
          end;
        end;
    13,
    14: begin;
          Inc(VisibleR.Right, Step);
          Dec(DstXDesp, Step);
          if DoScroll
          then
          begin
            ScrollDC(Data.Canvas.Handle, Step, 0, VisibleOldR, ClipR, 0, nil);
            BitBlt(Data.Canvas.Handle, 0, 0, Step, Data.Height,
              Data.DstBmp.Canvas.Handle, DstXDesp, 0, cmSrcCopy);
          end
          else
            BitBlt(Data.Canvas.Handle, 0, 0, VisibleR.Right, Data.Height,
              Data.DstBmp.Canvas.Handle, DstXDesp, 0, cmSrcCopy);
          Data.UpdateRect := Rect(0, 0, VisibleR.Right, Data.Height);
          if Mode = 14 then
          begin
            StretchBlt(Data.Canvas.Handle, VisibleR.Right, 0,
              Data.Width - VisibleR.Right, Data.Height,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(VisibleR.Right, 0, Data.Width, Data.Height));
          end;
        end;
    15,
    16: begin;
          Inc(VisibleR.Right, Step);
          StretchBlt(Data.Canvas.Handle, 0, 0, VisibleR.Right, Data.Height,
            Data.DstBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height, cmSrcCopy);
          Data.UpdateRect := Rect(0, 0, VisibleR.Right, Data.Height);
          if Mode = 16 then
          begin
            StretchBlt(Data.Canvas.Handle, VisibleR.Right, 0,
              Data.Width - VisibleR.Right, Data.Height,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(VisibleR.Right, 0, Data.Width, Data.Height));
          end;
        end;
    17: begin;
          VisibleR.Left   :=
            Data.Width - Round((Data.Width / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Bottom := Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          yDesp := VisibleR.Bottom - VisibleOldR.Bottom;
          Dec(DstYDesp, yDesp);
          if DoScroll
          then
          begin
            xDesp := VisibleR.Left - VisibleOldR.Left;
            ScrollDC(Data.Canvas.Handle, xDesp, yDesp, VisibleOldR, ClipR, 0,
              nil);
            BitBlt(Data.Canvas.Handle, VisibleR.Left, 0,
              VisibleR.Right - VisibleR.Left, yDesp,
              Data.DstBmp.Canvas.Handle, 0, Data.Height - VisibleR.Bottom,
              cmSrcCopy);
            BitBlt(Data.Canvas.Handle,
              Data.Width + xDesp, yDesp,
              -xDesp, VisibleOldR.Bottom,
              Data.DstBmp.Canvas.Handle,
              VisibleOldR.Right - VisibleOldR.Left, DstYDesp + yDesp,
              cmSrcCopy);
          end
          else
            BitBlt(Data.Canvas.Handle, VisibleR.Left, 0,
              VisibleR.Right - VisibleR.Left, VisibleR.Bottom,
              Data.DstBmp.Canvas.Handle, 0, DstYDesp, cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Left, 0, VisibleR.Right, VisibleR.Bottom);
        end;
    18: begin;
          VisibleR.Left   :=
            Data.Width - Round((Data.Width / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Bottom := Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          StretchBlt(Data.Canvas.Handle, VisibleR.Left, 0,
            Data.Width - VisibleR.Left, VisibleR.Bottom,
            Data.DstBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
            cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Left, 0, Data.Width, VisibleR.Bottom);
        end;
    19: begin;
          VisibleR.Right  := Round((Data.Width  / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Bottom := Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          xDesp := VisibleR.Right  - VisibleOldR.Right;
          yDesp := VisibleR.Bottom - VisibleOldR.Bottom;
          Dec(DstXDesp, xDesp);
          Dec(DstYDesp, yDesp);
          if DoScroll
          then
          begin
            ScrollDC(Data.Canvas.Handle, xDesp, yDesp, VisibleOldR, ClipR, 0,
              nil);
            BitBlt(Data.Canvas.Handle, 0, 0, VisibleR.Right - VisibleR.Left,
              yDesp, Data.DstBmp.Canvas.Handle, DstXDesp, DstYDesp, cmSrcCopy);
            BitBlt(Data.Canvas.Handle, 0, yDesp, xDesp, VisibleOldR.Bottom,
              Data.DstBmp.Canvas.Handle, DstXDesp, DstYDesp + yDesp,
              cmSrcCopy);
          end
          else
            BitBlt(Data.Canvas.Handle, 0, 0, VisibleR.Right, VisibleR.Bottom,
              Data.DstBmp.Canvas.Handle, DstXDesp, DstYDesp, cmSrcCopy);
          Data.UpdateRect := Rect(0, 0, VisibleR.Right, VisibleR.Bottom);
        end;
    20: begin;
          VisibleR.Right  := Round((Data.Width  / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Bottom := Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          StretchBlt(Data.Canvas.Handle, 0, 0, VisibleR.Right, VisibleR.Bottom,
            Data.DstBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
            cmSrcCopy);
          Data.UpdateRect := Rect(0, 0, VisibleR.Right, VisibleR.Bottom);
        end;
    21: begin;
          VisibleR.Left :=
            Data.Width  - Round((Data.Width  / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Top  :=
            Data.Height - Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          if DoScroll
          then
          begin
            xDesp := VisibleR.Left - VisibleOldR.Left;
            yDesp := VisibleR.Top  - VisibleOldR.Top;
            Dec(DstXDesp, xDesp);
            Dec(DstYDesp, yDesp);
            ScrollDC(Data.Canvas.Handle, xDesp, yDesp, VisibleOldR, ClipR, 0,
              nil);
            BitBlt(Data.Canvas.Handle, VisibleR.Left,
              Data.Height + yDesp, VisibleR.Right - VisibleR.Left, yDesp,
              Data.DstBmp.Canvas.Handle, 0, DstYDesp, cmSrcCopy);
            BitBlt(Data.Canvas.Handle, Data.Width + xDesp, VisibleR.Top, -xDesp,
              VisibleOldR.Bottom - VisibleOldR.Top, Data.DstBmp.Canvas.Handle,
              DstXDesp, 0, cmSrcCopy);
          end
          else
            BitBlt(Data.Canvas.Handle, VisibleR.Left, VisibleR.Top,
              VisibleR.Right - VisibleR.Left, VisibleR.Bottom - VisibleR.Top,
              Data.DstBmp.Canvas.Handle, 0, 0, cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Left, VisibleR.Top, VisibleR.Right,
            VisibleR.Bottom);
        end;
    22: begin;
          VisibleR.Left :=
            Data.Width  - Round((Data.Width  / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Top  :=
            Data.Height - Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          StretchBlt(Data.Canvas.Handle, VisibleR.Left, VisibleR.Top,
            Data.Width - VisibleR.Left, Data.Height - VisibleR.Top,
            Data.DstBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
            cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Left, VisibleR.Top, Data.Width,
            Data.Height);
        end;
    23: begin;
          VisibleR.Right := Round((Data.Width / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Top   :=
            Data.Height - Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          xDesp := VisibleR.Right - VisibleOldR.Right;
          Dec(DstXDesp, xDesp);
          if DoScroll
          then
          begin
            yDesp := VisibleR.Top - VisibleOldR.Top;
            Dec(DstYDesp, yDesp);
            ScrollDC(Data.Canvas.Handle, xDesp, yDesp, VisibleOldR, ClipR, 0,
              nil);
            BitBlt(Data.Canvas.Handle, 0, Data.Height + yDesp, VisibleR.Right,
              -yDesp, Data.DstBmp.Canvas.Handle, DstXDesp, DstYDesp, cmSrcCopy);
            BitBlt(Data.Canvas.Handle, 0, VisibleR.Top, xDesp,
              VisibleOldR.Bottom - VisibleOldR.Top, Data.DstBmp.Canvas.Handle,
              DstXDesp, 0, cmSrcCopy);
          end
          else
            BitBlt(Data.Canvas.Handle, 0, VisibleR.Top, VisibleR.Right,
              VisibleR.Bottom - VisibleR.Top, Data.DstBmp.Canvas.Handle,
              DstXDesp, 0, cmSrcCopy);
          Data.UpdateRect := Rect(0, VisibleR.Top, VisibleR.Right,
            VisibleR.Bottom);
        end;
    24: begin;
          VisibleR.Right := Round((Data.Width / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Top   :=
            Data.Height - Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          StretchBlt(Data.Canvas.Handle, 0, VisibleR.Top, VisibleR.Right,
            Data.Height - VisibleR.Top, Data.DstBmp.Canvas.Handle, 0, 0,
            Data.Width, Data.Height, cmSrcCopy);
          Data.UpdateRect := Rect(0, VisibleR.Top, VisibleR.Right, Data.Height);
        end;
    25: begin
          with VisibleR do
          begin
            Left   := (Data.Width -
              Round((Data.Width  / (Data.Frames + 1)) * CurrentFrame)) div 2;
            Top    := (Data.Height -
              Round((Data.Height / (Data.Frames + 1)) * CurrentFrame)) div 2;
            Right  := Data.Width  - Left;
            Bottom := Data.Height - Top;
          end;
          StretchBlt(Data.Canvas.Handle, VisibleR.Left, VisibleR.Top,
            VisibleR.Right - VisibleR.Left, VisibleR.Bottom - VisibleR.Top,
            Data.DstBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height, cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Left, VisibleR.Top, VisibleR.Right,
            VisibleR.Bottom);
        end;
    26,
    27,
    28: begin;
          Inc(VisibleR.Bottom, Step);
          if Mode <> 27
          then
          begin
            if DoScroll
            then
            begin
              ScrollR := Rect(0, VisibleOldR.Bottom, Data.Width, Data.Height);
              ScrollDC(Data.Canvas.Handle, 0, Step, ScrollR, ClipR, 0, nil);
            end
            else
              BitBlt(Data.Canvas.Handle, 0, VisibleR.Bottom, Data.Width,
                Data.Height - VisibleR.Bottom, Data.SrcBmp.Canvas.Handle, 0, 0,
                cmSrcCopy);
          end
          else
            StretchBlt(Data.Canvas.Handle, 0, VisibleR.Bottom, Data.Width,
              Data.Height - VisibleR.Bottom, Data.SrcBmp.Canvas.Handle, 0, 0,
              Data.Width, Data.Height, cmSrcCopy);
          Data.UpdateRect := Rect(0, VisibleR.Bottom, Data.Width, Data.Height);
          if Mode = 28
          then
          begin
            StretchBlt(Data.Canvas.Handle, 0, 0, Data.Width, VisibleR.Bottom,
              Data.DstBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, 0, Data.Width, VisibleR.Bottom));
          end
          else
          begin
            BitBlt(Data.Canvas.Handle, 0, VisibleOldR.Bottom, Data.Width,
              Step, Data.DstBmp.Canvas.Handle, 0, VisibleOldR.Bottom, cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, VisibleOldR.Bottom, Data.Width, VisibleOldR.Bottom + Step));
          end;
        end;
    29,
    30,
    31: begin;
          Dec(VisibleR.Top, Step);
          if Mode <> 30
          then
          begin
            if DoScroll
            then
            begin
              ScrollR := Rect(0, 0, Data.Width, VisibleOldR.Top);
              ScrollDC(Data.Canvas.Handle, 0, -Step, ScrollR, ClipR, 0, nil);
            end
            else
            begin
              Inc(DstYDesp, Step);
              BitBlt(Data.Canvas.Handle, 0, 0, Data.Width, VisibleR.Top,
                Data.SrcBmp.Canvas.Handle, 0, DstYDesp, cmSrcCopy);
            end;
          end
          else
            StretchBlt(Data.Canvas.Handle, 0, 0, Data.Width, VisibleR.Top,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
          Data.UpdateRect := Rect(0, 0, Data.Width, VisibleR.Top);
          if Mode = 31
          then
          begin
            StretchBlt(Data.Canvas.Handle, 0, VisibleR.Top, Data.Width,
              Data.Height - VisibleR.Top, Data.DstBmp.Canvas.Handle, 0, 0,
              Data.Width, Data.Height, cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, VisibleR.Top, Data.Width, Data.Height));
          end
          else
            BitBlt(Data.Canvas.Handle, 0, VisibleR.Top, Data.Width, Step,
              Data.DstBmp.Canvas.Handle, 0, VisibleR.Top, cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, VisibleR.Top, Data.Width, VisibleR.Top + Step));
        end;
    32,
    33,
    34: begin;
          Dec(VisibleR.Left, Step);
          if Mode <> 33
          then
          begin
            if DoScroll
            then
            begin
              ScrollR := Rect(0, 0, VisibleOldR.Left, Data.Height);
              ScrollDC(Data.Canvas.Handle, -Step, 0, ScrollR, ClipR, 0, nil);
            end
            else
            begin
              Inc(DstXDesp, Step);
              BitBlt(Data.Canvas.Handle, 0, 0, VisibleR.Left, Data.Height,
                Data.SrcBmp.Canvas.Handle, DstXDesp, 0, cmSrcCopy);
            end;
          end
          else
            StretchBlt(Data.Canvas.Handle, 0, 0, VisibleR.Left, Data.Height,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
          Data.UpdateRect := Rect(0, 0, VisibleR.Left, Data.Height);
          if Mode = 34
          then
          begin
            StretchBlt(Data.Canvas.Handle, VisibleR.Left, 0,
              Data.Width - VisibleR.Left, Data.Height,
              Data.DstBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(VisibleR.Left, 0, Data.Width, Data.Height));
          end
          else
          begin
            BitBlt(Data.Canvas.Handle, VisibleR.Left, 0, Step, Data.Height,
              Data.DstBmp.Canvas.Handle, VisibleR.Left, 0, cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(VisibleR.Left, 0, VisibleR.Left + Step, Data.Height));
          end;
        end;
    35,
    36,
    37: begin;
          Inc(VisibleR.Right, Step);
          if Mode <> 36
          then
          begin
            if DoScroll
            then
            begin
              ScrollR := Rect(VisibleOldR.Right, 0, Data.Width, Data.Height);
              ScrollDC(Data.Canvas.Handle, Step, 0, ScrollR, ClipR, 0, nil);
            end
            else
              BitBlt(Data.Canvas.Handle, VisibleR.Right, 0,
                Data.Width - VisibleR.Right, Data.Height,
                Data.SrcBmp.Canvas.Handle, 0, 0, cmSrcCopy);
          end
          else
            StretchBlt(Data.Canvas.Handle, VisibleR.Right, 0,
              Data.Width - VisibleR.Right, Data.Height,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Right, 0, Data.Width, Data.Height);
          if Mode = 37
          then
          begin
            StretchBlt(Data.Canvas.Handle, 0, 0, VisibleR.Right, Data.Height,
              Data.DstBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(0, 0, VisibleR.Right, Data.Height));
          end
          else
          begin
            BitBlt(Data.Canvas.Handle, VisibleOldR.Right, 0, Step, Data.Height,
              Data.DstBmp.Canvas.Handle, VisibleOldR.Right, 0, cmSrcCopy);
            UnionRect(Data.UpdateRect, Data.UpdateRect,
              Rect(VisibleOldR.Right, 0, VisibleOldR.Right + Step, Data.Height));
          end;
        end;
    38,
    39: begin;
          VisibleR.Right :=
            Data.Width - Round((Data.Width / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Top   := Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          xDesp := VisibleR.Right - VisibleOldR.Right;
          yDesp := VisibleR.Top   - VisibleOldR.Top;
          if Mode = 38
          then
          begin
            if DoScroll
            then
              ScrollDC(Data.Canvas.Handle, xDesp, yDesp, VisibleOldR, ClipR, 0,
                nil)
            else
            begin
              Dec(DstXDesp, xDesp);
              BitBlt(Data.Canvas.Handle, 0, VisibleR.Top, VisibleR.Right,
                Data.Height - VisibleR.Top, Data.SrcBmp.Canvas.Handle,
                DstXDesp, 0, cmSrcCopy);
            end;
          end
          else
            StretchBlt(Data.Canvas.Handle, 0, VisibleR.Top, VisibleR.Right,
              Data.Height - VisibleR.Top, Data.SrcBmp.Canvas.Handle, 0, 0,
              Data.Width, Data.Height, cmSrcCopy);
          Data.UpdateRect := Rect(0, VisibleR.Top, VisibleR.Right, Data.Height);
          BitBlt(Data.Canvas.Handle, 0, VisibleOldR.Top, VisibleOldR.Right, yDesp,
            Data.DstBmp.Canvas.Handle, 0, VisibleOldR.Top, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(0, VisibleOldR.Top, VisibleOldR.Right, VisibleOldR.Top + yDesp));
          BitBlt(Data.Canvas.Handle, VisibleR.Right, VisibleR.Top, -xDesp,
            VisibleR.Bottom - VisibleR.Top, Data.DstBmp.Canvas.Handle,
            VisibleR.Right, VisibleR.Top, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(VisibleR.Right, VisibleR.Top, VisibleR.Right - xDesp, VisibleR.Bottom));
        end;
    40,
    41: begin;
          VisibleR.Left := Round((Data.Width  / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Top  := Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          xDesp := VisibleR.Left - VisibleOldR.Left;
          yDesp := VisibleR.Top  - VisibleOldR.Top;
          if Mode = 40
          then
          begin
            if DoScroll
            then
              ScrollDC(Data.Canvas.Handle, xDesp, yDesp, VisibleOldR, ClipR, 0,
                nil)
            else
              BitBlt(Data.Canvas.Handle, VisibleR.Left, VisibleR.Top,
                Data.Width - VisibleR.Left, Data.Height - VisibleR.Top,
                Data.SrcBmp.Canvas.Handle, 0, 0, cmSrcCopy);
          end
          else
            StretchBlt(Data.Canvas.Handle, VisibleR.Left, VisibleR.Top,
              Data.Width - VisibleR.Left, Data.Height - VisibleR.Top,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width,
              Data.Height, cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Left, VisibleR.Top, Data.Width,
            Data.Height);
          BitBlt(Data.Canvas.Handle, VisibleOldR.Left, VisibleOldR.Top,
            Data.Width - VisibleOldR.Left, yDesp, Data.DstBmp.Canvas.Handle,
            VisibleOldR.Left, VisibleOldR.Top, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(VisibleOldR.Left, VisibleOldR.Top, Data.Width, VisibleOldR.Top + yDesp));
          BitBlt(Data.Canvas.Handle, VisibleOldR.Left, VisibleOldR.Top, xDesp,
            Data.Height - VisibleOldR.Top, Data.DstBmp.Canvas.Handle,
            VisibleOldR.Left, VisibleOldR.Top, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(VisibleOldR.Left, VisibleOldR.Top, VisibleOldR.Left + xDesp, Data.Height));
        end;
    42,
    43: begin;
          VisibleR.Right  :=
            Data.Width  - Round((Data.Width / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Bottom :=
            Data.Height - Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          xDesp := VisibleR.Right  - VisibleOldR.Right;
          yDesp := VisibleR.Bottom - VisibleOldR.Bottom;
          if Mode = 42
          then
          begin
            if DoScroll
            then
              ScrollDC(Data.Canvas.Handle, xDesp, yDesp, VisibleOldR, ClipR, 0,
                nil)
            else
            begin
              Dec(DstXDesp, xDesp);
              Dec(DstYDesp, yDesp);
              BitBlt(Data.Canvas.Handle, 0, 0, VisibleR.Right, VisibleR.Bottom,
                Data.SrcBmp.Canvas.Handle, DstXDesp, DstYDesp, cmSrcCopy);
            end;
          end
          else
            StretchBlt(Data.Canvas.Handle, 0, 0, VisibleR.Right,
              VisibleR.Bottom, Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width,
              Data.Height, cmSrcCopy);
          Data.UpdateRect := Rect(0, 0, VisibleR.Right, VisibleR.Bottom);
          BitBlt(Data.Canvas.Handle, 0, VisibleR.Bottom, VisibleOldR.Right, -yDesp,
            Data.DstBmp.Canvas.Handle, 0, VisibleR.Bottom, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(0, VisibleR.Bottom, VisibleOldR.Right, VisibleR.Bottom - yDesp));
          BitBlt(Data.Canvas.Handle, VisibleR.Right, 0, -xDesp, VisibleR.Bottom,
            Data.DstBmp.Canvas.Handle, VisibleR.Right, 0, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(VisibleR.Right, 0, VisibleR.Right - xDesp, VisibleR.Bottom));
        end;
    44,
    45: begin;
          VisibleR.Left  := Round((Data.Width / (Data.Frames + 1)) * CurrentFrame);
          VisibleR.Bottom :=
            Data.Height - Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
          xDesp := VisibleR.Left   - VisibleOldR.Left;
          yDesp := VisibleR.Bottom - VisibleOldR.Bottom;
          Dec(DstYDesp, yDesp);
          if Mode = 44
          then
          begin
          if DoScroll
          then
            ScrollDC(Data.Canvas.Handle, xDesp, yDesp, VisibleOldR, ClipR, 0,
              nil)
          else
            BitBlt(Data.Canvas.Handle, VisibleR.Left, 0,
              Data.Width - VisibleR.Left, VisibleR.Bottom,
              Data.SrcBmp.Canvas.Handle, 0, DstYDesp, cmSrcCopy);
          end
          else
            StretchBlt(Data.Canvas.Handle, VisibleR.Left, 0,
              Data.Width - VisibleR.Left, VisibleR.Bottom,
              Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
              cmSrcCopy);
          Data.UpdateRect := Rect(VisibleR.Left, 0, Data.Width, VisibleR.Bottom);
          BitBlt(Data.Canvas.Handle, VisibleOldR.Left, VisibleR.Bottom,
            Data.Width - VisibleOldR.Left, -yDesp, Data.DstBmp.Canvas.Handle,
            VisibleOldR.Left, VisibleR.Bottom, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(VisibleOldR.Left, VisibleR.Bottom, Data.Width, VisibleR.Bottom - yDesp));
          BitBlt(Data.Canvas.Handle, VisibleOldR.Left, 0, xDesp,
            VisibleR.Bottom, Data.DstBmp.Canvas.Handle, VisibleOldR.Left, 0,
            cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(VisibleOldR.Left, 0, VisibleOldR.Left + xDesp, VisibleR.Bottom));
        end;
    46: begin
          with VisibleR do
          begin
            Left   := (Round((Data.Width  / (Data.Frames + 1)) * CurrentFrame)) div 2;
            Top    := (Round((Data.Height / (Data.Frames + 1)) * CurrentFrame)) div 2;
            Right  := Data.Width  - Left;
            Bottom := Data.Height - Top;
          end;
          BitBlt(Data.Canvas.Handle, VisibleOldR.Left, VisibleR.Bottom,
            VisibleOldR.Right - VisibleOldR.Left,
            VisibleOldR.Bottom - VisibleR.Bottom, Data.DstBmp.Canvas.Handle,
            VisibleOldR.Left, VisibleR.Bottom, cmSrcCopy);
          Data.UpdateRect := Rect(VisibleOldR.Left, VisibleR.Bottom,
            VisibleOldR.Right, VisibleOldR.Bottom);
          BitBlt(Data.Canvas.Handle, VisibleOldR.Left, VisibleOldR.Top,
            VisibleOldR.Right - VisibleOldR.Left,
            VisibleR.Top - VisibleOldR.Top, Data.DstBmp.Canvas.Handle,
            VisibleOldR.Left, VisibleOldR.Top, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(VisibleOldR.Left, VisibleOldR.Top, VisibleOldR.Right, VisibleR.Top));
          BitBlt(Data.Canvas.Handle, VisibleOldR.Left, VisibleR.Top,
            VisibleR.Left - VisibleOldR.Left, VisibleR.Bottom - VisibleR.Top,
            Data.DstBmp.Canvas.Handle, VisibleOldR.Left, VisibleR.Top,
            cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(VisibleOldR.Left, VisibleR.Top, VisibleR.Left, VisibleR.Bottom));
          BitBlt(Data.Canvas.Handle, VisibleR.Right, VisibleR.Top,
            VisibleOldR.Right - VisibleR.Right, VisibleR.Bottom - VisibleR.Top,
            Data.DstBmp.Canvas.Handle, VisibleR.Right, VisibleR.Top, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect,
            Rect(VisibleR.Right, VisibleR.Top, VisibleOldR.Right, VisibleR.Bottom));
          StretchBlt(Data.Canvas.Handle, VisibleR.Left, VisibleR.Top,
            VisibleR.Right - VisibleR.Left, VisibleR.Bottom - VisibleR.Top,
            Data.SrcBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height, cmSrcCopy);
          UnionRect(Data.UpdateRect, Data.UpdateRect, VisibleR);
        end;
  end;
end;

function TSlideTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiOffScreenBmpCapable,
      tetiThreadSafe
    ];
end;

procedure TSlideTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'ElasticDst'];
  if Value <> '' then
    ElasticDst := SameText(Value, BoolToStr(True));

  Value := List.Values[Prefix + 'ElasticSrc'];
  if Value <> '' then
    ElasticSrc := SameText(Value, BoolToStr(True));

  Value := List.Values[Prefix + 'SlideOut'];
  if Value <> '' then
    SlideOut := SameText(Value, BoolToStr(True));
end;

procedure TSlideTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
begin
  inherited;

  if(not OmitDefaultValues) or
    (ElasticDst <> False) then
    List.Values[Prefix + 'ElasticDst'] := BoolToStr(ElasticDst);

  if(not OmitDefaultValues) or
    (ElasticSrc <> False) then
    List.Values[Prefix + 'ElasticSrc'] := BoolToStr(ElasticSrc);

  if(not OmitDefaultValues) or
    (SlideOut <> False) then
    List.Values[Prefix + 'SlideOut'] := BoolToStr(SlideOut);
end;

initialization

  TERegisterTransition(TSlideTransition);

end.
