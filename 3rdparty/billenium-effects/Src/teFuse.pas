unit teFuse;

interface

{$INCLUDE teDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, TransEff, teTimed, Graphics;

type
  TFuseTransition = class(TTimedTransitionEffect)
  private
    FCountOfStyles: Integer;
    FStyle,
    RandomStyle: Word;

    procedure SetStyle(const Value: Word);
  protected
    BrushBmp: TBitmap;

    procedure BrushFrame(BrushBmp: TBitmap;
      CurrentFrame, Step, Frames: Longint);
    procedure Initialize(Data: TTETransitionData; var Frames: Longint);
      override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    procedure Finalize(Data: TTETransitionData); override;
    function  StyleToUse: Word;
    procedure Style2Frame (BrushBmp: TBitmap; CurrentFrame: Integer);
    procedure Style3Frame (BrushBmp: TBitmap;
      CurrentFrame, Frames: Integer);
    procedure Style4Frame (BrushBmp: TBitmap; CurrentFrame: Integer);
    procedure Style5Frame (BrushBmp: TBitmap;
      CurrentFrame, Frames: Integer);
    procedure Style6Frame (BrushBmp: TBitmap; CurrentFrame: Integer);
    procedure Style7Frame (BrushBmp: TBitmap;
      CurrentFrame, Frames: Integer);
    procedure Style8Frame (BrushBmp: TBitmap; CurrentFrame: Integer);
    procedure Style9Frame (BrushBmp: TBitmap;
      CurrentFrame, Frames: Integer);
    procedure Style10Frame(BrushBmp: TBitmap; CurrentFrame: Integer);
    procedure Style11Frame(BrushBmp: TBitmap;
      CurrentFrame, Frames: Integer);
    procedure Style12Frame(BrushBmp: TBitmap; CurrentFrame: Integer);
    procedure Style13Frame(BrushBmp: TBitmap;
      CurrentFrame, Frames: Integer);
    function  CalcTotalFrames: Longint;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;

    procedure Assign(Source: TPersistent); override;
    class function GetEditor: String; override;

    property CountOfStyles: Integer read FCountOfStyles;
  published
    property Pass2Options;
    property PassSetting;
    property Reversed;
    property Style: Word read FStyle write SetStyle default 1;
  end;

implementation

uses teBlndWk;

{ TFuseTransition }

constructor TFuseTransition.Create(AOwner: TComponent);
begin
  inherited;

  FCountOfStyles := 13;
  FStyle         :=  1;
  RandomStyle    :=  0;
  BrushBmp       := nil;
end;

class function TFuseTransition.Description: String;
begin
  Result := 'Fuse';
end;

procedure TFuseTransition.Assign(Source: TPersistent);
var
  Transition: TFuseTransition;
begin
  if Source is TFuseTransition
  then
  begin
    inherited;

    Transition := TFuseTransition(Source);
    Style      := Transition.Style;
  end
  else inherited;
end;

class function TFuseTransition.GetEditor: String;
begin
  Result := 'TFuseTransitionEditor';
end;

function TFuseTransition.StyleToUse: Word;
begin
  if FStyle = 0
  then
  begin
    if RandomStyle = 0 then
      RandomStyle := Random(CountOfStyles) + 1;
    Result := RandomStyle;
  end
  else Result := FStyle;

  if Reversed then
    case Result of
       2: Result :=  3;
       3: Result :=  2;
       4: Result :=  5;
       5: Result :=  4;
       6: Result :=  7;
       7: Result :=  6;
       8: Result :=  9;
       9: Result :=  8;
      10: Result := 11;
      11: Result := 10;
      12: Result := 13;
      13: Result := 12;
    end;
end;

procedure TFuseTransition.SetStyle(const Value: Word);
begin
  if(FStyle <> Value) and (Value <= CountOfStyles) then
  begin
    FStyle := Value;
  end;
end;

procedure TFuseTransition.Initialize(Data: TTETransitionData; var Frames:
  Longint);
begin
  inherited;

  Randomize;
  BrushBmp := TBitmap.Create;
  BrushBmp.Canvas.Lock;
  BrushBmp.Monochrome := True;
  BrushBmp.Width      := 8;
  BrushBmp.Height     := 8;
  Frames := CalcTotalFrames;
end;

procedure TFuseTransition.Finalize(Data: TTETransitionData);
begin
  BrushBmp.Canvas.Unlock;
  FreeAndNil(BrushBmp);
  if(Data.PassCount = 1) or (Data.Pass = 2) then
    RandomStyle := 0;

  inherited;
end;

procedure TFuseTransition.ExecuteFrame(Data: TTETransitionData; CurrentFrame,
  Step, LastExecutedFrame: Longint);
begin
  Data.UpdateRect := Rect(0, 0, Data.Width, Data.Height);
  BrushFrame(BrushBmp, CurrentFrame, Step, Data.Frames);
  Data.Bitmap.Canvas.Brush.Bitmap := BrushBmp;
  BitBlt(Data.Bitmap.Canvas.Handle, 0, 0, Data.Width, Data.Height,
    Data.DstBmp.Canvas.Handle, 0, 0, $00AC0744);
  Data.Bitmap.Canvas.Brush.Bitmap := nil;
end;

function TFuseTransition.CalcTotalFrames: Longint;
begin
  Result := 0;

  case StyleToUse of
    2, 3, 4, 5, 6, 7, 10, 11, 12, 13:
      Result :=  7;
    1, 8, 9:
      Result := 63;
  end;
end;

procedure TFuseTransition.BrushFrame(BrushBmp: TBitmap;
  CurrentFrame, Step, Frames: Longint);
var
  i: Integer;
begin
  for i := CurrentFrame-Step+1 to CurrentFrame do
  begin
    case StyleToUse of
       1: StandardFuseFrame(BrushBmp, i);
       2: Style2Frame      (BrushBmp, i);
       3: Style3Frame      (BrushBmp, i, Frames);
       4: Style4Frame      (BrushBmp, i);
       5: Style5Frame      (BrushBmp, i, Frames);
       6: Style6Frame      (BrushBmp, i);
       7: Style7Frame      (BrushBmp, i, Frames);
       8: Style8Frame      (BrushBmp, i);
       9: Style9Frame      (BrushBmp, i, Frames);
      10: Style10Frame     (BrushBmp, i);
      11: Style11Frame     (BrushBmp, i, Frames);
      12: Style12Frame     (BrushBmp, i);
      13: Style13Frame     (BrushBmp, i, Frames);
    end;
  end;
end;

function TFuseTransition.GetInfo(Device: TTETransitionDevice):
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
  if Assigned(Device) and (not Device.UsingThread) then
    Include(Result, tetiUseSrcAsOffScreenBmp);
end;

procedure TFuseTransition.Style2Frame(BrushBmp: TBitmap; CurrentFrame: Integer);
begin
  BrushBmp.Canvas.MoveTo(0, CurrentFrame-1);
  BrushBmp.Canvas.LineTo(8, CurrentFrame-1);
end;

procedure TFuseTransition.Style3Frame(BrushBmp: TBitmap;
  CurrentFrame, Frames: Integer);
begin
  Style2Frame(BrushBmp, Frames - CurrentFrame + 2);
end;

procedure TFuseTransition.Style4Frame(BrushBmp: TBitmap; CurrentFrame: Integer);
begin
  BrushBmp.Canvas.MoveTo(CurrentFrame-1, 0);
  BrushBmp.Canvas.LineTo(CurrentFrame-1, 8);
end;

procedure TFuseTransition.Style5Frame(BrushBmp: TBitmap;
  CurrentFrame, Frames: Integer);
begin
  Style4Frame(BrushBmp, Frames - CurrentFrame + 2);
end;

procedure TFuseTransition.Style6Frame(BrushBmp: TBitmap; CurrentFrame: Integer);
begin
  if CurrentFrame = 1
  then BrushBmp.Canvas.Pixels[0, 0] := clBlack
  else
  begin
    BrushBmp.Canvas.MoveTo(CurrentFrame-1,              0);
    BrushBmp.Canvas.LineTo(CurrentFrame-1, CurrentFrame-1);
    BrushBmp.Canvas.LineTo(            -1, CurrentFrame-1);
  end;
end;

procedure TFuseTransition.Style7Frame(BrushBmp: TBitmap;
  CurrentFrame, Frames: Integer);
begin
  Style6Frame(BrushBmp, Frames - CurrentFrame + 2);
end;

procedure TFuseTransition.Style8Frame(BrushBmp: TBitmap; CurrentFrame: Integer);
const
  PixelArray: array[1..64*2] of byte =
    (4, 3,
     4, 4,
     3, 4,
     3, 3,
     3, 2,
     4, 2,
     5, 2,
     5, 3,
     5, 4,
     5, 5,
     4, 5,
     3, 5,
     2, 5,
     2, 4,
     2, 3,
     2, 2,
     2, 1,
     3, 1,
     4, 1,
     5, 1,
     6, 1,
     6, 2,
     6, 3,
     6, 4,
     6, 5,
     6, 6,
     5, 6,
     4, 6,
     3, 6,
     2, 6,
     1, 6,
     1, 5,
     1, 4,
     1, 3,
     1, 2,
     1, 1,
     1, 0,
     2, 0,
     3, 0,
     4, 0,
     5, 0,
     6, 0,
     7, 0,
     7, 1,
     7, 2,
     7, 3,
     7, 4,
     7, 5,
     7, 6,
     7, 7,
     6, 7,
     5, 7,
     4, 7,
     3, 7,
     2, 7,
     1, 7,
     0, 7,
     0, 6,
     0, 5,
     0, 4,
     0, 3,
     0, 2,
     0, 1,
     0, 0);
begin
  BrushBmp.Canvas.Pixels[
    PixelArray[(CurrentFrame*2)-1], PixelArray[CurrentFrame*2]] := clBlack;
end;

procedure TFuseTransition.Style9Frame(BrushBmp: TBitmap;
  CurrentFrame, Frames: Integer);
begin
  Style8Frame(BrushBmp, Frames - CurrentFrame + 2);
end;

procedure TFuseTransition.Style10Frame(BrushBmp: TBitmap; CurrentFrame: Integer);
begin
  BrushBmp.Canvas.MoveTo(CurrentFrame-8, 0);
  BrushBmp.Canvas.LineTo(CurrentFrame  , 8);
  BrushBmp.Canvas.MoveTo(CurrentFrame  , 0);
  BrushBmp.Canvas.LineTo(CurrentFrame+8, 8);
end;

procedure TFuseTransition.Style11Frame(BrushBmp: TBitmap;
  CurrentFrame, Frames: Integer);
begin
  Style10Frame(BrushBmp, Frames - CurrentFrame + 2);
end;

procedure TFuseTransition.Style12Frame(BrushBmp: TBitmap; CurrentFrame: Integer);
begin
  BrushBmp.Canvas.MoveTo(CurrentFrame+7 , 0);
  BrushBmp.Canvas.LineTo(CurrentFrame-1 , 8);
  BrushBmp.Canvas.MoveTo(CurrentFrame-1 , 0);
  BrushBmp.Canvas.LineTo(CurrentFrame-9,  8);
end;

procedure TFuseTransition.Style13Frame(BrushBmp: TBitmap;
  CurrentFrame, Frames: Integer);
begin
  Style12Frame(BrushBmp, Frames - CurrentFrame + 2);
end;

procedure TFuseTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'Style'];
  if Value <> '' then
    Style := StrToInt(Value);
end;

procedure TFuseTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
var
  Prop: String;
begin
  inherited;

  Prop := Prefix + 'Style';
  if(not OmitDefaultValues) or
    (Style <> 1) then
    List.Values[Prop] := IntToStr(Style);
end;

initialization

  TERegisterTransition(TFuseTransition);

end.
