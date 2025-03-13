unit teRoll;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teTimed, Windows, Messages, Graphics;

type
{$ifndef TE_NOHLP}
  TTELightMap = array[1..65535] of Shortint;
  PteLightMap = ^TTELightMap;
{$endif TE_NOHLP}

  TRollTransition = class(TTimedTransitionEffect)
  private
    FSize: Integer;
    FUse3D: Boolean;
    FUnroll: Boolean;

    procedure CreateLightMap(LightMap: PteLightMap; var LightMapSize: Integer;
      RollPixels: Integer);
    procedure ApplyLightMap(LightMap: PteLightMap; Work, Src: PAnsiChar; Width,
      RollScanLineSize, SrcScanLineSize, FirstLightIndex: Longint; RollRect,
      UnUpdateRect, SrcRect: TRect);
    procedure PaintRoll(Data: TTETransitionData; RollPixels, xAux, yAux: Integer);
  protected
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    function  GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    function  UnrollToUse: Boolean;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;
    procedure Assign(Source: TPersistent); override;
    class function GetEditor: String; override;
    function Is3D(Device: TTETransitionDevice): Boolean;
  published
    property Direction default tedDown;
    property Pass2Options;
    property PassSetting;
    property Size: Integer read FSize write FSize default 60;
    property Reversed;
    property Unroll: Boolean read FUnroll write FUnroll default True;
    property Use3D: Boolean read FUse3D write FUse3D default True;
  end;

implementation

uses teRender, teMskWk;

type
  TRollData = class(TTECustomData)
  public
    LightMap: PteLightMap;
    LightMapSize: Integer;
    ReversedBmp, RollBmp: TBitmap;
    RVisible, RRoll: TRect;

    destructor Destroy; override;
  end;

{ TRollTransition }

constructor TRollTransition.Create(AOwner: TComponent);
begin
  inherited;

  AllowedDirections := [tedRight, tedLeft, tedDown, tedUp, tedRandom];
  Direction         := tedDown;
  FSize             := 60;
  FUse3D            := True;
  FUnroll           := True;
end;

class function TRollTransition.Description: String;
begin
  Result := 'Roll';
end;

procedure TRollTransition.Assign(Source: TPersistent);
begin
  if Source is TRollTransition
  then
  begin
    inherited;

    Use3D  := TRollTransition(Source).Use3D;
    Unroll := TRollTransition(Source).Unroll;
    Size   := TRollTransition(Source).Size;
  end
  else inherited;
end;

class function TRollTransition.GetEditor: String;
begin
  Result := 'TRollTransitionEditor';
end;

function TRollTransition.Is3D(Device: TTETransitionDevice): Boolean;
begin
  Result :=
    TEProcessorInfo.MMX and
    Use3D               and
    (FSize > 0)         and
    Device.IsRGB;
end;

function TRollTransition.UnrollToUse: Boolean;
begin
  Result := FUnroll;
  if Reversed then
    Result := not Result;
end;

procedure TRollTransition.Initialize(Data: TTETransitionData; var TotalFrames:
  Longint);
var
  aux: TBitmap;
  RollData: TRollData;
begin
  inherited;

  RollData    := TRollData.Create(Data);
  Data.Custom := RollData;

  case DirectionToUse of
    tedRight:
      begin
        TotalFrames   := Data.Width - 1;
        if UnrollToUse
        then RollData.RRoll := Rect(-FSize, 0, 0, Data.Height)
        else RollData.RRoll := Rect(0, 0, 0, Data.Height);
      end;
    tedLeft:
      begin
        TotalFrames   := Data.Width - 1;
        if UnrollToUse
        then RollData.RRoll := Rect(Data.Width, 0, Data.Width + FSize, Data.Height)
        else RollData.RRoll := Rect(Data.Width, 0, Data.Width, Data.Height);
      end;
    tedDown:
      begin
        TotalFrames := Data.Height - 1;
        if UnrollToUse
        then RollData.RRoll := Rect(0, -FSize, Data.Width, 0)
        else RollData.RRoll := Rect(0, 0, Data.Width, 0);
      end;
    tedUp:
      begin
        TotalFrames   := Data.Height - 1;
        if UnrollToUse
        then RollData.RRoll := Rect(0, Data.Height, Data.Width, Data.Height + FSize)
        else RollData.RRoll := Rect(0, Data.Height, Data.Width, Data.Height);
      end;
  end;
  RollData.RVisible := RollData.RRoll;

  if Is3D(Data.Device) then
  begin
    RollData.LightMapSize := 0;
    GetMem(RollData.LightMap, FSize);
    if UnrollToUse then
      CreateLightMap(RollData.LightMap, RollData.LightMapSize, FSize);
  end;

  RollData.RollBmp := TBitmap.Create;
  RollData.RollBmp.Canvas.Lock;
  if DirectionToUse in [tedLeft, tedRight]
  then AdjustBmpForTransition(RollData.RollBmp, 0, FSize, Data.Height, pf32bit)
  else AdjustBmpForTransition(RollData.RollBmp, 0, Data.Width, FSize , pf32bit);

  RollData.ReversedBmp := TBitmap.Create;
  RollData.ReversedBmp.Canvas.Lock;
  AdjustBmpForTransition(RollData.ReversedBmp, 0, Data.Width, Data.Height, pf32bit);
  if UnrollToUse
  then aux := Data.DstBmp
  else aux := Data.SrcBmp;
  if DirectionToUse in [tedLeft, tedRight]
  then StretchBlt(RollData.ReversedBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
         aux.Canvas.Handle, Data.Width-1, 0, -Data.Width, Data.Height, cmSrcCopy)
  else StretchBlt(RollData.ReversedBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
         aux.Canvas.Handle, 0, Data.Height-1, Data.Width, -Data.Height, cmSrcCopy);
end;

procedure TRollTransition.ExecuteFrame(Data: TTETransitionData; CurrentFrame,
  Step, LastExecutedFrame: Longint);
var
  RollPixels,
  xAux,
  yAux: Integer;
  RVisAux: TRect;
  RollData: TRollData;
begin
  RollData := TRollData(Data.Custom);
  xAux     := 0;
  yAux     := 0;

  if UnRollToUse
  then
  begin
    if (Data.Frames + 1) - CurrentFrame >= FSize
    then RollPixels := FSize
    else RollPixels := (Data.Frames + 1) - CurrentFrame;

    case DirectionToUse of
      tedRight:
        begin
          RollData.RRoll   .Right := RollData.RRoll.Right + Step;
          RollData.RRoll   .Left  := RollData.RRoll.Right - RollPixels;
          RollData.RVisible.Left  := RollData.RVisible.Right;
          RollData.RVisible.Right := RollData.RRoll.Left;
          xAux                    := Data.Width - RollData.RRoll.Right - RollPixels;
        end;
      tedLeft:
        begin
          RollData.RRoll   .Left  := RollData.RRoll.Left - Step;
          RollData.RRoll   .Right := RollData.RRoll.Left + RollPixels;
          RollData.RVisible.Right := RollData.RVisible.Left;
          RollData.RVisible.Left  := RollData.RRoll.Right;
          xAux                    := Data.Width - RollData.RRoll.Right + RollPixels - 1;
        end;
      tedDown:
        begin
          RollData.RRoll   .Bottom := RollData.RRoll.Bottom + Step;
          RollData.RRoll   .Top    := RollData.RRoll.Bottom - RollPixels;
          RollData.RVisible.Top    := RollData.RVisible.Bottom;
          RollData.RVisible.Bottom := RollData.RRoll.Top;
          yAux                     := Data.Height - RollData.RRoll.Bottom - RollPixels;
        end;
      tedUp:
        begin
          RollData.RRoll   .Top    := RollData.RRoll.Top - Step;
          RollData.RRoll   .Bottom := RollData.RRoll.Top + RollPixels;
          RollData.RVisible.Bottom := RollData.RVisible.Top;
          RollData.RVisible.Top    := RollData.RRoll.Bottom;
          yAux                     := Data.Height - RollData.RRoll.Bottom + RollPixels - 1;
        end;
    end;
  end
  else
  begin
    if CurrentFrame >= FSize
    then RollPixels := FSize
    else RollPixels := CurrentFrame;

    case DirectionToUse of
      tedRight:
        begin
          RollData.RVisible.Left  := RollData.RVisible.Right;
          RollData.RVisible.Right := RollData.RVisible.Right + Step;
          RollData.RRoll   .Left  := RollData.RVisible.Right;
          RollData.RRoll   .Right := RollData.RRoll.Left + RollPixels;
          xAux                    := Data.Width - RollData.RRoll.Left - 1;
        end;
      tedLeft:
        begin
          RollData.RVisible.Right := RollData.RVisible.Left;
          RollData.RVisible.Left  := RollData.RVisible.Left - Step;
          RollData.RRoll   .Right := RollData.RVisible.Left;
          RollData.RRoll   .Left  := RollData.RRoll.Right - RollPixels;
          xAux                    := Data.Width - RollData.RRoll.Right - RollPixels;
        end;
      tedDown:
        begin
          RollData.RVisible.Top    := RollData.RVisible.Bottom;
          RollData.RVisible.Bottom := RollData.RVisible.Bottom + Step;
          RollData.RRoll   .Top    := RollData.RVisible.Bottom;
          RollData.RRoll   .Bottom := RollData.RRoll.Top + RollPixels;
          yAux                     := Data.Height - RollData.RRoll.Top - 1;
        end;
      tedUp:
        begin
          RollData.RVisible.Bottom := RollData.RVisible.Top;
          RollData.RVisible.Top    := RollData.RVisible.Top - Step;
          RollData.RRoll   .Bottom := RollData.RVisible.Top;
          RollData.RRoll   .Top    := RollData.RRoll.Bottom - RollPixels;
          yAux                     := Data.Height - RollData.RRoll.Bottom - RollPixels;
        end;
    end;
  end;

  IntersectRect(RVisAux, RollData.RVisible, Rect(0, 0, Data.Width, Data.Height));
  if not IsRectEmpty(RVisAux) then
    BitBlt(Data.Canvas.Handle, RVisAux.Left, RVisAux.Top,
      RVisAux.Right-RVisAux.Left, RVisAux.Bottom-RVisAux.Top,
      Data.DstBmp.Canvas.Handle, RVisAux.Left, RVisAux.Top, cmSrcCopy);

  if Is3D(Data.Device)
  then PaintRoll(Data, RollPixels, xAux, yAux)
  else BitBlt(Data.Canvas.Handle, RollData.RRoll.Left, RollData.RRoll.Top,
         RollData.RRoll.Right-RollData.RRoll.Left,
         RollData.RRoll.Bottom-RollData.RRoll.Top,
         RollData.ReversedBmp.Canvas.Handle, xAux, yAux, cmSrcCopy);
  UnionRect(Data.UpdateRect, RVisAux, RollData.RRoll);
end;

procedure TRollTransition.CreateLightMap(LightMap: PteLightMap;
  var LightMapSize: Integer; RollPixels: Integer);
const
  BaseLight = 110;
var
  Even: Boolean;
  Dif: Double;
  aux,
  Half,
  i,
  Values: Integer;
begin
  if RollPixels <> LightMapSize then
  begin
    if RollPixels > 2
    then
    begin
      Even   := (RollPixels mod 2) = 0;
      Values := (RollPixels - 1) div 4;
      Dif    := (BaseLight * 2) / ((RollPixels - 1) div 2);
      Half   := (RollPixels + 1) div 2;

      LightMap[1] := -BaseLight;
      LightMap[RollPixels] := -BaseLight;
      LightMap[Half      ] :=  BaseLight;
      if Even then
        LightMap[Half+1  ] :=  BaseLight;

      for i:= 1 to Values do
      begin
        aux := Round(BaseLight - (Dif * i));
        LightMap[i+1         ] := -aux;
        LightMap[RollPixels-i] := -aux;
        if RollPixels > 6 then
        begin
          LightMap[Half-i    ] :=  aux;
          if Even
          then LightMap[Half+i+1] := aux
          else LightMap[Half+i  ] := aux;
        end;
      end;
    end
    else
    begin
      if RollPixels = 1
      then LightMap[1] := BaseLight
      else
      begin
        LightMap[1] := -BaseLight;
        LightMap[2] :=  BaseLight;
      end;
    end;
    LightMapSize := RollPixels;
  end;
end;

procedure ApplyLightMapHrz(LightMap: PteLightMap; Work, Src: PByteArray; k,
  RowLenght: Longint);
var
  Light: Byte;
  i,
  Limit: Longint;
begin
  i := 1;
  while k < 0 do
  begin
    Limit := k + RowLenght;
    Light := -LightMap[i];
    if LightMap[i] > 0
    then
    begin
      while k < Limit do
      begin
        if Src[k] < Light
        then Work[k] := Src[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k);
        if Src[k] < Light
        then Work[k] := Src[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k);
        if Src[k] < Light
        then Work[k] := Src[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k, 2);
      end;
    end
    else
    begin
      while k < Limit do
      begin
        if Src[k] > Light
        then Work[k] := Src[k] - Light
        else Work[k] := 0;
        Inc(k);
        if Src[k] > Light
        then Work[k] := Src[k] - Light
        else Work[k] := 0;
        Inc(k);
        if Src[k] > Light
        then Work[k] := Src[k] - Light
        else Work[k] := 0;
        Inc(k, 2);
      end;
    end;
    Inc(i);
  end;
end;

procedure ApplyLightMapVrt(LightMap: PteLightMap; Work, Src: PByteArray; k, l,
  RollRowLenght, RollGap, SrcGap: Longint);
var
  Light: Byte;
  i,
  Limit: Longint;
begin
  // Relative adjustment for using a single index
  SrcGap := SrcGap - RollGap;
  Src    := PByteArray(PAnsiChar(Src) + l - k);

  while k < 0 do
  begin
    i     := 1;
    Limit := k + RollRowLenght;
    while k < Limit do
    begin
      Light := -LightMap[i];

      if LightMap[i] > 0
      then
      begin
        if Src[k] < Light
        then Work[k] := Src[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k);
        if Src[k] < Light
        then Work[k] := Src[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k);
        if Src[k] < Light
        then Work[k] := Src[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k, 2);
      end
      else
      begin
        if Src[k] > Light
        then Work[k] := Src[k] - Light
        else Work[k] := 0;
        Inc(k);
        if Src[k] > Light
        then Work[k] := Src[k] - Light
        else Work[k] := 0;
        Inc(k);
        if Src[k] > Light
        then Work[k] := Src[k] - Light
        else Work[k] := 0;
        Inc(k, 2);
      end;
      Inc(i);
    end;
    Inc(k, RollGap);
    Src := PByteArray(PAnsiChar(Src) + SrcGap);
  end;
end;
{
procedure ApplyLightMapHrz(LightMap: PteLightMap; Work, Src: PByteArray; k,
  RowLenght: Longint);
var
  Light: Byte;
  i,
  Limit: Longint;
begin
  i := 1;
  while k < 0 do
  begin
    Limit := k + RowLenght;
    Light := -LightMap[i];
    if LightMap[i] > 0
    then
    begin
      while k < Limit do
      begin
        if Work[k] < Light
        then Work[k] := Work[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k);
        if Work[k] < Light
        then Work[k] := Work[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k);
        if Work[k] < Light
        then Work[k] := Work[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k, 2);
      end;
    end
    else
    begin
      while k < Limit do
      begin
        if Work[k] > Light
        then Work[k] := Work[k] - Light
        else Work[k] := 0;
        Inc(k);
        if Work[k] > Light
        then Work[k] := Work[k] - Light
        else Work[k] := 0;
        Inc(k);
        if Work[k] > Light
        then Work[k] := Work[k] - Light
        else Work[k] := 0;
        Inc(k, 2);
      end;
    end;
    Inc(i);
  end;
end;

procedure ApplyLightMapVrt(LightMap: PteLightMap; Work, Src: PByteArray; k, l,
  RollRowLenght, RollGap, SrcGap: Longint);
var
  Light: Byte;
  i,
  Limit: Longint;
begin
  while k < 0 do
  begin
    i     := 1;
    Limit := k + RollRowLenght;
    while k < Limit do
    begin
      Light := -LightMap[i];

      if LightMap[i] > 0
      then
      begin
        if Work[k] < Light
        then Work[k] := Work[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k);
        if Work[k] < Light
        then Work[k] := Work[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k);
        if Work[k] < Light
        then Work[k] := Work[k] + LightMap[i]
        else Work[k] := 255;
        Inc(k, 2);
      end
      else
      begin
        if Work[k] > Light
        then Work[k] := Work[k] - Light
        else Work[k] := 0;
        Inc(k);
        if Work[k] > Light
        then Work[k] := Work[k] - Light
        else Work[k] := 0;
        Inc(k);
        if Work[k] > Light
        then Work[k] := Work[k] - Light
        else Work[k] := 0;
        Inc(k, 2);
      end;
      Inc(i);
    end;
    Inc(k, RollGap);
  end;
end;
}

procedure TRollTransition.ApplyLightMap(LightMap: PteLightMap;
  Work, Src: PAnsiChar;
  Width, RollScanLineSize, SrcScanLineSize, FirstLightIndex: Longint;
  RollRect, UnUpdateRect, SrcRect: TRect);
var
  RollUpdParams,
  SrcUpdParams: TTEUpdParams;
  LM: PteLightMap;
begin
  GiveMeTheUpdParams(2, RollUpdParams, RollScanLineSize, RollRect, UnUpdateRect,
    pf32bit);
  GiveMeTheUpdParams(2, SrcUpdParams , SrcScanLineSize , SrcRect , UnUpdateRect,
    pf32bit);

  LM := PteLightMap(PAnsiChar(LightMap) + FirstLightIndex);
  if DirectionToUse in [tedDown, tedUp]
  then ApplyLightMapHrz(LM, PByteArray(Work - RollUpdParams.Start1),
         PByteArray(Src - SrcUpdParams.Start1),
         -RollUpdParams.Lenght1 * 4, RollUpdParams.RowLenght1 * 4)
  else ApplyLightMapVrt(LM, PByteArray(Work - RollUpdParams.Start1),
         PByteArray(Src - SrcUpdParams.Start1),
         -RollUpdParams.Lenght1 * 4, -SrcUpdParams.Lenght1 * 4,
         RollUpdParams.RowLenght1 * 4, RollUpdParams.Gap1 * 4,
         SrcUpdParams.Gap1 * 4);
end;

function TRollTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiOffScreenBmpCapable,
      tetiThreadSafe
    ];
end;

procedure TRollTransition.PaintRoll(Data: TTETransitionData; RollPixels, xAux,
  yAux: Integer);
var
  RollScanLineSize,
  SrcScanLineSize,
  FirstLightIndex: Integer;
  Work,
  Src: Pointer;
  RRollAux,
  RRollVis,
  RSrc: TRect;
  RollData: TRollData;
begin
  RollData := TRollData(Data.Custom);
  IntersectRect(RRollVis, RollData.RRoll, Rect(0, 0, Data.Width, Data.Height));
  RRollAux    := RRollVis;
  OffsetRect(RRollAux, -RollData.RRoll.Left, -RollData.RRoll.Top);
  RSrc.Left   := xAux + (RRollVis.Left - RollData.RRoll.Left);
  RSrc.Top    := yAux + (RRollVis.Top  - RollData.RRoll.Top);
  RSrc.Right  := RSrc.Left + (RRollVis.Right  - RRollVis.Left);
  RSrc.Bottom := RSrc.Top  + (RRollVis.Bottom - RRollVis.Top);
{
  BitBlt(RollBmp.Canvas.Handle, RRollAux.Left, RRollAux.Top,
    RRollAux.Right - RRollAux.Left, RRollAux.Bottom - RRollAux.Top,
    ReversedBmp.Canvas.Handle, RSrc.Left, RSrc.Top, cmSrcCopy);
}
  CreateLightMap(RollData.LightMap, RollData.LightMapSize, RollPixels);

  RollScanLineSize := GetBytesPerScanline(RollData.RollBmp    , pf32bit, 32);
  SrcScanLineSize  := GetBytesPerScanline(RollData.ReversedBmp, pf32bit, 32);
  Work             := PAnsiChar(RollData.RollBmp    .ScanLine[0]) + RollScanlineSize;
  Src              := PAnsiChar(RollData.ReversedBmp.ScanLine[0]) + SrcScanlineSize;

  if DirectionToUse in [tedRight, tedLeft]
  then FirstLightIndex := RRollAux.Left
  else FirstLightIndex := RollPixels - RRollAux.Bottom;

  ApplyLightMap(RollData.LightMap, Work, Src, RollData.RollBmp.Width,
    RollScanLineSize, SrcScanLineSize, FirstLightIndex, RRollAux,
    Rect(0, 0, 0, 0), RSrc);

  BitBlt(Data.Canvas.Handle, RRollVis.Left, RRollVis.Top,
    RRollVis.Right - RRollVis.Left, RRollVis.Bottom - RRollVis.Top,
    RollData.RollBmp.Canvas.Handle, RRollAux.Left, RRollAux.Top, cmSrcCopy);
end;

procedure TRollTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'Size'];
  if Value <> '' then
    Size := StrToInt(Value);

  Value := List.Values[Prefix + 'Unroll'];
  if Value <> '' then
    Unroll := SameText(Value, BoolToStr(True));

  Value := List.Values[Prefix + 'Use3D'];
  if Value <> '' then
    Use3D := SameText(Value, BoolToStr(True));
end;

procedure TRollTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
begin
  inherited;

  if(not OmitDefaultValues) or (Size <> 60) then
    List.Values[Prefix + 'Size'] := IntToStr(Size);

  if(not OmitDefaultValues) or
    (Unroll <> True) then
    List.Values[Prefix + 'Unroll'] := BoolToStr(Unroll);

  if(not OmitDefaultValues) or
    (Use3D <> True) then
    List.Values[Prefix + 'Use3D'] := BoolToStr(Use3D);
end;

{ TRollData }

destructor TRollData.Destroy;
begin
  if TRollTransition(Data.Device.DelegateTransition).Is3D(Data.Device) then
    FreeMem(LightMap);

  ReversedBmp.Canvas.Unlock;
  ReversedBmp.Free;
  RollBmp    .Canvas.Unlock;
  RollBmp    .Free;

  inherited;
end;

initialization

  TERegisterTransition(TRollTransition);

end.
