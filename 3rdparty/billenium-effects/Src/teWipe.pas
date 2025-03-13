unit teWipe;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teTimed, Windows, Messages, Graphics;

type
  TWipeTransition = class(TTimedTransitionEffect)
  private
    FBandWidth: Word;
    FSmoothBand: Boolean;

    function GetBandData(Data: TTETransitionData;
      var ABandWidth: Word): Boolean;
  protected
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    function GetBitmapsWidth(Data: TTETransitionData): Integer; override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    function GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
    function Smooth(Device: TTETransitionDevice): Boolean;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;

    procedure Assign(Source: TPersistent); override;
    class function GetEditor: String; override;
  published
    property BandWidth: Word read FBandWidth write FBandWidth default 50;
    property Direction default tedRight;
    property Pass2Options;
    property PassSetting;
    property Reversed;
    property SmoothBand: Boolean read FSmoothBand write FSmoothBand default False;
  end;

implementation

uses teRender, teMskWk;

const
  Margin = 75;

type
  TWipeData = class(TTECustomData)
  public
    BandBmp,
    AuxBmp1,
    AuxBmp2: TBitmap;
    RVisible,
    R2,
    RBand: TRect;
    BandLeft,
    BandTop,
    BandWidth: Word;
    SmoothBand: Boolean;
    Apply256MaskSProc: PByteArray;

    destructor Destroy; override;
  end;

constructor TWipeTransition.Create(AOwner: TComponent);
begin
  inherited;

  AllowedDirections := [tedRight, tedLeft, tedDown, tedUp, tedDownRight,
    tedDownLeft, tedUpRight, tedUpLeft, tedIn, tedOut, tedRandom];
  Direction         := tedRight;
  FBandWidth        := 50;
  FSmoothBand       := False;
end;

class function TWipeTransition.Description: String;
begin
  Result := 'Wipe';
end;

procedure TWipeTransition.Assign(Source: TPersistent);
var
  Transition: TWipeTransition;
begin
  if Source is TWipeTransition
  then
  begin
    inherited;

    Transition  := TWipeTransition(Source);
    FBandWidth  := Transition.BandWidth;
    FSmoothBand := Transition.SmoothBand;
  end
  else inherited;
end;

class function TWipeTransition.GetEditor: String;
begin
  Result := 'TWipeTransitionEditor';
end;

procedure TWipeTransition.Initialize(Data: TTETransitionData;
  var TotalFrames: Longint);

  function CreateBandBmp: TBitmap;
  var
    i,
    j,
    Prob: Integer;
  begin
    RandSeed := 1642000;
    Result   := TBitmap.Create;
    Result.Canvas.Lock;
    Result.Monochrome := True;

    case DirectionToUse of
      tedRight,
      tedLeft:
      begin
        Result.Width  := FBandWidth;
        Result.Height := Data.Height + Margin;
      end;
      tedDown,
      tedUp:
      begin
        Result.Width  := Data.Width + Margin;
        Result.Height := FBandWidth;
      end;
    end;

    case DirectionToUse of
      tedRight:
      begin
        for i:=0 to FBandWidth-1 do
        begin
          Prob := 100 - ((i * 100) DIV FBandWidth);
          if Prob = 0 then
            Prob := 1;
          for j:=0 to Result.Height do
            if Random(100) <= Prob then
              Result.Canvas.Pixels[i, j] := clBlack;
        end;
      end;
      tedLeft:
      begin
        for i:=0 to FBandWidth-1 do
        begin
          Prob := ((i * 100) DIV FBandWidth);
          if Prob = 0 then
            Prob := 1;
          for j:=0 to Result.Height do
            if Random(100) <= Prob then
              Result.Canvas.Pixels[i, j] := clBlack;
        end;
      end;
      tedDown:
      begin
        for j:=0 to FBandWidth-1 do
        begin
          Prob := 100 - ((j * 100) DIV FBandWidth);
          if Prob = 0 then
            Prob := 1;
          for i:=0 to Result.Width do
            if Random(100) <= Prob then
              Result.Canvas.Pixels[i, j] := clBlack;
        end;
      end;
      tedUp:
      begin
        for j:=0 to FBandWidth-1 do
        begin
          Prob := ((j * 100) DIV FBandWidth);
          if Prob = 0 then
            Prob := 1;
          for i:=0 to Result.Width do
            if Random(100) <= Prob then
              Result.Canvas.Pixels[i, j] := clBlack;
        end;
      end;
    end;
  end;

  function CreateSmoothBandBmp(WipeData: TWipeData): TBitmap;
  var
    Src,
    Work: PAnsiChar;
    BandBmpWidth,
    BandBmpHeight,
    i,
    j,
    aux,
    ScanLineSize,
    First,
    Last: Integer;
    Level: Byte;
    Step: Double;
  begin
    Result := nil;
    case DirectionToUse of
      tedRight,
      tedLeft:
      begin
        BandBmpWidth  := WipeData.BandWidth;
        BandBmpHeight := Data.Height;

        Result := TBitmap.Create;
        Result.Canvas.Lock;
        AdjustBmpForTransition(Result, CreateGrayScalePalette, BandBmpWidth,
          BandBmpHeight, pf8bit);
        ScanLineSize := GetBytesPerScanline(Result, pf8bit, 32);
        Work         := Result.ScanLine[Data.Height-1];
        Src          := Work;
        Step         := 253 / (WipeData.BandWidth + 1);

        if DirectionToUse = tedRight
        then
        begin
          First := 0;
          Last  := BandBmpWidth;
        end
        else
        begin
          First := BandBmpWidth-1;
          Last  := -1;
        end;
        aux := First;
        while aux <> Last do
        begin
          Level := Round(aux * Step) + 1;
          Work^ := AnsiChar(Level);
          Inc(Work);
          if DirectionToUse = tedRight
          then Inc(aux)
          else Dec(aux);
        end;
        Work := PAnsiChar(Src) + ScanLineSize;
        for i := 1 to BandBmpHeight-1 do
        begin
          Move(Src[0], Work[0], ScanLineSize);
          Inc(Work, ScanLineSize);
        end;
      end;
      tedDown,
      tedUp:
      begin
        BandBmpHeight := WipeData.BandWidth;
        BandBmpWidth  := Data.Width;

        Result := TBitmap.Create;
        Result.Canvas.Lock;
        AdjustBmpForTransition(Result, CreateGrayScalePalette, BandBmpWidth,
          BandBmpHeight, pf8bit);
        ScanLineSize := GetBytesPerScanline(Result, pf8bit, 32);
        Work         := Result.ScanLine[BandBmpHeight-1];
        Step         := 253 / (BandBmpHeight + 1);

        if DirectionToUse = tedUp
        then First := 0
        else First := BandBmpHeight-1;
        aux := First;
        for i := 0 to BandBmpHeight-1 do
        begin
          Level := Round(aux * Step) + 1;
          for j := 0 to BandBmpWidth-1 do
            Work[j] := AnsiChar(Level);
          Inc(Work, ScanLineSize);
          if DirectionToUse = tedUp
          then Inc(aux)
          else Dec(aux);
        end;
      end;
    end;
  end;

var
  auxWidth,
  auxHeight: Integer;
  WipeData: TWipeData;
begin
  inherited;

  WipeData            := TWipeData.Create(Data);
  Data.Custom         := WipeData;
  WipeData.AuxBmp1    := nil;
  WipeData.AuxBmp2    := nil;
  WipeData.BandBmp    := nil;
  WipeData.BandLeft   := 0;
  WipeData.BandTop    := 0;
  WipeData.SmoothBand := Smooth(Data.Device);
  GetBandData(Data, WipeData.BandWidth);
  if WipeData.BandWidth > 0 then
  begin
    if WipeData.SmoothBand
    then
    begin
      WipeData.BandBmp := CreateSmoothBandBmp(WipeData);
      WipeData.AuxBmp1 := TBitmap.Create;
      WipeData.AuxBmp1.Canvas.Lock;
      AdjustBmpForTransition(WipeData.AuxBmp1, CreateGrayScalePalette,
        Data.Bitmap.Width, Data.Height, pf8bit);
      WipeData.Apply256MaskSProc := GetApply256MaskSProc;
    end
    else
    begin
      WipeData.BandBmp := CreateBandBmp;

      WipeData.AuxBmp1 := TBitmap.Create;
      WipeData.AuxBmp1.Canvas.Lock;
      WipeData.AuxBmp2 := TBitmap.Create;
      WipeData.AuxBmp2.Canvas.Lock;

      auxWidth  := 0;
      auxHeight := 0;
      case DirectionToUse of
        tedRight, tedLeft:
        begin
          auxWidth  := WipeData.BandBmp.Width;
          auxHeight := WipeData.BandBmp.Height - Margin;
        end;
        tedDown, tedUp   :
        begin
          auxWidth  := WipeData.BandBmp.Width - Margin;
          auxHeight := WipeData.BandBmp.Height;
        end;
      end;

      AdjustBmpForTransition(WipeData.AuxBmp1, Data.DstBmp.Palette, auxWidth,
        auxHeight, GetPixelFormat(Data.Device));
      AdjustBmpForTransition(WipeData.AuxBmp2, Data.DstBmp.Palette, auxWidth,
        auxHeight, WipeData.AuxBmp1.PixelFormat);
    end;
  end;

  case DirectionToUse of
    tedRight:
    begin
      TotalFrames       := Data.Width + WipeData.BandWidth - 1;
      WipeData.RVisible :=
        Rect(0 - WipeData.BandWidth+1, 0, -WipeData.BandWidth, Data.Height);
      if Assigned(WipeData.BandBmp) then
        WipeData.RBand := Rect(WipeData.RVisible.Right+1, WipeData.RVisible.Top,
          WipeData.RVisible.Right+WipeData.BandWidth+1, WipeData.RVisible.Bottom);
    end;
    tedLeft:
    begin
      TotalFrames   := Data.Width + WipeData.BandWidth - 1;
      WipeData.RVisible := Rect(Data.Width + WipeData.BandWidth, 0,
        Data.Width + WipeData.BandWidth, Data.Height);
      if Assigned(WipeData.BandBmp) then
        WipeData.RBand := Rect(WipeData.RVisible.Left-WipeData.BandWidth-1,
          WipeData.RVisible.Top, WipeData.RVisible.Right-1,
          WipeData.RVisible.Bottom);
    end;
    tedDown:
    begin
      TotalFrames   := Data.Height + WipeData.BandWidth - 1;
      WipeData.RVisible := Rect(0, - WipeData.BandWidth, Data.Width, -WipeData.BandWidth);
      if Assigned(WipeData.BandBmp) then
        WipeData.RBand := Rect(WipeData.RVisible.Left,
          WipeData.RVisible.Bottom+1, WipeData.RVisible.Right,
          WipeData.RVisible.Bottom+WipeData.BandWidth+1);
    end;
    tedUp:
    begin
      TotalFrames   := Data.Height + WipeData.BandWidth - 1;
      WipeData.RVisible := Rect(0, Data.Height + WipeData.BandWidth, Data.Width,
        Data.Height + WipeData.BandWidth);
      if WipeData.BandBmp <> nil then
        WipeData.RBand := Rect(WipeData.RVisible.Left,
          WipeData.RVisible.Top-WipeData.BandWidth-1, WipeData.RVisible.Right,
          WipeData.RVisible.Bottom-1);
    end;
    tedDownRight:
    begin
      TotalFrames       := Data.Width + Data.Height - 1;
      WipeData.RVisible := Rect(0, 0, 0, 0);
    end;
    tedDownLeft:
    begin
      if Data.Width > Data.Height
      then TotalFrames := Data.Width -1
      else TotalFrames := Data.Height-1;
      WipeData.RVisible := Rect(Data.Width, 0, Data.Width, 0);
    end;
    tedUpRight:
    begin
      if Data.Width > Data.Height
      then TotalFrames := Data.Width -1
      else TotalFrames := Data.Height-1;
      WipeData.RVisible := Rect(0, Data.Height, 0, Data.Height);
    end;
    tedUpLeft:
    begin
      if Data.Width > Data.Height
      then TotalFrames := Data.Width -1
      else TotalFrames := Data.Height-1;
      WipeData.RVisible := Rect(Data.Width, Data.Height, Data.Width, Data.Height);
    end;
    tedIn:
    begin
      if Data.Width > Data.Height
      then TotalFrames := (Data.Width  div 2) - 1
      else TotalFrames := (Data.Height div 2) - 1;
      WipeData.RVisible := Rect(0, 0, Data.Width, Data.Height);
    end;
    tedOut:
    begin
      if Data.Width > Data.Height
      then TotalFrames := (Data.Width  div 2) - 1
      else TotalFrames := (Data.Height div 2) - 1;
      WipeData.RVisible := Rect(Data.Width div 2, Data.Height div 2,
        Data.Width div 2, Data.Height div 2);
    end;
  end;
end;

procedure TWipeTransition.ExecuteFrame(Data: TTETransitionData; CurrentFrame,
  Step, LastExecutedFrame: Longint);

 procedure ApplySmoothing(WipeData: TWipeData);
 var
    Work,
    Dst,
    Src,
    Mask: Pointer;
    ScanLineSize,
    MaskScanLineSize: Integer;
    R: TRect;
  begin
    BitBlt(WipeData.AuxBmp1.Canvas.Handle, WipeData.RBand.Left,
      WipeData.RBand.Top, WipeData.RBand.Right-WipeData.RBand.Left,
      WipeData.RBand.Bottom-WipeData.RBand.Top, WipeData.BandBmp.Canvas.Handle,
      0, 0, cmSrcCopy);
    MaskScanLineSize := GetBytesPerScanline(WipeData.AuxBmp1, pf8bit          , 32);
    ScanLineSize     := GetBytesPerScanline(Data.Bitmap     , Data.PixelFormat, 32);
    Work := PAnsiChar(Data.Bitmap     .ScanLine[0]) + ScanlineSize;
    Dst  := PAnsiChar(Data.DstBmp     .ScanLine[0]) + ScanlineSize;
    Src  := PAnsiChar(Data.SrcBmp     .ScanLine[0]) + ScanlineSize;
    Mask := PAnsiChar(WipeData.AuxBmp1.ScanLine[0]) + MaskScanLineSize;
    IntersectRect(R, WipeData.RBand, Rect(0, 0, Data.Width, Data.Height));
    Apply256MaskS(TApply256MaskSProc(WipeData.Apply256MaskSProc), Work,
      Dst, Src, Mask, ScanLineSize, MaskScanLineSize, WipeData.AuxBmp1.Width,
      R, Rect(0, 0, 0, 0));
  end;

var
  R1, R2, R3, R4: TRect;
  aux1, aux2, aux3, aux4: Word;
  WipeData: TWipeData;
begin
  WipeData := TWipeData(Data.Custom);
  case DirectionToUse of
    tedRight:
    begin
      WipeData.RVisible.Left  := WipeData.RVisible.Right;
      WipeData.RVisible.Right := WipeData.RVisible.Right + Step;
      if Assigned(WipeData.BandBmp) then
      begin
        WipeData.RBand.Left  := WipeData.RVisible.Right + 1;
        WipeData.RBand.Right := WipeData.RBand.Left + WipeData.BandWidth;
        if not WipeData.SmoothBand then
          WipeData.BandTop   := Random(Margin);
      end;
    end;
    tedLeft:
    begin
      WipeData.RVisible.Right := WipeData.RVisible.Left;
      WipeData.RVisible.Left  := WipeData.RVisible.Left - Step;
      if Assigned(WipeData.BandBmp) then
      begin
        WipeData.RBand.Right := WipeData.RVisible.Left - 1;
        WipeData.RBand.Left  := WipeData.RBand.Right - WipeData.BandWidth;
        WipeData.BandTop     := Random(Margin);
      end;
    end;
    tedDown:
    begin
      WipeData.RVisible.Top    := WipeData.RVisible.Bottom;
      WipeData.RVisible.Bottom := WipeData.RVisible.Bottom + Step;
      if Assigned(WipeData.BandBmp) then
      begin
        WipeData.RBand.Top    := WipeData.RVisible.Bottom + 1;
        WipeData.RBand.Bottom := WipeData.RBand.Top + WipeData.BandWidth;
        WipeData.BandLeft     := Random(Margin);
      end;
    end;
    tedUp:
    begin
      WipeData.RVisible.Bottom := WipeData.RVisible.Top;
      WipeData.RVisible.Top    := WipeData.RVisible.Top - Step;
      if Assigned(WipeData.BandBmp) then
      begin
        WipeData.RBand.Bottom := WipeData.RVisible.Bottom - 1;
        WipeData.RBand.Top    := WipeData.RBand.Bottom - WipeData.BandWidth;
        WipeData.BandLeft     := Random(Margin);
      end;
    end;
    tedDownRight:
    begin
      aux1 := Round((Data.Width  / (Data.Frames + 1)) * CurrentFrame);
      aux2 := Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
      R1   := Rect(0, WipeData.RVisible.Bottom, aux1, aux2);
      R2   := Rect(WipeData.RVisible.Right, 0, aux1, WipeData.RVisible.Bottom);
      WipeData.RVisible.Right  := aux1;
      WipeData.RVisible.Bottom := aux2;
    end;
    tedDownLeft:
    begin
      aux1 := Data.Width - Round((Data.Width / (Data.Frames + 1)) * CurrentFrame);
      aux2 := Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
      R1   := Rect(aux1, WipeData.RVisible.Bottom, Data.Width, aux2);
      R2   := Rect(aux1, 0, WipeData.RVisible.Left, WipeData.RVisible.Bottom);
      WipeData.RVisible.Left   := aux1;
      WipeData.RVisible.Bottom := aux2;
    end;
    tedUpRight:
    begin
      aux1 := Round((Data.Width / (Data.Frames + 1)) * CurrentFrame);
      aux2 := Data.Height - Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
      R1   := Rect(0, aux2, aux1, WipeData.RVisible.Top);
      R2   := Rect(WipeData.RVisible.Right, WipeData.RVisible.Top, aux1, Data.Height);
      WipeData.RVisible.Right := aux1;
      WipeData.RVisible.Top   := aux2;
    end;
    tedUpLeft:
    begin
      aux1 := Data.Width  - Round((Data.Width  / (Data.Frames + 1)) * CurrentFrame);
      aux2 := Data.Height - Round((Data.Height / (Data.Frames + 1)) * CurrentFrame);
      R1   := Rect(aux1, aux2, Data.Width, WipeData.RVisible.Top);
      R2   := Rect(aux1, WipeData.RVisible.Top, WipeData.RVisible.Left, Data.Height);
      WipeData.RVisible.Left := aux1;
      WipeData.RVisible.Top  := aux2;
    end;
    tedIn:
    begin
      aux1 := (Round((Data.Width / (Data.Frames + 1)) * CurrentFrame)) div 2;
      aux3 := Data.Width - aux1;
      aux2 := (Round((Data.Height / (Data.Frames + 1)) * CurrentFrame)) div 2;
      aux4 := Data.Height - aux2;
      R1   := Rect(WipeData.RVisible.Left, WipeData.RVisible.Top, WipeData.RVisible.Right, aux2);
      R2   := Rect(WipeData.RVisible.Left, aux2, aux1, aux4);
      R3   := Rect(aux3, aux2, WipeData.RVisible.Right, aux4);
      R4   := Rect(WipeData.RVisible.Left, aux4, WipeData.RVisible.Right, WipeData.RVisible.Bottom);
      Data.UpdateRect   := WipeData.RVisible;
      WipeData.RVisible := Rect(aux1, aux2, aux3, aux4);
      Data.UnUpdateRect := WipeData.RVisible;
    end;
    tedOut:
    begin
      aux1 :=
        (Data.Width - Round((Data.Width / (Data.Frames + 1)) * CurrentFrame)) div 2;
      aux3 := Data.Width - aux1;
      aux2 :=
        (Data.Height - Round((Data.Height / (Data.Frames + 1)) * CurrentFrame)) div 2;
      aux4 := Data.Height - aux2;
      R1   := Rect(aux1, aux2, aux3, WipeData.RVisible.Top);
      R2   := Rect(aux1, WipeData.RVisible.Top, WipeData.RVisible.Left, aux4);
      R3   := Rect(WipeData.RVisible.Right, WipeData.RVisible.Top, aux3, WipeData.RVisible.Bottom);
      R4   := Rect(aux1, WipeData.RVisible.Bottom, aux3, aux4);
      Data.UnUpdateRect := WipeData.RVisible;
      WipeData.RVisible := Rect(aux1, aux2, aux3, aux4);
    end;
  end;

  if Assigned(WipeData.BandBmp)
  then
  begin
    if WipeData.SmoothBand
    then ApplySmoothing(WipeData)
    else
    begin
      BitBlt(WipeData.AuxBmp1.Canvas.Handle, 0, 0, WipeData.AuxBmp1.Width,
        WipeData.AuxBmp1.Height, Data.SrcBmp.Canvas.Handle, WipeData.RBand.Left,
        WipeData.RBand.Top, cmSrcCopy);
      BitBlt(WipeData.AuxBmp1.Canvas.Handle, 0, 0, WipeData.AuxBmp1.Width,
        WipeData.AuxBmp1.Height, WipeData.BandBmp.Canvas.Handle,
        WipeData.BandLeft, WipeData.BandTop, cmSrcAnd);
      BitBlt(WipeData.AuxBmp2.Canvas.Handle, 0, 0, WipeData.AuxBmp2.Width,
        WipeData.AuxBmp2.Height, Data.DstBmp.Canvas.Handle, WipeData.RBand.Left,
        WipeData.RBand.Top, cmSrcCopy);
      BitBlt(WipeData.AuxBmp2.Canvas.Handle, 0, 0, WipeData.AuxBmp2.Width,
        WipeData.AuxBmp2.Height, WipeData.BandBmp.Canvas.Handle, WipeData.BandLeft,
        WipeData.BandTop, $00220326);
      BitBlt(WipeData.AuxBmp2.Canvas.Handle, 0, 0, WipeData.AuxBmp2.Width,
        WipeData.AuxBmp2.Height, WipeData.AuxBmp1.Canvas.Handle, 0, 0, cmSrcPaint);
      BitBlt(Data.Canvas.Handle, WipeData.RBand.Left, WipeData.RBand.Top,
        WipeData.RBand.Right-WipeData.RBand.Left,
        WipeData.RBand.Bottom-WipeData.RBand.Top, WipeData.AuxBmp2.Canvas.Handle,
        0, 0, cmSrcCopy);
    end;
    Windows.UnionRect(Data.UpdateRect, WipeData.RBand, Data.UpdateRect);
    BitBlt(Data.Canvas.Handle, WipeData.RVisible.Left, WipeData.RVisible.Top,
      WipeData.RVisible.Right-WipeData.RVisible.Left+1,
      WipeData.RVisible.Bottom-WipeData.RVisible.Top+1,
      Data.DstBmp.Canvas.Handle, WipeData.RVisible.Left, WipeData.RVisible.Top,
      cmSrcCopy);
  end
  else
  begin
    case DirectionToUse of
      tedRight,
      tedLeft,
      tedDown,
      tedUp:
        BitBlt(Data.Canvas.Handle, WipeData.RVisible.Left, WipeData.RVisible.Top,
          WipeData.RVisible.Right-WipeData.RVisible.Left,
          WipeData.RVisible.Bottom-WipeData.RVisible.Top,
          Data.DstBmp.Canvas.Handle, WipeData.RVisible.Left,
          WipeData.RVisible.Top, cmSrcCopy);
      tedDownRight,
      tedDownLeft,
      tedUpRight,
      tedUpLeft:
      begin
        BitBlt(Data.Canvas.Handle, R1.Left, R1.Top, R1.Right-R1.Left,
          R1.Bottom-R1.Top, Data.DstBmp.Canvas.Handle, R1.Left, R1.Top,
          cmSrcCopy);
        BitBlt(Data.Canvas.Handle, R2.Left, R2.Top, R2.Right-R2.Left,
          R2.Bottom-R2.Top, Data.DstBmp.Canvas.Handle, R2.Left, R2.Top,
          cmSrcCopy);
      end;
      tedIn, tedOut:
      begin
        BitBlt(Data.Canvas.Handle, R1.Left, R1.Top, R1.Right-R1.Left,
          R1.Bottom-R1.Top, Data.DstBmp.Canvas.Handle, R1.Left, R1.Top,
          cmSrcCopy);
        BitBlt(Data.Canvas.Handle, R2.Left, R2.Top, R2.Right-R2.Left,
          R2.Bottom-R2.Top, Data.DstBmp.Canvas.Handle, R2.Left, R2.Top,
          cmSrcCopy);
        BitBlt(Data.Canvas.Handle, R3.Left, R3.Top, R3.Right-R3.Left,
          R3.Bottom-R3.Top, Data.DstBmp.Canvas.Handle, R3.Left, R3.Top,
          cmSrcCopy);
        BitBlt(Data.Canvas.Handle, R4.Left, R4.Top, R4.Right-R4.Left,
          R4.Bottom-R4.Top, Data.DstBmp.Canvas.Handle, R4.Left, R4.Top,
          cmSrcCopy);
      end;
    end;
  end;
  Windows.UnionRect(Data.UpdateRect, WipeData.RVisible, Data.UpdateRect);
  Windows.IntersectRect(Data.UpdateRect, Data.UpdateRect, Rect(0, 0, Data.Width, Data.Height));
end;

function TWipeTransition.Smooth(Device: TTETransitionDevice): Boolean;
begin
  Result :=
    FSmoothBand and (FBandWidth > 0) and TEProcessorInfo.MMX and Device.IsRGB;
end;

function TWipeTransition.GetPixelFormat(Device: TTETransitionDevice):
    TPixelFormat;
begin
  Result := Device.PixelFormat;
  if(Result <> pf32bit) and Smooth(Device) then
    Result := pf32bit;
end;

function TWipeTransition.GetBandData(Data: TTETransitionData;
  var ABandWidth: Word): Boolean;
begin
  case DirectionToUse of
    tedRight,
    tedLeft:
    begin
      ABandWidth := FBandWidth;
      if Assigned(Data) and (FBandWidth > Data.Width) then
        ABandWidth := Data.Width;
    end;
    tedDown,
    tedUp:
    begin
      ABandWidth := FBandWidth;
      if Assigned(Data) and (FBandWidth > Data.Height) then
        ABandWidth := Data.Height;
    end;
    else ABandWidth  := 0;
  end;
  Result := (ABandWidth > 0);
end;

function TWipeTransition.GetBitmapsWidth(Data: TTETransitionData): Integer;
begin
  if Smooth(Data.Device)
  then Result := (((Data.Width-1) div  8) + 1) *  8
  else Result := inherited GetBitmapsWidth(Data);
end;

function TWipeTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
var
  BandWidthAux: Word;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiOffScreenBmpCapable,
      tetiStaticSrcPixels,
      tetiThreadSafe
    ] -
    [
      tetiNeedSrcBmp
    ];
  if Assigned(Device) and GetBandData(nil, BandWidthAux) then
    Result := Result +
      [
        tetiNeedSrcBmp,
        tetiNeedOffScreenBmp
      ];
end;

{ TWipeData }

destructor TWipeData.Destroy;
begin
  if Assigned(AuxBmp1) then
  begin
    AuxBmp1.Canvas.Unlock;
    AuxBmp1.Free;
  end;
  if Assigned(AuxBmp2) then
  begin
    AuxBmp2.Canvas.Unlock;
    AuxBmp2.Free;
  end;
  if Assigned(BandBmp) then
  begin
    BandBmp.Canvas.Unlock;
    BandBmp.Free;
  end;
  if Assigned(Apply256MaskSProc) then
    VirtualFree(Apply256MaskSProc, 0, MEM_RELEASE);

  inherited;
end;

procedure TWipeTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'BandWidth'];
  if Value <> '' then
    BandWidth := StrToInt(Value);

  Value := List.Values[Prefix + 'SmoothBand'];
  if Value <> '' then
    SmoothBand := SameText(Value, BoolToStr(True));
end;

procedure TWipeTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
begin
  inherited;

  if(not OmitDefaultValues) or (BandWidth <> 50) then
    List.Values[Prefix + 'BandWidth'] := IntToStr(BandWidth);

  if(not OmitDefaultValues) or
    (SmoothBand <> False) then
    List.Values[Prefix + 'SmoothBand'] := BoolToStr(SmoothBand);
end;

initialization

  TERegisterTransition(TWipeTransition);

end.
