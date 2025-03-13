unit tePixelt;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teTimed, teRender, Windows, Messages, Graphics,
  teMskWk;

type
  TPixelateTransition = class(TTimedTransitionEffect)
  private
    FBoxSize: Word;
    procedure DoPrecomputation(Data: TTETransitionData; Bmp: TBitmap);
    procedure PaintPixelate(Work: PByteArray;
      ScanLineSize, RowGap, Width, Height, Box: Integer; BGR: PDWordArray);
    procedure Paint2x2Pixelate(Work: PByteArray; ScanLineSize, RowGap, Width,
      Height: Integer; BGR: PDWordArray);
    procedure SetBoxSize(const Value: Word);
  protected
    BoxSizeToUse: Integer;

    procedure Initialize(Data: TTETransitionData; var Frames: Longint); override;
    function  CalcTotalFrames(Data: TTETransitionData): Longint;
    function  GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
    procedure ExecuteFrame(Data: TTETransitionData;
      CurrentFrame, Step, LastExecutedFrame: Longint); override;
    procedure PixelateBmp(Bitmap: TBitmap; Data: TTETransitionData;
      Box: Integer);
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    procedure Assign(Source: TPersistent); override;
    function GetDelegate(Device: TTETransitionDevice;
      const ReturnCopy: Boolean): TTransitionEffect; override;
    class function Description: String; override;
    class function GetEditor: String; override;
  published
    property Pass2Options;
    property PassSetting;
    property BoxSize: Word read FBoxSize write SetBoxSize;
    property Reversed;
  end;

implementation

uses teFuse;

type
  TPixelateData = class(TTECustomData)
  public
    BGRImg: PDWordArray;
    BGRImgRowSize: Integer;
    Pass1: Boolean;
    Pass2Frame: Integer;

    destructor Destroy; override;
  end;

{ TPixelateTransition }

constructor TPixelateTransition.Create(AOwner: TComponent);
begin
  inherited;

  FBoxSize := 50;
end;

procedure TPixelateTransition.Assign(Source: TPersistent);
begin
  if Source is TPixelateTransition
  then
  begin
    inherited;

    BoxSize := TPixelateTransition(Source).BoxSize;
  end
  else inherited;
end;

function TPixelateTransition.CalcTotalFrames(Data: TTETransitionData): Longint;
begin
  BoxSizeToUse := FBoxSize;
  if Data.Width < BoxSizeToUse then
    BoxSizeToUse := Data.Width;
  if Data.Height < BoxSizeToUse then
    BoxSizeToUse := Data.Height;
  Result := (BoxSizeToUse * 2) - 1;
end;

class function TPixelateTransition.Description: String;
begin
  Result := 'Pixelate';
end;

function TPixelateTransition.GetPixelFormat(Device: TTETransitionDevice):
  TPixelFormat;
begin
  Result := pf24bit;
end;

procedure TPixelateTransition.Initialize(Data: TTETransitionData;
  var Frames: Integer);
var
  PixelateData: TPixelateData;
begin
  inherited;

  PixelateData := TPixelateData.Create(Data);
  Data.Custom  := PixelateData;
  Frames := CalcTotalFrames(Data);
  PixelateData.Pass2Frame    := (Frames div 2) + 1;
  PixelateData.BGRImgRowSize := Data.Width * 3;
  GetMem(PixelateData.BGRImg, Data.Height * (PixelateData.BGRImgRowSize * 4));

  DoPrecomputation(Data, Data.SrcBmp);
  PixelateData.Pass1 := True;
end;

procedure TPixelateTransition.DoPrecomputation(Data: TTETransitionData;
  Bmp: TBitmap);
var
  x,
  y,
  IndexWork,
  Index: Integer;
  Work: PByteArray;
  PixelateData: TPixelateData;
begin
  PixelateData := TPixelateData(Data.Custom);
  Index := 0;
  Work  := nil;
	for y := 0 to Data.Height - 1 do
  begin
    IndexWork := 0;
    if y >= 0 then
      Work := PByteArray(PAnsiChar(Bmp.ScanLine[Data.Height-y-1]));
    for x := 0 to Data.Width - 1 do
    begin
      if(x > 0) and (y > 0)
      then
      begin
        PixelateData.BGRImg[Index] :=
          Work[IndexWork]                                         +
          PixelateData.BGRImg[Index-3                           ] +
          PixelateData.BGRImg[Index-PixelateData.BGRImgRowSize  ] -
          PixelateData.BGRImg[Index-PixelateData.BGRImgRowSize-3];
        Inc(Index);
        PixelateData.BGRImg[Index] :=
          Work[IndexWork+1]                                       +
          PixelateData.BGRImg[Index-3                           ] +
          PixelateData.BGRImg[Index-PixelateData.BGRImgRowSize  ] -
          PixelateData.BGRImg[Index-PixelateData.BGRImgRowSize-3];
        Inc(Index);
        PixelateData.BGRImg[Index] :=
          Work[IndexWork+2]                                       +
          PixelateData.BGRImg[Index-3                           ] +
          PixelateData.BGRImg[Index-PixelateData.BGRImgRowSize  ] -
          PixelateData.BGRImg[Index-PixelateData.BGRImgRowSize-3];
        Inc(Index);
        Inc(IndexWork, 3);
      end
      else
      begin
        if x = 0
        then
        begin
          if y > 0
          then
          begin
            PixelateData.BGRImg[Index] :=
              Work[IndexWork  ] +
              PixelateData.BGRImg[Index-PixelateData.BGRImgRowSize];
            Inc(Index);
            PixelateData.BGRImg[Index] :=
              Work[IndexWork+1] +
              PixelateData.BGRImg[Index-PixelateData.BGRImgRowSize];
            Inc(Index);
            PixelateData.BGRImg[Index] :=
              Work[IndexWork+2] +
              PixelateData.BGRImg[Index-PixelateData.BGRImgRowSize];
            Inc(Index);
            Inc(IndexWork, 3);
          end
          else
          begin
            PixelateData.BGRImg[Index  ] := Work[IndexWork  ];
            PixelateData.BGRImg[Index+1] := Work[IndexWork+2];
            PixelateData.BGRImg[Index+2] := Work[IndexWork+3];
            Inc(Index    , 3);
            Inc(IndexWork, 3);
          end;
        end
        else
        begin
          PixelateData.BGRImg[Index  ] :=
            Work[IndexWork  ] +
            PixelateData.BGRImg[Index-3];
          PixelateData.BGRImg[Index+1] :=
            Work[IndexWork+1] +
            PixelateData.BGRImg[Index-2];
          PixelateData.BGRImg[Index+2] :=
            Work[IndexWork+2] +
            PixelateData.BGRImg[Index-1];
          Inc(Index,     3);
          Inc(IndexWork, 3);
        end;
      end;
    end;
  end;
end;

procedure TPixelateTransition.ExecuteFrame(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Integer);
var
  PixelateData: TPixelateData;
begin
  PixelateData := TPixelateData(Data.Custom);
  if CurrentFrame < PixelateData.Pass2Frame
  then PixelateBmp(Data.Bitmap, Data, CurrentFrame + 1)
  else
  begin
    if PixelateData.Pass1 then
    begin
      PixelateData.Pass1 := False;
      DoPrecomputation(Data, Data.DstBmp);
    end;
    PixelateBmp(Data.Bitmap, Data, Data.Frames - CurrentFrame + 1);
  end;
  Data.UpdateRect := Rect(0, 0, Data.Width, Data.Height);
end;

procedure TPixelateTransition.PixelateBmp(Bitmap: TBitmap; Data: TTETransitionData;
  Box: Integer);
var
  ScanLineSize: Integer;
  Work : PAnsiChar;
  UpdParams: TTEUpdParams;
  PixelateData: TPixelateData;
begin
  PixelateData := TPixelateData(Data.Custom);
  ScanLineSize := GetBytesPerScanline(Bitmap, Bitmap.PixelFormat, 32);
  GiveMeTheUpdParams(2, UpdParams, ScanLineSize,
    Rect(0, 0, Data.Width, Data.Height), Rect(0, 0, 0, 0), Bitmap.PixelFormat);
  Work         := PAnsiChar(Bitmap.ScanLine[Data.Height-1]);

  if Box > 2
  then PaintPixelate(
         PByteArray(Work),
         ScanLineSize,
         UpdParams.GapBytes1,
         Data.Width,
         Data.Height,
         Box,
         PixelateData.BGRImg)
  else Paint2x2Pixelate(
         PByteArray(Work),
         ScanLineSize,
         UpdParams.GapBytes1,
         Data.Width,
         Data.Height,
         PixelateData.BGRImg);
end;

function TPixelateTransition.GetDelegate(Device: TTETransitionDevice;
  const ReturnCopy: Boolean): TTransitionEffect;
var
  Transition: TTransitionEffect;
begin
  Result := nil;
  if Device.IsRGB
  then Result := inherited GetDelegate(Device, ReturnCopy)
  else
  begin
    Transition := TFuseTransition.Create(nil);
    try
      Transition.Assign(Self);
      TFuseTransition(Transition).Style := 1;
      Result := Transition.GetDelegate(Device, False);
      if Result <> Transition then
        Transition.Free;
    finally
      if Result <> Transition then
        Transition.Free;
    end;
  end;
end;

procedure TPixelateTransition.SetBoxSize(const Value: Word);
begin
  if(Value >= 1) then
    FBoxSize := Value;
end;

class function TPixelateTransition.GetEditor: String;
begin
  Result := 'TPixelateTransitionEditor';
end;

function TPixelateTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiNeedOffScreenBmp,
      tetiOffScreenBmpCapable,
      tetiThreadSafe
    ];
end;

procedure TPixelateTransition.PaintPixelate(Work: PByteArray;
  ScanLineSize, RowGap, Width, Height, Box: Integer; BGR: PDWordArray);
var
  BoxPixels: Cardinal;
  WIndex,
  aux,
  ColLenght,
  ColsLenght,
  RowsLimit,
  PixelsBytes,
  ColLimit,
  ColsLimit,
  RowLimit,
  BoxLimit,
  LastColAdjust,
  NextColOffset,
  NextRowOffset,
  BytesToEnd,
  CopyRows: Integer;
  BValue,
  GValue,
  RValue: Byte;
  NotFirst,
  FirstCol,
  FirstRow: Boolean;
begin
  NotFirst      := False;
  FirstRow      := True;
  FirstCol      := True;
  WIndex        := 0;
  ColLenght     := Box * 3;
  ColsLenght    := ColLenght * (Width div Box);
  RowsLimit     := ScanLineSize * (Height div Box) * Box;
  BoxPixels     := Box * Box;
  PixelsBytes   := Width * 3;
  aux           := Width * 3 * Box;
  NextColOffset := Box * 12;
  NextRowOffset := (Box - 1) * Width * 12;
  CopyRows      := Box - 1;
  LastColAdjust := (Box - (Width mod Box)) * 12;
  BGR           := PDWordArray(PAnsiChar(BGR) + ((Box - 1) * 12));

  while WIndex < RowsLimit do // Whole bitmap
  begin
    while WIndex < RowsLimit do // Rows which are complete
    begin
      BGR       := PDWordArray(PAnsiChar(BGR) + NextRowOffset);
      RowLimit  := WIndex + PixelsBytes;
      ColsLimit := WIndex + ColsLenght;
      while WIndex < ColsLimit do // Columns which are complete
      begin
        if NotFirst
        then
        begin
          BValue :=
            (
              BGR[0]                +
              BGR[-ColLenght - aux] -
              BGR[-aux]             -
              BGR[-ColLenght]
            ) div BoxPixels;
          BGR := PDWordArray(PAnsiChar(BGR) + 4);
          GValue :=
            (
              BGR[0]                +
              BGR[-ColLenght - aux] -
              BGR[-aux]             -
              BGR[-ColLenght]
            ) div BoxPixels;
          BGR := PDWordArray(PAnsiChar(BGR) + 4);
          RValue :=
            (
              BGR[0]                +
              BGR[-ColLenght - aux] -
              BGR[-aux]             -
              BGR[-ColLenght]
            ) div BoxPixels;
          BGR := PDWordArray(PAnsiChar(BGR) + NextColOffset - 8);
        end
        else
        begin
          if FirstRow
          then
          begin
            if FirstCol
            then
            begin
              BValue := BGR[0] div BoxPixels;
              GValue := BGR[1] div BoxPixels;
              RValue := BGR[2] div BoxPixels;
              BGR := PDWordArray(PAnsiChar(BGR) + NextColOffset);
              FirstCol := False;
            end
            else
            begin
              BValue :=
                (
                  BGR[0] -
                  BGR[-ColLenght]
                ) div BoxPixels;
              GValue :=
                (
                  BGR[1] -
                  BGR[1-ColLenght]
                ) div BoxPixels;
              RValue :=
                (
                  BGR[2] -
                  BGR[2-ColLenght]
                ) div BoxPixels;
              BGR := PDWordArray(PAnsiChar(BGR) + NextColOffset);
            end;
          end
          else
          begin
            FirstCol := False;
            NotFirst := True;
            BValue :=
              (
                BGR[0] -
                BGR[-aux]
              ) div BoxPixels;
            GValue :=
              (
                BGR[1] -
                BGR[1-aux]
              ) div BoxPixels;
            RValue :=
              (
                BGR[2] -
                BGR[2-aux]
              ) div BoxPixels;
            BGR := PDWordArray(PAnsiChar(BGR) + NextColOffset);
          end;
        end;
        ColLimit := WIndex + ColLenght;
        while WIndex < ColLimit do // Column
        begin
          Work[WIndex  ] := BValue;
          Work[WIndex+1] := GValue;
          Work[WIndex+2] := RValue;
          Inc(WIndex, 3);
        end;
      end;
      if WIndex < RowLimit then
      begin // Last column (incomplete)
        BGR := PDWordArray(PAnsiChar(BGR) - LastColAdjust);
        if FirstRow
        then
        begin
          BValue :=
            (
              BGR[0] -
              BGR[-ColLenght]
            ) div BoxPixels;
          GValue :=
            (
              BGR[1] -
              BGR[1-ColLenght]
            ) div BoxPixels;
          RValue :=
            (
              BGR[2] -
              BGR[2-ColLenght]
            ) div BoxPixels;
          BGR := PDWordArray(PAnsiChar(BGR) + NextColOffset);
        end
        else
        begin
          BValue :=
            (
              BGR[0]                +
              BGR[-ColLenght - aux] -
              BGR[-aux]             -
              BGR[-ColLenght]
            ) div BoxPixels;
          BGR := PDWordArray(PAnsiChar(BGR) + 4);
          GValue :=
            (
              BGR[0]                +
              BGR[-ColLenght - aux] -
              BGR[-aux]             -
              BGR[-ColLenght]
            ) div BoxPixels;
          BGR := PDWordArray(PAnsiChar(BGR) + 4);
          RValue :=
            (
              BGR[0]                +
              BGR[-ColLenght - aux] -
              BGR[-aux]             -
              BGR[-ColLenght]
            ) div BoxPixels;
          BGR := PDWordArray(PAnsiChar(BGR) + NextColOffset - 8);
        end;
        while WIndex < RowLimit do
        begin
          Work[WIndex  ] := BValue;
          Work[WIndex+1] := GValue;
          Work[WIndex+2] := RValue;
          Inc(WIndex, 3);
        end;
      end;
      Inc(WIndex, RowGap);
      BoxLimit := WIndex + (ScanLineSize * CopyRows);
      while WIndex < BoxLimit do // The other rows in the box
      begin
        Move(Work[WIndex-ScanLineSize], Work[WIndex], ScanLineSize);
        Inc(WIndex, ScanLineSize);
      end;
      FirstCol := True;
      FirstRow := False;
      NotFirst := False;
    end;

    BytesToEnd := (ScanLineSize * Height) - RowsLimit;
    if BytesToEnd > 0 then
    begin
      Inc(RowsLimit, BytesToEnd);
      Dec(CopyRows, Box - (Height mod Box));
      BGR := PDWordArray(PAnsiChar(BGR) - (Width * 12 * (Box - (Height mod Box))));
    end;
  end;
end;

procedure TPixelateTransition.Paint2x2Pixelate(Work: PByteArray;
  ScanLineSize, RowGap, Width, Height: Integer; BGR: PDWordArray);
var
  WIndex,
  aux,
  ColsLenght,
  RowsLimit,
  PixelsBytes,
  ColsLimit,
  RowLimit,
  LastColAdjust,
  NextRowOffset: Integer;
  BValue,
  GValue,
  RValue: Byte;
  NotFirst,
  FirstCol,
  FirstRow: Boolean;
begin
  BValue        := 0;
  GValue        := 0;
  RValue        := 0;
  NotFirst      := False;
  FirstRow      := True;
  FirstCol      := True;
  WIndex        := 0;
  ColsLenght    := 6 * (Width shr 1);
  RowsLimit     := ScanLineSize * (Height shr 1) shl 1;
  PixelsBytes   := Width * 3;
  aux           := PixelsBytes shl 1;
  NextRowOffset := aux shl 1;
  LastColAdjust := (2 - (Width mod 2)) * 12;
  BGR           := PDWordArray(PAnsiChar(BGR) - 12);

  while WIndex < RowsLimit do // Rows which are complete
  begin
    BGR       := PDWordArray(PAnsiChar(BGR) + NextRowOffset);
    RowLimit  := WIndex + PixelsBytes;
    ColsLimit := WIndex + ColsLenght;
    while WIndex < ColsLimit do // Columns which are complete
    begin
      if NotFirst
      then
      begin
        BValue :=
          (
            BGR[6]     +
            BGR[-aux]  -
            BGR[6-aux] -
            BGR[0]
          ) shr 2;
        BGR := PDWordArray(PAnsiChar(BGR) + 4);
        GValue :=
          (
            BGR[6]     +
            BGR[-aux]  -
            BGR[6-aux] -
            BGR[0]
          ) shr 2;
        BGR := PDWordArray(PAnsiChar(BGR) + 4);
        RValue :=
          (
            BGR[6]     +
            BGR[-aux]  -
            BGR[6-aux] -
            BGR[0]
          ) shr 2;
        BGR := PDWordArray(PAnsiChar(BGR) + 16);
      end
      else
      begin
        if FirstRow
        then
        begin
          if FirstCol
          then
          begin
            BValue := BGR[6] shr 2;
            GValue := BGR[7] shr 2;
            RValue := BGR[8] shr 2;
            BGR := PDWordArray(PAnsiChar(BGR) + 24);
            FirstCol := False;
          end
          else
          begin
            BValue :=
              (
                BGR[6] -
                BGR[0]
              ) shr 2;
            GValue :=
              (
                BGR[7] -
                BGR[1]
              ) shr 2;
            RValue :=
              (
                BGR[8] -
                BGR[2]
              ) shr 2;
            BGR := PDWordArray(PAnsiChar(BGR) + 24);
          end;
        end
        else
        begin
          FirstCol := False;
          NotFirst := True;
          BValue :=
            (
              BGR[6] -
              BGR[6-aux]
            ) shr 2;
          GValue :=
            (
              BGR[7] -
              BGR[7-aux]
            ) shr 2;
          RValue :=
            (
              BGR[8] -
              BGR[8-aux]
            ) shr 2;
          BGR := PDWordArray(PAnsiChar(BGR) + 24);
        end;
      end;
      Work[WIndex  ] := BValue;
      Work[WIndex+3] := BValue;
      Work[WIndex+1] := GValue;
      Work[WIndex+4] := GValue;
      Work[WIndex+2] := RValue;
      Work[WIndex+5] := RValue;
      Inc(WIndex, 6);
    end;
    if WIndex < RowLimit then
    begin // Last column (incomplete)
      BGR := PDWordArray(PAnsiChar(BGR) - LastColAdjust + 24);
      Work[WIndex  ] := BValue;
      Work[WIndex+1] := GValue;
      Work[WIndex+2] := RValue;
      Inc(WIndex, 3);
    end;
    Inc(WIndex, RowGap);
    Move(Work[WIndex-ScanLineSize], Work[WIndex], ScanLineSize);
    Inc(WIndex, ScanLineSize);
    FirstCol := True;
    FirstRow := False;
    NotFirst := False;
  end;

  if(ScanLineSize * Height) > RowsLimit then
    Move(Work[WIndex-ScanLineSize], Work[WIndex], ScanLineSize);
end;

destructor TPixelateData.Destroy;
begin
  FreeMem(BGRImg);

  inherited;
end;

procedure TPixelateTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'BoxSize'];
  if Value <> '' then
    BoxSize := StrToInt(Value);
end;

procedure TPixelateTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
begin
  inherited;

  List.Values[Prefix + 'BoxSize'] := IntToStr(BoxSize);
end;

initialization

  TERegisterTransition(TPixelateTransition);
  RegisterClasses([TPixelateTransition]);

end.
