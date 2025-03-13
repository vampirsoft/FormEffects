unit teMskWk;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses Classes, SysUtils, Windows, Graphics, teRender;

{$ifndef TE_NOHLP}
type
  TApply256MaskSProc = function(Work, Dst, Src, Mask: Pointer; Size,
    UpdateRowLenght, Gap: Longint): PByteArray;
  TApply256BmpMaskSProc = function(Work, Dst, Src, Mask: Pointer;
    Size, UpdateRowLenght, Gap, Dif: Longint): PByteArray;

  // General use routines
  function CreateGrayScalePalette: HPalette;

  // 1bpp mask routines
  procedure Apply1bppMask(Work, Dst, Mask: Pointer;
    MaskWidth, ScanLineSize, MaskScanLineSize: Integer;
    PixelFormat: TPixelFormat; UpdateRect, UnUpdateRect: TRect);
  procedure DoMaskOptimization(Mask1, Mask2: TBitmap; ScanLineSize: Integer;
    UpdateRect, UnUpdateRect: TRect);
  procedure InvertMask(Mask: PDWordArray; k: Longint);

  // 8bpp mask routines
  function  GetApply256MaskSProc: PByteArray;
  procedure Apply256MaskS(Apply256MaskSProc: TApply256MaskSProc;
    Work, Dst, Src, Mask: Pointer;
    ScanLineSize, MaskScanLineSize, Width: Integer;
    UpdateRect, UnUpdateRect: TRect);
  procedure Apply256Mask(Work, Dst, Mask: Pointer;
    MaskSize, LoValue, HiValue: Longint; PixelFormat: TPixelFormat);
  procedure ApplyLayered256Mask(Work, Dst, Mask: Pointer;
    MaskSize, LoValue, HiValue: Longint; PixelFormat: TPixelFormat);
  function  GetApply256BmpMaskSProc(Levels: Integer; Add: Boolean): PByteArray;
  procedure Apply256BmpMaskS(
    Apply256BmpMaskSSubProc, Apply256BmpMaskSAddProc: TApply256BmpMaskSProc;
    Work, Dst, Src, Mask: Pointer;
    ScanLineSize, MaskScanLineSize, Width, Dif: Integer;
    UpdateRect, UnUpdateRect: TRect);
  procedure DoDecMask1(Mask: Pointer; ScanLineSize, Width, DecValue: Longint;
    DecAll:Boolean; UpdateRect, UnUpdateRect: TRect);
  procedure DoDecMask2(Mask1, Mask2: Pointer;
    ScanLineSize, Width, DecValue: Longint; DecAll:Boolean;
    UpdateRect, UnUpdateRect: TRect);

type
  TTEUpdParams = record
    Start1,           // Bytes from end
    RowLenght1,       // Pixels to work in a row
    Gap1,             // Pixels to bypass from row to row
    GapBytes1,        // Bytes to bypass from row to row (takes into account alignment)
    Lenght1,          // Total pixels from start to end
    LenghtBytes1,     // Total bytes from start to end
    Start2,           // Bytes from end
    RowLenght2,       // Pixels to work in a row
    Gap2,             // Pixels to bypass from row to row
    GapBytes2,        // Bytes to bypass from row to row
    Lenght2,          // Total pixels from start to end
    Start3,           // Bytes from end
    RowLenght3,       // Pixels to work in a row
    Gap3,             // Pixels to bypass from row to row
    GapBytes3,        // Bytes to bypass from row to row
    Lenght3,          // Total pixels from start to end
    Start4,           // Bytes from end
    RowLenght4,       // Pixels to work in a row
    Gap4,             // Pixels to bypass from row to row
    GapBytes4,        // Bytes to bypass from row to row
    Lenght4: Longint; // Total pixels from start to end
  end;

  function GiveMeTheUpdMode(Width, MinGapPercentage, PixelGrain: Longint;
    var UpdateRect, UnUpdateRect: TRect; PixelFormat: TPixelFormat): Byte;
  procedure GiveMeTheUpdParams(Mode: Byte; var UpdParams: TTEUpdParams;
    ScanLineSize: Longint; UpdateRect, UnUpdateRect: TRect;
    PixelFormat: TPixelFormat);

{$endif TE_NOHLP}

implementation

uses teBlndWk
  {$ifdef D6}
  , Types
  {$endif D6}
  ;

// General use routines

function CreateGrayScalePalette: HPalette;
var
  Pal: TMaxLogPalette;
  i: Integer;
begin
  Pal.palVersion := $300;
  Pal.palNumEntries := 256;
  for i:=0 to 255 do
  begin
    with Pal.palPalEntry[i] do
    begin
      peRed   := i;
      peGreen := i;
      peBlue  := i;
      peFlags  := 0;
    end;
  end;
  Result := CreatePalette(PLogPalette(@Pal)^);
end;

// 1 bit mask routines

procedure Apply1bppMask_2_32Rect(Work2, Dst2, Mask2: Pointer;
  i, UpdateRowLenght, UpdateGap: Longint);
var
  Work,
  Dst: PDWordArray;
  Mask: PByteArray;
  k,
  UpdateGapMask,
  UpdRowLenMask,
  Limit: Longint;
begin
  k             := i               div 8;
  UpdateGapMask := UpdateGap       div 8;
  UpdRowLenMask := UpdateRowLenght div 8;
  Work          := Work2;
  Dst           := Dst2;
  Mask          := Mask2;

  while k < 0 do
  begin
    Limit := k + UpdRowLenMask;
    while k < Limit do
    begin
      if Mask[k] <> $FF then
      begin
        if(Mask[k] or $7F) = $7F then
          Work[i  ] := Dst[i  ];
        if(Mask[k] or $BF) = $BF then
          Work[i+1] := Dst[i+1];
        if(Mask[k] or $DF) = $DF then
          Work[i+2] := Dst[i+2];
        if(Mask[k] or $EF) = $EF then
          Work[i+3] := Dst[i+3];
        if(Mask[k] or $F7) = $F7 then
          Work[i+4] := Dst[i+4];
        if(Mask[k] or $FB) = $FB then
          Work[i+5] := Dst[i+5];
        if(Mask[k] or $FD) = $FD then
          Work[i+6] := Dst[i+6];
        if(Mask[k] or $FE) = $FE then
          Work[i+7] := Dst[i+7];
      end;
      Inc(k);
      Inc(i, 8);
    end;
    Inc(k, UpdateGapMask);
    Inc(i, UpdateGap);
  end;
end;

procedure Apply1bppMask_2_24Rect(Work2, Dst2, Mask2: Pointer;
  j, UpdateRowLenght, UpdateGap: Longint);
var
  Work,
  Dst,
  Mask: PByteArray;
  i,
  k,
  Limit,
  UpdRowLenMask,
  UpdateGapMask,
  UpdateGapBmp: Longint;
begin
  k             := j               div 8;
  UpdateGapMask := UpdateGap       div 8;
  UpdRowLenMask := UpdateRowLenght div 8;
  i             := j         * 3;
  UpdateGapBmp  := UpdateGap * 3;
  Work          := Work2;
  Dst           := Dst2;
  Mask          := Mask2;

  while k < 0 do
  begin
    Limit := k + UpdRowLenMask;
    while k < Limit do
    begin
      if Mask[k] <> $FF then
      begin
        if Mask[k] or $7F = $7F then
        begin
          Work[i   ] := Dst[i   ];
          Work[i+ 1] := Dst[i+ 1];
          Work[i+ 2] := Dst[i+ 2];
        end;
        if Mask[k] or $BF = $BF then
        begin
          Work[i+ 3] := Dst[i+ 3];
          Work[i+ 4] := Dst[i+ 4];
          Work[i+ 5] := Dst[i+ 5];
        end;
        if Mask[k] or $DF = $DF then
        begin
          Work[i+ 6] := Dst[i+ 6];
          Work[i+ 7] := Dst[i+ 7];
          Work[i+ 8] := Dst[i+ 8];
        end;
        if Mask[k] or $EF = $EF then
        begin
          Work[i+ 9] := Dst[i+ 9];
          Work[i+10] := Dst[i+10];
          Work[i+11] := Dst[i+11];
        end;
        if Mask[k] or $F7 = $F7 then
        begin
          Work[i+12] := Dst[i+12];
          Work[i+13] := Dst[i+13];
          Work[i+14] := Dst[i+14];
        end;
        if Mask[k] or $FB = $FB then
        begin
          Work[i+15] := Dst[i+15];
          Work[i+16] := Dst[i+16];
          Work[i+17] := Dst[i+17];
        end;
        if Mask[k] or $FD = $FD then
        begin
          Work[i+18] := Dst[i+18];
          Work[i+19] := Dst[i+19];
          Work[i+20] := Dst[i+20];
        end;
        if Mask[k] or $FE = $FE then
        begin
          Work[i+21] := Dst[i+21];
          Work[i+22] := Dst[i+22];
          Work[i+23] := Dst[i+23];
        end;
      end;
      Inc(k);
      Inc(i, 24);
    end;
    Inc(k, UpdateGapMask);
    Inc(i, UpdateGapBmp);
  end;
end;

procedure Apply1bppMask_2_16Rect(Work2, Dst2, Mask2: Pointer;
  i, UpdateRowLenght, UpdateGap: Longint);
var
  Work,
  Dst: PWordArray;
  Mask: PByteArray;
  k,
  UpdateGapMask,
  UpdRowLenMask,
  Limit: Longint;
begin
  k             := i               div 8;
  UpdateGapMask := UpdateGap       div 8;
  UpdRowLenMask := UpdateRowLenght div 8;
  Work          := Work2;
  Dst           := Dst2;
  Mask          := Mask2;

  while k < 0 do
  begin
    Limit := k + UpdRowLenMask;
    while k < Limit do
    begin
      if Mask[k] <> $FF then
      begin
        if(Mask[k] or $7F) = $7F then Work[i  ] := Dst[i  ];
        if(Mask[k] or $BF) = $BF then Work[i+1] := Dst[i+1];
        if(Mask[k] or $DF) = $DF then Work[i+2] := Dst[i+2];
        if(Mask[k] or $EF) = $EF then Work[i+3] := Dst[i+3];
        if(Mask[k] or $F7) = $F7 then Work[i+4] := Dst[i+4];
        if(Mask[k] or $FB) = $FB then Work[i+5] := Dst[i+5];
        if(Mask[k] or $FD) = $FD then Work[i+6] := Dst[i+6];
        if(Mask[k] or $FE) = $FE then Work[i+7] := Dst[i+7];
      end;
      Inc(k);
      Inc(i, 8);
    end;
    Inc(k, UpdateGapMask);
    Inc(i, UpdateGap);
  end;
end;

procedure Apply1bppMask_2_8Rect(Work2, Dst2, Mask2: Pointer;
  i, UpdateRowLenght, UpdateGap: Longint);
var
  Work,
  Dst,
  Mask: PByteArray;
  k,
  UpdateGapMask,
  UpdRowLenMask,
  Limit: Longint;
begin
  k             := i               div 8;
  UpdateGapMask := UpdateGap       div 8;
  UpdRowLenMask := UpdateRowLenght div 8;
  Work          := Work2;
  Dst           := Dst2;
  Mask          := Mask2;

  while k < 0 do
  begin
    Limit := k + UpdRowLenMask;
    while k < Limit do
    begin
      if Mask[k] <> $FF then
      begin
        if(Mask[k] or $7F) = $7F then Work[i  ] := Dst[i  ];
        if(Mask[k] or $BF) = $BF then Work[i+1] := Dst[i+1];
        if(Mask[k] or $DF) = $DF then Work[i+2] := Dst[i+2];
        if(Mask[k] or $EF) = $EF then Work[i+3] := Dst[i+3];
        if(Mask[k] or $F7) = $F7 then Work[i+4] := Dst[i+4];
        if(Mask[k] or $FB) = $FB then Work[i+5] := Dst[i+5];
        if(Mask[k] or $FD) = $FD then Work[i+6] := Dst[i+6];
        if(Mask[k] or $FE) = $FE then Work[i+7] := Dst[i+7];
      end;
      Inc(k);
      Inc(i, 8);
    end;
    Inc(k, UpdateGapMask);
    Inc(i, UpdateGap);
  end;
end;

procedure Apply1bppMask_2_4Rect(Work2, Dst2, Mask2: Pointer;
  j, UpdateRowLenght, UpdateGap: Longint);
var
  Work,
  Dst,
  Mask: PByteArray;
  i,
  k,
  UpdateGapMask,
  UpdateGapBmp,
  UpdRowLenMask,
  Limit: Longint;
begin
  k             := j               div 8;
  UpdateGapMask := UpdateGap       div 8;
  UpdRowLenMask := UpdateRowLenght div 8;
  UpdateGapBmp  := UpdateGap       div 2;
  i             := j               div 2;
  Work          := Work2;
  Dst           := Dst2;
  Mask          := Mask2;

  while k < 0 do
  begin
    Limit := k + UpdRowLenMask;
    while k < Limit do
    begin
      if Mask[k] <> $FF then
      begin
        if Mask[k] or $3F <> $FF then
        begin
          if Mask[k] or $3F = $3F
          then Work[i  ] := Dst[i  ]
          else if Mask[k] or $7F = $7F
          then Work[i  ] := (Dst[i  ] or $0F) and (Work[i  ] or $F0)
          else Work[i  ] := (Dst[i  ] or $F0) and (Work[i  ] or $0F);
        end;
        if Mask[k] or $CF <> $FF then
        begin
          if Mask[k] or $CF = $CF
          then Work[i+1] := Dst[i+1]
          else if Mask[k] or $DF = $DF
          then Work[i+1] := (Dst[i+1] or $0F) and (Work[i+1] or $F0)
          else Work[i+1] := (Dst[i+1] or $F0) and (Work[i+1] or $0F);
        end;
        if Mask[k] or $F3 <> $FF then
        begin
          if Mask[k] or $F3 = $F3
          then Work[i+2] := Dst[i+2]
          else if Mask[k] or $F7 = $F7
          then Work[i+2] := (Dst[i+2] or $0F) and (Work[i+2] or $F0)
          else Work[i+2] := (Dst[i+2] or $F0) and (Work[i+2] or $0F);
        end;
        if Mask[k] or $FC <> $FF then
        begin
          if Mask[k] or $FC = $FC
          then Work[i+3] := Dst[i+3]
          else if Mask[k] or $FD = $FD
          then Work[i+3] := (Dst[i+3] or $0F) and (Work[i+3] or $F0)
          else Work[i+3] := (Dst[i+3] or $F0) and (Work[i+3] or $0F);
        end;
      end;
      Inc(k);
      Inc(i, 4);
    end;
    Inc(k, UpdateGapMask);
    Inc(i, UpdateGapBmp);
  end;
end;

procedure Apply1bppMask(Work, Dst, Mask: Pointer;
  MaskWidth, ScanLineSize, MaskScanLineSize: Integer;
  PixelFormat: TPixelFormat; UpdateRect, UnUpdateRect: TRect);
type
  TApply1bppMask_2 = procedure(Work, Dst, Mask: Pointer;
    i, UpdateRowLenght, UpdateGap: Longint);
var
  Apply1bppMask_2: TApply1bppMask_2;
//  PixelGrain,
  Mode: Longint;
  UpdParBmp,
  UpdParMsk: TTEUpdParams;
begin
  Mode := GiveMeTheUpdMode(MaskWidth, 0, 8{PixelGrain}, UpdateRect,
    UnUpdateRect, pf1bit);
  GiveMeTheUpdParams(Mode, UpdParBmp, ScanLineSize    , UpdateRect,
    UnUpdateRect, PixelFormat);
  GiveMeTheUpdParams(Mode, UpdParMsk, MaskScanLineSize, UpdateRect,
    UnUpdateRect, pf1bit);

  Apply1bppMask_2 := nil;
  case PixelFormat of
    pf4bit : Apply1bppMask_2 := Apply1bppMask_2_4Rect;
    pf8bit : Apply1bppMask_2 := Apply1bppMask_2_8Rect;
    pf15bit,
    pf16bit: Apply1bppMask_2 := Apply1bppMask_2_16Rect;
    pf24bit: Apply1bppMask_2 := Apply1bppMask_2_24Rect;
    pf32bit: Apply1bppMask_2 := Apply1bppMask_2_32Rect;
  end;

  if Assigned(Apply1bppMask_2) then
  begin
    case Mode of
      1,
      2:   Apply1bppMask_2(
             PAnsiChar(Work ) - UpdParBmp.Start1,
             PAnsiChar(Dst  ) - UpdParBmp.Start1,
             PAnsiChar(Mask ) - UpdParMsk.Start1,
             -UpdParMsk.Lenght1,
             UpdParMsk.RowLenght1,
             UpdParMsk.Gap1);
      3: begin
           Apply1bppMask_2(
             PAnsiChar(Work ) - UpdParBmp.Start1,
             PAnsiChar(Dst  ) - UpdParBmp.Start1,
             PAnsiChar(Mask ) - UpdParMsk.Start1,
             -UpdParMsk.Lenght1,
             UpdParMsk.RowLenght1,
             UpdParMsk.Gap1);
           Apply1bppMask_2(
             PAnsiChar(Work ) - UpdParBmp.Start2,
             PAnsiChar(Dst  ) - UpdParBmp.Start2,
             PAnsiChar(Mask ) - UpdParMsk.Start2,
             -UpdParMsk.Lenght2,
             UpdParMsk.RowLenght2,
             UpdParMsk.Gap2);
           Apply1bppMask_2(
             PAnsiChar(Work ) - UpdParBmp.Start3,
             PAnsiChar(Dst  ) - UpdParBmp.Start3,
             PAnsiChar(Mask ) - UpdParMsk.Start3,
             -UpdParMsk.Lenght3,
             UpdParMsk.RowLenght3,
             UpdParMsk.Gap3);
           Apply1bppMask_2(
             PAnsiChar(Work ) - UpdParBmp.Start4,
             PAnsiChar(Dst  ) - UpdParBmp.Start4,
             PAnsiChar(Mask ) - UpdParMsk.Start4,
             -UpdParMsk.Lenght4,
             UpdParMsk.RowLenght4,
             UpdParMsk.Gap4);
        end;
    end;
  end;
end;

procedure MaskOptimization(Mask1, Mask2: PDWordArray; k: Longint);
var
  aux: DWord;
begin
  while k < 0 do
  begin
    aux      := not Mask2[k];
    Mask2[k] := Mask2[k] and Mask1[k];
    Mask1[k] := Mask1[k] or aux;
    Inc(k);
  end;
end;

procedure MaskOptimizationRect(Mask1, Mask2: PDWordArray; k, UpdateRowLenght,
  UpdateGap: Longint);
var
  Limit: Longint;
  aux: DWord;
begin
  while k < 0 do
  begin
    Limit := k + UpdateRowLenght;
    while k < Limit do
    begin
      aux      := not Mask2[k];
      Mask2[k] := Mask2[k] and Mask1[k];
      Mask1[k] := Mask1[k] or aux;
      Inc(k);
    end;
    Inc(k, UpdateGap);
  end;
end;

procedure DoMaskOptimization(Mask1, Mask2: TBitmap; ScanLineSize: Integer;
  UpdateRect, UnUpdateRect: TRect);
var
  P1,
  P2: PDWordArray;
  UpdParams: TTEUpdParams;
  Mode: Byte;
begin
  Mode := GiveMeTheUpdMode(Mask1.Width, 0, 32, UpdateRect, UnUpdateRect, pf1bit);
  GiveMeTheUpdParams(Mode, UpdParams, ScanLineSize, UpdateRect, UnUpdateRect,
    pf1bit);

  P1 := PDWordArray(PAnsiChar(Mask1.ScanLine[0]) + ScanLineSize);
  P2 := PDWordArray(PAnsiChar(Mask2.ScanLine[0]) + ScanLineSize);

  case Mode of
    1: MaskOptimization(
         PDWordArray(PAnsiChar(P1) - UpdParams.Start1),
         PDWordArray(PAnsiChar(P2) - UpdParams.Start1),
         -UpdParams.Lenght1 div 32);
    2: MaskOptimizationRect(
         PDWordArray(PAnsiChar(P1) - UpdParams.Start1),
         PDWordArray(PAnsiChar(P2) - UpdParams.Start1),
         -UpdParams.Lenght1 div 32,
         UpdParams.RowLenght1 div 32,
         UpdParams.Gap1 div 32);
    3: begin
         MaskOptimizationRect(
           PDWordArray(PAnsiChar(P1) - UpdParams.Start1),
           PDWordArray(PAnsiChar(P2) - UpdParams.Start1),
           -UpdParams.Lenght1 div 32,
           UpdParams.RowLenght1 div 32,
           UpdParams.Gap1 div 32);
         MaskOptimizationRect(
           PDWordArray(PAnsiChar(P1) - UpdParams.Start2),
           PDWordArray(PAnsiChar(P2) - UpdParams.Start2),
           -UpdParams.Lenght2 div 32,
           UpdParams.RowLenght2 div 32,
           UpdParams.Gap2 div 32);
         MaskOptimizationRect(
           PDWordArray(PAnsiChar(P1) - UpdParams.Start3),
           PDWordArray(PAnsiChar(P2) - UpdParams.Start3),
           -UpdParams.Lenght3 div 32,
           UpdParams.RowLenght3 div 32,
           UpdParams.Gap3 div 32);
         MaskOptimizationRect(
           PDWordArray(PAnsiChar(P1) - UpdParams.Start4),
           PDWordArray(PAnsiChar(P2) - UpdParams.Start4),
           -UpdParams.Lenght4 div 32,
           UpdParams.RowLenght4 div 32,
           UpdParams.Gap4 div 32);
       end;
  end;
end;

procedure InvertMask(Mask: PDWordArray; k: Longint);
begin
  while k < 0 do
  begin
    Mask[k] := not Mask[k];
    Inc(k);
  end;
end;

// 8 bit mask routines

function GetApply256MaskSProc: PByteArray;
{$ifdef TrialLimited}begin Result := nil; end;{$else}
const
  Body1: array[1..45] of Byte = (
    $55,                     
    $89,$E5,                 
    $57,                     
    $56,                     
    $53,                     
    $83,$7D,$10,$00,         
    $0F,$84,$9B,$01,$00,$00, 
    $BF,$01,$01,$01,$01,     
    $0F,$6E,$F7,             
    $89,$D7,                 
    $89,$CE,                 
    $8B,$5D,$14,             
    $8B,$4D,$10,             
    $89,$C2,                 
    $0F,$EF,$C0,             
    $0F,$60,$F0,             
    $8B,$45,$0C);            

  Prefetch: array[1..24] of Byte = (
    $0F,$18,$84,$8E,$20,$00,$00,$00, 
    $0F,$18,$84,$8F,$20,$00,$00,$00, 
    $0F,$18,$84,$19,$20,$00,$00,$00);

  Body2: array[1..346] of Byte = (
    $0F,$6F,$54,$8E,$18,     
    $0F,$6F,$5C,$8F,$18,     
    $0F,$6F,$E2,             
    $0F,$60,$D0,             
    $0F,$6F,$EB,             
    $0F,$60,$D8,             
    $0F,$F9,$D3,             
    $0F,$6F,$0C,$0B,         
    $0F,$68,$C8,             
    $0F,$FD,$CE,             
    $0F,$6F,$F9,             
    $0F,$65,$FE,             
    $0F,$DB,$CF,             
    $0F,$6F,$F9,             
    $0F,$69,$C9,             
    $0F,$6F,$D9,             
    $0F,$61,$C9,             
    $0F,$D5,$D1,             
    $0F,$69,$DB,             
    $0F,$68,$E0,             
    $0F,$68,$E8,             
    $0F,$F9,$E5,             
    $0F,$D5,$E3,             
    $0F,$71,$D2,$08,         
    $0F,$71,$D4,$08,         
    $0F,$67,$D4,             
    $0F,$FC,$54,$8F,$18,     
    $0F,$E7,$54,$8A,$18,     
    $0F,$6F,$54,$8E,$10,     
    $0F,$6F,$5C,$8F,$10,     
    $0F,$6F,$E2,             
    $0F,$60,$D0,             
    $0F,$6F,$EB,             
    $0F,$60,$D8,             
    $0F,$F9,$D3,             
    $0F,$61,$FF,             
    $0F,$6F,$DF,             
    $0F,$61,$FF,             
    $0F,$D5,$D7,             
    $0F,$69,$DB,             
    $0F,$68,$E0,             
    $0F,$68,$E8,             
    $0F,$F9,$E5,             
    $0F,$D5,$E3,             
    $0F,$71,$D2,$08,         
    $0F,$71,$D4,$08,         
    $0F,$67,$D4,             
    $0F,$FC,$54,$8F,$10,     
    $0F,$E7,$54,$8A,$10,     
    $0F,$6F,$54,$8E,$08,     
    $0F,$6F,$5C,$8F,$08,     
    $0F,$6F,$E2,             
    $0F,$60,$D0,             
    $0F,$6F,$EB,             
    $0F,$60,$D8,             
    $0F,$F9,$D3,             
    $0F,$6F,$0C,$0B,         
    $0F,$60,$C8,             
    $0F,$FD,$CE,             
    $0F,$6F,$F9,             
    $0F,$65,$FE,             
    $0F,$DB,$CF,             
    $0F,$6F,$F9,             
    $0F,$69,$C9,             
    $0F,$6F,$D9,             
    $0F,$61,$C9,             
    $0F,$D5,$D1,             
    $0F,$69,$DB,             
    $0F,$68,$E0,             
    $0F,$68,$E8,             
    $0F,$F9,$E5,             
    $0F,$D5,$E3,             
    $0F,$71,$D2,$08,         
    $0F,$71,$D4,$08,         
    $0F,$67,$D4,             
    $0F,$FC,$54,$8F,$08,     
    $0F,$E7,$54,$8A,$08,     
    $0F,$6F,$14,$8E,         
    $0F,$6F,$1C,$8F,         
    $0F,$6F,$E2,             
    $0F,$60,$D0,             
    $0F,$6F,$EB,             
    $0F,$60,$D8,             
    $0F,$F9,$D3,             
    $0F,$61,$FF,             
    $0F,$6F,$DF,             
    $0F,$61,$FF,             
    $0F,$D5,$D7,             
    $0F,$69,$DB,             
    $0F,$68,$E0,             
    $0F,$68,$E8,             
    $0F,$F9,$E5,             
    $0F,$D5,$E3,             
    $0F,$71,$D2,$08,         
    $0F,$71,$D4,$08,         
    $0F,$67,$D4,             
    $0F,$FC,$14,$8F,         
    $0F,$E7,$14,$8A,         
    $83,$C1,$08,             
    $74,$15,                 
    $83,$E8,$08,             
    $0F,$85,$8E,$FE,$FF,$FF);

  Sfence: array[1..3] of Byte = (
    $0F,$AE,$FF);             

  Body3: array[1..20] of Byte = (
    $03,$4D,$08,             
    $0F,$85,$7F,$FE,$FF,$FF, 
    $0F,$77,                 
    $5B,                     
    $5E,                     
    $5F,                     
    $5D,                     
    $C2,$10,$00,             
    $8B,$C0);                
var
  SizeBody1,
  SizePrefetch,
  SizeBody2,
  SizeSfence,
  SizeBody3: Integer;
begin
  SizeBody1    := SizeOf(Body1);
  SizePrefetch := SizeOf(Prefetch);
  SizeBody2    := SizeOf(Body2);
  SizeSfence   := SizeOf(Sfence);
  SizeBody3    := SizeOf(Body3);

  Result := VirtualAlloc(nil,
    SizeBody1 + SizePrefetch + SizeBody2 + SizeSfence + SizeBody3,
    MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  CopyMemory(@(Result[0]), @Body1, SizeBody1);

  if TEProcessorInfo.SSE
  then
  begin
    CopyMemory(@(Result[SizeBody1]), @Prefetch, SizePrefetch);
    CopyMemory(@(Result[SizeBody1 + SizePrefetch]), @Body2, SizeBody2);
    CopyMemory(@(Result[SizeBody1 + SizePrefetch + SizeBody2]), @Sfence,
      SizeSfence);
    CopyMemory(@(Result[SizeBody1 + SizePrefetch + SizeBody2 + SizeSfence]),
      @Body3, SizeBody3);
  end
  else
  begin
    CopyMemory(@(Result[SizeBody1]), @Body2, SizeBody2);
    CopyMemory(@(Result[SizeBody1 + SizeBody2]), @Body3, SizeBody3);

    PByte(@Result[SizeBody1 +  91])^ := $7F;
    PByte(@Result[SizeBody1 + 164])^ := $7F;
    PByte(@Result[SizeBody1 + 259])^ := $7F;
    PByte(@Result[SizeBody1 + 329])^ := $7F;
    PInteger(@Result[12])^ := PInteger(@Result[12])^ - SizePrefetch - SizeSfence;
    PByte(@Result[SizeBody1 + 336])^ :=
      PByte(@Result[SizeBody1 + 336])^ - SizeSfence;
    PByte(@Result[SizeBody1 + 342])^ :=
      PInteger(@Result[SizeBody1 + 342])^ + SizePrefetch;
    PByte(@Result[SizeBody1 + SizeBody2 + 5])^ :=
      PInteger(@Result[SizeBody1 + SizeBody2 + 5])^ + SizePrefetch + SizeSfence;
  end;
end;
{$endif TrialLimited}

function GiveMeTheUpdMode(Width, MinGapPercentage, PixelGrain: Longint;
  var UpdateRect, UnUpdateRect: TRect; PixelFormat: TPixelFormat): Byte;
var
  UpdateWidth,
  UnUpdateWidth,
  Gap,
  MinGap: Longint;
begin
  UpdateRect  .Left  :=  (UpdateRect  .Left                    div PixelGrain) * PixelGrain;
  UpdateRect  .Right := ((UpdateRect  .Right + (PixelGrain-1)) div PixelGrain) * PixelGrain;
  UnUpdateRect.Left  := ((UnUpdateRect.Left  + (PixelGrain-1)) div PixelGrain) * PixelGrain;
  UnUpdateRect.Right :=  (UnUpdateRect.Right                   div PixelGrain) * PixelGrain;

  UpdateWidth   := UpdateRect  .Right - UpdateRect  .Left;
  UnUpdateWidth := UnUpdateRect.Right - UnUpdateRect.Left;
  if PixelFormat = pf1bit then
  begin
    UpdateWidth   := UpdateWidth   div 8;
    UnUpdateWidth := UnUpdateWidth div 8;
    Width := Width div 8;
  end;

  // Total gap
  MinGap := (Width * MinGapPercentage) div 100;
  Gap    := Width - UpdateWidth + UnUpdateWidth;

  if Gap <= MinGap
  then Result := 1 // Full scanline update
  else
  begin
    // UnUpdate gap
    MinGap := (UpdateWidth * MinGapPercentage) div 100;
    Gap    := UnUpdateWidth;
    if Gap <= MinGap
    then Result := 2 // Single rect update
    else Result := 3 // Four rects update
  end;
end;

procedure GiveMeTheUpdParams(Mode: Byte; var UpdParams: TTEUpdParams;
  ScanLineSize: Longint; UpdateRect, UnUpdateRect: TRect;
  PixelFormat: TPixelFormat);
const
  ByteCounts: array [pf1Bit..pf32Bit] of Single = (1/8, 1/2, 1, 2, 2, 3, 4);
var
  BytesPerPixel: Single;
begin
  BytesPerPixel := ByteCounts[PixelFormat];
  case Mode of
    1: with UpdParams do // All pixels
         begin
           Start1     := UpdateRect.Top * ScanLineSize;
           Gap1       := 0;
           Lenght1    :=
             (UpdateRect.Bottom - UpdateRect.Top) *
             Round(ScanLineSize / BytesPerPixel);
           if Lenght1 < 0 then
             Lenght1 := 0;
           RowLenght1 := Lenght1;
           GapBytes1  := ScanLineSize - Round(RowLenght1 * BytesPerPixel);
         end;
    2: with UpdParams do // All pixels within a rect
         begin
           Start1     :=
             (UpdateRect.Top * ScanLineSize) +
             (ScanLineSize - Round(UpdateRect.Right * BytesPerPixel));
           RowLenght1 := UpdateRect.Right - UpdateRect.Left;
           Gap1       := Round(ScanLineSize / BytesPerPixel) - RowLenght1;
LenghtBytes1 :=
  (UpdateRect.Bottom * ScanLineSize) - Start1 - Round((UpdateRect.Left * BytesPerPixel));
           Lenght1    :=
             Round(((UpdateRect.Bottom * ScanLineSize) - Start1) / BytesPerPixel) -
             UpdateRect.Left;
           if(Lenght1 < 0) or (RowLenght1 <= 0) then
             Lenght1 := 0;
           GapBytes1  := ScanLineSize - Round(RowLenght1 * BytesPerPixel);
         end;
    3: with UpdParams do // Pixels within a rect, but not within a subrect
         begin
           Start1     :=
             (UpdateRect.Top * ScanLineSize) +
             (ScanLineSize - Round(UpdateRect.Right * BytesPerPixel));
           RowLenght1 := UpdateRect.Right - UpdateRect.Left;
           Gap1       := Round(ScanLineSize / BytesPerPixel) - RowLenght1;
           Lenght1    :=
             Round(((UnUpdateRect.Top * ScanLineSize) - Start1) / BytesPerPixel) -
             UpdateRect.Left;
           GapBytes1  := ScanLineSize - Round(RowLenght1 * BytesPerPixel);

           Start2     := Start1 + Round((Lenght1 + Gap1) * BytesPerPixel);
           RowLenght2 := UpdateRect.Right - UnUpdateRect.Right;
           Gap2       := Round(ScanLineSize / BytesPerPixel) - RowLenght2;
           Lenght2    :=
             Round(((UnUpdateRect.Bottom * ScanLineSize) - Start2) / BytesPerPixel) -
             UnUpdateRect.Right;
           GapBytes2  := ScanLineSize - Round(RowLenght2 * BytesPerPixel);

           Start3     :=
             Start2 +
             Round((RowLenght2 + (UnUpdateRect.Right - UnUpdateRect.Left)) *
             BytesPerPixel);
           RowLenght3 := UnUpdateRect.Left - UpdateRect.Left;
           Gap3       := Round(ScanLineSize / BytesPerPixel) - RowLenght3;
           Lenght3    :=
             Round(((UnUpdateRect.Bottom * ScanLineSize) - Start3) / BytesPerPixel) -
             UpdateRect.Left;
           GapBytes3  := ScanLineSize - Round(RowLenght3 * BytesPerPixel);

           Start4     :=
             (UnUpdateRect.Bottom * ScanLineSize) +
             (ScanLineSize - Round(UpdateRect.Right * BytesPerPixel));
           RowLenght4 := UpdateRect.Right - UpdateRect.Left;
           Gap4       := Round(ScanLineSize / BytesPerPixel) - RowLenght4;
           Lenght4    :=
             Round(((UpdateRect.Bottom * ScanLineSize) - Start4) / BytesPerPixel) -
             UpdateRect.Left;
           if(Lenght1 < 0) or (RowLenght1 <= 0) then
             Lenght1 := 0;
           if(Lenght2 < 0) or (RowLenght2 <= 0) then
             Lenght2 := 0;
           if(Lenght3 < 0) or (RowLenght3 <= 0) then
             Lenght3 := 0;
           if(Lenght4 < 0) or (RowLenght4 <= 0) then
             Lenght4 := 0;
           GapBytes4  := ScanLineSize - Round(RowLenght4 * BytesPerPixel);
         end;
  end;
end;

procedure Apply256MaskS(Apply256MaskSProc: TApply256MaskSProc;
  Work, Dst, Src, Mask: Pointer;
  ScanLineSize, MaskScanLineSize, Width: Integer;
  UpdateRect, UnUpdateRect: TRect);
var
  Mode: Longint;
  UpdParBmp,
  UpdParMsk: TTEUpdParams;
begin
  Mode := GiveMeTheUpdMode(Width, 0, 8, UpdateRect, UnUpdateRect, pf8bit);
  GiveMeTheUpdParams(Mode, UpdParBmp, ScanLineSize, UpdateRect, UnUpdateRect,
    pf32bit);
  GiveMeTheUpdParams(Mode, UpdParMsk, MaskScanLineSize, UpdateRect,
    UnUpdateRect, pf8bit);

  case Mode of
    1,
    2:
      begin
        Apply256MaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start1,
          PAnsiChar(Dst ) - UpdParBmp.Start1,
          PAnsiChar(Src ) - UpdParBmp.Start1,
          PAnsiChar(Mask) - UpdParMsk.Start1,
          -UpdParMsk.Lenght1,
          UpdParMsk.RowLenght1,
          UpdParMsk.Gap1);
      end;
    3:
      begin
        Apply256MaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start1,
          PAnsiChar(Dst ) - UpdParBmp.Start1,
          PAnsiChar(Src ) - UpdParBmp.Start1,
          PAnsiChar(Mask) - UpdParMsk.Start1,
          -UpdParMsk.Lenght1,
          UpdParMsk.RowLenght1,
          UpdParMsk.Gap1);
        Apply256MaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start2,
          PAnsiChar(Dst ) - UpdParBmp.Start2,
          PAnsiChar(Src ) - UpdParBmp.Start2,
          PAnsiChar(Mask) - UpdParMsk.Start2,
          -UpdParMsk.Lenght2,
          UpdParMsk.RowLenght2,
          UpdParMsk.Gap2);
        Apply256MaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start3,
          PAnsiChar(Dst ) - UpdParBmp.Start3,
          PAnsiChar(Src ) - UpdParBmp.Start3,
          PAnsiChar(Mask) - UpdParMsk.Start3,
          -UpdParMsk.Lenght3,
          UpdParMsk.RowLenght3,
          UpdParMsk.Gap3);
        Apply256MaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start4,
          PAnsiChar(Dst ) - UpdParBmp.Start4,
          PAnsiChar(Src ) - UpdParBmp.Start4,
          PAnsiChar(Mask) - UpdParMsk.Start4,
          -UpdParMsk.Lenght4,
          UpdParMsk.RowLenght4,
          UpdParMsk.Gap4);
      end;
  end;
end;

procedure Apply256Mask_32(Work, Dst: PDWordArray; Mask: PByteArray;
  k, LoValue, HiValue: Longint);
begin
  while k < 0 do
  begin
    if (Mask[k  ] >= LoValue) and (Mask[k  ] <= HiValue) then
      Work[k  ] := Dst[k  ];
    if (Mask[k+1] >= LoValue) and (Mask[k+1] <= HiValue) then
      Work[k+1] := Dst[k+1];
    if (Mask[k+2] >= LoValue) and (Mask[k+2] <= HiValue) then
      Work[k+2] := Dst[k+2];
    if (Mask[k+3] >= LoValue) and (Mask[k+3] <= HiValue) then
      Work[k+3] := Dst[k+3];
    Inc(k, 4);
  end;
end;

procedure Apply256Mask_24(Work, Dst, Mask: PByteArray;
  k, LoValue, HiValue: Longint);
var
  i: Longint;
begin
  i := k * 3;

  while k < 0 do
  begin
    if(Mask[k  ] >= LoValue) and (Mask[k  ] <= HiValue) then
    begin
      Work[i   ] := Dst[i   ];
      Work[i+ 1] := Dst[i+ 1];
      Work[i+ 2] := Dst[i+ 2];
    end;
    if(Mask[k+1] >= LoValue) and (Mask[k+1] <= HiValue) then
    begin
      Work[i+ 3] := Dst[i+ 3];
      Work[i+ 4] := Dst[i+ 4];
      Work[i+ 5] := Dst[i+ 5];
    end;
    if(Mask[k+2] >= LoValue) and (Mask[k+2] <= HiValue) then
    begin
      Work[i+ 6] := Dst[i+ 6];
      Work[i+ 7] := Dst[i+ 7];
      Work[i+ 8] := Dst[i+ 8];
    end;
    if(Mask[k+3] >= LoValue) and (Mask[k+3] <= HiValue) then
    begin
      Work[i+ 9] := Dst[i+ 9];
      Work[i+10] := Dst[i+10];
      Work[i+11] := Dst[i+11];
    end;
    Inc(i, 12);
    Inc(k, 4);
  end;
end;

procedure Apply256Mask_16(Work, Dst: PWordArray; Mask: PByteArray;
  k, LoValue, HiValue: Longint);
begin
  while k < 0 do
  begin
    if (Mask[k  ] >= LoValue) and (Mask[k  ] <= HiValue) then
      Work[k  ] := Dst[k  ];
    if (Mask[k+1] >= LoValue) and (Mask[k+1] <= HiValue) then
      Work[k+1] := Dst[k+1];
    if (Mask[k+2] >= LoValue) and (Mask[k+2] <= HiValue) then
      Work[k+2] := Dst[k+2];
    if (Mask[k+3] >= LoValue) and (Mask[k+3] <= HiValue) then
      Work[k+3] := Dst[k+3];
    Inc(k, 4);
  end;
end;

procedure Apply256Mask_8(Work, Dst, Mask: PByteArray;
  k, LoValue, HiValue: Longint);
begin
  while k < 0 do
  begin
    if (Mask[k  ] >= LoValue) and (Mask[k  ] <= HiValue) then
      Work[k  ] := Dst[k  ];
    if (Mask[k+1] >= LoValue) and (Mask[k+1] <= HiValue) then
      Work[k+1] := Dst[k+1];
    if (Mask[k+2] >= LoValue) and (Mask[k+2] <= HiValue) then
      Work[k+2] := Dst[k+2];
    if (Mask[k+3] >= LoValue) and (Mask[k+3] <= HiValue) then
      Work[k+3] := Dst[k+3];
    Inc(k, 4);
  end;
end;

procedure Apply256Mask_4(Work, Dst, Mask: PByteArray;
  k, LoValue, HiValue: Longint);
var
  i: Longint;
begin
  i := k div 2;

  while k < 0 do
  begin
    if(Mask[k] >= LoValue) and (Mask[k] <= HiValue)
    then
    begin
      if(Mask[k+1] >= LoValue) and (Mask[k+1] <= HiValue)
      then Work[i] := Dst[i]
      else Work[i] := (Work[i] and $0F) or (Dst[i] and $F0);
    end
    else
    begin
      if(Mask[k+1] >= LoValue) and (Mask[k+1] <= HiValue) then
        Work[i] := (Work[i] and $F0) or (Dst[i] and $0F);
    end;
    Inc(i);
    Inc(k, 2);
  end;
end;

procedure Apply256Mask(Work, Dst, Mask: Pointer;
  MaskSize, LoValue, HiValue: Longint; PixelFormat: TPixelFormat);
begin
  case PixelFormat of
    pf4bit : Apply256Mask_4 (Work, Dst, Mask, -MaskSize, LoValue, HiValue);
    pf8bit : Apply256Mask_8 (Work, Dst, Mask, -MaskSize, LoValue, HiValue);
    pf15bit,
    pf16bit: Apply256Mask_16(Work, Dst, Mask, -MaskSize, LoValue, HiValue);
    pf24bit: Apply256Mask_24(Work, Dst, Mask, -MaskSize, LoValue, HiValue);
    pf32bit: Apply256Mask_32(Work, Dst, Mask, -MaskSize, LoValue, HiValue);
  end;
end;

procedure ApplyLayered256Mask(Work, Dst, Mask: Pointer;
  MaskSize, LoValue, HiValue: Longint; PixelFormat: TPixelFormat);

  procedure ApplyLayered256Mask_32(Work, Dst: PDWordArray; Mask: PByteArray;
    k, LoValue, HiValue: Longint);
  begin
    while k < 0 do
    begin
      if (Mask[k  ] >= LoValue) and (Mask[k  ] <= HiValue) then
        Work[k  ] := Dst[k  ];
      if (Mask[k+1] >= LoValue) and (Mask[k+1] <= HiValue) then
        Work[k+1] := Dst[k+1];
      if (Mask[k+2] >= LoValue) and (Mask[k+2] <= HiValue) then
        Work[k+2] := Dst[k+2];
      if (Mask[k+3] >= LoValue) and (Mask[k+3] <= HiValue) then
        Work[k+3] := Dst[k+3];
      Inc(k, 4);
    end;
  end;

begin
  ApplyLayered256Mask_32(Work, Dst, Mask, -MaskSize, LoValue, HiValue);
end;

procedure DecMask1Value(Work: PByteArray; k, Value: Longint);
begin
  while k < 0 do
  begin
    if Work[k] <> $FF then
    begin
      if Work[k] <= Value
      then Work[k] := 0
      else Work[k] := Work[k] - Value;
    end;
    if Work[k+1] <> $FF then
    begin
      if Work[k+1] <= Value
      then Work[k+1] := 0
      else Work[k+1] := Work[k+1] - Value;
    end;
    if Work[k+2] <> $FF then
    begin
      if Work[k+2] <= Value
      then Work[k+2] := 0
      else Work[k+2] := Work[k+2] - Value;
    end;
    if Work[k+3] <> $FF then
    begin
      if Work[k+3] <= Value
      then Work[k+3] := 0
      else Work[k+3] := Work[k+3] - Value;
    end;
    Inc(k, 4);
  end;
end;

procedure DecMask1ValueRect(Work: PByteArray; k, Value, UpdateRowLenght,
  UpdateGap: Longint);
var
  Limit: Longint;
begin
  while k < 0 do
  begin
    Limit := k + UpdateRowLenght;
    while k < Limit do
    begin
      if Work[k] <> $FF then
      begin
        if Work[k] <= Value
        then Work[k] := 0
        else Work[k] := Work[k] - Value;
      end;
      Inc(k);
    end;
    Inc(k, UpdateGap);
  end;
end;

procedure DecMask1(Work: PByteArray; k: Longint);
begin
  while k < 0 do
  begin
    if Work[k] <> $FF then
      Work[k  ] := 0;
    if Work[k+1] <> $FF then
      Work[k+1] := 0;
    if Work[k+2] <> $FF then
      Work[k+2] := 0;
    if Work[k+3] <> $FF then
      Work[k+3] := 0;
    Inc(k, 4);
  end;
end;

procedure DecMask1Rect(Work: PByteArray; k, UpdateRowLenght, UpdateGap: Longint);
var
  Limit: Longint;
begin
  while k < 0 do
  begin
    Limit := k + UpdateRowLenght;
    while k < Limit do
    begin
      if Work[k] <> $FF then
        Work[k] := 0;
      Inc(k);
    end;
    Inc(k, UpdateGap);
  end;
end;

procedure DoDecMask1(Mask: Pointer; ScanLineSize, Width, DecValue: Longint;
  DecAll:Boolean; UpdateRect, UnUpdateRect: TRect);
var
  UpdParams: TTEUpdParams;
  Mode: Longint;
begin
  Mode := GiveMeTheUpdMode(Width, 0, 8, UpdateRect, UnUpdateRect, pf8bit);
  GiveMeTheUpdParams(Mode, UpdParams, ScanLineSize, UpdateRect, UnUpdateRect,
    pf8bit);

  case Mode of
    1: if DecAll
       then DecMask1(
              PByteArray(PAnsiChar(Mask) - UpdParams.Start1),
              -UpdParams.Lenght1)
       else DecMask1Value(
              PByteArray(PAnsiChar(Mask) - UpdParams.Start1),
              -UpdParams.Lenght1,
              DecValue);
    2: if DecAll
       then DecMask1Rect(
              PByteArray(PAnsiChar(Mask) - UpdParams.Start1),
              -UpdParams.Lenght1,
              UpdParams.RowLenght1,
              UpdParams.Gap1)
       else DecMask1ValueRect(
              PByteArray(PAnsiChar(Mask) - UpdParams.Start1),
              -UpdParams.Lenght1,
              DecValue,
              UpdParams.RowLenght1,
              UpdParams.Gap1);
    3: if DecAll
       then
       begin
         DecMask1Rect(
           PByteArray(PAnsiChar(Mask) - UpdParams.Start1),
           -UpdParams.Lenght1,
           UpdParams.RowLenght1,
           UpdParams.Gap1);
         DecMask1Rect(
           PByteArray(PAnsiChar(Mask) - UpdParams.Start2),
           -UpdParams.Lenght2,
           UpdParams.RowLenght2,
           UpdParams.Gap2);
         DecMask1Rect(
           PByteArray(PAnsiChar(Mask) - UpdParams.Start3),
           -UpdParams.Lenght3,
           UpdParams.RowLenght3,
           UpdParams.Gap3);
         DecMask1Rect(
           PByteArray(PAnsiChar(Mask) - UpdParams.Start4),
           -UpdParams.Lenght4,
           UpdParams.RowLenght4,
           UpdParams.Gap4);
       end
       else
       begin
         DecMask1ValueRect(
           PByteArray(PAnsiChar(Mask) - UpdParams.Start1),
           -UpdParams.Lenght1,
           DecValue,
           UpdParams.RowLenght1,
           UpdParams.Gap1);
         DecMask1ValueRect(
           PByteArray(PAnsiChar(Mask) - UpdParams.Start2),
           -UpdParams.Lenght2,
           DecValue,
           UpdParams.RowLenght2,
           UpdParams.Gap2);
         DecMask1ValueRect(
           PByteArray(PAnsiChar(Mask) - UpdParams.Start3),
           -UpdParams.Lenght3,
           DecValue,
           UpdParams.RowLenght3,
           UpdParams.Gap3);
         DecMask1ValueRect(
           PByteArray(PAnsiChar(Mask) - UpdParams.Start4),
           -UpdParams.Lenght4,
           DecValue,
           UpdParams.RowLenght4,
           UpdParams.Gap4);
        end;
  end;
end;

procedure DecMask2Value(Work, Mask: PByteArray; k, Value: Longint);
begin
  while k < 0 do
  begin
    if Mask[k  ] <> $FF then
      if Mask[k  ] <= Value
      then Work[k] := 0
      else Work[k  ] := Mask[k  ] - Value;
    if Mask[k+1] <> $FF then
      if Mask[k+1] <= Value
      then Work[k+1] := 0
      else Work[k+1] := Mask[k+1] - Value;
    if Mask[k+2] <> $FF then
      if Mask[k+2] <= Value
      then Work[k+2] := 0
      else Work[k+2] := Mask[k+2] - Value;
    if Mask[k+3] <> $FF then
      if Mask[k+3] <= Value
      then Work[k+3] := 0
      else Work[k+3] := Mask[k+3] - Value;
    Inc(k, 4);
  end;
end;

procedure DecMask2ValueRect(Work, Mask: PByteArray; k, Value, UpdateRowLenght,
  UpdateGap: Longint);
var
  Limit: Longint;
begin
  while k < 0 do
  begin
    Limit := k + UpdateRowLenght;
    while k < Limit do
    begin
      if(Mask[k] <> $FF) then
      begin
        if Mask[k] <= Value
        then Work[k] := 0
        else Work[k] := Mask[k] - Value;
      end;
      Inc(k);
    end;
    Inc(k, UpdateGap);
  end;
end;

procedure DecMask2(Work, Mask: PByteArray; k: Longint);
begin
  while k < 0 do
  begin
    if Mask[k] <> $FF then
      Work[k] := 0;
    if Mask[k+1] <> $FF then
      Work[k+1] := 0;
    if Mask[k+2] <> $FF then
      Work[k+2] := 0;
    if Mask[k+3] <> $FF then
      Work[k+3] := 0;
    Inc(k, 4);
  end;
end;

procedure DecMask2Rect(Work, Mask: PByteArray; k, UpdateRowLenght,
  UpdateGap: Longint);
var
  Limit: Longint;
begin
  while k < 0 do
  begin
    Limit := k + UpdateRowLenght;
    while k < Limit do
    begin
      if Mask[k] <> $FF then
        Work[k] := 0;
      Inc(k);
    end;
    Inc(k, UpdateGap);
  end;
end;

procedure DoDecMask2(Mask1, Mask2: Pointer;
  ScanLineSize, Width, DecValue: Longint; DecAll:Boolean;
  UpdateRect, UnUpdateRect: TRect);
var
  UpdParams: TTEUpdParams;
  Mode: Longint;
begin
  Mode := GiveMeTheUpdMode(Width, 0, 8, UpdateRect, UnUpdateRect, pf8bit);
  GiveMeTheUpdParams(Mode, UpdParams, ScanLineSize, UpdateRect, UnUpdateRect,
    pf8bit);

  case Mode of
    1: if DecAll
       then DecMask2(
              PByteArray(PAnsiChar(Mask1) - UpdParams.Start1),
              PByteArray(PAnsiChar(Mask2) - UpdParams.Start1),
              -UpdParams.Lenght1)
       else DecMask2Value(
              PByteArray(PAnsiChar(Mask1) - UpdParams.Start1),
              PByteArray(PAnsiChar(Mask2) - UpdParams.Start1),
              -UpdParams.Lenght1,
              DecValue);
    2: if DecAll
       then DecMask2Rect(
              PByteArray(PAnsiChar(Mask1) - UpdParams.Start1),
              PByteArray(PAnsiChar(Mask2) - UpdParams.Start1),
              -UpdParams.Lenght1,
              UpdParams.RowLenght1,
              UpdParams.Gap1)
       else DecMask2ValueRect(
              PByteArray(PAnsiChar(Mask1) - UpdParams.Start1),
              PByteArray(PAnsiChar(Mask2) - UpdParams.Start1),
              -UpdParams.Lenght1,
              DecValue,
              UpdParams.RowLenght1,
              UpdParams.Gap1);
    3: if DecAll
       then
       begin
         DecMask2Rect(
           PByteArray(PAnsiChar(Mask1) - UpdParams.Start1),
           PByteArray(PAnsiChar(Mask2) - UpdParams.Start1),
           -UpdParams.Lenght1,
           UpdParams.RowLenght1,
           UpdParams.Gap1);
         DecMask2Rect(
           PByteArray(PAnsiChar(Mask1) - UpdParams.Start2),
           PByteArray(PAnsiChar(Mask2) - UpdParams.Start2),
           -UpdParams.Lenght2,
           UpdParams.RowLenght2,
           UpdParams.Gap2);
         DecMask2Rect(
           PByteArray(PAnsiChar(Mask1) - UpdParams.Start3),
           PByteArray(PAnsiChar(Mask2) - UpdParams.Start3),
           -UpdParams.Lenght3,
           UpdParams.RowLenght3,
           UpdParams.Gap3);
         DecMask2Rect(
           PByteArray(PAnsiChar(Mask1) - UpdParams.Start4),
           PByteArray(PAnsiChar(Mask2) - UpdParams.Start4),
           -UpdParams.Lenght4,
           UpdParams.RowLenght4,
           UpdParams.Gap4);
       end
       else
       begin
         DecMask2ValueRect(
           PByteArray(PAnsiChar(Mask1) - UpdParams.Start1),
           PByteArray(PAnsiChar(Mask2) - UpdParams.Start1),
           -UpdParams.Lenght1,
           DecValue,
           UpdParams.RowLenght1,
           UpdParams.Gap1);
         DecMask2ValueRect(
           PByteArray(PAnsiChar(Mask1) - UpdParams.Start2),
           PByteArray(PAnsiChar(Mask2) - UpdParams.Start2),
           -UpdParams.Lenght2,
           DecValue,
           UpdParams.RowLenght2,
           UpdParams.Gap2);
         DecMask2ValueRect(
           PByteArray(PAnsiChar(Mask1) - UpdParams.Start3),
           PByteArray(PAnsiChar(Mask2) - UpdParams.Start3),
           -UpdParams.Lenght3,
           DecValue,
           UpdParams.RowLenght3,
           UpdParams.Gap3);
         DecMask2ValueRect(
           PByteArray(PAnsiChar(Mask1) - UpdParams.Start4),
           PByteArray(PAnsiChar(Mask2) - UpdParams.Start4),
           -UpdParams.Lenght4,
           DecValue,
           UpdParams.RowLenght4,
           UpdParams.Gap4);
        end;
  end;
end;

function GetApply256BmpMaskSProc(Levels: Integer; Add: Boolean): PByteArray;
{$ifdef TrialLimited}begin Result := nil; end;{$else}
const
  Body1: array[1..72] of Byte = (
    $55,                             
    $8B,$EC,                         
    $57,                             
    $56,                             
    $53,                             
    $83,$7D,$14,$00,                 
    $0F,$84,$B2,$01,$00,$00,         
    $BF,$FF,$00,$00,$00,             
    $83,$EF,$10,                     
    $0F,$6E,$F7,                     
    $0F,$60,$F6,                     
    $0F,$61,$F6,                     
    $0F,$62,$F6,                     
    $8B,$7D,$08,                     
    $0F,$6E,$FF,                     
    $0F,$60,$FF,                     
    $0F,$61,$FF,                     
    $0F,$62,$FF,                     
    $89,$D6,                         
    $89,$CF,                         
    $8B,$5D,$18,                     
    $8B,$4D,$14,                     
    $89,$C2,                         
    $0F,$EF,$C0,                     
    $0F,$60,$F6,                     
    $8B,$45,$10);                    

  Prefetch: array[1..15] of Byte = (
    $0F,$18,$44,$8E,$20,             
    $0F,$18,$44,$8F,$20,             
    $0F,$18,$44,$19,$20);            

  Body2: array[1..351] of Byte = (
    $0F,$6F,$0C,$0B,                 
    $0F,$D8,$CF,                     
    $0F,$DC,$CE,                     
    $0F,$D8,$CE,                     
    $0F,$6F,$E9,                     
    $0F,$6F,$54,$8E,$18,             
    $0F,$6F,$5C,$8F,$18,             
    $0F,$6F,$E2,                     
    $0F,$60,$D0,                     
    $0F,$60,$D8,                     
    $0F,$F9,$D3,                     
    $0F,$68,$C8,                     
    $0F,$71,$F1,$04,                 
    $0F,$69,$C9,                     
    $0F,$6F,$D9,                     
    $0F,$61,$C9,                     
    $0F,$D5,$D1,                     
    $0F,$69,$DB,                     
    $0F,$6F,$4C,$8F,$18,             
    $0F,$68,$E0,                     
    $0F,$68,$C8,                     
    $0F,$F9,$E1,                     
    $0F,$D5,$E3,                     
    $0F,$71,$D2,$08,                 
    $0F,$71,$D4,$08,                 
    $0F,$67,$D4,                     
    $0F,$FC,$54,$8F,$18,             
    $0F,$E7,$54,$8A,$18,             
    $0F,$6F,$54,$8E,$10,             
    $0F,$6F,$5C,$8F,$10,             
    $0F,$6F,$E2,                     
    $0F,$60,$D0,                     
    $0F,$60,$D8,                     
    $0F,$F9,$D3,                     
    $0F,$6F,$CD,                     
    $0F,$68,$C8,                     
    $0F,$71,$F1,$04,                 
    $0F,$61,$C9,                     
    $0F,$6F,$D9,                     
    $0F,$61,$C9,                     
    $0F,$D5,$D1,                     
    $0F,$69,$DB,                     
    $0F,$6F,$4C,$8F,$10,             
    $0F,$68,$E0,                     
    $0F,$68,$C8,                     
    $0F,$F9,$E1,                     
    $0F,$D5,$E3,                     
    $0F,$71,$D2,$08,                 
    $0F,$71,$D4,$08,                 
    $0F,$67,$D4,                     
    $0F,$FC,$54,$8F,$10,             
    $0F,$E7,$54,$8A,$10,             
    $0F,$6F,$54,$8E,$08,             
    $0F,$6F,$5C,$8F,$08,             
    $0F,$6F,$E2,                     
    $0F,$60,$D0,                     
    $0F,$60,$D8,                     
    $0F,$F9,$D3,                     
    $0F,$60,$E8,                     
    $0F,$71,$F5,$04,                 
    $0F,$6F,$CD,                     
    $0F,$69,$C9,                     
    $0F,$6F,$D9,                     
    $0F,$61,$C9,                     
    $0F,$D5,$D1,                     
    $0F,$69,$DB,                     
    $0F,$6F,$4C,$8F,$08,             
    $0F,$68,$E0,                     
    $0F,$68,$C8,                     
    $0F,$F9,$E1,                     
    $0F,$D5,$E3,                     
    $0F,$71,$D2,$08,                 
    $0F,$71,$D4,$08,                 
    $0F,$67,$D4,                     
    $0F,$FC,$54,$8F,$08,             
    $0F,$E7,$54,$8A,$08,             
    $0F,$6F,$14,$8E,                 
    $0F,$6F,$1C,$8F,                 
    $0F,$6F,$E2,                     
    $0F,$6F,$CB,                     
    $0F,$60,$D0,                     
    $0F,$60,$D8,                     
    $0F,$F9,$D3,                     
    $0F,$61,$ED,                     
    $0F,$6F,$DD,                     
    $0F,$61,$ED,                     
    $0F,$D5,$D5,                     
    $0F,$69,$DB,                     
    $0F,$68,$E0,                     
    $0F,$68,$C8,                     
    $0F,$F9,$E1,                     
    $0F,$D5,$E3,                     
    $0F,$71,$D2,$08,                 
    $0F,$71,$D4,$08,                 
    $0F,$67,$D4,                     
    $0F,$FC,$14,$8F,                 
    $0F,$E7,$14,$8A,                 
    $83,$C1,$08,                     
    $74,$15,                         
    $83,$E8,$08,                     
    $0F,$85,$92,$FE,$FF,$FF);        

  Sfence: array[1..3] of Byte = (
    $0F,$AE,$FF);             

  Body3: array[1..19] of Byte = (
    $03,$4D,$08,             
    $0F,$85,$83,$FE,$FF,$FF, 
    $0F,$77,                 
    $5B,                     
    $5E,                     
    $5F,                     
    $5D,                     
    $C2,$14,$00,             
    $90);                    
var
  SizeBody1,
  SizePrefetch,
  SizeBody2,
  SizeSfence,
  SizeBody3: Integer;
  Shift: Byte;
begin
  SizeBody1    := SizeOf(Body1);
  SizePrefetch := SizeOf(Prefetch);
  SizeBody2    := SizeOf(Body2);
  SizeSfence   := SizeOf(Sfence);
  SizeBody3    := SizeOf(Body3);

  Result := VirtualAlloc(nil,
    SizeBody1 + SizePrefetch + SizeBody2 + SizeSfence + SizeBody3,
    MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  CopyMemory(@(Result[0]), @Body1, SizeBody1);

  if TEProcessorInfo.SSE
  then
  begin
    CopyMemory(@(Result[SizeBody1]), @Prefetch, SizePrefetch);
    CopyMemory(@(Result[SizeBody1 + SizePrefetch]), @Body2, SizeBody2);
    CopyMemory(@(Result[SizeBody1 + SizePrefetch + SizeBody2]), @Sfence,
      SizeSfence);
    CopyMemory(@(Result[SizeBody1 + SizePrefetch + SizeBody2 + SizeSfence]),
      @Body3, SizeBody3);
  end
  else
  begin
    CopyMemory(@(Result[SizeBody1]), @Body2, SizeBody2);
    CopyMemory(@(Result[SizeBody1 + SizeBody2]), @Body3, SizeBody3);

    PByte(@Result[SizeBody1 +  94])^ := $7F;
    PByte(@Result[SizeBody1 + 179])^ := $7F;
    PByte(@Result[SizeBody1 + 264])^ := $7F;
    PByte(@Result[SizeBody1 + 334])^ := $7F;
    PInteger(@Result[12])^ := PInteger(@Result[12])^ - SizePrefetch - SizeSfence;
    PByte(@Result[SizeBody1 + 341])^ :=
      PByte(@Result[SizeBody1 + 341])^ - SizeSfence;
    PInteger(@Result[SizeBody1 + 347])^ :=
      PInteger(@Result[SizeBody1 + 347])^ + SizePrefetch;
    PInteger(@Result[SizeBody1 + SizeBody2 + 5])^ :=
      PInteger(@Result[SizeBody1 + SizeBody2 + 5])^ + SizePrefetch + SizeSfence;

    SizePrefetch := 0;
  end;

  if Add then
    PByte(@Result[SizeBody1 + SizePrefetch + 5])^ := $DC;
  if Levels <> 16 then
  begin
    PByte(@Result[23])^ := Levels;
    Shift := 0;
    case Levels of
        8: Shift := 5;
       32: Shift := 3;
       64: Shift := 2;
      128: Shift := 1;
    end;
    PByte(@Result[SizeBody1 + SizePrefetch +  44])^ := Shift;
    PByte(@Result[SizeBody1 + SizePrefetch + 129])^ := Shift;
    PByte(@Result[SizeBody1 + SizePrefetch + 211])^ := Shift;
  end;
end;
{$endif TrialLimited}

procedure Apply256BmpMaskS(
  Apply256BmpMaskSSubProc, Apply256BmpMaskSAddProc: TApply256BmpMaskSProc;
  Work, Dst, Src, Mask: Pointer;
  ScanLineSize, MaskScanLineSize, Width, Dif: Integer;
  UpdateRect, UnUpdateRect: TRect);
var
  Mode: Longint;
  UpdParBmp,
  UpdParMsk: TTEUpdParams;
  Apply256BmpMaskSProc: TApply256BmpMaskSProc;
begin
  Mode := GiveMeTheUpdMode(Width, 0, 8, UpdateRect, UnUpdateRect, pf8bit);
  GiveMeTheUpdParams(Mode, UpdParBmp, ScanLineSize, UpdateRect, UnUpdateRect,
    pf32bit);
  GiveMeTheUpdParams(Mode, UpdParMsk, MaskScanLineSize, UpdateRect,
    UnUpdateRect, pf8bit);

  if Dif >= 0
  then Apply256BmpMaskSProc := Apply256BmpMaskSSubProc
  else
  begin
    Dif := -Dif;
    Apply256BmpMaskSProc := Apply256BmpMaskSAddProc;
  end;

  case Mode of
    1,
    2:
      begin
        Apply256BmpMaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start1,
          PAnsiChar(Dst ) - UpdParBmp.Start1,
          PAnsiChar(Src ) - UpdParBmp.Start1,
          PAnsiChar(Mask) - UpdParMsk.Start1,
          -UpdParMsk.Lenght1,
          UpdParMsk.RowLenght1,
          UpdParMsk.Gap1,
          Dif);
      end;
    3:
      begin
        Apply256BmpMaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start1,
          PAnsiChar(Dst ) - UpdParBmp.Start1,
          PAnsiChar(Src ) - UpdParBmp.Start1,
          PAnsiChar(Mask) - UpdParMsk.Start1,
          -UpdParMsk.Lenght1,
          UpdParMsk.RowLenght1,
          UpdParMsk.Gap1,
          Dif);
        Apply256BmpMaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start2,
          PAnsiChar(Dst ) - UpdParBmp.Start2,
          PAnsiChar(Src ) - UpdParBmp.Start2,
          PAnsiChar(Mask) - UpdParMsk.Start2,
          -UpdParMsk.Lenght2,
          UpdParMsk.RowLenght2,
          UpdParMsk.Gap2,
          Dif);
        Apply256BmpMaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start3,
          PAnsiChar(Dst ) - UpdParBmp.Start3,
          PAnsiChar(Src ) - UpdParBmp.Start3,
          PAnsiChar(Mask) - UpdParMsk.Start3,
          -UpdParMsk.Lenght3,
          UpdParMsk.RowLenght3,
          UpdParMsk.Gap3,
          Dif);
        Apply256BmpMaskSProc(
          PAnsiChar(Work) - UpdParBmp.Start4,
          PAnsiChar(Dst ) - UpdParBmp.Start4,
          PAnsiChar(Src ) - UpdParBmp.Start4,
          PAnsiChar(Mask) - UpdParMsk.Start4,
          -UpdParMsk.Lenght4,
          UpdParMsk.RowLenght4,
          UpdParMsk.Gap4,
          Dif);
      end;
  end;
end;

end.
