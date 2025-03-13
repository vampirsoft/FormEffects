unit teBlndWk;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, teRender, Graphics;

const
  Mask_32_1 = $7F7F7F7F;
  Mask_32_2 = $3F3F3F3F;
  Mask_32_3 = $1F1F1F1F;
  Mask_32_4 = $0F0F0F0F;

  Mask_16_1 = $7BEF7BEF;
  Mask_16_2 = $39E739E7;
  Mask_16_3 = $18E318E3;
  Mask_16_4 = $08610861;

  Mask_15_1 = $3DEF3DEF;
  Mask_15_2 = $1CE71CE7;
  Mask_15_3 = $0C630C63;
  Mask_15_4 = $04210421;

  procedure StandardFuseFrame(BrushBmp: TBitmap; CurrentFrame: Integer);
  procedure EasyBlendBmps(WorkBmp, DstBmp, SrcBmp: TBitmap; Level: Integer);
  procedure BlendBmps(WorkBmp, DstBmp, SrcBmp, BrushBmp: TBitmap;
    PixelFormat: TPixelFormat; Level, EqualQuads: Integer);
  procedure BlendBmp(Bmp, BrushBmp: TBitmap; PixelFormat: TPixelFormat;
    Color: TColor; R: TRect; Level: Integer);

implementation

var
  ColorBlendArrayRed,
  ColorBlendArrayGreen,
  ColorBlendArrayBlue: array[0..255] of Byte;
  ColorBlendArrayRedValue,
  ColorBlendArrayGreenValue,
  ColorBlendArrayBlueValue,
  ColorBlendArrayLevel: Byte;
  ColorBlendArrayPixelFormat: TPixelFormat;
  ColorBlendArrayInitialized: Boolean = False;

procedure StandardFuseFrame(BrushBmp: TBitmap; CurrentFrame: Integer);
const
  PixelArray: array[1..64*2] of byte =
    (4, 4,
     0, 0,
     4, 0,
     0, 4,
     6, 6,
     2, 2,
     6, 2,
     2, 6,
     6, 4,
     2, 0,
     6, 0,
     2, 4,
     4, 6,
     0, 2,
     4, 2,
     0, 6,
     5, 7,
     1, 3,
     5, 3,
     1, 7,
     7, 5,
     3, 1,
     7, 1,
     3, 5,
     7, 7,
     3, 3,
     7, 3,
     3, 7,
     5, 5,
     1, 1,
     5, 1,
     1, 5,
     5, 4,
     1, 0,
     5, 0,
     1, 4,
     7, 6,
     3, 2,
     7, 2,
     3, 6,
     7, 4,
     3, 0,
     7, 0,
     3, 4,
     5, 6,
     1, 2,
     5, 2,
     1, 6,
     6, 7,
     2, 3,
     6, 3,
     2, 7,
     4, 5,
     0, 1,
     4, 1,
     0, 5,
     4, 7,
     0, 3,
     4, 3,
     0, 7,
     6, 5,
     2, 1,
     6, 1,
     2, 5);
begin
  BrushBmp.Canvas.Pixels[
    PixelArray[(CurrentFrame*2)-1], PixelArray[CurrentFrame*2]] := clBlack;
end;

procedure TEAlphaBlendMMX(Work, Dst, Src: Pointer; Size, Alpha: Integer);
{$ifdef TrialLimited}begin end;{$else}
type
  TEAlphaBlendMMXProc = procedure(Work, Dst, Src: Pointer; Size, Alpha: Integer);
const
  Body: array[1..364] of Byte = (
    $55,                     
    $8B,$EC,                 
    $57,                     
    $56,                     
    $53,                     

    $89,$D7,                 
    $89,$CE,                 
    $89,$C3,                 

    $8B,$4D,$0C,             
    $C1,$E9,$0C,             
    $C1,$E1,$09,             

    $0F,$84,$94,$00,$00,$00, 

    $8D,$3C,$CF,             
    $8D,$34,$CE,             
    $8D,$1C,$CB,             

    $F7,$D9,                 

    $0F,$EF,$C0,             

    $0F,$6E,$4D,$08,         
    $0F,$61,$C9,             
    $0F,$62,$C9,             

    $B8,$40,$00,$00,$00,     
    $81,$C1,$00,$02,$00,$00, 

    $8B,$54,$CE,$E0,         
    $8B,$54,$CE,$C0,         
    $83,$E9,$08,             
    $48,                     
    $75,$F2,                 

    $81,$C1,$00,$02,$00,$00, 
    $B8,$40,$00,$00,$00,     

    $8B,$54,$CF,$E0,         
    $8B,$54,$CF,$C0,         
    $83,$E9,$08,             
    $48,                     
    $75,$F2,                 
    $B8,$00,$02,$00,$00,     

    $0F,$6F,$14,$CF,         

    $0F,$6F,$1C,$CE,         
    $0F,$6F,$E2,             

    $0F,$60,$D0,             
    $0F,$6F,$EB,             

    $0F,$60,$D8,             

    $0F,$F9,$D3,             

    $0F,$D5,$D1,             
    $0F,$68,$E0,             

    $0F,$6F,$F5,             
    $0F,$68,$E8,             

    $0F,$F9,$E5,             

    $0F,$D5,$E1,             
    $0F,$71,$D2,$08,         

    $0F,$71,$D4,$08,         

    $0F,$67,$D4,             

    $0F,$FC,$D6,             

//  db $0F,$7F,$14,$CB       
    $0F,$E7,$14,$CB,         
    $83,$C1,$01,             

    $48,                     
    $75,$BF,                 

    $09,$C9,                 
    $7C,$84,                 

    $8B,$4D,$0C,             

    $81,$E1,$F8,$0F,$00,$00, 
    $C1,$E9,$03,             

    $74,$4D,                 

    $8D,$3C,$CF,             
    $8D,$34,$CE,             
    $8D,$1C,$CB,             

    $F7,$D9,                 

    $0F,$6F,$14,$CF,         

    $0F,$6F,$1C,$CE,         
    $0F,$6F,$E2,             

    $0F,$60,$D0,             
    $0F,$6F,$EB,             

    $0F,$60,$D8,             

    $0F,$F9,$D3,             

    $0F,$D5,$D1,             
    $0F,$68,$E0,             

    $0F,$6F,$F5,             
    $0F,$68,$E8,             

    $0F,$F9,$E5,             

    $0F,$D5,$E1,             
    $0F,$71,$D2,$08,         

    $0F,$71,$D4,$08,         

    $0F,$67,$D4,             

    $0F,$FC,$D6,             

//  db $0F,$7F,$14,$CB       
    $0F,$E7,$14,$CB,         
    $83,$C1,$01,             

    $09,$C9,                 
    $7C,$BE,                 

    $8B,$45,$0C,             
    $83,$E0,$04,             
    $74,$4D,                 

    $8B,$07,                 
    $8B,$16,                 
    $25,$00,$FF,$00,$FF,     
    $81,$E2,$00,$FF,$00,$FF, 
    $C1,$E8,$08,             
    $C1,$EA,$08,             
    $29,$D0,                 
    $0F,$AF,$45,$08,         
    $C1,$E8,$08,             
    $01,$D0,                 
    $C1,$E0,$08,             
    $25,$00,$FF,$00,$FF,     

    $8B,$0F,                 
    $8B,$16,                 
    $81,$E1,$FF,$00,$FF,$00, 
    $81,$E2,$FF,$00,$FF,$00, 
    $29,$D1,                 
    $0F,$AF,$4D,$08,         
    $C1,$E9,$08,             
    $01,$D1,                 
    $81,$E1,$FF,$00,$FF,$00, 

    $09,$C1,                 
    $89,$0B,                 

    $5B,                     
    $5E,                     
    $5F,                     

    $0F,$AE,$FF,             
    $0F,$77,                 
    $5D,                     
    $C2,$08,$00,             
    $90);                    
var
  BC: PByteArray;
begin
  BC := VirtualAlloc(nil, SizeOf(Body), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  try
    CopyMemory(@(BC[0]), @Body, SizeOf(Body));
    if not TEProcessorInfo.SSE then
    begin
      PByte(@BC[162])^ := $7F;
      PByte(@BC[256])^ := $7F;
      PByte(@BC[354])^ := $90;
      PByte(@BC[355])^ := $90;
      PByte(@BC[356])^ := $90;
    end;

    TEAlphaBlendMMXProc(BC)(Work, Dst, Src, Size, Alpha);
  finally
    VirtualFree(BC, 0, MEM_RELEASE);
  end;
end;
{$endif TrialLimited}

procedure TEAlphaBlendAsm(Work, Dst, Src: Pointer; PixelFormat: TPixelFormat;
  Size, Alpha, EqualPerc: Integer; IsColor: Boolean; Color: TColor);
type
  TAlphaBlendAsmProc = procedure(Work, Dst, Src:Pointer);
const
  CommonBody1: array[1..30] of Byte = (
    $53,                     
    $57,                     
    $56,                     
    $55,                     
    $89,$D7,                 
    $89,$CE,                 
    $EB,$14,                 
    $83,$C0,$04,             
    $83,$C6,$04,             
    $83,$C7,$04,             
    $3D,$67,$45,$23,$01,     
    $0F,$84,$51,$00,$00,$00);
  BodyMemCmp: array[1..8] of Byte = (
    $8B,$0F,                 
    $8B,$16,                 
    $39,$D1,                 
    $74,$E4);                
  BodyMemNoCmp: array[1..4] of Byte = (
    $8B,$0F,                 
    $8B,$16);                
  BodyColorCmp1: array[1..11] of Byte = (
    $B9,$67,$45,$23,$01,     
    $8B,$16,                 
    $39,$D1,                 
    $74,$E1);                
  BodyColorCmp2: array[1..11] of Byte = (
    $8B,$0F,                 
    $BA,$67,$45,$23,$01,     
    $39,$D1,                 
    $74,$E1);                
  BodyColorNoCmp1: array[1..7] of Byte = (
    $B9,$67,$45,$23,$01,     
    $8B,$16);                
  BodyColorNoCmp2: array[1..7] of Byte = (
    $8B,$0F,                 
    $BA,$67,$45,$23,$01);    
  CommonBody2: array[1..38] of Byte = (
    $89,$CB,                 
    $89,$D5,                 
    $81,$E3,$00,$FF,$00,$FF, 
    $81,$E5,$00,$FF,$00,$FF, 
    $C1,$EB,$08,             
    $81,$E1,$FF,$00,$FF,$00, 
    $C1,$ED,$08,             
    $81,$E2,$FF,$00,$FF,$00, 
    $29,$D1,                 
    $29,$EB                  
    );
  CommonBody3: array[1..48] of Byte =
    (
    $01,$EB,                 
    $01,$D1,                 
    $C1,$E3,$08,             
    $81,$E1,$FF,$00,$FF,$00, 
    $81,$E3,$00,$FF,$00,$FF, 
    $83,$C7,$04,             
    $09,$D9,                 
    $83,$C6,$04,             
    $89,$08,                 
    $83,$C0,$04,             
    $3D,$67,$45,$23,$01,     
    $0F,$85,$AF,$FF,$FF,$FF, 
    $5D,                     
    $5E,                     
    $5F,                     
    $5B,                     
    $C3);                    

  function BodyDst(Data: PByteArray; DoComp, IsColor, Exchange: Boolean;
    Color: DWord): Integer;
  begin
    if IsColor
    then
    begin
      if Exchange
      then
      begin
        if DoComp
        then
        begin
          CopyMemory(Data, @BodyColorCmp2, SizeOf(BodyColorCmp2));
          Result := SizeOf(BodyColorCmp2);
        end
        else
        begin
          CopyMemory(Data, @BodyColorNoCmp2, SizeOf(BodyColorNoCmp2));
          Result := SizeOf(BodyColorNoCmp2);
        end;
        PDWord(@Data[3])^ := Color;
      end
      else
      begin
        if DoComp
        then
        begin
          CopyMemory(Data, @BodyColorCmp1, SizeOf(BodyColorCmp1));
          Result := SizeOf(BodyColorCmp1);
        end
        else
        begin
          CopyMemory(Data, @BodyColorNoCmp1, SizeOf(BodyColorNoCmp1));
          Result := SizeOf(BodyColorNoCmp1);
        end;
        PDWord(@Data[1])^ := Color;
      end
    end
    else
    begin
      if DoComp
      then
      begin
        CopyMemory(Data, @BodyMemCmp, SizeOf(BodyMemCmp));
        Result := SizeOf(BodyMemCmp);
      end
      else
      begin
        CopyMemory(Data, @BodyMemNoCmp, SizeOf(BodyMemNoCmp));
        Result := SizeOf(BodyMemNoCmp);
      end;
    end;
  end;

  function MultX(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..18] of Byte = (
      $69,$C9,$80,$00,$00,$00, 
      $69,$DB,$80,$00,$00,$00, 
      $C1,$E9,$08,             
      $C1,$EB,$08);            
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[ 2])^ := Factor;
    PByte(@Data[ 8])^ := Factor;
    PByte(@Data[14])^ := Shift;
    PByte(@Data[17])^ := Shift;
    Result := SizeOf(Code);
  end;

  function Mult0(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..6] of Byte = (
      $C1,$E9,$08,  
      $C1,$EB,$08); 
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[2])^ := Shift;
    PByte(@Data[5])^ := Shift;
    Result := SizeOf(Code);
  end;

  function Mult1(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..12] of Byte = (
      $8D,$0C,$49, 
      $8D,$1C,$5B, 
      $C1,$E9,$08, 
      $C1,$EB,$08);
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[ 8])^ := Shift;
    PByte(@Data[11])^ := Shift;

    case Factor of
      5: begin
           PByte(@Data[2])^ := $89;
           PByte(@Data[5])^ := $9B;
         end;
      9: begin
           PByte(@Data[2])^ := $C9;
           PByte(@Data[5])^ := $DB;
         end;
    end;

    Result := SizeOf(Code);
  end;

  function Mult2(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..18] of Byte = (
      $8D,$0C,$89, 
      $8D,$1C,$9B, 
      $8D,$0C,$C9, 
      $8D,$1C,$DB, 
      $C1,$E9,$08, 
      $C1,$EB,$08);
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[14])^ := Shift;
    PByte(@Data[17])^ := Shift;

    case Factor of
      15: begin
            PByte(@Data[ 8])^ := $49;
            PByte(@Data[11])^ := $5B;
          end;
      25: begin
            PByte(@Data[ 8])^ := $89;
            PByte(@Data[11])^ := $9B;
          end;
      27: begin
            PByte(@Data[ 2])^ := $49;
            PByte(@Data[ 5])^ := $5B;
          end;
      81: begin
            PByte(@Data[ 2])^ := $C9;
            PByte(@Data[ 5])^ := $DB;
          end;
    end;

    Result := SizeOf(Code);
  end;

  function Mult3(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..22] of Byte = (
      $50,         
      $56,         
      $8D,$04,$89, 
      $8D,$34,$9B, 
      $8D,$0C,$C1, 
      $8D,$1C,$F3, 
      $C1,$E9,$08, 
      $5E,         
      $C1,$EB,$08, 
      $58);        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[16])^ := Shift;
    PByte(@Data[20])^ := Shift;

    case Factor of
      19: begin
            PByte(@Data[ 4])^ := $C9;
            PByte(@Data[ 7])^ := $DB;
            PByte(@Data[10])^ := $41;
            PByte(@Data[13])^ := $73;
          end;
      21: begin
            PByte(@Data[10])^ := $81;
            PByte(@Data[13])^ := $B3;
          end;
      37: begin
            PByte(@Data[ 4])^ := $C9;
            PByte(@Data[ 7])^ := $DB;
            PByte(@Data[10])^ := $81;
            PByte(@Data[13])^ := $B3;
          end;
      73: begin
            PByte(@Data[ 4])^ := $C9;
            PByte(@Data[ 7])^ := $DB;
          end;
    end;

    Result := SizeOf(Code);
  end;

  function Mult4(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..24] of Byte = (
      $50,         
      $56,         
      $8D,$04,$89, 
      $8D,$34,$9B, 
      $01,$C9,     
      $01,$DB,     
      $01,$C1,     
      $01,$F3,     
      $C1,$E9,$08, 
      $5E,         
      $C1,$EB,$08, 
      $58);        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[18])^ := Shift;
    PByte(@Data[22])^ := Shift;

    case Factor of
      11: begin
            PByte(@Data[4])^ := $C9;
            PByte(@Data[7])^ := $DB;
          end;
    end;

    Result := SizeOf(Code);
  end;

  function Mult5(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..34] of Byte = (
      $50,                             
      $56,                             
      $8D,$04,$89,                     
      $8D,$34,$9B,                     
      $8D,$0C,$CD, $00, $00, $00, $00, 
      $8D,$1C,$DD, $00, $00, $00, $00, 
      $01,$C1,                         
      $01,$F3,                         
      $C1,$E9,$08,                     
      $5E,                             
      $C1,$EB,$08,                     
      $58);                            
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[28])^ := Shift;
    PByte(@Data[32])^ := Shift;

    case Factor of
      17: begin
            PByte(@Data[4])^ := $C9;
            PByte(@Data[7])^ := $DB;
          end;
    end;

    Result := SizeOf(Code);
  end;

  function Mult6(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..28] of Byte = (
      $50,         
      $56,         
      $8D,$04,$49, 
      $8D,$34,$5B, 
      $8D,$0C,$C9, 
      $8D,$1C,$DB, 
      $8D,$0C,$C8, 
      $8D,$1C,$DE, 
      $C1,$E9,$08, 
      $5E,         
      $C1,$EB,$08, 
      $58);        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[22])^ := Shift;
    PByte(@Data[26])^ := Shift;

    case Factor of
      39: begin
            PByte(@Data[16])^ := $88;
            PByte(@Data[19])^ := $9E;
          end;
      43: begin
            PByte(@Data[10])^ := $89;
            PByte(@Data[13])^ := $9B;
          end;
      77: begin
            PByte(@Data[ 4])^ := $89;
            PByte(@Data[ 7])^ := $9B;
          end;
    end;

    Result := SizeOf(Code);
  end;

  function Mult7(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..26] of Byte = (
      $50,         
      $56,         
      $8D,$04,$49, 
      $8D,$34,$5B, 
      $C1,$E1,$06, 
      $C1,$E3,$06, 
      $29,$C1,     
      $29,$F3,     
      $C1,$E9,$08, 
      $5E,         
      $C1,$EB,$08, 
      $58);        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[20])^ := Shift;
    PByte(@Data[24])^ := Shift;

    case Factor of
       23: begin
             PByte(@Data[ 4])^ := $C9;
             PByte(@Data[ 7])^ := $DB;
             PByte(@Data[10])^ := $05;
             PByte(@Data[13])^ := $05;
           end;
       29: begin
             PByte(@Data[10])^ := $05;
             PByte(@Data[13])^ := $05;
           end;
       35: begin
             PByte(@Data[10])^ := $05;
             PByte(@Data[13])^ := $05;
             PByte(@Data[14])^ := $01;
             PByte(@Data[16])^ := $01;
           end;
       49: begin
             PByte(@Data[ 9])^ := $E0;
             PByte(@Data[12])^ := $E6;
             PByte(@Data[10])^ := $04;
             PByte(@Data[13])^ := $04;
             PByte(@Data[14])^ := $01;
             PByte(@Data[16])^ := $01;
           end;
       55: begin
             PByte(@Data[ 4])^ := $C9;
             PByte(@Data[ 7])^ := $DB;
           end;
       59: begin
             PByte(@Data[ 4])^ := $89;
             PByte(@Data[ 7])^ := $9B;
           end;
       67: begin
             PByte(@Data[14])^ := $01;
             PByte(@Data[16])^ := $01;
           end;
       69: begin
             PByte(@Data[ 4])^ := $89;
             PByte(@Data[ 7])^ := $9B;
             PByte(@Data[14])^ := $01;
             PByte(@Data[16])^ := $01;
           end;
       97: begin
             PByte(@Data[ 9])^ := $E0;
             PByte(@Data[12])^ := $E6;
             PByte(@Data[10])^ := $05;
             PByte(@Data[13])^ := $05;
             PByte(@Data[14])^ := $01;
             PByte(@Data[16])^ := $01;
           end;
      119: begin
             PByte(@Data[ 4])^ := $C9;
             PByte(@Data[ 7])^ := $DB;
             PByte(@Data[10])^ := $07;
             PByte(@Data[13])^ := $07;
           end;
      123: begin
             PByte(@Data[ 4])^ := $89;
             PByte(@Data[ 7])^ := $9B;
             PByte(@Data[10])^ := $07;
             PByte(@Data[13])^ := $07;
           end;
      125: begin
             PByte(@Data[10])^ := $07;
             PByte(@Data[13])^ := $07;
           end;
    end;

    Result := SizeOf(Code);
  end;

  function Mult8(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..24] of Byte = (
      $50,         
      $56,         
      $89,$C8,     
      $89,$DE,     
      $C1,$E1,$05, 
      $C1,$E3,$05, 
      $29,$C1,     
      $29,$F3,     
      $C1,$E9,$08, 
      $5E,         
      $C1,$EB,$08, 
      $58);        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[18])^ := Shift;
    PByte(@Data[22])^ := Shift;

    case Factor of
       33: begin
             PByte(@Data[12])^ := $01;
             PByte(@Data[14])^ := $01;
           end;
       63: begin
             PByte(@Data[ 8])^ := $06;
             PByte(@Data[11])^ := $06;
           end;
       65: begin
             PByte(@Data[ 8])^ := $06;
             PByte(@Data[11])^ := $06;
             PByte(@Data[12])^ := $01;
             PByte(@Data[14])^ := $01;
           end;
      127: begin
             PByte(@Data[ 8])^ := $07;
             PByte(@Data[11])^ := $07;
           end;
    end;
    Result := SizeOf(Code);
  end;

  function Mult9(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..30] of Byte = (
      $50,         
      $56,         
      $89,$C8,     
      $89,$DE,     
      $8D,$0C,$49, 
      $8D,$1C,$5B, 
      $C1,$E1,$04, 
      $C1,$E3,$04, 
      $29,$C1,     
      $29,$F3,     
      $C1,$E9,$08, 
      $5E,         
      $C1,$EB,$08, 
      $58);        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[24])^ := Shift;
    PByte(@Data[28])^ := Shift;

    case Factor of
      71: begin
            PByte(@Data[ 8])^ := $C9;
            PByte(@Data[11])^ := $DB;
            PByte(@Data[14])^ := $03;
            PByte(@Data[17])^ := $03;
          end;
      79: begin
            PByte(@Data[ 8])^ := $89;
            PByte(@Data[11])^ := $9B;
          end;
      95: begin
            PByte(@Data[14])^ := $05;
            PByte(@Data[17])^ := $05;
          end;
    end;
    Result := SizeOf(Code);
  end;

  function Mult10(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..32] of Byte = (
      $50,         
      $56,         
      $8D,$04,$49, 
      $8D,$34,$5B, 
      $8D,$0C,$49, 
      $8D,$1C,$5B, 
      $C1,$E1,$04, 
      $C1,$E3,$04, 
      $01,$C1,     
      $01,$F3,     
      $C1,$E9,$08, 
      $5E,         
      $C1,$EB,$08, 
      $58);        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[26])^ := Shift;
    PByte(@Data[30])^ := Shift;

    case Factor of
       53: begin
             PByte(@Data[10])^ := $89;
             PByte(@Data[13])^ := $9B;
             PByte(@Data[15])^ := $E0;
             PByte(@Data[18])^ := $E6;
           end;
       57: begin
             PByte(@Data[10])^ := $C9;
             PByte(@Data[13])^ := $DB;
             PByte(@Data[15])^ := $E0;
             PByte(@Data[18])^ := $E6;
           end;
       83: begin
             PByte(@Data[10])^ := $89;
             PByte(@Data[13])^ := $9B;
           end;
       85: begin
             PByte(@Data[ 4])^ := $89;
             PByte(@Data[ 7])^ := $9B;
             PByte(@Data[10])^ := $89;
             PByte(@Data[13])^ := $9B;
           end;
       87: begin
             PByte(@Data[ 4])^ := $C9;
             PByte(@Data[ 7])^ := $DB;
             PByte(@Data[16])^ := $05;
             PByte(@Data[19])^ := $05;
             PByte(@Data[20])^ := $29;
             PByte(@Data[22])^ := $29;
           end;
       89: begin
             PByte(@Data[ 4])^ := $89;
             PByte(@Data[ 7])^ := $9B;
             PByte(@Data[10])^ := $C9;
             PByte(@Data[13])^ := $DB;
             PByte(@Data[15])^ := $E0;
             PByte(@Data[18])^ := $E6;
           end;
       91: begin
             PByte(@Data[ 4])^ := $89;
             PByte(@Data[ 7])^ := $9B;
             PByte(@Data[16])^ := $05;
             PByte(@Data[19])^ := $05;
             PByte(@Data[20])^ := $29;
             PByte(@Data[22])^ := $29;
           end;
       93: begin
             PByte(@Data[16])^ := $05;
             PByte(@Data[19])^ := $05;
             PByte(@Data[20])^ := $29;
             PByte(@Data[22])^ := $29;
           end;
       99: begin
             PByte(@Data[16])^ := $05;
             PByte(@Data[19])^ := $05;
           end;
      101: begin
             PByte(@Data[10])^ := $89;
             PByte(@Data[13])^ := $9B;
             PByte(@Data[16])^ := $05;
             PByte(@Data[19])^ := $05;
             PByte(@Data[15])^ := $E0;
             PByte(@Data[18])^ := $E6;
           end;
      105: begin
             PByte(@Data[10])^ := $C9;
             PByte(@Data[13])^ := $DB;
             PByte(@Data[16])^ := $05;
             PByte(@Data[19])^ := $05;
             PByte(@Data[15])^ := $E0;
             PByte(@Data[18])^ := $E6;
           end;
    end;
    Result := SizeOf(Code);
  end;

  function Mult11(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..32] of Byte = (
      $50,         
      $56,         
      $8D,$04,$89, 
      $8D,$34,$9B, 
      $8D,$0C,$C9, 
      $8D,$1C,$DB, 
      $01,$C1,     
      $01,$F3,     
      $8D,$0C,$C8, 
      $8D,$1C,$DE, 
      $C1,$E9,$08, 
      $5E,         
      $C1,$EB,$08, 
      $58);        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[26])^ := Shift;
    PByte(@Data[30])^ := Shift;

    case Factor of
      121: begin
             PByte(@Data[15])^ := $C8;
             PByte(@Data[17])^ := $DE;
             PByte(@Data[20])^ := $C1;
             PByte(@Data[23])^ := $F3;
           end;
    end;
    Result := SizeOf(Code);
  end;

  function Mult12(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..36] of Byte = (
      $50,         
      $56,         
      $8D,$04,$C9, 
      $8D,$34,$DB, 
      $8D,$0C,$89, 
      $8D,$1C,$9B, 
      $01,$C1,     
      $01,$F3,     
      $C1,$E1,$03, 
      $C1,$E3,$03, 
      $29,$C1,     
      $29,$F3,     
      $C1,$E9,$08, 
      $5E,         
      $C1,$EB,$08, 
      $58);        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[30])^ := Shift;
    PByte(@Data[34])^ := Shift;
    Result := SizeOf(Code);
  end;

  function Mult13(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..44] of Byte = (
      $50,                         
      $56,                         
      $8D,$04,$49,                 
      $8D,$34,$5B,                 
      $8D,$0C,$8D,$00,$00,$00,$00, 
      $8D,$1C,$9D,$00,$00,$00,$00, 
      $01,$C1,                     
      $01,$F3,                     
      $C1,$E1,$04,                 
      $C1,$E3,$04,                 
      $01,$C1,                     
      $01,$F3,                     
      $C1,$E9,$08,                 
      $5E,                         
      $C1,$EB,$08,                 
      $58);                        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[38])^ := Shift;
    PByte(@Data[42])^ := Shift;
    Result := SizeOf(Code);
  end;

  function Mult14(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..34] of Byte = (
      $50,                         
      $56,                         
      $8D,$04,$89,                 
      $8D,$34,$9B,                 
      $01,$C9,                     
      $01,$DB,                     
      $01,$C1,                     
      $01,$F3,                     
      $C1,$E1,$04,                 
      $C1,$E3,$04,                 
      $29,$C1,                     
      $29,$F3,                     
      $C1,$E9,$08,                 
      $5E,                         
      $C1,$EB,$08,                 
      $58);                        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[28])^ := Shift;
    PByte(@Data[32])^ := Shift;
    Result := SizeOf(Code);
  end;

  function Mult15(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..34] of Byte = (
      $50,                         
      $56,                         
      $8D,$04,$C9,                 
      $8D,$34,$DB,                 
      $C1,$E0,$02,                 
      $C1,$E6,$02,                 
      $01,$C1,                     
      $01,$F3,                     
      $01,$C0,                     
      $01,$F6,                     
      $01,$C1,                     
      $01,$F3,                     
      $C1,$E9,$08,                 
      $5E,                         
      $C1,$EB,$08,                 
      $58);                        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[28])^ := Shift;
    PByte(@Data[32])^ := Shift;

    case Factor of
      113: begin
             PByte(@Data[ 4])^ := $89;
             PByte(@Data[ 7])^ := $9B;
             PByte(@Data[ 9])^ := $E1;
             PByte(@Data[12])^ := $E3;
             PByte(@Data[10])^ := $07;
             PByte(@Data[13])^ := $07;
             PByte(@Data[14])^ := $29;
             PByte(@Data[16])^ := $29;
             PByte(@Data[22])^ := $29;
             PByte(@Data[24])^ := $29;
           end;
    end;
    Result := SizeOf(Code);
  end;

  function Mult16(Data: PByteArray; Factor, Shift: Byte): Integer;
  const
    Code: array[1..40] of Byte = (
      $50,                         
      $56,                         
      $8D,$04,$49,                 
      $8D,$34,$5B,                 
      $8D,$0C,$89,                 
      $8D,$1C,$9B,                 
      $C1,$E0,$05,                 
      $C1,$E6,$05,                 
      $01,$C8,                     
      $01,$DE,                     
      $01,$C9,                     
      $01,$DB,                     
      $01,$C1,                     
      $01,$F3,                     
      $C1,$E9,$08,                 
      $5E,                         
      $C1,$EB,$08,                 
      $58);                        
  begin
    CopyMemory(Data, @Code, SizeOf(Code));
    PByte(@Data[34])^ := Shift;
    PByte(@Data[38])^ := Shift;
    Result := SizeOf(Code);
  end;

  function Multiply(Data: PByteArray; Factor: Byte; Exchange: Boolean): Integer;
  var
    Shift: Byte;
  begin
    Result := 0;
    if Exchange then
    begin
      if PixelFormat in [pf15bit, pf16bit] then
        Dec(Factor);
      Factor := 255 - Factor;
    end;

    Shift := 8;
    while not Odd(Factor) do
    begin
      Dec(Shift);
      Factor := Factor div 2;
    end;

    if(Factor <> 1) and (TEProcessorInfo.SSE)
    then Result := MultX(Data, Factor, Shift)
    else
    begin
      case Factor of
        1:
          Result := Mult0 (Data, Factor, Shift);
        3, 5, 9:
          Result := Mult1 (Data, Factor, Shift);
        15, 25, 27, 45, 81:
          Result := Mult2 (Data, Factor, Shift);
        19, 21, 37, 41, 73:
          Result := Mult3 (Data, Factor, Shift);
        7, 11:
          Result := Mult4 (Data, Factor, Shift);
        13, 17:
          Result := Mult5 (Data, Factor, Shift);
        39, 43, 75, 77:
          Result := Mult6 (Data, Factor, Shift);
        23, 29, 35, 49, 55, 59, 61, 67, 69, 97, 119, 123, 125:
          Result := Mult7 (Data, Factor, Shift);
        31, 33, 63, 65, 127:
          Result := Mult8 (Data, Factor, Shift);
        47, 71, 79, 95:
          Result := Mult9 (Data, Factor, Shift);
        51, 53, 57, 83, 85, 87, 89, 91, 93, 99, 101, 105:
          Result := Mult10(Data, Factor, Shift);
        117, 121:
          Result := Mult11(Data, Factor, Shift);
        103:
          Result := Mult12(Data, Factor, Shift);
        115:
          Result := Mult13(Data, Factor, Shift);
        107:
          Result := Mult14(Data, Factor, Shift);
        109, 113:
          Result := Mult15(Data, Factor, Shift);
        111:
          Result := Mult16(Data, Factor, Shift);
      end;
    end;
  end;

var
  BC: PByteArray;
  OffsetDst,
  OffsetMult: Integer;
  Limit: DWord;
  Exchange,
  DoComp: Boolean;
begin
  DoComp := EqualPerc >= 10;

  if PixelFormat in [pf15bit, pf16bit] then
    Alpha := Alpha * 8;

  Exchange := Alpha > 128;
  
  BC := VirtualAlloc(nil,
    SizeOf(CommonBody1) + SizeOf(CommonBody2) + 60 + SizeOf(CommonBody3),
    MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  try
    CopyMemory(@(BC[0]), @CommonBody1, SizeOf(CommonBody1));
    OffsetDst := BodyDst(@(BC[SizeOf(CommonBody1)]), DoComp, IsColor, Exchange,
      Color);
    CopyMemory(@(BC[SizeOf(CommonBody1) + OffsetDst]), @CommonBody2,
      SizeOf(CommonBody2));

    OffsetMult :=
      Multiply(@(BC[SizeOf(CommonBody1) + OffsetDst + SizeOf(CommonBody2)]),
        Alpha, Exchange);
    CopyMemory(@(BC[SizeOf(CommonBody1) + OffsetDst + SizeOf(CommonBody2) +
      OffsetMult]), @CommonBody3, SizeOf(CommonBody3));

    Limit := DWord(Work) + DWord(Size);
    PDWord(@BC[20])^ := Limit;
    PInteger(@BC[26])^ := PInteger(@BC[26])^ + OffsetDst + OffsetMult;
    PDWord(@BC[33 + SizeOf(CommonBody1) + OffsetDst + OffsetMult +
      SizeOf(CommonBody2)])^ := Limit;
    PInteger(@BC[39 + SizeOf(CommonBody1) + OffsetDst + OffsetMult +
      SizeOf(CommonBody2)])^ :=
      PInteger(@BC[39 + SizeOf(CommonBody1) + OffsetDst + OffsetMult +
        SizeOf(CommonBody2)])^ - OffsetDst - OffsetMult;

    case PixelFormat of
      pf15bit:
        begin
          PDWord(@BC[ 6 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $7C1F03E0;
          PDWord(@BC[12 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $7C1F03E0;
          PByte (@BC[18 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $05;
          PDWord(@BC[21 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $3E07C1F;
          PByte (@BC[27 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $05;
          PDWord(@BC[30 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $3E07C1F;
          PByte (@BC[ 6 + SizeOf(CommonBody1) + OffsetDst + OffsetMult +
            SizeOf(CommonBody2)])^ := $05;
          PDWord(@BC[ 9 + SizeOf(CommonBody1) + OffsetDst + OffsetMult +
            SizeOf(CommonBody2)])^ := $3E07C1F;
          PDWord(@BC[15 + SizeOf(CommonBody1) + OffsetDst + OffsetMult +
            SizeOf(CommonBody2)])^ := $7C1F03E0;
        end;
      pf16bit:
        begin
          PDWord(@BC[ 6 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $F81F07E0;
          PDWord(@BC[12 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $F81F07E0;
          PByte (@BC[18 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $05;
          PDWord(@BC[21 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $07E0F81F;
          PByte (@BC[27 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $05;
          PDWord(@BC[30 + SizeOf(CommonBody1) + OffsetDst])^ :=
            $07E0F81F;
          PByte (@BC[ 6 + SizeOf(CommonBody1) + OffsetDst + OffsetMult +
            SizeOf(CommonBody2)])^ := $05;
          PDWord(@BC[ 9 + SizeOf(CommonBody1) + OffsetDst + OffsetMult +
            SizeOf(CommonBody2)])^ := $07E0F81F;
          PDWord(@BC[15 + SizeOf(CommonBody1) + OffsetDst + OffsetMult +
            SizeOf(CommonBody2)])^ := $F81F07E0;
        end;
    end;

    if Exchange
    then TAlphaBlendAsmProc(BC)(Work, Src, Dst)
    else TAlphaBlendAsmProc(BC)(Work, Dst, Src);
  finally
    VirtualFree(BC, 0, MEM_RELEASE);
  end;
end;

procedure EasyBlendBmps(WorkBmp, DstBmp, SrcBmp: TBitmap; Level: Integer);
var
  PixelFormat: TPixelFormat;
  BrushBmp: TBitmap;
begin
  PixelFormat := WorkBmp.PixelFormat;
  BrushBmp    := nil;
  try
    case PixelFormat of
      pf4bit,
      pf8bit:
      begin
        BrushBmp := TBitmap.Create;
        BrushBmp.Canvas.Lock;
        BrushBmp.Monochrome := True;
        BrushBmp.Width      := 8;
        BrushBmp.Height     := 8;
        Level := Level div 4;
        if WorkBmp <> DstBmp then
          BitBlt(WorkBmp.Canvas.Handle, 0, 0, WorkBmp.Width, WorkBmp.Height,
            SrcBmp.Canvas.Handle, 0, 0, cmSrcCopy);
      end;
      pf15bit,
      pf16bit: Level := Level div 8;
    end;
    BlendBmps(WorkBmp, DstBmp, SrcBmp, BrushBmp, PixelFormat, Level, 0);
  finally
    if Assigned(BrushBmp) then
    begin
      BrushBmp.Canvas.Unlock;
      BrushBmp.Free;
    end;
  end;
end;

procedure BlendBmps(WorkBmp, DstBmp, SrcBmp, BrushBmp: TBitmap;
  PixelFormat: TPixelFormat; Level, EqualQuads: Integer);

  procedure DoFuse(WorkBmp, DstBmp, SrcBmp, BrushBmp: TBitmap; Level: Longint);
  var
    i: Integer;
  begin
    for i := 1 to Level do
      StandardFuseFrame(BrushBmp, i);

    WorkBmp.Canvas.Brush.Bitmap := BrushBmp;
    BitBlt(WorkBmp.Canvas.Handle, 0, 0, WorkBmp.Width, WorkBmp.Height,
      DstBmp.Canvas.Handle, 0, 0, $00AC0744);
    WorkBmp.Canvas.Brush.Bitmap := nil;
  end;

var
  Src,
  Dst,
  Work: PDWordArray;
  Size: Longint;
  ScanLineSize: Integer;
begin
  if BrushBmp <> nil
  then DoFuse(WorkBmp, DstBmp, SrcBmp, BrushBmp, Level)
  else
  begin
    ScanLineSize := GetBytesPerScanline(DstBmp, PixelFormat, 32);

    Src  := PDWordArray(SrcBmp .ScanLine[WorkBmp.Height-1]);
    Dst  := PDWordArray(DstBmp .ScanLine[WorkBmp.Height-1]);
    Work := PDWordArray(WorkBmp.ScanLine[WorkBmp.Height-1]);
    Size := ScanLineSize * WorkBmp.Height;

    if(PixelFormat in [pf15bit, pf16bit]) or (not TEProcessorInfo.MMX) or (Size < 1024*4)
    then TEAlphaBlendAsm(Work, Dst, Src, PixelFormat, Size, Level, EqualQuads,
           False, clNone)
    else TEAlphaBlendMMX(Work, Dst, Src, Size, Level);
  end;
end;

procedure BlendBmp(Bmp, BrushBmp: TBitmap; PixelFormat: TPixelFormat;
  Color: TColor; R: TRect; Level: Integer);

  procedure DoColorBlend_15or16(Work: PWordArray; Color: Word;
    ScanLineSize: Integer; i, BmpWidth, BmpHeight: Longint; R: TRect;
    Level: Longint; PixelFormat: TPixelFormat);

    procedure CalcColorBlend16R(R: TRect; BmpWidth, BmpHeight: Longint;
      var ScanLineSize, i, RWidth, Gap, Limit, W: Longint);
    var
      RightGap: Longint;
    begin
      ScanLineSize := ScanLineSize div 2;
      RWidth       := R.Right - R.Left;
      RightGap     := ScanLineSize - R.Right;
      Gap          := R.Left + RightGap;
      Limit        := -(R.Top * ScanLineSize + RightGap);
      Inc(i, R.Left + ((BmpHeight - R.Bottom) * ScanLineSize));
      W            := i + RWidth;
    end;

    procedure CalcColorArray(ColorBlendArray: PByteArray;
      ColorValue, Level, Bits: Byte);
    var
      LevelAux,
      LevelAux2,
      i: Integer;
    begin
      Inc(ColorValue);

      if Bits = 6
      then
      begin
        LevelAux  := (Level shr 2) + 1;
        LevelAux2 := 64 - LevelAux;

        for i:=0 to 63 do
          ColorBlendArray[i] :=
            (((ColorValue * LevelAux2) + ((i+1) * LevelAux)) div 64) - 1;
      end
      else
      begin
        LevelAux  := (Level shr 3) + 1;
        LevelAux2 := 32 - LevelAux;

        for i:=0 to 31 do
          ColorBlendArray[i] :=
            (((ColorValue * LevelAux2) + ((i+1) * LevelAux)) div 32) - 1;
      end;
    end;

  var
    ColorAux: Byte;
    W,
    RWidth,
    Gap,
    Limit: Longint;
  begin
    ColorAux := Color and $001F;
    if(not ColorBlendArrayInitialized)            or
      (ColorBlendArrayLevel <> Level)             or
      (ColorBlendArrayPixelFormat <> PixelFormat) or
      (ColorAux <> ColorBlendArrayBlueValue)      then
    begin // Recalculate blue lookup table
      ColorBlendArrayBlueValue := ColorAux;
      CalcColorArray(@ColorBlendArrayBlue, ColorBlendArrayBlueValue, Level, 5);
    end;

    if PixelFormat = pf16bit
    then ColorAux := (Color shr 5) and $003F
    else ColorAux := (Color shr 5) and $001F;
    if(not ColorBlendArrayInitialized)            or
      (ColorBlendArrayLevel <> Level)             or
      (ColorBlendArrayPixelFormat <> PixelFormat) or
      (ColorAux <> ColorBlendArrayGreenValue)     then
    begin // Recalculate green lookup table
      ColorBlendArrayGreenValue := ColorAux;
      if PixelFormat = pf16bit
      then CalcColorArray(@ColorBlendArrayGreen, ColorBlendArrayGreenValue, Level, 6)
      else CalcColorArray(@ColorBlendArrayGreen, ColorBlendArrayGreenValue, Level, 5)
    end;

    if PixelFormat = pf16bit
    then ColorAux := (Color shr 11) and $001F
    else ColorAux := (Color shr 10) and $001F;
    if(not ColorBlendArrayInitialized)            or
      (ColorBlendArrayLevel <> Level)             or
      (ColorBlendArrayPixelFormat <> PixelFormat) or
      (ColorAux <> ColorBlendArrayRedValue)       then
    begin // Recalculate red lookup table
      ColorBlendArrayRedValue := ColorAux;
      CalcColorArray(@ColorBlendArrayRed, ColorBlendArrayRedValue, Level, 5);
    end;
    ColorBlendArrayInitialized := True;
    ColorBlendArrayLevel       := Level;
    ColorBlendArrayPixelFormat := PixelFormat;

    if EqualRect(R, Rect(0, 0, BmpWidth, BmpHeight))
    then
    begin
      if PixelFormat = pf16bit
      then
      begin
        while i < 0 do
        begin
          Work[i] :=
            (ColorBlendArrayRed  [(Work[i] shr 11) and $001F] shl 11) or
            (ColorBlendArrayGreen[(Work[i] shr  5) and $003F] shl  5) or
            (ColorBlendArrayBlue [ Work[i]         and $001F]);
          Inc(i);
        end;
      end
      else
      begin
        while i < 0 do
        begin
          Work[i] :=
            $8000                                                     or
            (ColorBlendArrayRed  [(Work[i] shr 10) and $001F] shl 10) or
            (ColorBlendArrayGreen[(Work[i] shr  5) and $001F] shl  5) or
            (ColorBlendArrayBlue [ Work[i]         and $001F]);
          Inc(i);
        end;
      end;
    end
    else
    begin
      CalcColorBlend16R(R, BmpWidth, BmpHeight, ScanLineSize, i, RWidth, Gap,
        Limit, W);

      if PixelFormat = pf16bit
      then
      begin
        while i < Limit do
        begin
          while i < W do
          begin
            Work[i] :=
              (ColorBlendArrayRed  [(Work[i] shr 11) and $001F] shl 11) or
              (ColorBlendArrayGreen[(Work[i] shr  5) and $003F] shl  5) or
              (ColorBlendArrayBlue [ Work[i]         and $001F]);
            Inc(i);
          end;
          Inc(i, Gap);
          Inc(W, ScanLineSize);
        end
        end
        else
        begin
        while i < Limit do
        begin
          while i < W do
          begin
            Work[i] :=
              $8000                                                     or
              (ColorBlendArrayRed  [(Work[i] shr 10) and $001F] shl 10) or
              (ColorBlendArrayGreen[(Work[i] shr  5) and $001F] shl  5) or
              (ColorBlendArrayBlue [ Work[i]         and $001F]);
            Inc(i);
          end;
          Inc(i, Gap);
          Inc(W, ScanLineSize);
        end;
      end;
    end;
  end;

  procedure DoColorBlend_24or32(Work: PByteArray; Color: TColor;
    ScanLineSize: Integer; i, BmpWidth, BmpHeight: Longint; R: TRect;
    Level: Longint; PixelFormat: TPixelFormat);

    procedure CalcColorBlendR(R: TRect; BmpWidth, BmpHeight,
      ScanLineSize, BytesPerPixel: Longint;
      var i, RWidth, Gap, Limit, W: Longint);
    var
      RightGap: Longint;
    begin
      RWidth   := (R.Right  - R.Left) * BytesPerPixel;
      RightGap := ScanLineSize - (R.Right * BytesPerPixel);
      Gap      := (R.Left * BytesPerPixel) + RightGap;
      Limit    := -(R.Top * ScanLineSize + RightGap);
      Inc(i, (R.Left * BytesPerPixel) + ((BmpHeight - R.Bottom) * ScanLineSize));
      W        := i + RWidth;
    end;

    procedure CalcColorArray(ColorBlendArray: PByteArray;
      ColorValue, Level: Word);
    var
      LevelAux: Word;
      i: Byte;
    begin
      Inc(ColorValue);
      Inc(Level);
      LevelAux := 256 - Level;
      for i:=0 to 255 do
        ColorBlendArray[i] :=
          (((ColorValue * LevelAux) + ((i+1) * Level)) div 256) - 1;
    end;

  var
    ColorAux,
    aux: Byte;
    W,
    RWidth,
    Gap,
    Limit: Longint;
    BytesPerPixel: Byte;
    Fast: Boolean;
  begin
    ColorAux := GetRValue(Color);
    if(not ColorBlendArrayInitialized)                         or
      (ColorBlendArrayLevel <> Level)                          or
      (not (ColorBlendArrayPixelFormat in [pf24bit, pf32bit])) or
      (ColorAux <> ColorBlendArrayRedValue)                    then
    begin // Recalculate red lookup table
      ColorBlendArrayRedValue := ColorAux;
      CalcColorArray(@ColorBlendArrayRed, ColorBlendArrayRedValue, Level);
    end;
    ColorAux := GetGValue(Color);
    if(not ColorBlendArrayInitialized)                         or
      (ColorBlendArrayLevel <> Level)                          or
      (not (ColorBlendArrayPixelFormat in [pf24bit, pf32bit])) or
      (ColorAux <> ColorBlendArrayGreenValue)                  then
    begin // Recalculate green lookup table
      ColorBlendArrayGreenValue := ColorAux;
      CalcColorArray(@ColorBlendArrayGreen, ColorBlendArrayGreenValue, Level);
    end;
    ColorAux := GetBValue(Color);
    if(not ColorBlendArrayInitialized)                         or
      (ColorBlendArrayLevel <> Level)                          or
      (not (ColorBlendArrayPixelFormat in [pf24bit, pf32bit])) or
      (ColorAux <> ColorBlendArrayBlueValue)                   then
    begin // Recalculate blue lookup table
      ColorBlendArrayBlueValue := ColorAux;
      CalcColorArray(@ColorBlendArrayBlue, ColorBlendArrayBlueValue, Level);
    end;
    ColorBlendArrayInitialized := True;
    ColorBlendArrayLevel       := Level;
    ColorBlendArrayPixelFormat := PixelFormat;

    if PixelFormat = pf24bit
    then
    begin
      aux  := 1;
      Fast :=
        (Bmp.Width mod 4 = 0) and
        EqualRect(R, Rect(0, 0, BmpWidth, BmpHeight));
    end
    else
    begin
      aux  := 2;
      Fast := EqualRect(R, Rect(0, 0, BmpWidth, BmpHeight));
    end;

    if Fast
    then
    begin
      while i < 0 do
      begin
        Work[i] := ColorBlendArrayBlue [Work[i]];
        Inc(i);
        Work[i] := ColorBlendArrayGreen[Work[i]];
        Inc(i);
        Work[i] := ColorBlendArrayRed  [Work[i]];
        Inc(i, aux);
      end;
    end
    else
    begin
      BytesPerPixel := 0;
      case PixelFormat of
        pf24bit: BytesPerPixel := 3;
        pf32bit: BytesPerPixel := 4;
      end;
      CalcColorBlendR(R, BmpWidth, BmpHeight, ScanLineSize, BytesPerPixel, i,
        RWidth, Gap, Limit, W);

      while i < Limit do
      begin
        while i < W do
        begin
          Work[i] := ColorBlendArrayBlue [Work[i]];
          Inc(i);
          Work[i] := ColorBlendArrayGreen[Work[i]];
          Inc(i);
          Work[i] := ColorBlendArrayRed  [Work[i]];
          Inc(i, aux);
        end;
        Inc(i, Gap);
        Inc(W, ScanLineSize);
      end;
    end;
  end;

  procedure DoFuse(Bmp, BrushBmp: TBitmap; Color: TColor; R: TRect;
    Level: Integer);
  var
    i: Integer;
    ColorBmp: TBitmap;
  begin
    for i := 1 to Level do
      StandardFuseFrame(BrushBmp, i);

    Bmp.Canvas.Brush.Bitmap := BrushBmp;
    try
      ColorBmp := TBitmap.Create;
      try
        ColorBmp.Canvas.Lock;
        AdjustBmpForTransition(ColorBmp, 0,
          R.Right - R.Left, R.Bottom - R.Top, Bmp.PixelFormat);
        ColorBmp.Canvas.Brush.Color := Color;
        ColorBmp.Canvas.FillRect(Rect(0, 0, ColorBmp.Width, ColorBmp.Height));

        BitBlt(Bmp.Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom,
          ColorBmp.Canvas.Handle, 0, 0, $00AC0744);
      finally
        if Assigned(ColorBmp) then
        begin
          ColorBmp.Canvas.Unlock;
          ColorBmp.Free;
        end;
      end;
    finally
      Bmp.Canvas.Brush.Bitmap := nil;
    end;
  end;

var
  Clr: PWordArray;
  Work: PDWordArray;
  i,
  ScanLineSize: Integer;
  ColorBmp: TBitmap;
begin
  Color := ColorToRGB(Color);

    if BrushBmp <> nil
    then DoFuse(Bmp, BrushBmp, Color, R, 64 - Level)
    else
    begin
      ScanLineSize := GetBytesPerScanline(Bmp, PixelFormat, 32);
      Work         := PDWordArray(PAnsiChar(Bmp.ScanLine[0]) + ScanlineSize);
      i            := -(ScanLineSize * Bmp.Height);

      if PixelFormat in [pf15bit, pf16bit]
      then
      begin
        ColorBmp := TBitmap.Create;
        try
          ColorBmp.Canvas.Lock;
          AdjustBmpForTransition(ColorBmp, 0,
            32, 1, PixelFormat);
          ColorBmp.Canvas.Pen.Color := Color;
          ColorBmp.Canvas.MoveTo( 0, 0);
          ColorBmp.Canvas.LineTo(32, 0);

          Clr := PWordArray(ColorBmp.ScanLine[0]);
          DoColorBlend_15or16(PWordArray(Work), Clr[0], ScanLineSize, i div 2,
            Bmp.Width, Bmp.Height, R, Level, PixelFormat)
        finally
          if Assigned(ColorBmp) then
          begin
            ColorBmp.Canvas.Unlock;
            ColorBmp.Free;
          end;
        end;
      end
      else DoColorBlend_24or32(PByteArray(Work), Color, ScanLineSize, i,
             Bmp.Width, Bmp.Height, R, Level, PixelFormat);
    end;
end;

end.
