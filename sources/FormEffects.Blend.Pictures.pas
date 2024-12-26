/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Blend.Pictures.pas                             *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Blend.Pictures;

{$INCLUDE FormEffects.inc}

interface

uses
  System.Types,
  Vcl.Graphics
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

{$MESSAGE 'Need to test BlendBitmap'}
procedure BlendBitmap(
  const Bitmap, BrushBitmap: TBitmap;
  const PixelFormat: TPixelFormat;
  const Color: TColor;
  const Rect: TRect;
  const Level: Integer
);

implementation

procedure InternalBlendBitmap(
  const Bitmap, BrushBitmap: TBitmap;
  const PixelFormat: TPixelFormat;
  const Color: TColor;
  const Rect: TRect;
  const Level: Integer
); inline;
begin
{$MESSAGE WARN 'Not Implemented InternalBlendBitmap'}
end;

procedure BlendBitmap(
  const Bitmap, BrushBitmap: TBitmap;
  const PixelFormat: TPixelFormat;
  const Color: TColor;
  const Rect: TRect;
  const Level: Integer
);
begin
  InternalBlendBitmap(Bitmap, BrushBitmap, PixelFormat, ColorToRGB(Color), Rect, Level);
end;

end.
