/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Pictures.pas                             *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Pictures;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  Vcl.Graphics,
  FormEffects.TypeHelpers
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

procedure AdjustBitmapForTransition(
  const Bitmap: TBitmap;
  const Size: TSize;
  const PixelFormat: TPixelFormat
); overload; inline;
procedure AdjustBitmapForTransition(
  const Bitmap: TBitmap;
  const Palette: HPALETTE;
  const Size: TSize;
  const PixelFormat: TPixelFormat
); overload;

implementation

uses
  System.Types;

{$REGION 'Internal definitions'}

function CreateIdentityPalette: HPALETTE; inline;
var
  PaletteData: TMaxLogPalette;

begin
  with HDC.Create do
  try
    PaletteData.palVersion    := $300;
    PaletteData.palNumEntries := GetDeviceCaps(SIZEPALETTE);
    GetSystemPaletteEntries(0, PaletteData.palNumEntries, PaletteData.palPalEntry);
    Result := HPALETTE.Create(PaletteData);
  finally
    ReleaseDC;
  end;
end;

{$ENDREGION 'Internal definitions'}

procedure AdjustBitmapForTransition(const Bitmap: TBitmap; const Size: TSize; const PixelFormat: TPixelFormat);
begin
  AdjustBitmapForTransition(Bitmap, 0, Size, PixelFormat);
end;

procedure AdjustBitmapForTransition(
  const Bitmap: TBitmap;
  const Palette: HPALETTE;
  const Size: TSize;
  const PixelFormat: TPixelFormat
); overload;
begin
  Bitmap.PixelFormat := PixelFormat;
  case PixelFormat of
    pf1bit: Bitmap.Monochrome := True;
    pf8bit:
    begin
      if Palette = 0 then
        Bitmap.Palette := CreateIdentityPalette
      else
        Bitmap.Palette := Palette;
    end;
  end;
  Bitmap.Size := Size;
end;

end.
