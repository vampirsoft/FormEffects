/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Pictures.pas                             *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Pictures;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  Vcl.Graphics
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

function CreateBitmapFactory: TBitmap;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
procedure FreeAndNilBitmap(const [ref] Bitmap: TBitmap);{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}

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

{$IFDEF FORM_EFFECTS_TESTS}
function GetDeviceBitsPerPixel: Integer;
{$ENDIF ~ FORM_EFFECTS_TESTS}
function GetDevicePixelFormat: TPixelFormat;

implementation

uses
  FormEffects.TypeHelpers;

function CreateBitmapFactory: TBitmap;
begin
  Result := TBitmap.Create;
end;

procedure FreeAndNilBitmap(const [ref] Bitmap: TBitmap);
begin
  FreeAndNil(Bitmap);
end;

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

{$REGION 'Internal definitions'}

function CalculatePixelFormatFor16Bit: TPixelFormat; inline;
begin
  with HDC.Create do
  try
    const BitmapDC = CreateCompatibleDC;
    try
      const Bitmap = CreateCompatibleBitmap(10, 10);
      try
        with BitmapDC.Select(Bitmap) do
        try
          var PrevGPixel := 255;
          var Count      := 0;

          for var Green := 0 to 255 do
          begin
            const Pixel = RGB(0, Green, 0);
            BitmapDC.SetPixel(1, 1, Pixel);

            const PrevPixel = BitmapDC.GetPixel(1, 1);
            if GetGValue(PrevPixel) <> PrevGPixel then
              Inc(Count);

            PrevGPixel := GetGValue(PrevPixel);
          end;

          if Count > 32 then
            Result := pf16bit
          else
            Result := pf15bit;
        finally
          Delete;
        end;
      finally
        Bitmap.Delete;
      end;
    finally
      BitmapDC.Delete;
    end;
  finally
    ReleaseDC;
  end;
end;

{$ENDREGION 'Internal definitions'}

var
  DeviceBitsPerPixel: Integer = -1;

function GetDeviceBitsPerPixel: Integer;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
begin
  if DeviceBitsPerPixel = -1 then
  begin
    with HDC.Create do
    try
      DeviceBitsPerPixel := GetDeviceCaps(PLANES) * GetDeviceCaps(BITSPIXEL);
    finally
      ReleaseDC;
    end;
  end;

  Result := DeviceBitsPerPixel;
end;

var
  DevicePixelFormat: TPixelFormat = pfCustom;

function GetDevicePixelFormat: TPixelFormat;
begin
  if DevicePixelFormat = pfCustom then
  begin
    case GetDeviceBitsPerPixel of
      1 :  DevicePixelFormat := pf1bit;
      4 :  DevicePixelFormat := pf4bit;
      8 :  DevicePixelFormat := pf8bit;
      15:  DevicePixelFormat := pf15bit;
      16:  DevicePixelFormat := CalculatePixelFormatFor16Bit;
      24:  DevicePixelFormat := pf24bit;
      32:  DevicePixelFormat := pf32bit;
      else DevicePixelFormat := pf24bit;
    end;
  end;

  Result := DevicePixelFormat;
end;

end.
