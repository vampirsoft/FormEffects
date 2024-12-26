/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Pictures.Tests.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Pictures.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  System.Types,
  Vcl.Graphics,
  FormEffects.Vcl.Graphics.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TUtilsPicturesTests }

  [TestFixture]
  TUtilsPicturesTests = class
  strict private const
    FWidth : Integer = 199;
    FHeight: Integer = 59;

  strict private
    FSize: TSize;

  strict private
    procedure AdjustBitmapForTransition(
      const Bitmap: TBitmap;
      const PixelFormat: TPixelFormat;
      const Palette: HPALETTE
    );
    function GetDeviceBitsPerPixel: Integer;
    function GetDevicePixelFormat: TPixelFormat;

  public
    [Setup]
    procedure Setup;

  public
    [Test]
    [TestCase('AdjustBitmapForTransition должен модивицировать Bitmap PixelFormat = pf1bit', '')]
    procedure should_adjust_bitmap_with_PixelFormat_pf1bit;

    [Test]
    [TestCase('AdjustBitmapForTransition должен модивицировать Bitmap PixelFormat = pf8bit и Palette = 0',  '0')]
    [TestCase('AdjustBitmapForTransition должен модивицировать Bitmap PixelFormat = pf8bit и Palette = 19', '19')]
    procedure should_adjust_bitmap_with_PixelFormat_pf8bit(const Palette: HPALETTE);

    [Test]
    [TestCase('GetDeviceBitsPerPixel должен расчитать DeviceBitsPerPixel на базе системных настроек', '')]
    procedure GetDeviceBitsPerPixel_should_calculate_device_bits_per_pixel;

    [Test]
    [TestCase('GetDevicePixelFormat должен расчитать формат для 16 бит', '')]
    procedure GetDevicePixelFormat_should_calculate_format_for_16_bit;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  System.SysUtils,
  System.Math,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Pictures,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Mocks,
  FormEffects.Winapi.Windows.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$REGION 'Internal definitions'}

type

{ TUtilsPicturesMocks }

  TUtilsPicturesMocks = class abstract(TMocksManager)
  public
    function GetDeviceBitsPerPixel: Integer; virtual; abstract;

  public
    constructor Create; override;
  end;

var
  UtilsPicturesMocks: TUtilsPicturesMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TGetDeviceBitsPerPixel = function(Recalculate: Boolean): Integer;

function GetDeviceBitsPerPixelMock(Recalculate: Boolean): Integer;
begin
  Result := UtilsPicturesMocks.GetDeviceBitsPerPixel;
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TGetDeviceBitsPerPixel = function: Integer;

function GetDeviceBitsPerPixelMock: Integer;
begin
  Result := UtilsPicturesMocks.GetDeviceBitsPerPixel;
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TUtilsPicturesMocks }

constructor TUtilsPicturesMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetDeviceBitsPerPixel>(DeviceBitsPerPixel, GetDeviceBitsPerPixelMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetDeviceBitsPerPixel>(FormEffects.Utils.Pictures.GetDeviceBitsPerPixel, GetDeviceBitsPerPixelMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  UtilsPicturesMocks := Self;
end;

{$ENDREGION 'Internal definitions'}

{ TUtilsPicturesTests }

procedure TUtilsPicturesTests.AdjustBitmapForTransition(
  const Bitmap: TBitmap;
  const PixelFormat: TPixelFormat;
  const Palette: HPALETTE
);
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.AdjustBmpForTransition(Bitmap, Palette, FWidth, FHeight, PixelFormat);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Pictures.AdjustBitmapForTransition(Bitmap, Palette, FSize, PixelFormat);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TUtilsPicturesTests.should_adjust_bitmap_with_PixelFormat_pf1bit;
begin
  const BitmapMock = TMock<TBitmap>.Create;

  BitmapMock
    .Setup
    .Expect
    .Once
    .When
    .SetMonochrome(It0.IsEqualTo<Boolean>(True));
  BitmapMock
    .Setup
    .Expect
    .Never
    .When
    .SetPalette(It0.IsAny<HPALETTE>);

  const Bitmap = BitmapMock.Instance;

  AdjustBitmapForTransition(Bitmap, pf1bit, 0);

  BitmapMock.Verify;
end;

procedure TUtilsPicturesTests.should_adjust_bitmap_with_PixelFormat_pf8bit(const Palette: HPALETTE);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const ExpectedWnd: HWND   = 0;
  const TestDC: HDC         = 199;
  const palNumEntries: UINT = 1;

  const BitmapMock = TMock<TBitmap>.Create;

  BitmapMock
    .Setup
    .Expect
    .Never
    .When
    .SetMonochrome(It0.IsAny<Boolean>);
  BitmapMock
    .Setup
    .Expect
    .Once
    .When
    .SetPalette(It0.IsEqualTo<HPALETTE>(Palette));

  const Bitmap = BitmapMock.Instance;

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Palette = 0, 1, 0))
    .When
    .GetDC(It0.IsEqualTo<HWND>(ExpectedWnd));
  WinapiWindowsMock
    .Setup
    .WillReturn(TestDC)
    .When
    .GetDC(It0.IsEqualTo<HWND>(ExpectedWnd));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Palette = 0, 1, 0))
    .When
    .ReleaseDC(It0.IsEqualTo<HWND>(ExpectedWnd), It1.IsEqualTo<HDC>(TestDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Palette = 0, 1, 0))
    .When
    .GetDeviceCaps(It0.IsEqualTo<HDC>(TestDC), It1.IsEqualTo<Integer>(SIZEPALETTE));
  WinapiWindowsMock
    .Setup
    .WillReturn(palNumEntries)
    .When
    .GetDeviceCaps(It0.IsEqualTo<HDC>(TestDC), It1.IsEqualTo<Integer>(SIZEPALETTE));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Palette = 0, 1, 0))
    .When
    .GetSystemPaletteEntries(It0.IsEqualTo<HDC>(TestDC), It1.IsEqualTo<UINT>(0), It2.IsEqualTo<UINT>(palNumEntries));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Palette = 0, 1, 0))
    .When
    .CreatePalette;

  AdjustBitmapForTransition(Bitmap, pf8bit, Palette);

  BitmapMock.Verify;
  WinapiWindowsMock.Verify;
end;

function TUtilsPicturesTests.GetDeviceBitsPerPixel: Integer;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teRender.DeviceBitsPerPixel(True);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Pictures.GetDeviceBitsPerPixel;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TUtilsPicturesTests.GetDeviceBitsPerPixel_should_calculate_device_bits_per_pixel;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const DC: HDC = 199;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetDC(It0.IsEqualTo<HWND>(0));
  WinapiWindowsMock
    .Setup
    .WillReturn(DC)
    .When
    .GetDC(It0.IsEqualTo<HWND>(0));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetDeviceCaps(It0.IsEqualTo<HDC>(DC), It1.IsEqualTo<Integer>(PLANES));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetDeviceCaps(It0.IsEqualTo<HDC>(DC), It1.IsEqualTo<Integer>(BITSPIXEL));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .ReleaseDC(It0.IsEqualTo<HWND>(0), It1.IsEqualTo<HDC>(DC));

  const Actual = GetDeviceBitsPerPixel;

  Assert.AreEqual<Integer>(DC * PLANES * DC * BITSPIXEL, Actual);
  WinapiWindowsMock.Verify;
end;

function TUtilsPicturesTests.GetDevicePixelFormat: TPixelFormat;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teRender.DevicePixelFormat(True);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Pictures.GetDevicePixelFormat;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TUtilsPicturesTests.GetDevicePixelFormat_should_calculate_format_for_16_bit;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const UtilsPicturesMock = TMock<TUtilsPicturesMocks>.Create;

  const DC: HDC            = 199;
  const BitmapDC: HDC      = 299;
  const Bitmap: HBITMAP    = 399;
  const OldBitmap: HBITMAP = 499;

  UtilsPicturesMock
    .Setup
    .WillReturn(16)
    .When
    .GetDeviceBitsPerPixel;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetDC(It0.IsEqualTo<HWND>(0));
  WinapiWindowsMock
    .Setup
    .WillReturn(DC)
    .When
    .GetDC(It0.IsEqualTo<HWND>(0));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .CreateCompatibleDC(It0.IsEqualTo(DC));
  WinapiWindowsMock
    .Setup
    .WillReturn(BitmapDC)
    .When
    .CreateCompatibleDC(It0.IsEqualTo(DC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .CreateCompatibleBitmap(It0.IsEqualTo(DC), It1.IsEqualTo(10), It2.IsEqualTo(10));
  WinapiWindowsMock
    .Setup
    .WillReturn(Bitmap)
    .When
    .CreateCompatibleBitmap(It0.IsEqualTo(DC), It1.IsEqualTo(10), It2.IsEqualTo(10));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SelectObject(It0.IsEqualTo(BitmapDC), It1.IsEqualTo(Bitmap));
  WinapiWindowsMock
    .Setup
    .WillReturn(OldBitmap)
    .When
    .SelectObject(It0.IsEqualTo(BitmapDC), It1.IsEqualTo(Bitmap));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SelectObject(It0.IsEqualTo(BitmapDC), It1.IsEqualTo(OldBitmap));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(256)
    .When
    .SetPixel(It0.IsEqualTo(BitmapDC), It1.IsEqualTo(1), It2.IsEqualTo(1), It3.IsAny<COLORREF>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(256)
    .When
    .GetPixel(It0.IsEqualTo(BitmapDC), It1.IsEqualTo(1), It2.IsEqualTo(1));
  WinapiWindowsMock
    .Setup
    .WillReturn(-1)
    .When
    .GetPixel(It0.IsEqualTo(BitmapDC), It1.IsEqualTo(1), It2.IsEqualTo(1));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteObject(It0.IsEqualTo(Bitmap));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteDC(It0.IsEqualTo(BitmapDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .ReleaseDC(It0.IsEqualTo<HWND>(0), It1.IsEqualTo(DC));

  const Actual = GetDevicePixelFormat;

  Assert.AreEqual<TPixelFormat>(pf15bit, Actual);
  WinapiWindowsMock.Verify;
end;

procedure TUtilsPicturesTests.Setup;
begin
  FSize := TSize.InlineCreate(FWidth, FHeight);
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilsPicturesTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
