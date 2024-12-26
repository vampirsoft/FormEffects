/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Pictures.Tests.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Pictures.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
  Vcl.Graphics,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Winapi.Windows.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TUtilsPicturesTests }

  [TestFixture]
  TUtilsPicturesTests = class
  strict private const
    FWidth : Integer = 199;
    FHeight: Integer = 59;

  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;

    FBitmap: TBitmap;
    FSize: TSize;

  strict private
    procedure AdjustBitmapForTransition(const PixelFormat: TPixelFormat; const Palette: HPALETTE);

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('AdjustBitmapForTransition должен модивицировать Bitmap PixelFormat = pf1bit', 'pf1bit')]
    procedure should_adjust_bitmap(const PixelFormat: TPixelFormat);

    [Test]
    [TestCase('AdjustBitmapForTransition должен модивицировать Bitmap PixelFormat = pf8bit и Palette = 0',  'pf8bit,  0')]
    [TestCase('AdjustBitmapForTransition должен модивицировать Bitmap PixelFormat = pf8bit и Palette = 19', 'pf8bit, 19')]
    procedure should_adjust_bitmap_with_PixelFormat_pf8bit(const PixelFormat: TPixelFormat; const Palette: HPALETTE);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.Types,
  System.SysUtils,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Pictures
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

{$IFDEF FORM_EFFECTS_TESTS}

{ TUtilsPicturesTests }

procedure TUtilsPicturesTests.AdjustBitmapForTransition(const PixelFormat: TPixelFormat; const Palette: HPALETTE);
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.AdjustBmpForTransition(FBitmap, Palette, FWidth, FHeight, PixelFormat);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Pictures.AdjustBitmapForTransition(FBitmap, Palette, FSize, PixelFormat);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TUtilsPicturesTests.should_adjust_bitmap(const PixelFormat: TPixelFormat);
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

  FBitmap := BitmapMock.Instance;

  AdjustBitmapForTransition(PixelFormat, 0);

  BitmapMock.Verify;
end;

procedure TUtilsPicturesTests.should_adjust_bitmap_with_PixelFormat_pf8bit(
  const PixelFormat: TPixelFormat;
  const Palette: HPALETTE
);
begin
  var GetDCInvoked                   := False;
  var GetDeviceCapsInvoked           := False;
  var GetSystemPaletteEntriesInvoked := False;
  var CreatePaletteInvoked           := False;
  var ReleaseDCInvoked               := False;

  const ExpectedWnd: HWND   = 0;
  const TestDC: HDC         = 199;
  const palNumEntries: UINT = 1;

  FWinapiWindowsMocks.GetDC_Result(
    function(const hWnd: HWND): HDC
    begin
      GetDCInvoked := True;

      Assert.AreEqual(ExpectedWnd, hWnd);

      Result := TestDC;
    end
  );
  FWinapiWindowsMocks.GetDeviceCaps_Result(
    function(const DC: HDC; const Index: Integer): Integer
    begin
      GetDeviceCapsInvoked := True;

      Assert.AreEqual(TestDC, DC);
      Assert.AreEqual(SIZEPALETTE, Index);

      Result := palNumEntries;
    end
  );
  FWinapiWindowsMocks.GetSystemPaletteEntries_Result(
    procedure(const DC: HDC; const StartIndex, NumEntries: UINT; out PaletteEntries)
    begin
      GetSystemPaletteEntriesInvoked := True;

      Assert.AreEqual(TestDC, DC);
      Assert.AreEqual(0, StartIndex);
      Assert.AreEqual(palNumEntries, NumEntries);
    end
  );
  FWinapiWindowsMocks.CreatePalette_Result(
    function(const LogPalette: PLogPalette): HPALETTE
    begin
      CreatePaletteInvoked := True;

      Result := Palette;
    end
  );
  FWinapiWindowsMocks.ReleaseDC_Result(
    procedure(const hWnd: HWND; const hDC: HDC)
    begin
      ReleaseDCInvoked := True;

      Assert.AreEqual(ExpectedWnd, hWnd);
      Assert.AreEqual(TestDC, hDC);
    end
  );

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

  FBitmap := BitmapMock.Instance;

  AdjustBitmapForTransition(PixelFormat, Palette);

  BitmapMock.Verify;
  Assert.AreEqual(Palette = 0, GetDCInvoked);
  Assert.AreEqual(Palette = 0, GetDeviceCapsInvoked);
  Assert.AreEqual(Palette = 0, GetSystemPaletteEntriesInvoked);
  Assert.AreEqual(Palette = 0, CreatePaletteInvoked);
  Assert.AreEqual(Palette = 0, ReleaseDCInvoked);
end;

procedure TUtilsPicturesTests.Setup;
begin
  FWinapiWindowsMocks := TWinapiWindowsMocks.Create;

  FSize := TSize.InlineCreate(FWidth, FHeight);
end;

procedure TUtilsPicturesTests.TearDown;
begin
  FBitmap := nil;

  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilsPicturesTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
