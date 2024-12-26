/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Pictures.Tests.pas                   *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Pictures.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  FormEffects.Constants,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TPicturesRenderingTests }

  [TestFixture]
  TPicturesRenderingTests = class
  strict private
    FGraphic: TGraphic;
    FBitmap: TBitmap;
    FThisControl: TWinControl;
    FOrgControl: TWinControl;

  strict private
    procedure DrawPicture(
      const PictureMode: TBackgroundPictureMode;
      const TransparentColor: TColor;
      const Rect: TRect
    );

  public
    [TearDown]
    procedure TearDown;

  public
  {$IFNDEF USE_BILLENIUM_EFFECTS}
    [Test]
    [TestCase('AdjustBitmap должен вызвать PaintClientCallback с CanvasDC', '')]
    procedure AdjustBitmap_should_invoke_paint_client_callback_with_canvasdc;
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
    [Test]
    [TestCase('for Center',        'Center')]
    [TestCase('for CenterStretch', 'CenterStretch')]
    [TestCase('for Stretch',       'Stretch')]
    [TestCase('for Tile',          'Tile')]
    [TestCase('for Zoom',          'Zoom')]
    [TestCase('for TopLeft',       'TopLeft')]
    [TestCase('for TopRight',      'TopRight')]
    [TestCase('for BottomLeft',    'BottomLeft')]
    [TestCase('for BottomRight',   'BottomRight')]
    procedure DrawPicture_Should_Draw(const PictureMode: TBackgroundPictureMode);

    [Test]
    [TestCase('for Center',        'Center')]
    [TestCase('for CenterStretch', 'CenterStretch')]
    [TestCase('for Stretch',       'Stretch')]
    [TestCase('for Tile',          'Tile')]
    [TestCase('for Zoom',          'Zoom')]
    [TestCase('for TopLeft',       'TopLeft')]
    [TestCase('for TopRight',      'TopRight')]
    [TestCase('for BottomLeft',    'BottomLeft')]
    [TestCase('for BottomRight',   'BottomRight')]
    procedure DrawPicture_should_use_clip_rgn(const PictureMode: TBackgroundPictureMode);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  System.UITypes,
  System.SysUtils,
  System.Math,
  FormEffects.TypeHelpers,
  FormEffects.Utils.Rects,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
  teBkgrnd,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Utils.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Utils.Pictures.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TPicturesRenderingTests }

{$IFNDEF USE_BILLENIUM_EFFECTS}

procedure TPicturesRenderingTests.AdjustBitmap_should_invoke_paint_client_callback_with_canvasdc;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const UtilsPicturesMock = TMock<TUtilsPicturesMocks>.Create;

  var PaintClientCallbackInvoked      := False;

  const TestDC: HDC       = 199;
  const TestCanvasDC: HDC = 1999;

  const TestSize = TSize.InlineCreate(109, 209);

  const CanvasMock = TMock<TCanvas>.Create;
  const BitmapMock = TMock<TBitmap>.Create;

  CanvasMock
    .Setup
    .WillReturn(TestCanvasDC)
    .When
    .GetHandle;
  CanvasMock
    .Setup
    .Expect
    .Exactly(1)
    .When
    .Lock;
  CanvasMock
    .Setup
    .Expect
    .Exactly(1)
    .When
    .Unlock;

  BitmapMock
    .Setup
    .WillReturn(CanvasMock.Instance)
    .When
    .GetCanvas;

  const TestBitmap = BitmapMock.Instance;

  UtilsPicturesMock
    .Setup
    .Expect
    .Once
    .When
    .CreateBitmapFactory;
  UtilsPicturesMock
    .Setup
    .WillReturn(TestBitmap)
    .When
    .CreateBitmapFactory;

  UtilsPicturesMock
    .Setup
    .Expect
    .Once
    .When
    .FreeAndNilBitmap(It0.IsEqualTo(TestBitmap));

  UtilsPicturesMock
    .Setup
    .Expect
    .Once
    .When
    .AdjustBitmapForTransition(
      It0.IsEqualTo<TBitmap>(TestBitmap),
      It1.IsEqualTo<HPALETTE>(0),
      It2.IsEqualTo<TSize>(TestSize),
      It3.IsEqualTo<TPixelFormat>(pfDevice)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .BitBlt(
      It0.IsEqualTo<HDC>(TestDC),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(TestSize.Width),
      It4.IsEqualTo<Integer>(TestSize.Height),
      It5.IsEqualTo<HDC>(TestCanvasDC),
      It6.IsEqualTo<Integer>(0),
      It7.IsEqualTo<Integer>(0),
      It8.IsEqualTo<DWORD>(SRCCOPY)
    );

  AdjustBitmap(
    TestDC,
    TestSize,
      procedure(const CanvasDC: HDC)
      begin
        PaintClientCallbackInvoked := True;

        Assert.AreEqual<HDC>(TestCanvasDC, CanvasDC);
      end
  );

  Assert.IsTrue(PaintClientCallbackInvoked);
  CanvasMock.Verify;
  UtilsPicturesMock.Verify;
  WinapiWindowsMock.Verify;
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TPicturesRenderingTests.DrawPicture(
  const PictureMode: TBackgroundPictureMode;
  const TransparentColor: TColor;
  const Rect: TRect
);
begin
  const Margin = 84;
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd.DrawPicture(
    FGraphic,
    PictureMode.Resolve,
    TransparentColor,
    FOrgControl,
    FBitmap,
    Rect,
    Margin,
    FThisControl
  );
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures.DrawPicture(
    FGraphic,
    PictureMode,
    FThisControl,
    FOrgControl,
    TransparentColor,
    FBitmap,
    Rect,
    Margin
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TPicturesRenderingTests.DrawPicture_Should_Draw(const PictureMode: TBackgroundPictureMode);
begin
  const CanvasMock  = TMock<TCanvas>.Create;
  const BitmapMock  = TMock<TBitmap>.Create;
  const GraphicMock = TMock<TGraphic>.Create;

  BitmapMock
    .Setup
    .WillReturn(CanvasMock.Instance)
    .When
    .GetCanvas;

  FGraphic := GraphicMock.Instance;
  FBitmap  := BitmapMock.Instance;

  case PictureMode of
    TBackgroundPictureMode.Center,
    TBackgroundPictureMode.TopLeft,
    TBackgroundPictureMode.TopRight,
    TBackgroundPictureMode.BottomLeft,
    TBackgroundPictureMode.BottomRight:
    begin
      CanvasMock.Setup.Expect.Once.When.Draw(
        It0.IsEqualTo<Integer>(279),
        It1.IsEqualTo<Integer>(215),
        It2.IsEqualTo<TGraphic>(FGraphic)
      );
    end;

    TBackgroundPictureMode.Stretch,
    TBackgroundPictureMode.CenterStretch:
    begin
      CanvasMock.Setup.Expect.Once.When.StretchDraw(
        It0.IsEqualTo<TRect>(TRect.Create(279, 215, 977, 746)),
        It1.IsEqualTo<TGraphic>(FGraphic)
      );
    end;

    TBackgroundPictureMode.Tile:
    begin
      CanvasMock.Setup.Expect.Exactly(1).When.Draw(
        It0.IsEqualTo<Integer>(279),
        It1.IsEqualTo<Integer>(215),
        It2.IsEqualTo<TGraphic>(FGraphic)
      );
      CanvasMock.Setup.Expect.Exactly(1).When.Draw(
        It0.IsEqualTo<Integer>(279),
        It1.IsEqualTo<Integer>(609),
        It2.IsEqualTo<TGraphic>(FGraphic)
      );
      CanvasMock.Setup.Expect.Exactly(1).When.Draw(
        It0.IsEqualTo<Integer>(828),
        It1.IsEqualTo<Integer>(215),
        It2.IsEqualTo<TGraphic>(FGraphic)
      );
      CanvasMock.Setup.Expect.Exactly(1).When.Draw(
        It0.IsEqualTo<Integer>(828),
        It1.IsEqualTo<Integer>(609),
        It2.IsEqualTo<TGraphic>(FGraphic)
      );
    end;

    TBackgroundPictureMode.Zoom:
    begin
      CanvasMock.Setup.Expect.Once.When.StretchDraw(
        It0.IsEqualTo<TRect>(TRect.Create(258, 215, 998, 746)),
        It1.IsEqualTo<TGraphic>(FGraphic)
      );
    end;
  end;

  DrawPicture(PictureMode, clNone, TRect.Create(279, 215, 977, 746));

  CanvasMock.Verify;
end;

procedure TPicturesRenderingTests.DrawPicture_should_use_clip_rgn(const PictureMode: TBackgroundPictureMode);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const CanvasDC: HDC     = 199;
  const TestRectRgn: HRGN = 399;
  const UseClipRgn        =
     PictureMode in [
      TBackgroundPictureMode.Center,
      TBackgroundPictureMode.Tile,
      TBackgroundPictureMode.Zoom,
      TBackgroundPictureMode.TopLeft,
      TBackgroundPictureMode.TopRight,
      TBackgroundPictureMode.BottomLeft,
      TBackgroundPictureMode.BottomRight
    ];
  const ExistClipRgn = PictureMode = TBackgroundPictureMode.Center;

  const CanvasMock  = TMock<TCanvas>.Create;
  const BitmapMock  = TMock<TBitmap>.Create;
  const GraphicMock = TMock<TGraphic>.Create;

  CanvasMock
    .Setup
    .WillReturn(CanvasDC)
    .When
    .GetHandle;
  BitmapMock
    .Setup
    .WillReturn(CanvasMock.Instance)
    .When
    .GetCanvas;

  FGraphic := GraphicMock.Instance;
  FBitmap  := BitmapMock.Instance;

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(UseClipRgn, 1, 0))
    .When
    .DeleteObject(It0.IsEqualTo<HGDIOBJ>(TestRectRgn));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(UseClipRgn, 1, 0))
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(0),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(0)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(TestRectRgn)
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(0),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(0)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(UseClipRgn, 1, 0))
    .When
    .GetClipRgn(It0.IsEqualTo<HDC>(CanvasDC), It1.IsEqualTo<HRGN>(TestRectRgn));
  WinapiWindowsMock
    .Setup
    .WillReturn(IfThen(ExistClipRgn, 1, 0))
    .When
    .GetClipRgn(It0.IsEqualTo<HDC>(CanvasDC), It1.IsEqualTo<HRGN>(TestRectRgn));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(UseClipRgn, 1, 0))
    .When
    .SelectClipRgn(It0.IsEqualTo<HDC>(CanvasDC), It1.IsEqualTo<HRGN>(IfThen(ExistClipRgn, TestRectRgn, 0)));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(UseClipRgn, 1, 0))
    .When
    .IntersectClipRect(
      It0.IsEqualTo<HDC>(CanvasDC),
      It1.IsEqualTo<Integer>(363),
      It2.IsEqualTo<Integer>(299),
      It3.IsEqualTo<Integer>(893),
      It4.IsEqualTo<Integer>(662)
    );

  DrawPicture(PictureMode, clNone, TRect.Create(279, 215, 977, 746));

  WinapiWindowsMock.Verify;
end;

procedure TPicturesRenderingTests.TearDown;
begin
  FOrgControl  := nil;
  FThisControl := nil;
  FBitmap      := nil;
  FGraphic     := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TPicturesRenderingTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
