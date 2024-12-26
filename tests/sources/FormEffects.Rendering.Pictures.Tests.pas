/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Pictures.Tests.pas                   *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Pictures.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
  FormEffects.FormContainer,
  FormEffects.Utils.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Utils.Pictures.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{$REGION 'TPicturesRenderingMocks'}

{ TPicturesRenderingMocks }

  TPicturesRenderingMocks = class(TMocksManager)
  private type
    TCreateBitmapFactoryResultReference = reference to function: TBitmap;
  public
    procedure CreateBitmapFactory_Result(const CreateBitmapFactoryResult: TBitmap);

  public
    constructor Create; override;
  end;

{$ENDREGION 'TPicturesRenderingMocks'}

{ TPicturesRenderingTests }

  [TestFixture]
  TPicturesRenderingTests = class
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;
    FUtilsPicturesMocks: TUtilsPicturesMocks;
    FPicturesRenderingMocks: TPicturesRenderingMocks;

    FGraphic: TGraphic;
    FBitmap: TBitmap;
    FThisControl: TWinControl;
    FOrgControl: TWinControl;

  strict private
    procedure DrawPicture(
      const PictureMode: TFEBackgroundPictureMode;
      const TransparentColor: TColor;
      const Rect: TRect
    );

  public
    [Setup]
    procedure Setup;
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
    procedure DrawPicture_Should_Draw(const PictureMode: TFEBackgroundPictureMode);

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
    procedure DrawPicture_should_use_clip_rgn(const PictureMode: TFEBackgroundPictureMode);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.Types,
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
  FormEffects.TypeHelpers.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$REGION 'TPicturesRenderingMocks'}

var
  CreateBitmapFactoryResultReference: TPicturesRenderingMocks.TCreateBitmapFactoryResultReference = nil;

type
  TCreateBitmapFactory = function: TBitmap;

function CreateBitmapFactoryMock: TBitmap;
begin
  Result := CreateBitmapFactoryResultReference;
end;

procedure TPicturesRenderingMocks.CreateBitmapFactory_Result(const CreateBitmapFactoryResult: TBitmap);
begin
  CreateBitmapFactoryResultReference :=
    function: TBitmap
    begin
      Result := CreateBitmapFactoryResult;
    end
end;

{ TPicturesRenderingMocks }

constructor TPicturesRenderingMocks.Create;
begin
  inherited Create;

  AddIntercept<TCreateBitmapFactory>(CreateBitmapFactory, CreateBitmapFactoryMock);
  CreateBitmapFactory_Result(nil);
end;

{$ENDREGION 'TPicturesRenderingMocks'}

{ TPicturesRenderingTests }

{$IFNDEF USE_BILLENIUM_EFFECTS}
procedure TPicturesRenderingTests.AdjustBitmap_should_invoke_paint_client_callback_with_canvasdc;
begin
  var AdjustBitmapForTransitionIvoked := False;
  var PaintClientCallbackInvoked      := False;
  var BitBltInvoked                   := False;

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

  FPicturesRenderingMocks.CreateBitmapFactory_Result(TestBitmap);
  FUtilsPicturesMocks.AdjustBitmapForTransition_Result(
    procedure(const Bitmap: TBitmap; const Palette: HPALETTE; const Size: TSize; const PixelFormat: TPixelFormat)
    begin
      AdjustBitmapForTransitionIvoked := True;

      Assert.AreEqual<TBitmap>(TestBitmap, Bitmap);
      Assert.AreEqual<HPALETTE>(0, Palette);
      Assert.AreEqual<TSize>(TestSize, Size);
      Assert.AreEqual<TPixelFormat>(pfDevice, PixelFormat);
    end
  );
  FWinapiWindowsMocks.BitBlt_Result(
    procedure(
      const DestDC: HDC;
      const X, Y, Width, Height: Integer;
      const SrcDC: HDC;
      const XSrc, YSrc: Integer;
      const Rop: DWORD
    )
    begin
      BitBltInvoked := True;

      Assert.AreEqual<HDC>(TestDC, DestDC);
      Assert.AreEqual(0, X);
      Assert.AreEqual(0, Y);
      Assert.AreEqual<TSize>(TestSize, TSize.InlineCreate(Width, Height));
      Assert.AreEqual<HDC>(TestCanvasDC, SrcDC);
      Assert.AreEqual(0, XSrc);
      Assert.AreEqual(0, YSrc);
      Assert.AreEqual(SRCCOPY, Rop);
    end
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

  Assert.IsTrue(AdjustBitmapForTransitionIvoked);
  Assert.IsTrue(PaintClientCallbackInvoked);
  Assert.IsTrue(BitBltInvoked);
  CanvasMock.Verify;
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TPicturesRenderingTests.DrawPicture(
  const PictureMode: TFEBackgroundPictureMode;
  const TransparentColor: TColor;
  const Rect: TRect
);
begin
  const Margin = 84;
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd.DrawPicture(FGraphic, PictureMode.Cast, TransparentColor, FOrgControl, FBitmap, Rect, Margin, FThisControl);
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

procedure TPicturesRenderingTests.DrawPicture_Should_Draw(const PictureMode: TFEBackgroundPictureMode);
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
    TFEBackgroundPictureMode.Center,
    TFEBackgroundPictureMode.TopLeft,
    TFEBackgroundPictureMode.TopRight,
    TFEBackgroundPictureMode.BottomLeft,
    TFEBackgroundPictureMode.BottomRight:
    begin
      CanvasMock.Setup.Expect.Once.When.Draw(
        It0.IsEqualTo<Integer>(279),
        It1.IsEqualTo<Integer>(215),
        It2.IsEqualTo<TGraphic>(FGraphic)
      );
    end;

    TFEBackgroundPictureMode.Stretch,
    TFEBackgroundPictureMode.CenterStretch:
    begin
      CanvasMock.Setup.Expect.Once.When.StretchDraw(
        It0.IsEqualTo<TRect>(TRect.Create(279, 215, 977, 746)),
        It1.IsEqualTo<TGraphic>(FGraphic)
      );
    end;

    TFEBackgroundPictureMode.Tile:
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

    TFEBackgroundPictureMode.Zoom:
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

procedure TPicturesRenderingTests.DrawPicture_should_use_clip_rgn(const PictureMode: TFEBackgroundPictureMode);
begin
  var CreateRectRgnInvoked     := False;
  var GetClipRgnInvoked        := False;
  var IntersectClipRectInvoked := False;
  var SelectClipRgnInvoked     := False;
  var DeleteObjectInvoked      := False;

  const CanvasDC: HDC     = 199;
  const TestRectRgn: HRGN = 399;
  const UseClipRgn        =
     PictureMode in [
      TFEBackgroundPictureMode.Center,
      TFEBackgroundPictureMode.Tile,
      TFEBackgroundPictureMode.Zoom,
      TFEBackgroundPictureMode.TopLeft,
      TFEBackgroundPictureMode.TopRight,
      TFEBackgroundPictureMode.BottomLeft,
      TFEBackgroundPictureMode.BottomRight
    ];
  const ExistClipRgn = PictureMode = TFEBackgroundPictureMode.Center;

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

  FWinapiWindowsMocks.CreateRectRgn_Result(
    function(const Left, Top, Right, Bottom: Integer): HRGN
    begin
      CreateRectRgnInvoked := True;

      Assert.AreEqual<TRect>(TRect.Zero, TRect.InlineCreate(Left, Top, Right, Bottom));

      Result := TestRectRgn;
    end
  );
  FWinapiWindowsMocks.GetClipRgn_Result(
    procedure(const DC: HDC; const Rgn: HRGN)
    begin
      GetClipRgnInvoked := True;

      Assert.AreEqual(CanvasDC, DC);
      Assert.AreEqual(TestRectRgn, Rgn);
    end,
    IfThen(ExistClipRgn, 1, 0)
  );
  FWinapiWindowsMocks.IntersectClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      IntersectClipRectInvoked := True;

      Assert.AreEqual(CanvasDC, DC);
      Assert.AreEqual(363, Left);
      Assert.AreEqual(299, Top);
      Assert.AreEqual(893, Right);
      Assert.AreEqual(662, Bottom);
    end
  );
  FWinapiWindowsMocks.SelectClipRgn_Result(
    procedure(const DC: HDC; const Rgn: HRGN)
    begin
      SelectClipRgnInvoked := True;

      Assert.AreEqual(CanvasDC, DC);
      if ExistClipRgn then Assert.AreEqual<HRGN>(TestRectRgn, Rgn)
      else Assert.AreEqual<HRGN>(0, Rgn);
    end
  );
  FWinapiWindowsMocks.DeleteObject_Result(
    procedure(const Obj: HGDIOBJ)
    begin
      DeleteObjectInvoked := True;

      Assert.AreEqual<HRGN>(TestRectRgn, Obj);
    end
  );

  FGraphic := GraphicMock.Instance;
  FBitmap  := BitmapMock.Instance;

  DrawPicture(PictureMode, clNone, TRect.Create(279, 215, 977, 746));

  Assert.AreEqual(UseClipRgn, CreateRectRgnInvoked);
  Assert.AreEqual(UseClipRgn, GetClipRgnInvoked);
  Assert.AreEqual(UseClipRgn, IntersectClipRectInvoked);
  Assert.AreEqual(UseClipRgn, SelectClipRgnInvoked);
  Assert.AreEqual(UseClipRgn, DeleteObjectInvoked);
end;

procedure TPicturesRenderingTests.Setup;
begin
  FWinapiWindowsMocks     := TWinapiWindowsMocks.Create;
  FUtilsPicturesMocks     := TUtilsPicturesMocks.Create;
  FPicturesRenderingMocks := TPicturesRenderingMocks.Create;
end;

procedure TPicturesRenderingTests.TearDown;
begin
  FOrgControl  := nil;
  FThisControl := nil;
  FBitmap      := nil;
  FGraphic     := nil;

  FreeAndNil(FPicturesRenderingMocks);
  FreeAndNil(FUtilsPicturesMocks);
  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TPicturesRenderingTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
