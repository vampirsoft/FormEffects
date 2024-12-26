/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Tests.DrawBackground.pas           *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Tests.DrawBackground;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
  System.Types,
  Vcl.Graphics,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Constants,
  FormEffects.Backgrounds,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Backgrounds.Tests,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Utils.ScrollBars.Mocks,
  FormEffects.Utils.Pictures.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TDrawBackgroundTests }

  [TestFixture]
  TDrawBackgroundTests = class
  strict private
    FWnd: HWND;
    FDC: HDC;
    FCanvasDC: HDC;

  public
    [Setup]
    procedure Setup;

  public
    [Test]
    [TestCase('DrawBackground должен вызвать DrawStandardBackground, если BackgroundOptions не активны', '')]
    procedure should_draw_standard_background_if_not_active;
  end;

{ TDrawActiveBackgroundTests }

  TDrawActiveBackgroundTests = class
  strict private
    FWnd: HWND;
    FDC: HDC;
    FCanvasDC: HDC;
    FIsInternal: Boolean;
    FInternalBitmap: Boolean;
    FPixelFormat: TPixelFormat;

  strict private
    function ResolveBitmap(const BitmapMock: TMock<TBitmap>): TBitmap; inline;
    function ResolveRect: TRect; inline;
    procedure DrawBackground(const OptionsMock: TMock<TBackgroundOptions>; const BitmapMock: TMock<TBitmap>);
    procedure Setup(
      out WinapiWindowsMock: TMock<TWinapiWindowsMocks>;
      out UtilsScrollBarsMock: TMock<TUtilsScrollBarsMocks>;
      out UtilsPicturesMock: TMock<TUtilsPicturesMocks>;
      out OptionsMock: TMock<TBackgroundOptions>;
      out ParentOptionsMock: TMock<TBackgroundOptions>;
      out WinControlMock: TMock<TWinControl>;
      out ParentControlMock: TMock<TWinControl>;
      out CanvasMock: TMock<TCanvas>;
      out BitmapMock: TMock<TBitmap>
    );

  public
    constructor Create(const InternalBitmap: Boolean; const PixelFormat: TPixelFormat);

  public
    [Test]
    [TestCase('DrawBackground должен вызвать FillRect для Canvas, если используются Glass-свойства', '')]
    procedure should_fill_rect_for_canvas;

    [Test]
    [TestCase('DrawBackground должен вызвать DrawXRay и DrawPicture, если используются Picture-свойства', '')]
    procedure should_DrawXRay_and_DrawPicture;

    [Test]
    [TestCase('DrawBackground должен вызвать DrawBackgroundForm, если используются BackgroundForm-свойства', '')]
    procedure should_DrawBackgroundForm;

    [Test]
    [TestCase('DrawBackground должен вызвать DrawStandardBackground для parent-options, если используются Picture-свойства',  'True')]
    [TestCase('DrawBackground должен вызвать DrawStandardBackground для current-options, если используются Picture-свойства', 'False')]
    procedure should_DrawStandardBackground(const IsParent: Boolean);
  end;

{ TDrawActiveBackgroundWithInternalBitmapTests }

  [TestFixture('InternalBitmap = True && PixelFormat = pf1bit')]
  TDrawActiveBackgroundWithInternalBitmapTests = class(TDrawActiveBackgroundTests)
  public
    constructor Create; reintroduce;
  end;

{ TDrawActiveBackgroundWithDeviceBitmapTests }

  [TestFixture('InternalBitmap = False && PixelFormat = pfDevice')]
  TDrawActiveBackgroundWithDeviceBitmapTests = class(TDrawActiveBackgroundTests)
  public
    constructor Create; reintroduce;
  end;

{ TDrawActiveBackgroundWithExternalBitmapTests }

  [TestFixture('InternalBitmap = False && PixelFormat = pf1bit')]
  TDrawActiveBackgroundWithExternalBitmapTests = class(TDrawActiveBackgroundTests)
  public
    constructor Create; reintroduce;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.Rtti,
  System.Math,
  System.Classes,
  Vcl.Forms,
  FormEffects.TypeHelpers,
  FormEffects.Vcl.Forms.Mocks,
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Utils.Rects.Mocks,
  FormEffects.Rendering.Pictures.Mocks,
  FormEffects.Backgrounds.Rendering.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TDrawBackgroundTests }

procedure TDrawBackgroundTests.Setup;
begin
  FWnd      := 299;
  FDC       := 199;
  FCanvasDC := 399;
end;

procedure TDrawBackgroundTests.should_draw_standard_background_if_not_active;
begin
  const BackgroundsRenderingMock = TMock<TBackgroundsRenderingMocks>.Create;

  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const Rect = TRect.Zero;
  const Options = OptionsMock.Instance;

  OptionsMock
    .Setup
    .WillReturnNil
    .When
    .GetControl;

  BackgroundsRenderingMock
    .Setup
    .Expect
    .Once
    .When
    .DrawStandardBackground(
      It0.IsEqualTo(nil),
      It1.IsEqualTo(FDC),
      It2.IsEqualTo(Rect),
      It3.IsEqualTo(Options.ThemesEnabled)
    );

  Options.DrawBackground(FDC, nil, Rect);

  BackgroundsRenderingMock.Verify;
end;

{ TDrawActiveBackgroundTests }

constructor TDrawActiveBackgroundTests.Create(const InternalBitmap: Boolean; const PixelFormat: TPixelFormat);
begin
  FInternalBitmap := InternalBitmap;
  FPixelFormat    := PixelFormat;
  FIsInternal     := InternalBitmap or (PixelFormat = pfDevice);

  FWnd      := 299;
  FDC       := 199;
  FCanvasDC := 399;
end;

procedure TDrawActiveBackgroundTests.DrawBackground(
  const OptionsMock: TMock<TBackgroundOptions>;
  const BitmapMock: TMock<TBitmap>
);
begin
  const Options = OptionsMock.Instance;
  Options.DrawBackground(FDC, ResolveBitmap(BitmapMock), ResolveRect);
end;

function TDrawActiveBackgroundTests.ResolveBitmap(const BitmapMock: TMock<TBitmap>): TBitmap;
begin
  if FInternalBitmap then
    Result := nil
  else
    Result := BitmapMock.Instance;
end;

function TDrawActiveBackgroundTests.ResolveRect: TRect;
begin
  if FInternalBitmap then
    Result := TRect.Zero
  else
    Result := TRect.InlineCreate(19, 29, 159, 147);
end;

procedure TDrawActiveBackgroundTests.Setup(
  out WinapiWindowsMock: TMock<TWinapiWindowsMocks>;
  out UtilsScrollBarsMock: TMock<TUtilsScrollBarsMocks>;
  out UtilsPicturesMock: TMock<TUtilsPicturesMocks>;
  out OptionsMock: TMock<TBackgroundOptions>;
  out ParentOptionsMock: TMock<TBackgroundOptions>;
  out WinControlMock: TMock<TWinControl>;
  out ParentControlMock: TMock<TWinControl>;
  out CanvasMock: TMock<TCanvas>;
  out BitmapMock: TMock<TBitmap>
);
begin
  WinapiWindowsMock   := TMock<TWinapiWindowsMocks>.Create;
  UtilsScrollBarsMock := TMock<TUtilsScrollBarsMocks>.Create;
  UtilsPicturesMock   := TMock<TUtilsPicturesMocks>.Create;

  OptionsMock       := TMock<TBackgroundOptions>.Create;
  ParentOptionsMock := TMock<TBackgroundOptions>.Create;

  WinControlMock    := TMock<TWinControl>.Create;
  ParentControlMock := TMock<TWinControl>.Create;

  CanvasMock := TMock<TCanvas>.Create;
  BitmapMock := TMock<TBitmap>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl = WinControlMock.Instance;
  WinControl.ComponentState := [csDesigning];

  const ParentControl = ParentControlMock.Instance;
  ParentControl.ComponentState := [csDesigning];

  const Canvas = CanvasMock.Instance;
  const Bitmap = BitmapMock.Instance;
  Bitmap.PixelFormat := FPixelFormat;

  WinControlMock
    .Setup
    .WillReturn(FWnd)
    .When
    .GetHandle;

  WinControlMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .GetParent;

  WinControlMock
    .Setup
    .WillReturn(TRect.InlineCreate(TSize.InlineCreate(157, 89)))
    .When
    .GetClientRect;

  WinControlMock
    .Setup
    .WillReturn(LRESULT(Options))
    .When
    .Perform(It0.IsAny<Cardinal>, It1.IsAny<WPARAM>, It2.IsAny<LPARAM>);

  ParentControlMock
    .Setup
    .WillReturn(TValue.From<TControls>([WinControl]))
    .When
    .GetControls;

  ParentControlMock
    .Setup
    .WillReturn(LRESULT(ParentOptions))
    .When
    .Perform(It0.IsAny<Cardinal>, It1.IsAny<WPARAM>, It2.IsAny<LPARAM>);

  UtilsScrollBarsMock
    .Setup
    .Expect
    .Exactly(2)
    .When
    .IsScrollBarVisible(It0.IsEqualTo<HWND>(FWnd), It1.IsEqualTo<TControl>(WinControl), It2.IsAny<TScrollBarKind>);
  UtilsScrollBarsMock
    .Setup
    .WillReturn(True)
    .When
    .IsScrollBarVisible(It0.IsEqualTo<HWND>(FWnd), It1.IsEqualTo<TControl>(WinControl), It2.IsAny<TScrollBarKind>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(2)
    .When
    .GetScrollInfo(It0.IsEqualTo<HWND>(FWnd), It1.IsAny<Integer>);
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(0, 57))
    .When
    .GetScrollInfo(It0.IsEqualTo<HWND>(FWnd), It1.IsAny<Integer>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 1, 0))
    .When
    .BitBlt(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(157),
      It4.IsEqualTo<Integer>(89),
      It5.IsEqualTo<HDC>(FCanvasDC),
      It6.IsEqualTo<Integer>(57),
      It7.IsEqualTo<Integer>(57),
      It8.IsEqualTo<Integer>(SRCCOPY)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(not FInternalBitmap and FIsInternal, 1, 0))
    .When
    .BitBlt(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(19),
      It2.IsEqualTo<Integer>(29),
      It3.IsEqualTo<Integer>(140),
      It4.IsEqualTo<Integer>(118),
      It5.IsEqualTo<HDC>(FCanvasDC),
      It6.IsEqualTo<Integer>(76),
      It7.IsEqualTo<Integer>(86),
      It8.IsEqualTo<Integer>(SRCCOPY)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 1, 0))
    .When
    .SetWindowOrgEx(It0.IsEqualTo<HDC>(FCanvasDC), It1.IsEqualTo<Integer>(57), It2.IsEqualTo<Integer>(57));
  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(not FInternalBitmap and FIsInternal, 1, 0))
    .When
    .SetWindowOrgEx(It0.IsEqualTo<HDC>(FCanvasDC), It1.IsEqualTo<Integer>(76), It2.IsEqualTo<Integer>(86));

  CanvasMock
    .Setup
    .WillReturn(FCanvasDC)
    .When
    .GetHandle;

  CanvasMock
    .Setup
    .Expect
    .Once
    .When
    .Lock;
  CanvasMock
    .Setup
    .Expect
    .Once
    .When
    .Unlock;

  BitmapMock
    .Setup
    .WillReturn(Canvas)
    .When
    .GetCanvas;

  UtilsPicturesMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 1, 0))
    .When
    .AdjustBitmapForTransition(
      It0.IsEqualTo<TBitmap>(Bitmap),
      It1.IsEqualTo<HPALETTE>(0),
      It2.IsEqualTo<TSize>(TSize.InlineCreate(157, 89)),
      It3.IsEqualTo<TPixelFormat>(pf32bit)
    );
  UtilsPicturesMock
    .Setup
    .Expect
    .Exactly(IfThen(not FInternalBitmap and FIsInternal, 1, 0))
    .When
    .AdjustBitmapForTransition(
      It0.IsEqualTo<TBitmap>(Bitmap),
      It1.IsEqualTo<HPALETTE>(0),
      It2.IsEqualTo<TSize>(TSize.InlineCreate(140, 118)),
      It3.IsEqualTo<TPixelFormat>(pf32bit)
    );

  UtilsPicturesMock
    .Setup
    .WillReturn(pf32bit)
    .When
    .GetDevicePixelFormat;

  UtilsPicturesMock
    .Setup
    .Expect
    .Exactly(IfThen(FIsInternal, 1, 0))
    .When
    .CreateBitmapFactory;
  UtilsPicturesMock
    .Setup
    .WillReturn(Bitmap)
    .When
    .CreateBitmapFactory;

  UtilsPicturesMock
    .Setup
    .Expect
    .Exactly(IfThen(FIsInternal, 1, 0))
    .When
    .FreeAndNilBitmap(It0.IsEqualTo(Bitmap));

  Options.Opaque        := False;
  Options.Control       := WinControl;
  ParentOptions.Control := ParentControl;
end;

procedure TDrawActiveBackgroundTests.should_DrawBackgroundForm;
var
  WinapiWindowsMock: TMock<TWinapiWindowsMocks>;
  UtilsScrollBarsMock: TMock<TUtilsScrollBarsMocks>;
  UtilsPicturesMock: TMock<TUtilsPicturesMocks>;

  OptionsMock: TMock<TBackgroundOptions>;
  ParentOptionsMock: TMock<TBackgroundOptions>;

  WinControlMock: TMock<TWinControl>;
  ParentControlMock: TMock<TWinControl>;

  CanvasMock: TMock<TCanvas>;
  BitmapMock: TMock<TBitmap>;

begin
  Setup(
    WinapiWindowsMock,
    UtilsScrollBarsMock,
    UtilsPicturesMock,
    OptionsMock,
    ParentOptionsMock,
    WinControlMock,
    ParentControlMock,
    CanvasMock,
    BitmapMock
  );

  const BackgroundsRenderingMock = TMock<TBackgroundsRenderingMocks>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  Options.ParentBackgoundForm := True;
  ParentOptions.SetBackgoundForm(TCustomForm);

  const BackgoundForm = ParentOptions.GetBackgoundForm;

  BackgroundsRenderingMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 1, 0))
    .When
    .DrawBackgroundForm(
      It0.IsEqualTo<TControl>(WinControl),
      It1.IsEqualTo<TControl>(ParentControl),
      It2.IsEqualTo<TCustomForm>(BackgoundForm),
      It3.IsEqualTo<HDC>(FCanvasDC),
      It4.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 157, 89)),
      It5.IsEqualTo<TRect>(TRect.InlineCreate(57, 57, 214, 146))
    );
  BackgroundsRenderingMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 0, 1))
    .When
    .DrawBackgroundForm(
      It0.IsEqualTo<TControl>(WinControl),
      It1.IsEqualTo<TControl>(ParentControl),
      It2.IsEqualTo<TCustomForm>(BackgoundForm),
      It3.IsEqualTo<HDC>(FCanvasDC),
      It4.IsEqualTo<TRect>(TRect.InlineCreate(19, 29, 159, 147)),
      It5.IsEqualTo<TRect>(TRect.InlineCreate(76, 86, 216, 204))
    );

  DrawBackground(OptionsMock, BitmapMock);

  BackgroundsRenderingMock.Verify;

  CanvasMock.Verify;
  UtilsPicturesMock.Verify;
  UtilsScrollBarsMock.Verify;
  WinapiWindowsMock.Verify;

  Options.Control       := nil;
  ParentOptions.Control := nil;
end;

procedure TDrawActiveBackgroundTests.should_DrawStandardBackground(const IsParent: Boolean);
var
  WinapiWindowsMock: TMock<TWinapiWindowsMocks>;
  UtilsScrollBarsMock: TMock<TUtilsScrollBarsMocks>;
  UtilsPicturesMock: TMock<TUtilsPicturesMocks>;

  OptionsMock: TMock<TBackgroundOptions>;
  ParentOptionsMock: TMock<TBackgroundOptions>;

  WinControlMock: TMock<TWinControl>;
  ParentControlMock: TMock<TWinControl>;

  CanvasMock: TMock<TCanvas>;
  BitmapMock: TMock<TBitmap>;

begin
  Setup(
    WinapiWindowsMock,
    UtilsScrollBarsMock,
    UtilsPicturesMock,
    OptionsMock,
    ParentOptionsMock,
    WinControlMock,
    ParentControlMock,
    CanvasMock,
    BitmapMock
  );

  const BackgroundsRenderingMock = TMock<TBackgroundsRenderingMocks>.Create;
  const RenderingPicturesMock    = TMock<TRenderingPicturesMocks>.Create;
  const UtilsRectsMock           = TMock<TUtilsRectsMocks>.Create;

  const ParentPictureMock = TMock<TPicture>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  const Bitmap = BitmapMock.Instance;

  const ParentPicture = ParentPictureMock.Instance;
  ParentPicture.Graphic := Bitmap;

  Options.Opaque := True;
  Options.GlassTranslucency := 100;
  Options.ParentPicture := IsParent;
  Options.PictureTransparentColor := clNone;

  ParentOptions.Picture                 := ParentPicture;
  ParentOptions.PictureTransparentColor := clNone;

  BackgroundsRenderingMock
    .Setup
    .Expect
    .Exactly(IfThen(IsParent, 1, 0))
    .When
    .DrawStandardBackground(
      It0.IsEqualTo<TControl>(ParentControl),
      It1.IsEqualTo<HDC>(FCanvasDC),
      It2.IsEqualTo<TRect>(
        TRect.InlineCreate(
          IfThen(FInternalBitmap,  57, 76),
          IfThen(FInternalBitmap,  57, 86),
          IfThen(FInternalBitmap, 214, 216),
          IfThen(FInternalBitmap, 146, 204)
        )
      ),
      It3.IsEqualTo<Boolean>(Options.ThemesEnabled)
    );
  BackgroundsRenderingMock
    .Setup
    .Expect
    .Exactly(IfThen(IsParent, 0, 1))
    .When
    .DrawStandardBackground(
      It0.IsEqualTo<TControl>(WinControl),
      It1.IsEqualTo<HDC>(FCanvasDC),
      It2.IsEqualTo<TRect>(
        TRect.InlineCreate(
          IfThen(FInternalBitmap,  57, 76),
          IfThen(FInternalBitmap,  57, 86),
          IfThen(FInternalBitmap, 214, 216),
          IfThen(FInternalBitmap, 146, 204)
        )
      ),
      It3.IsEqualTo<Boolean>(Options.ThemesEnabled)
    );

  RenderingPicturesMock
    .Setup
    .Expect
    .Exactly(IfThen(IsParent, 1, 0))
    .When
    .DrawPicture(
      It0.IsAny<TGraphic>,
      It1.IsAny<TPictureMode>,
      It2.IsAny<TControl>,
      It3.IsAny<TWinControl>,
      It4.IsAny<TColor>,
      It5.IsAny<TBitmap>,
      It6.IsAny<TRect>,
      It7.IsAny<Word>
    );

  UtilsRectsMock
    .Setup
    .WillReturn(
      TRect.InlineCreate(
        IfThen(FInternalBitmap,  57, 76),
        IfThen(FInternalBitmap,  57, 86),
        IfThen(FInternalBitmap, 214, 216),
        IfThen(FInternalBitmap, 146, 204)
      )
    )
    .When
    .PictureRect(
      It0.IsAny<TGraphic>,
      It1.IsAny<TPictureMode>,
      It2.IsAny<TControl>,
      It3.IsAny<TWinControl>,
      It4.IsAny<Word>
    );
  UtilsRectsMock
    .Setup
    .WillReturn(TRect.Zero)
    .When
    .PictureDrawRect(
      It0.IsAny<TGraphic>,
      It1.IsAny<TPictureMode>,
      It2.IsAny<TControl>,
      It3.IsAny<TWinControl>,
      It4.IsAny<Word>
    );

  DrawBackground(OptionsMock, BitmapMock);

  RenderingPicturesMock.Verify;
  BackgroundsRenderingMock.Verify;

  CanvasMock.Verify;
  UtilsPicturesMock.Verify;
  UtilsScrollBarsMock.Verify;
  WinapiWindowsMock.Verify;

  Options.Control       := nil;
  ParentOptions.Control := nil;
end;

procedure TDrawActiveBackgroundTests.should_DrawXRay_and_DrawPicture;
var
  WinapiWindowsMock: TMock<TWinapiWindowsMocks>;
  UtilsScrollBarsMock: TMock<TUtilsScrollBarsMocks>;
  UtilsPicturesMock: TMock<TUtilsPicturesMocks>;

  OptionsMock: TMock<TBackgroundOptions>;
  ParentOptionsMock: TMock<TBackgroundOptions>;

  WinControlMock: TMock<TWinControl>;
  ParentControlMock: TMock<TWinControl>;

  CanvasMock: TMock<TCanvas>;
  BitmapMock: TMock<TBitmap>;

begin
  Setup(
    WinapiWindowsMock,
    UtilsScrollBarsMock,
    UtilsPicturesMock,
    OptionsMock,
    ParentOptionsMock,
    WinControlMock,
    ParentControlMock,
    CanvasMock,
    BitmapMock
  );

  const BackgroundsRenderingMock = TMock<TBackgroundsRenderingMocks>.Create;
  const RenderingPicturesMock    = TMock<TRenderingPicturesMocks>.Create;
  const UtilsRectsMock           = TMock<TUtilsRectsMocks>.Create;

  const ParentPictureMock = TMock<TPicture>.Create;
  const ParentBitmapMock  = TMock<TBitmap>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  const Bitmap = BitmapMock.Instance;

  const ParentBitmap = ParentBitmapMock.Instance;
  const ParentPicture = ParentPictureMock.Instance;
  ParentPicture.Graphic := ParentBitmap;

  Options.ParentPicture := True;

  ParentOptions.Picture                 := ParentPicture;
  ParentOptions.PictureTransparentColor := not ParentOptions.PictureTransparentColor;

  BackgroundsRenderingMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 1, 0))
    .When
    .DrawXRay(
      It0.IsEqualTo<TControl>(WinControl),
      It1.IsEqualTo<HDC>(FCanvasDC),
      It2.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 157, 89)),
      It3.IsEqualTo<TRect>(TRect.InlineCreate(57, 57, 214, 146))
    );
  BackgroundsRenderingMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 0, 1))
    .When
    .DrawXRay(
      It0.IsEqualTo<TControl>(WinControl),
      It1.IsEqualTo<HDC>(FCanvasDC),
      It2.IsEqualTo<TRect>(TRect.InlineCreate(19, 29, 159, 147)),
      It3.IsEqualTo<TRect>(TRect.InlineCreate(76, 86, 216, 204))
    );

  RenderingPicturesMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 1, 0))
    .When
    .DrawPicture(
      It0.IsEqualTo<TGraphic>(ParentOptions.Picture.Graphic),
      It1.IsEqualTo(ParentOptions.PictureMode),
      It2.IsEqualTo<TControl>(WinControl),
      It3.IsEqualTo<TWinControl>(ParentControl),
      It4.IsEqualTo(ParentOptions.PictureTransparentColor),
      It5.IsEqualTo<TBitmap>(Bitmap),
      It6.IsEqualTo<TRect>(TRect.InlineCreate(57, 57, 214, 146)),
      It7.IsEqualTo(0)
    );
  RenderingPicturesMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 0, 1))
    .When
    .DrawPicture(
      It0.IsEqualTo<TGraphic>(ParentOptions.Picture.Graphic),
      It1.IsEqualTo(ParentOptions.PictureMode),
      It2.IsEqualTo<TControl>(WinControl),
      It3.IsEqualTo<TWinControl>(ParentControl),
      It4.IsEqualTo(ParentOptions.PictureTransparentColor),
      It5.IsEqualTo<TBitmap>(Bitmap),
      It6.IsEqualTo<TRect>(TRect.InlineCreate(76, 86, 216, 204)),
      It7.IsEqualTo(0)
    );

  UtilsRectsMock
    .Setup
    .WillReturn(TRect.Zero)
    .When
    .PictureRect(
      It0.IsAny<TGraphic>,
      It1.IsAny<TPictureMode>,
      It2.IsAny<TControl>,
      It3.IsAny<TWinControl>,
      It4.IsAny<Word>
    );
  UtilsRectsMock
    .Setup
    .WillReturn(TRect.Zero)
    .When
    .PictureDrawRect(
      It0.IsAny<TGraphic>,
      It1.IsAny<TPictureMode>,
      It2.IsAny<TControl>,
      It3.IsAny<TWinControl>,
      It4.IsAny<Word>
    );

  DrawBackground(OptionsMock, BitmapMock);

  RenderingPicturesMock.Verify;
  BackgroundsRenderingMock.Verify;

  CanvasMock.Verify;
  UtilsPicturesMock.Verify;
  UtilsScrollBarsMock.Verify;
  WinapiWindowsMock.Verify;

  Options.Control       := nil;
  ParentOptions.Control := nil;
end;

procedure TDrawActiveBackgroundTests.should_fill_rect_for_canvas;
var
  WinapiWindowsMock: TMock<TWinapiWindowsMocks>;
  UtilsScrollBarsMock: TMock<TUtilsScrollBarsMocks>;
  UtilsPicturesMock: TMock<TUtilsPicturesMocks>;

  OptionsMock: TMock<TBackgroundOptions>;
  ParentOptionsMock: TMock<TBackgroundOptions>;

  WinControlMock: TMock<TWinControl>;
  ParentControlMock: TMock<TWinControl>;

  CanvasMock: TMock<TCanvas>;
  BitmapMock: TMock<TBitmap>;

begin
  Setup(
    WinapiWindowsMock,
    UtilsScrollBarsMock,
    UtilsPicturesMock,
    OptionsMock,
    ParentOptionsMock,
    WinControlMock,
    ParentControlMock,
    CanvasMock,
    BitmapMock
  );

  const BrushMock = TMock<TBrush>.Create;

  const Brush = BrushMock.Instance;

  const Options       = OptionsMock.Instance;
  Options.GlassTranslucency := 0;
  Options.ParentGlass := True;
  const ParentOptions = ParentOptionsMock.Instance;
  ParentOptions.GlassTranslucency := 0;
  ParentOptions.GlassColor := not ParentOptions.GlassColor;

  BrushMock
    .Setup
    .Expect
    .Once
    .When
    .SetColor(It0.IsEqualTo<TColor>(ParentOptions.GlassColor));

  CanvasMock
    .Setup
    .WillReturn(Brush)
    .When
    .GetBrush;

  CanvasMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 1, 0))
    .When
    .FillRect(It0.IsEqualTo<TRect>(TRect.InlineCreate(57, 57, 214, 146)));
  CanvasMock
    .Setup
    .Expect
    .Exactly(IfThen(FInternalBitmap, 0, 1))
    .When
    .FillRect(It0.IsEqualTo<TRect>(TRect.InlineCreate(76, 86, 216, 204)));

  DrawBackground(OptionsMock, BitmapMock);

  BrushMock.Verify;

  CanvasMock.Verify;
  UtilsPicturesMock.Verify;
  UtilsScrollBarsMock.Verify;
  WinapiWindowsMock.Verify;

  Options.Control       := nil;
  ParentOptions.Control := nil;
end;

{ TDrawActiveBackgroundWithInternalBitmapTests }

constructor TDrawActiveBackgroundWithInternalBitmapTests.Create;
begin
  inherited Create(True, pf1bit);
end;

{ TDrawActiveBackgroundWithDeviceBitmapTests }

constructor TDrawActiveBackgroundWithDeviceBitmapTests.Create;
begin
  inherited Create(False, pfDevice);
end;

{ TDrawActiveBackgroundWithExternalBitmapTests }

constructor TDrawActiveBackgroundWithExternalBitmapTests.Create;
begin
  inherited Create(False, pf1bit);
end;

initialization
  TDUnitX.RegisterTestFixture(TDrawBackgroundTests);
  TDUnitX.RegisterTestFixture(TDrawActiveBackgroundWithInternalBitmapTests);
  TDUnitX.RegisterTestFixture(TDrawActiveBackgroundWithDeviceBitmapTests);
  TDUnitX.RegisterTestFixture(TDrawActiveBackgroundWithExternalBitmapTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
