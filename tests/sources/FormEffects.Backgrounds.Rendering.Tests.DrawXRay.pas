/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Rendering.Tests.DrawXRay.pas       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Rendering.Tests.DrawXRay;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Vcl.Graphics,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Constants,
  FormEffects.Backgrounds,
  FormEffects.Backgrounds.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Backgrounds.Tests;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TDrawXRayTests }

  [TestFixture]
  TDrawXRayTests = class
  strict private
    procedure DrawXRay(const Options: TBackgroundOptions; Bitmap: TBitmap);

  public
    [Test]
    [TestCase('DrawXRay не должен выбирать ClipRgn, не должен вызывать ValidateRect', 'False,False')]
    [TestCase('DrawXRay не должен выбирать ClipRgn, должен вызывать ValidateRect',    'False,True')]
    [TestCase('DrawXRay должен выбирать ClipRgn, не должен вызывать ValidateRect',     'True,False')]
    [TestCase('DrawXRay должен выбирать ClipRgn, должен вызывать ValidateRect',        'True,True')]
    procedure should_render_window_to_dc(const CheckRgn, HasUpdateRect: Boolean);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  System.Math,
  FormEffects.TypeHelpers,
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Rendering.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TDrawXRayTests }

procedure TDrawXRayTests.DrawXRay(const Options: TBackgroundOptions; Bitmap: TBitmap);
begin
  const Rect     = TRect.InlineCreate(159, 89, 359, 119);
  const DrawRect = TRect.InlineCreate(279, 129, 569, 459);
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd.DrawXRay(
    Options,
    Bitmap,
    Rect,
    DrawRect,
    0,
    0,
    pfDevice
  );
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Backgrounds.Rendering.DrawXRay(Options.Control, Bitmap.Canvas.Handle, Rect, DrawRect);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TDrawXRayTests.should_render_window_to_dc(const CheckRgn, HasUpdateRect: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const RenderingMock     = TMock<TRenderingMocks>.Create;

  const BackgroundOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock    = TMock<TWinControl>.Create;
  const ParentControlMock = TMock<TWinControl>.Create;

  const CanvasMock = TMock<TCanvas>.Create;
  const BitmapMock = TMock<TBitmap>.Create;

  const Wnd      : HWND = 199;
  const ParentWnd: HWND = 299;

  const CanvasDC: HDC = 399;

  const ClipRgnZero: HRGN = 499;
  const ClipRgn    : HRGN = 599;

  const BackgroundOptions = BackgroundOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  const Canvas = CanvasMock.Instance;
  const Bitmap = BitmapMock.Instance;

  WinControlMock
    .Setup
    .WillReturn(Wnd)
    .When
    .GetHandle;
  WinControlMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .GetParent;

  ParentControlMock
    .Setup
    .WillReturn(ParentWnd)
    .When
    .GetHandle;

  CanvasMock
    .Setup
    .WillReturn(CanvasDC)
    .When
    .GetHandle;

  BitmapMock
    .Setup
    .WillReturn(Canvas)
    .When
    .GetCanvas;

  WinapiWindowsMock
    .Setup
    .WillReturn(ClipRgnZero)
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(0),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(0)
    );

  WinapiWindowsMock
    .Setup
    .WillReturn(IfThen(CheckRgn, 1, 0))
    .When
    .GetClipRgn(It0.IsEqualTo<HDC>(CanvasDC), It1.IsEqualTo<HRGN>(ClipRgnZero));

  WinapiWindowsMock
    .Setup
    .WillReturn(ClipRgn)
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(-120),
      It1.IsEqualTo<Integer>(-270),
      It2.IsEqualTo<Integer>( 170),
      It3.IsEqualTo<Integer>(  60)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SelectClipRgn(It0.IsEqualTo<HDC>(CanvasDC), It1.IsEqualTo<HRGN>(ClipRgn));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteObject(It0.IsEqualTo<HRGN>(ClipRgnZero));
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteObject(It0.IsEqualTo<HRGN>(ClipRgn));

  WinapiWindowsMock
    .Setup
    .WillReturn(TPoint.InlineCreate(29, 49))
    .When
    .OffsetWindowOrgEx(It0.IsEqualTo<HDC>(CanvasDC), It1.IsEqualTo<Integer>(-20), It2.IsEqualTo<Integer>(60));

  WinapiWindowsMock
    .Setup
    .WillReturn(HasUpdateRect)
    .When
    .GetUpdateRect(It0.IsEqualTo<HWND>(Wnd), It1.IsEqualTo(nil), It2.IsEqualTo<Boolean>(False));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(HasUpdateRect, 0, 1))
    .When
    .ValidateRect(It0.IsEqualTo<HWND>(Wnd), It1.IsEqualTo(nil));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SetWindowOrgEx(It0.IsEqualTo<HDC>(CanvasDC), It1.IsEqualTo<Integer>(29), It2.IsEqualTo<Integer>(49));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(CheckRgn, 1, 0))
    .When
    .SelectClipRgn(It0.IsEqualTo<HDC>(CanvasDC), It1.IsEqualTo<HRGN>(ClipRgnZero));
  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(CheckRgn, 0, 1))
    .When
    .SelectClipRgn(It0.IsEqualTo<HDC>(CanvasDC), It1.IsEqualTo<HRGN>(0));

  RenderingMock
    .Setup
    .Expect
    .Once
    .When
    .RenderWindowToDC(
      It0.IsEqualTo<HWND>(ParentWnd),
      It1.IsEqualTo<HWND>(Wnd),
      It2.IsEqualTo<TWinControl>(ParentControl),
      It3.IsEqualTo<HDC>(CanvasDC),
      It4.IsEqualTo<TRect>(TRect.InlineCreate(259, 189, 459, 219)),
      It5.IsEqualTo<Boolean>(True),
      It6.IsEqualTo<Boolean>(False),
      It7.IsEqualTo<Boolean>(False)
    );

  BackgroundOptions.Control := WinControl;

  DrawXRay(BackgroundOptions, Bitmap);

  WinapiWindowsMock.Verify;
  RenderingMock.Verify;
end;

initialization
  TDUnitX.RegisterTestFixture(TDrawXRayTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
