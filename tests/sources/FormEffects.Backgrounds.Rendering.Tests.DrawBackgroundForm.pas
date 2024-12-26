///////////////////////////////////////////////////////////////////////////////////////
//***********************************************************************************//
//* Project      : FormEffects                                                      *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                        *//
//* Unit Name    : FormEffects.Backgrounds.Rendering.Tests.DrawBackgroundForm.pas   *//
//* Author       : Сергей (LordVampir) Дворников                                    *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                       *//
//* Licensed under MIT                                                              *//
//***********************************************************************************//
///////////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Rendering.Tests.DrawBackgroundForm;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  System.Types,
  Vcl.Graphics,
  Vcl.Forms,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Constants,
  FormEffects.Backgrounds,
  FormEffects.Backgrounds.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Backgrounds.Tests,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{$IFDEF USE_BILLENIUM_EFFECTS}

{ TBackgroundOptions }

  TBackgroundOptions = class(FormEffects.Backgrounds.Tests.TBackgroundOptions)
  protected
    function GetBkgrndForm: TCustomForm; override;
  end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TDrawBackgroundFormTests }

  [TestFixture]
  TDrawBackgroundFormTests = class
  strict private
    FCanvasDC: HDC;
    FControlWnd: HWND;
    FParentWnd: HWND;
    FFormWnd: HWND;

  strict private
    procedure DrawBackgroundForm(const Control: TWinControl; const Options: TBackgroundOptions; Bitmap: TBitmap);

  public
    [Setup]
    procedure Setup;

  public
    [Test]
    [TestCase('DrawBackgroundForm не должен добавлять offset и делать проверку региона,  если Control = ParentControl', 'True')]
    [TestCase('DrawBackgroundForm не должен добавлять offset и сделать проверку региона, если Control = ParentControl', 'False')]
    procedure should_not_add_offset_for_control_equals_parent(const CheckRegion: Boolean);

    [Test]
    [TestCase('DrawBackgroundForm должен добавлять offset и делать проверку региона,  если Control <> ParentControl', 'True')]
    [TestCase('DrawBackgroundForm должен добавлять offset и сделать проверку региона, если Control <> ParentControl', 'False')]
    procedure should_add_offset_for_control_not_equal_parent(const CheckRegion: Boolean);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  System.Math,
  FormEffects.TypeHelpers,
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Utils.ScrollBars.Mocks,
  FormEffects.Rendering.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$IFDEF USE_BILLENIUM_EFFECTS}

{ TBackgroundOptions }

function TBackgroundOptions.GetBkgrndForm: TCustomForm;
begin
  Result := GetBackgoundForm;
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TDrawBackgroundFormTests }

procedure TDrawBackgroundFormTests.DrawBackgroundForm(
  const Control: TWinControl;
  const Options: TBackgroundOptions;
  Bitmap: TBitmap
);
begin
  const Rect     = TRect.InlineCreate(278, 38, 397, 127);
  const DrawRect = TRect.InlineCreate(467, 327, 627, 537);
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd.DrawBkgrndForm(
    Options,
    Control,
    Bitmap,
    Rect,
    DrawRect,
    0,
    0,
    pfDevice
  );
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Backgrounds.Rendering.DrawBackgroundForm(
    Control,
    Options.Control,
    Options.GetBackgoundForm,
    Bitmap.Canvas.Handle,
    Rect,
    DrawRect
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TDrawBackgroundFormTests.Setup;
begin
  FCanvasDC   := 199;
  FControlWnd := 299;
  FParentWnd  := 399;
  FFormWnd    := 499;
end;

procedure TDrawBackgroundFormTests.should_add_offset_for_control_not_equal_parent(const CheckRegion: Boolean);
begin
  const WinapiWindowsMock   = TMock<TWinapiWindowsMocks>.Create;
  const UtilsScrollBarsMock = TMock<TUtilsScrollBarsMocks>.Create;
  const RenderingMock       = TMock<TRenderingMocks>.Create;

  const OptionsMock    = TMock<TBackgroundOptions>.Create;
  const ControlMock    = TMock<TWinControl>.Create;
  const ParentMock     = TMock<TWinControl>.Create;
  const CustomFormMock = TMock<TCustomForm>.Create;

  const CanvasMock = TMock<TCanvas>.Create;
  const BitmapMock = TMock<TBitmap>.Create;

  const Options    = OptionsMock.Instance;
  const Control    = ControlMock.Instance;
  const Parent     = ParentMock.Instance;
  const CustomForm = CustomFormMock.Instance;

  const Canvas = CanvasMock.Instance;
  const Bitmap = BitmapMock.Instance;

  const ClipRgnForCheck : HRGN = 599;
  const ClipRgnForSelect: HRGN = 699;

  ControlMock
    .Setup
    .WillReturn(FControlWnd)
    .When
    .GetHandle;
//  ControlMock
//    .Setup
//    .WillReturn(TPoint.InlineCreate(79, 29))
//    .When
//    .GetClientOrigin;

  ParentMock
    .Setup
    .WillReturn(FParentWnd)
    .When
    .GetHandle;
  ParentMock
    .Setup
    .WillReturn(TPoint.InlineCreate(79, 29))
    .When
    .GetClientOrigin;

  CustomFormMock
    .Setup
    .WillReturn(FFormWnd)
    .When
    .GetHandle;

  OptionsMock
    .Setup
    .WillReturn(CustomForm)
    .When
    .GetBackgoundForm;

  CanvasMock
    .Setup
    .WillReturn(FCanvasDC)
    .When
    .GetHandle;

  BitmapMock
    .Setup
    .WillReturn(Canvas)
    .When
    .GetCanvas;

  UtilsScrollBarsMock
    .Setup
    .WillReturn(True)
    .When
    .IsScrollBarVisible(
      It0.IsEqualTo<HWND>(FParentWnd),
      It1.IsEqualTo<TControl>(Parent),
      It2.IsAny<TScrollBarKind>
    );
  UtilsScrollBarsMock
    .Setup
    .WillReturn(True)
    .When
    .IsScrollBarVisible(
      It0.IsEqualTo<HWND>(FControlWnd),
      It1.IsEqualTo<TControl>(Control),
      It2.IsAny<TScrollBarKind>
    );

  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(23, 57))
    .When
    .GetScrollInfo(
      It0.IsEqualTo<HWND>(FParentWnd),
      It1.IsEqualTo<Integer>(SB_HORZ)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(11, 41))
    .When
    .GetScrollInfo(
      It0.IsEqualTo<HWND>(FParentWnd),
      It1.IsEqualTo<Integer>(SB_VERT)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(23, 57))
    .When
    .GetScrollInfo(
      It0.IsEqualTo<HWND>(FControlWnd),
      It1.IsEqualTo<Integer>(SB_HORZ)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(11, 41))
    .When
    .GetScrollInfo(
      It0.IsEqualTo<HWND>(FControlWnd),
      It1.IsEqualTo<Integer>(SB_VERT)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(ClipRgnForCheck)
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(0),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(0)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(ClipRgnForSelect)
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(268),
      It1.IsEqualTo<Integer>(128),
      It2.IsEqualTo<Integer>(428),
      It3.IsEqualTo<Integer>(338)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SelectClipRgn(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<HRGN>(ClipRgnForSelect)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteObject(
      It0.IsEqualTo<HGDIOBJ>(ClipRgnForSelect)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(IfThen(CheckRegion, 0, 1))
    .When
    .GetClipRgn(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<HRGN>(ClipRgnForCheck)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(TPoint.InlineCreate(137, 57))
    .When
    .OffsetWindowOrgEx(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<Integer>(166),
      It2.IsEqualTo<Integer>(170)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SetWindowOrgEx(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<Integer>(137),
      It2.IsEqualTo<Integer>(57)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SelectClipRgn(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<HRGN>(IfThen(CheckRegion, 0, ClipRgnForCheck))
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteObject(
      It0.IsEqualTo<HGDIOBJ>(ClipRgnForCheck)
    );

  RenderingMock
    .Setup
    .Expect
    .Once
    .When
    .RenderWindowToDC(
      It0.IsEqualTo<HWND>(FFormWnd),
      It1.IsEqualTo<HWND>(0),
      It2.IsEqualTo<TWinControl>(CustomForm),
      It3.IsEqualTo<HDC>(FCanvasDC),
      It4.IsEqualTo<TRect>(TRect.Create(478, 238, 597, 327)),
      It5.IsEqualTo<Boolean>(True),
      It6.IsEqualTo<Boolean>(True),
      It7.IsEqualTo<Boolean>(False)
    );

  CustomFormMock
    .Setup
    .WillReturn(TRect.InlineCreate(45, -1, 102, 40))
    .When
    .GetBoundsRect;
  CustomFormMock
    .Setup
    .Expect
    .Never
    .When
    .SetBounds(
      It0.IsAny<Integer>,
      It0.IsAny<Integer>,
      It0.IsAny<Integer>,
      It0.IsAny<Integer>
    );

  Options.Control := Parent;

  DrawBackgroundForm(Control, Options, Bitmap);

  CustomFormMock.Verify;
  RenderingMock.Verify;
  WinapiWindowsMock.Verify;

  Options.Control := nil;
end;

procedure TDrawBackgroundFormTests.should_not_add_offset_for_control_equals_parent(const CheckRegion: Boolean);
begin
  const WinapiWindowsMock   = TMock<TWinapiWindowsMocks>.Create;
  const UtilsScrollBarsMock = TMock<TUtilsScrollBarsMocks>.Create;
  const RenderingMock       = TMock<TRenderingMocks>.Create;

  const OptionsMock    = TMock<TBackgroundOptions>.Create;
  const ControlMock    = TMock<TWinControl>.Create;
  const CustomFormMock = TMock<TCustomForm>.Create;

  const CanvasMock = TMock<TCanvas>.Create;
  const BitmapMock = TMock<TBitmap>.Create;

  const Options    = OptionsMock.Instance;
  const Control    = ControlMock.Instance;
  const CustomForm = CustomFormMock.Instance;

  const Canvas = CanvasMock.Instance;
  const Bitmap = BitmapMock.Instance;

  const ClipRgnForCheck : HRGN = 599;
  const ClipRgnForSelect: HRGN = 699;

  ControlMock
    .Setup
    .WillReturn(FControlWnd)
    .When
    .GetHandle;
  ControlMock
    .Setup
    .WillReturn(TPoint.InlineCreate(79, 29))
    .When
    .GetClientOrigin;

  CustomFormMock
    .Setup
    .WillReturn(FFormWnd)
    .When
    .GetHandle;

  OptionsMock
    .Setup
    .WillReturn(CustomForm)
    .When
    .GetBackgoundForm;

  CanvasMock
    .Setup
    .WillReturn(FCanvasDC)
    .When
    .GetHandle;

  BitmapMock
    .Setup
    .WillReturn(Canvas)
    .When
    .GetCanvas;

  UtilsScrollBarsMock
    .Setup
    .WillReturn(True)
    .When
    .IsScrollBarVisible(
      It0.IsEqualTo<HWND>(FControlWnd),
      It1.IsEqualTo<TControl>(Control),
      It2.IsAny<TScrollBarKind>
    );

  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(23, 57))
    .When
    .GetScrollInfo(
      It0.IsEqualTo<HWND>(FControlWnd),
      It1.IsEqualTo<Integer>(SB_HORZ)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(11, 41))
    .When
    .GetScrollInfo(
      It0.IsEqualTo<HWND>(FControlWnd),
      It1.IsEqualTo<Integer>(SB_VERT)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(ClipRgnForCheck)
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(0),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(0)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(ClipRgnForSelect)
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(268),
      It1.IsEqualTo<Integer>(128),
      It2.IsEqualTo<Integer>(428),
      It3.IsEqualTo<Integer>(338)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SelectClipRgn(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<HRGN>(ClipRgnForSelect)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteObject(
      It0.IsEqualTo<HGDIOBJ>(ClipRgnForSelect)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(IfThen(CheckRegion, 0, 1))
    .When
    .GetClipRgn(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<HRGN>(ClipRgnForCheck)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(TPoint.InlineCreate(137, 57))
    .When
    .OffsetWindowOrgEx(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<Integer>(166),
      It2.IsEqualTo<Integer>(170)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SetWindowOrgEx(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<Integer>(137),
      It2.IsEqualTo<Integer>(57)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SelectClipRgn(
      It0.IsEqualTo<HDC>(FCanvasDC),
      It1.IsEqualTo<HRGN>(IfThen(CheckRegion, 0, ClipRgnForCheck))
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteObject(
      It0.IsEqualTo<HGDIOBJ>(ClipRgnForCheck)
    );

  RenderingMock
    .Setup
    .Expect
    .Once
    .When
    .RenderWindowToDC(
      It0.IsEqualTo<HWND>(FFormWnd),
      It1.IsEqualTo<HWND>(0),
      It2.IsEqualTo<TWinControl>(CustomForm),
      It3.IsEqualTo<HDC>(FCanvasDC),
      It4.IsEqualTo<TRect>(TRect.Create(478, 238, 597, 327)),
      It5.IsEqualTo<Boolean>(True),
      It6.IsEqualTo<Boolean>(True),
      It7.IsEqualTo<Boolean>(False)
    );

  CustomFormMock
    .Setup
    .Expect
    .Once
    .When
    .SetBounds(
      It0.IsEqualTo<Integer>(45),
      It1.IsEqualTo<Integer>(-1),
      It2.IsEqualTo<Integer>(57),
      It3.IsEqualTo<Integer>(41)
    );

  Options.Control := Control;

  DrawBackgroundForm(Control, Options, Bitmap);

  CustomFormMock.Verify;
  RenderingMock.Verify;
  WinapiWindowsMock.Verify;

  Options.Control := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TDrawBackgroundFormTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
