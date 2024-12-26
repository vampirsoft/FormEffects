/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.PictureRect.Tests.pas                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Rects.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Forms,
  FormEffects.Constants,
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TRectUtilsTests }

  [TestFixture]
  TRectUtilsTests = class
  strict private const
    FThisControlWnd: HWND = 159;
    FOrgControlWnd : HWND = 268;
    FClientWnd     : HWND = 223;

  strict private
    FGraphic: TGraphic;
    FThisControl: TWinControl;
    FOrgControl: TWinControl;
    FDrawRect: TRect;

  strict private
    function PictureRect(const PictureMode: TBackgroundPictureMode): TRect;

  public
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('PictureRect должен вернуть TRect.Zero, если Graphic является неопределён', '')]
    procedure PictureRect_should_return_zero_rect_if_graphic_is_null;

    [Test]
    [TestCase('PictureRect должен вернуть TRect.Zero, если Graphic Имеет Width = 0',  'True')]
    [TestCase('PictureRect должен вернуть TRect.Zero, если Graphic Имеет Height = 0', 'False')]
    procedure PictureRect_should_return_zero_rect_if_graphic(const IsVertical: Boolean);

    [Test]
    [TestCase('PictureRect должен вернуть TRect для PictureMode = Center',        'Center,        75,   69, 624, 463')]
    [TestCase('PictureRect должен вернуть TRect для PictureMode = CenterStretch', 'CenterStretch, 96,   84, 601, 447')]
    [TestCase('PictureRect должен вернуть TRect для PictureMode = Stretch',       'Stretch,       84,   84, 614, 447')]
    [TestCase('PictureRect должен вернуть TRect для PictureMode = Tile',          'Tile,          84,   84, 614, 447')]
    [TestCase('PictureRect должен вернуть TRect для PictureMode = Zoom',          'Zoom,          84,   84, 614, 447')]
    [TestCase('PictureRect должен вернуть TRect для PictureMode = TopLeft',       'TopLeft,       84,   84, 614, 447')]
    [TestCase('PictureRect должен вернуть TRect для PictureMode = TopRight',      'TopRight,    -103,   84, 446, 478')]
    [TestCase('PictureRect должен вернуть TRect для PictureMode = BottomLeft',    'BottomLeft,    84, -115, 633, 279')]
    [TestCase('PictureRect должен вернуть TRect для PictureMode = BottomRight',   'BottomRight, -103, -115, 446, 279')]
    procedure PictureRect_should_return_rect(
      const PictureMode: TBackgroundPictureMode;
      const Left, Top, Right, Bottom: Integer
    );

    [Test]
    [TestCase('PictureRect должен вызвать IsScrollBarVisible с Control = null для MDI формы',  'fsMDIForm, -338, -261, 211, 133')]
    [TestCase('PictureRect должен вызвать IsScrollBarVisible с Control для не MDI формы',      'fsNormal,  -456, -341,  93,  53')]
    procedure PictureRect_should_return_rect_for_control_with_scrolls(
      const FormStyle: TFormStyle;
      const Left, Top, Right, Bottom: Integer
    );
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  System.Types,
  System.Rtti,
  System.SysUtils,
  System.Math,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Rects,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Utils.ScrollBars.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TRectUtilsTests }

function TRectUtilsTests.PictureRect(const PictureMode: TBackgroundPictureMode): TRect;
begin
  const Margin = 84;
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teBkgrnd.PictureRect(FGraphic, PictureMode.Resolve, Margin, FThisControl, FOrgControl, FDrawRect);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Rects.PictureRect(FGraphic, PictureMode, FThisControl, FOrgControl, Margin, FDrawRect);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TRectUtilsTests.PictureRect_should_return_rect(
  const PictureMode: TBackgroundPictureMode;
  const Left, Top, Right, Bottom: Integer
);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const GraphicMock    = TMock<TGraphic>.Create;
  const CustomFormMock = TMock<TCustomForm>.Create;

  CustomFormMock
    .Setup
    .WillReturn(FOrgControlWnd)
    .When
    .GetHandle;
  CustomFormMock
    .Setup
    .WillReturn(FClientWnd)
    .When
    .GetClientHandle;
  CustomFormMock
    .Setup
    .WillReturn(fsMDIForm)
    .When
    .GetFormStyle;

  FThisControl := CustomFormMock.Instance;
  FOrgControl  := CustomFormMock.Instance;
  FGraphic     := GraphicMock.Instance;

  WinapiWindowsMock
    .Setup
    .WillReturn(TRect.Create(279, 215, 977, 746))
    .When
    .GetClientRect(It0.IsEqualTo<HWND>(FClientWnd));

  const Actual = PictureRect(PictureMode);

  Assert.AreEqual<TRect>(TRect.Create(Left, Top, Right, Bottom), Actual);
  Assert.AreEqual<TRect>(TRect.Create(84, 84, 614, 447), FDrawRect);
end;

procedure TRectUtilsTests.PictureRect_should_return_rect_for_control_with_scrolls(
  const FormStyle: TFormStyle;
  const Left, Top, Right, Bottom: Integer
);
begin
  const WinapiWindowsMock   = TMock<TWinapiWindowsMocks>.Create;
  const UtilsScrollBarsMock = TMock<TUtilsScrollBarsMocks>.Create;

  const GraphicMock     = TMock<TGraphic>.Create;
  const ThisControlMock = TMock<TWinControl>.Create;
  const OrgControlMock  = TMock<TCustomForm>.Create;

  ThisControlMock
    .Setup
    .WillReturn(FThisControlWnd)
    .When
    .GetHandle;
  OrgControlMock
    .Setup
    .WillReturn(FOrgControlWnd)
    .When
    .GetHandle;
  OrgControlMock
    .Setup
    .WillReturn(FClientWnd)
    .When
    .GetClientHandle;
  OrgControlMock
    .Setup
    .WillReturn(FormStyle)
    .When
    .GetFormStyle;

  FThisControl := ThisControlMock.Instance;
  FOrgControl  := OrgControlMock.Instance;
  FGraphic     := GraphicMock.Instance;

  var Control: TControl;
  var Wnd: HWND;
  if FormStyle = fsMDIForm then
  begin
    Control := nil;
    Wnd     := FClientWnd;
  end
  else
  begin
    Control := FOrgControl;
    Wnd     := FOrgControlWnd;
  end;

  UtilsScrollBarsMock
    .Setup
    .Expect
    .Exactly(2)
    .When
    .IsScrollBarVisible(It0.IsEqualTo<HWND>(Wnd), It1.IsEqualTo<TControl>(Control), It2.IsAny<TScrollBarKind>);
  UtilsScrollBarsMock
    .Setup
    .Expect
    .Exactly(2)
    .When
    .IsScrollBarVisible(
      It0.IsEqualTo<HWND>(FThisControlWnd),
      It1.IsEqualTo<TControl>(FThisControl),
      It2.IsAny<TScrollBarKind>
    );
  UtilsScrollBarsMock
    .Setup
    .WillReturn(True)
    .When
    .IsScrollBarVisible(It0.IsAny<HWND>, It1.IsAny<TControl>, It2.IsAny<TScrollBarKind>);

  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(0, 196))
    .When
    .GetScrollInfo(It0.IsEqualTo<HWND>(FOrgControlWnd), It1.IsEqualTo<Integer>(SB_HORZ));
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(0, 114))
    .When
    .GetScrollInfo(It0.IsEqualTo<HWND>(FOrgControlWnd), It1.IsEqualTo<Integer>(SB_VERT));
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(0, 123))
    .When
    .GetScrollInfo(It0.IsNotIn<HWND>([FOrgControlWnd]), It1.IsEqualTo<Integer>(SB_HORZ));
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(0, 79))
    .When
    .GetScrollInfo(It0.IsNotIn<HWND>([FOrgControlWnd]), It1.IsEqualTo<Integer>(SB_VERT));

  const Actual = PictureRect(TBackgroundPictureMode.Center);

  Assert.AreEqual<TRect>(TRect.Create(Left, Top, Right, Bottom), Actual);
  UtilsScrollBarsMock.Verify;
end;

procedure TRectUtilsTests.PictureRect_should_return_zero_rect_if_graphic(const IsVertical: Boolean);
begin
  const GraphicMock     = TMock<TGraphic>.Create;
  const ThisControlMock = TMock<TWinControl>.Create;
  const OrgControlMock  = TMock<TWinControl>.Create;

  if IsVertical then
    GraphicMock
      .Setup
      .WillReturn(0)
      .When
      .GetWidth
  else
    GraphicMock
      .Setup
      .WillReturn(0)
      .When
      .GetHeight;

  FThisControl := ThisControlMock.Instance;
  FOrgControl  := OrgControlMock.Instance;
  FGraphic     := GraphicMock.Instance;

  const Actual = PictureRect(TBackgroundPictureMode.Center);

  Assert.AreEqual<TRect>(TRect.Zero, Actual);
end;

procedure TRectUtilsTests.PictureRect_should_return_zero_rect_if_graphic_is_null;
begin
  const ThisControlMock = TMock<TWinControl>.Create;
  const OrgControlMock  = TMock<TWinControl>.Create;

  FThisControl := ThisControlMock.Instance;
  FOrgControl  := OrgControlMock.Instance;

  const Actual = PictureRect(TBackgroundPictureMode.Center);

  Assert.AreEqual<TRect>(TRect.Zero, Actual);
end;

procedure TRectUtilsTests.TearDown;
begin
  FOrgControl  := nil;
  FThisControl := nil;
  FGraphic     := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TRectUtilsTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
