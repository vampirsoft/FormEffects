/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.PictureRect.Tests.pas                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Rects.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Forms,
  FormEffects.FormContainer,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks,
  FormEffects.Utils.ScrollBars.Mocks;

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
    FWinapiWindowsMocks: TWinapiWindowsMocks;
    FUtilsScrollBarsMocks: TUtilsScrollBarsMocks;

    FGraphic: TGraphic;
    FThisControl: TWinControl;
    FOrgControl: TWinControl;
    FDrawRect: TRect;

  strict private
    function PictureRect(const PictureMode: TFEBackgroundPictureMode): TRect;

  public
    [Setup]
    procedure Setup;
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
    [TestCase('PictureRect должен вызвать IsScrollBarVisible с Control = null для MDI формы', 'fsMDIForm')]
    [TestCase('PictureRect должен вызвать IsScrollBarVisible с Control для не MDI формы',     'fsNormal')]
    procedure PictureRect_should_invoke_IsScrollBarVisible_with(const FormStyle: TFormStyle);

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
      const PictureMode: TFEBackgroundPictureMode;
      const Left, Top, Right, Bottom: Integer
    );

    [Test]
    [TestCase('PictureRect должен вернуть TRect для Control со cкролом для PictureMode = Center',        'Center,      -310, -271,  239,  123')]
    [TestCase('PictureRect должен вернуть TRect для Control со cкролом для PictureMode = CenterStretch', 'CenterStretch, 48,  -14, -120, -134')]
    [TestCase('PictureRect должен вернуть TRect для Control со cкролом для PictureMode = Stretch',       'Stretch,       48,   10, -120, -158')]
    [TestCase('PictureRect должен вернуть TRect для Control со cкролом для PictureMode = Tile',          'Tile,          48,   10, -120, -158')]
    [TestCase('PictureRect должен вернуть TRect для Control со cкролом для PictureMode = Zoom',          'Zoom,          48,   10, -120, -158')]
    [TestCase('PictureRect должен вернуть TRect для Control со cкролом для PictureMode = TopLeft',       'TopLeft,      -36,  -74,  -36,  -74')]
    [TestCase('PictureRect должен вернуть TRect для Control со cкролом для PictureMode = TopRight',      'TopRight,    -837,   10, -288,  404')]
    [TestCase('PictureRect должен вернуть TRect для Control со cкролом для PictureMode = BottomLeft',    'BottomLeft,    48, -720,  597, -326')]
    [TestCase('PictureRect должен вернуть TRect для Control со cкролом для PictureMode = BottomRight',   'BottomRight, -837, -720, -288, -326')]
    procedure PictureRect_should_return_rect_for_control_with_scrolls(
      const PictureMode: TFEBackgroundPictureMode;
      const Left, Top, Right, Bottom: Integer
    );
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
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
  FormEffects.TypeHelpers.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TRectUtilsTests }

function TRectUtilsTests.PictureRect(const PictureMode: TFEBackgroundPictureMode): TRect;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teBkgrnd.PictureRect(FGraphic, PictureMode.Cast, 84, FThisControl, FOrgControl, FDrawRect);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Rects.PictureRect(FGraphic, PictureMode, FThisControl, FOrgControl, 84, FDrawRect);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TRectUtilsTests.PictureRect_should_invoke_IsScrollBarVisible_with(const FormStyle: TFormStyle);
begin
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
    .WillReturn(FormStyle.AsValue)
    .When
    .GetFormStyle;

  const CustomForm = CustomFormMock.Instance;

  FThisControl := CustomForm;
  FOrgControl  := CustomForm;
  FGraphic     := GraphicMock.Instance;

  FUtilsScrollBarsMocks.IsScrollBarVisible_Result(
    function(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean
    begin
      if FormStyle = fsMDIForm then
      begin
        Assert.IsNull(Control);
        Assert.AreEqual(FClientWnd, Wnd);
      end
      else
      begin
        Assert.AreEqual<TControl>(CustomForm, Control);
        Assert.AreEqual(FOrgControlWnd, Wnd);
      end;

      Result := False;
    end
  );

  PictureRect(TFEBackgroundPictureMode.Center);
end;

procedure TRectUtilsTests.PictureRect_should_return_rect(
  const PictureMode: TFEBackgroundPictureMode;
  const Left, Top, Right, Bottom: Integer
);
begin
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
    .WillReturn(fsMDIForm.AsValue)
    .When
    .GetFormStyle;

  FThisControl := CustomFormMock.Instance;
  FOrgControl  := CustomFormMock.Instance;
  FGraphic     := GraphicMock.Instance;

  FWinapiWindowsMocks.GetClientRect_Result(TRect.Create(279, 215, 977, 746));

  const Actual = PictureRect(PictureMode);
  Assert.AreEqual<TRect>(TRect.Create(Left, Top, Right, Bottom), Actual);
  Assert.AreEqual<TRect>(TRect.Create(84, 84, 614, 447), FDrawRect);
end;

procedure TRectUtilsTests.PictureRect_should_return_rect_for_control_with_scrolls(
  const PictureMode: TFEBackgroundPictureMode;
  const Left, Top, Right, Bottom: Integer
);
begin
  const GraphicMock     = TMock<TGraphic>.Create;
  const ThisControlMock = TMock<TWinControl>.Create;
  const OrgControlMock  = TMock<TWinControl>.Create;

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

  FThisControl := ThisControlMock.Instance;
  FOrgControl  := OrgControlMock.Instance;
  FGraphic     := GraphicMock.Instance;

  FUtilsScrollBarsMocks.IsScrollBarVisible_Result(True);
  FWinapiWindowsMocks.GetScrollInfo_Result(
    function(const Wnd: HWND; const BarFlag: Integer): TScrollInfo
    begin
      if Wnd = FThisControlWnd then
        Result.nPos := IfThen(BarFlag = SB_HORZ, 196, 114)
      else
        Result.nPos := IfThen(BarFlag = SB_HORZ, 123, 79);
    end
  );

  const Actual = PictureRect(PictureMode);
  Assert.AreEqual<TRect>(TRect.Create(Left, Top, Right, Bottom), Actual);
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

  const Actual = PictureRect(TFEBackgroundPictureMode.Center);
  Assert.AreEqual<TRect>(TRect.Zero, Actual);
end;

procedure TRectUtilsTests.PictureRect_should_return_zero_rect_if_graphic_is_null;
begin
  const ThisControlMock = TMock<TWinControl>.Create;
  const OrgControlMock  = TMock<TWinControl>.Create;

  FThisControl := ThisControlMock.Instance;
  FOrgControl  := OrgControlMock.Instance;

  const Actual = PictureRect(TFEBackgroundPictureMode.Center);
  Assert.AreEqual<TRect>(TRect.Zero, Actual);
end;

procedure TRectUtilsTests.Setup;
begin
  FWinapiWindowsMocks   := TWinapiWindowsMocks.Create;
  FUtilsScrollBarsMocks := TUtilsScrollBarsMocks.Create;
end;

procedure TRectUtilsTests.TearDown;
begin
  FOrgControl  := nil;
  FThisControl := nil;
  FGraphic     := nil;

  FreeAndNil(FUtilsScrollBarsMocks);
  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TRectUtilsTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
