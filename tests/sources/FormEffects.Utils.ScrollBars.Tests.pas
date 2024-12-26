/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.IsScrollBarVisible.Tests.pas         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.ScrollBars.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Vcl.Forms,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TScrollBarUtilsTests }

  [TestFixture]
  TScrollBarUtilsTests = class
  strict private
    FScrollBarMock: TMock<TControlScrollBar>;
    FControlMock: TMock<TScrollingWinControl>;

    FControl: TScrollingWinControl;

  strict private
    procedure SetupMocks(const Kind: TScrollBarKind);
    function IsScrollBarVisible(const Kind: TScrollBarKind): Boolean;

  public
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('IsScrollBarVisible должен вернуть False, если не ScrollingWinControl не имеет вертикальный ScrollBar',   'sbVertical')]
    [TestCase('IsScrollBarVisible должен вернуть False, если не ScrollingWinControl не имеет горизонтальный ScrollBar', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_False_If_No_ScrollingWinControl(const Kind: TScrollBarKind);

    [Test]
    [TestCase('IsScrollBarVisible должен вернуть False, если ScrollingWinControl не имеет вертикальный ScrollBar',   'sbVertical')]
    [TestCase('IsScrollBarVisible должен вернуть False, если ScrollingWinControl не имеет горизонтальный ScrollBar', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_False_If_ScrollingWinControl(const Kind: TScrollBarKind);

    [Test]
    [TestCase('IsScrollBarVisible должен вернуть False, если ScrollingWinControl не имеет вертикального стиль скролла',   'sbVertical')]
    [TestCase('IsScrollBarVisible должен вернуть False, если ScrollingWinControl не имеет горизонтального стиль скролла', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_False_If_ScrollingWinControl_With_Scrollbars(const Kind: TScrollBarKind);

    [Test]
    [TestCase('IsScrollBarVisible должен вернуть False, если не ScrollingWinControl имеет минимальную позицию вертикального скролла',   'sbVertical')]
    [TestCase('IsScrollBarVisible должен вернуть False, если не ScrollingWinControl имеет минимальную позицию горизонтального скролла', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_True_If_No_ScrollingWinControl(const Kind: TScrollBarKind);

    [Test]
    [TestCase('IsScrollBarVisible должен вернуть False, если ScrollingWinControl имеет минимальную позицию вертикального скролла',   'sbVertical')]
    [TestCase('IsScrollBarVisible должен вернуть False, если ScrollingWinControl имеет минимальную позицию горизонтального скролла', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_True_If_ScrollingWinControl(const Kind: TScrollBarKind);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.SysUtils,
  System.Math,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.ScrollBars,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Winapi.Windows.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TScrollBarUtilsTests }

function TScrollBarUtilsTests.IsScrollBarVisible(const Kind: TScrollBarKind): Boolean;
begin
  const Wnd: HWND = 0;
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teRender.IsScrollBarVisible(FControl, Wnd, Kind);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.ScrollBars.IsScrollBarVisible(Wnd, FControl, Kind);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_False_If_No_ScrollingWinControl(
  const Kind: TScrollBarKind
);
begin
  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(False, Actual);
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_False_If_ScrollingWinControl(
  const Kind: TScrollBarKind
);
begin
  SetupMocks(Kind);

  FScrollBarMock
    .Setup
    .WillReturn(False)
    .When
    .GetVisible;

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(False, Actual);
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_False_If_ScrollingWinControl_With_Scrollbars(
  const Kind: TScrollBarKind
);
begin
  SetupMocks(Kind);
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const Style = IfThen(Kind = sbVertical, WS_VSCROLL, WS_HSCROLL);
  WinapiWindowsMock
    .Setup
    .WillReturn(not Style)
    .When
    .GetWindowLongPtr(It0.IsEqualTo<HWND>(0), It1.IsEqualTo<Integer>(GWL_STYLE));

  const Actual = IsScrollBarVisible(Kind);

  Assert.AreEqual(False, Actual);
  WinapiWindowsMock.Verify;
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_True_If_No_ScrollingWinControl(
  const Kind: TScrollBarKind
);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const Style = IfThen(Kind = sbVertical, WS_VSCROLL, WS_HSCROLL);
  WinapiWindowsMock
    .Setup
    .WillReturn(Style)
    .When
    .GetWindowLongPtr(It0.IsEqualTo<HWND>(0), It1.IsEqualTo<Integer>(GWL_STYLE));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetScrollInfo(It0.IsEqualTo<HWND>(0), It1.IsEqualTo<Integer>(IfThen(Kind = sbVertical, SB_VERT, SB_HORZ)));
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(9, 0))
    .When
    .GetScrollInfo(It0.IsEqualTo<HWND>(0), It1.IsAny<Integer>);

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(True, Actual);
  WinapiWindowsMock.Verify;
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_True_If_ScrollingWinControl(const Kind: TScrollBarKind);
begin
  SetupMocks(Kind);
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const Style = IfThen(Kind = sbVertical, WS_VSCROLL, WS_HSCROLL);
  WinapiWindowsMock
    .Setup
    .WillReturn(Style)
    .When
    .GetWindowLongPtr(It0.IsEqualTo<HWND>(0), It1.IsEqualTo<Integer>(GWL_STYLE));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetScrollInfo(It0.IsEqualTo<HWND>(0), It1.IsEqualTo<Integer>(IfThen(Kind = sbVertical, SB_VERT, SB_HORZ)));
  WinapiWindowsMock
    .Setup
    .WillReturn(TScrollInfo.Create(0, 9))
    .When
    .GetScrollInfo(It0.IsEqualTo<HWND>(0), It1.IsAny<Integer>);

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(True, Actual);
  WinapiWindowsMock.Verify;
end;

procedure TScrollBarUtilsTests.SetupMocks(const Kind: TScrollBarKind);
begin
  FScrollBarMock := TMock<TControlScrollBar>.Create;
  FControlMock   := TMock<TScrollingWinControl>.Create;

  if Kind = sbHorizontal then
  begin
    FControlMock
      .Setup
      .WillReturn(FScrollBarMock.Instance)
      .When
      .GetHorzScrollBar;
  end
  else
  begin
    FControlMock
      .Setup
      .WillReturn(FScrollBarMock.Instance)
      .When
      .GetVertScrollBar;
  end;

  FControl := FControlMock.Instance;
end;

procedure TScrollBarUtilsTests.TearDown;
begin
  FControl := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TScrollBarUtilsTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
