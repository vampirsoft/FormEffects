/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.IsScrollBarVisible.Tests.pas         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
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
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TScrollBarUtilsTests }

  [TestFixture]
  TScrollBarUtilsTests = class
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;
    FControl: TScrollingWinControl;

  strict private
    function SetupMocks(const Kind: TScrollBarKind): TMock<TControlScrollBar>;
    function IsScrollBarVisible(const Kind: TScrollBarKind): Boolean;

  public
    [Setup]
    procedure Setup;
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
  Winapi.Windows,
  System.SysUtils,
  System.Math,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.ScrollBars
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

{$IFDEF FORM_EFFECTS_TESTS}

{ TScrollBarUtilsTests }

function TScrollBarUtilsTests.IsScrollBarVisible(const Kind: TScrollBarKind): Boolean;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teRender.IsScrollBarVisible(FControl, 0, Kind);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.ScrollBars.IsScrollBarVisible(0, FControl, Kind);
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
  const ScrollBarMock = SetupMocks(Kind);

  ScrollBarMock
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

  const Style = IfThen(Kind = sbVertical, WS_VSCROLL, WS_HSCROLL);
  FWinapiWindowsMocks.GetWindowLongPtr_Result(not Style);

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(False, Actual);
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_True_If_No_ScrollingWinControl(
  const Kind: TScrollBarKind
);
begin
  const Style = IfThen(Kind = sbVertical, WS_VSCROLL, WS_HSCROLL);
  FWinapiWindowsMocks.GetWindowLongPtr_Result(Style);

  FWinapiWindowsMocks.GetScrollInfo_Result(
    function(const Wnd: HWND; const BarFlag: Integer): TScrollInfo
    begin
      Assert.AreEqual(IfThen(Kind = sbVertical, SB_VERT, SB_HORZ), BarFlag);

      Result.nMin := 9;
      Result.nMax := 0;
    end
  );

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(True, Actual);
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_True_If_ScrollingWinControl(const Kind: TScrollBarKind);
begin
  SetupMocks(Kind);

  const Style = IfThen(Kind = sbVertical, WS_VSCROLL, WS_HSCROLL);
  FWinapiWindowsMocks.GetWindowLongPtr_Result(Style);

  FWinapiWindowsMocks.GetScrollInfo_Result(
    function(const Wnd: HWND; const BarFlag: Integer): TScrollInfo
    begin
      Assert.AreEqual(IfThen(Kind = sbVertical, SB_VERT, SB_HORZ), BarFlag);

      Result.nMin := 0;
      Result.nMax := 9;
    end
  );

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(True, Actual);
end;

function TScrollBarUtilsTests.SetupMocks(const Kind: TScrollBarKind): TMock<TControlScrollBar>;
begin
  Result           := TMock<TControlScrollBar>.Create;
  const ControlMock = TMock<TScrollingWinControl>.Create;

  if Kind = sbHorizontal then
  begin
    ControlMock
      .Setup
      .WillReturn(Result.Instance)
      .When
      .GetHorzScrollBar;
  end
  else
  begin
    ControlMock
      .Setup
      .WillReturn(Result.Instance)
      .When
      .GetVertScrollBar;
  end;

  FControl := ControlMock.Instance;
end;

procedure TScrollBarUtilsTests.Setup;
begin
  FWinapiWindowsMocks := TWinapiWindowsMocks.Create;
end;

procedure TScrollBarUtilsTests.TearDown;
begin
  FControl := nil;

  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TScrollBarUtilsTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
