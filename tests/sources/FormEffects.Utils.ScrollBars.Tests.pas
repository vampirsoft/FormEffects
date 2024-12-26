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
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.ScrollBars,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TScrollBarUtilsTests }

  [TestFixture]
  TScrollBarUtilsTests = class
  strict private
    FControl: TScrollingWinControl;

  strict private
    function SetupMocks(const Kind: TScrollBarKind): TMock<TControlScrollBar>; inline;
    function IsScrollBarVisible(const Kind: TScrollBarKind): Boolean; inline;

  public
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('have not Vertical scrollbar',   'sbVertical')]
    [TestCase('have not Horizontal scrollbar', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_False_If_No_ScrollingWinControl(const Kind: TScrollBarKind);

    [Test]
    [TestCase('have not visible Vertical scrollbar',   'sbVertical')]
    [TestCase('have not visible Horizontal scrollbar', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_False_If_ScrollingWinControl(const Kind: TScrollBarKind);

    [Test]
    [TestCase('have not Vertical scroll style',   'sbVertical')]
    [TestCase('have not Horizontal scroll style', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_False_If_ScrollingWinControl_With_Scrollbars(const Kind: TScrollBarKind);

    [Test]
    [TestCase('have Vertical with min pos',   'sbVertical')]
    [TestCase('have Horizontal with min pos', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_True_If_No_ScrollingWinControl(const Kind: TScrollBarKind);

    [Test]
    [TestCase('have Vertical with max pos',   'sbVertical')]
    [TestCase('have Horizontal with max pos', 'sbHorizontal')]
    procedure IsScrollBarVisible_should_Return_True_If_ScrollingWinControl(const Kind: TScrollBarKind);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Math;

{$IFDEF FORM_EFFECTS_TESTS}

{ TScrollBarUtilsTests }

function TScrollBarUtilsTests.IsScrollBarVisible(const Kind: TScrollBarKind): Boolean;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teRender.IsScrollBarVisible(FControl, 0, Kind);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.ScrollBars.IsScrollBarVisible(FControl, 0, Kind);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_False_If_No_ScrollingWinControl(
  const Kind: TScrollBarKind);
begin
  SetResult_GetWindowLongPtr(0);

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(False, Actual);
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_False_If_ScrollingWinControl(
  const Kind: TScrollBarKind);
begin
  const ScrollBarMock =  SetupMocks(Kind);

  ScrollBarMock.Setup.WillReturn(False).When.GetVisible;

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(False, Actual);
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_False_If_ScrollingWinControl_With_Scrollbars(
  const Kind: TScrollBarKind);
begin
  SetupMocks(Kind);

  const Style = IfThen(Kind = sbVertical, WS_VSCROLL, WS_HSCROLL);
  SetResult_GetWindowLongPtr(not Style);

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(False, Actual);
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_True_If_No_ScrollingWinControl(
  const Kind: TScrollBarKind);
begin
  FControl := nil;

  const Style = IfThen(Kind = sbVertical, WS_VSCROLL, WS_HSCROLL);
  SetResult_GetWindowLongPtr(Style);

  SetResult_GetScrollInfo(
    function(const hWnd: HWND; const BarFlag: Integer): TScrollInfo
    begin
      Result.nMin := 9;
      Result.nMax := 0;
    end
  );

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(True, Actual);
  Assert.AreEqual(IfThen(Kind = sbVertical, SB_VERT, SB_HORZ), GetScrollInfo_BarFlag);
end;

procedure TScrollBarUtilsTests.IsScrollBarVisible_should_Return_True_If_ScrollingWinControl(const Kind: TScrollBarKind);
begin
  SetupMocks(Kind);

  const Style = IfThen(Kind = sbVertical, WS_VSCROLL, WS_HSCROLL);
  SetResult_GetWindowLongPtr(Style);

  SetResult_GetScrollInfo(
    function(const hWnd: HWND; const BarFlag: Integer): TScrollInfo
    begin
      Result.nMin := 0;
      Result.nMax := 9;
    end
  );

  const Actual = IsScrollBarVisible(Kind);
  Assert.AreEqual(True, Actual);
  Assert.AreEqual(IfThen(Kind = sbVertical, SB_VERT, SB_HORZ), GetScrollInfo_BarFlag);
end;

function TScrollBarUtilsTests.SetupMocks(const Kind: TScrollBarKind): TMock<TControlScrollBar>;
begin
  Result           := TMock<TControlScrollBar>.Create;
  const ControlMock = TMock<TScrollingWinControl>.Create;

  if Kind = sbHorizontal then ControlMock.Setup.WillReturn(Result.Instance).When.GetHorzScrollBar
  else ControlMock.Setup.WillReturn(Result.Instance).When.GetVertScrollBar;

  FControl := ControlMock.Instance;
end;

procedure TScrollBarUtilsTests.TearDown;
begin
  GetScrollInfo_Wnd     := HWND(-1);
  GetScrollInfo_BarFlag := -1;

  SetResult_GetScrollInfo(nil);
end;

initialization
  TDUnitX.RegisterTestFixture(TScrollBarUtilsTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
