/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.PictureRect.Tests.pas                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Rects.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Vcl.Controls, Vcl.Forms,
  FormEffects.FormContainer,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Rects,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks,
  FormEffects.Utils.ScrollBars.Mocks;

type

{$IFDEF USE_BILLENIUM_EFFECTS}

{ TFEBackgroundPictureModeHelper }

  TFEBackgroundPictureModeHelper = record helper for TFEBackgroundPictureMode
  public
    function GetPictureMode: TFCPictureMode;
  end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TRectUtilsTests }

  [TestFixture]
  TRectUtilsTests = class
  strict private const
    FControlHandle   : HWND = 159;
    FWinControlHandle: HWND = 268;
    FClientHandle    : HWND = 223;

  strict private
    FGraphic: TGraphic;
    FControl: TControl;
    FWinControl: TWinControl;
    FDrawRect: TRect;

  strict private
    function GetZeroRect: TRect; inline;
    function PictureRect(const PictureMode: TFEBackgroundPictureMode): TRect; inline;

  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    
  public
    [Test]
    procedure PictureRect_should_return_zero_rect_if_graphic_is_null;

    [Test]
    [TestCase('have width is 0', 'True')]
    [TestCase('have height is 0', 'False')]
    procedure PictureRect_should_return_zero_rect_if_graphic(const IsVertical: Boolean);

    [Test]
    [TestCase('null as Control for MDI from', 'fsMDIForm')]
    [TestCase('win control for no MDI from', 'fsNormal')]
    procedure PictureRect_should_invoke_IsScrollBarVisible_with(const FormStyle: TFormStyle);

    [Test]
    [TestCase('for Center',        'Center,        75,   69, 624, 463')]
    [TestCase('for CenterStretch', 'CenterStretch, 96,   84, 601, 447')]
    [TestCase('for Stretch',       'Stretch,       84,   84, 614, 447')]
    [TestCase('for Tile',          'Tile,          84,   84, 614, 447')]
    [TestCase('for Zoom',          'Zoom,          84,   84, 614, 447')]
    [TestCase('for TopLeft',       'TopLeft,       84,   84, 614, 447')]
    [TestCase('for TopRight',      'TopRight,    -103,   84, 446, 478')]
    [TestCase('for BottomLeft',    'BottomLeft,    84, -115, 633, 279')]
    [TestCase('for BottomRight',   'BottomRight, -103, -115, 446, 279')]
    procedure PictureRect_should_return_rect(const PictureMode: TFEBackgroundPictureMode;
      const Left, Top, Right, Bottom: Integer);

    [Test]
    [TestCase('for Center',        'Center,      -310, -271,  239,  123')]
    [TestCase('for CenterStretch', 'CenterStretch, 48,  -14, -120, -134')]
    [TestCase('for Stretch',       'Stretch,       48,   10, -120, -158')]
    [TestCase('for Tile',          'Tile,          48,   10, -120, -158')]
    [TestCase('for Zoom',          'Zoom,          48,   10, -120, -158')]
    [TestCase('for TopLeft',       'TopLeft,      -36,  -74,  -36,  -74')]
    [TestCase('for TopRight',      'TopRight,    -837,   10, -288,  404')]
    [TestCase('for BottomLeft',    'BottomLeft,    48, -720,  597, -326')]
    [TestCase('for BottomRight',   'BottomRight, -837, -720, -288, -326')]
    procedure PictureRect_should_return_rect_for_control_with_scrolls(const PictureMode: TFEBackgroundPictureMode;
      const Left, Top, Right, Bottom: Integer);
  end;

implementation

uses
  System.SysUtils, System.Math;

{$IFDEF USE_BILLENIUM_EFFECTS}

{ TFEBackgroundPictureModeHelper }

function TFEBackgroundPictureModeHelper.GetPictureMode: TFCPictureMode;
begin
  case Self of
    TFEBackgroundPictureMode.Center       : Exit(fcpmCenter);
    TFEBackgroundPictureMode.CenterStretch: Exit(fcpmCenterStretch);
    TFEBackgroundPictureMode.Stretch      : Exit(fcpmStretch);
    TFEBackgroundPictureMode.Zoom         : Exit(fcpmZoom);
    TFEBackgroundPictureMode.TopLeft      : Exit(fcpmTopLeft);
    TFEBackgroundPictureMode.TopRight     : Exit(fcpmTopRight);
    TFEBackgroundPictureMode.BottomLeft   : Exit(fcpmBottomLeft);
    TFEBackgroundPictureMode.BottomRight  : Exit(fcpmBottomRight);
    else Result := fcpmTile;
  end;
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TRectUtilsTests }

function TRectUtilsTests.GetZeroRect: TRect;
begin
  Result := TRect.Create(0, 0, 0, 0);
end;

function TRectUtilsTests.PictureRect(const PictureMode: TFEBackgroundPictureMode): TRect;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teBkgrnd.PictureRect(FGraphic, PictureMode.GetPictureMode, 84, FControl, FWinControl, FDrawRect);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Rects.PictureRect(FGraphic, PictureMode, FWinControl, FControl, FDrawRect, 84);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TRectUtilsTests.PictureRect_should_invoke_IsScrollBarVisible_with(const FormStyle: TFormStyle);
begin
  const CustomForm      = TCustomForm.Create(FWinControlHandle, FClientHandle);
  CustomForm.FormStyle := FormStyle;

  FControl    := CustomForm;
  FWinControl := CustomForm;
  FGraphic    := TGraphic.Create;

  PictureRect(TFEBackgroundPictureMode.Center);

  if FormStyle = fsMDIForm then
  begin
    Assert.AreEqual<TControl>(nil, IsScrollBarVisible_Control);
    Assert.AreEqual(FClientHandle, IsScrollBarVisible_Wnd);
  end
  else
  begin
    Assert.AreEqual<TControl>(CustomForm, IsScrollBarVisible_Control);
    Assert.AreEqual(FWinControlHandle, IsScrollBarVisible_Wnd);
  end;
end;

procedure TRectUtilsTests.PictureRect_should_return_rect(const PictureMode: TFEBackgroundPictureMode; const Left, Top,
  Right, Bottom: Integer);
begin
  const CustomForm      = TCustomForm.Create(FWinControlHandle, FClientHandle);
  CustomForm.FormStyle := fsMDIForm;

  FControl    := CustomForm;
  FWinControl := CustomForm;
  FGraphic    := TGraphic.Create;

  SetResult_GetClientRect(TRect.Create(279, 215, 977, 746));

  const Actual = PictureRect(PictureMode);
  Assert.AreEqual<TRect>(TRect.Create(Left, Top, Right, Bottom), Actual);
  Assert.AreEqual<TRect>(TRect.Create(84, 84, 614, 447), FDrawRect);
end;

procedure TRectUtilsTests.PictureRect_should_return_rect_for_control_with_scrolls(
  const PictureMode: TFEBackgroundPictureMode; const Left, Top, Right, Bottom: Integer);

begin
  FControl    := TWinControl.Create(FControlHandle);
  FWinControl := TWinControl.Create(FWinControlHandle);
  FGraphic    := TGraphic.Create;

  SetResult_GetClientRect(GetZeroRect);
  SetResul_IsScrollBarVisible(True);
  SetResult_GetScrollInfo(
    function(const hWnd: HWND; const BarFlag: Integer): TScrollInfo
    begin
      if hWnd = FControlHandle then Result.nPos := IfThen(BarFlag = SB_HORZ, 196, 114)
      else Result.nPos := IfThen(BarFlag = SB_HORZ, 123, 79);
    end
  );

  const Actual = PictureRect(PictureMode);
  Assert.AreEqual<TRect>(TRect.Create(Left, Top, Right, Bottom), Actual);
end;

procedure TRectUtilsTests.PictureRect_should_return_zero_rect_if_graphic(const IsVertical: Boolean);
begin
  FControl    := TWinControl.Create(nil);
  FWinControl := TWinControl.Create(nil);

  FGraphic    := TGraphic.Create;
  if IsVertical then FGraphic.Width := 0 else FGraphic.Height := 0;

  const Actual = PictureRect(TFEBackgroundPictureMode.Center);
  Assert.AreEqual<TRect>(GetZeroRect, Actual);
end;

procedure TRectUtilsTests.PictureRect_should_return_zero_rect_if_graphic_is_null;
begin
  FControl    := TWinControl.Create(nil);
  FWinControl := TWinControl.Create(nil);

  const Actual = PictureRect(TFEBackgroundPictureMode.Center);
  Assert.AreEqual<TRect>(GetZeroRect, Actual);
end;

procedure TRectUtilsTests.SetUp;
begin
end;

procedure TRectUtilsTests.TearDown;
begin
  if Assigned(FWinControl) and (FControl <> FWinControl) then FreeAndNil(FWinControl);
  if Assigned(FControl) then FreeAndNil(FControl);
  if Assigned(FGraphic) then FreeAndNil(FGraphic);

  GetScrollInfo_Wnd     := HWND(-1);
  GetScrollInfo_BarFlag := -1;

  GetClientRect_Wnd  := HWND(-1);
  GetClientRect_Rect := TRect.Create(-1, -1, -1, -1);

  IsScrollBarVisible_Control := nil;
  IsScrollBarVisible_Wnd     := HWND(-1);

  SetResul_IsScrollBarVisible(False);
  SetResult_GetScrollInfo(nil);
end;

initialization
  TDUnitX.RegisterTestFixture(TRectUtilsTests);

end.
