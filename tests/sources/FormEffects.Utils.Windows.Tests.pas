/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Windows.Tests.pas                        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Windows.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  System.Types;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TUtilsWindowsTests }

  [TestFixture]
  TUtilsWindowsTests = class
  strict private
    function GetWindowOffset: TPoint;
    function HasWindowRegion: Boolean;
    function GetWindowSize: TSize;

  public
    [Test]
    [TestCase('GetWindowOffset должен вернуть смещение клиентского окна относительно экрана', '')]
    procedure GetWindowOffset_should_return_client_window_offset;

    [Test]
    [TestCase('HasWindowRegion должен вернуть True',  'True')]
    [TestCase('HasWindowRegion должен вернуть False', 'False')]
    procedure HasWindowRegion_should_return(const Expected: Boolean);

    [Test]
    [TestCase('GetWindowSize должен вернуть TSize для TRect окна',  'True')]
    procedure GetWindowSize_should_return_size_of_window_rect;
  end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  Winapi.Windows,
  System.SysUtils,
  System.Math,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Windows,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers,
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Winapi.Windows.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TUtilsWindowsTests }

function TUtilsWindowsTests.GetWindowOffset: TPoint;
begin
  const Wnd: HWND = 0;
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teRender.WindowClientOffset(Wnd);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Windows.GetWindowOffset(Wnd);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TUtilsWindowsTests.GetWindowOffset_should_return_client_window_offset;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .WillReturn(TRect.InlineCreate(187, 137, 257, 207))
    .When
    .GetWindowRect(It0.IsAny<HWND>);

  const Actual = GetWindowOffset;
  Assert.AreEqual<TPoint>(TPoint.InlineCreate(-187, -137), Actual);
end;

function TUtilsWindowsTests.GetWindowSize: TSize;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  var Width : Integer;
  var Height: Integer;
  teRender.GetSize(0, False, Width, Height);
  Result := TSize.InlineCreate(Width, Height);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Windows.GetWindowSize(0, False);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TUtilsWindowsTests.GetWindowSize_should_return_size_of_window_rect;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .WillReturn(TRect.InlineCreate(37, 17, 147, 123))
    .When
    .GetWindowRect(It0.IsAny<HWND>);

  const Actual = GetWindowSize;
  Assert.AreEqual<TSize>(TSize.InlineCreate(110, 106), Actual);
end;

function TUtilsWindowsTests.HasWindowRegion: Boolean;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teRender.WindowHasRegion(0);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Windows.HasWindowRegion(0);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TUtilsWindowsTests.HasWindowRegion_should_return(const Expected: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const TestRgn: HRGN = 199;

  WinapiWindowsMock
    .Setup
    .WillReturn(TestRgn)
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(0),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(0)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetWindowRgn(It0.IsEqualTo<HWND>(0), It1.IsEqualTo<HRGN>(TestRgn));
  WinapiWindowsMock
    .Setup
    .WillReturn(IfThen(Expected, SIMPLEREGION, ERROR))
    .When
    .GetWindowRgn(It0.IsEqualTo<HWND>(0), It1.IsEqualTo<HRGN>(TestRgn));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteObject(It0.IsEqualTo<HGDIOBJ>(TestRgn));

  const Actual = HasWindowRegion;

  Assert.AreEqual(Expected, Actual);
  WinapiWindowsMock.Verify;
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilsWindowsTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
