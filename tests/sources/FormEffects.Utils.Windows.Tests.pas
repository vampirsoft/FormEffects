/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Windows.Tests.pas                        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Windows.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  FormEffects.Winapi.Windows.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TUtilsWindowsTests }

  [TestFixture]
  TUtilsWindowsTests = class
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;

  strict private
    function GetWindowOffset: TPoint;
    function HasWindowRegion: Boolean;
    function GetWindowSize: TSize;

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

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
  System.Types,
  System.SysUtils,
  System.Math,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Windows,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers;

{$IFDEF FORM_EFFECTS_TESTS}

{ TUtilsWindowsTests }

function TUtilsWindowsTests.GetWindowOffset: TPoint;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := teRender.WindowClientOffset(0);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Windows.GetWindowOffset(0);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TUtilsWindowsTests.GetWindowOffset_should_return_client_window_offset;
begin
  FWinapiWindowsMocks.GetWindowRect_Result(TRect.InlineCreate(187, 137, 257, 207));

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
  FWinapiWindowsMocks.GetWindowRect_Result(TRect.InlineCreate(37, 17, 147, 123));

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
  var DeleteObjectInvoked := False;

  const TestRgn: HRGN = 199;

  FWinapiWindowsMocks.CreateRectRgn_Result(
    function(const Left, Top, Right, Bottom: Integer): HRGN
    begin
      Assert.AreEqual<TRect>(TRect.Zero, TRect.InlineCreate(Left, Top, Right, Bottom));

      Result := TestRgn;
    end
  );

  FWinapiWindowsMocks.GetWindowRgn_Result(
    function(const Wnd: HWND; const Rgn: HRGN): Integer
    begin
      Assert.AreEqual<HWND>(0, Wnd);
      Assert.AreEqual<HRGN>(TestRgn, Rgn);

      Result := IfThen(Expected, SIMPLEREGION, ERROR);
    end
  );

  FWinapiWindowsMocks.DeleteObject_Result(
    procedure(const Obj: HGDIOBJ)
    begin
      DeleteObjectInvoked := True;

      Assert.AreEqual<HRGN>(TestRgn, Obj);
    end
  );

  const Actual = HasWindowRegion;
  Assert.AreEqual(Expected, Actual);
  Assert.IsTrue(DeleteObjectInvoked);
end;

procedure TUtilsWindowsTests.Setup;
begin
  FWinapiWindowsMocks := TWinapiWindowsMocks.Create;
end;

procedure TUtilsWindowsTests.TearDown;
begin
  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilsWindowsTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
