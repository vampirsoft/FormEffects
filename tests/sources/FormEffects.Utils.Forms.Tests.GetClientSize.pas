/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Forms.Tests.GetClientSize.pas            *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms.Tests.GetClientSize;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  System.Types,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Utils.Forms.Tests;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TGetClientSizeTests }

  [TestFixture]
  TGetClientSizeTests = class(TUtilsFormsTests)
  strict private
    procedure GetClientSize(const IsMaximizedMDIChild: Boolean; out ClientSize: TSize; out ClientOrgPoint: TPoint);

  public
    [Test]
    [TestCase('GetClientSize должен вурнуть размер и точку для WinControl с Maximized стилем',    'True,   3,    3, 156, 64')]
    [TestCase('GetClientSize должен вурнуть размер и точку для WinControl без Maximized стилем', 'False, -79, -157, 156, 64')]
    public procedure should_return_size_and_point(const IsMaximized: Boolean; const X, Y, Width, Height: Integer);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  System.SysUtils,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Winapi.Windows.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TGetClientSizeTests }

procedure TGetClientSizeTests.GetClientSize(
  const IsMaximizedMDIChild: Boolean;
  out ClientSize: TSize;
  out ClientOrgPoint: TPoint
);
begin
  const Wnd: HWND = 0;
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.GetClientSize(FWinControl, Wnd, True, IsMaximizedMDIChild, ClientSize.cx, ClientSize.cy, ClientOrgPoint);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms.GetClientSize(Wnd, FWinControl, IsMaximizedMDIChild, ClientSize, ClientOrgPoint);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TGetClientSizeTests.should_return_size_and_point(
  const IsMaximized: Boolean;
  const X, Y, Width, Height: Integer
);
var
  ActualClientSize: TSize;
  ActualClientOrgPoint: TPoint;

begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const WinControlMock = TMock<TWinControl>.Create;

  WinControlMock
    .Setup
    .WillReturn(3)
    .When
    .GetBorderWidth;

  WinapiWindowsMock
    .Setup
    .WillReturn(TRect.InlineCreate(199,  59, 355, 123))
    .When
    .GetClientRect(It0.IsEqualTo<HWND>(0));

  WinapiWindowsMock
    .Setup
    .WillReturn(TRect.InlineCreate(79,  157, 250, 198))
    .When
    .GetWindowRect(It0.IsAny<HWND>);

  FWinControl := WinControlMock.Instance;

  GetClientSize(IsMaximized, ActualClientSize, ActualClientOrgPoint);

  Assert.AreEqual<TSize>(TSize.InlineCreate(Width, Height), ActualClientSize);
  Assert.AreEqual<TPoint>(TPoint.InlineCreate(X, Y), ActualClientOrgPoint);
end;

initialization
  TDUnitX.RegisterTestFixture(TGetClientSizeTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
