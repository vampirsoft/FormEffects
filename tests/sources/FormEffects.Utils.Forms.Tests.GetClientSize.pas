/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Forms.Tests.GetClientSize.pas            *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms.Tests.GetClientSize;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  System.Types,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Utils.Forms.Tests;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TGetClientSizeTests }

  [TestFixture]
  TGetClientSizeTests = class(TUtilsFormsTests)
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;

  strict private
    procedure GetClientSize(const IsMaximizedMDIChild: Boolean; out ClientSize: TSize; out ClientOrgPoint: TPoint);

  public
    [Setup]
    procedure Setup; override;
    [TearDown]
    procedure TearDown; override;

  public
    [Test]
    [TestCase('GetClientSize должен вурнуть размер и точку для WinControl с Maximized стилем',    'True,   3,    3, 156, 64')]
    [TestCase('GetClientSize должен вурнуть размер и точку для WinControl без Maximized стилем', 'False, -79, -157, 156, 64')]
    public procedure should_return_size_and_point(const IsMaximized: Boolean; const X, Y, Width, Height: Integer);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.SysUtils,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

{$IFDEF FORM_EFFECTS_TESTS}

{ TGetClientSizeTests }

procedure TGetClientSizeTests.GetClientSize(
  const IsMaximizedMDIChild: Boolean;
  out ClientSize: TSize;
  out ClientOrgPoint: TPoint
);
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.GetClientSize(FWinControl, 0, True, IsMaximizedMDIChild, ClientSize.cx, ClientSize.cy, ClientOrgPoint);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms.GetClientSize(0, FWinControl, IsMaximizedMDIChild, ClientSize, ClientOrgPoint);
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
  const WinControlMock = TMock<TWinControl>.Create;

  WinControlMock
    .Setup
    .WillReturn(3)
    .When
    .GetBorderWidth;
  FWinapiWindowsMocks.GetClientRect_Result(TRect.InlineCreate(199,  59, 355, 123));
  FWinapiWindowsMocks.GetWindowRect_Result(TRect.InlineCreate(79,  157, 250, 198));

  FWinControl := WinControlMock.Instance;

  GetClientSize(IsMaximized, ActualClientSize, ActualClientOrgPoint);

  Assert.AreEqual<TSize>(TSize.InlineCreate(Width, Height), ActualClientSize);
  Assert.AreEqual<TPoint>(TPoint.InlineCreate(X, Y), ActualClientOrgPoint);
end;

procedure TGetClientSizeTests.Setup;
begin
  inherited Setup;

  FWinapiWindowsMocks := TWinapiWindowsMocks.Create;
end;

procedure TGetClientSizeTests.TearDown;
begin
  FreeAndNil(FWinapiWindowsMocks);

  inherited TearDown;
end;

initialization
  TDUnitX.RegisterTestFixture(TGetClientSizeTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
