/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.pas                            *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Vcl.Controls.Mocks;

type

{ TRenderingTests }

  [TestFixture]
  TRenderingTests = class
  strict private
    FWinControl: TWinControl;

  public
    [TearDown]
    procedure TearDown;

  public
    [Test]
    procedure CompleteFlags_Test();
  end;

implementation

uses
  System.SysUtils;

{ TRenderingTests }

procedure TRenderingTests.CompleteFlags_Test;
begin
  FWinControl := TWinControl.Create(nil);

//  const kva = CompleteFlags(FWinControl, $00100000);
  const kva = CompleteFlags(FWinControl, $00000001);
end;

procedure TRenderingTests.TearDown;
begin
  if Assigned(FWinControl) then FreeAndNil(FWinControl);
end;

initialization
  TDUnitX.RegisterTestFixture(TRenderingTests);

end.
