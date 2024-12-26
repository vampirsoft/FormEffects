/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.pas                            *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Vcl.Controls,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TUtilsFormsTests }

  [TestFixture]
  TUtilsFormsTests = class
  public
    [Test]
    procedure Test;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

{$IFDEF FORM_EFFECTS_TESTS}

{ TUtilsFormsTests }

procedure TUtilsFormsTests.Test;
begin
//  const WinControlMock = TMock<TWinControl>.Create;
//
////  const Kva = WinControlMock.Setup.Expect;
////  WinControlMock.Setup.Expect.Once.When.CanEnumerateControl(It0.IsEqualTo<TControl>(nil));
//  WinControlMock.Setup.Expect.Exactly(1).When.CanEnumerateControl(It0.IsNotNil<TControl>);
//  WinControlMock.Setup.Expect.Exactly(1).When.CanEnumerateControl(It0.IsAny<TControl>);
//
////  WinControlMock.Instance.ScaleForPPI(100);
//  WinControlMock.Instance.CanEnumerateControl(nil);
//  WinControlMock.Instance.CanEnumerateControl(TMock<TControl>.Create.Instance);
//  WinControlMock.Instance.CanEnumerateControl(nil);
//
//  WinControlMock.Verify('Kva');
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilsFormsTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
