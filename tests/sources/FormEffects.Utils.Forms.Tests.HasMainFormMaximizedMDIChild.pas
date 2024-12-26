///////////////////////////////////////////////////////////////////////////////////////
//***********************************************************************************//
//* Project      : FormEffects                                                      *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                        *//
//* Unit Name    : FormEffects.Utils.Forms.Tests.HasMainFormMaximizedMDIChild.pas   *//
//* Author       : Сергей (LordVampir) Дворников                                    *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                       *//
//* Licensed under MIT                                                              *//
//***********************************************************************************//
///////////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms.Tests.HasMainFormMaximizedMDIChild;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Vcl.Forms,
  FormEffects.Utils.Forms.Tests,
  FormEffects.Vcl.Forms.Mocks;

{$IF DEFINED(FORM_EFFECTS_TESTS) AND NOT DEFINED(USE_BILLENIUM_EFFECTS)}

type

{ THasMainFormMaximizedMDIChildTests }

  [TestFixture]
  THasMainFormMaximizedMDIChildTests = class(TUtilsFormsTests)
  public
    [Test]
    [TestCase('HasMainFormMaximizedMDIChild должен вернуть False, если MainForm не имеет MDIChild с WindowState = wsMaximized', '')]
    procedure should_return_false_if_MainForm_has_not_Maximized_MDIChild;

    [Test]
    [TestCase('HasMainFormMaximizedMDIChild должен вернуть True, если MainForm имеет MDIChild с WindowState = wsMaximized', '')]
    procedure should_return_true_if_MainForm_has_Maximized_MDIChild;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS and not USE_BILLENIUM_EFFECTS}

implementation

uses
  System.UITypes,
  System.Rtti,
{$IFNDEF USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks;

{$IF DEFINED(FORM_EFFECTS_TESTS) AND NOT DEFINED(USE_BILLENIUM_EFFECTS)}

{ THasMainFormMaximizedMDIChildTests }

procedure THasMainFormMaximizedMDIChildTests.should_return_false_if_MainForm_has_not_Maximized_MDIChild;
begin
  ApplicationInit;

  FChildFormMock
    .Setup
    .WillReturn(wsMinimized.AsValue)
    .When
    .GetWindowState;

  Assert.IsFalse(HasMainFormMaximizedMDIChild);
end;

procedure THasMainFormMaximizedMDIChildTests.should_return_true_if_MainForm_has_Maximized_MDIChild;
begin
  ApplicationInit;

  Assert.IsTrue(HasMainFormMaximizedMDIChild);
end;

initialization
  TDUnitX.RegisterTestFixture(THasMainFormMaximizedMDIChildTests);

{$ENDIF ~ FORM_EFFECTS_TESTS and not USE_BILLENIUM_EFFECTS}

end.
