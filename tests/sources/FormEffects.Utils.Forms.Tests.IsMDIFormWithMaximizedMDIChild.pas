///////////////////////////////////////////////////////////////////////////////////////
//***********************************************************************************//
//* Project      : FormEffects                                                      *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                        *//
//* Unit Name    : FormEffects.Utils.Forms.Tests.IsMDIFormWithMaximizedMDIChild.pas *//
//* Author       : Сергей (LordVampir) Дворников                                    *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                       *//
//* Licensed under MIT                                                              *//
//***********************************************************************************//
///////////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms.Tests.IsMDIFormWithMaximizedMDIChild;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Vcl.Forms,
  FormEffects.Utils.Forms.Tests,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TIsMDIFormWithMaximizedMDIChildTests }

  [TestFixture]
  TIsMDIFormWithMaximizedMDIChildTests = class(TUtilsFormsTests)
  strict private
    function IsMDIFormWithMaximizedMDIChild: Boolean;

  public
    [Test]
    [TestCase('IsMDIFormWithMaximizedMDIChild должен вернуть False для не TCustomForm', '')]
    procedure should_return_false_for_no_CustomForm;

    [Test]
    [TestCase('IsMDIFormWithMaximizedMDIChild должен вернуть False для TCustomForm с FormStyle <> fsMDIForm', '')]
    procedure should_return_false_for_no_MDIForm_CustomForm;

    [Test]
    [TestCase('IsMDIFormWithMaximizedMDIChild должен вернуть True, если MainForm имеет MDIChild с WindowState = wsMaximized', '')]
    procedure should_return_true_if_MainForm_has_Maximized_MDIChild;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.Rtti,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TIsMDIFormWithMaximizedMDIChildTests }

function TIsMDIFormWithMaximizedMDIChildTests.IsMDIFormWithMaximizedMDIChild: Boolean;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := GetMDIFormWithMaximizedMDIChild(FWinControl);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Forms.IsMDIFormWithMaximizedMDIChild(FWinControl);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TIsMDIFormWithMaximizedMDIChildTests.should_return_false_for_no_CustomForm;
begin
  Assert.IsFalse(IsMDIFormWithMaximizedMDIChild);
end;

procedure TIsMDIFormWithMaximizedMDIChildTests.should_return_false_for_no_MDIForm_CustomForm;
begin
  const WinControlMock = TMock<TCustomForm>.Create;

  WinControlMock
    .Setup
    .WillReturn(fsStayOnTop.AsValue)
    .When
    .GetFormStyle;

  FWinControl := WinControlMock.Instance;

  Assert.IsFalse(IsMDIFormWithMaximizedMDIChild);
end;

procedure TIsMDIFormWithMaximizedMDIChildTests.should_return_true_if_MainForm_has_Maximized_MDIChild;
begin
  ApplicationInit;

  const WinControlMock = TMock<TCustomForm>.Create;

  FWinControl := WinControlMock.Instance;

  Assert.IsTrue(IsMDIFormWithMaximizedMDIChild);
end;

initialization
  TDUnitX.RegisterTestFixture(TIsMDIFormWithMaximizedMDIChildTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
