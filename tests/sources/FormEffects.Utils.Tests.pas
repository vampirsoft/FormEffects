/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Tests.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TUtilsTests }

  [TestFixture]
  TUtilsTests = class
  public
    [Test]
    [TestCase('False for TPanel',          'False,TPanel')]          // TPanel inherits from TWinControl
    [TestCase('False for TGraphicControl', 'False,TGraphicControl')] // TGraphicControl inherits from TControl but no TWinControl
    [TestCase('True  for TControl',         'True,TControl')]        // TWinControl inherits from TControl
    [TestCase('True  for tcontrol',         'True,tcontrol')]        // TWinControl inherits from TControl (lower case)
  {$IFDEF USE_BILLENIUM_EFFECTS}
    [TestCase('False  for TObject',        'False,TObject')]         // Incorrect result - TWinControl inherits from TObject
  {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
    [TestCase('True  for TObject',          'True,TObject')]         // TWinControl inherits from TObject
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
    procedure IsInheritsClass_for_WinControl_class_should_return(const Expected: Boolean; const ClassName: string);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.SysUtils;

{$IFDEF FORM_EFFECTS_TESTS}

{ TUtilsTests }

procedure TUtilsTests.IsInheritsClass_for_WinControl_class_should_return(const Expected: Boolean;
  const ClassName: string);
begin
  const Actual = IsInheritsClass(TWinControl, ClassName);

  Assert.AreEqual(Expected, Actual);
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilsTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
