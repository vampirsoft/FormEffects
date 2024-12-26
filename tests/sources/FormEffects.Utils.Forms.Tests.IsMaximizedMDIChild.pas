/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Forms.Tests.IsMaximizedMDIChild.pas      *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms.Tests.IsMaximizedMDIChild;

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

{ TIsMaximizedMDIChildTests }

  [TestFixture]
  TIsMaximizedMDIChildTests = class(TUtilsFormsTests)
  strict private
    function IsMaximizedMDIChild: Boolean;

  public
    [Test]
    [TestCase('IsMaximizedMDIChild должен вернуть False для не TCustomForm', '')]
    procedure should_return_false_for_no_CustomForm;

    [Test]
    [TestCase('IsMaximizedMDIChild должен вернуть False для TCustomForm с FormStyle <> fsMDIChild', '')]
    procedure should_return_false_for_no_MDIChild_CustomForm;

    [Test]
    [TestCase('IsMaximizedMDIChild должен вернуть False, если MainForm не установлена', '')]
    procedure should_return_false_if_MainForIsNull;

    [Test]
    [TestCase('IsMaximizedMDIChild должен вернуть False для для TCustomForm с FormStyle = fsMDIChild и MainForm с FormStyle <> fsMDIForm', '')]
    procedure should_return_false_for_MDIChild_CustomForm_no_MDIForm_MainForm;

    [Test]
    [TestCase('IsMaximizedMDIChild должен вернуть True для TCustomForm с WindowState = wsMaximized', '')]
    procedure should_return_true_for_CustomForm_with_WindowState_as_Maximized;

    [Test]
    [TestCase('IsMaximizedMDIChild должен вернуть True, если MainForm имеет MDIChild с WindowState = wsMaximized', '')]
    procedure should_return_true_if_MainForm_has_Maximized_MDIChild;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.UITypes,
  System.Rtti,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TIsMaximizedMDIChildTests }

function TIsMaximizedMDIChildTests.IsMaximizedMDIChild: Boolean;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := GetMaximizedMDIChild(FWinControl);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Forms.IsMaximizedMDIChild(FWinControl);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TIsMaximizedMDIChildTests.should_return_false_for_no_MDIChild_CustomForm;
begin
  ApplicationInit;

  const WinControlMock = TMock<TCustomForm>.Create;

  WinControlMock
    .Setup
    .WillReturn(fsStayOnTop.AsValue)
    .When
    .GetFormStyle;

  FWinControl := WinControlMock.Instance;

  Assert.IsFalse(IsMaximizedMDIChild);
end;

procedure TIsMaximizedMDIChildTests.should_return_false_for_MDIChild_CustomForm_no_MDIForm_MainForm;
begin
  ApplicationInit;

  FMainFormMock
    .Setup
    .WillReturn(fsStayOnTop.AsValue)
    .When
    .GetFormStyle;

  const WinControlMock = TMock<TCustomForm>.Create;

  WinControlMock
    .Setup
    .WillReturn(fsMDIChild.AsValue)
    .When
    .GetFormStyle;

  FWinControl := WinControlMock.Instance;

  Assert.IsFalse(IsMaximizedMDIChild);
end;

procedure TIsMaximizedMDIChildTests.should_return_false_for_no_CustomForm;
begin
  Assert.IsFalse(IsMaximizedMDIChild);
end;

procedure TIsMaximizedMDIChildTests.should_return_false_if_MainForIsNull;
begin
  FApplicationMock     := TMock<TApplication>.Create;
  const WinControlMock  = TMock<TCustomForm>.Create;

  Application := FApplicationMock.Instance;
  FWinControl := WinControlMock.Instance;

  Assert.IsFalse(IsMaximizedMDIChild);
end;

procedure TIsMaximizedMDIChildTests.should_return_true_for_CustomForm_with_WindowState_as_Maximized;
begin
  ApplicationInit;

  FMDIChildren.Clear;

  const WinControlMock  = TMock<TCustomForm>.Create;

  WinControlMock
    .Setup
    .WillReturn(fsMDIChild.AsValue)
    .When
    .GetFormStyle;

  FWinControl := WinControlMock.Instance;

  Assert.IsTrue(IsMaximizedMDIChild);
end;

procedure TIsMaximizedMDIChildTests.should_return_true_if_MainForm_has_Maximized_MDIChild;
begin
  ApplicationInit;

  const WinControlMock  = TMock<TCustomForm>.Create;

  WinControlMock
    .Setup
    .WillReturn(fsMDIChild.AsValue)
    .When
    .GetFormStyle;

  WinControlMock
    .Setup
    .WillReturn(wsMinimized.AsValue)
    .When
    .GetWindowState;

  FWinControl := WinControlMock.Instance;

  Assert.IsTrue(IsMaximizedMDIChild);
end;

initialization
  TDUnitX.RegisterTestFixture(TIsMaximizedMDIChildTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
