/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Forms.Tests.IsMaximizedMDIClient.pas     *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms.Tests.IsMaximizedMDIClient;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  FormEffects.Utils.Forms.Tests;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TIsMaximizedMDIClientTests }

  [TestFixture]
  TIsMaximizedMDIClientTests = class(TUtilsFormsTests)
  strict private
    function IsMaximizedMDIClient(const ClassName: string): Boolean;

  public
    [Test]
    [TestCase('IsMaximizedMDIClient должен вернуть False, если ClassName <> MDICLIENT',                                                      'Test_MDICLIENT,False')]
    [TestCase('IsMaximizedMDIClient должен вернуть True для ClassName = MDICLIENT, если MainForm имеет MDIChild с WindowState = wsMaximized',     'MDICLIENT,True')]
    procedure should_return(const ClassName: string; const Expected: Boolean);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.SysUtils,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

{$IFDEF FORM_EFFECTS_TESTS}

{ TIsMaximizedMDIClientTests }

function TIsMaximizedMDIClientTests.IsMaximizedMDIClient(const ClassName: string): Boolean;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := GetMaximizedMDIClient(PWideChar(ClassName));
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := FormEffects.Utils.Forms.IsMaximizedMDIClient(ClassName);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TIsMaximizedMDIClientTests.should_return(const ClassName: string; const Expected: Boolean);
begin
  ApplicationInit;

  Assert.AreEqual(Expected, IsMaximizedMDIClient(ClassName));
end;

initialization
  TDUnitX.RegisterTestFixture(TIsMaximizedMDIClientTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
