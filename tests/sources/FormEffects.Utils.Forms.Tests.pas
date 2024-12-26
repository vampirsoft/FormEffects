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
  System.Generics.Collections,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TUtilsFormsTests }

  TUtilsFormsTests = class
  protected
    FApplicationMock: TMock<TApplication>;
    FMainFormMock: TMock<TForm>;
    FChildFormMock: TMock<TForm>;

    FWinControl: TWinControl;
    FMDIChildren: TList<TForm>;

  protected
    procedure ApplicationInit;

  public
    [Setup]
    procedure Setup; virtual;
    [TearDown]
    procedure TearDown; virtual;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.SysUtils;

{$IFDEF FORM_EFFECTS_TESTS}

{ TUtilsFormsTests }

procedure TUtilsFormsTests.ApplicationInit;
begin
  FApplicationMock := TMock<TApplication>.Create;
  FMainFormMock    := TMock<TForm>.Create;
  FChildFormMock   := TMock<TForm>.Create;

  FMDIChildren.Add(FChildFormMock.Instance);
  FMainFormMock
    .Setup
    .WillReturn(FMDIChildren)
    .When
    .GetMDIChildren;
  FApplicationMock
    .Setup
    .WillReturn(FMainFormMock.Instance)
    .When
    .GetMainForm;

  Application := FApplicationMock.Instance;
end;

procedure TUtilsFormsTests.Setup;
begin
  FMDIChildren := TList<TForm>.Create;
end;

procedure TUtilsFormsTests.TearDown;
begin
  FWinControl := nil;
  Application := nil;

  FreeAndNil(FMDIChildren);
end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
