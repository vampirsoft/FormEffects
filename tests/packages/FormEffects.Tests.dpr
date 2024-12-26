/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Tests.dpr                                      *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

program FormEffects.Tests;

{$INCLUDE FormEffects.Tests.inc}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitXTestRunner,
  FormEffects.TypeHelpers in '..\..\sources\FormEffects.TypeHelpers.pas',
  FormEffects.Utils.ScrollBars in '..\..\sources\FormEffects.Utils.ScrollBars.pas',
  FormEffects.Utils.Rects in '..\..\sources\FormEffects.Utils.Rects.pas',
  FormEffects.Rendering in '..\..\sources\FormEffects.Rendering.pas',
  FormEffects.Rendering.Pictures in '..\..\sources\FormEffects.Rendering.Pictures.pas',
  FormEffects.FormContainer in '..\..\sources\FormEffects.FormContainer.pas',
  FormEffects.Winapi.Windows.Mocks in '..\sources\FormEffects.Winapi.Windows.Mocks.pas',
  FormEffects.System.Classes.Mocks in '..\sources\FormEffects.System.Classes.Mocks.pas',
  FormEffects.Vcl.Graphics.Mocks in '..\sources\FormEffects.Vcl.Graphics.Mocks.pas',
  FormEffects.Vcl.Controls.Mocks in '..\sources\FormEffects.Vcl.Controls.Mocks.pas',
  FormEffects.Vcl.OleCtrls.Mocks in '..\sources\FormEffects.Vcl.OleCtrls.Mocks.pas',
  FormEffects.Vcl.Forms.Mocks in '..\sources\FormEffects.Vcl.Forms.Mocks.pas',
  FormEffects.Utils.ScrollBars.Mocks in '..\sources\FormEffects.Utils.ScrollBars.Mocks.pas',
  FormEffects.Utils.Rects.Mocks in '..\sources\FormEffects.Utils.Rects.Mocks.pas',
  FormEffects.Rendering.Pictures.Mocks in '..\sources\FormEffects.Rendering.Pictures.Mocks.pas',
  FormEffects.Utils.ScrollBars.Tests in '..\sources\FormEffects.Utils.ScrollBars.Tests.pas',
  FormEffects.Utils.Rects.Tests in '..\sources\FormEffects.Utils.Rects.Tests.pas',
  FormEffects.Rendering.Tests in '..\sources\FormEffects.Rendering.Tests.pas',
  FormEffects.Rendering.Pictures.Tests in '..\sources\FormEffects.Rendering.Pictures.Tests.pas';

{$R *.RES}

begin
  RunRegisteredTests;
end.
