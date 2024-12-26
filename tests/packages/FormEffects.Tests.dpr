/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Tests.dpr                                      *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
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
  FormEffects.Utils in '..\..\sources\FormEffects.Utils.pas',
  FormEffects.Utils.OS in '..\..\sources\FormEffects.Utils.OS.pas',
  FormEffects.Utils.Windows in '..\..\sources\FormEffects.Utils.Windows.pas',
  FormEffects.Utils.ScrollBars in '..\..\sources\FormEffects.Utils.ScrollBars.pas',
  FormEffects.Utils.Rects in '..\..\sources\FormEffects.Utils.Rects.pas',
  FormEffects.Utils.Pictures in '..\..\sources\FormEffects.Utils.Pictures.pas',
  FormEffects.Utils.Forms in '..\..\sources\FormEffects.Utils.Forms.pas',
  FormEffects.Rendering in '..\..\sources\FormEffects.Rendering.pas',
  FormEffects.Rendering.Ext in '..\..\sources\FormEffects.Rendering.Ext.pas',
  FormEffects.Rendering.Pictures in '..\..\sources\FormEffects.Rendering.Pictures.pas',
  FormEffects.FormContainer in '..\..\sources\FormEffects.FormContainer.pas',
  FormEffects.TypeHelpers.Mocks in '..\sources\FormEffects.TypeHelpers.Mocks.pas',
  FormEffects.Utils.Mocks in '..\sources\FormEffects.Utils.Mocks.pas',
  FormEffects.Winapi.Windows.Mocks in '..\sources\FormEffects.Winapi.Windows.Mocks.pas',
  FormEffects.System.Classes.Mocks in '..\sources\FormEffects.System.Classes.Mocks.pas',
  FormEffects.Vcl.Graphics.Mocks in '..\sources\FormEffects.Vcl.Graphics.Mocks.pas',
  FormEffects.Vcl.Controls.Mocks in '..\sources\FormEffects.Vcl.Controls.Mocks.pas',
  FormEffects.Vcl.ToolWin.Mocks in '..\sources\FormEffects.Vcl.ToolWin.Mocks.pas',
  FormEffects.Vcl.ComCtrls.Mocks in '..\sources\FormEffects.Vcl.ComCtrls.Mocks.pas',
  FormEffects.Vcl.StdCtrls.Mocks in '..\sources\FormEffects.Vcl.StdCtrls.Mocks.pas',
  FormEffects.Vcl.ExtCtrls.Mocks in '..\sources\FormEffects.Vcl.ExtCtrls.Mocks.pas',
  FormEffects.Vcl.DockTabSet.Mocks in '..\sources\FormEffects.Vcl.DockTabSet.Mocks.pas',
  FormEffects.Vcl.OleCtrls.Mocks in '..\sources\FormEffects.Vcl.OleCtrls.Mocks.pas',
  FormEffects.Vcl.Forms.Mocks in '..\sources\FormEffects.Vcl.Forms.Mocks.pas',
  FormEffects.Utils.Windows.Mocks in '..\sources\FormEffects.Utils.Windows.Mocks.pas',
  FormEffects.Utils.ScrollBars.Mocks in '..\sources\FormEffects.Utils.ScrollBars.Mocks.pas',
  FormEffects.Utils.Rects.Mocks in '..\sources\FormEffects.Utils.Rects.Mocks.pas',
  FormEffects.Utils.Pictures.Mocks in '..\sources\FormEffects.Utils.Pictures.Mocks.pas',
  FormEffects.Vcl.Themes.Mocks in '..\sources\FormEffects.Vcl.Themes.Mocks.pas',
  FormEffects.Utils.Forms.Mocks in '..\sources\FormEffects.Utils.Forms.Mocks.pas',
  FormEffects.Rendering.Ext.Mocks in '..\sources\FormEffects.Rendering.Ext.Mocks.pas',
  FormEffects.Rendering.Pictures.Mocks in '..\sources\FormEffects.Rendering.Pictures.Mocks.pas',
  FormEffects.Utils.Tests in '..\sources\FormEffects.Utils.Tests.pas',
  FormEffects.Utils.Windows.Tests in '..\sources\FormEffects.Utils.Windows.Tests.pas',
  FormEffects.Utils.ScrollBars.Tests in '..\sources\FormEffects.Utils.ScrollBars.Tests.pas',
  FormEffects.Utils.Rects.Tests in '..\sources\FormEffects.Utils.Rects.Tests.pas',
  FormEffects.Utils.Pictures.Tests in '..\sources\FormEffects.Utils.Pictures.Tests.pas',
  FormEffects.Utils.Forms.Tests in '..\sources\FormEffects.Utils.Forms.Tests.pas',
  FormEffects.Utils.Forms.Tests.IsMaximizedMDIClient in '..\sources\FormEffects.Utils.Forms.Tests.IsMaximizedMDIClient.pas',
  FormEffects.Utils.Forms.Tests.IsMaximizedMDIChild in '..\sources\FormEffects.Utils.Forms.Tests.IsMaximizedMDIChild.pas',
  FormEffects.Utils.Forms.Tests.GetClientSize in '..\sources\FormEffects.Utils.Forms.Tests.GetClientSize.pas',
  FormEffects.Utils.Forms.Tests.IsMDIFormWithMaximizedMDIChild in '..\sources\FormEffects.Utils.Forms.Tests.IsMDIFormWithMaximizedMDIChild.pas',
  FormEffects.Utils.Forms.Tests.HasMainFormMaximizedMDIChild in '..\sources\FormEffects.Utils.Forms.Tests.HasMainFormMaximizedMDIChild.pas',
  FormEffects.Rendering.Tests in '..\sources\FormEffects.Rendering.Tests.pas',
  FormEffects.Rendering.Ext.Tests in '..\sources\FormEffects.Rendering.Ext.Tests.pas',
  FormEffects.Rendering.Tests.RenderWindowToDC in '..\sources\FormEffects.Rendering.Tests.RenderWindowToDC.pas',
  FormEffects.Rendering.Tests.RenderWindowToDCAux in '..\sources\FormEffects.Rendering.Tests.RenderWindowToDCAux.pas',
  FormEffects.Rendering.Tests.ToolWindowNCPaint in '..\sources\FormEffects.Rendering.Tests.ToolWindowNCPaint.pas',
  FormEffects.Rendering.Tests.WinControlNCPaint in '..\sources\FormEffects.Rendering.Tests.WinControlNCPaint.pas',
  FormEffects.Rendering.Tests.EmulateNCPaint in '..\sources\FormEffects.Rendering.Tests.EmulateNCPaint.pas',
  FormEffects.Rendering.Pictures.Tests in '..\sources\FormEffects.Rendering.Pictures.Tests.pas';

{$R *.RES}

begin
  RunRegisteredTests;
end.
