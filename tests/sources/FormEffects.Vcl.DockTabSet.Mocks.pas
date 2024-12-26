/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.DockTabSet.Mocks.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.DockTabSet.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Messages,
  FormEffects.Vcl.ExtCtrls.Mocks;

type

{ TTabDockPanel }

  TTabDockPanel = class(TPanel)
  protected
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMPrint(var Message: TWMPrint); message WM_PRINT;
  end;

implementation

{ TTabDockPanel }

procedure TTabDockPanel.WMNCPaint(var Message: TWMNCPaint);
begin
end;

procedure TTabDockPanel.WMPrint(var Message: TWMPrint);
begin
end;

end.
