/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.ComCtrls.Mocks.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.ComCtrls.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.ToolWin.Mocks;

type

{ TAnimate }

  TAnimate = class(TWinControl)
  end;

{ TToolBar }

  TToolBar = class(TToolWindow)
  end;

{ TCommonCalendar }

  TCommonCalendar = class(TWinControl)
  end;

implementation

end.
