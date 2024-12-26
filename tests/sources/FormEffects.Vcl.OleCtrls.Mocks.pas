/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.OleCtrls.Mocks.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.OleCtrls.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  FormEffects.Vcl.Controls.Mocks;

type

{ TOleControl }

  TOleControl = class(TWinControl)
  strict private
    FOleObject: Variant;

  public
    property OleObject: Variant read FOleObject write FOleObject;
  end;

implementation

end.
