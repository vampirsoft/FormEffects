/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.OS.pas                                   *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.OS;

{$INCLUDE FormEffects.inc}

interface

type

{ TWinVersion }

  TWinVersion = class
  private
    class var FWinXP: Boolean;

  strict private
  {$HINTS OFF}
    constructor Create; reintroduce;
  {$HINTS ON}

  public
    class property IsWinXP: Boolean read FWinXP;
  end;

implementation

uses
  System.SysUtils;

{ TWinVersion }

constructor TWinVersion.Create;
begin
// Nothing...
end;

initialization
  TWinVersion.FWinXP := TOSVersion.Check(5, 1);

end.
