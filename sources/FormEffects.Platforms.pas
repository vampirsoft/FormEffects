/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Platforms.pas                                  *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Platforms;

{$INCLUDE FormEffects.inc}

interface

uses
  System.SysUtils;

{$IFDEF USE_TRANSITION_EFFECTS}
function IsWinNTUp   : Boolean;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
function IsWin2000Up : Boolean;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
{$ENDIF ~ USE_TRANSITION_EFFECTS}
function IsWinXPUp   : Boolean;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
{$IFDEF USE_TRANSITION_EFFECTS}
function IsWinVistaUp: Boolean;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
{$ENDIF ~ USE_TRANSITION_EFFECTS}

implementation

{$IFDEF USE_TRANSITION_EFFECTS}

function IsWinNTUp: Boolean;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Result := False
  else
    Result := True;
end;

function IsWin2000Up: Boolean;
begin
  Result := TOSVersion.Check(5);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

function IsWinXPUp: Boolean;
begin
  Result := TOSVersion.Check(5, 1);
end;

{$IFDEF USE_TRANSITION_EFFECTS}

function IsWinVistaUp: Boolean;
begin
  Result := TOSVersion.Check(6);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

end.
