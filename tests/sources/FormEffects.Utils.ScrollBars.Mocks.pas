/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.ScrollBars.Mocks.pas                     *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.ScrollBars.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Vcl.Controls,  Vcl.Forms
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

function IsScrollBarVisible(const Control: TControl; const Wnd: HWND; const Kind: TScrollBarKind): Boolean;
procedure SetResul_IsScrollBarVisible(const Result: Boolean);

var
  IsScrollBarVisible_Control: TControl;
  IsScrollBarVisible_Wnd: HWND;
  IsScrollBarVisible_Kind: TScrollBarKind;

implementation

var
  IsScrollBarVisibleResult: Boolean;

function IsScrollBarVisible(const Control: TControl; const Wnd: HWND; const Kind: TScrollBarKind): Boolean;
begin
  IsScrollBarVisible_Control := Control;
  IsScrollBarVisible_Wnd     := Wnd;
  IsScrollBarVisible_Kind    := Kind;

  Result := IsScrollBarVisibleResult;
end;

procedure SetResul_IsScrollBarVisible(const Result: Boolean);
begin
  IsScrollBarVisibleResult := Result;
end;

end.
