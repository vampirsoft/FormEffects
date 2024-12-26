/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.ScrollBars.Mocks.pas                     *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.ScrollBars.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Forms,
  FormEffects.Utils.Mocks
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

type

{ TUtilsScrollBarsMocks }

  TUtilsScrollBarsMocks = class abstract(TMocksManager)
  public
    function IsScrollBarVisible(
      const Wnd: HWND;
      const Control: TControl;
      const Kind: TScrollBarKind
    ): Boolean; virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.ScrollBars
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  UtilsScrollBarsMocks: TUtilsScrollBarsMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TIsScrollBarVisible = function(Control: TControl; Window: HWND; Kind: TScrollBarKind): Boolean;

function IsScrollBarVisibleMock(Control: TControl; Wnd: HWND; Kind: TScrollBarKind): Boolean;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TIsScrollBarVisible = function(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean;

function IsScrollBarVisibleMock(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
begin
  Result := UtilsScrollBarsMocks.IsScrollBarVisible(Wnd, Control, Kind);
end;

{ TUtilsScrollBarsMocks }

constructor TUtilsScrollBarsMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TIsScrollBarVisible>(teRender.IsScrollBarVisible, IsScrollBarVisibleMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TIsScrollBarVisible>(FormEffects.Utils.ScrollBars.IsScrollBarVisible, IsScrollBarVisibleMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  UtilsScrollBarsMocks := Self;
end;

end.
