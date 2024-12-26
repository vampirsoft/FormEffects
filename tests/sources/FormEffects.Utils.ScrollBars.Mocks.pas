/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.ScrollBars.Mocks.pas                     *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
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

  TUtilsScrollBarsMocks = class(TMocksManager)
  private type
    TIsScrollBarVisibleResultReference =
      reference to function(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean;
  public
    procedure IsScrollBarVisible_Result(const IsScrollBarVisibleResult: Boolean); overload;
    procedure IsScrollBarVisible_Result(const Callback: TIsScrollBarVisibleResultReference); overload;

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
  IsScrollBarVisibleResultReference: TUtilsScrollBarsMocks.TIsScrollBarVisibleResultReference = nil;

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
  Result := IsScrollBarVisibleResultReference(Wnd, Control, Kind);
end;

procedure TUtilsScrollBarsMocks.IsScrollBarVisible_Result(const IsScrollBarVisibleResult: Boolean);
begin
  IsScrollBarVisible_Result(
    function(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean
    begin
      Result := IsScrollBarVisibleResult;
    end
  );
end;

procedure TUtilsScrollBarsMocks.IsScrollBarVisible_Result(const Callback: TIsScrollBarVisibleResultReference);
begin
  if Assigned(Callback) then
    IsScrollBarVisibleResultReference := Callback
  else
  begin
    IsScrollBarVisibleResultReference :=
      function(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean
      begin
        Result := False;
      end;
  end;
end;

{ TUtilsScrollBarsMocks }

constructor TUtilsScrollBarsMocks.Create;
begin
  inherited Create;

  AddIntercept<TIsScrollBarVisible>(IsScrollBarVisible, IsScrollBarVisibleMock);
  IsScrollBarVisible_Result(nil);
end;

end.
