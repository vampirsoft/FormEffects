/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.ScrollBars.pas                           *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.ScrollBars;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Forms
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

function IsScrollBarVisible(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean;

implementation

uses
  FormEffects.TypeHelpers;

{$REGION 'Internal definitions'}

type

{ TScrollBarKindHelper }

  TScrollBarKindHelper = record helper for TScrollBarKind
  public
    function GetBarStyle: NativeInt; inline;
    function GetBarType: Integer; inline;
  end;

{ TScrollingWinControlHelper }

  TScrollingWinControlHelper = class helper for TScrollingWinControl
  public
    function GetControlScrollBar(const Kind: TScrollBarKind): TControlScrollBar; inline;
  end;

function GetControlScrollBar(const Control: TControl; const Kind: TScrollBarKind): TControlScrollBar; inline;
begin
  if Control is TScrollingWinControl then
    Exit((Control as TScrollingWinControl).GetControlScrollBar(Kind));
  Result := nil;
end;

{ TScrollBarKindHelper }

function TScrollBarKindHelper.GetBarStyle: NativeInt;
begin
  if Self = sbVertical then
    Exit(WS_VSCROLL);
  Result := WS_HSCROLL;
end;

function TScrollBarKindHelper.GetBarType: Integer;
begin
  if Self = sbVertical then
    Exit(SB_VERT);
  Result := SB_HORZ;
end;

{ TScrollingWinControlHelper }

function TScrollingWinControlHelper.GetControlScrollBar(const Kind: TScrollBarKind): TControlScrollBar;
begin
  if Kind = sbVertical then
    Exit(Self.VertScrollBar);
  Result := Self.HorzScrollBar;
end;

{$ENDREGION 'Internal definitions'}

function IsScrollBarVisible(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean;
begin
  const ControlScrollBar = GetControlScrollBar(Control, Kind);
  const Style            = Kind.GetBarStyle;

  Result := ((ControlScrollBar = nil) or ControlScrollBar.Visible) and (Wnd.GetWindowData(GWL_STYLE) and Style <> 0);

  if Result then
  begin
    const ScrollInfo = TScrollInfo.GetInfo(Wnd, Kind.GetBarType, SIF_RANGE);
    Result := (ScrollInfo.nMin <> 0) or (ScrollInfo.nMax <> 0);
  end;
end;

end.
