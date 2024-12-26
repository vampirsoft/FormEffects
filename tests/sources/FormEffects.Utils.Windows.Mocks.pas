/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Windows.Mocks.pas                        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Windows.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  FormEffects.Utils.Mocks;

type

{ TUtilsWindowsMocks }

  TUtilsWindowsMocks = class abstract(TMocksManager)
  public
    function GetWindowOffset(const Wnd: HWND): TPoint; virtual; abstract;
    function HasWindowRegion(const Wnd: HWND): Boolean; virtual; abstract;
    function GetWindowSize(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize; virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Windows
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  UtilsWindowsMocks: TUtilsWindowsMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TGetWindowOffset = function(Window: HWND): TPoint;

function GetWindowOffsetMock(Window: HWND): TPoint;
begin
  Result := UtilsWindowsMocks.GetWindowOffset(Window);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TGetWindowOffset = function(const Wnd: HWND): TPoint;

function GetWindowOffsetMock(const Wnd: HWND): TPoint;
begin
  Result := UtilsWindowsMocks.GetWindowOffset(Wnd);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  THasWindowRegion = function(Window: HWnd): Boolean;

function HasWindowRegionMock(Window: HWND): Boolean;
begin
   Result := UtilsWindowsMocks.HasWindowRegion(Window);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  THasWindowRegion = function(const Wnd: HWND): Boolean;

function HasWindowRegionMock(const Wnd: HWND): Boolean;
begin
   Result := UtilsWindowsMocks.HasWindowRegion(Wnd);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TGetWindowSize = procedure(Window: HWnd; IsMaximizedMDIChild: Boolean; var Width, Height: Integer);

procedure GetWindowSizeMock(Window: HWnd; IsMaximizedMDIChild: Boolean; var Width, Height: Integer);
begin
  const Result = UtilsWindowsMocks.GetWindowSize(Window, IsMaximizedMDIChild);
  Width  := Result.Width;
  Height := Result.Height;
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TGetWindowSize = function(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize;

function GetWindowSizeMock(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize;
begin
  Result := UtilsWindowsMocks.GetWindowSize(Wnd, IsMaximizedMDIChild);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TUtilsWindowsMocks }

constructor TUtilsWindowsMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetWindowOffset>(WindowClientOffset, GetWindowOffsetMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetWindowOffset>(FormEffects.Utils.Windows.GetWindowOffset, GetWindowOffsetMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<THasWindowRegion>(WindowHasRegion, HasWindowRegionMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<THasWindowRegion>(FormEffects.Utils.Windows.HasWindowRegion, HasWindowRegionMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TGetWindowSize>(GetSize, GetWindowSizeMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetWindowSize>(FormEffects.Utils.Windows.GetWindowSize, GetWindowSizeMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  UtilsWindowsMocks := Self;
end;

end.
