/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.Themes.Mocks.pas                           *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.Themes.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Themes,
  FormEffects.Utils.Mocks,
  FormEffects.Vcl.Controls.Mocks;

type

{ TCustomStyleServices }

  TCustomStyleServices = class
  public
    function GetEnabled: Boolean; virtual;
    function ThemesEnabled: Boolean; inline; deprecated 'Use TCustomStyleServices.Enabled';
    function GetElementDetails(Detail: TThemedEdit): TThemedElementDetails; virtual;
    function DrawElement(
      DC: HDC;
      Details: TThemedElementDetails;
      const R: TRect;
      ClipRect: PRect = nil;
      DPI: Integer = 0
    ): Boolean; virtual;
    function DrawParentBackground(
      Window: HWND;
      Target: HDC;
      Details: PThemedElementDetails;
      OnlyIfTransparent: Boolean;
      Bounds: PRect = nil
    ): Boolean; virtual;

  public
    property Enabled: Boolean read GetEnabled;
  end;

  TVclThemesMocks = class abstract(TMocksManager)
  public
    function StyleServices: TCustomStyleServices; virtual; abstract;

  public
    constructor Create; override;
  end;

function StyleServices(AControl: TControl = nil): TCustomStyleServices;
function ThemeServices: TCustomStyleServices; inline; deprecated 'Use StyleServices';

implementation

uses
  System.SysUtils;

var
  FStyleService: TCustomStyleServices;

function StyleServices(AControl: TControl = nil): TCustomStyleServices;
begin
  if FStyleService = nil then
    FStyleService := TCustomStyleServices.Create;
  Result := FStyleService;
end;

function ThemeServices: TCustomStyleServices;
begin
  Result := StyleServices;
end;

{ TCustomStyleServices }

function TCustomStyleServices.DrawElement(
  DC: HDC;
  Details: TThemedElementDetails;
  const R: TRect;
  ClipRect: PRect;
  DPI: Integer
): Boolean;
begin
  Result := False;
end;

function TCustomStyleServices.DrawParentBackground(
  Window: HWND;
  Target: HDC;
  Details: PThemedElementDetails;
  OnlyIfTransparent: Boolean;
  Bounds: PRect
): Boolean;
begin
  Result := False;
end;

function TCustomStyleServices.GetElementDetails(Detail: TThemedEdit): TThemedElementDetails;
begin
end;

function TCustomStyleServices.GetEnabled: Boolean;
begin
  Result := False;
end;

function TCustomStyleServices.ThemesEnabled: Boolean;
begin
  Result := GetEnabled;
end;

var
  VclThemesMocks: TVclThemesMocks;

type
  TStyleServices = function(AControl: TControl): TCustomStyleServices;

function StyleServicesMock(AControl: TControl): TCustomStyleServices;
begin
  Result := VclThemesMocks.StyleServices;
end;

{ TVclThemesMocks }

constructor TVclThemesMocks.Create;
begin
  inherited;

  AddIntercept<TStyleServices>(FormEffects.Vcl.Themes.Mocks.StyleServices, StyleServicesMock);

  VclThemesMocks := Self;
end;

initialization

finalization
  if Assigned(FStyleService) then
    FreeAndNil(FStyleService);

end.
