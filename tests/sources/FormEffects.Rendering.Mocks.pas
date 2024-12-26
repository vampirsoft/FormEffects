/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Mocks.pas                            *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Vcl.Controls,
  FormEffects.Utils.Mocks
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

type

{ TRenderingMocks }

  TRenderingMocks = class abstract(TMocksManager)
  public
    procedure RenderWindowToDC(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const DC: HDC;
      const Rect: TRect;
      const ClientCoordinates, CheckVisibility, CheckRegion: Boolean
    ); virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  RenderingMocks: TRenderingMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TRenderWindowToDC =
    procedure(
      Window, StopWnd: HWND;
      WinControl: TWinControl;
      DC: HDC;
      R: TRect;
      ClientCoordinates, CheckVisibility, CheckRegion, Fast: Boolean
    );

procedure RenderWindowToDCMock(
  Wnd, StopWnd: HWND;
  WinControl: TWinControl;
  DC: HDC;
  Rect: TRect;
  ClientCoordinates, CheckVisibility, CheckRegion, Fast: Boolean
);
begin
  RenderingMocks.RenderWindowToDC(
    Wnd,
    StopWnd,
    WinControl,
    DC,
    Rect,
    ClientCoordinates,
    CheckVisibility,
    CheckRegion
  );
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TRenderWindowToDC =
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const DC: HDC;
      const Rect: TRect;
      const ClientCoordinates, CheckVisibility, CheckRegion: Boolean
    );

procedure RenderWindowToDCMock(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const DC: HDC;
  const Rect: TRect;
  const ClientCoordinates, CheckVisibility, CheckRegion: Boolean
);
begin
  RenderingMocks.RenderWindowToDC(
    Wnd,
    StopWnd,
    WinControl,
    DC,
    Rect,
    ClientCoordinates,
    CheckVisibility,
    CheckRegion
  );
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TRenderingMocks }

constructor TRenderingMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TRenderWindowToDC>(teRender.RenderWindowToDC, RenderWindowToDCMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TRenderWindowToDC>(FormEffects.Rendering.RenderWindowToDC, RenderWindowToDCMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  RenderingMocks := Self;
end;

end.
