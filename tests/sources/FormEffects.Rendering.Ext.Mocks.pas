/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Ext.Mocks.pas                        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Ext.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Vcl.Controls,
  FormEffects.Utils.Mocks,
  FormEffects.Vcl.Controls.Mocks;

type

{ TRenderingExtMocks }

  TRenderingExtMocks = class abstract(TMocksManager)
  public
    procedure PaintCopy(const WinControl: TWinControl; const DC: HDC); virtual; abstract;
    procedure NCPrintControl(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC); virtual; abstract;
    procedure PaintThemeBorder(const WinControl: TWinControl; const DC: HDC); virtual; abstract;
    procedure EraseAndPaintMessage(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC); virtual; abstract;
    procedure EmulatePaint(const WinControl: TWinControl; const DC: HDC); virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Ext
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  RenderingExtMocks: TRenderingExtMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TPaintCopy = procedure(DC: HDC; WinControl: TWinControl);

procedure PaintCopyMock(DC: HDC; WinControl: TWinControl);
begin
  RenderingExtMocks.PaintCopy(WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TPaintCopy = procedure(const WinControl: TWinControl; const DC: HDC);

procedure PaintCopyMock(const WinControl: TWinControl; const DC: HDC);
begin
  RenderingExtMocks.PaintCopy(WinControl, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TNCPrintControl = procedure(DC: HDC; WinControl: TWinControl; Window: HWND);

procedure NCPrintControlMock(DC: HDC; WinControl: TWinControl; Window: HWND);
begin
  RenderingExtMocks.NCPrintControl(Window, WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TNCPrintControl = procedure(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);

procedure NCPrintControlMock(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
begin
  RenderingExtMocks.NCPrintControl(Wnd, WinControl, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TPaintThemeBorder = procedure(const WinControl: TWinControl; const DC: HDC; const EraseLRCorner: Boolean);

procedure PaintThemeBorderMock(const WinControl: TWinControl; const DC: HDC; const EraseLRCorner: Boolean);
begin
  RenderingExtMocks.PaintThemeBorder(WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TPaintThemeBorder = procedure(const WinControl: TWinControl; const DC: HDC);

procedure PaintThemeBorderMock(const WinControl: TWinControl; const DC: HDC);
begin
  RenderingExtMocks.PaintThemeBorder(WinControl, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TEraseAndPaintMessage = procedure(DC: HDC; WinControl: TWinControl; Window: HWND);

procedure EraseAndPaintMessageMock(DC: HDC; WinControl: TWinControl; Window: HWND);
begin
  RenderingExtMocks.EraseAndPaintMessage(Window, WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TEraseAndPaintMessage = procedure(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);

procedure EraseAndPaintMessageMock(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
begin
  RenderingExtMocks.EraseAndPaintMessage(Wnd, WinControl, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TEmulatePaint = procedure(DC: HDC; WinControl: TWinControl);

procedure EmulatePaintMock(DC: HDC; WinControl: TWinControl);
begin
  RenderingExtMocks.EmulatePaint(WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TEmulatePaint = procedure(const WinControl: TWinControl; const DC: HDC);

procedure EmulatePaintMock(const WinControl: TWinControl; const DC: HDC);
begin
  RenderingExtMocks.EmulatePaint(WinControl, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TRenderingExtMocks }

constructor TRenderingExtMocks.Create;
begin
  inherited Create;

{$IFDEF FORM_EFFECTS_TESTS}
  {$IFDEF USE_BILLENIUM_EFFECTS}
    AddIntercept<TPaintCopy>(teRender.PaintCopy, PaintCopyMock);
  {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
    AddIntercept<TPaintCopy>(FormEffects.Rendering.Ext.PaintCopy, PaintCopyMock);
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
{$ENDIF ~ FORM_EFFECTS_TESTS}

{$IFDEF FORM_EFFECTS_TESTS}
  {$IFDEF USE_BILLENIUM_EFFECTS}
    AddIntercept<TNCPrintControl>(teRender.NCPrintControl, NCPrintControlMock);
  {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
    AddIntercept<TNCPrintControl>(FormEffects.Rendering.Ext.NCPrintControl, NCPrintControlMock);
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
{$ENDIF ~ FORM_EFFECTS_TESTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TPaintThemeBorder>(PaintThemeBorderExt, PaintThemeBorderMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TPaintThemeBorder>(FormEffects.Rendering.Ext.PaintThemeBorder, PaintThemeBorderMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF FORM_EFFECTS_TESTS}
  {$IFDEF USE_BILLENIUM_EFFECTS}
    AddIntercept<TEraseAndPaintMessage>(teRender.EraseAndPaintMessage, EraseAndPaintMessageMock);
  {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
    AddIntercept<TEraseAndPaintMessage>(FormEffects.Rendering.Ext.EraseAndPaintMessage, EraseAndPaintMessageMock);
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
{$ENDIF ~ FORM_EFFECTS_TESTS}

{$IFDEF FORM_EFFECTS_TESTS}
  {$IFDEF USE_BILLENIUM_EFFECTS}
    AddIntercept<TEmulatePaint>(teRender.EmulatePaint, EmulatePaintMock);
  {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
    AddIntercept<TEmulatePaint>(FormEffects.Rendering.Ext.EmulatePaint, EmulatePaintMock);
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
{$ENDIF ~ FORM_EFFECTS_TESTS}

  RenderingExtMocks := Self;
end;

end.
