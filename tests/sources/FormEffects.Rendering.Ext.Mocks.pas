/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Ext.Mocks.pas                        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Ext.Mocks;

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

{ TRenderingExtMocks }

  TRenderingExtMocks = class(TMocksManager)
  private type
    TPaintCopyResultReference = reference to procedure(const WinControl: TWinControl; const DC: HDC);
  public
    procedure PaintCopy_Result(const Callback: TPaintCopyResultReference);

  private type
    TNCPrintControlResultReference =
      reference to procedure(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
  public
    procedure NCPrintControl_Result(const Callback: TNCPrintControlResultReference);

  private type
    TPaintThemeBorderResultReference = reference to procedure(const WinControl: TWinControl; const DC: HDC);
  public
    procedure PaintThemeBorder_Result(const Callback: TPaintThemeBorderResultReference);

  private type
    TEraseAndPaintMessageResultReference =
      reference to procedure(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
  public
    procedure EraseAndPaintMessage_Result(const Callback: TEraseAndPaintMessageResultReference);

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Ext
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  PaintCopyResultReference: TRenderingExtMocks.TPaintCopyResultReference = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TPaintCopy = procedure(DC: HDC; WinControl: TWinControl);

procedure PaintCopyMock(DC: HDC; WinControl: TWinControl);
begin
  PaintCopyResultReference(WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TPaintCopy = procedure(const WinControl: TWinControl; const DC: HDC);

procedure PaintCopyMock(const WinControl: TWinControl; const DC: HDC);
begin
  PaintCopyResultReference(WinControl, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TRenderingExtMocks.PaintCopy_Result(const Callback: TPaintCopyResultReference);
begin
  PaintCopyResultReference := Callback;
end;

var
  NCPrintControlResultReference: TRenderingExtMocks.TNCPrintControlResultReference = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TNCPrintControl = procedure(DC: HDC; WinControl: TWinControl; Window: HWND);

procedure NCPrintControlMock(DC: HDC; WinControl: TWinControl; Window: HWND);
begin
  NCPrintControlResultReference(Window, WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TNCPrintControl = procedure(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);

procedure NCPrintControlMock(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
begin
  NCPrintControlResultReference(Wnd, WinControl, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TRenderingExtMocks.NCPrintControl_Result(const Callback: TNCPrintControlResultReference);
begin
  NCPrintControlResultReference := Callback;
end;

var
  PaintThemeBorderResultReference: TRenderingExtMocks.TPaintThemeBorderResultReference = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TPaintThemeBorder = procedure(const WinControl: TWinControl; const DC: HDC; const EraseLRCorner: Boolean);

procedure PaintThemeBorderMock(const WinControl: TWinControl; const DC: HDC; const EraseLRCorner: Boolean);
begin
  PaintThemeBorderResultReference(WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TPaintThemeBorder = procedure(const WinControl: TWinControl; const DC: HDC);

procedure PaintThemeBorderMock(const WinControl: TWinControl; const DC: HDC);
begin
  PaintThemeBorderResultReference(WinControl, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TRenderingExtMocks.PaintThemeBorder_Result(const Callback: TPaintThemeBorderResultReference);
begin
  PaintThemeBorderResultReference := Callback;
end;

var
  EraseAndPaintMessageResultReference: TRenderingExtMocks.TEraseAndPaintMessageResultReference = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TEraseAndPaintMessage = procedure(DC: HDC; WinControl: TWinControl; Window: HWND);

procedure EraseAndPaintMessageMock(DC: HDC; WinControl: TWinControl; Window: HWND);
begin
  EraseAndPaintMessageResultReference(Window, WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TEraseAndPaintMessage = procedure(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);

procedure EraseAndPaintMessageMock(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
begin
  EraseAndPaintMessageResultReference(Wnd, WinControl, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TRenderingExtMocks.EraseAndPaintMessage_Result(const Callback: TEraseAndPaintMessageResultReference);
begin
  EraseAndPaintMessageResultReference := Callback;
end;

{ TRenderingExtMocks }

constructor TRenderingExtMocks.Create;
begin
  inherited Create;

{$IFDEF FORM_EFFECTS_TESTS}
  AddIntercept<TPaintCopy>(PaintCopy, PaintCopyMock);
{$ENDIF ~ FORM_EFFECTS_TESTS}
  PaintCopy_Result(
    procedure(const WinControl: TWinControl; const DC: HDC)
    begin
    end
  );

{$IFDEF FORM_EFFECTS_TESTS}
  AddIntercept<TNCPrintControl>(NCPrintControl, NCPrintControlMock);
{$ENDIF ~ FORM_EFFECTS_TESTS}
  NCPrintControl_Result(
    procedure(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC)
    begin
    end
  );

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TPaintThemeBorder>(PaintThemeBorderExt, PaintThemeBorderMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TPaintThemeBorder>(PaintThemeBorder, PaintThemeBorderMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  PaintThemeBorder_Result(
    procedure(const WinControl: TWinControl; const DC: HDC)
    begin
    end
  );

{$IFDEF FORM_EFFECTS_TESTS}
  AddIntercept<TEraseAndPaintMessage>(EraseAndPaintMessage, EraseAndPaintMessageMock);
{$ENDIF ~ FORM_EFFECTS_TESTS}
  EraseAndPaintMessage_Result(
    procedure(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC)
    begin
    end
  );
end;

end.
