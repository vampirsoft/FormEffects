/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Ext.pas                              *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Ext;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  Vcl.Controls,
  Vcl.OleCtrls,
  Vcl.Themes,
  FormEffects.TypeHelpers
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.OleCtrls.Mocks
  , FormEffects.Vcl.Themes.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

procedure PaintCopy(const WinControl: TWinControl; const DC: HDC);
procedure NCPrintControl(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
procedure PaintThemeBorder(const WinControl: TWinControl; const DC: HDC);{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
procedure EraseAndPaintMessage(
  const Wnd: HWND;
  const WinControl: TWinControl;
  const DC: HDC
);{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
procedure EmulatePaint(const WinControl: TWinControl; const DC: HDC);{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}

implementation

uses
  Winapi.Messages,
  Winapi.ActiveX,
  System.Types,
  System.SysUtils,
  Vcl.Forms,
  FormEffects.Utils.OS,
  FormEffects.Utils.Windows,
  FormEffects.Rendering,
  FormEffects.Rendering.Pictures
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

procedure PaintCopy(const WinControl: TWinControl; const DC: HDC);
begin
  if WinControl = nil then
    Exit;

  WinControl.ControlState := WinControl.ControlState + [csPaintCopy];
  try
    WinControl.Handle.SendMessage(WM_PAINT, DC, FE_ID);
  finally
    WinControl.ControlState := WinControl.ControlState - [csPaintCopy];
  end;
end;

procedure NCPrintControl(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
begin
  if
    (WinControl is TCustomForm)                                      and
    ((WinControl as TCustomForm).GetProtectedFormStyle = fsMDIChild) and
    IsWinXPUp                                                        and
    HasWindowRegion(Wnd)
  then
  begin // XP does something weird with the clipping region
    AdjustBitmap(
      DC,
      TSize.InlineCreate(WinControl),
      procedure(const CanvasDC: HDC)
      begin
        Wnd.SendMessage(WM_PRINT, CanvasDC, PRF_NONCLIENT);
      end
    );
  end
  else
    Wnd.SendMessage(WM_PRINT, DC, PRF_NONCLIENT);
end;

procedure PaintThemeBorder(const WinControl: TWinControl; const DC: HDC);
begin
  const WinControlWnd = WinControl.Handle;

  const ExStyle = WinControlWnd.GetWindowData(GWL_EXSTYLE);
  if (ExStyle and WS_EX_CLIENTEDGE) = 0 then
    Exit;

  const DrawRect = WinControlWnd.GetWindowRect;

  DrawRect.OffsetRect(-DrawRect.TopLeft);
  DC.ExcludeClip(DrawRect, 2);

  const StyleService = StyleServices;
  const Details = StyleService.GetElementDetails(teEditTextNormal);
  StyleService.DrawElement(DC, Details, DrawRect);
end;

procedure EraseAndPaintMessage(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
begin
  const DoubleBuffered = Assigned(WinControl) and WinControl.DoubleBuffered;
  if DoubleBuffered then
    WinControl.DoubleBuffered := False;

  with DC.Save do
  try
    Wnd.SendMessage(WM_ERASEBKGND, DC, 0);
  finally
    RestoreDC;
  end;
  Wnd.SendMessage(WM_PAINT, DC, FE_ID);

  if DoubleBuffered then
    WinControl.DoubleBuffered := True;
end;

procedure EmulatePaint(const WinControl: TWinControl; const DC: HDC);
begin
  if WinControl is TOleControl then
  begin
    WinControl.HandleNeeded;
    (WinControl as TOleControl).OleDraw(DC);
  end;
end;

end.
