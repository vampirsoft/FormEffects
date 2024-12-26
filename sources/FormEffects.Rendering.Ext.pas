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
  Vcl.Controls
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

procedure PaintCopy(const WinControl: TWinControl; const DC: HDC);
procedure NCPrintControl(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);

implementation

uses
  System.Types,
  Winapi.Messages,
  Vcl.Forms,
  FormEffects.TypeHelpers,
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
    TWinVersion.IsWinXP                                              and
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

end.
