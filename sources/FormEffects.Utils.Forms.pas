/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Forms.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  Vcl.Controls,
  FormEffects.TypeHelpers
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Winapi.Windows.Mocks
  , FormEffects.Vcl.Controls.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

const MDI_CLIENT_CLASS_NAME = 'MDICLIENT';

function IsMDIFormWithMaximizedMDIChild(const WinControl: TWinControl): Boolean; inline;
{$MESSAGE 'Need to test IsMaximizedMDIClient'}
function IsMaximizedMDIClient(ClassName: string): Boolean;
{$MESSAGE 'Need to test IsMaximizedMDIChild'}
function IsMaximizedMDIChild(const WinControl: TWinControl): Boolean;
{$MESSAGE 'Need to test GetClientSize'}
procedure GetClientSize(const Wnd: HWND; const WinControl: TWinControl; const IsMaximizedMDIChild: Boolean;
  out ClientSize: TSize; out ClientOrgPoint: TPoint);

implementation

uses
  System.SysUtils, System.Types,
  Vcl.Forms
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

function IsMDIFormWithMaximizedMDIChild(const WinControl: TWinControl): Boolean;
begin
  Result :=
    (WinControl is TCustomForm)                                               and
    ((WinControl as TCustomForm).GetInternalFormStyle = TFormStyle.fsMDIForm) and
    IsMaximizedMDIClient(MDI_CLIENT_CLASS_NAME);
end;

function IsMaximizedMDIClient(ClassName: string): Boolean;
begin
  if SameText(ClassName, MDI_CLIENT_CLASS_NAME) then
  begin
    for var I := 0 to Application.MainForm.MDIChildCount - 1 do
    begin
      const Child = Application.MainForm.MDIChildren[I];
      if Child.WindowState = TWindowState.wsMaximized then Exit(True);
    end;
  end;

  Result := False;
end;

function IsMaximizedMDIChild(const WinControl: TWinControl): Boolean;
begin
  if
    (WinControl is TCustomForm)                                                and
    ((WinControl as TCustomForm).GetInternalFormStyle = TFormStyle.fsMDIChild) and
    (Application.MainForm <> nil)                                              and
    (Application.MainForm.GetInternalFormStyle = TFormStyle.fsMDIForm)
  then
  begin
    if (WinControl as TCustomForm).WindowState = wsMaximized then Exit(True);

    for var I := 0 to Application.MainForm.MDIChildCount - 1 do
    begin
      const Child = Application.MainForm.MDIChildren[I];
      if Child.WindowState = wsMaximized then Exit(True);
    end;
  end;

  Result := False;
end;

procedure GetClientSize(const Wnd: HWND; const WinControl: TWinControl; const IsMaximizedMDIChild: Boolean;
  out ClientSize: TSize; out ClientOrgPoint: TPoint);
begin
  if IsMaximizedMDIChild then
  begin
    ClientOrgPoint := TPoint.InlineCreate(WinControl.GetInternalBorderWidth, WinControl.GetInternalBorderWidth);
  end
  else
  begin
    var WindowRect := TRect.CreateWindowRect(Wnd);
    var Point := TPoint.Zero;
    Wnd.MapWindowToScreen(Point);
    ClientOrgPoint := Point.SubtractInline(WindowRect.TopLeft);
    Wnd.MapWindowFromScreen(WindowRect);
  end;

  const ClientRect = TRect.CreateClientRect(Wnd);
  ClientSize := TSize.InlineCreate(ClientRect);
end;

end.
