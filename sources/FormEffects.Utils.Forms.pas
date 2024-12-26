/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Forms.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  Vcl.Controls
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

function HasMainFormMaximizedMDIChild: Boolean;
function IsMDIFormWithMaximizedMDIChild(
  const WinControl: TWinControl
): Boolean;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
function IsMaximizedMDIClient(
  const ClassName: string
): Boolean;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
function IsMaximizedMDIChild(const WinControl: TWinControl): Boolean;
procedure GetClientSize(
  const Wnd: HWND;
  const WinControl: TWinControl;
  const IsMaximizedMDIChild: Boolean;
  out ClientSize: TSize;
  out ClientOrgPoint: TPoint
);

implementation

uses
  Vcl.Forms,
{$IFDEF FORM_EFFECTS_TESTS}
  FormEffects.Vcl.Forms.Mocks,
{$ENDIF ~ FORM_EFFECTS_TESTS}
  FormEffects.Constants,
  FormEffects.TypeHelpers;

function HasMainFormMaximizedMDIChild: Boolean;
begin
  for var I := 0 to Application.MainForm.MDIChildCount - 1 do
  begin
    const Child = Application.MainForm.MDIChildren[I];
    if Child.WindowState = wsMaximized then
      Exit(True);
  end;

  Result := False;
end;

function IsMDIFormWithMaximizedMDIChild(const WinControl: TWinControl): Boolean;
begin
  Result :=
    (WinControl is TCustomForm)                                 and
    (TCustomForm(WinControl).GetProtectedFormStyle = fsMDIForm) and
    HasMainFormMaximizedMDIChild;
end;

function IsMaximizedMDIClient(const ClassName: string): Boolean;
begin
  if SameText(ClassName, MDI_CLIENT_CLASS_NAME) then
    Result := HasMainFormMaximizedMDIChild
  else
    Result := False;
end;

function IsMaximizedMDIChild(const WinControl: TWinControl): Boolean;
begin
  if not (WinControl is TCustomForm) or (Application.MainForm = nil) then
    Exit(False);

  with TCustomForm(WinControl) do
  if (GetProtectedFormStyle = fsMDIChild) and (Application.MainForm.GetProtectedFormStyle = fsMDIForm) then
  begin
    if WindowState = wsMaximized then
      Exit(True);

    Exit(HasMainFormMaximizedMDIChild);
  end;

  Result := False;
end;

procedure GetClientSize(
  const Wnd: HWND;
  const WinControl: TWinControl;
  const IsMaximizedMDIChild: Boolean;
  out ClientSize: TSize;
  out ClientOrgPoint: TPoint
);
begin
  if IsMaximizedMDIChild then
    ClientOrgPoint := TPoint.InlineCreate(WinControl.GetProtectedBorderWidth)
  else
  begin
    var Point := TPoint.Zero;
    Point.MapWindowPointToScreen(Wnd);

    const WindowRect = Wnd.GetWindowRect;
    ClientOrgPoint := Point.InlineSubtract(WindowRect.TopLeft);
    WindowRect.MapWindowRectFromScreen(Wnd);
  end;

  const ClientRect = Wnd.GetClientRect;
  ClientSize := TSize.InlineCreate(ClientRect);
end;

end.
