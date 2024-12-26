/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Forms.Mocks.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  Vcl.Controls,
  FormEffects.Utils.Mocks,
  FormEffects.Vcl.Controls.Mocks;

type

{ TUtilsFormsMocks }

  TUtilsFormsMocks = class abstract(TMocksManager)
  public
{$IFNDEF USE_BILLENIUM_EFFECTS}
    function HasMainFormMaximizedMDIChild: Boolean; virtual; abstract;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
    function IsMDIFormWithMaximizedMDIChild(const WinControl: TWinControl): Boolean; virtual;
    function IsMaximizedMDIClient(const ClassName: string): Boolean; virtual; abstract;
    function IsMaximizedMDIChild(const WinControl: TWinControl): Boolean; virtual;
    function GetClientSize(
      const Wnd: HWND;
      const WinControl: TWinControl;
      const IsMaximizedMDIChild: Boolean
    ): TSize; virtual; abstract;
    function GetClientOrgPoint(
      const Wnd: HWND;
      const WinControl: TWinControl;
      const IsMaximizedMDIChild: Boolean
    ): TPoint; virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Forms
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  UtilsFormsMocks: TUtilsFormsMocks;

{$IFNDEF USE_BILLENIUM_EFFECTS}

type
  THasMainFormMaximizedMDIChild = function: Boolean;

function HasMainFormMaximizedMDIChildMock: Boolean;
begin
  Result := UtilsFormsMocks.HasMainFormMaximizedMDIChild;
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TIsMDIFormWithMaximizedMDIChild = function(WinControl: TWinControl): Boolean;

function IsMDIFormWithMaximizedMDIChildMock(WinControl: TWinControl): Boolean;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TIsMDIFormWithMaximizedMDIChild = function(const WinControl: TWinControl): Boolean;

function IsMDIFormWithMaximizedMDIChildMock(const WinControl: TWinControl): Boolean;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
begin
  Result := UtilsFormsMocks.IsMDIFormWithMaximizedMDIChild(WinControl);
end;

function TUtilsFormsMocks.IsMDIFormWithMaximizedMDIChild(const WinControl: TWinControl): Boolean;
begin
  Result := False;
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TIsMaximizedMDIClient = function(ClassName: PChar): Boolean;

function IsMaximizedMDIClientMock(ClassName: PChar): Boolean;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TIsMaximizedMDIClient = function(const ClassName: string): Boolean;

function IsMaximizedMDIClientMock(const ClassName: string): Boolean;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
begin
  Result := UtilsFormsMocks.IsMaximizedMDIClient(ClassName);
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TIsMaximizedMDIChild = function(WinControl: TWinControl): Boolean;

function IsMaximizedMDIChildMock(WinControl: TWinControl): Boolean;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TIsMaximizedMDIChild = function(const WinControl: TWinControl): Boolean;

function IsMaximizedMDIChildMock(const WinControl: TWinControl): Boolean;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
begin
  Result := UtilsFormsMocks.IsMaximizedMDIChild(WinControl);
end;

function TUtilsFormsMocks.IsMaximizedMDIChild(const WinControl: TWinControl): Boolean;
begin
  Result := False;
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TGetClientSize =
    procedure(
      WinControl: TWinControl;
      Window: HWnd;
      IsMaximizedMDIClient, IsMaximizedMDIChild: Boolean;
      var ClientWidth, ClientHeight: Integer;
      var ClientOrg: TPoint
    );

procedure GetClientSizeMock(
  WinControl: TWinControl;
  Window: HWnd;
  IsMaximizedMDIClient, IsMaximizedMDIChild: Boolean;
  var ClientWidth, ClientHeight: Integer;
  var ClientOrg: TPoint
);
begin
  ClientOrg       := UtilsFormsMocks.GetClientOrgPoint(Window, WinControl, IsMaximizedMDIChild);
  const ClientSize = UtilsFormsMocks.GetClientSize(Window, WinControl, IsMaximizedMDIChild);
  ClientWidth  := ClientSize.Width;
  ClientHeight := ClientSize.Height;
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TGetClientSize =
    procedure(
      const Wnd: HWND;
      const WinControl: TWinControl;
      const IsMaximizedMDIChild: Boolean;
      out ClientSize: TSize;
      out ClientOrgPoint: TPoint
    );

procedure GetClientSizeMock(
  const Wnd: HWND;
  const WinControl: TWinControl;
  const IsMaximizedMDIChild: Boolean;
  out ClientSize: TSize;
  out ClientOrgPoint: TPoint
);
begin
  ClientSize     := UtilsFormsMocks.GetClientSize(Wnd, WinControl, IsMaximizedMDIChild);
  ClientOrgPoint := UtilsFormsMocks.GetClientOrgPoint(Wnd, WinControl, IsMaximizedMDIChild);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TUtilsFormsMocks }

constructor TUtilsFormsMocks.Create;
begin
  inherited Create;

{$IFNDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<THasMainFormMaximizedMDIChild>(
    FormEffects.Utils.Forms.HasMainFormMaximizedMDIChild,
    HasMainFormMaximizedMDIChildMock
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TIsMDIFormWithMaximizedMDIChild>(GetMDIFormWithMaximizedMDIChild, IsMDIFormWithMaximizedMDIChildMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TIsMDIFormWithMaximizedMDIChild>(
    FormEffects.Utils.Forms.IsMDIFormWithMaximizedMDIChild,
    IsMDIFormWithMaximizedMDIChildMock
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TIsMaximizedMDIClient>(GetMaximizedMDIClient, IsMaximizedMDIClientMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TIsMaximizedMDIClient>(FormEffects.Utils.Forms.IsMaximizedMDIClient, IsMaximizedMDIClientMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TIsMaximizedMDIChild>(GetMaximizedMDIChild, IsMaximizedMDIChildMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TIsMaximizedMDIChild>(FormEffects.Utils.Forms.IsMaximizedMDIChild, IsMaximizedMDIChildMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TGetClientSize>(teRender.GetClientSize, GetClientSizeMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetClientSize>(FormEffects.Utils.Forms.GetClientSize, GetClientSizeMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  UtilsFormsMocks := Self;
end;

end.
