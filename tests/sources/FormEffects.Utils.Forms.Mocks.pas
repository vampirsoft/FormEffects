/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Forms.Mocks.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Forms.Mocks;

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

{ TUtilsFormsMocks }

  TUtilsFormsMocks = class(TMocksManager)
{$IFNDEF USE_BILLENIUM_EFFECTS}
  public
    procedure SetResult_HasMainFormMaximizedMDIChild(const HasMainFormMaximizedMDIChildResult: Boolean); overload;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  private type
    TIsMDIFormWithMaximizedMDIChildResultReference = reference to function(const WinControl: TWinControl): Boolean;
  public
    procedure IsMDIFormWithMaximizedMDIChild_Result(const IsMDIFormWithMaximizedMDIChild: Boolean); overload;
    procedure IsMDIFormWithMaximizedMDIChild_Result(
      const Callback: TIsMDIFormWithMaximizedMDIChildResultReference
    ); overload;

  private type
    TIsMaximizedMDIClientResultReference = reference to function(const ClassName: string): Boolean;
  public
    procedure IsMaximizedMDIClient_Result(const IsMaximizedMDIClientResult: Boolean); overload;
    procedure IsMaximizedMDIClient_Result(const Callback: TIsMaximizedMDIClientResultReference); overload;

  private type
    TIsMaximizedMDIChildResultReference = reference to function(const WinControl: TWinControl): Boolean;
  public
    procedure IsMaximizedMDIChild_Result(const IsMaximizedMDIChildResult: Boolean); overload;
    procedure IsMaximizedMDIChild_Result(const Callback: TIsMaximizedMDIChildResultReference); overload;

  private type
    TGetClientSizeResultReference =
      reference to procedure(
        const Wnd: HWND;
        const WinControl: TWinControl;
        const IsMaximizedMDIChild: Boolean;
        out ClientSize: TSize;
        out ClientOrgPoint: TPoint
      );
  public
    procedure GetClientSize_Result(const ClientSizeResult: TSize; const ClientOrgPointResult: TPoint); overload;
    procedure GetClientSize_Result(const Callback: TGetClientSizeResultReference); overload;

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

{$IFNDEF USE_BILLENIUM_EFFECTS}

var
  HasMainFormMaximizedMDIChildResult: Boolean;

type
  THasMainFormMaximizedMDIChild = function: Boolean;

function HasMainFormMaximizedMDIChildMock: Boolean;
begin
  Result := HasMainFormMaximizedMDIChildResult;
end;

procedure TUtilsFormsMocks.SetResult_HasMainFormMaximizedMDIChild(const HasMainFormMaximizedMDIChildResult: Boolean);
begin
  FormEffects.Utils.Forms.Mocks.HasMainFormMaximizedMDIChildResult := HasMainFormMaximizedMDIChildResult;
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

var
  IsMDIFormWithMaximizedMDIChildResultReference: TUtilsFormsMocks.TIsMDIFormWithMaximizedMDIChildResultReference = nil;

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
  Result := IsMDIFormWithMaximizedMDIChildResultReference(WinControl);
end;

procedure TUtilsFormsMocks.IsMDIFormWithMaximizedMDIChild_Result(const IsMDIFormWithMaximizedMDIChild: Boolean);
begin
  IsMDIFormWithMaximizedMDIChild_Result(
    function(const WinControl: TWinControl): Boolean
    begin
      Result := IsMDIFormWithMaximizedMDIChild;
    end
  );
end;

procedure TUtilsFormsMocks.IsMDIFormWithMaximizedMDIChild_Result(
  const Callback: TIsMDIFormWithMaximizedMDIChildResultReference
);
begin
  if Assigned(Callback) then
    IsMDIFormWithMaximizedMDIChildResultReference := Callback
  else
  begin
    IsMDIFormWithMaximizedMDIChildResultReference :=
      function(const WinControl: TWinControl): Boolean
      begin
        Result := False;
      end;
  end;
end;

var
  IsMaximizedMDIClientResultReference: TUtilsFormsMocks.TIsMaximizedMDIClientResultReference = nil;

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
  Result := IsMaximizedMDIClientResultReference(ClassName);
end;

procedure TUtilsFormsMocks.IsMaximizedMDIClient_Result(const IsMaximizedMDIClientResult: Boolean);
begin
  IsMaximizedMDIClient_Result(
    function(const ClassName: string): Boolean
    begin
      Result := IsMaximizedMDIClientResult;
    end
  );
end;

procedure TUtilsFormsMocks.IsMaximizedMDIClient_Result(const Callback: TIsMaximizedMDIClientResultReference);
begin
  if Assigned(Callback) then
    IsMaximizedMDIClientResultReference := Callback
  else
  begin
    IsMaximizedMDIClientResultReference :=
      function(const ClassName: string): Boolean
      begin
        Result := False;
      end;
  end;
end;

var
  IsMaximizedMDIChildResultReference: TUtilsFormsMocks.TIsMaximizedMDIChildResultReference = nil;

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
  Result := IsMaximizedMDIChildResultReference(WinControl);
end;

procedure TUtilsFormsMocks.IsMaximizedMDIChild_Result(const IsMaximizedMDIChildResult: Boolean);
begin
  IsMaximizedMDIChild_Result(
    function(const WinControl: TWinControl): Boolean
    begin
      Result := IsMaximizedMDIChildResult;
    end
  );
end;

procedure TUtilsFormsMocks.IsMaximizedMDIChild_Result(const Callback: TIsMaximizedMDIChildResultReference);
begin
  if Assigned(Callback) then
    IsMaximizedMDIChildResultReference := Callback
  else
  begin
    IsMaximizedMDIChildResultReference :=
      function(const WinControl: TWinControl): Boolean
      begin
        Result := False;
      end;
  end;
end;

var
  GetClientSizeResultReference: TUtilsFormsMocks.TGetClientSizeResultReference = nil;

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
var
  ClientSize: TSize;

begin
  GetClientSizeResultReference(Window, WinControl, IsMaximizedMDIChild, ClientSize, ClientOrg);
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
  GetClientSizeResultReference(Wnd, WinControl, IsMaximizedMDIChild, ClientSize, ClientOrgPoint);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TUtilsFormsMocks.GetClientSize_Result(const ClientSizeResult: TSize; const ClientOrgPointResult: TPoint);
begin
  GetClientSize_Result(
    procedure(
      const Wnd: HWND;
      const WinControl: TWinControl;
      const IsMaximizedMDIChild: Boolean;
      out ClientSize: TSize;
      out ClientOrgPoint: TPoint
    )
    begin
      ClientSize     := ClientSizeResult;
      ClientOrgPoint := ClientOrgPointResult;
    end
  );
end;

procedure TUtilsFormsMocks.GetClientSize_Result(const Callback: TGetClientSizeResultReference);
begin
  if Assigned(Callback) then
    GetClientSizeResultReference := Callback
  else
  begin
    GetClientSizeResultReference :=
      procedure(
        const Wnd: HWND;
        const WinControl: TWinControl;
        const IsMaximizedMDIChild: Boolean;
        out ClientSize: TSize;
        out ClientOrgPoint: TPoint
      )
      begin
      end;
  end;
end;

{ TUtilsFormsMocks }

constructor TUtilsFormsMocks.Create;
begin
  inherited Create;

{$IFNDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<THasMainFormMaximizedMDIChild>(HasMainFormMaximizedMDIChild, HasMainFormMaximizedMDIChildMock);
  SetResult_HasMainFormMaximizedMDIChild(False);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TIsMDIFormWithMaximizedMDIChild>(GetMDIFormWithMaximizedMDIChild, IsMDIFormWithMaximizedMDIChildMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TIsMDIFormWithMaximizedMDIChild>(IsMDIFormWithMaximizedMDIChild, IsMDIFormWithMaximizedMDIChildMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  IsMDIFormWithMaximizedMDIChild_Result(nil);

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TIsMaximizedMDIClient>(GetMaximizedMDIClient, IsMaximizedMDIClientMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TIsMaximizedMDIClient>(IsMaximizedMDIClient, IsMaximizedMDIClientMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  IsMaximizedMDIClient_Result(nil);

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TIsMaximizedMDIChild>(GetMaximizedMDIChild, IsMaximizedMDIChildMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TIsMaximizedMDIChild>(IsMaximizedMDIChild, IsMaximizedMDIChildMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  IsMaximizedMDIChild_Result(nil);

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TGetClientSize>(GetClientSize, GetClientSizeMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetClientSize>(GetClientSize, GetClientSizeMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  GetClientSize_Result(nil);
end;

end.
