/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Windows.Mocks.pas                        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Windows.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  FormEffects.Utils.Mocks;

type

{ TUtilsWindowsMocks }

  TUtilsWindowsMocks = class(TMocksManager)
  private type
    TGetWindowOffsetResultReference = reference to function(const Wnd: HWND): TPoint;
  public
    procedure GetWindowOffset_Result(const GetWindowOffsetResult: TPoint); overload;
    procedure GetWindowOffset_Result(const Callback: TGetWindowOffsetResultReference); overload;

  private type
    THasWindowRegionResultReference = reference to function(const Wnd: HWND): Boolean;
  public
    procedure HasWindowRegion_Result(const HasWindowRegionResult: Boolean); overload;
    procedure HasWindowRegion_Result(const Callback: THasWindowRegionResultReference); overload;

  private type
    TGetWindowSizeResultReference = reference to function(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize;
  public
    procedure GetWindowSize_Result(const GetWindowSizeResult: TSize); overload;
    procedure GetWindowSize_Result(const Callback: TGetWindowSizeResultReference); overload;

  public
    constructor Create; override;
  end;

implementation

uses
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Windows
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  GetWindowOffsetResultReference: TUtilsWindowsMocks.TGetWindowOffsetResultReference = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TGetWindowOffset = function(Window: HWND): TPoint;

function GetWindowOffsetMock(Window: HWND): TPoint;
begin
  Result := GetWindowOffsetResultReference(Window);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TGetWindowOffset = function(const Wnd: HWND): TPoint;

function GetWindowOffsetMock(const Wnd: HWND): TPoint;
begin
  Result := GetWindowOffsetResultReference(Wnd);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TUtilsWindowsMocks.GetWindowOffset_Result(const GetWindowOffsetResult: TPoint);
begin
  GetWindowOffset_Result(
    function(const Wnd: HWND): TPoint
    begin
      Result := GetWindowOffsetResult;
    end
  );
end;

procedure TUtilsWindowsMocks.GetWindowOffset_Result(const Callback: TGetWindowOffsetResultReference);
begin
  if Assigned(Callback) then
    GetWindowOffsetResultReference := Callback
  else
  begin
    GetWindowOffsetResultReference :=
      function(const Wnd: HWND): TPoint
      begin
        Result := TPoint.Zero;
      end;
  end;
end;

var
  HasWindowRegionResultReference: TUtilsWindowsMocks.THasWindowRegionResultReference = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  THasWindowRegion = function(Window: HWnd): Boolean;

function HasWindowRegionMock(Window: HWND): Boolean;
begin
   Result := HasWindowRegionResultReference(Window);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  THasWindowRegion = function(const Wnd: HWND): Boolean;

function HasWindowRegionMock(const Wnd: HWND): Boolean;
begin
   Result := HasWindowRegionResultReference(Wnd);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TUtilsWindowsMocks.HasWindowRegion_Result(const HasWindowRegionResult: Boolean);
begin
  HasWindowRegion_Result(
    function(const Wnd: HWND): Boolean
    begin
      Result := HasWindowRegionResult;
    end
  );
end;

procedure TUtilsWindowsMocks.HasWindowRegion_Result(const Callback: THasWindowRegionResultReference);
begin
  if Assigned(Callback) then
    HasWindowRegionResultReference := Callback
  else
  begin
    HasWindowRegionResultReference :=
      function(const Wnd: HWND): Boolean
      begin
        Result := False;
      end;
  end;
end;

var
  GetWindowSizeResultReference: TUtilsWindowsMocks.TGetWindowSizeResultReference = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TGetWindowSize = procedure(Window: HWnd; IsMaximizedMDIChild: Boolean; var Width, Height: Integer);

procedure GetWindowSizeMock(Window: HWnd; IsMaximizedMDIChild: Boolean; var Width, Height: Integer);
begin
  const Result = GetWindowSizeResultReference(Window, IsMaximizedMDIChild);
  Width  := Result.Width;
  Height := Result.Height;
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TGetWindowSize = function(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize;

function GetWindowSizeMock(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize;
begin
  Result := GetWindowSizeResultReference(Wnd, IsMaximizedMDIChild);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TUtilsWindowsMocks.GetWindowSize_Result(const GetWindowSizeResult: TSize);
begin
  GetWindowSize_Result(
    function(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize
    begin
      Result := GetWindowSizeResult;
    end
  );
end;

procedure TUtilsWindowsMocks.GetWindowSize_Result(const Callback: TGetWindowSizeResultReference);
begin
  if Assigned(Callback) then
    GetWindowSizeResultReference := Callback
  else
  begin
    GetWindowSizeResultReference :=
      function(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize
      begin
        Result := Size.Zero;
      end;
  end;
end;

{ TUtilsWindowsMocks }

constructor TUtilsWindowsMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetWindowOffset>(WindowClientOffset, GetWindowOffsetMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetWindowOffset>(GetWindowOffset, GetWindowOffsetMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  GetWindowOffset_Result(nil);

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<THasWindowRegion>(WindowHasRegion, HasWindowRegionMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<THasWindowRegion>(HasWindowRegion, HasWindowRegionMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  HasWindowRegion_Result(nil);

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TGetWindowSize>(GetSize, GetWindowSizeMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetWindowSize>(GetWindowSize, GetWindowSizeMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  GetWindowSize_Result(nil);
end;

end.
