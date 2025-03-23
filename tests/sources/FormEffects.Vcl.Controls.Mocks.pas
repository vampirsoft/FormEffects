/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.Controls.Mocks.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.Controls.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
  System.Generics.Collections,
  Vcl.Controls,
  FormEffects.Utils.Mocks,
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Graphics.Mocks;

type

  TWinControl = class;

{ TControl }

  TControl = class abstract(TComponent)
  strict private
    FVisible: Boolean;
    FClientWidth: Integer;
    FClientHeight: Integer;
    FPerformResult: LRESULT;
    FClientOrigin: TPoint;
    FBoundsRect: TRect;
    FColor: TColor;
    FParent: TWinControl;

  public
    function GetControlStyle: TControlStyle; virtual;
    function GetLeft: Integer; virtual; abstract;
    function GetTop: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetFocused: Boolean; virtual; abstract;
    function GetControlState: TControlState; virtual; abstract;
    function GetClientRect: TRect; virtual; abstract;
    procedure SetLeft(const Value: Integer); virtual; abstract;
    procedure SetTop(const Value: Integer); virtual; abstract;
    procedure SetFocused(const Value: Boolean); virtual; abstract;
    procedure SetControlState(const Value: TControlState); virtual; abstract;

  public
    procedure Invalidate; virtual; abstract;
    procedure SetBounds(Left, Top, Width, Height: Integer); virtual; abstract;
    function Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT; overload;
    procedure SetResult_Perform(const Result: LRESULT);
    function ScreenToClient(const Point: TPoint): TPoint;
    function ClientToScreen(const Point: TPoint): TPoint;
    function GetPalette: HPALETTE; dynamic;

  public
    property Focused: Boolean read GetFocused write SetFocused;
    property Parent: TWinControl read FParent write FParent;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property ClientWidth: Integer read FClientWidth write FClientWidth;
    property ClientHeight: Integer read FClientHeight write FClientHeight;
    property Visible: Boolean read FVisible write FVisible;
    property ClientOrigin: TPoint read FClientOrigin write FClientOrigin;
    property ClientRect: TRect read GetClientRect;
    property BoundsRect: TRect read FBoundsRect write FBoundsRect;
    property ControlStyle: TControlStyle read GetControlStyle;
    property ControlState: TControlState read GetControlState write SetControlState;
    property Color: TColor read FColor write FColor;
  end;

  TControlClass = class of TControl;

{ TWinControl }

  TWinControl = class abstract(TControl)
  strict private
    FParentWindow: HWND;
    FControlCount: Integer;
    FControls: TList<TControl>;

  public
    function GetHandle: HWND; virtual;
    function GetBevelKind: TBevelKind; virtual;
    function GetBevelEdges: TBevelEdges; virtual;
    function GetBevelInner: TBevelCut; virtual;
    function GetBevelOuter: TBevelCut; virtual;
    function GetCtl3D: Boolean; virtual;
    function GetBevelWidth: TBevelWidth; virtual; abstract;
    function GetBorderWidth: TBorderWidth; virtual;
    function GetBrush: TBrush; virtual; abstract;
    function GetDoubleBuffered: Boolean; virtual; abstract;
    procedure SetHandle(const Value: HWND); virtual; abstract;
    procedure SetDoubleBuffered(const Value: Boolean); virtual; abstract;

  strict private
    function GetControl(Index: Integer): TControl;
    procedure SetControl(Index: Integer; const Value: TControl);

  protected
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;

  protected
    property BevelEdges: TBevelEdges read GetBevelEdges;
    property BevelInner: TBevelCut read GetBevelInner;
    property BevelOuter: TBevelCut read GetBevelOuter;
    property BorderWidth: TBorderWidth read GetBorderWidth;
    property BevelKind: TBevelKind read GetBevelKind;
    property Ctl3D: Boolean read GetCtl3D;

  public
    constructor Create(Owner: TComponent); overload; override;
    destructor Destroy; override;

  public
    function HandleAllocated: Boolean;
    procedure SetBounds(Left, Top, Width, Height: Integer); override;
    procedure HandleNeeded; virtual; abstract;
    procedure CreateParams(var Params: TCreateParams); virtual;

  public
    property Handle: HWND read GetHandle write SetHandle;
    property WindowHandle: HWND read GetHandle write SetHandle;
    property ParentWindow: HWND read FParentWindow write FParentWindow;
    property BevelWidth: TBevelWidth read GetBevelWidth;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered;
    property Controls[Index: Integer]: TControl read GetControl write SetControl;
    property ControlCount: Integer read FControlCount;
    property Brush: TBrush read GetBrush;
  end;

{ TCustomControl }

  TCustomControl = class(TWinControl)
  end;

{ THintWindow }

  THintWindow = class(TWinControl)
  protected
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
  end;

{ TCustomListControl }

  TCustomListControl = class(TWinControl)
  end;

{ TCustomMultiSelectListControl }

  TCustomMultiSelectListControl = class(TCustomListControl)
  end;

{ TCustomListBox }

  TCustomListBox = class(TCustomMultiSelectListControl)
  end;

function FindControl(Handle: HWND): TWinControl;

type
  TVclControlsMocks = class abstract(TMocksManager)
  public
    function FindControl(const Handle: HWND): TWinControl; virtual; abstract;

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils;

{ TControl }

function TControl.ClientToScreen(const Point: TPoint): TPoint;
begin
end;

function TControl.GetControlStyle: TControlStyle;
begin
  Result := [];
end;

function TControl.GetPalette: HPALETTE;
begin
  Result := 0;
end;

function TControl.Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;
begin
  Result := FPerformResult;
end;

function TControl.ScreenToClient(const Point: TPoint): TPoint;
begin
end;

procedure TControl.SetResult_Perform(const Result: LRESULT);
begin
  FPerformResult := Result;
end;

{ TWinControl }

constructor TWinControl.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FControls    := TList<TControl>.Create;
end;

procedure TWinControl.CreateParams(var Params: TCreateParams);
begin
end;

destructor TWinControl.Destroy;
begin
  FreeAndNil(FControls);

  inherited;
end;

function TWinControl.GetBevelEdges: TBevelEdges;
begin
  Result := [];
end;

function TWinControl.GetBevelInner: TBevelCut;
begin
  Result := bvNone;
end;

function TWinControl.GetBevelKind: TBevelKind;
begin
  Result := bkNone;
end;

function TWinControl.GetBevelOuter: TBevelCut;
begin
  Result := bvNone;
end;

function TWinControl.GetBorderWidth: TBorderWidth;
begin
  Result := 0;
end;

function TWinControl.GetControl(Index: Integer): TControl;
begin
  Result := FControls[Index];
end;

function TWinControl.GetCtl3D: Boolean;
begin
  Result := True;
end;

function TWinControl.GetHandle: HWND;
begin
  Result := 0;
end;

function TWinControl.HandleAllocated: Boolean;
begin
  Result := WindowHandle <> 0;
end;

procedure TWinControl.SetBounds(Left, Top, Width, Height: Integer);
begin
  inherited SetBounds(Left, Top, Width, Height);
end;

procedure TWinControl.SetControl(Index: Integer; const Value: TControl);
begin
  FControls.Insert(Index, Value);
end;

procedure TWinControl.WMNCPaint(var Message: TWMNCPaint);
begin
end;

{ THintWindow }

procedure THintWindow.WMPrint(var Message: TMessage);
begin
end;

var
  VclControlsMocks: TVclControlsMocks;

function FindControl(Handle: HWND): TWinControl;
begin
  if VclControlsMocks = nil then
    Exit(nil);

  Result := VclControlsMocks.FindControl(Handle);
end;

{ TVclControlsMocks }

constructor TVclControlsMocks.Create;
begin
  inherited Create;

  VclControlsMocks := Self;
end;

destructor TVclControlsMocks.Destroy;
begin
  VclControlsMocks := nil;

  inherited Destroy;
end;

end.
