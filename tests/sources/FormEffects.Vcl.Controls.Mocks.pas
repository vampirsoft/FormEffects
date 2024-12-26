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
  Winapi.Windows, Winapi.Messages,
  System.UITypes, System.Generics.Collections,
  Vcl.Controls,
  FormEffects.System.Classes.Mocks, FormEffects.Vcl.Graphics.Mocks;

type

  TWinControl = class;

{ TControl }

  TControl = class abstract(TComponent)
  strict private
    FVisible: Boolean;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FClientWidth: Integer;
    FClientHeight: Integer;
    FPerformResult: LRESULT;
    FClientOrigin: TPoint;
    FClientRect: TRect;
    FBoundsRect: TRect;
    FControlStyle: TControlStyle;
    FControlState: TControlState;
    FColor: TColor;
    FParent: TWinControl;

  public
    function GetFocused: Boolean; virtual; abstract;
    procedure SetFocused(const Value: Boolean); virtual; abstract;

  public
    procedure Invalidate;
    procedure SetBounds(Left, Top, Width, Height: Integer); virtual;
    function Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT; overload;
    procedure SetResult_Perform(const Result: LRESULT);
    function ScreenToClient(const Point: TPoint): TPoint;
    function ClientToScreen(const Point: TPoint): TPoint;
    function GetPalette: HPALETTE; dynamic;

  public
    property Focused: Boolean read GetFocused write SetFocused;

  public
    property Parent: TWinControl read FParent write FParent;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property ClientWidth: Integer read FClientWidth write FClientWidth;
    property ClientHeight: Integer read FClientHeight write FClientHeight;
    property Visible: Boolean read FVisible write FVisible;
    property ClientOrigin: TPoint read FClientOrigin write FClientOrigin;
    property ClientRect: TRect read FClientRect write FClientRect;
    property BoundsRect: TRect read FBoundsRect write FBoundsRect;
    property ControlStyle: TControlStyle read FControlStyle write FControlStyle;
    property ControlState: TControlState read FControlState write FControlState;
    property Color: TColor read FColor write FColor;
  end;

  TControlClass = class of TControl;

{ TWinControl }

  TWinControl = class abstract(TControl)
  strict private
    FCtl3D: Boolean;
    FDoubleBuffered: Boolean;
    FParentWindow: HWND;
    FControlCount: Integer;
    FBorderWidth: TBorderWidth;
    FBevelInner: TBevelCut;
    FBevelOuter: TBevelCut;
    FBevelKind: TBevelKind;
    FBevelWidth: TBevelWidth;
    FBevelEdges: TBevelEdges;
    FControls: TList<TControl>;
    FBrush: TBrush;

  public
    function GetHandle: HWND; virtual;
    procedure SetHandle(const Value: HWND); virtual; abstract;

  strict private
    function GetControl(Index: Integer): TControl;
    procedure SetControl(Index: Integer; const Value: TControl);

  protected
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;

  protected
    property BevelEdges: TBevelEdges read FBevelEdges write FBevelEdges;
    property BevelInner: TBevelCut index 0 read FBevelInner write FBevelInner;
    property BevelOuter: TBevelCut index 1 read FBevelOuter write FBevelOuter;
    property BorderWidth: TBorderWidth read FBorderWidth write FBorderWidth;
    property BevelKind: TBevelKind read FBevelKind write FBevelKind;
    property Ctl3D: Boolean read FCtl3D write FCtl3D;

  public
    constructor Create(Owner: TComponent); overload; override;
    destructor Destroy; override;

  public
    function HandleAllocated: Boolean;
    procedure SetBounds(Left, Top, Width, Height: Integer); override;
    procedure HandleNeeded;
    procedure CreateParams(var Params: TCreateParams); virtual;

  public
    property Handle: HWND read GetHandle write SetHandle;
    property WindowHandle: HWND read GetHandle write SetHandle;
    property ParentWindow: HWND read FParentWindow write FParentWindow;
    property BevelWidth: TBevelWidth read FBevelWidth write FBevelWidth;
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    property Controls[Index: Integer]: TControl read GetControl write SetControl;
    property ControlCount: Integer read FControlCount;
    property Brush: TBrush read FBrush;
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
procedure SetResult_FindControl(const WinControl: TWinControl);

implementation

uses
  System.SysUtils;

var
  FindControlResult: TWinControl;

function FindControl(Handle: HWND): TWinControl;
begin
  Result := FindControlResult;
end;

procedure SetResult_FindControl(const WinControl: TWinControl);
begin
  FindControlResult := WinControl;
end;

{ TControl }

function TControl.ClientToScreen(const Point: TPoint): TPoint;
begin

end;

function TControl.GetPalette: HPALETTE;
begin
  Result := 0;
end;

procedure TControl.Invalidate;
begin

end;

function TControl.Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;
begin
  Result := FPerformResult;
end;

function TControl.ScreenToClient(const Point: TPoint): TPoint;
begin

end;

procedure TControl.SetBounds(Left, Top, Width, Height: Integer);
begin
  FLeft   := Left;
  FTop    := Top;
  FWidth  := Width;
  FHeight := Height;
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
  FBrush       := TBrush.Create;
  FBevelWidth  := 1;
  FBorderWidth := 0;

  FBevelEdges  := [beLeft, beTop, beRight, beBottom];
end;

procedure TWinControl.CreateParams(var Params: TCreateParams);
begin

end;

destructor TWinControl.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FControls);

  inherited;
end;

function TWinControl.GetControl(Index: Integer): TControl;
begin
  Result := FControls[Index];
end;

function TWinControl.GetHandle: HWND;
begin
  Result := 0;
end;

function TWinControl.HandleAllocated: Boolean;
begin
  Result := WindowHandle <> 0;
end;

procedure TWinControl.HandleNeeded;
begin

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

end.
