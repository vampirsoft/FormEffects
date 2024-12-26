/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.Controls.Mocks.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.Controls.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Menus,
  FormEffects.Utils.Mocks,
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Graphics.Mocks;

type

  TControl = class;
  TWinControl = class;

{ TControlCanvas }

  TControlCanvas = class abstract(TCanvas)
  strict private
    FControl: TControl;

  public
    procedure UpdateTextFlags; virtual;

  public
    property Control: TControl read FControl write FControl;
  end;

{ TControl }

  TControl = class abstract(TComponent)
  strict private
    FAlignWithMargins: Boolean;
    FAction: TBasicAction;
    FAlign: TAlign;
    FAnchors: TAnchors;
    FBiDiMode: TBiDiMode;
    FConstraints: TSizeConstraints;
    FColor: TColor;
    FDragKind: TDragKind;
    FDragCursor: TCursor;
    FDragMode: TDragMode;
    FEnabled: Boolean;
    FFont: TFont;
    FParentBiDiMode: Boolean;
    FParentColor: Boolean;
    FParentFont: Boolean;
    FParentShowHint: Boolean;
    FPopupMenu: TPopupMenu;
    FShowHint: Boolean;
    FTouchManager: TTouchManager;
    FText: TCaption;
    FMargins: TMargins;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnEndDock: TEndDragEvent;
    FOnEndDrag: TEndDragEvent;
    FOnStartDock: TStartDockEvent;
    FOnStartDrag: TStartDragEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnCanResize: TCanResizeEvent;
    FOnGesture: TGestureEvent;
    FOnConstrainedResize: TConstrainedResizeEvent;
    FOnContextPopup: TContextPopupEvent;
    FOnResize: TNotifyEvent;

  protected
    property Caption: TCaption read FText write FText;
    property Color: TColor read FColor write FColor;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property DragKind: TDragKind read FDragKind write FDragKind default dkDrag;
    property DragCursor: TCursor read FDragCursor write FDragCursor default crDrag;
    property DragMode: TDragMode read FDragMode write FDragMode default dmManual;
    property Font: TFont read FFont write FFont;
    property ParentBiDiMode: Boolean read FParentBiDiMode write FParentBiDiMode default True;
    property ParentColor: Boolean read FParentColor write FParentColor default True;
    property ParentFont: Boolean read FParentFont write FParentFont default True;
    property ParentShowHint: Boolean read FParentShowHint write FParentShowHint default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnEndDock: TEndDragEvent read FOnEndDock write FOnEndDock;
    property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnStartDock: TStartDockEvent read FOnStartDock write FOnStartDock;
    property OnStartDrag: TStartDragEvent read FOnStartDrag write FOnStartDrag;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnConstrainedResize: TConstrainedResizeEvent read FOnConstrainedResize write FOnConstrainedResize;
    property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;

  public
    function GetControlStyle: TControlStyle; virtual;
    function GetLeft: Integer; virtual; abstract;
    function GetTop: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetControlState: TControlState; virtual; abstract;
    function GetClientRect: TRect; virtual;
    function GetBoundsRect: TRect; virtual;
    function GetClientWidth: Integer; virtual;
    function GetClientHeight: Integer; virtual;
    function GetParent: TWinControl; virtual;
    function GetVisible: Boolean; virtual; abstract;
    function GetClientOrigin: TPoint; virtual; abstract;
    function DrawTextBiDiModeFlags(Flags: Longint): Longint; virtual; abstract;
    procedure SetParent(AParent: TWinControl); virtual; abstract;
    procedure SetLeft(const Value: Integer); virtual;
    procedure SetTop(const Value: Integer); virtual;
    procedure SetBoundsRect(const Value: TRect); virtual;
    procedure SetControlState(const Value: TControlState); virtual; abstract;
    procedure SetControlStyle(const Value: TControlStyle); virtual; abstract;
    procedure SetWidth(const Value: Integer); virtual; abstract;
    procedure SetHeight(const Value: Integer); virtual; abstract;
    procedure SetVisible(const Value: Boolean); virtual;

  public
    function Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT; overload; virtual;
    function ScreenToClient(const Point: TPoint): TPoint;
    function ClientToScreen(const Point: TPoint): TPoint; overload;
    function ClientToScreen(const Rect: TRect): TRect; overload;
    function GetPalette: HPALETTE; dynamic;
    procedure Invalidate; virtual;
    procedure SetBounds(Left, Top, Width, Height: Integer); virtual;
    procedure BringToFront; virtual; abstract;

  public
    property Action: TBasicAction read FAction write FAction;
    property Align: TAlign read FAlign write FAlign default alNone;
    property Anchors: TAnchors read FAnchors write FAnchors default [akLeft, akTop];
    property BiDiMode: TBiDiMode read FBiDiMode write FBiDiMode;
    property Constraints: TSizeConstraints read FConstraints write FConstraints;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property ShowHint: Boolean read FShowHint write FShowHint;
    property Parent: TWinControl read GetParent write SetParent;
    property ClientWidth: Integer read GetClientWidth;
    property ClientHeight: Integer read GetClientHeight;
    property Visible: Boolean read GetVisible write SetVisible;
    property ClientOrigin: TPoint read GetClientOrigin;
    property ClientRect: TRect read GetClientRect;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property ControlStyle: TControlStyle read GetControlStyle write SetControlStyle;
    property ControlState: TControlState read GetControlState write SetControlState;
    property Touch: TTouchManager read FTouchManager write FTouchManager;
    property OnGesture: TGestureEvent read FOnGesture write FOnGesture;

  published
    property AlignWithMargins: Boolean read FAlignWithMargins write FAlignWithMargins default False;
    property Margins: TMargins read FMargins write FMargins;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

  TControlClass = class of TControl;

{ TWinControl }

  TWinControl = class abstract(TControl)
  strict private
    FParentWindow: HWND;
    FControls: TArray<TControl>;
    FParentCtl3D: Boolean;
    FParentBackground: Boolean;
    FPadding: TPadding;
    FUseDockManager: Boolean;
    FDockSite: Boolean;
    FPixelsPerInch: Integer;
    FTabOrder: TTabOrder;
    FTabStop: Boolean;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnDockDrop: TDockDropEvent;
    FOnDockOver: TDockOverEvent;
    FOnGetSiteInfo: TGetSiteInfoEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnUnDock: TUnDockEvent;

  public
    function GetControls: TArray<TControl>; virtual;
    function GetHandle: HWND; virtual;
    function GetParentWindow: HWND; virtual;
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
    function GetControlCount: Integer;
    function GetControl(Index: Integer): TControl;

  protected
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure CreateParams(var Params: TCreateParams); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); dynamic; abstract;
    procedure AlignControls(AControl: TControl; var Rect: TRect); virtual; abstract;
    procedure NotifyControls(Msg: Word); virtual; abstract;
    procedure PaintHandler(var Message: TWMPaint); virtual; abstract;
    procedure PaintWindow(DC: HDC); virtual; abstract;

  protected
    property BevelEdges: TBevelEdges read GetBevelEdges;
    property BevelInner: TBevelCut read GetBevelInner;
    property BevelOuter: TBevelCut read GetBevelOuter;
    property BevelKind: TBevelKind read GetBevelKind;
    property BevelWidth: TBevelWidth read GetBevelWidth;
    property BorderWidth: TBorderWidth read GetBorderWidth;
    property Ctl3D: Boolean read GetCtl3D;
    property ParentCtl3D: Boolean read FParentCtl3D write FParentCtl3D default True;
    property ParentBackground: Boolean read FParentBackground write FParentBackground;
    property WindowHandle: HWND read GetHandle write SetHandle;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnDockDrop: TDockDropEvent read FOnDockDrop write FOnDockDrop;
    property OnDockOver: TDockOverEvent read FOnDockOver write FOnDockOver;
    property OnGetSiteInfo: TGetSiteInfoEvent read FOnGetSiteInfo write FOnGetSiteInfo;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnUnDock: TUnDockEvent read FOnUnDock write FOnUnDock;

  public
    constructor Create(Owner: TComponent); override;
    constructor CreateParented(ParentWindow: HWND);

  public
    function HandleAllocated: Boolean; virtual;
    function Focused: Boolean; virtual; abstract;
    function CanFocus: Boolean; dynamic; abstract;
    procedure HandleNeeded; virtual; abstract;
    procedure Realign; virtual; abstract;
    procedure RecreateWnd; virtual; abstract;
    procedure InsertControl(AControl: TControl); virtual; abstract;
    procedure SetFocus; virtual; abstract;

  public
    property Handle: HWND read GetHandle write SetHandle;
    property ParentWindow: HWND read GetParentWindow;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered;
    property Controls[Index: Integer]: TControl read GetControl;
    property ControlCount: Integer read GetControlCount;
    property Brush: TBrush read GetBrush;
    property Padding: TPadding read FPadding write FPadding;
    property UseDockManager: Boolean read FUseDockManager write FUseDockManager default False;
    property DockSite: Boolean read FDockSite write FDockSite default False;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
    property TabOrder: TTabOrder read FTabOrder write FTabOrder default -1;
    property TabStop: Boolean read FTabStop write FTabStop default False;
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
  System.SysUtils,
  FormEffects.TypeHelpers;

{ TControlCanvas }

procedure TControlCanvas.UpdateTextFlags;
begin
end;

{ TControl }

function TControl.ClientToScreen(const Point: TPoint): TPoint;
begin
  const Origin = ClientOrigin;
  Result.X    := Point.X + Origin.X;
  Result.Y    := Point.Y + Origin.Y;
end;

function TControl.ClientToScreen(const Rect: TRect): TRect;
begin
  Result := Rect;
  const Origin = ClientOrigin;
  OffsetRect(Result, Origin.X, Origin.Y);
end;

function TControl.GetBoundsRect: TRect;
begin
  Result := TRect.Zero;
end;

function TControl.GetClientHeight: Integer;
begin
  Result := ClientRect.Bottom;
end;

function TControl.GetClientRect: TRect;
begin
  Result := TRect.Zero;
end;

function TControl.GetClientWidth: Integer;
begin
  Result := ClientRect.Right;
end;

function TControl.GetControlStyle: TControlStyle;
begin
  Result := [];
end;

function TControl.GetPalette: HPALETTE;
begin
  Result := 0;
end;

function TControl.GetParent: TWinControl;
begin
  Result := nil;
end;

procedure TControl.Invalidate;
begin
end;

function TControl.Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;
begin
  Result := 0;
end;

function TControl.ScreenToClient(const Point: TPoint): TPoint;
begin
end;

procedure TControl.SetBounds(Left, Top, Width, Height: Integer);
begin
end;

procedure TControl.SetBoundsRect(const Value: TRect);
begin
  with Value do
    SetBounds(Left, Top, Right - Left, Bottom - Top);
end;

procedure TControl.SetLeft(const Value: Integer);
begin
end;

procedure TControl.SetTop(const Value: Integer);
begin
end;

procedure TControl.SetVisible(const Value: Boolean);
begin
end;

{ TWinControl }

constructor TWinControl.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FControls := [];
end;

procedure TWinControl.CreateParams(var Params: TCreateParams);
begin
end;

constructor TWinControl.CreateParented(ParentWindow: HWND);
begin
  FParentWindow := ParentWindow;
  Create(nil);
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
  Result := GetControls[Index];
end;

function TWinControl.GetControlCount: Integer;
begin
  Result := Length(GetControls);
end;

function TWinControl.GetControls: TArray<TControl>;
begin
  Result := FControls;
end;

function TWinControl.GetCtl3D: Boolean;
begin
  Result := True;
end;

function TWinControl.GetHandle: HWND;
begin
  Result := 0;
end;

function TWinControl.GetParentWindow: HWND;
begin
  Result := FParentWindow;
end;

function TWinControl.HandleAllocated: Boolean;
begin
  Result := WindowHandle <> 0;
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
