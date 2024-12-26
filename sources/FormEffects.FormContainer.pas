/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.FormContainer.pas                              *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.FormContainer;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.SysConst,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Themes,
{$IFDEF FORM_EFFECTS_TESTS}
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks,
  FormEffects.Vcl.Themes.Mocks,
{$ENDIF ~ FORM_EFFECTS_TESTS}
  FormEffects.Constants,
  FormEffects.Backgrounds;

type

  TFEEmbeddedFormHistory = class;

//  EFEFormContainerException = class(Exception);

{ TFEFormContainer }

  TFEFormContainer = class(TScrollingWinControl, IWithBackgroundOptions)
  public type

  { TEmbeddedForm }

    TEmbeddedForm = class(TCustomForm, IWithBackgroundOptions)
    strict private const
      DefaultAlign = TEmbeddedFormAlign.Center;

    strict private
      FAlign: TEmbeddedFormAlign;
      FBackgroundOptions: TBackgroundOptions;

    strict private
      procedure SetBackgroundOptions(const Value: TBackgroundOptions); inline;

    strict private
      procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
      procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
      procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
      procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    strict protected
      function  GetPalette: HPALETTE; override;
      procedure CreateParams(var Params: TCreateParams); override;
      procedure Paint; override;
      procedure SetParent(AParent: TWinControl); override;

    public
      constructor Create(Owner: TComponent); override;
      destructor Destroy; override;

    public
      function GetBackgroundOptions: TBackgroundOptions; // Implementation for IWithBackgroundOptions
      procedure ShowEmbedded; inline;

    published
      property Align: TEmbeddedFormAlign read FAlign write FAlign default DefaultAlign;
      property BackgroundOptions: TBackgroundOptions read GetBackgroundOptions write SetBackgroundOptions;

    published
      property Action;
      property ActiveControl;
      property BiDiMode;
      property Caption;
      property ClientHeight stored True;
      property ClientWidth stored True;
      property Color;
      property Ctl3D;
      property UseDockManager;
      property DockSite;
      property DragKind;
      property DragMode;
      property Enabled;
      property ParentFont stored True;
      property Font;
      property Height stored False;
      property HelpFile;
      property KeyPreview;
      property Menu;
      property ObjectMenuItem;
      property ParentBackground;
      property ParentBiDiMode;
      property ParentColor stored True;
      property ParentShowHint;
      property PixelsPerInch;
      property PopupMenu;
      property Scaled;
      property ShowHint;
      property Touch;
      property Width stored False;

    published
      property OnCanResize;
      property OnClick;
      property OnClose;
      property OnCloseQuery;
      property OnConstrainedResize;
      property OnContextPopup;
      property OnCreate;
      property OnDblClick;
      property OnDestroy;
      property OnDockDrop;
      property OnDockOver;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDock;
      property OnEnter;
      property OnExit;
      property OnGesture;
      property OnGetSiteInfo;
      property OnHide;
      property OnHelp;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnMouseWheel;
      property OnMouseWheelDown;
      property OnMouseWheelUp;
      property OnPaint;
      property OnResize;
      property OnShortCut;
      property OnShow;
      property OnStartDock;
      property OnUnDock;
    end;

    TEmbeddedFormClass = class of TEmbeddedForm;

  strict private type
    TFormLifeCycleEvent = procedure(const Sender: TFEFormContainer; const Form: TEmbeddedForm) of object;
    TFormChangeEvent = procedure(
      const Sender: TFEFormContainer;
      const PrevForm, NewForm: TEmbeddedForm;
      var CanChange: Boolean
    ) of object;

  strict private const
    DefaultBorderStyle = bsNone;
    DefaultAutoScroll  = False;

  strict private
    FLocked: Boolean;
    FBorderStyle: TBorderStyle;
    FCanvas: TControlCanvas;
    FForm: TEmbeddedForm;
    FHistory: TFEEmbeddedFormHistory;
    FBackgroundOptions: TBackgroundOptions;
    FOnFormCreate: TFormLifeCycleEvent;
    FOnFormChange: TFormChangeEvent;
    FOnFormDestroy: TFormLifeCycleEvent;

  strict private
    function GetPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
    procedure SetBackgroundOptions(const Value: TBackgroundOptions); inline;
    procedure SetBorderStyle(const Value: TBorderStyle); inline;
    procedure SetHistory(const Value: TFEEmbeddedFormHistory);

  strict private
    function DoFormChange(const PrevForm, NewForm: TEmbeddedForm): Boolean; inline;
    procedure DoFormCreate(const Form: TEmbeddedForm); inline;
    procedure DoFormDestroy(const Form: TEmbeddedForm); inline;
    procedure AdjustForm(const Form: TEmbeddedForm; const CheckVisible: Boolean);
    procedure Paint; inline;
    procedure Scrolled;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

  strict protected
    function  GetPalette: HPALETTE; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure PaintWindow(DC: HDC); override;
    procedure SetParent(AParent: TWinControl); override;

  public
    constructor Create(Owner: TComponent); override;
    destructor  Destroy; override;

  public
    function GetBackgroundOptions: TBackgroundOptions; // Implementation for IWithBackgroundOptions
    function GetForm: TEmbeddedForm; overload; inline;
    function GetForm<F: TEmbeddedForm>: F; overload; inline;
    function  CloseQuery: Boolean; inline;
    procedure CreateForm(const InstanceClass: TEmbeddedFormClass; out Reference: TEmbeddedForm);
    procedure ShowForm(const InstanceClass: TEmbeddedFormClass); overload; inline; // Create & Show
    procedure ShowForm(const Form: TEmbeddedForm); overload;

  published
    property BackgroundOptions: TBackgroundOptions read GetBackgroundOptions write SetBackgroundOptions;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default DefaultBorderStyle;
    property History: TFEEmbeddedFormHistory read FHistory write SetHistory;
    property  Picture: TPicture read GetPicture write SetPicture stored False;

  published
    property OnFormCreate: TFormLifeCycleEvent read FOnFormCreate write FOnFormCreate;
    property OnFormChange: TFormChangeEvent read FOnFormChange write FOnFormChange;
    property OnFormDestroy: TFormLifeCycleEvent read FOnFormDestroy write FOnFormDestroy;

  published
    property Align;
    property Anchors;
    property AutoScroll default DefaultAutoScroll;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Color nodefault;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Padding;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;

  published
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnEndDock;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TFEEmbeddedFormHistory }

  TFEEmbeddedFormHistory = class(TComponent)
  public type
    TCapacity = 1..Byte.MaxValue;

  strict private const
    DefaultCapacity = 20;

  strict private type
    TForms = TObjectQueue<TFEFormContainer.TEmbeddedForm>;

  strict private
    FCurrentFormIndex: SmallInt;
    FForms: TForms;

  strict private
    function GetCurrentForm: TFEFormContainer.TEmbeddedForm; inline;
    function GetFormsCount: Byte; inline;
    function GetCapacity: TCapacity; inline;
    procedure SetCapacity(const Value: TCapacity);


  private
    procedure AddForm(const Form: TFEFormContainer.TEmbeddedForm); inline;
    procedure ShowForm(const Form: TFEFormContainer.TEmbeddedForm); inline;

  public
    constructor Create(Owner: TComponent); override;
    destructor  Destroy; override;

  public
    function CloseQuery: Boolean;
    function HasPrevForm: Boolean; inline;
    function HasNextForm: Boolean; inline;
    procedure ShowPrevForm;
    procedure ShowNextForm;

  public
    property CurrentForm: TFEFormContainer.TEmbeddedForm read GetCurrentForm;
    property FormsCount: Byte read GetFormsCount;

  published
    property Capacity: TCapacity read GetCapacity write SetCapacity default DefaultCapacity;
  end;

implementation

uses
  FormEffects.TypeHelpers,
  FormEffects.Utils.ScrollBars,
  FormEffects.Rendering;

{ TFEFormContainer }

procedure TFEFormContainer.AdjustForm(const Form: TEmbeddedForm; const CheckVisible: Boolean);

  function CalculateFormAligment(const Form: TEmbeddedForm): TEmbeddedFormAlign; inline;
  begin
    const CurrentForm = GetForm;

    if Assigned(CurrentForm) then
      Result := CurrentForm.Align
    else
      Result := TEmbeddedFormAlign.Default;

    if Result = TEmbeddedFormAlign.Default then
      Result := Form.Align;
  end;

  function CalculateMarginRect(const Form: TEmbeddedForm): TRect; inline;
  begin
    if Form.AlignWithMargins then
      Result := TRect.InlineCreate(Form.Margins.Left, Form.Margins.Top, Form.Margins.Right, Form.Margins.Bottom)
    else
      Result := TRect.Zero;
  end;

begin
  if Assigned(Form) and (Form.Visible or not CheckVisible) then
  begin
    var Point: TPoint;
    var Size: TSize;

    const Align      = CalculateFormAligment(Form);
    const MarginRect = CalculateMarginRect(Form);

    case Align of
      TEmbeddedFormAlign.Default:
      begin
        Point := TPoint.Zero;
        Size  := TSize.Zero;
      end;
      // Itґs sizeable, so we adjust it to fit the Parent
      TEmbeddedFormAlign.Client:
      begin
        Point := MarginRect.TopLeft;
        Size  := TSize.InlineCreate(
          ClientWidth  - MarginRect.Left - MarginRect.Right,
          ClientHeight - MarginRect.Top  - MarginRect.Bottom
        );
      end;
      // It maintains its size, and positions at origin
      TEmbeddedFormAlign.TopLeft:
      begin
        Point := MarginRect.TopLeft;
        Size  := TSize.InlineCreate(Width, Height);
      end;
      // It maintains its size, and positions at center
      TEmbeddedFormAlign.Center:
      begin
        Size := TSize.InlineCreate(Width, Height);

        const ClientSize = TSize.InlineCreate(ClientWidth, ClientHeight);
        if ClientSize.Width >= Size.Width then
          // Itґs smaller horizontally, so we center it in the Parent
          Point.X := (ClientSize.Width - Size.Width) div 2
        else
          // Itґs bigger horizontally, so we position it at 0
          Point.X := 0;
        if ClientSize.Height >= Size.Width then
          // Itґs smaller vertically, so we center it in the Parent
          Point.Y := (ClientSize.Height - Size.Height) div 2
        else
          // Itґs bigger vertically, so we position it at 0
          Point.Y := 0;
      end;
      // it maintains its size, and positions at center of main form
      TEmbeddedFormAlign.MainFormCenter:
      begin
        Size := TSize.InlineCreate(Width, Height);

        const ParentForm = GetParentForm(Self);

        Point.X := (ParentForm.ClientWidth  - Size.Width ) div 2;
        Point.Y := (ParentForm.ClientHeight - Size.Height) div 2;
        Point := ParentForm.ClientToScreen(Point);
        Point := ScreenToClient(Point);
      end;

      TEmbeddedFormAlign.None:
      begin
        Point := TPoint.InlineCreate(Left, Top);
        Size := TSize.InlineCreate(Width, Height);
      end;
    end;

    if IsScrollBarVisible(Handle, Self, sbHorizontal) then
    begin
      Point.X     := Left;
      Size .Width := Width;
    end;
    if IsScrollBarVisible(Handle, Self, sbVertical) then
    begin
      Point.Y      := Top;
      Size .Height := Height;
    end;

    if (Point <> TPoint.InlineCreate(Left, Top)) or (Size <> TSize.InlineCreate(Width, Height)) then
    begin
      SetBounds(Point.X, Point.Y, Size.Width, Size.Height);
      Realign;
    end;
  end;
end;

procedure TFEFormContainer.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  AdjustForm(GetForm, True);
end;

function TFEFormContainer.CloseQuery: Boolean;
begin
  const Form = GetForm;
  Result := not Assigned(Form) or Form.CloseQuery;
end;

constructor TFEFormContainer.Create(Owner: TComponent);
begin
  FLocked := False;
  FForm   := nil;

  inherited Create(Owner);

  FCanvas         := TControlCanvas.Create;
  FCanvas.Control := Self;

  FBorderStyle := DefaultBorderStyle;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks];
  AutoScroll   := DefaultAutoScroll;
  Width        := 185;
  Height       :=  41;

  FBackgroundOptions         := TBackgroundOptions.Create;
{$MESSAGE 'Check Needed'}
//  FBackgroundOptions.Control := Self;

{$MESSAGE WARN 'Not Implemented TFEFormContainer.Create'}
end;

procedure TFEFormContainer.CreateForm(const InstanceClass: TEmbeddedFormClass; out Reference: TEmbeddedForm);
begin
  Assert(Assigned(InstanceClass), SClassIsNull);
  Assert(not FLocked, SLockedFormContainer);

  FLocked := True;
  try
    Reference := InstanceClass.CreateParented(Handle);

    if Reference.ParentFont then
    begin
      Reference.Perform(CM_PARENTFONTCHANGED, 0, 0);
      Reference.NotifyControls(CM_PARENTFONTCHANGED);
    end;

    InsertComponent(Reference);
    InsertControl(Reference);
    Reference.BringToFront;

    if Assigned(FHistory) then
      FHistory.AddForm(Reference);

    DoFormCreate(Reference);

    AdjustForm(Reference, False);
  finally
    FLocked := False;
  end;
end;

procedure TFEFormContainer.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);

begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];

    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;

    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

destructor TFEFormContainer.Destroy;
begin
{$MESSAGE WARN 'Not Implemented TFEFormContainer.Destroy'}
  FHistory := nil;

  FForm := nil;

  FreeAndNil(FBackgroundOptions);

  FreeAndNil(FCanvas);

  inherited Destroy;
end;

function TFEFormContainer.DoFormChange(const PrevForm, NewForm: TEmbeddedForm): Boolean;
begin
  Result := True;
  if Assigned(FOnFormChange) then
  try
    FOnFormChange(Self, PrevForm, NewForm, Result);
  except
    Application.HandleException(Self);
  end;
end;

procedure TFEFormContainer.DoFormCreate(const Form: TEmbeddedForm);
begin
  if Assigned(FOnFormCreate) then
  try
    FOnFormCreate(Self, Form);
  except
    Application.HandleException(Self);
  end;
end;

procedure TFEFormContainer.DoFormDestroy(const Form: TEmbeddedForm);
begin
  if Assigned(FOnFormDestroy) then
  try
    FOnFormDestroy(Self,  Form);
  except
    Application.HandleException(Self);
  end;
end;

function TFEFormContainer.GetBackgroundOptions: TBackgroundOptions;
begin
  Result := FBackgroundOptions;
end;

function TFEFormContainer.GetForm: TEmbeddedForm;
begin
  if Assigned(FHistory) then
    Result := FHistory.CurrentForm
  else
    Result := FForm;
end;

function TFEFormContainer.GetForm<F>: F;
begin
  const Form = GetForm;

  Assert((Form = nil) or (Form is F), SInvalidCast);

  Result := F(Form);
end;

function TFEFormContainer.GetPalette: HPALETTE;
begin
  FBackgroundOptions.Palette;
end;

function TFEFormContainer.GetPicture: TPicture;
begin
  Result := FBackgroundOptions.Picture;
end;

procedure TFEFormContainer.Paint;
begin
  FBackgroundOptions.DrawBackGround(FCanvas.Handle, FCanvas.ClipRect);

  if csDesigning in ComponentState then
  begin
    var Rect := ClientRect;

    FCanvas.Pen  .Style := psDash;
    FCanvas.Brush.Style := bsClear;
    FCanvas.Rectangle(0, 0, Rect.Right, Rect.Bottom);
    FCanvas.Pen  .Style := psSolid;

    const Flags = DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS;
    FCanvas.Brush.Color := clWhite;
    FCanvas.Brush.Style := bsSolid;
    DrawText(FCanvas.Handle, PChar(Name), -1, Rect, DrawTextBiDiModeFlags(Flags));
  end;
end;

procedure TFEFormContainer.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      FCanvas.UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TFEFormContainer.Scrolled;
begin
  if not (csDestroying in ComponentState) then
  begin
    if FBackgroundOptions.IsActive then
      FBackgroundOptions.ControlChange;

    if ThemeServices.ThemesEnabled and Assigned(Parent) and (csParentBackground in ControlStyle) then
      Invalidate;
  end;
end;

procedure TFEFormContainer.SetBackgroundOptions(const Value: TBackgroundOptions);
begin
  FBackgroundOptions.Assign(Value);
end;

procedure TFEFormContainer.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle = Value then
    Exit;

  FBorderStyle := Value;
  RecreateWnd;
end;

procedure TFEFormContainer.SetHistory(const Value: TFEEmbeddedFormHistory);
begin
  if FHistory = Value then
    Exit;

  FHistory := Value;
{$MESSAGE WARN 'Not Implemented TFEFormContainer.SetHistory'}
end;

procedure TFEFormContainer.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);

  Invalidate;
end;

procedure TFEFormContainer.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
{$MESSAGE 'Check Needed'}
  if Assigned(FBackgroundOptions) and not (csDestroying in ComponentState) then
    FBackgroundOptions.Control := Self;
end;

procedure TFEFormContainer.SetPicture(const Value: TPicture);
begin
  FBackgroundOptions.Parent.Assign(Value);
end;

procedure TFEFormContainer.ShowForm(const InstanceClass: TEmbeddedFormClass);
var
  Form: TEmbeddedForm;

begin
  CreateForm(InstanceClass, Form);
  Form.ShowEmbedded;
end;

procedure TFEFormContainer.ShowForm(const Form: TEmbeddedForm);
begin
  Assert(not Assigned(Form) or (Form.Parent = Self), SIncorrectFormContainer);

  const CurrentForm = GetForm;

  if not DoFormChange(CurrentForm, Form) then
    Exit;

  if Assigned(FHistory) then
  begin
    FHistory.ShowForm(Form);
    Exit;
  end;

{$MESSAGE WARN 'Not Implemented TFEFormContainer.ShowForm'}
end;

procedure TFEFormContainer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if FBackgroundOptions.IsParentPainted(Handle) then
    FBackgroundOptions.DrawBackGround(Message.DC, TRect.Zero);

  Message.Result := 1;
end;

procedure TFEFormContainer.WMHScroll(var Message: TWMHScroll);
begin
  inherited;

  Scrolled;
end;

procedure TFEFormContainer.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TFEFormContainer.WMVScroll(var Message: TWMVScroll);
begin
  inherited;

  Scrolled;
end;

procedure TFEFormContainer.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  Invalidate;

  inherited;

  if not(csDestroying in ComponentState) and FBackgroundOptions.IsActive then
    FBackgroundOptions.ControlChange;
end;

{ TFEFormContainer.TEmbeddedForm }

procedure TFEFormContainer.TEmbeddedForm.CMParentFontChanged(var Message: TMessage);
begin
  if csDesigning in ComponentState then
    inherited
  else
  begin
    if ParentFont and (Message.wParam = 0) and Assigned(Parent) then
      Font := TFEFormContainer(Parent).Font
    else
      inherited;
  end;
end;

constructor TFEFormContainer.TEmbeddedForm.Create(Owner: TComponent);

  function GenerateUniqueName(const ParentControl: TWinControl; const CurrentName: string): string; inline;
  begin
    var Index := 0;
    Result    := CurrentName;
    while Assigned(ParentControl.FindComponent(Result)) do
    begin
      Inc(Index);
      Result := Format('%s_%d', [CurrentName, Index]);
    end;
  end;

begin
  inherited Create(Owner);

  BorderStyle  := bsNone;
  ParentFont   := False;

  FAlign := DefaultAlign;

  FBackgroundOptions         := TBackgroundOptions.Create;
  FBackgroundOptions.Control := Self;

  const ParentControl = FindControl(ParentWindow);
  if Assigned(ParentControl) then
    Name := GenerateUniqueName(ParentControl, Name);
end;

procedure TFEFormContainer.TEmbeddedForm.CreateParams(var Params: TCreateParams);
begin
  BorderStyle := bsNone;

  inherited CreateParams(Params);
end;

destructor TFEFormContainer.TEmbeddedForm.Destroy;
begin
  FreeAndNil(FBackgroundOptions);

  inherited Destroy;
end;

function TFEFormContainer.TEmbeddedForm.GetBackgroundOptions: TBackgroundOptions;
begin
  Result := FBackgroundOptions;
end;

function TFEFormContainer.TEmbeddedForm.GetPalette: HPALETTE;
begin
  Result := FBackgroundOptions.Palette;
end;

procedure TFEFormContainer.TEmbeddedForm.Paint;
begin
  if FBackgroundOptions.IsActive then
  begin
    const FormCanvas = Canvas;
    FBackgroundOptions.DrawBackground(FormCanvas.Handle, FormCanvas.ClipRect);
  end;

  inherited Paint;
end;

procedure TFEFormContainer.TEmbeddedForm.SetBackgroundOptions(const Value: TBackgroundOptions);
begin
  FBackgroundOptions.Assign(Value);
end;

procedure TFEFormContainer.TEmbeddedForm.SetParent(AParent: TWinControl);
begin
  Assert(Assigned(AParent) and (AParent is TFEFormContainer), SIncorrectFormContainer);

  inherited SetParent(AParent);
{$MESSAGE 'Check Needed'}
  if Assigned(FBackgroundOptions) and not (csDestroying in ComponentState) then
    FBackgroundOptions.Control := Self;
end;

procedure TFEFormContainer.TEmbeddedForm.ShowEmbedded;
begin
  Assert(Assigned(Parent), SInstanceIsNull);

  TFEFormContainer(Parent).ShowForm(Self);
end;

procedure TFEFormContainer.TEmbeddedForm.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if FBackgroundOptions.IsActive then
  begin
    if FBackgroundOptions.IsParentPainted(Handle) then
      FBackgroundOptions.DrawBackground(Message.DC, TRect.Zero);
    Message.Result := -1;
  end
  else
    inherited;
end;

procedure TFEFormContainer.TEmbeddedForm.WMPaint(var Message: TWMPaint);
begin
  const SaveDesigner = Designer;

  if FBackgroundOptions.IsActive then
    Designer := nil;

  inherited;

  Designer := SaveDesigner;
end;

procedure TFEFormContainer.TEmbeddedForm.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;

  if not (csDestroying in ComponentState) and BackgroundOptions.IsActive then
    BackgroundOptions.ControlChange;
end;

{ TFEEmbeddedFormHistory }

procedure TFEEmbeddedFormHistory.AddForm(const Form: TFEFormContainer.TEmbeddedForm);
begin
  Assert(Assigned(Form), SInstanceIsNull);
{$MESSAGE WARN 'Not Implemented TFEEmbeddedFormHistory.AddForm'}
end;

function TFEEmbeddedFormHistory.CloseQuery: Boolean;
begin
  for var Form in FForms do
  begin
    if not Form.CloseQuery then
      Exit(False);
  end;
  Result := True;
end;

constructor TFEEmbeddedFormHistory.Create(Owner: TComponent);
begin
  FCurrentFormIndex := -1;

  inherited Create(Owner);

  FForms          := TForms.Create;
  FForms.Capacity := DefaultCapacity;

{$MESSAGE WARN 'Not Implemented TFEEmbeddedFormHistory.Create'}
end;

destructor TFEEmbeddedFormHistory.Destroy;
begin
{$MESSAGE WARN 'Not Implemented TFEEmbeddedFormHistory.Destroy'}
  FreeAndNil(FForms);

  inherited Destroy;
end;

function TFEEmbeddedFormHistory.GetCapacity: TCapacity;
begin
  Result := FForms.Capacity;
end;

function TFEEmbeddedFormHistory.GetCurrentForm: TFEFormContainer.TEmbeddedForm;
begin
  if (FCurrentFormIndex > -1) and (FCurrentFormIndex < FormsCount) then
    Result := FForms.List[FCurrentFormIndex]
  else
    Result := nil;
end;

function TFEEmbeddedFormHistory.GetFormsCount: Byte;
begin
  Result := FForms.Count;
end;

function TFEEmbeddedFormHistory.HasNextForm: Boolean;
begin
  Result := (FormsCount > 0) and (FCurrentFormIndex < FormsCount - 1);
end;

function TFEEmbeddedFormHistory.HasPrevForm: Boolean;
begin
  Result := (FormsCount > 0) and (FCurrentFormIndex > 0);
end;

procedure TFEEmbeddedFormHistory.SetCapacity(const Value: TCapacity);
begin
  if FForms.Capacity = Value then
    Exit;
{$MESSAGE WARN 'Not Implemented TFEEmbeddedFormHistory.SetCapacity'}
end;

procedure TFEEmbeddedFormHistory.ShowForm(const Form: TFEFormContainer.TEmbeddedForm);
begin
  Assert(Assigned(Form), SInstanceIsNull);
{$MESSAGE WARN 'Not Implemented TFEEmbeddedFormHistory.ShowForm'}
end;

procedure TFEEmbeddedFormHistory.ShowNextForm;
begin
  if not HasNextForm then
    Exit;
{$MESSAGE WARN 'Not Implemented TFEEmbeddedFormHistory.ShowNextForm'}
end;

procedure TFEEmbeddedFormHistory.ShowPrevForm;
begin
  if not HasPrevForm then
    Exit;
{$MESSAGE WARN 'Not Implemented TFEEmbeddedFormHistory.ShowPrevForm'}
end;

end.
