/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  Winapi.UxTheme,
  System.SysConst,
  System.Types,
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
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
  FormEffects.Constants;

type

{ TBackgroundOptions }

  TBackgroundOptions = class {$IFNDEF FORM_EFFECTS_TESTS}sealed{$ENDIF}(TPersistent)
  public type
    TGlassTranslucency = 0..Byte.MaxValue;

  private type
    TChildrenBackgroundOptions = TList<TBackgroundOptions>;

    TOnChangeEvent = procedure(const Sender: TBackgroundOptions) of object;

  { TPropertiesGroup<T> }

    TPropertiesGroup<T> = class abstract
    strict protected
      FUseParent: Boolean;
      FValue: T;
      FOptions: TBackgroundOptions;

    strict protected
      function GetValue: T; inline;
      procedure SetValue(const Value: T); virtual; abstract;
      procedure SetUseParent(const Value: Boolean); inline;

    strict protected
      function GetPropertiesGroup(const Options: TBackgroundOptions): TPropertiesGroup<T>; virtual; abstract;
      procedure ClearValue(const UseParent: Boolean); virtual;

    public
      constructor Create(const Options: TBackgroundOptions); virtual;

    public
      procedure OnPropertiesChange(const Propagate: Boolean);
      function FindParent(const IsActive: Boolean): TBackgroundOptions; overload; inline;

    strict protected
      function FindParent: TBackgroundOptions; overload; virtual;

    public
      property UseParent: Boolean read FUseParent write SetUseParent;
      property Value: T read GetValue write SetValue;
    end;

  { TVisiblePropertiesGroup<T> }

    TVisiblePropertiesGroup<T> = class abstract(TPropertiesGroup<T>)
    strict private
      FVisible: Boolean;

    strict private
      procedure SetVisible(const Value: Boolean); inline;

    public
      constructor Create(const Options: TBackgroundOptions); override;

    public
      property Visible: Boolean read FVisible write SetVisible;
    end;

  { TBackgoundFormPropertiesGroup }

    TBackgoundFormPropertiesGroup = class sealed(TVisiblePropertiesGroup<TCustomForm>)
    strict protected
      procedure SetValue(const Value: TCustomForm); override; // property BackgoundForm

    strict protected
      procedure ClearValue(const UseParent: Boolean); override;
      function GetPropertiesGroup(const Options: TBackgroundOptions): TPropertiesGroup<TCustomForm>; override;

    public
      constructor Create(const Options: TBackgroundOptions); override;
      destructor Destroy; override;

    public
      function IsActive: Boolean; inline;
      procedure Assign(const Source: TBackgoundFormPropertiesGroup); inline;
    end;

  { TPicturePropertiesGroup }

    TPicturePropertiesGroup = class sealed(TVisiblePropertiesGroup<TPicture>)
    strict private
      FMode: TBackgroundPictureMode;
      FTransparentColor: TColor;

    strict protected
      function GetMode: TBackgroundPictureMode; inline;
      function GetTransparentColor: TColor; inline;
      procedure SetMode(const Value: TBackgroundPictureMode); inline;
      procedure SetTransparentColor(const Value: TColor); inline;
      procedure SetValue(const Value: TPicture); override; // property picture

    strict protected
      procedure ClearValue(const UseParent: Boolean); override;
      function GetPropertiesGroup(const Options: TBackgroundOptions): TPropertiesGroup<TPicture>; override;

    strict private
      procedure OnPictureChange(Sender: TObject);

    public
      constructor Create(const Options: TBackgroundOptions); override;
      destructor Destroy; override;

    public
      function IsActive: Boolean;
      procedure Assign(const Source: TPicturePropertiesGroup); inline;

    public
      property Mode: TBackgroundPictureMode read GetMode write SetMode;
      property TransparentColor: TColor read GetTransparentColor write SetTransparentColor;
    end;

  { TGlassPropertiesGroup }

    TGlassPropertiesGroup = class sealed(TVisiblePropertiesGroup<TGlassTranslucency>)
    strict private
      FColor: TColor;

    strict protected
      function GetColor: TColor; inline;
      procedure SetColor(const Value: TColor); inline;
      procedure SetValue(const Value: TGlassTranslucency); override; // property GlassTranslucency

    strict protected
      function GetPropertiesGroup(const Options: TBackgroundOptions): TPropertiesGroup<TGlassTranslucency>; override;

    public
      constructor Create(const Options: TBackgroundOptions); override;

    public
      function IsActive: Boolean; inline;
      procedure Assign(const Source: TGlassPropertiesGroup); inline;

    public
      property Color: TColor read GetColor write SetColor;
    end;

  { TOpaquePropertiesGroup }

    TOpaquePropertiesGroup = class sealed(TPropertiesGroup<Boolean>)
    strict protected
      procedure SetValue(const Value: Boolean); override; // property Opaque

    strict protected
      function GetPropertiesGroup(const Options: TBackgroundOptions): TPropertiesGroup<Boolean>; override;

    public
      constructor Create(const Options: TBackgroundOptions); override;

    strict protected
      function FindParent: TBackgroundOptions; override;

    public
      function IsActive: Boolean; inline;
      procedure Assign(const Source: TOpaquePropertiesGroup); inline;
    end;

    TActive = record
    strict private
      FBackgroundFormActive: Boolean;
      FPictureActive: Boolean;
      FGlassActive: Boolean;
      FOpaqueActive: Boolean;

    strict private
      procedure SetValues(const BackgroundFormActive, PictureActive, GlassActive, OpaqueActive: Boolean); inline;

    public
      constructor Create(const Options: TBackgroundOptions);

    public
      function IsActive: Boolean; inline;

    public
      property IsBackgroundFormActive: Boolean read FBackgroundFormActive;
      property IsPictureActive: Boolean read FPictureActive;
      property IsGlassActive: Boolean read FGlassActive;
      property IsOpaqueActive: Boolean read FOpaqueActive;
    end;

  private const
    DefaultUseParent = False;
    DefaultVisible   = True;

    DefaultPictureMode             = TBackgroundPictureMode.Tile;
    DefaultPictureTransparentColor = clNone;

    DefaultGlassColor        = clBlack;
    DefaultGlassTranslucency = High(TGlassTranslucency);

    DefaultOpaque = True;

    DefaultThemesEnabled = True;

// ParentPaintedManagement
  private
    class var FParentPaintedList: TList<HWND>;

  private
    FBackgoundFormPropertiesGroup: TBackgoundFormPropertiesGroup;
    FPicturePropertiesGroup: TPicturePropertiesGroup;
    FGlassPropertiesGroup: TGlassPropertiesGroup;
    FOpaquePropertiesGroup: TOpaquePropertiesGroup;

    FThemesEnabled: Boolean;

    FControl: TWinControl;

    FParent: TBackgroundOptions;
    FChildren: TChildrenBackgroundOptions;

    FOnChange: TOnChangeEvent;

  private
    function CoveredByPicture(const ParentForPicture: TBackgroundOptions; const Rect: TRect): Boolean; inline;
    function isXRayActive(
      const ParentForPicture: TBackgroundOptions;
      const Active: TActive;
      const Rect: TRect
    ): Boolean; inline;

    procedure AddChild(const Child: TBackgroundOptions);
    procedure OnChildNotify(Sender: TObject; const Item: TBackgroundOptions; Action: TCollectionNotification);

    procedure DoPaletteChange; inline;
    procedure DoChange;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;

  public
    function IsActive: Boolean; {$INCLUDE inline.inc}
    function IsParentPainted(const Handle: HWND): Boolean; {$INCLUDE inline.inc}
    procedure Assign(Source: TPersistent); override;
    procedure ControlChange; {$INCLUDE inline.inc}
    procedure DrawBackground(const DC: HDC; const Rect: TRect); overload; inline;
    procedure DrawBackground(const DC: HDC; const Bitmap: TBitmap; const Rect: TRect); overload;// {$INCLUDE default.inc}

  public
    function GetBackgoundForm: TCustomForm; overload; {$INCLUDE inline.inc}
    function GetBackgoundForm<F: TCustomForm>: F; overload; inline;
    /// <summary>
    ///   Предыдущая форма будет уничтожена
    /// </summary>
    procedure SetBackgoundForm(const Value: TCustomForm); overload; {$INCLUDE inline.inc}
    procedure SetBackgoundForm(const Value: TCustomFormClass); overload; {$INCLUDE inline.inc}

{$IFNDEF FORM_EFFECTS_TESTS}
  strict private
{$ENDIF ~ FORM_EFFECTS_TESTS}
    function GetParentBackgoundForm: Boolean; {$INCLUDE inline.inc}
    function GetBackgoundFormVisible: Boolean; {$INCLUDE inline.inc}
    procedure SetParentBackgoundForm(const Value: Boolean); {$INCLUDE inline.inc}
    procedure SetBackgoundFormVisible(const Value: Boolean); {$INCLUDE inline.inc}

    function IsPictureStored: Boolean; {$INCLUDE inline.inc}
    function GetParentPicture: Boolean; {$INCLUDE inline.inc}
    function GetPictureVisible: Boolean; {$INCLUDE inline.inc}
    function GetPictureMode: TBackgroundPictureMode; {$INCLUDE inline.inc}
    function GetPictureTransparentColor: TColor; {$INCLUDE inline.inc}
    function GetPicture: TPicture; {$INCLUDE inline.inc}
    procedure SetParentPicture(const Value: Boolean); {$INCLUDE inline.inc}
    procedure SetPictureVisible(const Value: Boolean); {$INCLUDE inline.inc}
    procedure SetPictureMode(const Value: TBackgroundPictureMode); {$INCLUDE inline.inc}
    procedure SetPictureTransparentColor(const Value: TColor); {$INCLUDE inline.inc}
    procedure SetPicture(const Value: TPicture); {$INCLUDE inline.inc}

    function IsGlassStored: Boolean; {$INCLUDE inline.inc}
    function GetParentGlass: Boolean; {$INCLUDE inline.inc}
    function GetGlassVisible: Boolean; {$INCLUDE inline.inc}
    function GetGlassColor: TColor; {$INCLUDE inline.inc}
    function GetGlassTranslucency: TGlassTranslucency; {$INCLUDE inline.inc}
    procedure SetParentGlass(const Value: Boolean); {$INCLUDE inline.inc}
    procedure SetGlassVisible(const Value: Boolean); {$INCLUDE inline.inc}
    procedure SetGlassColor(const Value: TColor); {$INCLUDE inline.inc}
    procedure SetGlassTranslucency(const Value: TGlassTranslucency); {$INCLUDE inline.inc}

    function GetParentOpaque: Boolean; {$INCLUDE inline.inc}
    function GetOpaque: Boolean; {$INCLUDE inline.inc}
    procedure SetParentOpaque(const Value: Boolean); {$INCLUDE inline.inc}
    procedure SetOpaque(const Value: Boolean); {$INCLUDE inline.inc}

    procedure SetThemesEnabled(const Value: Boolean); {$INCLUDE inline.inc}

    function GetControl: TWinControl; {$INCLUDE inline.inc}
    procedure SetControl(const Value: TWinControl); {$INCLUDE default.inc}

    function GetParent: TBackgroundOptions; {$INCLUDE inline.inc}
    function GetChildren: TChildrenBackgroundOptions; {$INCLUDE inline.inc}

    function GetPalette: HPALETTE; {$INCLUDE default.inc}

  public
    property Parent: TBackgroundOptions read GetParent;
    property Control: TWinControl read GetControl write SetControl;
    property Palette: HPALETTE read GetPalette;

  published
    property ParentBackgoundForm: Boolean read GetParentBackgoundForm write SetParentBackgoundForm
      default DefaultUseParent;
    property BackgoundFormVisible: Boolean read GetBackgoundFormVisible write SetBackgoundFormVisible
      default DefaultVisible;

    property ParentPicture: Boolean read GetParentPicture write SetParentPicture default DefaultUseParent;
    property PictureVisible: Boolean read GetPictureVisible write SetPictureVisible default DefaultVisible;
    property PictureMode: TBackgroundPictureMode read GetPictureMode write SetPictureMode
      stored IsPictureStored default DefaultPictureMode;
    property PictureTransparentColor: TColor read GetPictureTransparentColor write SetPictureTransparentColor
      stored IsPictureStored default DefaultPictureTransparentColor;
    property Picture: TPicture read GetPicture write SetPicture stored IsPictureStored;

    property ParentGlass: Boolean read GetParentGlass write SetParentGlass default DefaultUseParent;
    property GlassVisible: Boolean read GetGlassVisible write SetGlassVisible default DefaultVisible;
    property GlassColor: TColor read GetGlassColor write SetGlassColor stored IsGlassStored default DefaultGlassColor;
    property GlassTranslucency: TGlassTranslucency read GetGlassTranslucency write SetGlassTranslucency
      stored IsGlassStored default DefaultGlassTranslucency;

    property ParentOpaque: Boolean read GetParentOpaque write SetParentOpaque default DefaultUseParent;
    property Opaque: Boolean read GetOpaque write SetOpaque default DefaultOpaque;

    property ThemesEnabled: Boolean read FThemesEnabled write SetThemesEnabled default DefaultThemesEnabled;

    property OnChange: TOnChangeEvent read FOnChange write FOnChange;
  end;

  IWithBackgroundOptions = interface
  ['{364A1458-8965-410C-AB77-DDE236AB7626}']
    function GetBackgroundOptions: TBackgroundOptions;
  end;

implementation

uses
  Winapi.Messages,
  System.RTLConsts,
  System.TypInfo,
  FormEffects.TypeHelpers,
  FormEffects.Utils.Rects,
  FormEffects.Utils.ScrollBars,
  FormEffects.Utils.Pictures,
  FormEffects.Rendering,
  FormEffects.Rendering.Ext,
  FormEffects.Rendering.Pictures,
  FormEffects.Backgrounds.Rendering;

{$REGION 'Internal definitions'}

type

{ TControlHelper }

  TControlHelper = class helper for TControl
  public
    function GetBackgroundOptions: TBackgroundOptions; inline;
    /// <summary>
    ///   Return Rect as Left = 0, Top = 0, Right = ClientWidth, Bottom = ClientHeight of self if rect is empty, otherwise - rect
    /// </summary>
    function ResolveControlRect(const Rect: TRect): TRect; inline;
  end;

{ TDrawBackgroundBitmapHelper }

  TDrawBackgroundBitmapHelper = class
  strict protected
    FBitmap: TBitmap;
    FPixelFormat: TPixelFormat;

  strict private
    function GetCanvas: TCanvas; inline;
    function GetCanvasDC: HDC; inline;

  strict protected
    constructor Create(const Bitmap: TBitmap; const PixelFormat: TPixelFormat); reintroduce;

  public
    procedure BitBlt(const DestDC: HDC; const Rect: TRect; const SrcPoint: TPoint); virtual; abstract;
    function CalculateBitmapRect(const DrawRect: TRect): TRect; virtual; abstract;

  public
    property Bitmap: TBitmap read FBitmap;
    property Canvas: TCanvas read GetCanvas;
    property CanvasDC: HDC read GetCanvasDC;
    property PixelFormat: TPixelFormat read FPixelFormat;
  end;

{ TDrawBackgroundInternalBitmapHelper }

  TDrawBackgroundInternalBitmapHelper = class(TDrawBackgroundBitmapHelper)
  public
    constructor Create(const Point: TPoint; const Size: TSize); reintroduce;
    destructor Destroy; override;

  public
    procedure BitBlt(const DestDC: HDC; const Rect: TRect; const SrcPoint: TPoint); override;
    function CalculateBitmapRect(const DrawRect: TRect): TRect; override;
  end;

{ TDrawBackgroundExternalBitmapHelper }

  TDrawBackgroundExternalBitmapHelper = class(TDrawBackgroundBitmapHelper)
  public
    constructor Create(const Bitmap: TBitmap); reintroduce;

  public
    procedure BitBlt(const DestDC: HDC; const Rect: TRect; const SrcPoint: TPoint); override;
    function CalculateBitmapRect(const DrawRect: TRect): TRect; override;
  end;

procedure DrawActiveBackground(
  const Options: TBackgroundOptions;
  const DC: HDC;
  const Rect: TRect;
  const Bitmap: TBitmap;
  const Active: TBackgroundOptions.TActive
); inline;
var
  Helper: TDrawBackgroundBitmapHelper;

begin
  with Options, Active do
  begin
    var DrawRect := Rect;
    DrawRect.OffsetRect(GetScrollbarsOffset(Control.Handle, Control, SIF_POS));

    if (Bitmap = nil) or (Bitmap.PixelFormat = pfDevice) then
      Helper := TDrawBackgroundInternalBitmapHelper.Create(DrawRect.TopLeft, Rect.Size)
    else
      Helper := TDrawBackgroundExternalBitmapHelper.Create(Bitmap);

    with Helper do
    begin
      Canvas.Lock;
      try
        const ParentWithGlass = FGlassPropertiesGroup.FindParent(IsGlassActive);

        if Assigned(ParentWithGlass) and (ParentWithGlass.GlassTranslucency = 0) then
        begin
          Canvas.Brush.Color := ParentWithGlass.GlassColor;
          Canvas.FillRect(DrawRect);
        end
        else
        begin
          const ParentWithPicture = FPicturePropertiesGroup.FindParent(IsPictureActive);

          if isXRayActive(ParentWithPicture, Active, DrawRect) then
            DrawXRay(Control, CanvasDC, Rect, DrawRect)
          else
          begin
            const ParentWithBackgroundForm = FBackgoundFormPropertiesGroup.FindParent(IsBackgroundFormActive);

            if Assigned(ParentWithBackgroundForm) then
            begin
              DrawBackgroundForm(
                Control,
                ParentWithBackgroundForm.Control,
                ParentWithBackgroundForm.GetBackgoundForm,
                CanvasDC,
                Rect,
                DrawRect
              );
            end
            else if Assigned(ParentWithPicture) then
              DrawStandardBackground(ParentWithPicture.Control, CanvasDC, DrawRect, FThemesEnabled)
            else
              DrawStandardBackground(Control, CanvasDC, DrawRect, FThemesEnabled);
          end;

          if Assigned(ParentWithPicture) then
          begin
            DrawPicture(
              ParentWithPicture.Picture.Graphic,
              ParentWithPicture.PictureMode,
              Control,
              ParentWithPicture.Control,
              ParentWithPicture.PictureTransparentColor,
              Bitmap,
              DrawRect,
              0
            );
          end;

          if Assigned(ParentWithGlass) then
          begin
            BlendBackground(
              ParentWithGlass.Control,
              ParentWithGlass.GlassTranslucency,
              ParentWithGlass.GlassColor,
              Bitmap,
              CanvasDC,
              PixelFormat,
              CalculateBitmapRect(DrawRect)
            );
          end;
        end;

        BitBlt(DC, Rect, DrawRect.TopLeft);
      finally
        Canvas.Unlock;
        FreeAndNil(Helper);
      end;
    end;
  end;
end;

{ TControlHelper }

function TControlHelper.GetBackgroundOptions: TBackgroundOptions;
var
  WithBackgroundOptions: IWithBackgroundOptions;

begin
  if Supports(Self, IWithBackgroundOptions, WithBackgroundOptions) then
    Result := WithBackgroundOptions.GetBackgroundOptions
  else
    Result := TBackgroundOptions(Self.Perform(CM_FEGETBACKGRONDOPTIONS, 0, 0));
end;

function TControlHelper.ResolveControlRect(const Rect: TRect): TRect;
begin
  if Rect.IsEmptyRect then
    Result := TRect.InlineCreate(0, 0, ClientWidth, ClientHeight)
  else
    Result := Rect;
end;

{ TDrawBackgroundBitmapHelper }

constructor TDrawBackgroundBitmapHelper.Create(const Bitmap: TBitmap; const PixelFormat: TPixelFormat);
begin
  FBitmap      := Bitmap;
  FPixelFormat := PixelFormat;
end;

function TDrawBackgroundBitmapHelper.GetCanvas: TCanvas;
begin
  Result := FBitmap.Canvas;
end;

function TDrawBackgroundBitmapHelper.GetCanvasDC: HDC;
begin
  Result := FBitmap.Canvas.Handle;
end;

{ TDrawBackgroundInternalBitmapHelper }

procedure TDrawBackgroundInternalBitmapHelper.BitBlt(const DestDC: HDC; const Rect: TRect; const SrcPoint: TPoint);
begin
  CanvasDC.BitBlt(DestDC, Rect, SrcPoint);
end;

function TDrawBackgroundInternalBitmapHelper.CalculateBitmapRect(const DrawRect: TRect): TRect;
begin
  if FPixelFormat = pf8bit then
    Result := DrawRect
  else
    Result := TRect.InlineCreate(FBitmap.Size);
end;

constructor TDrawBackgroundInternalBitmapHelper.Create(const Point: TPoint; const Size: TSize);
begin
  inherited Create(CreateBitmapFactory, GetDevicePixelFormat);

  AdjustBitmapForTransition(FBitmap, Size, FPixelFormat);
  CanvasDC.SetWindowOrgEx(Point);
end;

destructor TDrawBackgroundInternalBitmapHelper.Destroy;
begin
  FreeAndNilBitmap(FBitmap);
end;

{ TDrawBackgroundExternalBitmapHelper }

procedure TDrawBackgroundExternalBitmapHelper.BitBlt(const DestDC: HDC; const Rect: TRect; const SrcPoint: TPoint);
begin
// nothing...
end;

function TDrawBackgroundExternalBitmapHelper.CalculateBitmapRect(const DrawRect: TRect): TRect;
begin
  Result := DrawRect;
  if FPixelFormat <> pf8bit then
    Result.ToDeviceRect(CanvasDC);
end;

constructor TDrawBackgroundExternalBitmapHelper.Create(const Bitmap: TBitmap);
begin
  inherited Create(Bitmap, Bitmap.PixelFormat);
end;

{$ENDREGION 'Internal definitions'}

{ TBackgroundOptions }

function TBackgroundOptions.IsActive: Boolean;
begin
  Result := TActive.Create(Self).IsActive;
end;

procedure TBackgroundOptions.Assign(Source: TPersistent);
begin
  Assert(Source is TBackgroundOptions, SIncorrectBackground);

  const Options = TBackgroundOptions(Source);

  FBackgoundFormPropertiesGroup.Assign(Options.FBackgoundFormPropertiesGroup);

  FPicturePropertiesGroup.Assign(Options.FPicturePropertiesGroup);

  FGlassPropertiesGroup.Assign(Options.FGlassPropertiesGroup);

  FOpaquePropertiesGroup.Assign(Options.FOpaquePropertiesGroup);

  ThemesEnabled := Options.ThemesEnabled;
end;

procedure TBackgroundOptions.ControlChange;
begin
  FPicturePropertiesGroup      .OnPropertiesChange(True);
  FBackgoundFormPropertiesGroup.OnPropertiesChange(True);
  FGlassPropertiesGroup        .OnPropertiesChange(True);
end;

function TBackgroundOptions.CoveredByPicture(const ParentForPicture: TBackgroundOptions; const Rect: TRect): Boolean;
begin
  Result := Assigned(ParentForPicture) and (PictureTransparentColor = clNone);
  if Result then
  begin
    var DrawRect: TRect;
    const PictureRect =
      FormEffects.Utils.Rects.PictureRect(
        ParentForPicture.Picture.Graphic,
        ParentForPicture.PictureMode,
        Control,
        ParentForPicture.Control,
        0,
        DrawRect
      );
    DrawRect := TRect.UnionRects(PictureRect, Rect);
    Result := DrawRect.IsEqual(PictureRect);
  end;
end;

function TBackgroundOptions.isXRayActive(
  const ParentForPicture: TBackgroundOptions;
  const Active: TActive;
  const Rect: TRect
): Boolean;
begin
  with Active do
  begin
    Result :=
      Assigned(Control.Parent)   and
      not IsOpaqueActive         and
      not IsBackgroundFormActive and
      not CoveredByPicture(ParentForPicture, Rect);
  end;
end;

procedure TBackgroundOptions.DrawBackground(const DC: HDC; const Rect: TRect);
begin
  DrawBackground(DC, nil, Rect);
end;

procedure TBackgroundOptions.DrawBackground(const DC: HDC; const Bitmap: TBitmap; const Rect: TRect);
begin
  const Active = TActive.Create(Self);

  if Active.IsActive then
    DrawActiveBackground(Self, DC, Control.ResolveControlRect(Rect), Bitmap, Active)
  else
    DrawStandardBackground(Control, DC, Rect, FThemesEnabled);
end;

constructor TBackgroundOptions.Create;
begin
  FChildren          := TChildrenBackgroundOptions.Create;
  FChildren.OnNotify := OnChildNotify;

  FBackgoundFormPropertiesGroup := TBackgoundFormPropertiesGroup.Create(Self);
  FPicturePropertiesGroup       := TPicturePropertiesGroup.Create(Self);
  FGlassPropertiesGroup         := TGlassPropertiesGroup.Create(Self);
  FOpaquePropertiesGroup        := TOpaquePropertiesGroup.Create(Self);

  FThemesEnabled := DefaultThemesEnabled;
end;

destructor TBackgroundOptions.Destroy;
begin
  if TActive.Create(Self).IsActive and not (csDestroying in Control.ComponentState) then
    DoChange;

  if Assigned(FParent) then
    FParent.FChildren.Remove(Self);

  FreeAndNil(FOpaquePropertiesGroup);
  FreeAndNil(FGlassPropertiesGroup);
  FreeAndNil(FPicturePropertiesGroup);
  FreeAndNil(FBackgoundFormPropertiesGroup);

  FChildren.Clear;
  FreeAndNil(FChildren);
end;

procedure TBackgroundOptions.DoChange;
begin
  if Assigned(Control) then
  begin
    DoPaletteChange;
    Control.Invalidate;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBackgroundOptions.DoPaletteChange;
begin
  if csLoading in Control.ComponentState then
    Exit;

  const Graphic = FPicturePropertiesGroup.Value.Graphic;
  if FPicturePropertiesGroup.IsActive and Graphic.PaletteModified then
  begin
    if Graphic.Palette = 0 then
      Graphic.PaletteModified := False
    else
    begin
      const ParentForm = GetParentForm(Control);
      if Assigned(ParentForm) and ParentForm.Active and ParentForm.HandleAllocated then
      begin
        ParentForm.Handle.SendMessage(WM_QUERYNEWPALETTE, 0, 0);
        Graphic.PaletteModified := False;
      end;
    end;
  end;
end;

procedure TBackgroundOptions.AddChild(const Child: TBackgroundOptions);
begin
  if Child = nil then
    Exit;

  if Assigned(Child.Parent) then
    Child.Parent.FChildren.Remove(Child);

  FChildren.Add(Child);
end;

procedure TBackgroundOptions.OnChildNotify(
  Sender: TObject;
  const Item: TBackgroundOptions;
  Action: TCollectionNotification
);
begin
  case Action of
    cnAdded:
      Item.FParent := Self;
    cnRemoved:
      Item.FParent := nil;
  end;
end;

function TBackgroundOptions.GetControl: TWinControl;
begin
  Result := FControl;
end;

procedure TBackgroundOptions.SetControl(const Value: TWinControl);

  function GetParentBackgroundOptions(const WinControl: TWinControl): TBackgroundOptions;
  begin
    var ParentControl := WinControl.Parent;

    if ParentControl = nil then
    begin
      ParentControl := FindControl(WinControl.ParentWindow);

      if (ParentControl = nil) and (WinControl is TForm) and (TForm(WinControl).FormStyle = fsMDIChild) then
        ParentControl := Application.MainForm;
    end;

    if Assigned(ParentControl) then
    begin
      Result := ParentControl.GetBackgroundOptions;

      if Result = nil then
        Result := GetParentBackgroundOptions(ParentControl);
    end
    else
      Result := nil;
  end;

  procedure EnrichChildBackgroundOptions(const WinControl: TWinControl);
  begin
    for var Index := 0 to WinControl.ControlCount - 1 do
    begin
      const ChildControl = WinControl.Controls[Index];

      const Options = ChildControl.GetBackgroundOptions;
      if Assigned(Options) then
        AddChild(Options)
      else if ChildControl is TWinControl then
        EnrichChildBackgroundOptions(TWinControl(ChildControl));
    end;
  end;

begin
  if FControl = Value then
    Exit;

  FControl := Value;

  if Assigned(FParent) then
    FParent.FChildren.Remove(Self);

  FChildren.Clear;

  if FControl = nil then
    Exit;

  const ParentBackgroundOptions = GetParentBackgroundOptions(FControl);
  if Assigned(ParentBackgroundOptions) then
    ParentBackgroundOptions.AddChild(Self);

  EnrichChildBackgroundOptions(FControl);
end;

function TBackgroundOptions.GetBackgoundForm: TCustomForm;
begin
  Result := FBackgoundFormPropertiesGroup.Value;
end;

function TBackgroundOptions.GetBackgoundForm<F>: F;
begin
  const Form = GetBackgoundForm;

  Assert((Form = nil) or (Form is F), SInvalidCast);

  Result := F(Form);
end;

function TBackgroundOptions.GetParentBackgoundForm: Boolean;
begin
  Result := FBackgoundFormPropertiesGroup.UseParent;
end;

function TBackgroundOptions.GetBackgoundFormVisible: Boolean;
begin
  Result := FBackgoundFormPropertiesGroup.Visible;
end;

procedure TBackgroundOptions.SetBackgoundForm(const Value: TCustomForm);
begin
  {$MESSAGE WARN 'Need to add Assert for parent of form'}
  FBackgoundFormPropertiesGroup.Value := Value;
end;

procedure TBackgroundOptions.SetBackgoundForm(const Value: TCustomFormClass);
begin
  if Assigned(Value) and Assigned(Control) then
    SetBackgoundForm(Value.Create(Control))
  else
    SetBackgoundForm(nil);
end;

procedure TBackgroundOptions.SetParentBackgoundForm(const Value: Boolean);
begin
  FBackgoundFormPropertiesGroup.UseParent := Value;
end;

procedure TBackgroundOptions.SetBackgoundFormVisible(const Value: Boolean);
begin
  FBackgoundFormPropertiesGroup.Visible := Value;
end;

function TBackgroundOptions.IsParentPainted(const Handle: HWND): Boolean;
begin
  Result :=
    Assigned(FParentPaintedList)   and
    (FParentPaintedList.Count > 0) and
    (FParentPaintedList[FParentPaintedList.Count - 1] = Handle);
end;

function TBackgroundOptions.IsPictureStored: Boolean;
begin
  Result := not FPicturePropertiesGroup.UseParent;
end;

function TBackgroundOptions.GetParentPicture: Boolean;
begin
  Result := FPicturePropertiesGroup.UseParent;
end;

function TBackgroundOptions.GetPictureVisible: Boolean;
begin
  Result := FPicturePropertiesGroup.Visible;
end;

function TBackgroundOptions.GetPictureMode: TBackgroundPictureMode;
begin
  Result := FPicturePropertiesGroup.Mode;
end;

function TBackgroundOptions.GetPictureTransparentColor: TColor;
begin
  Result := FPicturePropertiesGroup.TransparentColor;
end;

function TBackgroundOptions.GetPicture: TPicture;
begin
  Result := FPicturePropertiesGroup.Value;
end;

procedure TBackgroundOptions.SetParentPicture(const Value: Boolean);
begin
  FPicturePropertiesGroup.UseParent := Value;
end;

procedure TBackgroundOptions.SetPictureVisible(const Value: Boolean);
begin
  FPicturePropertiesGroup.Visible := Value;
end;

procedure TBackgroundOptions.SetPictureMode(const Value: TBackgroundPictureMode);
begin
  FPicturePropertiesGroup.Mode := Value;
end;

procedure TBackgroundOptions.SetPictureTransparentColor(const Value: TColor);
begin
  FPicturePropertiesGroup.TransparentColor := Value;
end;

procedure TBackgroundOptions.SetPicture(const Value: TPicture);
begin
  FPicturePropertiesGroup.Value := Value;
end;

function TBackgroundOptions.IsGlassStored: Boolean;
begin
  Result := not FGlassPropertiesGroup.UseParent;
end;

function TBackgroundOptions.GetParentGlass: Boolean;
begin
  Result := FGlassPropertiesGroup.UseParent;
end;

function TBackgroundOptions.GetGlassVisible: Boolean;
begin
  Result := FGlassPropertiesGroup.Visible;
end;

function TBackgroundOptions.GetGlassColor: TColor;
begin
  Result := FGlassPropertiesGroup.Color;
end;

function TBackgroundOptions.GetGlassTranslucency: TGlassTranslucency;
begin
  Result := FGlassPropertiesGroup.Value;
end;

procedure TBackgroundOptions.SetParentGlass(const Value: Boolean);
begin
  FGlassPropertiesGroup.UseParent := Value;
end;

procedure TBackgroundOptions.SetGlassVisible(const Value: Boolean);
begin
  FGlassPropertiesGroup.Visible := Value;
end;

procedure TBackgroundOptions.SetGlassColor(const Value: TColor);
begin
  FGlassPropertiesGroup.Color := Value;
end;

procedure TBackgroundOptions.SetGlassTranslucency(const Value: TGlassTranslucency);
begin
  FGlassPropertiesGroup.Value := Value;
end;

function TBackgroundOptions.GetParentOpaque: Boolean;
begin
  Result := FOpaquePropertiesGroup.UseParent;
end;

function TBackgroundOptions.GetOpaque: Boolean;
begin
  Result := FOpaquePropertiesGroup.Value;
end;

procedure TBackgroundOptions.SetParentOpaque(const Value: Boolean);
begin
  FOpaquePropertiesGroup.UseParent := Value;
end;

procedure TBackgroundOptions.SetOpaque(const Value: Boolean);
begin
  FOpaquePropertiesGroup.Value := Value;
end;

procedure TBackgroundOptions.SetThemesEnabled(const Value: Boolean);
begin
  if FThemesEnabled <> Value then
  begin
    FThemesEnabled := Value;
    DoChange;
  end;
end;

function TBackgroundOptions.GetPalette: HPALETTE;
begin
  const Graphic = Picture.Graphic;
  if Assigned(Graphic) then
    Result := Graphic.Palette
  else
    Result := 0;
end;

function TBackgroundOptions.GetParent: TBackgroundOptions;
begin
  Result := FParent;
end;

function TBackgroundOptions.GetChildren: TChildrenBackgroundOptions;
begin
  Result := FChildren;
end;

{ TBackgroundOptions.TPropertiesGroup<T> }

procedure TBackgroundOptions.TPropertiesGroup<T>.ClearValue(const UseParent: Boolean);
begin
// nothing...
end;

constructor TBackgroundOptions.TPropertiesGroup<T>.Create(const Options: TBackgroundOptions);
begin
  FOptions := Options;

  FUseParent := TBackgroundOptions.DefaultUseParent;
end;

function TBackgroundOptions.TPropertiesGroup<T>.FindParent: TBackgroundOptions;
begin
  const Parent = FOptions.Parent;
  if GetPropertiesGroup(FOptions).FUseParent and Assigned(Parent) then
    Result := GetPropertiesGroup(Parent).FindParent
  else
    Result := FOptions;
end;

function TBackgroundOptions.TPropertiesGroup<T>.FindParent(const IsActive: Boolean): TBackgroundOptions;
begin
  if IsActive then
    Result := FindParent
  else
    Result := nil;
end;

function TBackgroundOptions.TPropertiesGroup<T>.GetValue: T;
begin
  Result := GetPropertiesGroup(FindParent).FValue;
end;

procedure TBackgroundOptions.TPropertiesGroup<T>.OnPropertiesChange(const Propagate: Boolean);
begin
  if Propagate then
  begin
    for var Child in FOptions.GetChildren do
    begin
      const PropertiesGroup = GetPropertiesGroup(Child);
      if PropertiesGroup.FUseParent then
        PropertiesGroup.OnPropertiesChange(True);
    end;
  end;

  FOptions.DoChange;
end;

procedure TBackgroundOptions.TPropertiesGroup<T>.SetUseParent(const Value: Boolean);
begin
  if FUseParent = Value then
    Exit;

  ClearValue(Value);

  FUseParent := Value;
  OnPropertiesChange(True);
end;

{ TBackgroundOptions.TVisiblePropertiesGroup<T> }

constructor TBackgroundOptions.TVisiblePropertiesGroup<T>.Create(const Options: TBackgroundOptions);
begin
  inherited Create(Options);

  FVisible := TBackgroundOptions.DefaultVisible;
end;

procedure TBackgroundOptions.TVisiblePropertiesGroup<T>.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then
    Exit;

  FVisible := Value;
  OnPropertiesChange(False);
end;

{ TBackgroundOptions.TBackgoundFormPropertiesGroup }

procedure TBackgroundOptions.TBackgoundFormPropertiesGroup.Assign(const Source: TBackgoundFormPropertiesGroup);
begin
  const UseParentValue = Source.UseParent;

  Visible   := Source.Visible;
  UseParent := UseParentValue;

  if UseParentValue then
    Exit;

  const Form = Source.Value;
  if Assigned(Form) then
    FOptions.SetBackgoundForm(TCustomFormClass(Form.ClassType));
end;

procedure TBackgroundOptions.TBackgoundFormPropertiesGroup.ClearValue(const UseParent: Boolean);
begin
  if UseParent and Assigned(FValue) then
    FreeAndNil(FValue);
end;

constructor TBackgroundOptions.TBackgoundFormPropertiesGroup.Create(const Options: TBackgroundOptions);
begin
  inherited Create(Options);

  FValue := nil;
end;

destructor TBackgroundOptions.TBackgoundFormPropertiesGroup.Destroy;
begin
  if Assigned(FValue) then
    FreeAndNil(FValue);
end;

function TBackgroundOptions.TBackgoundFormPropertiesGroup.GetPropertiesGroup(
  const Options: TBackgroundOptions
): TPropertiesGroup<TCustomForm>;
begin
  Result := Options.FBackgoundFormPropertiesGroup;
end;

function TBackgroundOptions.TBackgoundFormPropertiesGroup.IsActive: Boolean;
begin
  Result := Visible and Assigned(Value);
end;

procedure TBackgroundOptions.TBackgoundFormPropertiesGroup.SetValue(const Value: TCustomForm);
begin
  if FValue = Value then
    Exit;

  FUseParent := False;

  if Assigned(FValue) then
    FreeAndNil(FValue);

  const Control = FOptions.Control;

  if Assigned(Value) and Assigned(Control) then
  begin
    FValue             := Value;
    FValue.BorderStyle := bsNone;
    FValue.Left        := 0;
    FValue.Top         := 0;
    FValue.SetProtectedBorderIcons([]);
    FValue.SetBounds(-1, -1, Control.ClientWidth, Control.ClientHeight);

    const ControlWnd = Control.Handle;
    ControlWnd.SetWindowRgn(HRGN.CreateRectRgn(0, 0, 1, 1), False);
    ControlWnd.ShowWindow(SW_SHOWNOACTIVATE);

    FValue.Visible := True;
  end;

  OnPropertiesChange(True);
end;

{ TBackgroundOptions.TPicturePropertiesGroup }

procedure TBackgroundOptions.TPicturePropertiesGroup.Assign(const Source: TPicturePropertiesGroup);
begin
  const UseParentValue = Source.UseParent;

  Visible   := Source.Visible;
  UseParent := UseParentValue;

  if UseParentValue then
    Exit;

  Value.Assign(Source.Value);
  Mode             := Source.Mode;
  TransparentColor := Source.TransparentColor;
end;

procedure TBackgroundOptions.TPicturePropertiesGroup.ClearValue(const UseParent: Boolean);
begin
  if UseParent then
    FValue.Graphic := nil;
end;

constructor TBackgroundOptions.TPicturePropertiesGroup.Create(const Options: TBackgroundOptions);
begin
  inherited Create(Options);

  FValue            := TPicture.Create;
  FValue.OnChange   := OnPictureChange;
  FMode             := TBackgroundOptions.DefaultPictureMode;
  FTransparentColor := TBackgroundOptions.DefaultPictureTransparentColor;
end;

destructor TBackgroundOptions.TPicturePropertiesGroup.Destroy;
begin
  FreeAndNil(FValue);
end;

function TBackgroundOptions.TPicturePropertiesGroup.GetMode: TBackgroundPictureMode;
begin
  Result := FindParent.FPicturePropertiesGroup.FMode;
end;

function TBackgroundOptions.TPicturePropertiesGroup.GetPropertiesGroup(
  const Options: TBackgroundOptions
): TPropertiesGroup<TPicture>;
begin
  Result := Options.FPicturePropertiesGroup;
end;

function TBackgroundOptions.TPicturePropertiesGroup.GetTransparentColor: TColor;
begin
  Result := FindParent.FPicturePropertiesGroup.FTransparentColor;
end;

function TBackgroundOptions.TPicturePropertiesGroup.IsActive: Boolean;
begin
  Result := Visible;
  if Result then
  begin
    const Graphic = Value.Graphic;
    Result := Assigned(Graphic) and not Graphic.Empty and (Graphic.Width > 0) and (Graphic.Height > 0);
  end;
end;

procedure TBackgroundOptions.TPicturePropertiesGroup.OnPictureChange(Sender: TObject);
begin
  OnPropertiesChange(True);
end;

procedure TBackgroundOptions.TPicturePropertiesGroup.SetMode(const Value: TBackgroundPictureMode);
begin
  if FMode = Value then
    Exit;

  FMode := Value;
  OnPropertiesChange(True);
end;

procedure TBackgroundOptions.TPicturePropertiesGroup.SetTransparentColor(const Value: TColor);
begin
  if FTransparentColor = Value then
    Exit;

  FTransparentColor := Value;
  OnPropertiesChange(True);
end;

procedure TBackgroundOptions.TPicturePropertiesGroup.SetValue(const Value: TPicture);
begin
  if FValue = Value then
    Exit;

  FUseParent := False;
  FValue.Assign(Value);
end;

{ TBackgroundOptions.TGlassPropertiesGroup }

procedure TBackgroundOptions.TGlassPropertiesGroup.Assign(const Source: TGlassPropertiesGroup);
begin
  const UseParentValue = Source.UseParent;

  Visible   := Source.Visible;
  UseParent := UseParentValue;

  if UseParentValue then
    Exit;

  Color := Source.Color;
  Value := Source.Value;
end;

constructor TBackgroundOptions.TGlassPropertiesGroup.Create(const Options: TBackgroundOptions);
begin
  inherited Create(Options);

  FValue := TBackgroundOptions.DefaultGlassTranslucency;
  FColor := TBackgroundOptions.DefaultGlassColor;
end;

function TBackgroundOptions.TGlassPropertiesGroup.GetColor: TColor;
begin
  Result := FindParent.FGlassPropertiesGroup.FColor;
end;

function TBackgroundOptions.TGlassPropertiesGroup.GetPropertiesGroup(
  const Options: TBackgroundOptions
): TPropertiesGroup<TGlassTranslucency>;
begin
  Result := Options.FGlassPropertiesGroup;
end;

function TBackgroundOptions.TGlassPropertiesGroup.IsActive: Boolean;
begin
  Result := Visible and (Value < High(TGlassTranslucency));
end;

procedure TBackgroundOptions.TGlassPropertiesGroup.SetColor(const Value: TColor);
begin
  if FColor = Value then
    Exit;

  FUseParent := False;
  FColor     := Value;
  OnPropertiesChange(True);
end;

procedure TBackgroundOptions.TGlassPropertiesGroup.SetValue(const Value: TGlassTranslucency);
begin
  if FValue = Value then
    Exit;

  FUseParent := False;
  FValue     := Value;

  OnPropertiesChange(True);
end;

{ TBackgroundOptions.TOpaquePropertiesGroup }

procedure TBackgroundOptions.TOpaquePropertiesGroup.Assign(const Source: TOpaquePropertiesGroup);
begin
  const UseParentValue = Source.UseParent;

  UseParent := UseParentValue;

  if UseParentValue then
    Exit;

  Value := Source.Value;
end;

constructor TBackgroundOptions.TOpaquePropertiesGroup.Create(const Options: TBackgroundOptions);
begin
  inherited Create(Options);

  FValue := TBackgroundOptions.DefaultOpaque;
end;

function TBackgroundOptions.TOpaquePropertiesGroup.FindParent: TBackgroundOptions;
begin
  const Parent  = FOptions.Parent;
  const Control = FOptions.Control;
  if
    FUseParent        and
    Assigned(Parent)  and
    Assigned(Control) and
    (Parent.Control = Control.Parent)
  then
    Result := Parent.FOpaquePropertiesGroup.FindParent
  else
    Result := FOptions;
end;

function TBackgroundOptions.TOpaquePropertiesGroup.GetPropertiesGroup(
  const Options: TBackgroundOptions
): TPropertiesGroup<Boolean>;
begin
  Result := Options.FOpaquePropertiesGroup;
end;

function TBackgroundOptions.TOpaquePropertiesGroup.IsActive: Boolean;
begin
  Result := Value;
end;

procedure TBackgroundOptions.TOpaquePropertiesGroup.SetValue(const Value: Boolean);
begin
  if FValue = Value then
    Exit;

  FUseParent := False;
  FValue     := Value;

  const Control = FOptions.Control;
  if Assigned(Control) then
    Control.Invalidate;
end;

{ TBackgroundOptions.TActive }

constructor TBackgroundOptions.TActive.Create(const Options: TBackgroundOptions);
begin
  if Assigned(Options.Control) then
  begin
    SetValues(
      Options.FBackgoundFormPropertiesGroup.IsActive,
      Options.FPicturePropertiesGroup.IsActive,
      Options.FGlassPropertiesGroup.IsActive,
      Options.FOpaquePropertiesGroup.IsActive
    );
  end
  else
    SetValues(False, False, False, True);
end;

function TBackgroundOptions.TActive.IsActive: Boolean;
begin
  Result := FBackgroundFormActive or FPictureActive or FGlassActive or not FOpaqueActive;
end;

procedure TBackgroundOptions.TActive.SetValues(
  const BackgroundFormActive, PictureActive, GlassActive, OpaqueActive: Boolean
);
begin
  FBackgroundFormActive := BackgroundFormActive;
  FPictureActive        := PictureActive;
  FGlassActive          := GlassActive;
  FOpaqueActive         := OpaqueActive;
end;

var SavedDrawThemeParentBackground: function(Wnd: HWND; DC: HDC; prc: PRect): HRESULT; stdcall;

function FormEffectstDrawThemeParentBackground(Wnd: HWND; DC: HDC; prc: PRect): HRESULT; stdcall;
begin
  TBackgroundOptions.FParentPaintedList.Add(Wnd.GetParent);
  try
    Result := SavedDrawThemeParentBackground(Wnd, DC, prc);
  finally
    TBackgroundOptions.FParentPaintedList.Delete(TBackgroundOptions.FParentPaintedList.Count - 1);
  end;
end;

procedure InitializParentPaintedManagement; inline;
begin
  StyleServices;

  if Assigned(DrawThemeParentBackground) then
  begin
    SavedDrawThemeParentBackground := DrawThemeParentBackground;
    DrawThemeParentBackground := FormEffectstDrawThemeParentBackground;
    TBackgroundOptions.FParentPaintedList := TList<HWND>.Create;
  end;
end;

procedure FinalizeParentPaintedManagement; inline;
begin
  if Assigned(TBackgroundOptions.FParentPaintedList) then
    FreeAndNil(TBackgroundOptions.FParentPaintedList);
end;

initialization
  InitializParentPaintedManagement;

finalization
  FinalizeParentPaintedManagement;

end.
