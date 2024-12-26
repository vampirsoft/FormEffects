/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.FormContainer.pas                              *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.FormContainer;

{$INCLUDE FormEffects.inc}

interface

uses
  System.Types,
  Winapi.Windows,
  System.Rtti,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  FormEffects.TypeHelpers,
  FormEffects.Utils.ScrollBars
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.System.Classes.Mocks
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

{ VCL control message IDs }

const
  CM_FEBASE                = CM_BASE    + 532;
  CM_FEGETBACKGRONDOPTIONS = CM_FEBASE +   0;

type

  TFEBackgroundPictureMode = (Center, CenterStretch, Stretch, Tile, Zoom, TopLeft, TopRight, BottomLeft, BottomRight);

{ TFEFormContainer }

  TFEFormContainer = class(TScrollingWinControl)
  public type
    TBackgroundOptions = class(TPersistent)
    strict private type
      TActive = record
      strict private
        FOpaqueActive: Boolean;
        FBackgroundFormActive: Boolean;
        FPictureActive: Boolean;
        FGlassActive: Boolean;

      strict private
        function GetActive: Boolean; inline;

      private
        constructor Create(const OpaqueActive, BackgroundFormActive, PictureActive, GlassActive: Boolean); reintroduce;

      public
        property Active: Boolean read GetActive;
        property OpaqueActive: Boolean read FOpaqueActive;
        property BackgroundFormActive: Boolean read FBackgroundFormActive;
        property PictureActive: Boolean read FPictureActive;
        property GlassActive: Boolean read FGlassActive;
      end;

      TChildrenBackgroundOptions = TObjectList<TBackgroundOptions>;

    strict private
      FParentOpaque: Boolean;
      FOpaque: Boolean;
      FParentBackgoundForm: Boolean;
      FBackgoundFormVisible: Boolean;
      FParentPicture: Boolean;
      FPictureVisible: Boolean;
      FParentGlass: Boolean;
      FGlassVisible: Boolean;
      FThemesDisabled: Boolean;
      FPictureMode: TFEBackgroundPictureMode;
      FGlassTranslucency: Byte;
      FParent: TBackgroundOptions;
      FPictureTransparentColor: TColor;
      FGlassColor: TColor;
      FChildren: TChildrenBackgroundOptions;
      FPicture: TPicture;
      FControl: TWinControl;
      FBackgoundForm: TCustomForm;

      FOnChange: TNotifyEvent;

    strict private
      function IsPictureStored: Boolean; inline;
      function IsGlassStored: Boolean; inline;
      function GetOpaque: Boolean; inline;
      function GetBackgoundForm: TCustomForm; inline;
      function GetPictureMode: TFEBackgroundPictureMode; inline;
      function GetPictureTransparentColor: TColor; inline;
      function GetPicture: TPicture; inline;
      function GetGlassColor: TColor; inline;
      function GetGlassTranslucency: Byte; inline;
      function GetPalette: HPALETTE; inline;
      procedure SetControl(const Value: TWinControl);
      procedure SetParentOpaque(const Value: Boolean); inline;
      procedure SetOpaque(const Value: Boolean); inline;
      procedure SetParentBackgoundForm(const Value: Boolean); inline;
      procedure SetBackgoundFormVisible(const Value: Boolean); inline;
      procedure SetParentPicture(const Value: Boolean); inline;
      procedure SetPictureVisible(const Value: Boolean); inline;
      procedure SetPictureMode(const Value: TFEBackgroundPictureMode); inline;
      procedure SetPictureTransparentColor(const Value: TColor); inline;
      procedure SetPicture(const Value: TPicture); inline;
      procedure SetParentGlass(const Value: Boolean); inline;
      procedure SetGlassVisible(const Value: Boolean); inline;
      procedure SetGlassColor(const Value: TColor); inline;
      procedure SetGlassTranslucency(const Value: Byte); inline;
      procedure SetThemesDisabled(const Value: Boolean); inline;

    strict private
      function IsOpaqueActive: Boolean; inline;
      function IsBackgroundFormActive: Boolean; inline;
      function IsPictureActive: Boolean;
      function IsGlassActive: Boolean; inline;
      function isXRayActive(const BackgroundOptions: TBackgroundOptions; const Rect: TRect): Boolean; inline;
      function IsInternalActive: TActive;

      function FindParentWithOpaque: TBackgroundOptions;
      function FindParentWithBackgroundForm: TBackgroundOptions;
      function FindParentWithPicture: TBackgroundOptions;
      function FindParentWithGlass: TBackgroundOptions;

      function ResolveParentWithBackgroundForm(const isActive: Boolean): TBackgroundOptions; inline;
      function ResolveParentWithPicture(const isActive: Boolean): TBackgroundOptions; inline;
      function ResolveParentWithGlass(const isActive: Boolean): TBackgroundOptions; inline;

      procedure AddChild(const Child: TBackgroundOptions);
      procedure RemoveChild(const Child: TBackgroundOptions); inline;
      procedure OpaqueChange(const Sender: TObject);
      procedure BackgroundFormChange(const Sender: TObject; const Propagate: Boolean);
      procedure PictureChange(const Sender: TObject; const Propagate: Boolean);
      procedure GlassChange(const Sender: TObject; const Propagate: Boolean);
      procedure DoPaletteChange; inline;
      procedure DoChange;

      procedure OnPictureChange(Sender: TObject);

    public
      constructor Create; reintroduce;
      destructor Destroy; override;

    public
      function  IsActive: Boolean; inline;
      procedure Assign(Source: TPersistent); override;
      procedure ControlChange(const Sender: TObject); inline;
      procedure DrawBackground(const DC: HDC; const Bitmap: TBitmap; const Rect: TRect);
      procedure SetBackgoundForm(const Value: TCustomFormClass); inline;

    public
      property BackgoundForm: TCustomForm read GetBackgoundForm;
      property Control: TWinControl read FControl write SetControl;
      property Parent: TBackgroundOptions read FParent;
      property Palette: HPALETTE read GetPalette;

    published
      property ParentOpaque: Boolean read FParentOpaque write SetParentOpaque default False;
      property Opaque: Boolean read GetOpaque write SetOpaque default True;

      property ParentBackgoundForm: Boolean read FParentBackgoundForm write SetParentBackgoundForm default False;
      property BackgoundFormVisible: Boolean read FBackgoundFormVisible write SetBackgoundFormVisible default True;

      property ParentPicture: Boolean read FParentPicture write SetParentPicture default False;
      property PictureVisible: Boolean read FPictureVisible write SetPictureVisible default True;
      property PictureMode: TFEBackgroundPictureMode read GetPictureMode write SetPictureMode
        stored IsPictureStored default TFEBackgroundPictureMode.Tile;
      property PictureTransparentColor: TColor read GetPictureTransparentColor write SetPictureTransparentColor
        stored IsPictureStored default clNone;
      property Picture: TPicture read GetPicture write SetPicture stored IsPictureStored;

      property ParentGlass: Boolean read FParentGlass write SetParentGlass default False;
      property GlassVisible: Boolean read FGlassVisible write SetGlassVisible default True;
      property GlassColor: TColor read GetGlassColor write SetGlassColor stored IsGlassStored default clBlack;
      property GlassTranslucency: Byte read GetGlassTranslucency write SetGlassTranslucency
        stored IsGlassStored default 255;

      property ThemesDisabled: Boolean read FThemesDisabled write SetThemesDisabled default False;

      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TEmbeddedForm = class(TCustomForm)
    strict private
      FBackgroundOptions: TBackgroundOptions;

    strict private
      procedure SetBackgroundOptions(const Value: TBackgroundOptions);

    public
      constructor Create(Owner: TComponent); override;
      destructor Destroy; override;

    published
      property BackgroundOptions: TBackgroundOptions read FBackgroundOptions write SetBackgroundOptions;
    end;

  strict private
    FBackgroundOptions: TBackgroundOptions;

  strict private
    procedure SetBackgroundOptions(const Value: TBackgroundOptions);

  public
    constructor Create(Owner: TComponent); override;
    destructor  Destroy; override;

  published
    property BackgroundOptions: TBackgroundOptions read FBackgroundOptions write SetBackgroundOptions;
  end;

implementation

uses
  Winapi.Messages,
  System.UITypes,
  System.SysUtils,
  FormEffects.Utils.Rects,
  FormEffects.Utils.Pictures,
  FormEffects.Rendering,
  FormEffects.Rendering.Pictures;

{$REGION 'Internal definitions'}

procedure DrawXRay(
  const WinControl: TWinControl;
  const Canvas: TCanvas;
  const Rect, DrawRect: TRect;
  const Size: TSize;
  const PixelFormat: TPixelFormat
); inline;
begin
  const WinControlWnd = WinControl.Handle;
  const ParentWnd     = WinControl.Parent.Handle;
  const CanvasDC      = Canvas.Handle;

  var TempDrawRect := DrawRect;
  LPToDP(CanvasDC, TempDrawRect, 2);

  var TempRect := Rect;

  WinControlWnd.MapWindowRect(ParentWnd, TempRect);

  const SaveClipRgn   = HRGN.Zero;
  const ExistsClipRgn = GetClipRgn(CanvasDC, SaveClipRgn) = 1;
  const ClipRgn       = HRGN.Create(TempDrawRect);

  SelectClipRgn(CanvasDC, ClipRgn);
  ClipRgn.Delete;

  try
    var Point: TPoint;
    OffsetWindowOrgEx(
      CanvasDC,
      TempRect.Left - Rect.Left - (DrawRect.Left - Rect.Left),
      TempRect.Top - Rect.Top - (DrawRect.Top - Rect.Top),
      Point
    );
    try
      const HasUpdateRect = GetUpdateRect(WinControlWnd, TRect(nil^), False);
      try
        RenderWindowToDC(
          ParentWnd,
          WinControlWnd,
          WinControl.Parent,
          CanvasDC,
          TempRect,
          True,
          False,
          False
        );
      finally
        if not HasUpdateRect then
          ValidateRect(WinControlWnd, nil);
      end;
    finally
      SetWindowOrgEx(CanvasDC, Point.x, Point.y, nil);
    end;
  finally
    if ExistsClipRgn then
      SelectClipRgn(CanvasDC, SaveClipRgn)
    else
      SelectClipRgn(CanvasDC, 0);
    SaveClipRgn.Delete;
  end;
end;

procedure DrawBackgroundForm(
  const BackgroundOptions: TFEFormContainer.TBackgroundOptions;
  const Control: TWinControl;
  const Bitmap: TBitmap;
  const Rect, DrawRect: TRect;
  const Size: TSize;
  const PixelFormat: TPixelFormat
); inline;
begin
{$MESSAGE WARN 'Not Implemented DrawBackgroundForm'}
end;

procedure DrawStandardBackground(
  const Control: TWinControl;
  const DC: HDC;
  const Rect: TRect;
  const ThemesDisabled: Boolean
);
begin
{$MESSAGE WARN 'Not Implemented DrawStandardBackground'}
end;

procedure BlendBackground(
  const BackgroundOptions: TFEFormContainer.TBackgroundOptions;
  const Bitmap: TBitmap;
  const isLocalBmp: Boolean;
  const Rect: TRect;
  const Size: TSize;
  const PixelFormat: TPixelFormat
); inline;
begin
{$MESSAGE WARN 'Not Implemented BlendBackground'}
end;

{$ENDREGION 'Internal definitions'}

{ TFEFormContainer }

constructor TFEFormContainer.Create(Owner: TComponent);
begin
{$MESSAGE WARN 'Not Implemented TFEFormContainer.Create'}
  inherited Create(Owner);

  FBackgroundOptions := TBackgroundOptions.Create;
end;

destructor TFEFormContainer.Destroy;
begin
{$MESSAGE WARN 'Not Implemented TFEFormContainer.Destroy'}
  FreeAndNil(FBackgroundOptions);

  inherited Destroy;
end;

procedure TFEFormContainer.SetBackgroundOptions(const Value: TBackgroundOptions);
begin
{$MESSAGE WARN 'Not Implemented TFEFormContainer.SetBackgroundOptions'}
end;

{ TFEFormContainer.TBackgroundOptions }

procedure TFEFormContainer.TBackgroundOptions.AddChild(const Child: TBackgroundOptions);
begin
  if Assigned(Child) then
  begin
    if Assigned(Child.FParent) then
      Child.FParent.RemoveChild(Child);

    FChildren.Add(Child);
    Child.FParent := Self;
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.Assign(Source: TPersistent);
var
  ParentBooleanValue: Boolean;

begin
  if Source is TBackgroundOptions then
  begin
    const Options = Source as TBackgroundOptions;

    ParentBooleanValue := Options.ParentOpaque;
    ParentOpaque       := ParentBooleanValue;
    if not ParentBooleanValue then
      Opaque := Options.Opaque;

    ParentBooleanValue   := Options.ParentBackgoundForm;
    BackgoundFormVisible := Options.BackgoundFormVisible;
    ParentBackgoundForm  := ParentBooleanValue;
    const BackgoundForm   = Options.BackgoundForm;
    if not ParentBooleanValue and Assigned(BackgoundForm) then
    begin
      SetBackgoundForm(TCustomFormClass(BackgoundForm.ClassType));
    end;

    ParentBooleanValue := Options.ParentPicture;
    PictureVisible     := Options.PictureVisible;
    ParentPicture      := ParentBooleanValue;
    if not ParentBooleanValue then
    begin
      Picture.Assign(Options.Picture);
      PictureMode             := Options.PictureMode;
      PictureTransparentColor := Options.PictureTransparentColor;
    end;

    ParentBooleanValue := Options.ParentGlass;
    GlassVisible       := Options.GlassVisible;
    ParentGlass        := ParentBooleanValue;
    if not ParentBooleanValue then
    begin
      GlassColor        := Options.GlassColor;
      GlassTranslucency := Options.GlassTranslucency;
    end;

    ThemesDisabled := Options.ThemesDisabled;
  end
  else
    inherited Assign(Source);
end;

procedure TFEFormContainer.TBackgroundOptions.BackgroundFormChange(const Sender: TObject; const Propagate: Boolean);
begin
  if Propagate then
  begin
    for var Child in FChildren do
    begin
      if Child.FParentBackgoundForm then
        Child.BackgroundFormChange(Self, True);
    end;
  end;

  DoChange;
end;

procedure TFEFormContainer.TBackgroundOptions.ControlChange(const Sender: TObject);
begin
  PictureChange       (Sender, True);
  BackgroundFormChange(Sender, True);
  GlassChange         (Sender, True);
end;

constructor TFEFormContainer.TBackgroundOptions.Create;
begin
  FChildren                := TChildrenBackgroundOptions.Create;

  FParentOpaque            := False;
  FOpaque                  := True;

  FParentBackgoundForm     := False;
  FBackgoundFormVisible    := True;
  FBackgoundForm           := nil;

  FParentPicture           := False;
  FPictureVisible          := True;
  FPictureMode             := TFEBackgroundPictureMode.Tile;
  FPictureTransparentColor := clNone;
  FPicture                 := TPicture.Create;
  FPicture.OnChange        := OnPictureChange;
  FParentGlass             := False;
  FGlassVisible            := True;
  FGlassColor              := clBlack;
  FGlassTranslucency       := 255;
  FThemesDisabled          := False;
end;

destructor TFEFormContainer.TBackgroundOptions.Destroy;
begin
  if Assigned(FControl) and (not(csDestroying in Control.ComponentState)) and IsActive then
    DoChange;

  if Assigned(FParent) then
    FParent.RemoveChild(Self);

  FreeAndNil(FPicture);

  for var Child in FChildren do
    RemoveChild(Child);
  FreeAndNil(FChildren);

  inherited Destroy;
end;

procedure TFEFormContainer.TBackgroundOptions.DoChange;
begin
  if Assigned(FControl) then
  begin
    DoPaletteChange;
    FControl.Invalidate;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TFEFormContainer.TBackgroundOptions.DoPaletteChange;
begin
  const Graphic = Picture.Graphic;
  if
    IsPictureActive                              and
    (not (csLoading in FControl.ComponentState)) and
    Assigned(Graphic)                            and
    (Graphic.PaletteModified)
  then
  begin
    if Graphic.Palette = 0 then
      Graphic.PaletteModified := False
    else
    begin
      const ParentForm = GetParentForm(FControl);
      if Assigned(ParentForm) and ParentForm.Active and ParentForm.HandleAllocated then
      begin
        ParentForm.Handle.SendMessage(WM_QUERYNEWPALETTE, 0, 0);
        Graphic.PaletteModified := False;
      end;
    end;
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.DrawBackground(
  const DC: HDC;
  const Bitmap: TBitmap;
  const Rect: TRect
);
{$MESSAGE WARN 'Not Implemented TFEFormContainer.TBackgroundOptions.DrawBackground'}
  procedure BitBlt(const DestDC, SrcDC: HDC; const LocalRect, DrawRect: TRect; const Rop: DWORD); inline;
  begin
    Winapi.Windows.BitBlt(
      DestDC,
      LocalRect.Left,
      LocalRect.Top,
      LocalRect.Right - LocalRect.Left,
      LocalRect.Bottom - LocalRect.Top,
      SrcDC,
      DrawRect.Left,
      DrawRect.Top,
      Rop
    );
  end;

begin
  with IsInternalActive do
  if Active then
  begin
    const LocalRect = Rect.ResolveRect(FControl);
    var DrawRect := LocalRect;

    const ControlWnd = FControl.Handle;

    if IsScrollBarVisible(ControlWnd, FControl, sbHorizontal) then
    begin
      const ScrollInfo = TScrollInfo.GetHorzScrollInfo(ControlWnd, SIF_POS);
      OffsetRect(DrawRect, ScrollInfo.nPos, 0);
    end;
    if IsScrollBarVisible(ControlWnd, FControl, sbVertical) then
    begin
      const ScrollInfo = TScrollInfo.GetVertScrollInfo(ControlWnd, SIF_POS);
      OffsetRect(DrawRect, 0, ScrollInfo.nPos);
    end;

    var PixelFormat      := TPixelFormat.DevicePixelFormat;

    const LocalRectSize   = TSize.InlineCreate(LocalRect);

    const IsDestroyBitmap = (Bitmap = nil) or (Bitmap.PixelFormat = pfDevice);
    var TempBitmap       := TBitmap.ResolveBitmap(IsDestroyBitmap, Bitmap);

    with TempBitmap.Canvas do
    begin
      Lock;
      try
        if IsDestroyBitmap then
        begin
          AdjustBitmapForTransition(TempBitmap, LocalRectSize, PixelFormat);
          SetWindowOrgEx(Handle, DrawRect.Left, DrawRect.Top, nil);
        end
        else
          PixelFormat := TempBitmap.PixelFormat;

        const ParentForBackgroundForm = ResolveParentWithBackgroundForm(BackgroundFormActive);
        const ParentForPicture        = ResolveParentWithPicture(PictureActive);
        const ParentForGlass          = ResolveParentWithGlass(GlassActive);

        if GlassActive and (ParentForGlass.GlassTranslucency = 0) then
        begin
          Brush.Color := ParentForGlass.GlassColor;
          FillRect(DrawRect);
        end
        else
        begin
          if isXRayActive(ParentForPicture, DrawRect) then
          begin
            DrawXRay(FControl, TempBitmap.Canvas, LocalRect, DrawRect, LocalRectSize, PixelFormat);
          end
          else if BackgroundFormActive then
          begin
            DrawBackgroundForm(
              ParentForBackgroundForm,
              FControl,
              TempBitmap,
              Rect,
              DrawRect,
              LocalRectSize,
              PixelFormat
            );
          end
          else if PictureActive then
          begin
            DrawStandardBackground(ParentForPicture.FControl, Handle, DrawRect, FThemesDisabled)
          end
          else
            DrawStandardBackground(FControl, Handle, DrawRect, FThemesDisabled);

          if PictureActive and (ParentForPicture.FControl is TWinControl) then
          begin
            DrawPicture(
              ParentForPicture.Picture.Graphic,
              ParentForPicture.PictureMode,
              FControl,
              ParentForPicture.FControl,
              ParentForPicture.PictureTransparentColor,
              TempBitmap,
              DrawRect,
              0
            );
          end;

          if GlassActive then
          begin
            BlendBackground(ParentForGlass, Bitmap, IsDestroyBitmap, DrawRect, LocalRectSize, PixelFormat);
          end;
        end;

        if IsDestroyBitmap then
          BitBlt(DC, Handle, LocalRect, DrawRect, cmSrcCopy);
      finally
        Unlock;
        if IsDestroyBitmap then
          FreeAndNil(TempBitmap);
      end;
    end;
  end
  else
    DrawStandardBackground(FControl, DC, Rect, FThemesDisabled);
end;

function TFEFormContainer.TBackgroundOptions.FindParentWithBackgroundForm: TBackgroundOptions;
begin
  if FParentBackgoundForm and Assigned(FParent) then
    Exit(FParent.FindParentWithBackgroundForm);
  Result := Self;
end;

function TFEFormContainer.TBackgroundOptions.FindParentWithGlass: TBackgroundOptions;
begin
  if FParentGlass and Assigned(FParent) then
    Exit(FParent.FindParentWithGlass);
  Result := Self;
end;

function TFEFormContainer.TBackgroundOptions.FindParentWithOpaque: TBackgroundOptions;
begin
  if
    FParentOpaque                        and
    Assigned(FParent)                    and
    Assigned(FControl)                   and
    (FParent.FControl = FControl.Parent)
  then
  begin
    Exit(FParent.FindParentWithOpaque);
  end;
  Result := Self;
end;

function TFEFormContainer.TBackgroundOptions.FindParentWithPicture: TBackgroundOptions;
begin
  if FParentPicture and Assigned(FParent) then
    Exit(FParent.FindParentWithPicture);
  Result := Self;
end;

function TFEFormContainer.TBackgroundOptions.GetBackgoundForm: TCustomForm;
begin
  Result := FindParentWithBackgroundForm.FBackgoundForm;
end;

function TFEFormContainer.TBackgroundOptions.GetGlassColor: TColor;
begin
  Result := FindParentWithOpaque.FGlassColor;
end;

function TFEFormContainer.TBackgroundOptions.GetGlassTranslucency: Byte;
begin
  Result := FindParentWithOpaque.FGlassTranslucency;
end;

function TFEFormContainer.TBackgroundOptions.GetOpaque: Boolean;
begin
  Result := FindParentWithOpaque.FOpaque;
end;

function TFEFormContainer.TBackgroundOptions.GetPalette: HPALETTE;
begin
  if Assigned(FPicture.Graphic) then
    Exit(FPicture.Graphic.Palette);
  Result := 0;
end;

function TFEFormContainer.TBackgroundOptions.GetPicture: TPicture;
begin
  Result := FindParentWithPicture.FPicture;
end;

function TFEFormContainer.TBackgroundOptions.GetPictureMode: TFEBackgroundPictureMode;
begin
  Result := FindParentWithPicture.FPictureMode;
end;

function TFEFormContainer.TBackgroundOptions.GetPictureTransparentColor: TColor;
begin
  Result := FindParentWithPicture.FPictureTransparentColor;
end;

procedure TFEFormContainer.TBackgroundOptions.GlassChange(const Sender: TObject; const Propagate: Boolean);
begin
  if Propagate then
  begin
    for var Child in FChildren do
    begin
      if Child.FParentGlass then
        Child.GlassChange(Self, True);
    end;
  end;

  DoChange;
end;

function TFEFormContainer.TBackgroundOptions.IsActive: Boolean;
begin
  Result := IsInternalActive.Active;
end;

function TFEFormContainer.TBackgroundOptions.IsBackgroundFormActive: Boolean;
begin
  Result := FBackgoundFormVisible and Assigned(BackgoundForm);
end;

function TFEFormContainer.TBackgroundOptions.IsGlassActive: Boolean;
begin
  Result := FGlassVisible and (GlassTranslucency < 255);
end;

function TFEFormContainer.TBackgroundOptions.IsGlassStored: Boolean;
begin
  Result := not FParentGlass;
end;

function TFEFormContainer.TBackgroundOptions.IsInternalActive: TActive;
begin
  if Assigned(FControl) then
    Exit(TActive.Create(IsOpaqueActive, IsBackgroundFormActive, IsPictureActive, IsGlassActive));
  Result := TActive.Create(True, False, False, False);
end;

function TFEFormContainer.TBackgroundOptions.IsOpaqueActive: Boolean;
begin
  Result := Opaque;
end;

function TFEFormContainer.TBackgroundOptions.IsPictureActive: Boolean;
begin
  Result := FPictureVisible;
  if Result then
  begin
    const Graphic = Self.Picture.Graphic;
    Result := Assigned(Graphic) and not Graphic.Empty and (Graphic.Width > 0) and (Graphic.Height > 0);
  end;
end;

function TFEFormContainer.TBackgroundOptions.IsPictureStored: Boolean;
begin
  Result := not FParentPicture;
end;

function TFEFormContainer.TBackgroundOptions.isXRayActive(const BackgroundOptions: TBackgroundOptions;
  const Rect: TRect): Boolean;
begin
{$MESSAGE WARN 'Not Implemented TFEFormContainer.TBackgroundOptions.isXRayActive'}
end;

procedure TFEFormContainer.TBackgroundOptions.OnPictureChange(Sender: TObject);
begin
  PictureChange(Sender, True);
end;

procedure TFEFormContainer.TBackgroundOptions.OpaqueChange(const Sender: TObject);
begin
  for var Child in FChildren do
  begin
    if Child.FParentOpaque then
      Child.OpaqueChange(Self);
  end;

  DoChange;
end;

procedure TFEFormContainer.TBackgroundOptions.PictureChange(const Sender: TObject; const Propagate: Boolean);
begin
  if Propagate then
  begin
    for var Child in FChildren do
    begin
      if Child.FParentPicture then
        Child.PictureChange(Self, True);
    end;
  end;

  DoChange;
end;

procedure TFEFormContainer.TBackgroundOptions.RemoveChild(const Child: TBackgroundOptions);
begin
  FChildren.Remove(Child);
  Child.FParent := nil;
end;

function TFEFormContainer.TBackgroundOptions.ResolveParentWithBackgroundForm(
  const isActive: Boolean): TBackgroundOptions;
begin
  if isActive then
    Exit(FindParentWithBackgroundForm);
  Result := nil;
end;

function TFEFormContainer.TBackgroundOptions.ResolveParentWithGlass(const isActive: Boolean): TBackgroundOptions;
begin
  if isActive then
    Exit(FindParentWithGlass);
  Result := nil;
end;

function TFEFormContainer.TBackgroundOptions.ResolveParentWithPicture(const isActive: Boolean): TBackgroundOptions;
begin
  if isActive then
    Exit(FindParentWithPicture);
  Result := nil;
end;

procedure TFEFormContainer.TBackgroundOptions.SetBackgoundForm(const Value: TCustomFormClass);
begin
  if Assigned(FBackgoundForm) then
    FreeAndNil(FBackgoundForm);

  FParentBackgoundForm := False;
  if Assigned(Value) and Assigned(FControl) then
  begin
    FBackgoundForm             := Value.Create(FControl);
    FBackgoundForm.BorderStyle := bsNone;
    FBackgoundForm.Left        := 0;
    FBackgoundForm.Top         := 0;
    FBackgoundForm.SetBounds(-1, -1, FControl.ClientWidth, FControl.ClientHeight);
    SetWindowRgn(FBackgoundForm.Handle, HRGN.Create(0, 0, 1, 1), False);
    ShowWindow(FBackgoundForm.Handle, SW_SHOWNOACTIVATE);
    FBackgoundForm.Visible := True;
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetBackgoundFormVisible(const Value: Boolean);
begin
  if FBackgoundFormVisible <> Value then
  begin
    FBackgoundFormVisible := Value;
    BackgroundFormChange(Self, False);
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetControl(const Value: TWinControl);
{$MESSAGE WARN 'Not Implemented TFEFormContainer.TBackgroundOptions.SetControl'}
  function GetBackgroundOptions(const RttiContext: TRttiContext; const Control: TControl): TBackgroundOptions; inline;
  begin
    var OptionsProperty: TRttiProperty := nil;
    const ParentControlTypeInfo = RttiContext.GetType(Control.ClassInfo);
    if Assigned(ParentControlTypeInfo) then
    begin
      OptionsProperty := ParentControlTypeInfo.GetProperty('BackgroundOptions');
      if Assigned(OptionsProperty) then
      begin
        const PropertyType = OptionsProperty.PropertyType;
        if Assigned(PropertyType) and (PropertyType.ToString <> TBackgroundOptions.ClassName) then
          OptionsProperty := nil;
      end;
    end;

    if Assigned(OptionsProperty) then
    begin
      const PropertyValue = OptionsProperty.GetValue(Control);
      if not PropertyValue.TryAsType<TBackgroundOptions>(Result) then
        Result := nil;
    end
    else
      Result := TBackgroundOptions(Control.Perform(CM_FEGETBACKGRONDOPTIONS, 0, 0));
  end;

  function GetParentBackgroundOptions(const RttiContext: TRttiContext; const Control: TControl): TBackgroundOptions;
  begin
    var ParentControl: TWinControl := nil;

    if Assigned(Control) then
    begin
      if Assigned(Control.Parent) then
        ParentControl := Control.Parent
      else
      begin
        if Control is TWinControl then
          ParentControl := FindControl((Control as TWinControl).ParentWindow);
        if
          (ParentControl = nil) and
          (Control is TForm)    and
          (TForm(Control).GetProtectedFormStyle = fsMDIChild)
        then
        begin
          ParentControl := Application.MainForm;
        end;
      end;
    end;

    if Assigned(ParentControl) then
    begin
      Result := GetBackgroundOptions(RttiContext, ParentControl);
      if Result = nil then
        Result := GetParentBackgroundOptions(RttiContext, ParentControl);
    end
    else
      Result := nil;
  end;

  procedure CheckChildBackgroundOptions(const RttiContext: TRttiContext; const Control: TWinControl);
  begin
    for var Index := 0 to Control.ControlCount - 1 do
    begin
      const Child = Control.Controls[Index];
      const Options = GetBackgroundOptions(RttiContext, Child);

      if Assigned(Options) then
        AddChild(Options)
      else if Child is TWinControl then
        CheckChildBackgroundOptions(RttiContext, Child as TWinControl);
    end;
  end;

begin
  if FControl <> Value then
  begin
    if Assigned(FParent) then
      FParent.RemoveChild(Self);

    FControl := Value;
    const RttiContext = TRttiContext.Create;
    try
      const ParentBackgroundOptions = GetParentBackgroundOptions(RttiContext, FControl);
      if Assigned(ParentBackgroundOptions) then
        ParentBackgroundOptions.AddChild(Self);

      for var Child in FChildren do
        RemoveChild(Child);
      CheckChildBackgroundOptions(RttiContext, FControl);
    finally
      RttiContext.Free;
    end;
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetGlassColor(const Value: TColor);
begin
  FParentGlass := False;
  FGlassColor  := Value;
  GlassChange(Self, True);
end;

procedure TFEFormContainer.TBackgroundOptions.SetGlassTranslucency(const Value: Byte);
begin
  FParentGlass       := False;
  FGlassTranslucency := Value;
  GlassChange(Self, True);
end;

procedure TFEFormContainer.TBackgroundOptions.SetGlassVisible(const Value: Boolean);
begin
  if FGlassVisible <> Value then
  begin
    FGlassVisible := Value;
    GlassChange(Self, False);
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetOpaque(const Value: Boolean);
begin
  FParentOpaque := False;
  if FOpaque <> Value then
  begin
    FOpaque := Value;
    if Assigned(FControl) then
      FControl.Invalidate;
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetParentBackgoundForm(const Value: Boolean);
begin
  if FParentBackgoundForm <> Value then
  begin
    if Value and Assigned(FBackgoundForm) then
      FreeAndNil(FBackgoundForm);

    FParentBackgoundForm := Value;
    BackgroundFormChange(Self, True);
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetParentGlass(const Value: Boolean);
begin
  if FParentGlass <> Value then
  begin
    FParentGlass := Value;
    GlassChange(Self, True);
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetParentOpaque(const Value: Boolean);
begin
  if FParentOpaque <> Value then
  begin
    FParentOpaque := Value;
    OpaqueChange(Self);
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetParentPicture(const Value: Boolean);
begin
  if FParentPicture <> Value then
  begin
    if Value then
      FPicture.Graphic := nil;
    FParentPicture := Value;
    PictureChange(Self, True);
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetPicture(const Value: TPicture);
begin
  FParentPicture := False;
  FPicture.Assign(Value);
end;

procedure TFEFormContainer.TBackgroundOptions.SetPictureMode(const Value: TFEBackgroundPictureMode);
begin
  if FPictureMode <> Value then
  begin
    FPictureMode := Value;
    PictureChange(Self, True);
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetPictureTransparentColor(const Value: TColor);
begin
  if FPictureTransparentColor <> Value then
  begin
    FPictureTransparentColor := Value;
    PictureChange(Self, True);
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetPictureVisible(const Value: Boolean);
begin
  if FPictureVisible <> Value then
  begin
    FPictureVisible := Value;
    PictureChange(Self, False);
  end;
end;

procedure TFEFormContainer.TBackgroundOptions.SetThemesDisabled(const Value: Boolean);
begin
  if FThemesDisabled <> Value then
  begin
    FThemesDisabled := Value;
    DoChange;
  end;
end;

{ TFEFormContainer.TBackgroundOptions.TActive }

constructor TFEFormContainer.TBackgroundOptions.TActive.Create(
  const OpaqueActive, BackgroundFormActive, PictureActive, GlassActive: Boolean
);
begin
  FOpaqueActive         := OpaqueActive;
  FBackgroundFormActive := BackgroundFormActive;
  FPictureActive        := PictureActive;
  FGlassActive          := GlassActive;
end;

function TFEFormContainer.TBackgroundOptions.TActive.GetActive: Boolean;
begin
  Result := not FOpaqueActive or FBackgroundFormActive or FPictureActive or FGlassActive;
end;

{ TFEFormContainer.TEmbeddedForm }

constructor TFEFormContainer.TEmbeddedForm.Create(Owner: TComponent);
begin
{$MESSAGE WARN 'Not Implemented TFEFormContainer.TEmbeddedForm.Create'}
  inherited Create(Owner);

  FBackgroundOptions         := TFEFormContainer.TBackgroundOptions.Create;
  FBackgroundOptions.Control := Self;
end;

destructor TFEFormContainer.TEmbeddedForm.Destroy;
begin
{$MESSAGE WARN 'Not Implemented TFEFormContainer.TEmbeddedForm.Destroy'}
  FreeAndNil(FBackgroundOptions);

  inherited Destroy;
end;

procedure TFEFormContainer.TEmbeddedForm.SetBackgroundOptions(const Value: TBackgroundOptions);
begin
{$MESSAGE WARN 'Not Implemented TFEFormContainer.TEmbeddedForm.SetBackgroundOptions'}
end;

end.
