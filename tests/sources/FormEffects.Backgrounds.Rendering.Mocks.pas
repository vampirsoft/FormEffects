/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Rendering.Mocks.pas                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Rendering.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  FormEffects.Utils.Mocks
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

type

{ TBackgroundsRenderingMocks }

  TBackgroundsRenderingMocks = class abstract(TMocksManager)
  public
    procedure DrawStandardBackground(
      const Control: TControl;
      const DC: HDC;
      const Rect: TRect;
      const ThemesEnabled: Boolean
    ); virtual; abstract;
    procedure DrawXRay(
      const Control: TControl;
      const DC: HDC;
      const Rect, DrawRect: TRect
    ); virtual; abstract;
    procedure DrawBackgroundForm(
      const Control, ParentControl: TControl;
      const Form: TCustomForm;
      const DC: HDC;
      const Rect, DrawRect: TRect
    ); virtual; abstract;
    procedure BlendBackground(
      const Control: TControl;
      const GlassTranslucency: Byte;
      const GlassColor: TColor;
      const Bitmap: TBitmap;
      const DC: HDC;
      const PixelFormat: TPixelFormat;
      const BitmapRect: TRect
    ); virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Backgrounds.Rendering
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  BackgroundsRenderingMocks: TBackgroundsRenderingMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
{$IFDEF FORM_EFFECTS_TESTS}
  type
    TDrawStandardBackground =
      procedure(Control: TFCControl; DC: HDC; Rect: TRect; ThemesDisabled: Boolean);

  procedure DrawStandardBackgroundMock(Control: TFCControl; DC: HDC; Rect: TRect; ThemesDisabled: Boolean);
  begin
    BackgroundsRenderingMocks.DrawStandardBackground(Control, DC, Rect, not ThemesDisabled);
  end;
{$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TDrawStandardBackground =
    procedure(const WinControl: TWinControl; const DC: HDC; const Rect: TRect; const ThemesEnabled: Boolean);

procedure DrawStandardBackgroundMock(
  const WinControl: TWinControl;
  const DC: HDC;
  const Rect: TRect;
  const ThemesEnabled: Boolean
);
begin
  BackgroundsRenderingMocks.DrawStandardBackground(WinControl, DC, Rect, ThemesEnabled);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TDrawXRay =
    procedure(
      Options: TFCBackgroundOptions;
      var Bitmap: TBitmap;
      Rect, DrawRect: TRect;
      BmpWidth, BmpHeight: Integer;
      PixelFormat: TPixelFormat
    );

procedure DrawXRayMock(
  Options: TFCBackgroundOptions;
  var Bitmap: TBitmap;
  Rect, DrawRect: TRect;
  BmpWidth, BmpHeight: Integer;
  PixelFormat: TPixelFormat
);
begin
  BackgroundsRenderingMocks.DrawXRay(Options.Control, Bitmap.Canvas.Handle, Rect, DrawRect);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TDrawXRay = procedure(const Control: TWinControl; const DC: HDC; const Rect, DrawRect: TRect);

procedure DrawXRayMock(const Control: TWinControl; const DC: HDC; const Rect, DrawRect: TRect);
begin
  BackgroundsRenderingMocks.DrawXRay(Control, DC, Rect, DrawRect);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TDrawBackgroundForm =
    procedure(
      Options: TFCBackgroundOptions;
      Control: TControl;
      var Bitmap: TBitmap;
      Rect, DrawRect: TRect;
      BmpWidth, BmpHeight: Integer;
      PixelFormat: TPixelFormat
    );

procedure DrawBackgroundFormMock(
  Options: TFCBackgroundOptions;
  Control: TControl;
  var Bitmap: TBitmap;
  Rect, DrawRect: TRect;
  BmpWidth, BmpHeight: Integer;
  PixelFormat: TPixelFormat
);
begin
  BackgroundsRenderingMocks.DrawBackgroundForm(
    Control,
    Options.Control,
    Options.BkgrndForm,
    Bitmap.Canvas.Handle,
    Rect,
    DrawRect
  );
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TDrawBackgroundForm =
    procedure(
      const Control, ParentControl: TWinControl;
      const Form: TCustomForm;
      const DC: HDC;
      const Rect, DrawRect: TRect
    );

procedure DrawBackgroundFormMock(
  const Control, ParentControl: TWinControl;
  const Form: TCustomForm;
  const DC: HDC;
  const Rect, DrawRect: TRect
);
begin
  BackgroundsRenderingMocks.DrawBackgroundForm(Control, ParentControl, Form, DC, Rect, DrawRect);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TBlendBackground =
    procedure(
      Options: TFCBackgroundOptions;
      Bitmap: TBitmap;
      LocalBmp: Boolean;
      R: TRect;
      RWidth, RHeight: Integer;
      PixelFormat: TPixelFormat
    );

procedure BlendBackgroundMock(
  Options: TFCBackgroundOptions;
  Bitmap: TBitmap;
  LocalBmp: Boolean;
  R: TRect;
  RWidth, RHeight: Integer;
  PixelFormat: TPixelFormat
);
begin
  BackgroundsRenderingMocks.BlendBackground(
    Options.Control,
    Options.GlassTranslucency,
    Options.GlassColor,
    Bitmap,
    Bitmap.Canvas.Handle,
    PixelFormat,
    R
  );
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TBlendBackground =
    procedure(
      const Control: TWinControl;
      const GlassTranslucency: Byte;
      const GlassColor: TColor;
      const Bitmap: TBitmap;
      const DC: HDC;
      const PixelFormat: TPixelFormat;
      const BitmapRect: TRect
    );

procedure BlendBackgroundMock(
  const Control: TWinControl;
  const GlassTranslucency: Byte;
  const GlassColor: TColor;
  const Bitmap: TBitmap;
  const DC: HDC;
  const PixelFormat: TPixelFormat;
  const BitmapRect: TRect
);
begin
  BackgroundsRenderingMocks.BlendBackground(
    Control,
    GlassTranslucency,
    GlassColor,
    Bitmap,
    DC,
    PixelFormat,
    BitmapRect
  );
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TBackgroundsRenderingMocks }

constructor TBackgroundsRenderingMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TDrawStandardBackground>(teBkgrnd.DrawStandardBackground, DrawStandardBackgroundMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TDrawStandardBackground>(
    FormEffects.Backgrounds.Rendering.DrawStandardBackground,
    DrawStandardBackgroundMock
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TDrawXRay>(teBkgrnd.DrawXRay, DrawXRayMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TDrawXRay>(FormEffects.Backgrounds.Rendering.DrawXRay, DrawXRayMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TDrawBackgroundForm>(teBkgrnd.DrawBkgrndForm, DrawBackgroundFormMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TDrawBackgroundForm>(FormEffects.Backgrounds.Rendering.DrawBackgroundForm, DrawBackgroundFormMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TBlendBackground>(teBkgrnd.BlendBkgrnd, BlendBackgroundMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TBlendBackground>(FormEffects.Backgrounds.Rendering.BlendBackground, BlendBackgroundMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  BackgroundsRenderingMocks := Self;
end;

end.
