/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Pictures.Mocks.pas                   *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Pictures.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  System.Types,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  FormEffects.Constants,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Utils.Mocks
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Graphics.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

type

{ TRenderingPicturesMocks }

  TRenderingPicturesMocks = class abstract(TMocksManager)
  public
    procedure DrawPicture(
      const Graphic: TGraphic;
      const PictureMode: TPictureMode;
      const ThisControl: TControl;
      const OrgControl: TWinControl;
      const TransparentColor: TColor;
      const Bitmap: TBitmap;
      const Rect: TRect;
      const Margin: Word
    ); overload; virtual; abstract;
    procedure DrawPicture(
      const Graphic: TGraphic;
      const PictureMode: TPictureMode;
      const TransparentColor: TColor;
      const Bitmap: TBitmap;
      const Rect, PictureRect, DrawRect: TRect;
      const Margin: Word
    ); overload; virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
  System.SysUtils,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  RenderingPicturesMocks: TRenderingPicturesMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TDrawPictureExtended =
    procedure(
      Pic: TGraphic;
      PictureMode: TFCPictureMode;
      PictureTranspColor: TColor;
      PicCtrl: TWinControl;
      Bmp: TBitmap;
      R: TRect;
      Margin: Word;
      Ctrl: TControl
    );

procedure DrawPictureMock(
  Pic: TGraphic;
  PictureMode: TFCPictureMode;
  PictureTranspColor: TColor;
  PicCtrl: TWinControl;
  Bmp: TBitmap;
  R: TRect;
  Margin: Word;
  Ctrl: TControl
); overload;
begin
  RenderingPicturesMocks.DrawPicture(Pic, PictureMode, Ctrl, PicCtrl, PictureTranspColor, Bmp, R, Margin);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TDrawPictureExtended =
    procedure(
      const Graphic: TGraphic;
      const PictureMode: TBackgroundPictureMode;
      const ThisControl, OrgControl: TWinControl;
      const TransparentColor: TColor;
      const Bitmap: TBitmap;
      const Rect: TRect;
      const Margin: Word
    );

procedure DrawPictureMock(
  const Graphic: TGraphic;
  const PictureMode: TBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const TransparentColor: TColor;
  const Bitmap: TBitmap;
  const Rect: TRect;
  const Margin: Word
); overload;
begin
  RenderingPicturesMocks.DrawPicture(
    Graphic,
    PictureMode,
    ThisControl,
    OrgControl,
    TransparentColor,
    Bitmap,
    Rect,
    Margin
  );
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TDrawPicture =
    procedure(
      Pic: TGraphic;
      PictureMode: TFCPictureMode;
      PictureTranspColor: TColor;
      Bmp: TBitmap;
      R, PicRect,
      DrawRect: TRect;
      Margin: Word
    );

procedure DrawPictureMock(
  Pic: TGraphic;
  PictureMode: TFCPictureMode;
  PictureTranspColor: TColor;
  Bmp: TBitmap;
  R, PicRect, DrawRect: TRect;
  Margin: Word
); overload;
begin
  RenderingPicturesMocks.DrawPicture(Pic, PictureMode, PictureTranspColor, Bmp, R, PicRect, DrawRect, Margin);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TDrawPicture =
    procedure(
      const Graphic: TGraphic;
      const PictureMode: TBackgroundPictureMode;
      const TransparentColor: TColor;
      const Bitmap: TBitmap;
      const Rect, PictureRect, DrawRect: TRect;
      const Margin: Word
    );

procedure DrawPictureMock(
  const Graphic: TGraphic;
  const PictureMode: TBackgroundPictureMode;
  const TransparentColor: TColor;
  const Bitmap: TBitmap;
  const Rect, PictureRect, DrawRect: TRect;
  const Margin: Word
); overload;
begin
  RenderingPicturesMocks.DrawPicture(
    Graphic,
    PictureMode,
    TransparentColor,
    Bitmap,
    Rect,
    PictureRect,
    DrawRect,
    Margin
  );
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TRenderingPicturesMocks }

constructor TRenderingPicturesMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TDrawPictureExtended>(teBkgrnd.DrawPicture, DrawPictureMock);
  AddIntercept<TDrawPicture>(teBkgrnd.DrawPicture, DrawPictureMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TDrawPictureExtended>(FormEffects.Rendering.Pictures.DrawPicture, DrawPictureMock);
  AddIntercept<TDrawPicture>(FormEffects.Rendering.Pictures.DrawPicture, DrawPictureMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  RenderingPicturesMocks := Self;
end;

end.
