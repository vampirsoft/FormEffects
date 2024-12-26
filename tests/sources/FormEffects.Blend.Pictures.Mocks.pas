/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Blend.Pictures.Mocks.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Blend.Pictures.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  System.Types,
  Vcl.Graphics,
  FormEffects.Utils.Mocks,
  FormEffects.Vcl.Graphics.Mocks;

type

  TBlendPicturesMocks = class abstract(TMocksManager)
  public
    procedure BlendBitmap(
      const Bitmap, BrushBitmap: TBitmap;
      const PixelFormat: TPixelFormat;
      const Color: TColor;
      const Rect: TRect;
      const Level: Integer
    ); virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBlndWk
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Blend.Pictures
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  BlendPicturesMocks: TBlendPicturesMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TBlendBitmap =
    procedure(
      Bitmap, BrushBitmap: TBitmap;
      PixelFormat: TPixelFormat;
      Color: TColor;
      Rect: TRect;
      Level: Integer
    );

procedure BlendBitmapMock(
  Bitmap, BrushBitmap: TBitmap;
  PixelFormat: TPixelFormat;
  Color: TColor;
  Rect: TRect;
  Level: Integer
);
begin
  BlendPicturesMocks.BlendBitmap(Bitmap, BrushBitmap, PixelFormat, Color, Rect, Level);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TBlendBitmap =
    procedure(
      const Bitmap, BrushBitmap: TBitmap;
      const PixelFormat: TPixelFormat;
      const Color: TColor;
      const Rect: TRect;
      const Level: Integer
    );

procedure BlendBitmapMock(
  const Bitmap, BrushBitmap: TBitmap;
  const PixelFormat: TPixelFormat;
  const Color: TColor;
  const Rect: TRect;
  const Level: Integer
);
begin
  BlendPicturesMocks.BlendBitmap(Bitmap, BrushBitmap, PixelFormat, Color, Rect, Level);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TBlendPicturesMocks }

constructor TBlendPicturesMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TBlendBitmap>(teBlndWk.BlendBmp, BlendBitmapMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TBlendBitmap>(FormEffects.Blend.Pictures.BlendBitmap, BlendBitmapMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  BlendPicturesMocks := Self;
end;

end.
