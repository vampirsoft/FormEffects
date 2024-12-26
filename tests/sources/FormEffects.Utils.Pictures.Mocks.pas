/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Pictures.Mocks.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Pictures.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  Vcl.Graphics,
  FormEffects.Utils.Mocks,
  FormEffects.Vcl.Graphics.Mocks;

type

{ TUtilsPicturesMocks }

  TUtilsPicturesMocks = class abstract(TMocksManager)
  public
    function CreateBitmapFactory: TBitmap; virtual; abstract;
    procedure FreeAndNilBitmap(const Bitmap: TBitmap); virtual; abstract;
    procedure AdjustBitmapForTransition(
      const Bitmap: TBitmap;
      const Palette: HPALETTE;
      const Size: TSize;
      const PixelFormat: TPixelFormat
    ); virtual; abstract;
    function GetDevicePixelFormat: TPixelFormat; virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Pictures
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  UtilsPicturesMocks: TUtilsPicturesMocks;

type
  TCreateBitmapFactory = function: TBitmap;

function CreateBitmapFactoryMock: TBitmap;
begin
  Result := UtilsPicturesMocks.CreateBitmapFactory;
end;

type
  TFreeAndNilBitmap = procedure(const [ref] Bitmap: TBitmap);

procedure FreeAndNilBitmapMock(const [ref] Bitmap: TBitmap);
begin
  UtilsPicturesMocks.FreeAndNilBitmap(Bitmap);
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TAdjustBitmapForTransition =
    procedure(
      Bmp: TBitmap;
      Palette: HPalette;
      Width, Height: Integer;
      PixelFormat: TPixelFormat
    );

procedure AdjustBitmapForTransitionMock(
  Bmp: TBitmap;
  Palette: HPalette;
  Width, Height: Integer;
  PixelFormat: TPixelFormat
);
begin
  UtilsPicturesMocks.AdjustBitmapForTransition(Bmp, Palette, TSize.Create(Width, Height), PixelFormat);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TAdjustBitmapForTransition =
    procedure(
      const Bitmap: TBitmap;
      const Palette: HPALETTE;
      const Size: TSize;
      const PixelFormat: TPixelFormat
    );

procedure AdjustBitmapForTransitionMock(
  const Bitmap: TBitmap;
  const Palette: HPALETTE;
  const Size: TSize;
  const PixelFormat: TPixelFormat
);
begin
  UtilsPicturesMocks.AdjustBitmapForTransition(Bitmap, Palette, Size, PixelFormat);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TGetDevicePixelFormat = function(Recalculate: Boolean): TPixelFormat;

function GetDevicePixelFormatMock(Recalculate: Boolean): TPixelFormat;
begin
  Result := UtilsPicturesMocks.GetDevicePixelFormat;
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TGetDevicePixelFormat = function: TPixelFormat;

function GetDevicePixelFormatMock: TPixelFormat;
begin
  Result := UtilsPicturesMocks.GetDevicePixelFormat;
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TUtilsPicturesMocks }

constructor TUtilsPicturesMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TCreateBitmapFactory>(teRender.CreateBitmapFactory, CreateBitmapFactoryMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TCreateBitmapFactory>(FormEffects.Utils.Pictures.CreateBitmapFactory, CreateBitmapFactoryMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TFreeAndNilBitmap>(teRender.FreeAndNilBitmap, FreeAndNilBitmapMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TFreeAndNilBitmap>(FormEffects.Utils.Pictures.FreeAndNilBitmap, FreeAndNilBitmapMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  {$IFDEF FORM_EFFECTS_TESTS}
    AddIntercept<TAdjustBitmapForTransition>(AdjustBmpForTransition, AdjustBitmapForTransitionMock);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TAdjustBitmapForTransition>(
    FormEffects.Utils.Pictures.AdjustBitmapForTransition,
    AdjustBitmapForTransitionMock
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetDevicePixelFormat>(DevicePixelFormat, GetDevicePixelFormatMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetDevicePixelFormat>(FormEffects.Utils.Pictures.GetDevicePixelFormat, GetDevicePixelFormatMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  UtilsPicturesMocks := Self;
end;

end.
