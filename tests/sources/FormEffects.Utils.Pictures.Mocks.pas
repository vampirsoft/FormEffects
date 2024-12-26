/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Pictures.Mocks.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Pictures.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Vcl.Graphics,
  FormEffects.Utils.Mocks
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;


type
  TUtilsPicturesMocks = class(TMocksManager)
  private type
    TAdjustBitmapForTransitionResultReference =
      reference to procedure(
        const Bitmap: TBitmap;
        const Palette: HPALETTE;
        const Size: TSize;
        const PixelFormat: TPixelFormat
      );
  public
    procedure AdjustBitmapForTransition_Result(const Callback: TAdjustBitmapForTransitionResultReference);

  public
    constructor Create; override;
  end;

implementation

uses
  System.Types,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Pictures
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  AdjustBitmapForTransitionResultReference: TUtilsPicturesMocks.TAdjustBitmapForTransitionResultReference = nil;

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
  AdjustBitmapForTransitionResultReference(Bmp, Palette, TSize.InlineCreate(Width, Height), PixelFormat);
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
  AdjustBitmapForTransitionResultReference(Bitmap, Palette, Size, PixelFormat);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TUtilsPicturesMocks.AdjustBitmapForTransition_Result(
  const Callback: TAdjustBitmapForTransitionResultReference
);
begin
  if Assigned(Callback) then
    AdjustBitmapForTransitionResultReference := Callback
  else
  begin
    AdjustBitmapForTransitionResultReference :=
      procedure(const Bitmap: TBitmap; const Palette: HPALETTE; const Size: TSize; const PixelFormat: TPixelFormat)
      begin
      end;
  end;
end;

{ TUtilsPicturesMocks }

constructor TUtilsPicturesMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TAdjustBitmapForTransition>(AdjustBmpForTransition, AdjustBitmapForTransitionMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TAdjustBitmapForTransition>(AdjustBitmapForTransition, AdjustBitmapForTransitionMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  AdjustBitmapForTransition_Result(nil);
end;

end.
