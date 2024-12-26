/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Pictures.Mocks.pas                   *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Pictures.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  FormEffects.FormContainer,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Mocks
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Graphics.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

type

{ TRenderingPicturesMocks }

  TRenderingPicturesMocks = class(TMocksManager)
  private type
    TDrawPictureExtendedResultReference =
      reference to procedure(
        const Graphic: TGraphic;
        const PictureMode: TFEBackgroundPictureMode;
      {$IFDEF USE_BILLENIUM_EFFECTS}
        const ThisControl: TControl;
        const OrgControl: TWinControl;
      {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
        const ThisControl, OrgControl: TWinControl;
      {$ENDIF ~ USE_BILLENIUM_EFFECTS}
        const TransparentColor: TColor;
        const Bitmap: TBitmap;
        const Rect: TRect;
        const Margin: Word
      );
    TDrawPictureResultReference =
      reference to procedure(
        const Graphic: TGraphic;
        const PictureMode: TFEBackgroundPictureMode;
        const TransparentColor: TColor;
        const Bitmap: TBitmap;
        const Rect, PictureRect, DrawRect: TRect;
        const Margin: Word
      );
  public
    procedure DrawPicture_Result(const Callback: TDrawPictureExtendedResultReference); overload;
    procedure DrawPicture_Result(const Callback: TDrawPictureResultReference); overload;

  private type
    TCreateBitmapFactoryResultReference = reference to function: TBitmap;
  public
    procedure CreateBitmapFactory_Result(const CreateBitmapFactoryResult: TBitmap);

  public
    constructor Create; override;
  end;

implementation

uses
  System.SysUtils,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
  FormEffects.TypeHelpers.Mocks
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  DrawPictureExtendedResultReference: TRenderingPicturesMocks.TDrawPictureExtendedResultReference = nil;

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
  DrawPictureExtendedResultReference(Pic, PictureMode.Cast, Ctrl, PicCtrl, PictureTranspColor, Bmp, R, Margin);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TDrawPictureExtended =
    procedure(
      const Graphic: TGraphic;
      const PictureMode: TFEBackgroundPictureMode;
      const ThisControl, OrgControl: TWinControl;
      const TransparentColor: TColor;
      const Bitmap: TBitmap;
      const Rect: TRect;
      const Margin: Word
    );

procedure DrawPictureMock(
  const Graphic: TGraphic;
  const PictureMode: TFEBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const TransparentColor: TColor;
  const Bitmap: TBitmap;
  const Rect: TRect;
  const Margin: Word
); overload;
begin
  DrawPictureExtendedResultReference(
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

procedure TRenderingPicturesMocks.DrawPicture_Result(const Callback: TDrawPictureExtendedResultReference);
begin
  DrawPictureExtendedResultReference := Callback;
end;

var
  DrawPictureResultReference: TRenderingPicturesMocks.TDrawPictureResultReference = nil;

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
  DrawPictureResultReference(Pic, PictureMode.Cast, PictureTranspColor, Bmp, R, PicRect, DrawRect, Margin);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TDrawPicture =
    procedure(
      const Graphic: TGraphic;
      const PictureMode: TFEBackgroundPictureMode;
      const TransparentColor: TColor;
      const Bitmap: TBitmap;
      const Rect, PictureRect, DrawRect: TRect;
      const Margin: Word
    );

procedure DrawPictureMock(
  const Graphic: TGraphic;
  const PictureMode: TFEBackgroundPictureMode;
  const TransparentColor: TColor;
  const Bitmap: TBitmap;
  const Rect, PictureRect, DrawRect: TRect;
  const Margin: Word
); overload;
begin
  DrawPictureResultReference(Graphic, PictureMode, TransparentColor, Bitmap, Rect, PictureRect, DrawRect, Margin);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TRenderingPicturesMocks.DrawPicture_Result(const Callback: TDrawPictureResultReference);
begin
  DrawPictureResultReference := Callback;
end;

var
  CreateBitmapFactoryResultReference: TRenderingPicturesMocks.TCreateBitmapFactoryResultReference = nil;

type
  TCreateBitmapFactory = function: TBitmap;

function CreateBitmapFactoryMock: TBitmap;
begin
  Result := CreateBitmapFactoryResultReference;
end;

procedure TRenderingPicturesMocks.CreateBitmapFactory_Result(const CreateBitmapFactoryResult: TBitmap);
begin
  CreateBitmapFactoryResultReference :=
    function: TBitmap
    begin
      Result := CreateBitmapFactoryResult;
    end
end;

{ TRenderingPicturesMocks }

constructor TRenderingPicturesMocks.Create;
begin
  inherited Create;

  AddIntercept<TDrawPictureExtended>(DrawPicture, DrawPictureMock);
  DrawPicture_Result(
    procedure (
      const Graphic: TGraphic;
      const PictureMode: TFEBackgroundPictureMode;
    {$IFDEF USE_BILLENIUM_EFFECTS}
      const ThisControl: TControl;
      const OrgControl: TWinControl;
    {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
      const ThisControl, OrgControl: TWinControl;
    {$ENDIF ~ USE_BILLENIUM_EFFECTS}
      const TransparentColor: TColor;
      const Bitmap: TBitmap;
      const Rect: TRect;
      const Margin: Word
    )
    begin
    end
  );

  AddIntercept<TDrawPicture>(DrawPicture, DrawPictureMock);
  DrawPicture_Result(
    procedure (
      const Graphic: TGraphic;
      const PictureMode: TFEBackgroundPictureMode;
      const TransparentColor: TColor;
      const Bitmap: TBitmap;
      const Rect, PictureRect, DrawRect: TRect;
      const Margin: Word
    )
    begin
    end
  );

{$IFDEF FORM_EFFECTS_TESTS}
  AddIntercept<TCreateBitmapFactory>(CreateBitmapFactory, CreateBitmapFactoryMock);
{$ENDIF FORM_EFFECTS_TESTS}
  CreateBitmapFactory_Result(nil);
end;

end.
