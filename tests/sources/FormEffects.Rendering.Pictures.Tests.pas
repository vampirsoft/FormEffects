/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Pictures.Tests.pas                   *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Pictures.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Vcl.Graphics, Vcl.Controls,
  FormEffects.FormContainer,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks;

type

{$IFDEF USE_BILLENIUM_EFFECTS}

{ TFEBackgroundPictureModeHelper }

  TFEBackgroundPictureModeHelper = record helper for TFEBackgroundPictureMode
  public
    function GetPictureMode: TFCPictureMode;
  end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TPicturesRenderingTests }

  [TestFixture]
  TPicturesRenderingTests = class
  strict private
    FGraphic: TGraphic;
    FBitmap: TBitmap;
    FControl: TControl;
    FWinControl: TWinControl;

  strict private
    procedure DrawPicture(const PictureMode: TFEBackgroundPictureMode; const TransparentColor: TColor;
      const Rect: TRect); inline;

  public
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('for Center',        'Center')]
    [TestCase('for CenterStretch', 'CenterStretch')]
    [TestCase('for Stretch',       'Stretch')]
    [TestCase('for Tile',          'Tile')]
    [TestCase('for Zoom',          'Zoom')]
    [TestCase('for TopLeft',       'TopLeft')]
    [TestCase('for TopRight',      'TopRight')]
    [TestCase('for BottomLeft',    'BottomLeft')]
    [TestCase('for BottomRight',   'BottomRight')]
    procedure DrawPicture_Should_Draw(const PictureMode: TFEBackgroundPictureMode);
  end;

implementation

uses
  System.SysUtils;

{$IFDEF USE_BILLENIUM_EFFECTS}

{ TFEBackgroundPictureModeHelper }

function TFEBackgroundPictureModeHelper.GetPictureMode: TFCPictureMode;
begin
  case Self of
    TFEBackgroundPictureMode.Center       : Exit(fcpmCenter);
    TFEBackgroundPictureMode.CenterStretch: Exit(fcpmCenterStretch);
    TFEBackgroundPictureMode.Stretch      : Exit(fcpmStretch);
    TFEBackgroundPictureMode.Zoom         : Exit(fcpmZoom);
    TFEBackgroundPictureMode.TopLeft      : Exit(fcpmTopLeft);
    TFEBackgroundPictureMode.TopRight     : Exit(fcpmTopRight);
    TFEBackgroundPictureMode.BottomLeft   : Exit(fcpmBottomLeft);
    TFEBackgroundPictureMode.BottomRight  : Exit(fcpmBottomRight);
    else Result := fcpmTile;
  end;
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TPicturesRenderingTests }

procedure TPicturesRenderingTests.DrawPicture(const PictureMode: TFEBackgroundPictureMode;
  const TransparentColor: TColor; const Rect: TRect);
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd.DrawPicture(FGraphic, PictureMode.GetPictureMode, TransparentColor, FWinControl, FBitmap, Rect, 84, FControl);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures.DrawPicture(
    FGraphic,
    PictureMode,
    FWinControl,
    FControl,
    TransparentColor,
    FBitmap,
    Rect,
    84
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TPicturesRenderingTests.DrawPicture_Should_Draw(const PictureMode: TFEBackgroundPictureMode);
begin
  FGraphic := TGraphic.Create;
  FBitmap  := TBitmap.Create;

  DrawPicture(PictureMode, clNone, TRect.Create(279, 215, 977, 746));

  case PictureMode of
    TFEBackgroundPictureMode.Center,
    TFEBackgroundPictureMode.TopLeft,
    TFEBackgroundPictureMode.TopRight,
    TFEBackgroundPictureMode.BottomLeft,
    TFEBackgroundPictureMode.BottomRight:
    begin
      Assert.AreEqual<TArray<TRect>>(
        [
          TRect.Create(279, 215, -1, -1)
        ],
        FBitmap.Canvas.DrawRects
      );
    end;

    TFEBackgroundPictureMode.Stretch,
    TFEBackgroundPictureMode.CenterStretch:
    begin
      Assert.AreEqual<TArray<TRect>>(
        [
          TRect.Create(279, 215, 977, 746)
        ],
        FBitmap.Canvas.DrawRects
      );
    end;

    TFEBackgroundPictureMode.Tile:
    begin
      Assert.AreEqual<TArray<TRect>>(
        [
          TRect.Create(279, 215, -1, -1),
          TRect.Create(279, 609, -1, -1),
          TRect.Create(828, 215, -1, -1),
          TRect.Create(828, 609, -1, -1)
        ],
        FBitmap.Canvas.DrawRects
      );
    end;

    TFEBackgroundPictureMode.Zoom:
    begin
      Assert.AreEqual<TArray<TRect>>(
        [
          TRect.Create(258, 215, 998, 746)
        ],
        FBitmap.Canvas.DrawRects
      );
    end;
  end;
end;

procedure TPicturesRenderingTests.TearDown;
begin
  if Assigned(FWinControl) then FreeAndNil(FWinControl);
  if Assigned(FControl) then FreeAndNil(FControl);
  if Assigned(FBitmap) then FreeAndNil(FBitmap);
  if Assigned(FGraphic) then FreeAndNil(FGraphic);
end;

initialization
  TDUnitX.RegisterTestFixture(TPicturesRenderingTests);

end.
