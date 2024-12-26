/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Rects.Mocks.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Rects.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  FormEffects.Constants,
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Utils.Mocks
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

type

{ TUtilsRectsMocks }

  TUtilsRectsMocks = class abstract(TMocksManager)
  public
    function PictureRect(
      const Graphic: TGraphic;
      const PictureMode: TPictureMode;
      const ThisControl: TControl;
      const OrgControl: TWinControl;
      const Margin: Word
    ): TRect; virtual; abstract;
    function PictureDrawRect(
      const Graphic: TGraphic;
      const PictureMode: TPictureMode;
      const ThisControl: TControl;
      const OrgControl: TWinControl;
      const Margin: Word
    ): TRect; virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Rects
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  UtilsRectsMocks: TUtilsRectsMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TPictureRect =
    function(
      Pic: TGraphic;
      PictureMode: TFCPictureMode;
      Margin: Word;
      CtrlThis: TControl;
      CtrlOrg: TWinControl;
      var DrawRect: TRect
    ): TRect;

function PictureRectMock(
  Pic: TGraphic;
  PictureMode: TFCPictureMode;
  Margin: Word;
  CtrlThis: TControl;
  CtrlOrg: TWinControl;
  var DrawRect: TRect
): TRect;
begin
  Result   := UtilsRectsMocks.PictureRect    (Pic, PictureMode, CtrlThis, CtrlOrg, Margin);
  DrawRect := UtilsRectsMocks.PictureDrawRect(Pic, PictureMode, CtrlThis, CtrlOrg, Margin);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TPictureRect =
    function(
      const Graphic: TGraphic;
      const PictureMode: TBackgroundPictureMode;
      const ThisControl, OrgControl: TWinControl;
      const Margin: Word;
      out DrawRect: TRect
    ): TRect;

function PictureRectMock(
  const Graphic: TGraphic;
  const PictureMode: TBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const Margin: Word;
  out DrawRect: TRect
): TRect;
begin
  Result   := UtilsRectsMocks.PictureRect    (Graphic, PictureMode, ThisControl, OrgControl, Margin);
  DrawRect := UtilsRectsMocks.PictureDrawRect(Graphic, PictureMode, ThisControl, OrgControl, Margin);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TUtilsRectsMocks }

constructor TUtilsRectsMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TPictureRect>(teBkgrnd.PictureRect, PictureRectMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TPictureRect>(FormEffects.Utils.Rects.PictureRect, PictureRectMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  UtilsRectsMocks := Self;
end;

end.
