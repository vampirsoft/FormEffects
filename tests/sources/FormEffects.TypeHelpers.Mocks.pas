/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.TypeHelpers.Mocks.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.TypeHelpers.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  System.UITypes,
  System.Rtti,
  Vcl.Forms,
  FormEffects.FormContainer
{$IFDEF USE_BILLENIUM_EFFECTS}
  , teBkgrnd
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
 ;

type

{ TFormStyleHelper }

  TFormStyleHelper = record helper for TFormStyle
  public
    function AsValue: TValue; inline;
  end;

{ TWindowStateHelper }

  TWindowStateHelper = record helper for TWindowState
  public
    function AsValue: TValue; inline;
  end;

{$IFDEF USE_BILLENIUM_EFFECTS}

{ TFCPictureModeHelper }

  TFCPictureModeHelper = record helper for TFCPictureMode
  public
    function Cast: TFEBackgroundPictureMode;
  end;

{ TFEBackgroundPictureModeHelper }

  TFEBackgroundPictureModeHelper = record helper for TFEBackgroundPictureMode
  public
    function Cast: TFCPictureMode;
  end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

implementation

{ TFormStyleHelper }

function TFormStyleHelper.AsValue: TValue;
begin
  TValue.Make<TFormStyle>(Self, Result);
end;

{ TWindowStateHelper }

function TWindowStateHelper.AsValue: TValue;
begin
  TValue.Make<TWindowState>(Self, Result);
end;

{$IFDEF USE_BILLENIUM_EFFECTS}

{ TFCPictureModeHelper }

function TFCPictureModeHelper.Cast: TFEBackgroundPictureMode;
begin
  case Self of
    fcpmCenter       : Exit(TFEBackgroundPictureMode.Center);
    fcpmCenterStretch: Exit(TFEBackgroundPictureMode.CenterStretch);
    fcpmStretch      : Exit(TFEBackgroundPictureMode.Stretch);
    fcpmZoom         : Exit(TFEBackgroundPictureMode.Zoom);
    fcpmTopLeft      : Exit(TFEBackgroundPictureMode.TopLeft);
    fcpmTopRight     : Exit(TFEBackgroundPictureMode.TopRight);
    fcpmBottomLeft   : Exit(TFEBackgroundPictureMode.BottomLeft);
    fcpmBottomRight  : Exit(TFEBackgroundPictureMode.BottomRight);
    else Result := TFEBackgroundPictureMode.Tile;
  end;
end;

{ TFEBackgroundPictureModeHelper }

function TFEBackgroundPictureModeHelper.Cast: TFCPictureMode;
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

end.
