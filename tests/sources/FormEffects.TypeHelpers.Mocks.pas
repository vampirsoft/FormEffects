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
  Vcl.Controls,
  Vcl.ToolWin,
  Vcl.Themes,
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

{ TEdgeStyleHelper }

  TEdgeStyleHelper = record helper for TEdgeStyle
  public
    function AsValue: TValue; inline;
  end;

{ TEdgeBordersHelper }

  TEdgeBordersHelper = record helper for TEdgeBorders
  public
    function AsValue: TValue; inline;
  end;

  TThemedElementDetailsHelper = record helper for TThemedElementDetails
  public
    function AsValue: TValue; inline;
  end;

{ TTControlStyleHelper }

  TTControlStyleHelper = record helper for TControlStyle
  public
    function AsValue: TValue; inline;
  end;

{ TBevelKindHelper }

  TBevelKindHelper = record helper for TBevelKind
  public
    function AsValue: TValue; inline;
  end;

{ TBevelCutHelper }

  TBevelCutHelper = record helper for TBevelCut
  public
    function AsValue: TValue; inline;
  end;

{ TBevelEdgesHelper }

  TBevelEdgesHelper = record helper for TBevelEdges
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
  Result := TValue.From<TFormStyle>(Self);
end;

{ TWindowStateHelper }

function TWindowStateHelper.AsValue: TValue;
begin
  Result := TValue.From<TWindowState>(Self);
end;

{ TEdgeStyleHelper }

function TEdgeStyleHelper.AsValue: TValue;
begin
  Result := TValue.From<TEdgeStyle>(Self);
end;

{ TEdgeBordersHelper }

function TEdgeBordersHelper.AsValue: TValue;
begin
  Result := TValue.From<TEdgeBorders>(Self);
end;

{ TThemedElementDetailsHelper }

function TThemedElementDetailsHelper.AsValue: TValue;
begin
  Result := TValue.From<TThemedElementDetails>(Self);
end;

{ TTControlStyleHelper }

function TTControlStyleHelper.AsValue: TValue;
begin
  Result := TValue.From<TControlStyle>(Self);
end;

{ TBevelKindHelper }

function TBevelKindHelper.AsValue: TValue;
begin
  Result := TValue.From<TBevelKind>(Self);
end;

{ TBevelCutHelper }

function TBevelCutHelper.AsValue: TValue;
begin
  Result := TValue.From<TBevelCut>(Self);
end;

{ TBevelEdgesHelper }

function TBevelEdgesHelper.AsValue: TValue;
begin
  Result := TValue.From<TBevelEdges>(Self);
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
