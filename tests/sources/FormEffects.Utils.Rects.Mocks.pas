/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Rects.Mocks.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Rects.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
  FormEffects.FormContainer,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Mocks
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
;

type

{ TUtilsRectsMocks }

  TUtilsRectsMocks = class(TMocksManager)
  private type
    TPictureRectResultReference =
      reference to function(
        const Graphic: TGraphic;
        const PictureMode: TFEBackgroundPictureMode;
      {$IFDEF USE_BILLENIUM_EFFECTS}
        const ThisControl: TControl;
        const OrgControl: TWinControl;
      {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
        const ThisControl, OrgControl: TWinControl;
      {$ENDIF ~ USE_BILLENIUM_EFFECTS}
        const Margin: Word;
        var DrawRect: TRect
      ): TRect;
  public
    procedure PictureRect_Result(const DrawRectResult, PictureRectResult: TRect); overload;
    procedure PictureRect_Result(const Callback: TPictureRectResultReference); overload;

  public
    constructor Create; override;
  end;

implementation

uses
  System.Types,
  FormEffects.TypeHelpers
{$IFDEF USE_BILLENIUM_EFFECTS}
  , FormEffects.TypeHelpers.Mocks
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  , FormEffects.Utils.Rects
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  ;

var
  PictureRectResultReference: TUtilsRectsMocks.TPictureRectResultReference = nil;

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
  Result := PictureRectResultReference(Pic, PictureMode.Cast, CtrlThis, CtrlOrg, Margin, DrawRect);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TPictureRect =
    function(
      const Graphic: TGraphic;
      const PictureMode: TFEBackgroundPictureMode;
      const ThisControl, OrgControl: TWinControl;
      const Margin: Word;
      var DrawRect: TRect
    ): TRect;

function PictureRectMock(
  const Graphic: TGraphic;
  const PictureMode: TFEBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const Margin: Word;
  var DrawRect: TRect
): TRect;
begin
  Result := PictureRectResultReference(Graphic, PictureMode, ThisControl, OrgControl, Margin, DrawRect);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TUtilsRectsMocks.PictureRect_Result(const DrawRectResult, PictureRectResult: TRect);
begin
  PictureRect_Result(
    function(
      const Graphic: TGraphic;
      const PictureMode: TFEBackgroundPictureMode;
    {$IFDEF USE_BILLENIUM_EFFECTS}
      const ThisControl: TControl;
      const OrgControl: TWinControl;
    {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
      const ThisControl, OrgControl: TWinControl;
    {$ENDIF ~ USE_BILLENIUM_EFFECTS}
      const Margin: Word;
      var DrawRect: TRect
    ): TRect
    begin
      DrawRect := DrawRectResult;
      Result   := PictureRectResult;
    end
  );
end;

procedure TUtilsRectsMocks.PictureRect_Result(const Callback: TPictureRectResultReference);
begin
  if Assigned(Callback) then
    PictureRectResultReference := Callback
  else
  begin
    PictureRectResultReference :=
      function(
        const Graphic: TGraphic;
        const PictureMode: TFEBackgroundPictureMode;
      {$IFDEF USE_BILLENIUM_EFFECTS}
        const ThisControl: TControl;
        const OrgControl: TWinControl;
      {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
        const ThisControl, OrgControl: TWinControl;
      {$ENDIF ~ USE_BILLENIUM_EFFECTS}
        const Margin: Word;
        var DrawRect: TRect
      ): TRect
      begin
        DrawRect := TRect.Zero;
        Result   := TRect.Zero;
      end;
  end;
end;

{ TUtilsRectsMocks }

constructor TUtilsRectsMocks.Create;
begin
  inherited Create;

  AddIntercept<TPictureRect>(PictureRect, PictureRectMock);
  PictureRect_Result(nil);
end;

end.
