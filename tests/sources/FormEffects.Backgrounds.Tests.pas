/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Tests.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Vcl.Graphics,
  Vcl.Forms,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Constants,
  FormEffects.Backgrounds,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Vcl.Forms.Mocks;

type

{$IFDEF FORM_EFFECTS_TESTS}
  {$IFDEF USE_BILLENIUM_EFFECTS}

{ TBackgroundOptions }

  TBackgroundOptions = class(TFCBackgroundOptions)
  public
    function GetBackgoundForm: TCustomForm; virtual;
    function GetParentBackgoundForm: Boolean;
    function GetBackgoundFormVisible: Boolean;
    function GetPictureTransparentColor: TColor;
    function GetThemesEnabled: Boolean;
    procedure SetParentBackgoundForm(const Value: Boolean);
    procedure SetBackgoundForm(const Value: TCustomFormClass);
    procedure SetBackgoundFormVisible(const Value: Boolean);
    procedure SetPictureTransparentColor(const Value: TColor);
    procedure SetThemesEnabled(const Value: Boolean);

  public
    property ParentBackgoundForm: Boolean read GetParentBackgoundForm write SetParentBackgoundForm;
    property BackgoundFormVisible: Boolean read GetBackgoundFormVisible write SetBackgoundFormVisible;
    property PictureTransparentColor: TColor read GetPictureTransparentColor write SetPictureTransparentColor;
    property ThemesEnabled: Boolean read GetThemesEnabled write SetThemesEnabled;
  end;

  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
{$ENDIF ~ FORM_EFFECTS_TESTS}

{ TChangeNotifier }

  TChangeNotifier = class abstract
  public
  {$IFDEF USE_BILLENIUM_EFFECTS}
    procedure OnChange(Sender: TObject); virtual; abstract;
  {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
    procedure OnChange(const Sender: TBackgroundOptions); virtual; abstract;
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
  end;

implementation

{$IFDEF FORM_EFFECTS_TESTS}
  {$IFDEF USE_BILLENIUM_EFFECTS}

{ TBackgroundOptions }

function TBackgroundOptions.GetBackgoundForm: TCustomForm;
begin
  Result := BkgrndForm;
end;

function TBackgroundOptions.GetBackgoundFormVisible: Boolean;
begin
  Result := BkgrndFormVisible;
end;

function TBackgroundOptions.GetParentBackgoundForm: Boolean;
begin
  Result := ParentBkgrndForm;
end;

function TBackgroundOptions.GetPictureTransparentColor: TColor;
begin
  Result := PictureTranspColor;
end;

function TBackgroundOptions.GetThemesEnabled: Boolean;
begin
  Result := not ThemesDisabled;
end;

procedure TBackgroundOptions.SetBackgoundForm(const Value: TCustomFormClass);
begin
  SetBkgrndForm(Value);
end;

procedure TBackgroundOptions.SetBackgoundFormVisible(const Value: Boolean);
begin
  BkgrndFormVisible := Value;
end;

procedure TBackgroundOptions.SetParentBackgoundForm(const Value: Boolean);
begin
  ParentBkgrndForm := Value
end;

procedure TBackgroundOptions.SetPictureTransparentColor(const Value: TColor);
begin
  PictureTranspColor := Value;
end;

procedure TBackgroundOptions.SetThemesEnabled(const Value: Boolean);
begin
  ThemesDisabled := not Value;
end;

  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
