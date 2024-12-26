/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Rects.Mocks.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Rects.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  FormEffects.FormContainer, FormEffects.Vcl.Graphics.Mocks, FormEffects.Vcl.Controls.Mocks;

function PictureRect(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const WinControl: TWinControl; const Control: TControl; var DrawRect: TRect; const Margin: Word): TRect;

implementation

function PictureRect(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const WinControl: TWinControl; const Control: TControl; var DrawRect: TRect; const Margin: Word): TRect;
begin

end;

end.
