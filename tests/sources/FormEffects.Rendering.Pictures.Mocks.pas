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
  Vcl.Graphics, Vcl.Forms,
  FormEffects.FormContainer,
  FormEffects.Vcl.Controls.Mocks, FormEffects.Vcl.Graphics.Mocks;

procedure DrawPicture(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const WinControl: TWinControl; const Control: TControl; const TransparentColor: TColor; const Bitmap: TBitmap;
  const Rect: TRect; const Margin: Word); overload;
procedure DrawPicture(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const TransparentColor: TColor; const Bitmap: TBitmap; const Rect, PictureRect, DrawRect: TRect;
  const Margin: Word); overload;

implementation

procedure DrawPicture(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const WinControl: TWinControl; const Control: TControl; const TransparentColor: TColor; const Bitmap: TBitmap;
  const Rect: TRect; const Margin: Word);
begin

end;

procedure DrawPicture(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const TransparentColor: TColor; const Bitmap: TBitmap; const Rect, PictureRect, DrawRect: TRect;
  const Margin: Word);
begin

end;

end.
