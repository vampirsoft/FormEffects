/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Rendering.pas                      *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Rendering;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  FormEffects.Rendering,
  FormEffects.Utils.Pictures,
  FormEffects.Blend.Pictures
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

{$MESSAGE 'Need to test DrawStandardBackground'}
procedure DrawStandardBackground(
  const WinControl: TWinControl;
  const DC: HDC;
  const Rect: TRect;
  const ThemesEnabled: Boolean
);
procedure DrawXRay(
  const Control: TWinControl;
  const DC: HDC;
  const Rect, DrawRect: TRect
);{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
procedure DrawBackgroundForm(
  const Control, ParentControl: TWinControl;
  const Form: TCustomForm;
  const DC: HDC;
  const Rect, DrawRect: TRect
);{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
{$MESSAGE 'Need to test BlendBackground'}
procedure BlendBackground(
  const Control: TWinControl;
  const GlassTranslucency: Byte;
  const GlassColor: TColor;
  const Bitmap: TBitmap;
  const DC: HDC;
  const PixelFormat: TPixelFormat;
  const BitmapRect: TRect
);{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}

implementation

uses
  Vcl.Themes,
  FormEffects.TypeHelpers,
  FormEffects.Utils.ScrollBars
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Themes.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

procedure DrawStandardBackground(
  const WinControl: TWinControl;
  const DC: HDC;
  const Rect: TRect;
  const ThemesEnabled: Boolean
);
begin
  const StyleService = StyleServices;
  if
    ThemesEnabled               and
    StyleService.Enabled        and
    Assigned(WinControl.Parent) and
    (csParentBackground in WinControl.ControlStyle)
  then
    StyleService.DrawParentBackground(WinControl.Handle, DC, nil, False, @Rect)
  else
  begin
    const Brush = HBRUSH.Create(ColorToRGB(WinControl.GetProtectedColor));
    try
      DC.FillRect(Rect, Brush);
    finally
      Brush.Delete;
    end;
  end;
end;

procedure DrawXRay(const Control: TWinControl; const DC: HDC; const Rect, DrawRect: TRect);
begin
  const Parent     = Control.Parent;
  const ControlWnd = Control.Handle;
  const ParentWnd  = Parent.Handle;

  const TempRect     = Rect;
  const TempDrawRect = DrawRect;

  TempRect.MapWindowRect(ControlWnd, ParentWnd);
  TempDrawRect.ToDeviceRect(DC);

  with DC.CreateClipRgn(True) do
  try
    const DrawRectRegion = TempDrawRect.CreateRectRgn;
    DC.SelectClipRgn(DrawRectRegion);
    DrawRectRegion.Delete;

    const Point = DC.OffsetWindowOrgEx(TempRect.TopLeft - DrawRect.TopLeft);
    try
      const HasUpdateRect = ControlWnd.HasUpdateRect(False);
      try
        RenderWindowToDC(
          ParentWnd,
          ControlWnd,
          Parent,
          DC,
          TempRect,
          True,
          False,
          False
        );
      finally
        if not HasUpdateRect then
          ControlWnd.ValidateRect;
      end;
    finally
      DC.SetWindowOrgEx(Point);
    end;
  finally
    DeleteRegion;
  end;
end;

procedure DrawBackgroundForm(
  const Control, ParentControl: TWinControl;
  const Form: TCustomForm;
  const DC: HDC;
  const Rect, DrawRect: TRect
);
begin
  const ControlWnd = Control.Handle;
  const ParentWnd  = ParentControl.Handle;
  const FormWnd    = Form.Handle;

  var TempRect     := Control.ClientRect;
  var ClientOffset := TPoint.Zero;
  var ClientSize   := TempRect.Size;

  if IsScrollBarVisible(ParentWnd, ParentControl, sbHorizontal) then
  begin
    const ScrollInfo   = TScrollInfo.GetHorzScrollInfo(ParentWnd, SIF_ALL);
    ClientOffset.X    := ScrollInfo.nPos;
    ClientSize.Width  := ScrollInfo.nMax;
  end;
  if IsScrollBarVisible(ParentWnd, ParentControl, sbVertical) then
  begin
    const ScrollInfo   = TScrollInfo.GetVertScrollInfo(ParentWnd, SIF_ALL);
    ClientOffset.Y    := ScrollInfo.nPos;
    ClientSize.Height := ScrollInfo.nMax;
  end;

  TempRect := ParentControl.ClientToScreen(TRect.InlineCreate(ClientSize));
  TempRect.OffsetRect(-ClientOffset);
  if not TempRect.IsEqual(Form.BoundsRect) then
    Form.BoundsRect := TempRect;

  TempRect := Rect;
  TempRect.MapWindowRect(ControlWnd, FormWnd);
  if Control <> ParentControl then
    TempRect.OffsetRect(ClientOffset - GetScrollbarsOffset(ControlWnd, Control, SIF_POS));

  const TempDrawRect = DrawRect;
  TempDrawRect.ToDeviceRect(DC);
  with DC.CreateClipRgn(True) do
  try
    const DrawRectRegion = TempDrawRect.CreateRectRgn;
    DC.SelectClipRgn(DrawRectRegion);
    DrawRectRegion.Delete;

    const Point = DC.OffsetWindowOrgEx(TempRect.TopLeft - Rect.TopLeft - ClientOffset);
    try
      RenderWindowToDC(
        FormWnd,
        0,
        Form,
        DC,
        TempRect,
        True,
        True,
        False
      );
    finally
      DC.SetWindowOrgEx(Point);
    end;
  finally
    DeleteRegion;
  end;
end;

procedure BlendBackground(
  const Control: TWinControl;
  const GlassTranslucency: Byte;
  const GlassColor: TColor;
  const Bitmap: TBitmap;
  const DC: HDC;
  const PixelFormat: TPixelFormat;
  const BitmapRect: TRect
);
begin
  if PixelFormat <> pf8bit then
  begin
    if not BitmapRect.IsEmptyRect then
      BlendBitmap(Bitmap, nil, PixelFormat, GlassColor, BitmapRect, GlassTranslucency);

    Exit;
  end;

  const BrushBitmap = CreateBitmapFactory;
  const BrushCanvas = BrushBitmap.Canvas;
  try
    BrushCanvas.Lock;

    BrushBitmap.Width      := 8;
    BrushBitmap.Height     := 8;
    BrushBitmap.Monochrome := True;

    var ParentControl := Control.Parent;
    while ParentControl.Parent <> nil do
      ParentControl := ParentControl.Parent;

    const BrushAlign = Control.ClientToScreen(BitmapRect.TopLeft);
    BrushAlign.Offset(-ParentControl.Left, -ParentControl.Top);

    const Point = DC.SetBrushOrgEx(-BrushAlign);
    try
      BlendBitmap(Bitmap, BrushBitmap, PixelFormat, GlassColor, BitmapRect, Round(GlassTranslucency * 63 / 255));
    finally
      DC.SetBrushOrgEx(Point);
    end;
  finally
    BrushCanvas.Unlock;
    FreeAndNilBitmap(BrushBitmap);
  end;
end;

end.
