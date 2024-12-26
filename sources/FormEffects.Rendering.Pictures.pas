/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Pictures.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Pictures;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  FormEffects.TypeHelpers, FormEffects.FormContainer, FormEffects.Utils.ScrollBars, FormEffects.Utils.Rects
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Graphics.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

procedure DrawPicture(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const WinControl: TWinControl; const Control: TControl; const TransparentColor: TColor; const Bitmap: TBitmap;
  const Rect: TRect; const Margin: Word); overload; inline;
procedure DrawPicture(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const TransparentColor: TColor; const Bitmap: TBitmap; const Rect, GraphicRect, DrawRect: TRect;
  const Margin: Word); overload;

implementation

uses
  System.Types, System.Math;

{$REGION Internal definitions}

type

{ TCanvasHelper }

  TCanvasHelper = class helper for TCanvas
  public
    procedure Tile(const Rect, PictureRect: TRect; const Graphic: TGraphic); inline;
    procedure Zoom(const PictureRect: TRect; const Graphic: TGraphic); inline;
  end;

{ TCanvasHelper }

procedure TCanvasHelper.Tile(const Rect, PictureRect: TRect; const Graphic: TGraphic);
begin
  const GraphicSize = TSize.InlineCreate(Graphic);

  const TileStart   = TPoint.InlineCreate(
    Rect.Left - ((Rect.Left - PictureRect.Left) mod GraphicSize.Width ),
    Rect.Top  - ((Rect.Top  - PictureRect.Top ) mod GraphicSize.Height)
  );

  var Rows    := (Rect.Bottom - TileStart.Y) div GraphicSize.Height;
  if (Rect.Bottom - TileStart.Y) mod GraphicSize.Height <> 0 then Inc(Rows);

  var Columns := (Rect.Right  - TileStart.X) div GraphicSize.Width;
  if Rect.Right - TileStart.X mod GraphicSize.Width  <> 0 then Inc(Columns);

  var PosX := TileStart.X;
  for var I := 0 to Columns - 1 do
  begin
    var PosY := TileStart.Y;
    for var J := 0 to Rows - 1 do
    begin
      Draw(PosX, PosY, Graphic);
      Inc(PosY, GraphicSize.Height);
    end;
    Inc(PosX, GraphicSize.Width);
  end;
end;

procedure TCanvasHelper.Zoom(const PictureRect: TRect; const Graphic: TGraphic);

  procedure RecalculateRect(const PictureRectSideSize, GraphicSideSize, GraphicAnotherSideSize: Integer;
    var PictureRectSideStart, PictureRectSideEnd: FixedInt); inline;
  begin
    const ZoomLevel = PictureRectSideSize / GraphicSideSize;
    const NewSize   = Ceil(GraphicAnotherSideSize * ZoomLevel);
    const Delta     = (NewSize - (PictureRectSideEnd - PictureRectSideStart)) div 2;

    PictureRectSideStart  := PictureRectSideStart - Delta;
    PictureRectSideEnd    := PictureRectSideEnd + (NewSize - (PictureRectSideEnd - PictureRectSideStart));
  end;

begin
  const GraphicSize     = TSize.InlineCreate(Graphic);
  const PictureRectSize = TSize.InlineCreate(PictureRect);

  var FullPictureRect := PictureRect;

  const Ratio = GraphicSize.Width / GraphicSize.Height;
  if Ratio > PictureRectSize.Width / PictureRectSize.Height
  then // Zoomed picture is wider than the target canvas
  begin
    RecalculateRect(
      PictureRectSize.Height,
      GraphicSize.Height,
      GraphicSize.Width,
      FullPictureRect.Left,
      FullPictureRect.Right
    );
  end
  else // Zoomed picture is taller than the target canvas
  begin
    RecalculateRect(
      PictureRectSize.Width,
      GraphicSize.Width,
      GraphicSize.Height,
      FullPictureRect.Top,
      FullPictureRect.Bottom
    );
  end;

  StretchDraw(FullPictureRect, Graphic);
end;

{$ENDREGION Internal definitions}

procedure DrawPicture(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const WinControl: TWinControl; const Control: TControl; const TransparentColor: TColor; const Bitmap: TBitmap;
  const Rect: TRect; const Margin: Word);
var
  GraphicRect: TRect;
  DrawRect: TRect;

begin
  if Assigned(Control) and Assigned(WinControl) then
  begin
    GraphicRect := PictureRect(Graphic, PictureMode, WinControl, Control, DrawRect, Margin);

    var TempRect: TRect;
    IntersectRect(TempRect, DrawRect, Rect);
    if IsRectEmpty(TempRect) then Exit;
  end
  else
  begin
    GraphicRect  := Rect;
    DrawRect := Rect;
    InflateRect(DrawRect, -Margin, -Margin);
  end;

  DrawPicture(Graphic, PictureMode, TransparentColor, Bitmap, Rect, GraphicRect, DrawRect, Margin);
end;

procedure DrawPicture(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const TransparentColor: TColor; const Bitmap: TBitmap; const Rect, GraphicRect, DrawRect: TRect;
  const Margin: Word);
begin
  if (Graphic = nil) or (Bitmap = nil) or IsRectEmpty(GraphicRect) then Exit;

  if TransparentColor = clNone then Graphic.Transparent := False
  else
  begin
    Graphic.Transparent := True;
    if Graphic is TBitmap then (Graphic as TBitmap).TransparentColor := TransparentColor;
  end;

  with Bitmap do
  begin
    const UseClipRgn =
      (Margin <> 0) and
      (PictureMode in [
        TFEBackgroundPictureMode.Center,
        TFEBackgroundPictureMode.Tile,
        TFEBackgroundPictureMode.Zoom,
        TFEBackgroundPictureMode.TopLeft,
        TFEBackgroundPictureMode.TopRight,
        TFEBackgroundPictureMode.BottomLeft,
        TFEBackgroundPictureMode.BottomRight
      ]);

    var SaveClipRgn: HRGN;
    var ExistsClipRgn: Boolean;
    if UseClipRgn then begin
      // Remember current clipping region
      SaveClipRgn   := CreateRectRgn(0, 0, 0, 0);
      ExistsClipRgn := GetClipRgn(Canvas.Handle, SaveClipRgn) = 1;
    end
    else
    begin
      SaveClipRgn   := 0;
      ExistsClipRgn := False;
    end;

    try
      if UseClipRgn then IntersectClipRect(Canvas.Handle, DrawRect.Left, DrawRect.Top, DrawRect.Right, DrawRect.Bottom);

      case PictureMode of
        TFEBackgroundPictureMode.Center,
        TFEBackgroundPictureMode.TopLeft,
        TFEBackgroundPictureMode.TopRight,
        TFEBackgroundPictureMode.BottomLeft,
        TFEBackgroundPictureMode.BottomRight: Canvas.Draw(GraphicRect.Left, GraphicRect.Top, Graphic);

        TFEBackgroundPictureMode.Stretch,
        TFEBackgroundPictureMode.CenterStretch: Canvas.StretchDraw(GraphicRect, Graphic);

        TFEBackgroundPictureMode.Tile: Canvas.Tile(Rect, GraphicRect, Graphic);

        TFEBackgroundPictureMode.Zoom: Canvas.Zoom(GraphicRect, Graphic);
      end;
    finally
      if UseClipRgn then
      begin
        if ExistsClipRgn then SelectClipRgn(Canvas.Handle, SaveClipRgn) else SelectClipRgn(Canvas.Handle, 0);
        DeleteObject(SaveClipRgn);
      end;
    end;
  end;
end;

end.
