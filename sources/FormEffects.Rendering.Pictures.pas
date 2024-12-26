/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Pictures.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Pictures;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  FormEffects.TypeHelpers,
  FormEffects.FormContainer,
  FormEffects.Utils.ScrollBars,
  FormEffects.Utils.Rects
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Graphics.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

type
  TPaintClientReference = reference to procedure(const DC: HDC);

procedure AdjustBitmap(
  const DC: HDC;
  const ClientSize: TSize;
  const PaintClientCallback: TPaintClientReference
); overload;

procedure DrawPicture(
  const Graphic: TGraphic;
  const PictureMode: TFEBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const TransparentColor: TColor;
  const Bitmap: TBitmap;
  const Rect: TRect;
  const Margin: Word
); overload; inline;
procedure DrawPicture(
  const Graphic: TGraphic;
  const PictureMode: TFEBackgroundPictureMode;
  const TransparentColor: TColor;
  const Bitmap: TBitmap;
  const Rect, GraphicRect, DrawRect: TRect;
  const Margin: Word
); overload;

{$IFDEF FORM_EFFECTS_TESTS}
function CreateBitmapFactory: TBitmap;
{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.Types,
  System.SysUtils,
  System.Math,
  FormEffects.Utils.Pictures;

function CreateBitmapFactory: TBitmap;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
begin
  Result := TBitmap.Create;
end;

procedure AdjustBitmap(const DC: HDC; const ClientSize: TSize; const PaintClientCallback: TPaintClientReference);
begin
  const Bitmap = CreateBitmapFactory;
  try
    const Canvas = Bitmap.Canvas;
    Canvas.Lock;
    try
      const CanvasDC = Canvas.Handle;
      AdjustBitmapForTransition(Bitmap, ClientSize, pfDevice);
      PaintClientCallback(CanvasDC);
      CanvasDC.BitBlt(DC, ClientSize);
    finally
      Canvas.Unlock;
    end;
  finally
  {$IFNDEF FORM_EFFECTS_TESTS}
    FreeAndNil(Bitmap);
  {$ENDIF ~ FORM_EFFECTS_TESTS}
  end;
end;

{$REGION 'Internal definitions'}

type

{ TCanvasHelper }

  TCanvasHelper = class helper for TCanvas
  public
    procedure Tile(const Rect, PictureRect: TRect; const Graphic: TGraphic); inline;
    procedure Zoom(const PictureRect: TRect; const Graphic: TGraphic); inline;
  end;

{ TDrawPictureHelper }

  TDrawPictureHelper = record
  strict private
    FUseClipRgn: Boolean;
    FDC: HDC;
    FRect: TRect;
    FClipRgn: THRGNHelper.TClipRegin;

  public
    class function Create(const DC: HDC; const UseClipRgn: Boolean; Rect: TRect): TDrawPictureHelper; inline; static;

  public
    procedure Delete; inline;
    procedure IntersectClip; inline;
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
  if (Rect.Bottom - TileStart.Y) mod GraphicSize.Height <> 0 then
    Inc(Rows);

  var Columns := (Rect.Right  - TileStart.X) div GraphicSize.Width;
  if (Rect.Right - TileStart.X) mod GraphicSize.Width <> 0 then
    Inc(Columns);

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

  procedure RecalculateRect(
    const PictureRectSideSize, GraphicSideSize, GraphicAnotherSideSize: Integer;
    var PictureRectSideStart, PictureRectSideEnd: FixedInt
  ); inline;
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

{ TDrawPictureHelper }

class function TDrawPictureHelper.Create(const DC: HDC; const UseClipRgn: Boolean; Rect: TRect): TDrawPictureHelper;
begin
  with Result do
  begin
    FUseClipRgn := UseClipRgn;
    FDC         := DC;
    FRect       := Rect;

    if UseClipRgn then
      FClipRgn := DC.CreateClipRgn(True);
  end;
end;

procedure TDrawPictureHelper.Delete;
begin
  if not FUseClipRgn then
    Exit;

  FClipRgn.DeleteRegion;
end;

procedure TDrawPictureHelper.IntersectClip;
begin
  if FUseClipRgn then
    FDC.IntersectClip(FRect);
end;

{$ENDREGION 'Internal definitions'}

procedure DrawPicture(
  const Graphic: TGraphic;
  const PictureMode: TFEBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const TransparentColor: TColor;
  const Bitmap: TBitmap;
  const Rect: TRect;
  const Margin: Word
);
var
  GraphicRect: TRect;
  DrawRect: TRect;

begin
  if Assigned(ThisControl) and Assigned(OrgControl) then
  begin
    GraphicRect := PictureRect(Graphic, PictureMode, ThisControl, OrgControl, Margin, DrawRect);

    const TempRect = TRect.IntersectRects(DrawRect, Rect);
    if TempRect.IsEmptyRect then
      Exit;
  end
  else
  begin
    GraphicRect  := Rect;
    DrawRect := Rect;
    DrawRect.InflateRect(-Margin);
  end;

  DrawPicture(Graphic, PictureMode, TransparentColor, Bitmap, Rect, GraphicRect, DrawRect, Margin);
end;

procedure DrawPicture(
  const Graphic: TGraphic;
  const PictureMode: TFEBackgroundPictureMode;
  const TransparentColor: TColor;
  const Bitmap: TBitmap;
  const Rect, GraphicRect, DrawRect: TRect;
  const Margin: Word
);
begin
  if (Graphic = nil) or (Bitmap = nil) or GraphicRect.IsEmptyRect then
    Exit;

  if TransparentColor = clNone then
    Graphic.Transparent := False
  else
  begin
    Graphic.Transparent := True;
    if Graphic is TBitmap then
      (Graphic as TBitmap).TransparentColor := TransparentColor;
  end;

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

  const Canvas = Bitmap.Canvas;

  with TDrawPictureHelper.Create(Canvas.Handle, UseClipRgn, DrawRect) do
  try
    IntersectClip;

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
    Delete;
  end;
end;

end.
