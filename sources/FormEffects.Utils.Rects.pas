/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Rects.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Rects;

{$INCLUDE FormEffects.inc}

interface

uses
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
{$IFDEF FORM_EFFECTS_TESTS}
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks,
{$ENDIF ~ FORM_EFFECTS_TESTS}
  FormEffects.Constants;

function PictureRect(
  const Graphic: TGraphic;
  const PictureMode: TBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const Margin: Word;
  out DrawRect: TRect
): TRect;

implementation

uses
  Winapi.Windows,
  FormEffects.TypeHelpers,
  FormEffects.Utils.ScrollBars;

{$REGION 'Internal definitions'}

type

{ TPictureRectHelper }

  TPictureRectHelper = record
  strict private
    FIsMDIOrgControl: Boolean;
    FOrgControlWnd: HWND;
    FThisControlWnd: HWND;
    FOrgControl: TWinControl;
    FThisControl: TWinControl;

  strict private
    function ResolveOrgControl: TWinControl; inline;
    function ResolveOrgControlWnd: HWND; inline;

    function GetOrgControlScrollbarsOffset: TPoint; inline;
    function GetThisControlScrollbarsOffset: TPoint; inline;

  public
    class function Create(const ThisControl, OrgControl: TWinControl): TPictureRectHelper; static; inline;

  public
    property ThisControlWnd: HWND read FThisControlWnd;
    property OrgControlWnd: HWND read FOrgControlWnd;
    property OrgControlScrollbarsOffset: TPoint read GetOrgControlScrollbarsOffset;
    property ThisControlScrollbarsOffset: TPoint read GetThisControlScrollbarsOffset;
  end;

{ TPictureRectHelper }

class function TPictureRectHelper.Create(const ThisControl, OrgControl: TWinControl): TPictureRectHelper;
begin
  with Result do
  begin
    FIsMDIOrgControl := (OrgControl is TCustomForm) and (TCustomForm(OrgControl).GetProtectedFormStyle = fsMDIForm);
    FOrgControl    := OrgControl;
    FOrgControlWnd := ResolveOrgControlWnd;

    if FIsMDIOrgControl and (ThisControl = OrgControl) then
      FThisControlWnd := FOrgControlWnd
    else
      FThisControlWnd := ThisControl.Handle;
    FThisControl := ThisControl;
  end;
end;

function TPictureRectHelper.GetOrgControlScrollbarsOffset: TPoint;
begin
  Result := GetScrollbarsOffset(FOrgControlWnd, ResolveOrgControl, SIF_ALL);
end;

function TPictureRectHelper.GetThisControlScrollbarsOffset: TPoint;
begin
  Result := GetScrollbarsOffset(FThisControlWnd, FThisControl, SIF_POS);
end;

function TPictureRectHelper.ResolveOrgControl: TWinControl;
begin
  if FIsMDIOrgControl then
    Result := nil
  else
    Result := FOrgControl;
end;

function TPictureRectHelper.ResolveOrgControlWnd: HWND;
begin
  if FIsMDIOrgControl then
    Result := TCustomForm(FOrgControl).GetProtectedClientHandle
  else
    Result := FOrgControl.Handle;
end;

{$ENDREGION 'Internal definitions'}

function PictureRect(
  const Graphic: TGraphic;
  const PictureMode: TBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const Margin: Word;
  out DrawRect: TRect
): TRect;
begin
  const GraphicSize = TSize.InlineCreate(Graphic);

  if (GraphicSize.Width = 0) or (GraphicSize.Height = 0) then
    Exit(TRect.Zero);

  with TPictureRectHelper.Create(ThisControl, OrgControl) do
  begin
    DrawRect := OrgControlWnd.GetClientRect;

    var ClientSize := TSize.InlineCreate(DrawRect);

    DrawRect := TRect.InlineCreate(ClientSize);
    DrawRect.InflateRect(-Margin);
    ClientSize := TSize.InlineCreate(DrawRect);

    case PictureMode of
      TBackgroundPictureMode.Center:
      begin
        Result := TRect.InlineCreate(
          (ClientSize.Width  - GraphicSize.Width ) div 2,
          (ClientSize.Height - GraphicSize.Height) div 2,
          GraphicSize,
          Margin
        );
      end;
      TBackgroundPictureMode.CenterStretch:
      begin
        const MaxSize = TSize.InlineCreate(ClientSize, GraphicSize);
        Result := TRect.InlineCreate(
          (ClientSize.Width  - MaxSize.Width ) div 2 + Margin,
          (ClientSize.Height - MaxSize.Height) div 2 + Margin,
          MaxSize
        );
      end;
      TBackgroundPictureMode.Stretch,
      TBackgroundPictureMode.Tile,
      TBackgroundPictureMode.Zoom:
      begin
        Result := DrawRect;
      end;
      TBackgroundPictureMode.TopLeft:
      begin
        Result := TRect.IntersectRects(DrawRect, TRect.InlineCreate(GraphicSize, Margin));
      end;
      TBackgroundPictureMode.TopRight:
      begin
        Result := TRect.InlineCreate(
          ClientSize.Width   - GraphicSize.Width - Margin,
          Margin,
          ClientSize.Width   - Margin,
          GraphicSize.Height + Margin
        );
      end;
      TBackgroundPictureMode.BottomLeft:
      begin
        Result := TRect.InlineCreate(
          Margin,
          ClientSize.Height - GraphicSize.Height - Margin,
          GraphicSize.Width + Margin,
          ClientSize.Height - Margin
        );
      end;
      TBackgroundPictureMode.BottomRight:
      begin
        Result := TRect.InlineCreate(
          ClientSize.Width  - GraphicSize.Width  - Margin,
          ClientSize.Height - GraphicSize.Height - Margin,
          ClientSize.Width  - Margin,
          ClientSize.Height - Margin
        );
      end;
    end;

    if OrgControlWnd <> ThisControlWnd then
    begin
      Result.MapWindowRect(OrgControlWnd, ThisControlWnd);
//      MapWindowPoints(OrgControlWnd, ThisControlWnd, Result, 2);

//      MapWindowPoints(OrgControlWnd, HWND_DESKTOP, Result, 2);
//      MapWindowPoints(HWND_DESKTOP, ThisControlWnd, Result, 2);

//      ClientToScreen(OrgControlWnd, Result.TopLeft);
//      ClientToScreen(OrgControlWnd, Result.BottomRight);
//      ScreenToClient(ThisControlWnd, Result.TopLeft);
//      ScreenToClient(ThisControlWnd, Result.BottomRight);

      Result.OffsetRect(ThisControlScrollbarsOffset - OrgControlScrollbarsOffset);
    end;
  end;
end;

end.
