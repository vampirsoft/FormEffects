/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Rects.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
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
  FormEffects.TypeHelpers,
  FormEffects.Utils.ScrollBars,
  FormEffects.FormContainer
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

function PictureRect(
  const Graphic: TGraphic;
  const PictureMode: TFEBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const Margin: Word;
  var DrawRect: TRect
): TRect;

implementation

uses
  Winapi.Windows;

{$REGION 'Internal definitions'}

type

{ TPictureRectHelper }

  TPictureRectHelper = record
  strict private
    FIsMDIOrgControl: Boolean;
    FOrgControlWnd: HWND;
    FThisControlWnd: HWND;
    FOrgControl: TWinControl;

  strict private
    function GetHorizontalOffset: Integer; inline;
    function GetVerticalOffset: Integer; inline;
    function ResolveOrgControl: TWinControl; inline;
    function ResolveOrgControlWnd: HWND; inline;

  public
    class function Create(const ThisControl, OrgControl: TWinControl): TPictureRectHelper; static; inline;

  public
    property ThisControlWnd: HWND read FThisControlWnd;
    property OrgControlWnd: HWND read FOrgControlWnd;
    property HorizontalOffset: Integer read GetHorizontalOffset;
    property VerticalOffset: Integer read GetVerticalOffset;
  end;

{ TPictureRectHelper }

class function TPictureRectHelper.Create(const ThisControl, OrgControl: TWinControl): TPictureRectHelper;
begin
  with Result do
  begin
    FIsMDIOrgControl :=
      (OrgControl is TCustomForm) and
      ((OrgControl as TCustomForm).GetProtectedFormStyle = fsMDIForm);
    FOrgControl    := OrgControl;
    FOrgControlWnd := ResolveOrgControlWnd;

    if FIsMDIOrgControl and (ThisControl = OrgControl) then
      FThisControlWnd := FOrgControlWnd
    else
      FThisControlWnd := ThisControl.Handle;
  end;
end;

function TPictureRectHelper.GetHorizontalOffset: Integer;
begin
  if IsScrollBarVisible(OrgControlWnd, ResolveOrgControl, sbHorizontal) then
  begin
    Exit(TScrollInfo.GetHorzScrollInfo(OrgControlWnd, SIF_ALL).nPos);
  end;
  Result := 0;
end;

function TPictureRectHelper.GetVerticalOffset: Integer;
begin
  if IsScrollBarVisible(OrgControlWnd, ResolveOrgControl, sbVertical) then
  begin
    Exit(TScrollInfo.GetVertScrollInfo(OrgControlWnd, SIF_ALL).nPos);
  end;
  Result := 0;
end;

function TPictureRectHelper.ResolveOrgControl: TWinControl;
begin
  if FIsMDIOrgControl then
    Exit(nil);
  Result := FOrgControl;
end;

function TPictureRectHelper.ResolveOrgControlWnd: HWND;
begin
  if FIsMDIOrgControl then
    Exit((FOrgControl as TCustomForm).GetProtectedClientHandle);
  Result := FOrgControl.Handle;
end;

{$ENDREGION 'Internal definitions'}

function PictureRect(
  const Graphic: TGraphic;
  const PictureMode: TFEBackgroundPictureMode;
  const ThisControl, OrgControl: TWinControl;
  const Margin: Word;
  var DrawRect: TRect
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
      TFEBackgroundPictureMode.Center:
      begin
        Result := TRect.InlineCreate(
          (ClientSize.Width  - GraphicSize.Width ) div 2,
          (ClientSize.Height - GraphicSize.Height) div 2,
          GraphicSize,
          Margin
        );
      end;
      TFEBackgroundPictureMode.CenterStretch:
      begin
        const MaxSize = TSize.InlineCreate(ClientSize, GraphicSize);
        Result := TRect.InlineCreate(
          (ClientSize.Width  - MaxSize.Width ) div 2 + Margin,
          (ClientSize.Height - MaxSize.Height) div 2 + Margin,
          MaxSize
        );
      end;
      TFEBackgroundPictureMode.Stretch,
      TFEBackgroundPictureMode.Tile,
      TFEBackgroundPictureMode.Zoom:
      begin
        Result := DrawRect;
      end;
      TFEBackgroundPictureMode.TopLeft:
      begin
        Result := TRect.IntersectRects(DrawRect, TRect.InlineCreate(GraphicSize, Margin));
      end;
      TFEBackgroundPictureMode.TopRight:
      begin
        Result := TRect.InlineCreate(
          ClientSize.Width   - GraphicSize.Width - Margin,
          Margin,
          ClientSize.Width   - Margin,
          GraphicSize.Height + Margin
        );
      end;
      TFEBackgroundPictureMode.BottomLeft:
      begin
        Result := TRect.InlineCreate(
          Margin,
          ClientSize.Height - GraphicSize.Height - Margin,
          GraphicSize.Width + Margin,
          ClientSize.Height - Margin
        );
      end;
      TFEBackgroundPictureMode.BottomRight:
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
      OrgControlWnd.MapWindowRect(ThisControlWnd, Result);
//      MapWindowPoints(OrgControlWnd, ThisControlWnd, Result, 2);

//      MapWindowPoints(OrgControlWnd, HWND_DESKTOP, Result, 2);
//      MapWindowPoints(HWND_DESKTOP, ThisControlWnd, Result, 2);

//      ClientToScreen(OrgControlWnd, Result.TopLeft);
//      ClientToScreen(OrgControlWnd, Result.BottomRight);
//      ScreenToClient(ThisControlWnd,    Result.TopLeft);
//      ScreenToClient(ThisControlWnd,    Result.BottomRight);

      Result.OffsetRect(-HorizontalOffset, -VerticalOffset);

      if IsScrollBarVisible(ThisControlWnd, ThisControl, sbHorizontal) then
      begin
        const ScrollInfo = TScrollInfo.GetHorzScrollInfo(ThisControlWnd, SIF_POS);
        Result.OffsetRect(ScrollInfo.nPos, 0);
      end;

      if IsScrollBarVisible(ThisControlWnd, ThisControl, sbVertical) then
      begin
        const ScrollInfo = TScrollInfo.GetVertScrollInfo(ThisControlWnd, SIF_POS);
        Result.OffsetRect(0, ScrollInfo.nPos);
      end;
    end;
  end;
end;

end.
