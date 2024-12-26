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
  Winapi.Windows,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  FormEffects.FormContainer, FormEffects.TypeHelpers, FormEffects.Utils.ScrollBars
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Winapi.Windows.Mocks
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Forms.Mocks
  , FormEffects.Utils.ScrollBars.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

function PictureRect(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const WinControl: TWinControl; const Control: TControl; var DrawRect: TRect; const Margin: Word): TRect;

implementation

{$REGION 'Internal definitions'}

type

{ TPictureRectHelper }

  TPictureRectHelper = record
  strict private
    FIsMDIWinControl: Boolean;
    FWinControlHandle: HWND;
    FControlHandle: HWND;
    FWinControl: TWinControl;

  public
    class function Create(const WinControl: TWinControl; const Control: TControl): TPictureRectHelper; static; inline;

  public
    function ResolveWinControl: TWinControl; inline;

  public
    property ControlHandle: HWND read FControlHandle;
    property WinControlHandle: HWND read FWinControlHandle;
  end;

{ TPictureRectHelper }

class function TPictureRectHelper.Create(const WinControl: TWinControl; const Control: TControl): TPictureRectHelper;
begin
  with Result do
  begin
    FWinControl := WinControl;

    FIsMDIwinControl := (WinControl is TCustomForm) and ((WinControl as TCustomForm).GetInternalFormStyle = fsMDIForm);

    FWinControlHandle := WinControl.GetInternalHandle(FIsMDIwinControl);

    if FIsMDIwinControl and (Control = WinControl) then FControlHandle := FWinControlHandle
    else FControlHandle := Control.GetWinControlHandle;
  end;
end;

function TPictureRectHelper.ResolveWinControl: TWinControl;
begin
  if FIsMDIwinControl then Exit(nil);
  Result := FWinControl;
end;

{$ENDREGION 'Internal definitions'}

function PictureRect(const Graphic: TGraphic; const PictureMode: TFEBackgroundPictureMode;
  const WinControl: TWinControl; const Control: TControl; var DrawRect: TRect; const Margin: Word): TRect;
begin
  const GraphicSize = TSize.InlineCreate(Graphic);

  if (GraphicSize.Width = 0) or (GraphicSize.Height = 0) then Exit(TRect.Zero);

  with TPictureRectHelper.Create(WinControl, Control) do
  begin
    DrawRect := TRect.CreateClientRect(WinControlHandle);

    var ClientSize := TSize.InlineCreate(DrawRect);

    var HorizontalOffset := 0;
    var VerticalOffset   := 0;

    if IsScrollBarVisible(ResolveWinControl, WinControlHandle, sbHorizontal) then
    begin
      const ScrollInfo  = TScrollInfo.GetHorzScrollInfo(WinControlHandle, SIF_ALL);
      HorizontalOffset := ScrollInfo.nPos;
    end;
    if IsScrollBarVisible(ResolveWinControl, WinControlHandle, sbVertical) then
    begin
      const ScrollInfo = TScrollInfo.GetVertScrollInfo(WinControlHandle, SIF_ALL);
      VerticalOffset  := ScrollInfo.nPos;
    end;

    DrawRect := TRect.InlineCreate(0, 0, ClientSize.Width, ClientSize.Height);
    DrawRect.Inflate(-Margin);
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
        IntersectRect(
          Result,
          DrawRect,
          TRect.InlineCreate(0, 0, GraphicSize, Margin)
        );
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

    if WinControlHandle <> ControlHandle then
    begin
      Result.MapWindow(WinControlHandle, ControlHandle);
//      MapWindowPoints(WinControlHandle, ControlHandle, Result, 2);

//      MapWindowPoints(WinControlHandle, HWND_DESKTOP, Result, 2);
//      MapWindowPoints(HWND_DESKTOP, ControlHandle, Result, 2);

//      ClientToScreen(WinControlHandle, Result.TopLeft);
//      ClientToScreen(WinControlHandle, Result.BottomRight);
//      ScreenToClient(ControlHandle,    Result.TopLeft);
//      ScreenToClient(ControlHandle,    Result.BottomRight);
      OffsetRect(Result, -HorizontalOffset, -VerticalOffset);

      if IsScrollBarVisible(Control, ControlHandle, sbHorizontal) then
      begin
        const ScrollInfo = TScrollInfo.GetHorzScrollInfo(ControlHandle, SIF_POS);
        OffsetRect(Result, ScrollInfo.nPos, 0);
      end;

      if IsScrollBarVisible(Control, ControlHandle, sbVertical) then
      begin
        const ScrollInfo = TScrollInfo.GetVertScrollInfo(ControlHandle, SIF_POS);
        OffsetRect(Result, 0, ScrollInfo.nPos);
      end;
    end;
  end;
end;

end.
