/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.TypeHelpers.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.TypeHelpers;

{$INCLUDE FormEffects.inc}

interface

uses
  System.Types,
  Winapi.Windows,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Winapi.Windows.Mocks
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

type

{ TPixelFormatHelper }

  TPixelFormatHelper = record helper for TPixelFormat
  public
    class function DevicePixelFormat: TPixelFormat; static;
  end;

{ TScrollInfoHelper }

  TScrollInfoHelper = record helper for TScrollInfo
  public
    class function Create(const Mask: UINT): TScrollInfo; inline; static;
    /// <summary>
    ///   Invoke GetScrollInfo
    /// </summary>
    class function GetInfo(const Wnd: HWND; const BarFlag: Integer; const Mask: UINT): TScrollInfo; inline; static;
    /// <summary>
    ///   Invoke GetScrollInfo for SB_HORZ bar flag
    /// </summary>
    class function GetHorzScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo; inline; static;
    /// <summary>
    ///   Invoke GetScrollInfo for SB_VERT bar flag
    /// </summary>
    class function GetVertScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo; inline; static;

  public
    /// <summary>
    ///   Invoke GetScrollInfo
    /// </summary>
    procedure LoadInfo(const Wnd: HWND; const BarFlag: Integer); inline;
  end;

{ TSizeHelper }

  TSizeHelper = record helper for TSize
  public
    class function Zero: TSize; static; inline;
    class function InlineCreate(const Width, Height: Integer): TSize; overload; static; inline;
    class function InlineCreate(const Graphic: TGraphic): TSize; overload; static; inline;
    class function InlineCreate(const ClientSize, GraphicSize: TSize): TSize; overload; static; inline;
    class function InlineCreate(const Rect: TRect): TSize; overload; static; inline;
  end;

{ TPointHelper }

  TPointHelper = record helper for TPoint
  public
    class function InlineCreate(const X, Y : Integer): TPoint; overload; static; inline;

  public
    procedure SetLocationInline(const X, Y : Integer); overload; inline;
    procedure SetLocationInline(const Point : TPoint); overload; inline;
    function SubtractInline(const Point: TPoint): TPoint; inline;
    /// <summary>
    ///   Invoke MapWindowPoints
    /// </summary>
    function MapWindow(const FromWnd, ToWnd: HWND): Integer; inline;
    /// <summary>
    ///   Invoke MapWindowPoints
    /// </summary>
    function MapWindowToScreen(const FromWnd: HWND): Integer; inline;
//    /// <summary>
//    ///   Invoke MapWindowPoints
//    /// </summary>
//    function MapWindowFromScreen(const ToWnd: HWND): Integer; inline;
  end;

{ TRectHelper }

  TRectHelper = record helper for TRect
  public
    class function Zero: TRect; static; inline;
    class function InlineCreate(const Left, Top, Right, Bottom: Integer): TRect; overload; static; inline;
//    class function InlineCreate(const Point: TPoint; const Width, Height: Integer): TRect; overload; static; inline;
    class function InlineCreate(const Left, Top: Integer; const Size: TSize): TRect; overload; static; inline;
    class function InlineCreate(const Left, Top: Integer; const Size: TSize;
      const Margin: Word): TRect; overload; static; inline;
// Specific constructors;
    /// <summary>
    ///   Invoke GetClientRect
    /// </summary>
    class function CreateClientRect(const Wnd: HWND): TRect; static; inline;
    /// <summary>
    ///   Invoke GetWindowRect
    /// </summary>
    class function CreateWindowRect(const Wnd: HWND): TRect; static; inline;
    /// <summary>
    ///   Invoke CreateRectRgn
    /// </summary>
    class function CreateRgn(const Left, Top, Right, Bottom: Integer): HRGN; overload; static; inline;

  public
    /// <summary>
    ///   Return Rect as Left, Top, Right, Bottom of Control if self is empty, otherwise - self
    /// </summary>
    function ResolveRect(const Control: TControl): TRect; inline;
    /// <summary>
    ///   Invoke ExcludeClipRect
    /// </summary>
    function ExcludeClip(const DC: HDC): Integer; inline;
    /// <summary>
    ///   Invoke IntersectClipRect
    /// </summary>
    function IntersectClip(const DC: HDC): Integer; inline;
    /// <summary>
    ///   Invoke CreateRectRgn
    /// </summary>
    function CreateRgn: HRGN; overload; inline;
    /// <summary>
    ///   Invoke InflateRect
    /// </summary>
    function Inflate(const Delta: Integer): BOOL; overload; inline;
    /// <summary>
    ///   Invoke MapWindowPoints
    /// </summary>
    function MapWindow(const FromWnd, ToWnd: HWND): Integer; inline;
//    /// <summary>
//    ///   Invoke MapWindowPoints
//    /// </summary>
//    function MapWindowToScreen(const FromWnd: HWND): Integer; inline;
    /// <summary>
    ///   Invoke MapWindowPoints
    /// </summary>
    function MapWindowFromScreen(const ToWnd: HWND): Integer; inline;
  end;

{ TBitmapHelper }

  TBitmapHelper = class helper for TBitmap
  strict private
    procedure SetSize(const Value: TSize); inline;

  public
    /// <summary>
    ///   Return new bitmap if need to destroy bitmap, otherwise - passed bitmap
    /// </summary>
    class function ResolveBitmap(const IsDestroyBitmap: Boolean; const Bitmap: TBitmap): TBitmap; static; inline;
    procedure AdjustForTransition(const Palette: HPALETTE; const Size: TSize; const PixelFormat: TPixelFormat);

  public
    /// <summary>
    ///   Set width and height into bitmap
    /// </summary>
    property Size: TSize write SetSize;
  end;

{ TControlHelper }

  TControlHelper = class helper for TControl
  public
    /// <summary>
    ///   Resturn control handle if control is WinControl, otherwise - 0
    /// </summary>
    function GetWinControlHandle: HWND; inline;
  end;

{ TWinControlHelper }

  TWinControlHelper = class helper for TWinControl
  public
    function GetInternalHandle(const IsMDIClient: Boolean): HWND; inline;
    function GetInternalBorderWidth: TBorderWidth; inline;
    function GetInternalBevelKind: TBevelKind; inline;
    function GetInternalBevelInner: TBevelCut; inline;
    function GetInternalBevelOuter: TBevelCut; inline;
    function GetInternalBevelWidth: TBevelWidth; inline;
    function GetInternalBevelEdges: TBevelEdges; inline;
    function GetInternalCtl3D: Boolean; inline;
  end;

{ TCustomFormHelper }

  TCustomFormHelper = class helper for TCustomForm
  public
    function GetInternalFormStyle: TFormStyle; inline;
    function GetInternalClientHandle: HWND; inline;
  end;

{ THWNDHelper }

  THWNDHelper = record helper for HWND
  public
    function GetWindowClassName: string; inline;
    function HasRegion: Boolean;
    /// <summary>
    ///   Returns the offset of the client area within the window
    /// </summary>
    function GetWindowOffset: TPoint; inline;
    /// <summary>
    ///   Invoke GetClientRect for True, otherwise - GetWindowRect
    /// </summary>
    function GetWindowSize(const IsMaximizedMDIChild: Boolean): TSize; inline;
    /// <summary>
    ///   Invoke GetWindowLongPtr
    /// </summary>
    function GetWindowData(const Index: Integer): LONG_PTR; inline;
    /// <summary>
    ///   Invoke SetWindowLongPtr
    /// </summary>
    function SetWindowData(const Index: Integer; const Value: LONG_PTR): LONG_PTR; inline;
    /// <summary>
    ///   Invoke MapWindowPoints
    /// </summary>
    function MapWindowToScreen(var Point: TPoint): Integer; inline;
    /// <summary>
    ///   Invoke MapWindowPoints
    /// </summary>
    function MapWindowFromScreen(var Rect: TRect): Integer; inline;
  end;

{ THDCHelper }

  THDCHelper = record helper for HDC
  public
    /// <summary>
    ///   Invoke ExcludeClipRect
    /// </summary>
    function ExcludeClip(const Left, Top, Right, Bottom: Integer): Integer; overload; inline;
//    /// <summary>
//    ///   Invoke ExcludeClipRect
//    /// </summary>
//    function ExcludeClip(const X, Y: Integer; const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke ExcludeClipRect
    /// </summary>
    function ExcludeClip(const Point: TPoint; const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke ExcludeClipRect
    /// </summary>
    function ExcludeClip(const Rect: TRect; const Padding: Integer): Integer; overload; inline;
    /// <summary>
    ///   Invoke IntersectClipRect
    /// </summary>
    function IntersectClip(const Left, Top, Right, Bottom: Integer): Integer; overload; inline;
    /// <summary>
    ///   Invoke IntersectClipRect
    /// </summary>
    function IntersectClip(const Size: TSize): Integer; overload; inline;
//    /// <summary>
//    ///   Invoke IntersectClipRect
//    /// </summary>
//    function IntersectClip(const X, Y: Integer; const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke IntersectClipRect
    /// </summary>
    function IntersectClip(const Point: TPoint; const Size: TSize): Integer; overload; inline;
  end;

implementation

uses
  System.Math;

{$REGION 'Internal definitions'}

var
  FDeviceBitsPerPixel: Integer = -1;
  FDevicePixelFormat: TPixelFormat;

function CheckPixelFormatFor16Bit: TPixelFormat; inline;
begin
  const DisplayDC = GetDC(0);
  try
    const BitmapDC = CreateCompatibleDC(DisplayDC);
    try
      const Bitmap = CreateCompatibleBitmap(DisplayDC, 10, 10);
      try
        const OldBitmap = SelectObject(BitmapDC, Bitmap);
        try
          var PrevGPixel := 255;
          var Count      := 0;
          for var Green := 0 to 255 do
          begin
            const Pixel = RGB(0, Green, 0);
            SetPixel(BitmapDC, 1, 1, Pixel);
            var PrevPixel := GetPixel(BitmapDC, 1, 1);
            if GetGValue(PrevPixel) <> PrevGPixel then Inc(Count);
            PrevGPixel := GetGValue(PrevPixel);
          end;
          if Count > 32 then Result := pf16bit
          else Result := pf15bit;
        finally
          SelectObject(BitmapDC, OldBitmap);
        end;
      finally
        DeleteObject(Bitmap);
      end;
    finally
      DeleteDC(BitmapDC);
    end;
  finally
    ReleaseDC(0, DisplayDC);
  end;
end;

procedure CalculateDeviceBitsPerPixel; inline;
begin
  const DC = GetDC(0);
  try
    FDeviceBitsPerPixel := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
  finally
    ReleaseDC(0, DC);
  end;

  case FDeviceBitsPerPixel of
    1  : FDevicePixelFormat := pf1bit;
    4  : FDevicePixelFormat := pf4bit;
    8  : FDevicePixelFormat := pf8bit;
    15 : FDevicePixelFormat := pf15bit;
    16 : FDevicePixelFormat := CheckPixelFormatFor16Bit;
    24 : FDevicePixelFormat := pf24bit;
    32 : FDevicePixelFormat := pf32bit;
    else FDevicePixelFormat := pf24bit;
  end;
end;

function CreateIdentityPalette: HPALETTE; inline;
var
  PaletteData: TMaxLogPalette;

begin
  const DC = GetDC(0);
  try
    const SysPalSize           = GetDeviceCaps(DC, SIZEPALETTE);
    PaletteData.palVersion    := $300;
    PaletteData.palNumEntries := SysPalSize;
    GetSystemPaletteEntries(DC, 0, SysPalSize, PaletteData.palPalEntry);
    Result := CreatePalette(PLogPalette(@PaletteData)^);
  finally
    ReleaseDC(0, DC);
  end;
end;

{$ENDREGION 'Internal definitions'}

{ TPixelFormatHelper }

class function TPixelFormatHelper.DevicePixelFormat: TPixelFormat;
begin
  Result := FDevicePixelFormat;
end;

{ TScrollInfoHelper }

class function TScrollInfoHelper.Create(const Mask: UINT): TScrollInfo;
begin
  Result.cbSize := SizeOf(Result);
  Result.fMask  := Mask;
end;

class function TScrollInfoHelper.GetHorzScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo;
begin
  Result := TScrollInfo.GetInfo(Wnd, SB_HORZ, Mask);
end;

class function TScrollInfoHelper.GetInfo(const Wnd: HWND; const BarFlag: Integer; const Mask: UINT): TScrollInfo;
begin
  Result := TScrollInfo.Create(Mask);
  Result.LoadInfo(Wnd, BarFlag);
end;

class function TScrollInfoHelper.GetVertScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo;
begin
  Result := TScrollInfo.GetInfo(Wnd, SB_VERT, Mask);
end;

procedure TScrollInfoHelper.LoadInfo(const Wnd: HWND; const BarFlag: Integer);
begin
  GetScrollInfo(Wnd, BarFlag, Self);
end;

{ TSizeHelper }

class function TSizeHelper.InlineCreate(const Width, Height: Integer): TSize;
begin
  Result.Width  := Width;
  Result.Height := Height;
end;

class function TSizeHelper.InlineCreate(const Graphic: TGraphic): TSize;
begin
  if Assigned(Graphic) then Exit(TSize.InlineCreate(Graphic.Width, Graphic.Height));
  Result := TSize.Zero;
end;

class function TSizeHelper.InlineCreate(const ClientSize, GraphicSize: TSize): TSize;
begin
  if (ClientSize.Width / ClientSize.Height) > (GraphicSize.Width / GraphicSize.Height) then
  begin
    Exit(TSize.InlineCreate((GraphicSize.Width * ClientSize.Height) div GraphicSize.Height, ClientSize.Height));
  end;
  Result := TSize.InlineCreate(ClientSize.Width, (GraphicSize.Height * ClientSize.Width) div GraphicSize.Width);
end;

class function TSizeHelper.InlineCreate(const Rect: TRect): TSize;
begin
  Result := TSize.InlineCreate(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
end;

class function TSizeHelper.Zero: TSize;
begin
  Result.Width  := 0;
  Result.Height := 0;
end;

{ TPointHelper }

class function TPointHelper.InlineCreate(const X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TPointHelper.MapWindow(const FromWnd, ToWnd: HWND): Integer;
begin
  Result := MapWindowPoints(FromWnd, ToWnd, Self, 1);
end;

//function TPointHelper.MapWindowFromScreen(const ToWnd: HWND): Integer;
//begin
//  Result := MapWindow(HWND_DESKTOP, ToWnd);
//end;

function TPointHelper.MapWindowToScreen(const FromWnd: HWND): Integer;
begin
  Result := MapWindow(FromWnd, HWND_DESKTOP);
end;

procedure TPointHelper.SetLocationInline(const Point: TPoint);
begin
  Self := Point;
end;

function TPointHelper.SubtractInline(const Point: TPoint): TPoint;
begin
  Result.SetLocationInline(Self.X - Point.X, Self.Y - Point.Y);
end;

procedure TPointHelper.SetLocationInline(const X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

{ TRectHelper }

class function TRectHelper.InlineCreate(const Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left   := Left;
  Result.Top    := Top;
  Result.Right  := Right;
  Result.Bottom := Bottom;
end;

//class function TRectHelper.InlineCreate(const Point: TPoint; const Width, Height: Integer): TRect;
//begin
//  Result := TRect.InlineCreate(Point.X, Point.Y, Point.X + Width, Point.Y + Height);
//end;

class function TRectHelper.InlineCreate(const Left, Top: Integer; const Size: TSize): TRect;
begin
  Result := TRect.InlineCreate(Left, Top, Left + Size.Width, Top + Size.Height);
end;

//function TRectHelper.MapWindowToScreen(const FromWnd: HWND): Integer;
//begin
//  Result := MapWindow(FromWnd, HWND_DESKTOP);
//end;

class function TRectHelper.CreateClientRect(const Wnd: HWND): TRect;
begin
  GetClientRect(Wnd, Result);
end;

function TRectHelper.CreateRgn: HRGN;
begin
  Result := TRect.CreateRgn(Self.Left, Self.Top, Self.Right, Self.Bottom);
end;

class function TRectHelper.CreateRgn(const Left, Top, Right, Bottom: Integer): HRGN;
begin
  Result := CreateRectRgn(Left, Top, Right, Bottom);
end;

class function TRectHelper.CreateWindowRect(const Wnd: HWND): TRect;
begin
  GetWindowRect(Wnd, Result);
end;

function TRectHelper.ExcludeClip(const DC: HDC): Integer;
begin
  Result := DC.ExcludeClip(Self.Left, Self.Top, Self.Right, Self.Bottom);
end;

function TRectHelper.Inflate(const Delta: Integer): BOOL;
begin
  Result := InflateRect(Self, Delta, Delta);
end;

class function TRectHelper.InlineCreate(const Left, Top: Integer; const Size: TSize; const Margin: Word): TRect;
begin
  Result := TRect.InlineCreate(Left + Margin, Top + Margin, Size);
end;

function TRectHelper.IntersectClip(const DC: HDC): Integer;
begin
  Result := DC.IntersectClip(Left, Top, Right, Bottom);
end;

function TRectHelper.MapWindow(const FromWnd, ToWnd: HWND): Integer;
begin
  Result := MapWindowPoints(FromWnd, ToWnd, Self, 2);
end;

function TRectHelper.ResolveRect(const Control: TControl): TRect;
begin
  if IsRectEmpty(Self) then Exit(TRect.InlineCreate(0, 0, Control.ClientWidth, Control.ClientHeight));
  Result := Self;
end;

function TRectHelper.MapWindowFromScreen(const ToWnd: HWND): Integer;
begin
  Result := MapWindow(HWND_DESKTOP, ToWnd);
end;

class function TRectHelper.Zero: TRect;
begin
  Result := TRect.InlineCreate(0, 0, 0, 0);
end;

{ TBitmapHelper }

procedure TBitmapHelper.AdjustForTransition(const Palette: HPALETTE; const Size: TSize; const PixelFormat: TPixelFormat);
begin
  Self.PixelFormat := PixelFormat;
  case PixelFormat of
    pf1bit: Self.Monochrome := True;
    pf8bit:
    begin
      if Palette = 0 then Self.Palette := CreateIdentityPalette
      else Self.Palette := Palette;
    end;
  end;
  Self.Size := Size;
end;

class function TBitmapHelper.ResolveBitmap(const IsDestroyBitmap: Boolean; const Bitmap: TBitmap): TBitmap;
begin
  if IsDestroyBitmap then Exit(TBitmap.Create);
  Result := Bitmap;
end;

procedure TBitmapHelper.SetSize(const Value: TSize);
begin
  Self.Width  := Value.Width;
  Self.Height := Value.Height;
end;

{ TControlHelper }

function TControlHelper.GetWinControlHandle: HWND;
begin
  if Self is TWinControl then Exit((Self as TWinControl).Handle);
  Result := 0;
end;

{ TWinControlHelper }

function TWinControlHelper.GetInternalBevelEdges: TBevelEdges;
begin
  Result := Self.BevelEdges;
end;

function TWinControlHelper.GetInternalBevelInner: TBevelCut;
begin
  Result := Self.BevelInner;
end;

function TWinControlHelper.GetInternalBevelKind: TBevelKind;
begin
  Result := Self.BevelKind;
end;

function TWinControlHelper.GetInternalBevelOuter: TBevelCut;
begin
  Result := Self.BevelOuter;
end;

function TWinControlHelper.GetInternalBevelWidth: TBevelWidth;
begin
  Result := Self.BevelWidth;
end;

function TWinControlHelper.GetInternalBorderWidth: TBorderWidth;
begin
  Result := Self.BorderWidth;
end;

function TWinControlHelper.GetInternalCtl3D: Boolean;
begin
  Result := Self.Ctl3D;
end;

function TWinControlHelper.GetInternalHandle(const IsMDIClient: Boolean): HWND;
begin
  if IsMDIClient then Exit((Self as TCustomForm).GetInternalClientHandle);
  Result := Self.Handle;
end;

{ TCustomFormHelper }

function TCustomFormHelper.GetInternalClientHandle: HWND;
begin
  Result := Self.ClientHandle;
end;

function TCustomFormHelper.GetInternalFormStyle: TFormStyle;
begin
  Result := Self.FormStyle;
end;

{ THWNDHelper }

function THWNDHelper.GetWindowClassName: string;
const
  MaxClassNameLength = 257; //256 plus null terminator

var
  ClassName: array[0..MaxClassNameLength - 1] of Char;

begin
  const ClassNameLength = GetClassName(Self, ClassName, Length(ClassName));
  SetString(Result, ClassName, ClassNameLength);
end;

function THWNDHelper.GetWindowData(const Index: Integer): LONG_PTR;
begin
  Result := GetWindowLongPtr(Self, Index);
end;

function THWNDHelper.GetWindowOffset: TPoint;
begin
  const ScreenRect = TRect.CreateWindowRect(Self);

  Result := TPoint.Zero;
  MapWindowToScreen(Result);

  Result := Result.SubtractInline(ScreenRect.TopLeft);
end;

function THWNDHelper.GetWindowSize(const IsMaximizedMDIChild: Boolean): TSize;
var
  Rect: TRect;

begin
  if IsMaximizedMDIChild then GetClientRect(GetParent(Self), Rect) else GetWindowRect(Self, Rect);
  Result := TSize.InlineCreate(Rect);
end;

function THWNDHelper.HasRegion: Boolean;
begin
  const Rgn = TRect.CreateRgn(0, 0, 0, 0);
  try
    Result := GetWindowRgn(Self, Rgn) <> ERROR;
  finally
    DeleteObject(Rgn);
  end;
end;

function THWNDHelper.MapWindowFromScreen(var Rect: TRect): Integer;
begin
  Result := Rect.MapWindowFromScreen(Self);
end;

function THWNDHelper.MapWindowToScreen(var Point: TPoint): Integer;
begin
  Result := Point.MapWindowToScreen(Self);
end;

function THWNDHelper.SetWindowData(const Index: Integer; const Value: LONG_PTR): LONG_PTR;
begin
  Result := SetWindowLongPtr(Self, Index, Value);
end;

{ THDCHelper }

function THDCHelper.ExcludeClip(const Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := ExcludeClipRect(Self, Left, Top, Right, Bottom);
end;

//function THDCHelper.ExcludeClip(const X, Y: Integer; const Size: TSize): Integer;
//begin
//  Result := ExcludeClipRect(Self, X, Y, X + Size.Width, Y + Size.Height);
//end;

function THDCHelper.ExcludeClip(const Point: TPoint; const Size: TSize): Integer;
begin
  Result := ExcludeClipRect(Self, Point.X, Point.Y, Point.X + Size.Width, Point.Y + Size.Height);
end;

function THDCHelper.ExcludeClip(const Rect: TRect; const Padding: Integer): Integer;
begin
  Result := ExcludeClipRect(Self, Rect.Left + Padding, Rect.Top + Padding, Rect.Right - 2, Rect.Bottom - 2);
end;

function THDCHelper.IntersectClip(const Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := IntersectClipRect(Self, Left, Top, Right, Bottom);
end;

function THDCHelper.IntersectClip(const Size: TSize): Integer;
begin
  Result := IntersectClipRect(Self, 0, 0, Size.Width, Size.Height);
end;

//function THDCHelper.IntersectClip(const X, Y: Integer; const Size: TSize): Integer;
//begin
//  Result := IntersectClipRect(Self, X, Y, X + Size.Width, Y + Size.Height);
//end;

function THDCHelper.IntersectClip(const Point: TPoint; const Size: TSize): Integer;
begin
  Result := IntersectClipRect(Self, Point.X, Point.Y, Point.X + Size.Width, Point.Y + Size.Height);
end;

initialization
  CalculateDeviceBitsPerPixel;

end.
