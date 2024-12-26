/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.TypeHelpers.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.TypeHelpers;

{$INCLUDE FormEffects.inc}

interface

uses
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
  end;

{ TSizeHelper }

  TSizeHelper = record helper for TSize
  public
    class function InlineCreate(const Width, Height: Integer): TSize; overload; static; inline;
    class function InlineCreate(const Graphic: TGraphic): TSize; overload; static; inline;
    class function InlineCreate(const ClientSize, GraphicSize: TSize): TSize; overload; static; inline;
    class function InlineCreate(const Rect: TRect): TSize; overload; static; inline;
  end;

{ TPointHelper }

  TPointHelper = record helper for TPoint
  public
    class function InlineCreate(const X, Y : Integer): TPoint; overload; static; inline;
  end;

{ TRectHelper }

  TRectHelper = record helper for TRect
  public
    class function InlineCreate(const Left, Top, Right, Bottom: Integer): TRect; overload; static; inline;
//    class function InlineCreate(const Point: TPoint; const Width, Height: Integer): TRect; overload; static; inline;
    class function InlineCreate(const Left, Top: Integer; const Size: TSize): TRect; overload; static; inline;
    class function InlineCreate(const Left, Top: Integer; const Size: TSize;
      const Margin: Word): TRect; overload; static; inline;

      function ResolveRect(const Control: TControl): TRect; inline;
  end;

{ TBitmapHelper }

  TBitmapHelper = class helper for TBitmap
  strict private
    procedure SetSize(const Value: TSize); inline;

  public
    class function ResolveBitmap(const IsDestroyBitmap: Boolean; const Bitmap: TBitmap): TBitmap; static; inline;
    procedure AdjustForTransition(const Palette: HPALETTE; const Size: TSize; const PixelFormat: TPixelFormat);

  public
    property Size: TSize write SetSize;
  end;

{ TControlHelper }

  TControlHelper = class helper for TControl
  public
    function GetWinControlHandle: HWND; inline;
  end;

{ TWinControlHelper }

  TWinControlHelper = class helper for TWinControl
  public
    function GetInternalHandle(const IsMDIClient: Boolean): HWND; inline;
  end;

{ TCustomFormHelper }

  TCustomFormHelper = class helper for TCustomForm
  public
    function GetInternalFormStyle: TFormStyle; inline;
    function GetInternalClientHandle: HWND; inline;
  end;

implementation

uses
  System.Math;

{$REGION Internal definitions}

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

{$ENDREGION Internal definitions}

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

{ TSizeHelper }

class function TSizeHelper.InlineCreate(const Width, Height: Integer): TSize;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

class function TSizeHelper.InlineCreate(const Graphic: TGraphic): TSize;
begin
  if Assigned(Graphic) then Exit(TSize.InlineCreate(Graphic.Width, Graphic.Height));
  Result := TSize.InlineCreate(0, 0);
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

{ TPointHelper }

class function TPointHelper.InlineCreate(const X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
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
//
class function TRectHelper.InlineCreate(const Left, Top: Integer; const Size: TSize): TRect;
begin
  Result := TRect.InlineCreate(Left, Top, Left + Size.Width, Top + Size.Height);
end;

class function TRectHelper.InlineCreate(const Left, Top: Integer; const Size: TSize; const Margin: Word): TRect;
begin
  Result := TRect.InlineCreate(Left + Margin, Top + Margin, Size);
end;

function TRectHelper.ResolveRect(const Control: TControl): TRect;
begin
  if IsRectEmpty(Self) then Exit(TRect.InlineCreate(0, 0, Control.ClientWidth, Control.ClientHeight));
  Result := Self;
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

initialization
  CalculateDeviceBitsPerPixel;

end.
