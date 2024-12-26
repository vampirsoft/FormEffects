/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.Graphics.Mocks.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.Graphics.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  System.Classes,
  Vcl.Graphics,
  FormEffects.System.Classes.Mocks;

type

  TBitmap = class;
  
{ TBrush }

  TBrush = class(TPersistent)
  strict private
    FHandle: HBRUSH;
    FColor: TColor;
    FBitmap: TBitmap;

  public
    constructor Create;
    
  public
    property Handle: HBRUSH read FHandle write FHandle;
    property Color: TColor read FColor write FColor;
    property Bitmap: TBitmap read FBitmap write FBitmap;
  end;

{ TPen }

  TPen = class(TPersistent)
  strict private
    FColor: TColor;

  public
    property Color: TColor read FColor write FColor;
  end;

{ TGraphic }

  TGraphic = class(TPersistent)
  strict private
    FTransparent: Boolean;
    FPaletteModified: Boolean;
    FEmpty: Boolean;
    FPalette: HPALETTE;
    FWidth: Integer;
    FHeight: Integer;

  public
    constructor Create; virtual;

  public
    property Transparent: Boolean read FTransparent write FTransparent;
    property Palette: HPALETTE read FPalette write FPalette;
    property PaletteModified: Boolean read FPaletteModified write FPaletteModified;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Empty: Boolean read FEmpty write FEmpty;
  end;

{ TCanvas }

  TCanvas = class(TPersistent)
  strict private
    FHandle: HDC;
    FBrush: TBrush;
    FPen: TPen;
    FPixels: array[0..9] of array[0..9] of TColor;

  strict private
    function GetPixel(X, Y: Integer): TColor;
    procedure SetPixel(X, Y: Integer; Value: TColor);

  public
    DrawRects: TArray<TRect>;

  public
    constructor Create;
    destructor Destroy; override;
    
  public
    procedure Draw(X, Y: Integer; Graphic: TGraphic);
    procedure StretchDraw(const Rect: TRect; Graphic: TGraphic);
    procedure FillRect(const Rect: TRect);
    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);
    procedure Lock;
    procedure Unlock;

  public
    property Handle: HDC read FHandle write FHandle;
    property Brush: TBrush read FBrush;
    property Pen: TPen read FPen;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
  end;

{ TBitmap }

  TBitmap = class(TGraphic)
  strict private
    FMonochrome: Boolean;
    FTransparentColor: TColor;
    FPixelFormat: TPixelFormat;
    FCanvas: TCanvas;

  strict private
    function GetScanLine(Row: Integer): Pointer;
    procedure SetScanLine(Row: Integer; Value: Pointer);

  public
    constructor Create; override;
    destructor Destroy; override;
    
  public
    property Canvas: TCanvas read FCanvas;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor;
    property Monochrome: Boolean read FMonochrome write FMonochrome;
    property PixelFormat: TPixelFormat read FPixelFormat write FPixelFormat;
    property ScanLine[Row: Integer]: Pointer read GetScanLine write SetScanLine;
  end;

{ TPicture }

  TPicture = class(TPersistent)
  strict private
    FOnChange: TNotifyEvent;
    FGraphic: TGraphic;

  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Graphic: TGraphic read FGraphic write FGraphic;
  end;

implementation

uses
  System.SysUtils;

{ TBrush }

constructor TBrush.Create;
begin
  inherited Create;

  FColor := clWhite;
end;

{ TGraphic }

constructor TGraphic.Create;
begin
  FTransparent     := False;
  FPalette         := 0;
  FPaletteModified := False;
  FWidth           := 549;
  FHeight          := 394;
  FEmpty           := False;
end;

{ TCanvas }

constructor TCanvas.Create;
begin
  inherited Create;

  FBrush := TBrush.Create;
  FPen   := TPen.Create;

  DrawRects := [];
end;

destructor TCanvas.Destroy;
begin
  FreeAndNil(FPen);
  FreeAndNil(FBrush);

  inherited Destroy;
end;

procedure TCanvas.Draw(X, Y: Integer; Graphic: TGraphic);
begin
  DrawRects := DrawRects + [TRect.Create(X, Y, -1, -1)];
end;

procedure TCanvas.FillRect(const Rect: TRect);
begin
end;

function TCanvas.GetPixel(X, Y: Integer): TColor;
begin
  Result := FPixels[X, Y];
end;

procedure TCanvas.LineTo(X, Y: Integer);
begin

end;

procedure TCanvas.Lock;
begin

end;

procedure TCanvas.MoveTo(X, Y: Integer);
begin

end;

procedure TCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  FPixels[X, Y] := Value;
end;

procedure TCanvas.StretchDraw(const Rect: TRect; Graphic: TGraphic);
begin
  DrawRects := DrawRects + [Rect];
end;

procedure TCanvas.Unlock;
begin

end;

{ TBitmap }

constructor TBitmap.Create;
begin
  inherited Create;

  FCanvas := TCanvas.Create;
end;

destructor TBitmap.Destroy;
begin
  FreeAndNil(FCanvas);

  inherited;
end;

function TBitmap.GetScanLine(Row: Integer): Pointer;
begin
{$MESSAGE WARN 'Not Implemented TBitmap.GetScanLine'}
end;

procedure TBitmap.SetScanLine(Row: Integer; Value: Pointer);
begin
{$MESSAGE WARN 'Not Implemented TBitmap.SetScanLine'}
end;

end.
