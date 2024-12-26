/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.Graphics.Mocks.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
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

  TBrush = class abstract(TPersistent)
  strict private
    FColor: TColor;
    FBitmap: TBitmap;

  public
    function GetHandle: HBRUSH; virtual; abstract;

  public
    constructor Create;

  public
    property Handle: HBRUSH read GetHandle;
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
    FWidth: Integer;
    FHeight: Integer;

  public
    function GetPalette: HPALETTE; virtual;
    function GetWidth: Integer; virtual;
    function GetHeight: Integer; virtual;
    procedure SetPalette(const Value: HPALETTE); virtual;
    procedure SetWidth(const Value: Integer); virtual;
    procedure SetHeight(const Value: Integer); virtual;

  public
    constructor Create; virtual;

  public
    property Transparent: Boolean read FTransparent write FTransparent;
    property Palette: HPALETTE read GetPalette write SetPalette;
    property PaletteModified: Boolean read FPaletteModified write FPaletteModified;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Empty: Boolean read FEmpty write FEmpty;
  end;

{ TCanvas }

  TCanvas = class abstract(TPersistent)
  strict private
    FPen: TPen;
    FPixels: array[0..9] of array[0..9] of TColor;

  strict private
    function GetPixel(X, Y: Integer): TColor;
    procedure SetPixel(X, Y: Integer; Value: TColor);

  public
    function GetHandle: HDC; virtual;
    function GetBrush: TBrush; virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;

  public
    procedure Draw(X, Y: Integer; Graphic: TGraphic); virtual; abstract;
    procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); virtual; abstract;
    procedure FillRect(const Rect: TRect); virtual; abstract;
    procedure MoveTo(X, Y: Integer); virtual; abstract;
    procedure LineTo(X, Y: Integer); virtual; abstract;
    procedure Lock; virtual; abstract;
    procedure Unlock; virtual; abstract;

  public
    property Handle: HDC read GetHandle;
    property Brush: TBrush read GetBrush;
    property Pen: TPen read FPen;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
  end;

{ TBitmap }

  TBitmap = class(TGraphic)
  strict private
    FTransparentColor: TColor;
    FPixelFormat: TPixelFormat;

  public
    function GetCanvas: TCanvas; virtual;
    procedure SetMonochrome(const Value: Boolean); virtual;

  strict private
    function GetScanLine(Row: Integer): Pointer;
    procedure SetScanLine(Row: Integer; Value: Pointer);

  public
    property Canvas: TCanvas read GetCanvas;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor;
    property Monochrome: Boolean write SetMonochrome;
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
  FPaletteModified := False;
  FWidth           := 549;
  FHeight          := 394;
  FEmpty           := False;
end;

function TGraphic.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TGraphic.GetPalette: HPALETTE;
begin
  Result := 0;
end;

function TGraphic.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TGraphic.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

procedure TGraphic.SetPalette(const Value: HPALETTE);
begin
end;

procedure TGraphic.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

{ TCanvas }

constructor TCanvas.Create;
begin
  inherited Create;

  FPen   := TPen.Create;
end;

destructor TCanvas.Destroy;
begin
  FreeAndNil(FPen);

  inherited Destroy;
end;

function TCanvas.GetHandle: HDC;
begin
  Result := 0;
end;

function TCanvas.GetPixel(X, Y: Integer): TColor;
begin
  Result := FPixels[X, Y];
end;

procedure TCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  FPixels[X, Y] := Value;
end;

{ TBitmap }

function TBitmap.GetCanvas: TCanvas;
begin
  Result := nil;
end;

function TBitmap.GetScanLine(Row: Integer): Pointer;
begin
{$MESSAGE WARN 'Not Implemented TBitmap.GetScanLine'}
end;

procedure TBitmap.SetMonochrome(const Value: Boolean);
begin
end;

procedure TBitmap.SetScanLine(Row: Integer; Value: Pointer);
begin
{$MESSAGE WARN 'Not Implemented TBitmap.SetScanLine'}
end;

end.
