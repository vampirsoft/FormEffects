/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.TypeHelpers.Mocks.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.TypeHelpers.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  System.Rtti,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ToolWin,
  Vcl.Themes,
  Vcl.Forms,
  FormEffects.Constants,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Vcl.Controls.Mocks;

type

  TControls = TArray<TControl>;

{ TValueHelper }

  TValueHelper = record helper for TValue
  public
    class operator Implicit(const Value: TFormStyle): TValue;
    class operator Implicit(const Value: TWindowState): TValue;
    class operator Implicit(const Value: TEdgeStyle): TValue;
    class operator Implicit(const Value: TEdgeBorders): TValue;
    class operator Implicit(const Value: TThemedElementDetails): TValue;
    class operator Implicit(const Value: TControlStyle): TValue;
    class operator Implicit(const Value: TBevelKind): TValue;
    class operator Implicit(const Value: TBevelCut): TValue;
    class operator Implicit(const Value: TBevelEdges): TValue;
    class operator Implicit(const Value: TScrollInfo): TValue;
    class operator Implicit(const Value: TSize): TValue;
    class operator Implicit(const Value: TPoint): TValue;
    class operator Implicit(const Value: TRect): TValue;
    class operator Implicit(const Value: TPixelFormat): TValue;
  end;

{ TFEBackgroundPictureModeHelper }

  TPictureMode = {$IFDEF USE_BILLENIUM_EFFECTS}TFCPictureMode{$ELSE}TBackgroundPictureMode{$ENDIF};

  TFEBackgroundPictureModeHelper = record helper for TBackgroundPictureMode
  public
    function Resolve: TPictureMode;
  end;

{ TScrollInfoHelper }

  TScrollInfoHelper = record helper for TScrollInfo
  public
    class function Create(const Min, Max: Integer): TScrollInfo; overload; inline; static;
  end;

implementation

{ TValueHelper }

class operator TValueHelper.Implicit(const Value: TFormStyle): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TWindowState): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TEdgeStyle): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TEdgeBorders): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TThemedElementDetails): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TControlStyle): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TBevelKind): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TBevelCut): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TBevelEdges): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TScrollInfo): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TSize): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TPoint): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TRect): TValue;
begin
  Result := TValue.From(Value);
end;

class operator TValueHelper.Implicit(const Value: TPixelFormat): TValue;
begin
  Result := TValue.From(Value);
end;

{ TFEBackgroundPictureModeHelper }

function TFEBackgroundPictureModeHelper.Resolve: TPictureMode;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := TPictureMode(Ord(Self));
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  Result := Self;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

{ TScrollInfoHelper }

class function TScrollInfoHelper.Create(const Min, Max: Integer): TScrollInfo;
begin
  Result.nMin := Min;
  Result.nMax := Max;
  Result.nPos := Max - Min;
end;

end.
