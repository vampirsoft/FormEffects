/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.ToolWin.Mocks.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.ToolWin.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Vcl.ToolWin,
  FormEffects.Vcl.Controls.Mocks;

type

{ TToolWindow }

  TToolWindow = class abstract(TWinControl)
  public
    function GetEdgeBorders: TEdgeBorders; virtual;
    function GetEdgeInner: TEdgeStyle; virtual;
    function GetEdgeOuter: TEdgeStyle; virtual;

  public
    property EdgeBorders: TEdgeBorders read GetEdgeBorders;
    property EdgeInner: TEdgeStyle read GetEdgeInner;
    property EdgeOuter: TEdgeStyle read GetEdgeOuter;
  end;

implementation

{ TToolWindow }

function TToolWindow.GetEdgeBorders: TEdgeBorders;
begin
  Result := [];
end;

function TToolWindow.GetEdgeInner: TEdgeStyle;
begin
  Result := esNone;
end;

function TToolWindow.GetEdgeOuter: TEdgeStyle;
begin
  Result := esNone;
end;

end.
