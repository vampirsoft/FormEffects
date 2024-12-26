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
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Controls.Mocks;

type

{ TToolWindow }

  TToolWindow = class(TWinControl)
  private
    FEdgeBorders: TEdgeBorders;
    FEdgeInner: TEdgeStyle;
    FEdgeOuter: TEdgeStyle;
  public
    constructor Create(Owner: TComponent); overload; override;

  public
    property EdgeBorders: TEdgeBorders read FEdgeBorders write FEdgeBorders default [ebLeft, ebTop, ebRight, ebBottom];
    property EdgeInner: TEdgeStyle read FEdgeInner write FEdgeInner default esRaised;
    property EdgeOuter: TEdgeStyle read FEdgeOuter write FEdgeOuter default esLowered;
  end;

implementation

{ TToolWindow }

constructor TToolWindow.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FEdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  FEdgeInner   := esRaised;
  FEdgeOuter   := esLowered;
end;

end.
