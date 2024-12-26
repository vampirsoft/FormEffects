/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.System.Classes.Mocks.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.System.Classes.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  System.Classes;

type

{ TPersistent }

{$M+}

  TPersistent = class(TInterfacedObject)
  public
    procedure Assign(Source: TPersistent); virtual;
  end;

{$M-}

{ TComponent }

  TComponent = class(TPersistent)
  strict private
    FComponentState: TComponentState;
    FOwner: TComponent;

  public
    constructor Create(Owner: TComponent); virtual;

  public
    property ComponentState: TComponentState read FComponentState write FComponentState;
    property Owner: TComponent read FOwner;
  end;

implementation

{ TPersistent }

procedure TPersistent.Assign(Source: TPersistent);
begin
end;

{ TComponent }

constructor TComponent.Create(Owner: TComponent);
begin
  FOwner := Owner;
end;

end.
