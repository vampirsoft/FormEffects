/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Mocks.Mocks.pas                          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  System.Generics.Collections;

type

{ TMocksManager }

{$M+}

  TMocksManager = class
  strict private
    FTrampolineIntercepts: TList<IUnknown>;

  strict protected
    procedure AddIntercept<T>(const TargetProc, InterceptProc: T); inline;

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

{$M-}

implementation

uses
  System.SysUtils,
  DDetours;

{ TMocksManager }

procedure TMocksManager.AddIntercept<T>(const TargetProc, InterceptProc: T);
begin
  FTrampolineIntercepts.Add(TIntercept<T>.Create(TargetProc, InterceptProc));
end;

constructor TMocksManager.Create;
begin
  FTrampolineIntercepts := TList<IUnknown>.Create;
end;

destructor TMocksManager.Destroy;
begin
  FreeAndNil(FTrampolineIntercepts);
end;

end.
