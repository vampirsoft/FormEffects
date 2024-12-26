/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.System.Classes.Mocks.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.System.Classes.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  System.Classes;

type

{ TPersistent }

{$M+}

  TPersistent = class
  public
    procedure Assign(Source: TPersistent); virtual;
  end;

{$M-}

  TPersistentClass = class of TPersistent;

{ TComponent }

  TComponent = class(TPersistent, IInterface)
  strict private
    FName: TComponentName;
    FComponentState: TComponentState;
    FOwner: TComponent;

  public
    procedure SetName(const NewName: TComponentName); virtual;

  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  public
    constructor Create(Owner: TComponent); virtual;

  public
    function FindComponent(const AName: string): TComponent; virtual; abstract;
    procedure InsertComponent(const AComponent: TComponent); virtual;

  public
    property ComponentState: TComponentState read FComponentState write FComponentState;
    property Owner: TComponent read FOwner;

  published
    property Name: TComponentName read FName write SetName stored False;
  end;

function ActivateClassGroup(AClass: TPersistentClass): TPersistentClass;
procedure StartClassGroup(AClass: TPersistentClass);
procedure GroupDescendentsWith(AClass, AClassGroup: TPersistentClass);

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

procedure TComponent.InsertComponent(const AComponent: TComponent);
begin
end;

function TComponent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TComponent.SetName(const NewName: TComponentName);
begin
  FName := NewName;
end;

function TComponent._AddRef: Integer;
begin
  Result := -1;
end;

function TComponent._Release: Integer;
begin
  Result := -1;
end;

function ActivateClassGroup(AClass: TPersistentClass): TPersistentClass;
begin
  Result := AClass;
end;

procedure StartClassGroup(AClass: TPersistentClass);
begin
end;

procedure GroupDescendentsWith(AClass, AClassGroup: TPersistentClass);
begin
end;

end.
