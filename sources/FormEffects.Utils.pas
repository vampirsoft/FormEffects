/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.pas                                      *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils;

{$INCLUDE FormEffects.inc}

interface

function IsInheritsClass(const ClassType: TClass; const ClassName: string): Boolean;

implementation

function IsInheritsClass(const ClassType: TClass; const ClassName: string): Boolean;
begin
  var Parent := ClassType;
  while Parent <> TObject do
  begin
    if Parent.ClassNameIs(ClassName) then
      Exit(True);
    Parent := Parent.ClassParent;
  end;
  Result := Parent.ClassNameIs(ClassName);
end;

end.
