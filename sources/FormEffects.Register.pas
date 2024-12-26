/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Register.pas                                   *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Register;

{$INCLUDE FormEffects.inc}

interface

procedure Register;

implementation

uses
  System.Classes,
  Vcl.Controls,
  FormEffects.FormContainer;

procedure Register;
begin
  StartClassGroup(TControl);
  GroupDescendentsWith(TFEFormContainer              , TControl);
  GroupDescendentsWith(TFEFormContainer.TEmbeddedForm, TControl);

  RegisterComponents('Form Effects', [TFEFormContainer, TFEEmbeddedFormHistory]);
end;

end.
