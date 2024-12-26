/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Tests.inc                                      *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

{$IF NOT DEFINED(FORM_EFFECTS_TESTS_INC)}
{$DEFINE FORM_EFFECTS_TESTS_INC}

// Для тестов
{$DEFINE USE_BILLENIUM_EFFECTS}

// ВНИМАНИЕ!!!
// =============================================================================
// КОД СЛЕДУЮЩИЙ НИЖЕ НЕ ПРЕДНАЗНАЧЕН ДЛЯ ИЗМЕНЕНИЯ ПРОГРАММИСТОМ И
// СОДЕРЖИТ КРИТИЧЕСКИЕ НАСТРОЙКИ

{$INCLUDE FormEffects.inc}
{$INCLUDE Common.Tests.inc}

{$IF DEFINED(WIN64)}
  {$UNDEF USE_BILLENIUM_EFFECTS} // Billenium Effects not supported Win64
{$ENDIF ~ WIN64}

{$ENDIF ~ FORM_EFFECTS_TESTS_INC}
