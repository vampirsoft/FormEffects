/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Tests.inc                                      *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
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

{$IF NOT DEFINED(MSWINDOWS)}
  {$UNDEF USE_VCL_TESTRUNNER}
{$ENDIF MSWINDOWS}
{$IF NOT (DEFINED(USE_VCL_TESTRUNNER) OR DEFINED(USE_MOBILE_TESTRUNNER))}
  {$DEFINE CONSOLE_TESTRUNNER}
{$ENDIF CONSOLE_TESTRUNNER}

{$ENDIF ~ FORM_EFFECTS_TESTS_INC}
