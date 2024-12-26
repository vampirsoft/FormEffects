/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.inc                                            *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

{$IF NOT DEFINED(FORM_EFFECTS_INC)}
{$DEFINE FORM_EFFECTS_INC}

// Использовать эффекты смены форм
//{$DEFINE USE_TRANSITION_EFFECTS}

// ВНИМАНИЕ!!!
// =============================================================================
// КОД СЛЕДУЮЩИЙ НИЖЕ НЕ ПРЕДНАЗНАЧЕН ДЛЯ ИЗМЕНЕНИЯ ПРОГРАММИСТОМ И
// СОДЕРЖИТ КРИТИЧЕСКИЕ НАСТРОЙКИ ДЛЯ БИБЛИОТЕКИ csv

{$INCLUDE Common.inc}

{$IMPORTEDDATA ON}

{$IF DEFINED(DEBUG)}
  {$IOCHECKS     ON}      // {$I+}
{$ELSE ~ NOT DEBUG}
  {$IOCHECKS     OFF}     // {$I-}
{$ENDIF ~ DEBUG}

{$ENDIF ~ FORM_EFFECTS_INC}
