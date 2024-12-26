/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Constants.pas                                  *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Constants;

{$INCLUDE FormEffects.inc}

interface

uses
  Vcl.Controls;

{ VCL control message IDs }

const
  CM_FEBASE                = CM_BASE   + $0214;
  CM_FEGETBACKGRONDOPTIONS = CM_FEBASE + $0000;

  MDI_CLIENT_CLASS_NAME = 'MDICLIENT';

{ Form Effects message IDs }

  // Thir party components interface
  FE_ID           = $41A2;
  FE_BASE         = CM_BASE + $0C4A;
  CM_FEPAINT      = FE_BASE + $0000; // Paint client area to Form Effects' DC
  CM_FENCPAINT    = FE_BASE + $0001; // Paint non client area to Form Effects' DC
  CM_FEFULLRENDER = FE_BASE + $0002; // Paint whole control to Form Effects' DC

{ Control render modes }

  feAuto           = $00000000;
  fePaint          = $00000001;
  fePrint          = $00000002;
  feEmulate        = $00000003;
  feCallback       = $00000004;
  fePaintCopy      = $00000005;

  feThemed         = $10000000;
  feRefreshFocused = $20000000;
  feOwnCanvas      = $40000000;
  feNoRender       = $80000000;

{ Control render flags }

  RCF_RENDERNC         = $00000001; // Do render the non-client area
  RCF_REFRESHNC        = $00000002; // Always refresh the non-client area
  RCF_REFRESHFOCUSEDNC = $00000004; // Refresh the non-client area only if the control is focused
  RCF_PRINTNC          = $00000008; // Render non-client area using WM_PRINT message
  RCF_PAINTNC          = $00000010; // Render non-client area using WM_PAINT message
  RCF_EMULNC           = $00000020; // Render non-client with custom code
  RCF_CALLBACKNC       = $00000040; // Render non-client area using a callback method
  RCF_THEMEDNC         = $00000080; // Render non-client area XP themes
  RCF_PAINTCOPYNC      = $00000100; // Render non-client area setting the csPaintCopy control state
  RCF_FENCPREPAINT     = $00000200; // Use BE_NCPAINT, then render non-client area
  RCF_FENCPAINT        = $00000400; // Render non-client area only using BE_NCPAINT
  RCF_FENCPOSTPAINT    = $00000800; // Render non-client area, then use BE_NCPAINT
  RCF_OWNCANVASNC      = $00001000; // Render non-client area in a separate bitmap

  RCF_FEFULLRENDER     = $00080000; // Renders whole window at once using CM_FEFULLRENDER

  RCF_RENDER           = $00100000; // Do render the client area
  RCF_REFRESH          = $00200000; // Always refresh the client area
  RCF_REFRESHFOCUSED   = $00400000; // Refresh the client area only if the control is focused
  RCF_PRINT            = $00800000; // Render client area using WM_PRINT message
  RCF_PAINT            = $01000000; // Render client area using WM_PAINT message
  RCF_EMUL             = $02000000; // Render client with custom code
  RCF_CALLBACK         = $04000000; // Render client area using a callback method
  RCF_PAINTCOPY        = $08000000; // Render client area setting the csPaintCopy control state
  RCF_FEPREPAINT       = $10000000; // Use BE_PAINT, then render client area
  RCF_FEPAINT          = $20000000; // Render client area only using BE_PAINT
  RCF_FEPOSTPAINT      = $40000000; // Render client area, then use BE_PAINT
  RCF_OWNCANVAS        = $80000000; // Render client area in a separate bitmap

  RCF_RENDERMASK       = $FFF00000;
  RCF_RENDERNCMASK     = $00001FFF;

type
  TBackgroundPictureMode = (Center, CenterStretch, Stretch, Tile, Zoom, TopLeft, TopRight, BottomLeft, BottomRight);
  TEmbeddedFormAlign     = (Default, None, Center, Client, TopLeft, MainFormCenter);

resourcestring
  SLockedFormContainer    = 'FELockedFormContainer is locked';
  SIncorrectFormContainer = 'The form was created in a different container';
  SIncorrectBackground    = 'Background is incorrect';
  SClassIsNull            = 'Class is null';
  SInstanceIsNull         = 'Instance is null';

implementation

end.
