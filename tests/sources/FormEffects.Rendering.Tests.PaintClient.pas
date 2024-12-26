/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.PaintClient.pas                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.PaintClient;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{ TPaintClientTests }

  TPaintClientTests = class
  strict private
    FWnd: HWND;
    FDC: HDC;
    FSaveDCIndex: Integer;
    FID: Cardinal;

    FWinControl: TWinControl;

  strict private
    procedure PaintClient(const Flags: DWORD; const ClientCallback: TPaintCallback);

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('PaintClient должен отправить CM_FEPAINT, если имеет RCF_FEPREPAINT флаг',        'True')]
    [TestCase('PaintClient не должен отправлять CM_FEPAINT, если не имеет RCF_FEPREPAINT флаг', 'False')]
    procedure should_send_CM_FEPAINT_message_if_has_RCF_FEPREPAINT_flag(const Expected: Boolean);

    [Test]
    [TestCase('PaintClient должен отправить WM_PRINT, если имеет RCF_PRINT флаг',        'True')]
    [TestCase('PaintClient не должен отправлять WM_PRINT, если не имеет RCF_PRINT флаг', 'False')]
    procedure should_send_WM_PRINT_message_if_has_RCF_PRINT_flag(const Expected: Boolean);

    [Test]
    [TestCase('PaintClient должен вызвать EraseAndPaintMessage, если имеет RCF_PAINT флаг',        'True')]
    [TestCase('PaintClient не должен вызывать EraseAndPaintMessage, если не имеет RCF_PAINT флаг', 'False')]
    procedure should_invoke_EraseAndPaintMessage_if_has_RCF_PAINT_flag(const Expected: Boolean);

    [Test]
    [TestCase('PaintClient должен вызвать callback, если имеет RCF_CALLBACK флаг',        'True')]
    [TestCase('PaintClient не должен вызывать callback, если не имеет RCF_CALLBACK флаг', 'False')]
    procedure should_invoke_callback_if_has_RCF_CALLBACK_flag(const Expected: Boolean);

    [Test]
    [TestCase('PaintClient должен отправить CM_FEPAINT, если имеет RCF_FEPAINT флаг',        'True')]
    [TestCase('PaintClient не должен отправлять CM_FEPAINT, если не имеет RCF_FEPAINT флаг', 'False')]
    procedure should_send_CM_FEPAINT_if_has_RCF_FEPAINT_flag(const Expected: Boolean);

    [Test]
    [TestCase('PaintClient должен вызвать EmulatePaint, если не имеет флагов', 'True')]
    [TestCase('PaintClient не должен вызывать EmulatePaint, если имеет флаг',  'False')]
    procedure should_invoke_EmulatePaint_if_has_not_flags(const Expected: Boolean);

    [Test]
    [TestCase('PaintClient должен отправить CM_FEPAINT, если имеет RCF_FEPOSTPAINT флаг',        'True')]
    [TestCase('PaintClient не должен отправлять CM_FEPAINT, если не имеет RCF_FEPOSTPAINT флаг', 'False')]
    procedure should_send_CM_FEPAINT_if_has_RCF_FEPOSTPAINT_flag(const Expected: Boolean);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  Winapi.Messages,
  System.Math,
  FormEffects.Constants,
  FormEffects.TypeHelpers,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Rendering.Ext.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$M+}

type

{ TMock }

  TMock = class abstract
  public
    procedure ClientCallback(const WinControl: TWinControl; const DC: HDC); virtual; abstract;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

{$M-}

var
  Mock: TMock;

{$IFDEF USE_BILLENIUM_EFFECTS}
procedure ClientCallback(WinControl: TWinControl; DC: HDC);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
procedure ClientCallback(const WinControl: TWinControl; const DC: HDC);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
begin
  if Assigned(Mock) then
    Mock.ClientCallback(WinControl, DC);
end;

{ TMock }

constructor TMock.Create;
begin
  Mock := Self;
end;

destructor TMock.Destroy;
begin
  Mock := nil;
end;

{ TPaintClientTests }

procedure TPaintClientTests.PaintClient(const Flags: DWORD; const ClientCallback: TPaintCallback);
begin
  const StopWnd: HWND = 0;
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.PaintClientExt(
    FWnd,
    StopWnd,
    FWinControl,
    Flags,
    nil,
    ClientCallback,
    FDC
  );
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.PaintClient(
    FWnd,
    StopWnd,
    FWinControl,
    Flags,
    nil,
    ClientCallback,
    FDC
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TPaintClientTests.should_invoke_callback_if_has_RCF_CALLBACK_flag(const Expected: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const CallbackMock      = TMock<TMock>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));
  WinapiWindowsMock
    .Setup
    .WillReturn(FSaveDCIndex)
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(FSaveDCIndex));

  CallbackMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 1, 0))
    .When
    .ClientCallback(
      It0.IsEqualTo<TWinControl>(nil),
      It1.IsEqualTo<HDC>(FDC)
    );

  PaintClient(IfThen(Expected, RCF_CALLBACK, 0), ClientCallback);

  CallbackMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TPaintClientTests.should_invoke_EmulatePaint_if_has_not_flags(const Expected: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const RenderingExtMock  = TMock<TRenderingExtMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));
  WinapiWindowsMock
    .Setup
    .WillReturn(FSaveDCIndex)
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(FSaveDCIndex));

  RenderingExtMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 1, 0))
    .When
    .EmulatePaint(
      It0.IsEqualTo<TWinControl>(FWinControl),
      It1.IsEqualTo<HDC>(FDC)
    );

  PaintClient(IfThen(Expected, 0, RCF_PAINT), nil);

  RenderingExtMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TPaintClientTests.should_invoke_EraseAndPaintMessage_if_has_RCF_PAINT_flag(const Expected: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const RenderingExtMock  = TMock<TRenderingExtMocks>.Create;

  const WinControlMock = TMock<TWinControl>.Create;

  FWinControl := WinControlMock.Instance;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));
  WinapiWindowsMock
    .Setup
    .WillReturn(FSaveDCIndex)
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(FSaveDCIndex));

  RenderingExtMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 1, 0))
    .When
    .EraseAndPaintMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<TWinControl>(FWinControl),
      It2.IsEqualTo<HDC>(FDC)
    );

  PaintClient(IfThen(Expected, RCF_PAINT, 0), nil);

  RenderingExtMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TPaintClientTests.should_send_CM_FEPAINT_if_has_RCF_FEPAINT_flag(const Expected: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));
  WinapiWindowsMock
    .Setup
    .WillReturn(FSaveDCIndex)
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(FSaveDCIndex));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 1, 0))
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(CM_FEPAINT),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(FID)
    );

  PaintClient(IfThen(Expected, RCF_FEPAINT, 0), nil);

  WinapiWindowsMock.Verify;
end;

procedure TPaintClientTests.should_send_CM_FEPAINT_if_has_RCF_FEPOSTPAINT_flag(const Expected: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 2, 1))
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));
  WinapiWindowsMock
    .Setup
    .WillReturn(FSaveDCIndex)
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 2, 1))
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(FSaveDCIndex));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 1, 0))
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(CM_FEPAINT),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(FID)
    );

  PaintClient(IfThen(Expected, RCF_FEPOSTPAINT, 0), nil);

  WinapiWindowsMock.Verify;
end;

procedure TPaintClientTests.should_send_CM_FEPAINT_message_if_has_RCF_FEPREPAINT_flag(const Expected: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 2, 1))
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));
  WinapiWindowsMock
    .Setup
    .WillReturn(FSaveDCIndex)
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 2, 1))
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(FSaveDCIndex));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 1, 0))
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(CM_FEPAINT),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(FID)
    );

  PaintClient(IfThen(Expected, RCF_FEPREPAINT, 0), nil);

  WinapiWindowsMock.Verify;
end;

procedure TPaintClientTests.should_send_WM_PRINT_message_if_has_RCF_PRINT_flag(const Expected: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));
  WinapiWindowsMock
    .Setup
    .WillReturn(FSaveDCIndex)
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(FSaveDCIndex));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 1, 0))
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(WM_PRINT),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(PRF_ERASEBKGND or PRF_CLIENT)
    );

  PaintClient(IfThen(Expected, RCF_PRINT, 0), nil);

  WinapiWindowsMock.Verify;
end;

procedure TPaintClientTests.Setup;
begin
  FWnd := 111;
  FDC  := 222;
  FID  := {$IFDEF USE_BILLENIUM_EFFECTS}BE_ID{$ELSE}FE_ID{$ENDIF};

  FSaveDCIndex := 1999;
end;

procedure TPaintClientTests.TearDown;
begin
  FWinControl := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TPaintClientTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
