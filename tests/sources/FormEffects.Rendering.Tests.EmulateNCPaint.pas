/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.EmulateNCPaint.pas             *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.EmulateNCPaint;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TEmulateNCPaintTests }

  [TestFixture]
  TEmulateNCPaintTests = class
  strict private
    FDC: HDC;

  strict private
    procedure EmulateNCPaint(const WinControl: TWinControl);

  public
    [Setup]
    procedure Setup;

  public
    [Test]
    [TestCase('EmulateNCPaint должен прекратить выполнение, если WinControl неопределён', '')]
    procedure should_stop_executing_when_wincontrol_is_null;

    [Test]
    [TestCase('EmulateNCPaint должен вызвать ToolWindowNCPaint для ToolWindow', '')]
    procedure should_invoke_ToolWindowNCPaint_for_toolwindow;

    [Test]
    [TestCase('EmulateNCPaint должен вызвать WinControlNCPaint для не ToolWindow', '')]
    procedure should_invoke_WinControlNCPaint_for_no_toolwindow;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  System.Types,
  System.Rtti,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Utils.Mocks,
  FormEffects.Vcl.ToolWin.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$REGION 'TEmulateNCPaintMocks'}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{ TEmulateNCPaintMocks}

  TEmulateNCPaintMocks = class abstract(TMocksManager)
  public
    procedure ToolWindowNCPaint(const WinControl: TWinControl; const DC: HDC); virtual; abstract;
    procedure WinControlNCPaint(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const DC: HDC;
      const Themed: Boolean
    ); virtual; abstract;

  public
    constructor Create; override;
  end;

var
  EmulateNCPaintMocks: TEmulateNCPaintMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TToolWindowNCPaint = procedure(WinControl: TWinControl; DC: HDC);

procedure ToolWindowNCPaintMock(WinControl: TWinControl; DC: HDC);
begin
  EmulateNCPaintMocks.ToolWindowNCPaint(WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TToolWindowNCPaint = procedure(const ToolWindow: TToolWindow; const DC: HDC);

procedure ToolWindowNCPaintMock(const ToolWindow: TToolWindow; const DC: HDC);
begin
  EmulateNCPaintMocks.ToolWindowNCPaint(ToolWindow, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TWinControlNCPaint =
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const DC: HDC;
      const Themed: Boolean
    );

procedure WinControlNCPaintMock(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TPaintCallback;
  const DC: HDC;
  const Themed: Boolean
);
begin
  EmulateNCPaintMocks.WinControlNCPaint(Wnd, StopWnd, WinControl, Flags, NonClientCallback, ClientCallback, DC, Themed);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TWinControlNCPaint =
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback;
      const DC: HDC
    );

procedure WinControlNCPaintMock(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC
);
begin
  EmulateNCPaintMocks.WinControlNCPaint(Wnd, StopWnd, WinControl, Flags, NonClientCallback, ClientCallback, DC, False);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TEmulateNCPaintMocks }

constructor TEmulateNCPaintMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TToolWindowNCPaint>(teRender.ToolWindowNCPaint, ToolWindowNCPaintMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TToolWindowNCPaint>(FormEffects.Rendering.ToolWindowNCPaint, ToolWindowNCPaintMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TWinControlNCPaint>(WinControlNCPaintExt, WinControlNCPaintMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TWinControlNCPaint>(FormEffects.Rendering.WinControlNCPaint, WinControlNCPaintMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  EmulateNCPaintMocks := Self;
end;

{$ENDREGION 'TEmulateNCPaintMocks'}

{ TEmulateNCPaintTests }

procedure TEmulateNCPaintTests.EmulateNCPaint(const WinControl: TWinControl);
begin
  const Flags = BF_MONO;
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.EmulateNCPaintExt(
    0,
    0,
    WinControl,
    Flags,
    nil,
    nil,
    FDC,
    False
  );
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.EmulateNCPaint(
    0,
    0,
    WinControl,
    Flags,
    nil,
    nil,
    FDC
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TEmulateNCPaintTests.should_invoke_ToolWindowNCPaint_for_toolwindow;
begin
  const EmulateNCPaintMock = TMock<TEmulateNCPaintMocks>.Create;

  const WinControlMock = TMock<TToolWindow>.Create;

  const Control = WinControlMock.Instance;

  EmulateNCPaintMock
    .Setup
    .Expect
    .Once
    .When
    .ToolWindowNCPaint(
      It0.IsEqualTo<TWinControl>(Control),
      It1.IsEqualTo<HDC>(FDC)
    );

  EmulateNCPaint(Control);

  EmulateNCPaintMock.Verify;
end;

procedure TEmulateNCPaintTests.should_invoke_WinControlNCPaint_for_no_toolwindow;
begin
  const EmulateNCPaintMock = TMock<TEmulateNCPaintMocks>.Create;

  const WinControlMock = TMock<TWinControl>.Create;

  const Control = WinControlMock.Instance;

  EmulateNCPaintMock
    .Setup
    .Expect
    .Once
    .When
    .WinControlNCPaint(
      It0.IsAny<HWND>,
      It1.IsAny<HWND>,
      It2.IsEqualTo<TWinControl>(Control),
      It3.IsEqualTo<DWORD>(BF_MONO),
      It4.IsEqualTo<TPaintCallback>(nil),
      It5.IsEqualTo<TPaintCallback>(nil),
      It6.IsEqualTo<HDC>(FDC),
      It7.IsAny<Boolean>
    );

  EmulateNCPaint(Control);

  EmulateNCPaintMock.Verify;
end;

procedure TEmulateNCPaintTests.should_stop_executing_when_wincontrol_is_null;
begin
  const EmulateNCPaintMock = TMock<TEmulateNCPaintMocks>.Create;

  EmulateNCPaintMock
    .Setup
    .Expect
    .Never
    .When
    .ToolWindowNCPaint(
      It0.IsAny<TWinControl>,
      It1.IsAny<HDC>
    );
  EmulateNCPaintMock
    .Setup
    .Expect
    .Never
    .When
    .WinControlNCPaint(
      It0.IsAny<HWND>,
      It1.IsAny<HWND>,
      It2.IsAny<TWinControl>,
      It3.IsAny<DWORD>,
      It4.IsEqualTo<TPaintCallback>(nil),
      It5.IsEqualTo<TPaintCallback>(nil),
      It6.IsAny<HDC>,
      It7.IsAny<Boolean>
    );

  EmulateNCPaint(nil);

  EmulateNCPaintMock.Verify;
end;

procedure TEmulateNCPaintTests.Setup;
begin
  FDC  := 507;
end;

initialization
  TDUnitX.RegisterTestFixture(TEmulateNCPaintTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
