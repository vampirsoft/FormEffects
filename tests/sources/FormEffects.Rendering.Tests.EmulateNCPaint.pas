/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.EmulateNCPaint.pas             *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.EmulateNCPaint;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
  Vcl.Themes,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.ToolWin.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{$REGION 'TEmulateNCPaintMocks'}

{ TEmulateNCPaintMocks}

  TEmulateNCPaintMocks = class(TMocksManager)
  private type
    TToolWindowNCPaintResultReference = reference to procedure(const WinControl: TWinControl; const DC: HDC);
  public
    procedure ToolWindowNCPaint_Result(const Callback: TToolWindowNCPaintResultReference);

  private type
    TWinControlNCPaintResultReference =
      reference to procedure(
        const Wnd, StopWnd: HWND;
        const WinControl: TWinControl;
        const Flags: DWORD;
        const NonClientCallback, ClientCallback: TPaintCallback;
        const DC: HDC;
        const Themed: Boolean
      );
  public
    procedure WinControlNCPaint_Result(const Callback: TWinControlNCPaintResultReference);

  public
    constructor Create; override;
  end;

{$ENDREGION 'TEmulateNCPaintMocks'}

{ TEmulateNCPaintTests }

  [TestFixture]
  TEmulateNCPaintTests = class
  strict private
    FVclControlsMocks: TVclControlsMocks;
    FEmulateNCPaintMocks: TEmulateNCPaintMocks;

    FDC: HDC;

  strict private
    procedure EmulateNCPaint(const WinControl: TWinControl);

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

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
  System.Types,
  System.Rtti,
  System.SysUtils,
  FormEffects.TypeHelpers,
  FormEffects.TypeHelpers.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$REGION 'TEmulateNCPaintMocks'}

var
  ToolWindowNCPaintResultReference: TEmulateNCPaintMocks.TToolWindowNCPaintResultReference = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TToolWindowNCPaint = procedure(WinControl: TWinControl; DC: HDC);

procedure ToolWindowNCPaintMock(WinControl: TWinControl; DC: HDC);
begin
  ToolWindowNCPaintResultReference(WinControl, DC);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TToolWindowNCPaint = procedure(const ToolWindow: TToolWindow; const DC: HDC);

procedure ToolWindowNCPaintMock(const ToolWindow: TToolWindow; const DC: HDC);
begin
  ToolWindowNCPaintResultReference(ToolWindow, DC);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TEmulateNCPaintMocks.ToolWindowNCPaint_Result(const Callback: TToolWindowNCPaintResultReference);
begin
  ToolWindowNCPaintResultReference := Callback;
end;

var
  WinControlNCPaintResultReference: TEmulateNCPaintMocks.TWinControlNCPaintResultReference = nil;

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
  WinControlNCPaintResultReference(Wnd, StopWnd, WinControl, Flags, NonClientCallback, ClientCallback, DC, Themed);
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
  WinControlNCPaintResultReference(Wnd, StopWnd, WinControl, Flags, NonClientCallback, ClientCallback, DC, False);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TEmulateNCPaintMocks.WinControlNCPaint_Result(const Callback: TWinControlNCPaintResultReference);
begin
  WinControlNCPaintResultReference := Callback;
end;

{ TEmulateNCPaintMocks }

constructor TEmulateNCPaintMocks.Create;
begin
  inherited Create;

  AddIntercept<TToolWindowNCPaint>(ToolWindowNCPaint, ToolWindowNCPaintMock);
  ToolWindowNCPaint_Result(
    procedure(const WinControl: TWinControl; const DC: HDC)
    begin
    end
  );

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TWinControlNCPaint>(WinControlNCPaintExt, WinControlNCPaintMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TWinControlNCPaint>(WinControlNCPaint, WinControlNCPaintMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  WinControlNCPaint_Result(
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const DC: HDC;
      const Themed: Boolean
    )
    begin
    end
  );
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
  var ToolWindowNCPaintInvoked := False;

  const WinControlMock = TMock<TToolWindow>.Create;

  const Control = WinControlMock.Instance;

  FEmulateNCPaintMocks.ToolWindowNCPaint_Result(
    procedure(const WinControl: TWinControl; const DC: HDC)
    begin
      ToolWindowNCPaintInvoked := True;

      Assert.AreEqual<TWinControl>(Control, WinControl);
      Assert.AreEqual<HDC>(FDC, DC);
    end
  );

  EmulateNCPaint(Control);

  Assert.IsTrue(ToolWindowNCPaintInvoked);
end;

procedure TEmulateNCPaintTests.should_invoke_WinControlNCPaint_for_no_toolwindow;
begin
  var WinControlNCPaintInvoked := False;

  const WinControlMock = TMock<TWinControl>.Create;

  const Control = WinControlMock.Instance;

  FEmulateNCPaintMocks.WinControlNCPaint_Result(
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const DC: HDC;
      const Themed: Boolean
    )
    begin
      WinControlNCPaintInvoked := True;

      Assert.AreEqual<TWinControl>(Control, WinControl);
      Assert.AreEqual<DWORD>(BF_MONO, Flags);
      Assert.AreEqual<HDC>(FDC, DC);
    end
  );

  EmulateNCPaint(Control);

  Assert.IsTrue(WinControlNCPaintInvoked);
end;

procedure TEmulateNCPaintTests.should_stop_executing_when_wincontrol_is_null;
begin
  var ToolWindowNCPaintInvoked := False;
  var WinControlNCPaintInvoked := False;

  FEmulateNCPaintMocks.ToolWindowNCPaint_Result(
    procedure(const WinControl: TWinControl; const DC: HDC)
    begin
      ToolWindowNCPaintInvoked := True;
    end
  );
  FEmulateNCPaintMocks.WinControlNCPaint_Result(
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const DC: HDC;
      const Themed: Boolean
    )
    begin
      WinControlNCPaintInvoked := True;
    end
  );

  EmulateNCPaint(nil);

  Assert.IsFalse(ToolWindowNCPaintInvoked);
  Assert.IsFalse(WinControlNCPaintInvoked);
end;

procedure TEmulateNCPaintTests.Setup;
begin
  FVclControlsMocks    := TVclControlsMocks.Create;
  FEmulateNCPaintMocks := TEmulateNCPaintMocks.Create;

  FDC  := 507;
end;

procedure TEmulateNCPaintTests.TearDown;
begin
  FreeAndNil(FEmulateNCPaintMocks);
  FreeAndNil(FVclControlsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TEmulateNCPaintTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
