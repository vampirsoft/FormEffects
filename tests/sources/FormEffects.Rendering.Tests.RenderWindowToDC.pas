/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.RenderWindowToDC.pas           *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.RenderWindowToDC;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TRenderWindowToDCTests }

  [TestFixture]
  TRenderWindowToDCTests = class
  strict private
    FWnd: HWND;
    FStopWnd: HWND;
    FDC: HDC;
    FWinControl: TWinControl;

  strict private
    procedure RenderWindowToDC(
      const ClientCoordinates: Boolean = False;
      const CheckVisibility: Boolean = False
    ); inline;

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('RenderWindowToDC должен прекратить исполнение, если Wnd = StopWnd', '')]
    procedure should_stop_executing_when_wnd_equal_stopwnd;

    [Test]
    [TestCase('RenderWindowToDC должен прекратить исполнение, если Wnd не виден при CheckVisibility = True', '')]
    procedure should_stop_executing_if_wnd_not_visible_when_CheckVisibility;

    [Test]
    [TestCase('RenderWindowToDC должен вызвать SetWindowOrgEx после выполнения, если была выполнена предподготовка и Rect был изменён',         'True')]
    [TestCase('RenderWindowToDC не должен вызвать SetWindowOrgEx после выполнения, если была выполнена предподготовка, но Rect не был изменён', 'False')]
    procedure should_invoke_SetWindowOrgEx_after_render(const Expected: Boolean);

    [Test]
    [TestCase('RenderWindowToDC должен обнулить StopWnd, если StopWnd не является дочерним окном от Wnd', 'False')]
    [TestCase('RenderWindowToDC не должен обнулять StopWnd, если StopWnd является дочерним окном от Wnd', 'True')]
    procedure should_override_stopwnd(const IsChild: Boolean);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  System.Types,
  System.Rtti,
  System.SysUtils,
  System.Math,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Utils.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Utils.Forms.Mocks,
  FormEffects.Utils.Windows.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$REGION 'TRenderWindowToDCAuxMocks'}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{ TRenderWindowToDCAuxMocks }

  TRenderWindowToDCAuxMocks = class abstract(TMocksManager)
  public
    procedure RenderWindowToDCAux(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const DC: HDC;
      const Rect: TRect;
      const CheckVisibility, CheckRegion: Boolean
    ); virtual; abstract;

  public
    constructor Create; reintroduce;
  end;

var
  RenderWindowToDCAuxMocks: TRenderWindowToDCAuxMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}

type
  TRenderWindowToDCAux =
    procedure(
      const Wnd, StopWnd, Parent: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TTEPaintCallback;
      const DC: HDC;
      const Rect: TRect;
      const CheckVisibility, CheckRegion, Fast: Boolean
    );

procedure RenderWindowToDCAuxMock(
  const Wnd, StopWnd, Parent: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TTEPaintCallback;
  const DC: HDC;
  const Rect: TRect;
  const CheckVisibility, CheckRegion, Fast: Boolean
);
begin
  RenderWindowToDCAuxMocks
    .RenderWindowToDCAux(
      Wnd,
      StopWnd,
      WinControl,
      Flags,
      NonClientCallback,
      ClientCallback,
      DC,
      Rect,
      CheckVisibility,
      CheckRegion
    );
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TRenderWindowToDCAux =
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback;
      const DC: HDC;
      const Rect: TRect;
      const CheckVisibility, CheckRegion: Boolean
    );

procedure RenderWindowToDCAuxMock(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC;
  const Rect: TRect;
  const CheckVisibility, CheckRegion: Boolean
);
begin
  RenderWindowToDCAuxMocks
    .RenderWindowToDCAux(
      Wnd,
      StopWnd,
      WinControl,
      Flags,
      NonClientCallback,
      ClientCallback,
      DC,
      Rect,
      CheckVisibility,
      CheckRegion
    );
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TRenderWindowToDCAuxMocks }

constructor TRenderWindowToDCAuxMocks.Create;
begin
  inherited Create;

  RenderWindowToDCAuxMocks := Self;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TRenderWindowToDCAux>(RenderWindowToDCAuxExt, RenderWindowToDCAuxMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TRenderWindowToDCAux>(FormEffects.Rendering.RenderWindowToDCAux, RenderWindowToDCAuxMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

{$ENDREGION 'TRenderWindowToDCAuxMocks'}

{ TRenderWindowToDCTests }

procedure TRenderWindowToDCTests.RenderWindowToDC(const ClientCoordinates, CheckVisibility: Boolean);
begin
  const Rect        = TRect.InlineCreate(100, 55, 379, 177);
  const CheckRegion = True;
{$IFDEF USE_BILLENIUM_EFFECTS}
  TEAPIHooksDisabled := True;
  teRender.RenderWindowToDC(
    FWnd,
    FStopWnd,
    FWinControl,
    FDC,
    Rect,
    ClientCoordinates,
    CheckVisibility,
    CheckRegion,
    False
  );
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.RenderWindowToDC(
    FWnd,
    FStopWnd,
    FWinControl,
    FDC,
    Rect,
    ClientCoordinates,
    CheckVisibility,
    CheckRegion
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TRenderWindowToDCTests.should_invoke_SetWindowOrgEx_after_render(const Expected: Boolean);
begin
  const WinapiWindowsMock        = TMock<TWinapiWindowsMocks>.Create;
  const RenderWindowToDCAuxMocks = TMock<TRenderWindowToDCAuxMocks>.Create;
  const UtilsWindowsMock         = TMock<TUtilsWindowsMocks>.Create;
  const UtilsFormsMock           = TMock<TUtilsFormsMocks>.Create;

  var WindowOffset := TPoint.Zero;
  if Expected then
    WindowOffset := TPoint.InlineCreate(141, 77);

  UtilsWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetWindowOffset(It0.IsEqualTo<HWND>(FWnd));
  UtilsWindowsMock
    .Setup
    .WillReturn(WindowOffset)
    .When
    .GetWindowOffset(It0.IsEqualTo<HWND>(FWnd));

  UtilsFormsMock
    .Setup
    .Expect
    .Once
    .When
    .IsMaximizedMDIChild(It0.IsAny<TWinControl>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .OffsetWindowOrgEx(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(WindowOffset.X),
      It2.IsEqualTo<Integer>(WindowOffset.Y)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(WindowOffset)
    .When
    .OffsetWindowOrgEx(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(WindowOffset.X),
      It2.IsEqualTo<Integer>(WindowOffset.Y)
    );

  var Rect: TRect;
  if Expected then
    Rect := TRect.InlineCreate(241, 132, 520, 254)
  else
    Rect := TRect.InlineCreate(100, 55, 379, 177);

  RenderWindowToDCAuxMocks
    .Setup
    .Expect
    .Once
    .When
    .RenderWindowToDCAux(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsAny<HWND>,
      It2.IsEqualTo<TWinControl>(FWinControl),
      It3.IsAny<DWORD>,
      It4.IsEqualTo<TPaintCallback>(nil),
      It5.IsEqualTo<TPaintCallback>(nil),
      It6.IsEqualTo<HDC>(FDC),
      It7.IsEqualTo<TRect>(Rect),
      It8.IsEqualTo<Boolean>(False),
      It9.IsEqualTo<Boolean>(True)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(IfThen(Expected, 1, 0))
    .When
    .SetWindowOrgEx(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(WindowOffset.X),
      It2.IsEqualTo<Integer>(WindowOffset.Y)
    );

  RenderWindowToDC(True);

  RenderWindowToDCAuxMocks.Verify;
  UtilsWindowsMock.Verify;
  UtilsFormsMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TRenderWindowToDCTests.should_override_stopwnd(const IsChild: Boolean);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .IsChild(It0.IsEqualTo<HWND>(FWnd), It1.IsEqualTo<HWND>(FStopWnd));
  WinapiWindowsMock
    .Setup
    .WillReturn(IsChild)
    .When
    .IsChild(It0.IsEqualTo<HWND>(FWnd), It1.IsEqualTo<HWND>(FStopWnd));

  var StopWnd: HWND;
  if IsChild then
    StopWnd := FStopWnd
  else
    StopWnd := 0;

  const RenderWindowToDCAuxMocks = TMock<TRenderWindowToDCAuxMocks>.Create;
  RenderWindowToDCAuxMocks
    .Setup
    .Expect
    .Once
    .When
    .RenderWindowToDCAux(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<HWND>(StopWnd),
      It2.IsAny<TWinControl>,
      It3.IsAny<DWORD>,
      It4.IsEqualTo<TPaintCallback>(nil),
      It5.IsEqualTo<TPaintCallback>(nil),
      It6.IsAny<HDC>,
      It7.IsAny<TRect>,
      It8.IsAny<Boolean>,
      It9.IsAny<Boolean>
    );

  RenderWindowToDC;

  RenderWindowToDCAuxMocks.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TRenderWindowToDCTests.should_stop_executing_if_wnd_not_visible_when_CheckVisibility;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const UtilsFormsMock    = TMock<TUtilsFormsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .IsWindowVisible(It0.IsEqualTo<HWND>(FWnd));
  WinapiWindowsMock
    .Setup
    .WillReturn(True)
    .When
    .IsWindowVisible(It0.IsEqualTo<HWND>(FStopWnd));
  WinapiWindowsMock
    .Setup
    .WillReturn(False)
    .When
    .IsWindowVisible(It0.IsNotIn<HWND>([FStopWnd]));

  UtilsFormsMock
    .Setup
    .Expect
    .Never
    .When
    .IsMaximizedMDIChild(It0.IsAny<TWinControl>);

  RenderWindowToDC(True, True);

  UtilsFormsMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TRenderWindowToDCTests.should_stop_executing_when_wnd_equal_stopwnd;
begin
  const UtilsFormsMock = TMock<TUtilsFormsMocks>.Create;

  FStopWnd := FWnd;

  UtilsFormsMock
    .Setup
    .Expect
    .Never
    .When
    .IsMaximizedMDIChild(It0.IsAny<TWinControl>);

  RenderWindowToDC(True);

  UtilsFormsMock.Verify;
end;

procedure TRenderWindowToDCTests.Setup;
begin
  FWnd     := 199;
  FStopWnd := 299;
  FDC      := 399;
end;

procedure TRenderWindowToDCTests.TearDown;
begin
  FWinControl := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TRenderWindowToDCTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
