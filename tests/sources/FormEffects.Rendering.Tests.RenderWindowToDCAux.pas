/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.RenderWindowToDCAux.pas        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.RenderWindowToDCAux;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TRenderWindowToDCAuxTests }

  TRenderWindowToDCAuxTests = class
  strict private
    FWnd: HWND;
    FStopWnd: HWND;
    FDC: HDC;
    FWinControl: TWinControl;

  strict private
    procedure RenderWindowToDCAux(const CheckVisibility: Boolean = False);

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('RenderWindowToDCAux должен прекратить выполнение, если CheckVisibility = True и Wnd невидно', '')]
    procedure should_stop_executing_when_CheckVisibility_and_no_visible_window;

    [Test]
    [TestCase('RenderWindowToDCAux должен создать регион и уничтожить его при выполнении', '')]
    procedure should_use_clipping_region_when_executing;

    [Test]
    [TestCase('RenderWindowToDCAux должен выполнить SetWindowLongPtr и SetWindowPos для MainForm, если WinControl является MDI формой с дочерними MDI формами', '')]
    procedure should_set_window_data_and_pos_for_main_form_if_wincontrol_is_mdi_with_mdi_children;

    [Test]
    [TestCase('RenderWindowToDCAux должен вызвать GetRegControl и DoRender с подготовленными данными', '')]
    procedure should_invoke_GetRegControl_and_DoRender_with_loaded_params;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  System.Types,
  System.Rtti,
  System.SysUtils,
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
  FormEffects.Utils.Windows.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$REGION 'TDoRenderMocks'}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{ TDoRenderMocks }

  TDoRenderMocks = class abstract(TMocksManager)
  type
    TRegControl = record
    private
      FFlags: DWORD;
      FNonClientCallback: TPaintCallback;
      FClientCallback: TPaintCallback;

    public
      constructor Create(const Flags: DWORD; const NonClientCallback, ClientCallback: TPaintCallback);

    public
      function AsValue: TValue;
    end;

  public
    procedure DoRender(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
      const DC: HDC;
      const Size: TSize;
      const Rect: TRect
    ); virtual; abstract;
    function GetRegControl(
      const Wnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback
    ): TRegControl; virtual;
    procedure CheckClipRegion(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const IsMaximizedMDIChild: Boolean;
      const DC: HDC;
      const CheckRegion: Boolean;
      const Size: TSize
    ); virtual; abstract;

  public
    constructor Create; override;
  end;

var
  DoRenderMocks: TDoRenderMocks;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TDoRender =
    procedure(
      const Wnd, StopWnd: HWnd;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TTEPaintCallback;
      const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient, Fast: Boolean;
      const DC: HDC;
      const Width, Height: Integer;
      const R: TRect;
      const ClassType: TClass
    );

procedure DoRenderMock(
  const Wnd, StopWnd: HWnd;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TTEPaintCallback;
  const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient, Fast: Boolean;
  const DC: HDC;
  const Width, Height: Integer;
  const R: TRect;
  const ClassType: TClass
);
begin
  DoRenderMocks.DoRender(
    Wnd,
    StopWnd,
    WinControl,
    Flags,
    NonClientCallback,
    ClientCallback,
    IsMaximizedMDIClient,
    IsMaximizedMDIChild,
    IsMDIClient,
    DC,
    TSize.InlineCreate(Width, Height),
    R
  );
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TDoRender =
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback;
      const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
      const DC: HDC;
      const Size: TSize;
      const Rect: TRect
    );

procedure DoRenderMock(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
  const DC: HDC;
  const Size: TSize;
  const Rect: TRect
);
begin
  DoRenderMocks.DoRender(
    Wnd,
    StopWnd,
    WinControl,
    Flags,
    NonClientCallback,
    ClientCallback,
    IsMaximizedMDIClient,
    IsMaximizedMDIChild,
    IsMDIClient,
    DC,
    Size,
    Rect
  );
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

type
  TGetRegControl =
    procedure(
      const Wnd: HWND;
      const WinControl: TWinControl;
      var Flags: DWORD;
      var NonClientCallback, ClientCallback: TPaintCallback
    );

procedure GetRegControlMock(
  const Wnd: HWND;
  const WinControl: TWinControl;
  var Flags: DWORD;
  var NonClientCallback, ClientCallback: TPaintCallback
);
begin
  const RegControl = DoRenderMocks.GetRegControl(Wnd, WinControl, Flags, NonClientCallback, ClientCallback);

  Flags := RegControl.FFlags;

  NonClientCallback := RegControl.FNonClientCallback;
  ClientCallback    := RegControl.FClientCallback;
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TCheckClipRegion =
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TTEPaintCallback;
      const IsMaximizedMDIChild: Boolean;
      const DC: HDC;
      const CheckRegion: Boolean;
      const Width, Height: Integer;
      const Rect: TRect
    );

procedure CheckClipRegionMock(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TTEPaintCallback;
  const IsMaximizedMDIChild: Boolean;
  const DC: HDC;
  const CheckRegion: Boolean;
  const Width, Height: Integer;
  const Rect: TRect
);
begin
  DoRenderMocks.CheckClipRegion(
    Wnd,
    StopWnd,
    WinControl,
    Flags,
    NonClientCallback,
    ClientCallback,
    IsMaximizedMDIChild,
    DC,
    CheckRegion,
    TSize.InlineCreate(Width, Height)
  );
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TCheckClipRegion =
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback;
      const IsMaximizedMDIChild: Boolean;
      const DC: HDC;
      const CheckRegion: Boolean;
      const Size: TSize
    );

procedure CheckClipRegionMock(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const IsMaximizedMDIChild: Boolean;
  const DC: HDC;
  const CheckRegion: Boolean;
  const Size: TSize
); overload;
begin
  DoRenderMocks.CheckClipRegion(
    Wnd,
    StopWnd,
    WinControl,
    Flags,
    NonClientCallback,
    ClientCallback,
    IsMaximizedMDIChild,
    DC,
    CheckRegion,
    Size
  );
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TDoRenderMocks }

constructor TDoRenderMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TDoRender>(DoRenderExt, DoRenderMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TDoRender>(FormEffects.Rendering.DoRender, DoRenderMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetRegControl>(teRender.GetRegControl, GetRegControlMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetRegControl>(FormEffects.Rendering.GetRegControl, GetRegControlMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TCheckClipRegion>(CheckClipRegionExt, CheckClipRegionMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TCheckClipRegion>(FormEffects.Rendering.CheckClipRegion, CheckClipRegionMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  DoRenderMocks := Self;
end;

function TDoRenderMocks.GetRegControl(
  const Wnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TPaintCallback
): TRegControl;
begin
end;

{ TDoRenderMocks.TRegControl }

function TDoRenderMocks.TRegControl.AsValue: TValue;
begin
  Result := TValue.From<TRegControl>(Self);
end;

constructor TDoRenderMocks.TRegControl.Create(
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TPaintCallback
);
begin
  FFlags := Flags;

  FNonClientCallback := NonClientCallback;
  FClientCallback    := ClientCallback;
end;

{$ENDREGION 'TDoRenderMocks'}

{ TRenderWindowToDCAuxTests }

procedure TRenderWindowToDCAuxTests.RenderWindowToDCAux(const CheckVisibility: Boolean);
begin
  const Rect = TRect.InlineCreate(100, 55, 379, 177);

  const Flags            : DWORD          = 0;
  const NonClientCallback: TPaintCallback = nil;
  const ClientCallback   : TPaintCallback = nil;

  const CheckRegion     = False;
{$IFDEF USE_BILLENIUM_EFFECTS}
  TEAPIHooksDisabled := True;
  teRender.RenderWindowToDCAuxExt(
    FWnd,
    FStopWnd,
    0,
    FWinControl,
    Flags,
    NonClientCallback,
    ClientCallback,
    FDC,
    Rect,
    CheckVisibility,
    CheckRegion,
    True
  );
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.RenderWindowToDCAux(
    FWnd,
    FStopWnd,
    FWinControl,
    Flags,
    NonClientCallback,
    ClientCallback,
    FDC,
    Rect,
    CheckVisibility,
    CheckRegion
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
procedure NonClientCallbackMock(Control: TWinControl; DC: HDC);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
procedure NonClientCallbackMock(const WinControl: TWinControl; const DC: HDC);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
begin
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
procedure ClientCallbackMock(Control: TWinControl; DC: HDC);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
procedure ClientCallbackMock(const WinControl: TWinControl; const DC: HDC);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
begin
end;

procedure TRenderWindowToDCAuxTests.should_invoke_GetRegControl_and_DoRender_with_loaded_params;
begin
  const DoRenderMock     = TMock<TDoRenderMocks>.Create;
  const UtilsWindowsMock = TMock<TUtilsWindowsMocks>.Create;
  const UtilsFormsMock   = TMock<TUtilsFormsMocks>.Create;

  const TestSize         = TSize.InlineCreate(173, 131);
  const TestFlags: DWORD = $00001000;

  UtilsWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetWindowSize(It0.IsEqualTo<HWND>(FWnd), It1.IsAny<Boolean>);
  UtilsWindowsMock
    .Setup
    .WillReturn(TestSize)
    .When
    .GetWindowSize(It0.IsEqualTo<HWND>(FWnd), It1.IsAny<Boolean>);

  DoRenderMock
    .Setup
    .WillReturn(TDoRenderMocks.TRegControl.Create(TestFlags, NonClientCallbackMock, ClientCallbackMock).AsValue)
    .When
    .GetRegControl(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<TWinControl>(FWinControl),
      It2.IsEqualTo<DWORD>(0),
      It3.IsEqualTo<TPaintCallback>(nil),
      It4.IsEqualTo<TPaintCallback>(nil)
    );
  DoRenderMock
    .Setup
    .Expect
    .Once
    .When
    .DoRender(
      It( 0).IsEqualTo<HWND>(FWnd),
      It( 1).IsEqualTo<HWND>(FStopWnd),
      It( 2).IsEqualTo<TWinControl>(FWinControl),
      It( 3).IsEqualTo<DWORD>(TestFlags),
      It( 4).IsEqualTo<TPaintCallback>(NonClientCallbackMock),
      It( 5).IsEqualTo<TPaintCallback>(ClientCallbackMock),
      It( 6).IsEqualTo<Boolean>(True),
      It( 7).IsEqualTo<Boolean>(False),
      It( 8).IsEqualTo<Boolean>(False),
      It( 9).IsEqualTo<HDC>(FDC),
      It(10).IsEqualTo<TSize>(TestSize),
      It(11).IsEqualTo<TRect>(TRect.InlineCreate(100, 55, 379, 177))
    );

  UtilsFormsMock
    .Setup
    .WillReturn(True)
    .When
    .IsMaximizedMDIClient(It0.IsAny<string>);

  RenderWindowToDCAux;

  DoRenderMock.Verify;
  UtilsWindowsMock.Verify;
end;

procedure TRenderWindowToDCAuxTests.should_set_window_data_and_pos_for_main_form_if_wincontrol_is_mdi_with_mdi_children;
begin
  with TMock<TDoRenderMocks>.Create do
  begin
    const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
    const UtilsFormsMock    = TMock<TUtilsFormsMocks>.Create;

    const MainFormWnd: HWND = 1;
    const GetWindowLongPtrResult: LONG_PTR = $00000100;

    const ApplicationMock = TMock<TApplication>.Create;
    const MainFormMock    = TMock<TForm>.Create;
    const WinControlMock  = TMock<TWinControl>.Create;

    MainFormMock
      .Setup
      .WillReturn(MainFormWnd)
      .When
      .GetClientHandle;
    ApplicationMock
      .Setup
      .WillReturn(MainFormMock.Instance)
      .When
      .GetMainForm;

    Application := ApplicationMock.Instance;
    FWinControl := WinControlMock.Instance;

    UtilsFormsMock
      .Setup
      .Expect
      .Once
      .When
      .IsMDIFormWithMaximizedMDIChild(It0.IsEqualTo<TWinControl>(FWinControl));
    UtilsFormsMock
      .Setup
      .WillReturn(True)
      .When
      .IsMDIFormWithMaximizedMDIChild(It0.IsEqualTo<TWinControl>(FWinControl));

    WinapiWindowsMock
      .Setup
      .Expect
      .Once
      .When
      .GetWindowLongPtr(It0.IsEqualTo<HWND>(MainFormWnd), It1.IsEqualTo<Integer>(GWL_EXSTYLE));
    WinapiWindowsMock
      .Setup
      .WillReturn(GetWindowLongPtrResult)
      .When
      .GetWindowLongPtr(It0.IsEqualTo<HWND>(MainFormWnd), It1.IsEqualTo<Integer>(GWL_EXSTYLE));

    WinapiWindowsMock
      .Setup
      .Expect
      .Once
      .When
      .SetWindowLongPtr(
        It0.IsEqualTo<HWND>(MainFormWnd),
        It1.IsEqualTo<Integer>(GWL_EXSTYLE),
        It2.IsEqualTo<LONG_PTR>($0100)
      );

    WinapiWindowsMock
      .Setup
      .Expect
      .Once
      .When
      .SetWindowPos(
        It0.IsEqualTo<HWND>(MainFormWnd),
        It1.IsEqualTo<HWND>(0),
        It2.IsEqualTo<Integer>(0),
        It3.IsEqualTo<Integer>(0),
        It4.IsEqualTo<Integer>(0),
        It5.IsEqualTo<Integer>(0),
        It6.IsEqualTo<UINT>($0037)
      );

    RenderWindowToDCAux;

    UtilsFormsMock.Verify;
    WinapiWindowsMock.Verify;
  end;
end;

procedure TRenderWindowToDCAuxTests.should_stop_executing_when_CheckVisibility_and_no_visible_window;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const UtilsWindowsMock  = TMock<TUtilsWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .WillReturn(False)
    .When
    .IsWindowVisible(It0.IsAny<HWND>);

  UtilsWindowsMock
    .Setup
    .Expect
    .Never
    .When
    .GetWindowSize(It0.IsAny<HWND>, It1.IsAny<Boolean>);

  RenderWindowToDCAux(True);

  UtilsWindowsMock.Verify;
end;

procedure TRenderWindowToDCAuxTests.should_use_clipping_region_when_executing;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const DoRenderMock      = TMock<TDoRenderMocks>.Create;
  const UtilsWindowsMock  = TMock<TUtilsWindowsMocks>.Create;
  const UtilsFormsMock    = TMock<TUtilsFormsMocks>.Create;

  const TestRgn: HRGN = 1999;
  const TestSize      = TSize.InlineCreate(137, 59);

  const WinControlMock = TMock<TWinControl>.Create;

  FWinControl := WinControlMock.Instance;

  DoRenderMock
    .Setup
    .Expect
    .Once
    .When
    .CheckClipRegion(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<HWND>(FStopWnd),
      It2.IsEqualTo<TWinControl>(FWinControl),
      It3.IsAny<DWORD>,
      It4.IsEqualTo<TPaintCallback>(nil),
      It5.IsEqualTo<TPaintCallback>(nil),
      It6.IsEqualTo<Boolean>(True),
      It7.IsEqualTo<HDC>(FDC),
      It8.IsEqualTo<Boolean>(False),
      It9.IsEqualTo<TSize>(TestSize)
    );

  UtilsFormsMock
    .Setup
    .WillReturn(True)
    .When
    .IsMaximizedMDIChild(It0.IsAny<TWinControl>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetClipRgn(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<HRGN>(TestRgn));

  UtilsWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetWindowSize(It0.IsEqualTo<HWND>(FWnd), It1.IsEqualTo<Boolean>(True));
  UtilsWindowsMock
    .Setup
    .WillReturn(TestSize)
    .When
    .GetWindowSize(It0.IsEqualTo<HWND>(FWnd), It1.IsEqualTo<Boolean>(True));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SelectClipRgn(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<HRGN>(TestRgn));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(0),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(0)
    );
  WinapiWindowsMock
    .Setup
    .WillReturn(TestRgn)
    .When
    .CreateRectRgn(
      It0.IsEqualTo<Integer>(0),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(0)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DeleteObject(It0.IsEqualTo<HGDIOBJ>(TestRgn));

  RenderWindowToDCAux;

  DoRenderMock.Verify;
  UtilsWindowsMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TRenderWindowToDCAuxTests.Setup;
begin
  FWnd     := 199;
  FStopWnd := 299;
  FDC      := 399;
end;

procedure TRenderWindowToDCAuxTests.TearDown;
begin
  Application := nil;
  FWinControl := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TRenderWindowToDCAuxTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
