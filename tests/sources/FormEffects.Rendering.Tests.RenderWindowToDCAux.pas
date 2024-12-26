/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.RenderWindowToDCAux.pas        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.RenderWindowToDCAux;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Utils.Windows.Mocks,
  FormEffects.Utils.Forms.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{$REGION 'TDoRenderMocks'}

{ TDoRenderMocks }

  TDoRenderMocks = class(TMocksManager)
  private type
    TDoRenderResultReference =
      reference to procedure(
        const Wnd, StopWnd: HWND;
        const WinControl: TWinControl;
        const Flags: DWORD;
        const NonClientCallback, ClientCallback: TPaintCallback;
        const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
        const DC: HDC;
        const Size: TSize;
        const Rect: TRect
      );
  public
    procedure DoRender_Result(const Callback: TDoRenderResultReference);

  private type
    TGetRegControlResultReference =
      reference to procedure(
        const Wnd: HWND;
        const WinControl: TWinControl;
        var Flags: DWORD;
        var NonClientCallback, ClientCallback: TPaintCallback
      );
  public
    procedure GetRegControl_Result(const Callback: TGetRegControlResultReference);

  private type
    TCheckClipRegionResultReference =
      reference to procedure(
        const Wnd, StopWnd: HWND;
        const WinControl: TWinControl;
        const Flags: DWORD;
        const NonClientCallback, ClientCallback: TPaintCallback;
        const IsMaximizedMDIChild: Boolean;
        const DC: HDC;
        const CheckRegion: Boolean;
        const Size: TSize
      );
  public
    procedure CheckClipRegion_Result(const Callback: TCheckClipRegionResultReference);

  public
    constructor Create; override;
  end;

{$ENDREGION 'TDoRenderMocks'}

{ TRenderWindowToDCAuxTests }

  TRenderWindowToDCAuxTests = class
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;
    FUtilsWindowsMocks: TUtilsWindowsMocks;
    FUtilsFormsMocks: TUtilsFormsMocks;
    FDoRenderMocks: TDoRenderMocks;

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
  System.Types,
  System.SysUtils,
  FormEffects.TypeHelpers,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$REGION 'TDoRenderMocks'}

var
  DoRenderResultReference: TDoRenderMocks.TDoRenderResultReference = nil;

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
  DoRenderResultReference(
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
  DoRenderResultReference(
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

procedure TDoRenderMocks.DoRender_Result(const Callback: TDoRenderResultReference);
begin
  if Assigned(Callback) then
    DoRenderResultReference := Callback
  else
  begin
    DoRenderResultReference :=
      procedure(
        const Wnd, StopWnd: HWND;
        const WinControl: TWinControl;
        const Flags: DWORD;
        const NonClientCallback, ClientCallback: TPaintCallback;
        const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
        const DC: HDC;
        const Size: TSize;
        const Rect: TRect
      )
      begin
      end;
  end;
end;

var
  GetRegControlResultReference: TDoRenderMocks.TGetRegControlResultReference = nil;

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
  GetRegControlResultReference(Wnd, WinControl, Flags, NonClientCallback, ClientCallback);
end;

procedure TDoRenderMocks.GetRegControl_Result(const Callback: TGetRegControlResultReference);
begin
  if Assigned(Callback) then
    GetRegControlResultReference := Callback
  else
  begin
    GetRegControlResultReference :=
      procedure(
        const Wnd: HWND;
        const WinControl: TWinControl;
        var Flags: DWORD;
        var NonClientCallback, ClientCallback: TPaintCallback
      )
      begin
      end;
  end;
end;

var
  CheckClipRegionResultReference: TDoRenderMocks.TCheckClipRegionResultReference = nil;

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
  CheckClipRegionResultReference(
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
  CheckClipRegionResultReference(
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

procedure TDoRenderMocks.CheckClipRegion_Result(const Callback: TCheckClipRegionResultReference);
begin
  if Assigned(Callback) then
    CheckClipRegionResultReference := Callback
  else
  begin
    CheckClipRegionResultReference :=
      procedure(
        const Wnd, StopWnd: HWND;
        const WinControl: TWinControl;
        const Flags: DWORD;
        const NonClientCallback, ClientCallback: TPaintCallback;
        const IsMaximizedMDIChild: Boolean;
        const DC: HDC;
        const CheckRegion: Boolean;
        const Size: TSize
      )
      begin
      end;
  end;
end;

{ TDoRenderMocks }

constructor TDoRenderMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TDoRender>(DoRenderExt, DoRenderMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TDoRender>(DoRender, DoRenderMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  DoRender_Result(nil);

  AddIntercept<TGetRegControl>(GetRegControl, GetRegControlMock);
  GetRegControl_Result(nil);

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TCheckClipRegion>(CheckClipRegionExt, CheckClipRegionMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TCheckClipRegion>(CheckClipRegion, CheckClipRegionMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  CheckClipRegion_Result(nil);
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
  var GetRegControlInvoke := False;
  var DoRenderInvoked     := False;

  const TestSize         = TSize.InlineCreate(173, 131);
  const TestFlags: DWORD = $00001000;

  FUtilsFormsMocks.IsMaximizedMDIClient_Result(True);
  FUtilsWindowsMocks.GetWindowSize_Result(TestSize);
  FDoRenderMocks.GetRegControl_Result(
    procedure(
      const Wnd: HWND;
      const WinControl: TWinControl;
      var Flags: DWORD;
      var NonClientCallback, ClientCallback: TPaintCallback
    )
    begin
      GetRegControlInvoke := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.AreEqual<TWinControl>(FWinControl, WinControl);
      Assert.AreEqual<DWORD>(0, Flags);
      Assert.IsFalse(Assigned(NonClientCallback));
      Assert.IsFalse(Assigned(ClientCallback));

      Flags := TestFlags;

      NonClientCallback := NonClientCallbackMock;
      ClientCallback    := ClientCallbackMock;
    end
  );
  FDoRenderMocks.DoRender_Result(
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
      const DC: HDC;
      const Size: TSize;
      const Rect: TRect
    )
    begin
      DoRenderInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.AreEqual<HWND>(FStopWnd, StopWnd);
      Assert.AreEqual<TWinControl>(FWinControl, WinControl);
      Assert.AreEqual<DWORD>(TestFlags, Flags);
      Assert.AreEqual<TPaintCallback>(NonClientCallbackMock, NonClientCallback);
      Assert.AreEqual<TPaintCallback>(ClientCallbackMock, ClientCallback);
      Assert.IsTrue(IsMaximizedMDIClient);
      Assert.IsFalse(IsMaximizedMDIChild);
      Assert.IsFalse(IsMDIClient);
      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TSize>(TestSize, Size);
      Assert.AreEqual<TRect>(TRect.InlineCreate(100, 55, 379, 177), Rect);
    end
  );

  RenderWindowToDCAux;

  Assert.IsTrue(GetRegControlInvoke);
  Assert.IsTrue(DoRenderInvoked);
end;

procedure TRenderWindowToDCAuxTests.should_set_window_data_and_pos_for_main_form_if_wincontrol_is_mdi_with_mdi_children;
begin
  var IsMDIFormWithMaximizedMDIChildInvoked := False;
  var GetWindowLongPtrInvoked               := False;
  var SetWindowLongPtrInvoked               := False;
  var SetWindowPosInvoked                   := False;

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

  FUtilsFormsMocks.IsMDIFormWithMaximizedMDIChild_Result(
    function(const WinControl: TWinControl): Boolean
    begin
      IsMDIFormWithMaximizedMDIChildInvoked := True;

      Assert.AreEqual<TWinControl>(FWinControl, WinControl);

      Result := True;
    end
  );
  FWinapiWindowsMocks.GetWindowLongPtr_Result(
    function(const Wnd: HWND; const Index: Integer): LONG_PTR
    begin
      GetWindowLongPtrInvoked := True;

      Assert.AreEqual<HWND>(MainFormWnd, Wnd);
      Assert.AreEqual(GWL_EXSTYLE, Index);

      Result := GetWindowLongPtrResult;

    end
  );
  FWinapiWindowsMocks.SetWindowLongPtr_Result(
    function(const Wnd: HWND; const Index: Integer; const NewLong: LONG_PTR): LONG_PTR
    begin
      SetWindowLongPtrInvoked := True;

      Assert.AreEqual<HWND>(MainFormWnd, Wnd);
      Assert.AreEqual(GWL_EXSTYLE, Index);
      Assert.AreEqual<LONG_PTR>($00000100, NewLong);

      Result := GetWindowLongPtrResult;
    end
  );
  FWinapiWindowsMocks.SetWindowPos_Result(
    procedure(const Wnd, InsertAfterWnd: HWND; const Left, Top, Width, Height: Integer; const Flags: UINT)
    begin
      SetWindowPosInvoked := True;

      Assert.AreEqual<HWND>(MainFormWnd, Wnd);
      Assert.AreEqual<HWND>(0, InsertAfterWnd);
      Assert.AreEqual<TRect>(TRect.Zero, TRect.InlineCreate(Left, Top, Width, Height));
      Assert.AreEqual<UINT>($00000037, Flags);
    end
  );

  RenderWindowToDCAux;

  Assert.IsTrue(IsMDIFormWithMaximizedMDIChildInvoked);
  Assert.IsTrue(GetWindowLongPtrInvoked);
  Assert.IsTrue(SetWindowLongPtrInvoked);
  Assert.IsTrue(SetWindowPosInvoked);
end;

procedure TRenderWindowToDCAuxTests.should_stop_executing_when_CheckVisibility_and_no_visible_window;
begin
  var GetWindowSizeInvoked := False;

  FWinapiWindowsMocks.IsWindowVisible_Result(False);

  FUtilsWindowsMocks.GetWindowSize_Result(
    function(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize
    begin
      GetWindowSizeInvoked := True;

      Result := TSize.Zero;
    end
  );

  RenderWindowToDCAux(True);

  Assert.IsFalse(GetWindowSizeInvoked);
end;

procedure TRenderWindowToDCAuxTests.should_use_clipping_region_when_executing;
begin
  const TestRgn: HRGN = 1999;
  const TestSize      = TSize.InlineCreate(137, 59);

  var CreateRectRgnInvoked   := False;
  var GetClipRgnInvoked      := False;
  var GetWindowSizeInvoked   := False;
  var CheckClipRegionInvoked := False;
  var SelectClipRgnInvoked   := False;
  var DeleteObjectInvoked    := False;

  const WinControlMock = TMock<TWinControl>.Create;

  FWinControl := WinControlMock.Instance;

  FUtilsFormsMocks.IsMaximizedMDIChild_Result(True);
  FWinapiWindowsMocks.CreateRectRgn_Result(
    function(const Left, Top, Right, Bottom: Integer): HRGN
    begin
      CreateRectRgnInvoked := True;

      Assert.AreEqual<TRect>(TRect.Zero, TRect.InlineCreate(Left, Top, Right, Bottom));

      Result := TestRgn;
    end
  );
  FWinapiWindowsMocks.GetClipRgn_Result(
    procedure(const DC: HDC; const Rgn: HRGN)
    begin
      GetClipRgnInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<HRGN>(TestRgn, Rgn);
    end
  );
  FUtilsWindowsMocks.GetWindowSize_Result(
    function(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize
    begin
      GetWindowSizeInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.IsTrue(IsMaximizedMDIChild);

      Result := TestSize;
    end
  );
  FDoRenderMocks.CheckClipRegion_Result(
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const IsMaximizedMDIChild: Boolean;
      const DC: HDC;
      const CheckRegion: Boolean;
      const Size: TSize
    )
    begin
      CheckClipRegionInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.AreEqual<HWND>(FStopWnd, StopWnd);
      Assert.AreEqual<TWinControl>(FWinControl, WinControl);
      Assert.IsTrue(IsMaximizedMDIChild);
      Assert.AreEqual<HDC>(FDC, DC);
      Assert.IsFalse(CheckRegion);
      Assert.AreEqual<TSize>(TestSize, Size);
    end
  );
  FWinapiWindowsMocks.SelectClipRgn_Result(
    procedure(const DC: HDC; const Rgn: HRGN)
    begin
      SelectClipRgnInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<HRGN>(TestRgn, Rgn);
    end
  );
  FWinapiWindowsMocks.DeleteObject_Result(
    procedure(const Obj: HGDIOBJ)
    begin
      DeleteObjectInvoked := True;

      Assert.AreEqual<HRGN>(TestRgn, Obj);
    end
  );

  RenderWindowToDCAux;

  Assert.IsTrue(CreateRectRgnInvoked);
  Assert.IsTrue(GetClipRgnInvoked);
  Assert.IsTrue(GetWindowSizeInvoked);
  Assert.IsTrue(CheckClipRegionInvoked);
  Assert.IsTrue(SelectClipRgnInvoked);
  Assert.IsTrue(DeleteObjectInvoked);
end;

procedure TRenderWindowToDCAuxTests.Setup;
begin
  FWinapiWindowsMocks := TWinapiWindowsMocks.Create;
  FUtilsWindowsMocks  := TUtilsWindowsMocks.Create;
  FUtilsFormsMocks    := TUtilsFormsMocks.Create;
  FDoRenderMocks      := TDoRenderMocks.Create;

  FWnd     := 199;
  FStopWnd := 299;
  FDC      := 399;
end;

procedure TRenderWindowToDCAuxTests.TearDown;
begin
  Application := nil;
  FWinControl := nil;

  FreeAndNil(FDoRenderMocks);
  FreeAndNil(FUtilsFormsMocks);
  FreeAndNil(FUtilsWindowsMocks);
  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TRenderWindowToDCAuxTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
