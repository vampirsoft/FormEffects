/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.RenderWindowToDC.pas           *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.RenderWindowToDC;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Utils.Mocks,
  FormEffects.Utils.Windows.Mocks,
  FormEffects.Utils.Forms.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{$REGION 'TRenderWindowToDCAuxMocks'}

{ TRenderWindowToDCAuxMocks }

  TRenderWindowToDCAuxMocks = class(TMocksManager)
  private type
    TRenderWindowToDCAuxResultReference =
      reference to procedure(
        const Wnd, StopWnd: HWND;
        const WinControl: TWinControl;
        const Flags: DWORD;
        const NonClientCallback, ClientCallback: TPaintCallback;
        const DC: HDC;
        const Rect: TRect;
        const CheckVisibility, CheckRegion: Boolean
      );
  public
    procedure RenderWindowToDCAux_Result(const Callback: TRenderWindowToDCAuxResultReference);

  public
    constructor Create; override;
  end;

{$ENDREGION 'TRenderWindowToDCAuxMocks'}

{ TRenderWindowToDCTests }

  [TestFixture]
  TRenderWindowToDCTests = class
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;
    FUtilsWindowsMocks: TUtilsWindowsMocks;
    FUtilsFormsMocks: TUtilsFormsMocks;
    FVclControlsMocks: TVclControlsMocks;
    FRenderWindowToDCAuxMocks: TRenderWindowToDCAuxMocks;

    FWnd: HWND;
    FStopWnd: HWND;
    FDC: HDC;
    FWinControl: TWinControl;

  strict private
    procedure RenderWindowToDC(const ClientCoordinates: Boolean = False; const CheckVisibility: Boolean = False); inline;

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
  System.Types,
  System.SysUtils;

{$IFDEF FORM_EFFECTS_TESTS}

{$REGION 'TRenderWindowToDCAuxMocks'}

var
  RenderWindowToDCAuxResultReference: TRenderWindowToDCAuxMocks.TRenderWindowToDCAuxResultReference = nil;

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
  RenderWindowToDCAuxResultReference(
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
  RenderWindowToDCAuxResultReference(
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

procedure TRenderWindowToDCAuxMocks.RenderWindowToDCAux_Result(const Callback: TRenderWindowToDCAuxResultReference);
begin
  if Assigned(Callback) then
    RenderWindowToDCAuxResultReference := Callback
  else
  begin
    RenderWindowToDCAuxResultReference :=
      procedure(
        const Wnd, StopWnd: HWND;
        const WinControl: TWinControl;
        const Flags: DWORD;
        const NonClientCallback, ClientCallback: TPaintCallback;
        const DC: HDC;
        const Rect: TRect;
        const CheckVisibility, CheckRegion: Boolean
      )
      begin
      end;
  end;
end;

{ TRenderWindowToDCAuxMocks }

constructor TRenderWindowToDCAuxMocks.Create;
begin
  inherited Create;

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TRenderWindowToDCAux>(RenderWindowToDCAuxExt, RenderWindowToDCAuxMock);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  AddIntercept<TRenderWindowToDCAux>(RenderWindowToDCAux, RenderWindowToDCAuxMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  RenderWindowToDCAux_Result(nil);
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
  var IsMaximizedMDIChildInvoked := False;
  var GetWindowOffsetInvoked     := False;
  var OffsetWindowOrgExInvoked   := False;
  var RenderWindowToDCAuxInvoked := False;
  var SetWindowOrgExInvoked      := False;

  var WindowOffset := TPoint.Zero;
  if Expected then
    WindowOffset := TPoint.InlineCreate(141, 77);

  FUtilsFormsMocks.IsMaximizedMDIChild_Result(
    function(const WinControl: TWinControl): Boolean
    begin
      IsMaximizedMDIChildInvoked := True;

      Result := False;
    end
  );
  FUtilsWindowsMocks.GetWindowOffset_Result(
    function(const Wnd: HWND): TPoint
    begin
      GetWindowOffsetInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);

      Result := WindowOffset;
    end
  );
  FWinapiWindowsMocks.OffsetWindowOrgEx_Result(
    function(const DC: HDC; const X, Y: Integer): TPoint
    begin
      OffsetWindowOrgExInvoked := True;

      Result := TPoint.InlineCreate(X, Y);

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TPoint>(WindowOffset, Result);
    end
  );
  FRenderWindowToDCAuxMocks.RenderWindowToDCAux_Result(
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const DC: HDC;
      const Rect: TRect;
      const CheckVisibility, CheckRegion: Boolean
    )
    begin
      RenderWindowToDCAuxInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.AreEqual<TWinControl>(FWinControl, WinControl);
      Assert.AreEqual<HDC>(FDC, DC);
      Assert.IsFalse(CheckVisibility);
      Assert.IsTrue(CheckRegion);
      if Expected then
        Assert.AreEqual<TRect>(TRect.InlineCreate(241, 132, 520, 254), Rect)
      else
        Assert.AreEqual<TRect>(TRect.InlineCreate(100, 55, 379, 177), Rect);
    end
  );
  FWinapiWindowsMocks.SetWindowOrgEx_Result(
    procedure(const DC: HDC; const X, Y: Integer)
    begin
      SetWindowOrgExInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TPoint>(WindowOffset, TPoint.InlineCreate(X, Y));
    end
  );

  RenderWindowToDC(True);

  Assert.IsTrue(IsMaximizedMDIChildInvoked);
  Assert.IsTrue(GetWindowOffsetInvoked);
  Assert.IsTrue(OffsetWindowOrgExInvoked);
  Assert.IsTrue(RenderWindowToDCAuxInvoked);
  Assert.AreEqual(Expected, SetWindowOrgExInvoked);
end;

procedure TRenderWindowToDCTests.should_override_stopwnd(const IsChild: Boolean);
begin
  var IsChildInvoked             := False;
  var RenderWindowToDCAuxInvoked := False;

  FWinapiWindowsMocks.IsChild_Result(
    function(const ParentWnd, Wnd: HWND): Boolean
    begin
      IsChildInvoked := True;

      Assert.AreEqual<HWND>(FWnd, ParentWnd);
      Assert.AreEqual<HWND>(FStopWnd, Wnd);

      Result := IsChild;
    end
  );
  FRenderWindowToDCAuxMocks.RenderWindowToDCAux_Result(
    procedure(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TPaintCallback;
      const DC: HDC;
      const Rect: TRect;
      const CheckVisibility, CheckRegion: Boolean
    )
    begin
      RenderWindowToDCAuxInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      if IsChild then
        Assert.AreEqual<HWND>(FStopWnd, StopWnd)
      else
        Assert.AreEqual<HWND>(0, StopWnd);
    end
  );

  RenderWindowToDC;

  Assert.IsTrue(IsChildInvoked);
  Assert.IsTrue(RenderWindowToDCAuxInvoked);
end;

procedure TRenderWindowToDCTests.should_stop_executing_if_wnd_not_visible_when_CheckVisibility;
begin
  var IsWindowVisibleInvoked     := False;
  var IsMaximizedMDIChildInvoked := False;

  FWinapiWindowsMocks.IsWindowVisible_Result(
    function(const Wnd: HWND): BOOL
    begin
      IsWindowVisibleInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);

      Result := Wnd = FStopWnd;
    end
  );

  FUtilsFormsMocks.IsMaximizedMDIChild_Result(
    function(const WinControl: TWinControl): Boolean
    begin
      IsMaximizedMDIChildInvoked := True;

      Result := False;
    end
  );

  RenderWindowToDC(True, True);

  Assert.IsTrue(IsWindowVisibleInvoked);
  Assert.IsFalse(IsMaximizedMDIChildInvoked);
end;

procedure TRenderWindowToDCTests.should_stop_executing_when_wnd_equal_stopwnd;
begin
  var IsMaximizedMDIChildInvoked := False;

  FStopWnd := FWnd;

  FUtilsFormsMocks.IsMaximizedMDIChild_Result(
    function(const WinControl: TWinControl): Boolean
    begin
      IsMaximizedMDIChildInvoked := True;

      Result := False;
    end
  );

  RenderWindowToDC(True);

  Assert.IsFalse(IsMaximizedMDIChildInvoked);
end;

procedure TRenderWindowToDCTests.Setup;
begin
  FWinapiWindowsMocks       := TWinapiWindowsMocks.Create;
  FUtilsWindowsMocks        := TUtilsWindowsMocks.Create;
  FUtilsFormsMocks          := TUtilsFormsMocks.Create;
  FVclControlsMocks         := TVclControlsMocks.Create;
  FRenderWindowToDCAuxMocks := TRenderWindowToDCAuxMocks.Create;

  FWnd     := 199;
  FStopWnd := 299;
  FDC      := 399;
end;

procedure TRenderWindowToDCTests.TearDown;
begin
  FWinControl := nil;

  FreeAndNil(FRenderWindowToDCAuxMocks);
  FreeAndNil(FVclControlsMocks);
  FreeAndNil(FUtilsFormsMocks);
  FreeAndNil(FUtilsWindowsMocks);
  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TRenderWindowToDCTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
