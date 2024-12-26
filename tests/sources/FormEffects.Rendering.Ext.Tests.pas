/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Ext.Tests.pas                        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Ext.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Themes,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Pictures.Mocks,
  FormEffects.Utils.Windows.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Rendering.Pictures.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks,
  FormEffects.Vcl.Themes.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{ TRenderingExtTests }

  [TestFixture]
  TRenderingExtTests = class
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;
    FUtilsPicturesMocks: TUtilsPicturesMocks;
    FUtilsWindowsMocks: TUtilsWindowsMocks;
    FRenderingPicturesMocks: TRenderingPicturesMocks;
    FVclThemesMocks: TVclThemesMocks;

    FWnd: HWND;
    FDC: HDC;

    FWinControl: TWinControl;

  strict private
    procedure PaintCopy;
    procedure NCPrintControl;
    procedure PaintThemeBorder;
    procedure EraseAndPaintMessage;
    procedure EmulatePaint;

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('PaintCopy должен прекратить выполнение, если WinControl = null', '')]
    procedure PaintCopy_should_stop_executing_when_wincontrol_is_null;

    [Test]
    [TestCase('PaintCopy должен отправить сообщение PAINT, если WinControl определён', '')]
    procedure PaintCopy_should_send_paint_message_for_wincontrol;

    [Test]
    [TestCase('NCPrintControl должен отправить сообщение PRINT с DC, если WinControl не является CustomForm', '')]
    procedure NCPrintControl_should_send_print_message_for_dc_if_wincontrol_is_not_custom_form;

    [Test]
    [TestCase('NCPrintControl должен отправить сообщение PRINT с DC, если CustomForm не является MDIChild', '')]
    procedure NCPrintControl_should_send_print_message_for_dc_if_customform_is_not_MDIChild;

    [Test]
    [TestCase('NCPrintControl должен отправить сообщение PRINT с DC, если Wnd не имеет региона', '')]
    procedure NCPrintControl_should_send_print_message_for_dc_if_wnd_has_not_region;

    [Test]
    [TestCase('NCPrintControl должен отправить сообщение PRINT с Canvas DC', '')]
    procedure NCPrintControl_should_send_print_message_for_canvasdc;

    [Test]
    [TestCase('PaintThemeBorder должен прекратить исполнение, если WinControl не имеет WS_EX_CLIENTEDGE флаг', '')]
    procedure PaintThemeBorder_should_stop_executing_when_no_WS_EX_CLIENTEDGE;

    [Test]
    [TestCase('PaintThemeBorder должен полностью выполниться, если WinControl имеет WS_EX_CLIENTEDGE флаг', '')]
    procedure PaintThemeBorder_should_executing_when_WS_EX_CLIENTEDGE;

    [Test]
    [TestCase('EraseAndPaintMessage должен изменить DoubleBuffered у WinControl, если DoubleBuffered = True',   'True')]
    [TestCase('EraseAndPaintMessage не должен менять DoubleBuffered у WinControl, если DoubleBuffered = False', 'False')]
    procedure EraseAndPaintMessage_should_check_WinControl_DoubleBuffered(const Value: Boolean);

    [Test]
    [TestCase('EraseAndPaintMessage должен отправить WM_ERASEBKGND и WM_PAINT сообщения',   'True')]
    procedure EraseAndPaintMessage_should_send_ERASEBKGND_and_PAINT_messages;

    [Test]
    [TestCase('EmulatePaint должен прекратить выполнение для не OleControl', '')]
    procedure EmulatePaint_should_stop_executing_when_no_OleControl;

    [Test]
    [TestCase('EmulatePaint должен вызвать OleDraw для OleControl', '')]
    procedure EmulatePaint_should_OleDraw_for_OleControl;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Winapi.Messages,
  System.Rtti,
  System.Types,
  System.SysUtils,
  Winapi.ActiveX,
{$IFNDEF USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures,
  FormEffects.Rendering.Ext,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Vcl.OleCtrls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$M+}

type
  IMock = interface(IUnknown)
  ['{0D0082BF-B86C-4409-8A48-920815B0CC96}']
    procedure Test;
  end;

{$M-}

{ TRenderingExtTests }

procedure TRenderingExtTests.EmulatePaint;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.EmulatePaint(FDC, FWinControl);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Ext.EmulatePaint(FWinControl, FDC);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TRenderingExtTests.EmulatePaint_should_OleDraw_for_OleControl;
begin
  var OleDrawInvoked := False;

  const TestClientRect = TRect.InlineCreate(199, 299, 499, 599);

  const WinControlMock = TMock<TOleControl>.Create;
  const OleObjectMock  = TMock<IMock>.Create;

  FWinControl    := WinControlMock.Instance;
  const OleObject = OleObjectMock.Instance;

  const ExpectedOle = TValue.From<IUnknown>(OleObject);

  WinControlMock
    .Setup
    .Expect
    .Once
    .When
    .HandleNeeded;
  WinControlMock
    .Setup
    .WillReturn(ExpectedOle)
    .When
    .GetOleObject;
  WinControlMock
    .Setup
    .WillReturn(TValue.From<TRect>(TestClientRect))
    .When
    .GetClientRect;

  FWinapiWindowsMocks.OleDraw_Result(
    procedure(const Unknown: IUnknown; const Aspect: LongInt; const DC: HDC; const Bounds: TRect)
    begin
      OleDrawInvoked := True;

      Assert.AreEqual<IUnknown>(OleObject, Unknown);
      Assert.AreEqual(DVASPECT_CONTENT, Aspect);
      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TestClientRect, Bounds);
    end
  );

  EmulatePaint;

  Assert.IsTrue(OleDrawInvoked);
  WinControlMock.Verify;
end;

procedure TRenderingExtTests.EmulatePaint_should_stop_executing_when_no_OleControl;
begin
  var OleDrawInvoked := False;

  FWinapiWindowsMocks.OleDraw_Result(
    procedure(const Unknown: IUnknown; const Aspect: Longint; const DC: HDC; const Bounds: TRect)
    begin
      OleDrawInvoked := True;
    end
  );

  EmulatePaint;

  Assert.IsFalse(OleDrawInvoked);
end;

procedure TRenderingExtTests.EraseAndPaintMessage;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.EraseAndPaintMessage(FDC, FWinControl, FWnd);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Ext.EraseAndPaintMessage(FWnd, FWinControl, FDC);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TRenderingExtTests.EraseAndPaintMessage_should_check_WinControl_DoubleBuffered(const Value: Boolean);
begin
  const WinControlMock  = TMock<TWinControl>.Create;

  if Value then
  begin
    const WinControlExpect =
      WinControlMock
        .Setup
        .Expect;
    WinControlExpect
      .Exactly(1)
      .When
      .SetDoubleBuffered(It0.IsEqualTo<Boolean>(False));
    WinControlExpect
      .Exactly(1)
      .When
      .SetDoubleBuffered(It0.IsEqualTo<Boolean>(True));
  end
  else
    WinControlMock
      .Setup
      .Expect
      .Never
      .When
      .SetDoubleBuffered(It0.IsAny<Boolean>);

  WinControlMock
    .Setup
    .WillReturn(Value)
    .When
    .GetDoubleBuffered;

  FWinControl := WinControlMock.Instance;

  EraseAndPaintMessage;

  WinControlMock.Verify;
end;

procedure TRenderingExtTests.EraseAndPaintMessage_should_send_ERASEBKGND_and_PAINT_messages;
begin
  var SaveDCInvoked    := False;
  var RestoreDCInvoked := False;

  var ERASEBKGNDMessageSending := False;
  var PAINTMessageSending      := False;

  const TestIndex = 2999;

  FWinapiWindowsMocks.SaveDC_Result(
    function(const DC: HDC): Integer
    begin
      SaveDCInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);

      Result := TestIndex;
    end
  );
  FWinapiWindowsMocks.SendMessage_Result(
    function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
    begin
      const ID = {$IFDEF USE_BILLENIUM_EFFECTS}BE_ID{$ELSE}FE_ID{$ENDIF};
      ERASEBKGNDMessageSending :=
        ERASEBKGNDMessageSending or (Msg = WM_ERASEBKGND) and (wParam = FDC) and (lParam = 0);
      PAINTMessageSending      :=
        PAINTMessageSending      or (Msg = WM_PAINT     ) and (wParam = FDC) and (lParam = ID);

      Assert.AreEqual<HWND>(FWnd, Wnd);

      Result := -1;
    end
  );
  FWinapiWindowsMocks.RestoreDC_Result(
    procedure(const DC: HDC; const Index: Integer)
    begin
      RestoreDCInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual(TestIndex, Index);
    end
  );

  EraseAndPaintMessage;

  Assert.IsTrue(SaveDCInvoked);
  Assert.IsTrue(RestoreDCInvoked);
  Assert.IsTrue(ERASEBKGNDMessageSending);
  Assert.IsTrue(PAINTMessageSending);
end;

procedure TRenderingExtTests.NCPrintControl;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.NCPrintControl(FDC, FWinControl, FWnd);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Ext.NCPrintControl(FWnd, FWinControl, FDC);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TRenderingExtTests.NCPrintControl_should_send_print_message_for_canvasdc;
begin
  var AdjustBitmapForTransitionIvoked := False;
  var SendMessageInvoked              := False;
  var BitBltInvoked                   := False;

  const CanvasDC: HDC = 1999;

  const WinControlWidth  = 151;
  const WinControlHeight = 67;

  const CanvasMock     = TMock<TCanvas>.Create;
  const BitmapMock     = TMock<TBitmap>.Create;
  const WinControlMock = TMock<TCustomForm>.Create;

  CanvasMock
    .Setup
    .WillReturn(CanvasDC)
    .When
    .GetHandle;
  CanvasMock
    .Setup
    .Expect
    .Exactly(1)
    .When
    .Lock;
  CanvasMock
    .Setup
    .Expect
    .Exactly(1)
    .When
    .Unlock;

  BitmapMock
    .Setup
    .WillReturn(CanvasMock.Instance)
    .When
    .GetCanvas;

  WinControlMock
    .Setup
    .WillReturn(fsMDIChild.AsValue)
    .When
    .GetFormStyle;
  WinControlMock
    .Setup
    .WillReturn(WinControlWidth)
    .When
    .GetWidth;
  WinControlMock
    .Setup
    .WillReturn(WinControlHeight)
    .When
    .GetHeight;

  FWinControl     := WinControlMock.Instance;
  const TestBitmap = BitmapMock.Instance;

  FUtilsWindowsMocks.HasWindowRegion_Result(True);
  FRenderingPicturesMocks.CreateBitmapFactory_Result(TestBitmap);
  FUtilsPicturesMocks.AdjustBitmapForTransition_Result(
    procedure(const Bitmap: TBitmap; const Palette: HPALETTE; const Size: TSize; const PixelFormat: TPixelFormat)
    begin
      AdjustBitmapForTransitionIvoked := True;

      Assert.AreEqual<TBitmap>(TestBitmap, Bitmap);
      Assert.AreEqual<HPALETTE>(0, Palette);
      Assert.AreEqual<TSize>(TSize.InlineCreate(WinControlWidth, WinControlHeight), Size);
      Assert.AreEqual<TPixelFormat>(pfDevice, PixelFormat);
    end
  );
  FWinapiWindowsMocks.SendMessage_Result(
    function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
    begin
      SendMessageInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.AreEqual<UINT>(WM_PRINT, Msg);
      Assert.IsTrue(CanvasDC = wParam);
      Assert.IsTrue(PRF_NONCLIENT = lParam);

      Result := 0;
    end
  );
  FWinapiWindowsMocks.BitBlt_Result(
    procedure(
      const DestDC: HDC;
      const X, Y, Width, Height: Integer;
      const SrcDC: HDC;
      const XSrc, YSrc: Integer;
      const Rop: DWORD
    )
    begin
      BitBltInvoked := True;

      Assert.AreEqual<HDC>(FDC, DestDC);
      Assert.AreEqual(0, X);
      Assert.AreEqual(0, Y);
      Assert.AreEqual(WinControlWidth, Width);
      Assert.AreEqual(WinControlHeight, Height);
      Assert.AreEqual<HDC>(CanvasDC, SrcDC);
      Assert.AreEqual(0, XSrc);
      Assert.AreEqual(0, YSrc);
      Assert.AreEqual(SRCCOPY, Rop);
    end
  );

  Assert.AreNotEqual<HDC>(FDC, CanvasDC);

  NCPrintControl;

  Assert.IsTrue(AdjustBitmapForTransitionIvoked);
  Assert.IsTrue(SendMessageInvoked);
  Assert.IsTrue(BitBltInvoked);
  CanvasMock.Verify;
end;

procedure TRenderingExtTests.NCPrintControl_should_send_print_message_for_dc_if_customform_is_not_MDIChild;
begin
  var SendMessageInvoked := False;

  const WinControlMock = TMock<TCustomForm>.Create;

  FWinControl := WinControlMock.Instance;

  FWinapiWindowsMocks.SendMessage_Result(
    function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
    begin
      SendMessageInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.AreEqual<UINT>(WM_PRINT, Msg);
      Assert.IsTrue(FDC = wParam);
      Assert.IsTrue(PRF_NONCLIENT = lParam);

      Result := 0;
    end
  );

  NCPrintControl;

  Assert.IsTrue(SendMessageInvoked);
end;

procedure TRenderingExtTests.NCPrintControl_should_send_print_message_for_dc_if_wincontrol_is_not_custom_form;
begin
  var SendMessageInvoked := False;

  FWinapiWindowsMocks.SendMessage_Result(
    function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
    begin
      SendMessageInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.AreEqual<UINT>(WM_PRINT, Msg);
      Assert.IsTrue(FDC = wParam);
      Assert.IsTrue(PRF_NONCLIENT = lParam);

      Result := 0;
    end
  );

  NCPrintControl;

  Assert.IsTrue(SendMessageInvoked);
end;

procedure TRenderingExtTests.NCPrintControl_should_send_print_message_for_dc_if_wnd_has_not_region;
begin
  var HasWindowRegion    := False;
  var SendMessageInvoked := False;

  const WinControlMock = TMock<TCustomForm>.Create;

  WinControlMock
    .Setup
    .WillReturn(fsMDIChild.AsValue)
    .When
    .GetFormStyle;

  FWinControl := WinControlMock.Instance;

  FUtilsWindowsMocks.HasWindowRegion_Result(
    function(const Wnd: HWND): Boolean
    begin
      HasWindowRegion := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);

      Result := False;
    end
  );
  FWinapiWindowsMocks.SendMessage_Result(
    function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
    begin
      SendMessageInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.AreEqual<UINT>(WM_PRINT, Msg);
      Assert.IsTrue(FDC = wParam);
      Assert.IsTrue(PRF_NONCLIENT = lParam);

      Result := 0;
    end
  );

  NCPrintControl;

  Assert.IsTrue(HasWindowRegion);
  Assert.IsTrue(SendMessageInvoked);
end;

procedure TRenderingExtTests.PaintCopy;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.PaintCopy(FDC, FWinControl);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Ext.PaintCopy(FWinControl, FDC);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TRenderingExtTests.PaintCopy_should_send_paint_message_for_wincontrol;
begin
  var SendMessageInvoked := False;

  const TestWnd: HWND = 199;

  const WinControlMock = TMock<TWinControl>.Create;

  WinControlMock
    .Setup
    .WillReturn(TestWnd)
    .When
    .GetHandle;
  WinControlMock
    .Setup
    .WillReturn(TValue.From<TControlState>([csDestroyingHandle]))
    .When
    .GetControlState;
  WinControlMock
    .Setup
    .Expect
    .Exactly(1)
    .When
    .SetControlState(It0.IsEqualTo<TControlState>([csDestroyingHandle, csPaintCopy]));
  WinControlMock
    .Setup
    .Expect
    .Exactly(1)
    .When
    .SetControlState(It0.IsEqualTo<TControlState>([csDestroyingHandle]));

  FWinControl := WinControlMock.Instance;

  FWinapiWindowsMocks.SendMessage_Result(
    function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
    begin
      SendMessageInvoked := True;

      const ID = {$IFDEF USE_BILLENIUM_EFFECTS}BE_ID{$ELSE}FE_ID{$ENDIF};
      Assert.AreEqual<HWND>(TestWnd, Wnd);
      Assert.AreEqual<UINT>(WM_PAINT, Msg);
      Assert.AreEqual<HDC>(FDC, wParam);
      Assert.IsTrue(ID = lParam);

      Result := 0;
    end
  );

  PaintCopy;

  WinControlMock.Verify;
  Assert.IsTrue(SendMessageInvoked);
end;

procedure TRenderingExtTests.PaintCopy_should_stop_executing_when_wincontrol_is_null;
begin
  var SendMessageInvoked := False;

  FWinapiWindowsMocks.SendMessage_Result(
    function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
    begin
      SendMessageInvoked := True;

      Result := 0;
    end
  );

  PaintCopy;

  Assert.IsFalse(SendMessageInvoked);
end;

procedure TRenderingExtTests.PaintThemeBorder;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.PaintThemeBorderExt(FWinControl, FDC, False);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Ext.PaintThemeBorder(FWinControl, FDC);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TRenderingExtTests.PaintThemeBorder_should_executing_when_WS_EX_CLIENTEDGE;
begin
  var GetWindowRectInvoked   := False;
  var ExcludeClipRectInvoked := False;

  const ThemedElementDetails = TThemedElementDetails.Create(teTab, 99, 339);

  const WinControlMock   = TMock<TWinControl>.Create;
  const StyleServiceMock = TMock<TCustomStyleServices>.Create;

  WinControlMock
    .Setup
    .WillReturn(FWnd)
    .When
    .GetHandle;

  StyleServiceMock
    .Setup
    .WillReturn(ThemedElementDetails.AsValue)
    .When
    .GetElementDetails(It0.IsEqualTo<TThemedEdit>(teEditTextNormal));
  StyleServiceMock
    .Setup
    .Expect
    .Once
    .When
    .DrawElement(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TThemedElementDetails>(ThemedElementDetails),
      It2.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 66, 66))
    );

  FWinControl       := WinControlMock.Instance;
  const StyleService = StyleServiceMock.Instance;

  FWinapiWindowsMocks.GetWindowLongPtr_Result(WS_EX_CLIENTEDGE);
  FWinapiWindowsMocks.GetWindowRect_Result(
    function(const Wnd: HWND): TRect
    begin
      GetWindowRectInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);

      Result := TRect.InlineCreate(133, 57, 199, 123);
    end
  );
  FWinapiWindowsMocks.ExcludeClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      ExcludeClipRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual( 2, Left);
      Assert.AreEqual( 2, Top);
      Assert.AreEqual(64, Right);
      Assert.AreEqual(64, Bottom);
    end
  );
  FVclThemesMocks.StyleServices_Result(StyleService);

  PaintThemeBorder;

  Assert.IsTrue(GetWindowRectInvoked);
  Assert.IsTrue(ExcludeClipRectInvoked);
  StyleServiceMock.Verify;
end;

procedure TRenderingExtTests.PaintThemeBorder_should_stop_executing_when_no_WS_EX_CLIENTEDGE;
begin
  var GetWindowLongPtrInvoked := False;
  var GetWindowRectInvoked    := False;

  const WinControlMock = TMock<TWinControl>.Create;

  WinControlMock
    .Setup
    .WillReturn(FWnd)
    .When
    .GetHandle;

  FWinControl := WinControlMock.Instance;

  FWinapiWindowsMocks.GetWindowLongPtr_Result(
    function(const Wnd: HWND; const Index: Integer): LONG_PTR
    begin
      GetWindowLongPtrInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);
      Assert.AreEqual(GWL_EXSTYLE, Index);

      Result := 0;
    end
  );
  FWinapiWindowsMocks.GetWindowRect_Result(
    function(const Wnd: HWND): TRect
    begin
      GetWindowRectInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);

      Result := TRect.Zero;
    end
  );

  PaintThemeBorder;

  Assert.IsTrue(GetWindowLongPtrInvoked);
  Assert.IsFalse(GetWindowRectInvoked);
end;

procedure TRenderingExtTests.Setup;
begin
  FWinapiWindowsMocks      := TWinapiWindowsMocks.Create;
  FUtilsPicturesMocks      := TUtilsPicturesMocks.Create;
  FUtilsWindowsMocks       := TUtilsWindowsMocks.Create;
  FRenderingPicturesMocks  := TRenderingPicturesMocks.Create;
  FVclThemesMocks          := TVclThemesMocks.Create;

  FWnd := 111;
  FDC  := 501;
end;

procedure TRenderingExtTests.TearDown;
begin
  FWinControl := nil;

  FreeAndNil(FVclThemesMocks);
  FreeAndNil(FRenderingPicturesMocks);
  FreeAndNil(FUtilsWindowsMocks);
  FreeAndNil(FUtilsPicturesMocks);
  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TRenderingExtTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
