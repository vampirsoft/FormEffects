/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Ext.Tests.pas                        *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
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
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TRenderingExtTests }

  [TestFixture]
  TRenderingExtTests = class
  strict private
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
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Constants,
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
{$IFNDEF USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures,
  FormEffects.Rendering.Ext,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Utils.Windows.Mocks,
  FormEffects.Utils.Pictures.Mocks,
  FormEffects.Vcl.OleCtrls.Mocks,
  FormEffects.Vcl.Themes.Mocks,
  FormEffects.Rendering.Pictures.Mocks;

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
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

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
    .WillReturn(TestClientRect)
    .When
    .GetClientRect;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .OleDraw(
      It0.IsEqualTo<IUnknown>(OleObject),
      It1.IsEqualTo<LongInt>(DVASPECT_CONTENT),
      It2.IsEqualTo<HDC>(FDC),
      It3.IsEqualTo<TRect>(TestClientRect)
    );

  EmulatePaint;

  WinControlMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TRenderingExtTests.EmulatePaint_should_stop_executing_when_no_OleControl;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Never
    .When
    .OleDraw(
      It0.IsAny<IUnknown>,
      It1.IsAny<LongInt>,
      It2.IsAny<HDC>,
      It3.IsAny<TRect>
    );

  EmulatePaint;

  WinapiWindowsMock.Verify;
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
  const WinControlMock = TMock<TWinControl>.Create;

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
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const TestIndex = 2999;

  const ID = {$IFDEF USE_BILLENIUM_EFFECTS}BE_ID{$ELSE}FE_ID{$ENDIF};
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(WM_PAINT),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(ID)
    );
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(WM_ERASEBKGND),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(0)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));
  WinapiWindowsMock
    .Setup
    .WillReturn(TestIndex)
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(TestIndex));

  EraseAndPaintMessage;

  WinapiWindowsMock.Verify;
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
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const UtilsPicturesMock = TMock<TUtilsPicturesMocks>.Create;
  const UtilsWindowsMock  = TMock<TUtilsWindowsMocks>.Create;

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
    .WillReturn(fsMDIChild)
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

  UtilsPicturesMock
    .Setup
    .Expect
    .Once
    .When
    .CreateBitmapFactory;
  UtilsPicturesMock
    .Setup
    .WillReturn(TestBitmap)
    .When
    .CreateBitmapFactory;

  UtilsPicturesMock
    .Setup
    .Expect
    .Once
    .When
    .AdjustBitmapForTransition(
      It0.IsEqualTo<TBitmap>(TestBitmap),
      It1.IsEqualTo<HPALETTE>(0),
      It2.IsEqualTo<TSize>(TSize.InlineCreate(WinControlWidth, WinControlHeight)),
      It3.IsEqualTo<TPixelFormat>(pfDevice)
    );

  UtilsPicturesMock
    .Setup
    .Expect
    .Once
    .When
    .FreeAndNilBitmap(It0.IsEqualTo(TestBitmap));

  UtilsWindowsMock
    .Setup
    .WillReturn(True)
    .When
    .HasWindowRegion(It0.IsEqualTo<HWND>(FWnd));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(WM_PRINT),
      It2.IsEqualTo<WPARAM>(CanvasDC),
      It3.IsEqualTo<LPARAM>(PRF_NONCLIENT)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .BitBlt(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(WinControlWidth),
      It4.IsEqualTo<Integer>(WinControlHeight),
      It5.IsEqualTo<HDC>(CanvasDC),
      It6.IsEqualTo<Integer>(0),
      It7.IsEqualTo<Integer>(0),
      It8.IsEqualTo<DWORD>(SRCCOPY)
    );

  Assert.AreNotEqual<HDC>(FDC, CanvasDC);

  NCPrintControl;

  CanvasMock.Verify;
  UtilsPicturesMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TRenderingExtTests.NCPrintControl_should_send_print_message_for_dc_if_customform_is_not_MDIChild;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const WinControlMock = TMock<TCustomForm>.Create;

  FWinControl := WinControlMock.Instance;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(WM_PRINT),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(PRF_NONCLIENT)
    );

  NCPrintControl;

  WinapiWindowsMock.Verify;
end;

procedure TRenderingExtTests.NCPrintControl_should_send_print_message_for_dc_if_wincontrol_is_not_custom_form;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(WM_PRINT),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(PRF_NONCLIENT)
    );

  NCPrintControl;

  WinapiWindowsMock.Verify;
end;

procedure TRenderingExtTests.NCPrintControl_should_send_print_message_for_dc_if_wnd_has_not_region;
begin
  const UtilsWindowsMock  = TMock<TUtilsWindowsMocks>.Create;
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const WinControlMock = TMock<TCustomForm>.Create;

  WinControlMock
    .Setup
    .WillReturn(fsMDIChild)
    .When
    .GetFormStyle;

  FWinControl := WinControlMock.Instance;

  UtilsWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .HasWindowRegion(It0.IsEqualTo<HWND>(FWnd));
  UtilsWindowsMock
    .Setup
    .WillReturn(False)
    .When
    .HasWindowRegion(It0.IsAny<HWND>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(FWnd),
      It1.IsEqualTo<UINT>(WM_PRINT),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(PRF_NONCLIENT)
    );

  NCPrintControl;

  UtilsWindowsMock.Verify;
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
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

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

  const ID = {$IFDEF USE_BILLENIUM_EFFECTS}BE_ID{$ELSE}FE_ID{$ENDIF};
  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .SendMessage(
      It0.IsEqualTo<HWND>(TestWnd),
      It1.IsEqualTo<UINT>(WM_PAINT),
      It2.IsEqualTo<WPARAM>(FDC),
      It3.IsEqualTo<LPARAM>(ID)
    );

  PaintCopy;

  WinControlMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TRenderingExtTests.PaintCopy_should_stop_executing_when_wincontrol_is_null;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  WinapiWindowsMock
    .Setup
    .Expect
    .Never
    .When
    .SendMessage(It0.IsAny<HWND>, It1.IsAny<UINT>, It2.IsAny<WPARAM>, It3.IsAny<LPARAM>);

  PaintCopy;

  WinapiWindowsMock.Verify;
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
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const VclThemesMock     = TMock<TVclThemesMocks>.Create;

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
    .WillReturn(ThemedElementDetails)
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

  VclThemesMock
    .Setup
    .Expect
  {$IFDEF USE_BILLENIUM_EFFECTS}
    .Exactly(2)
  {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
    .Exactly(1)
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
    .When
    .StyleServices;
  VclThemesMock
    .Setup
    .WillReturn(StyleService)
    .When
    .StyleServices;

  WinapiWindowsMock
    .Setup
    .WillReturn(WS_EX_CLIENTEDGE)
    .When
    .GetWindowLongPtr(It0.IsEqualTo<HWND>(FWnd), It1.IsEqualTo<Integer>(GWL_EXSTYLE));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetWindowRect(It0.IsEqualTo<HWND>(FWnd));
  WinapiWindowsMock
    .Setup
    .WillReturn(TRect.InlineCreate(133, 57, 199, 123))
    .When
    .GetWindowRect(It0.IsEqualTo<HWND>(FWnd));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .ExcludeClipRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(2),
      It2.IsEqualTo<Integer>(2),
      It3.IsEqualTo<Integer>(64),
      It4.IsEqualTo<Integer>(64)
    );

  PaintThemeBorder;

  StyleServiceMock.Verify;
  VclThemesMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TRenderingExtTests.PaintThemeBorder_should_stop_executing_when_no_WS_EX_CLIENTEDGE;
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const WinControlMock = TMock<TWinControl>.Create;

  WinControlMock
    .Setup
    .WillReturn(FWnd)
    .When
    .GetHandle;

  FWinControl := WinControlMock.Instance;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetWindowLongPtr(It0.IsEqualTo<HWND>(FWnd), It1.IsEqualTo<Integer>(GWL_EXSTYLE));

  WinapiWindowsMock
    .Setup
    .Expect
    .Never
    .When
    .GetWindowRect(It0.IsEqualTo<HWND>(FWnd));

  PaintThemeBorder;

  WinapiWindowsMock.Verify;
end;

procedure TRenderingExtTests.Setup;
begin
  FWnd := 111;
  FDC  := 501;
end;

procedure TRenderingExtTests.TearDown;
begin
  FWinControl := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TRenderingExtTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
