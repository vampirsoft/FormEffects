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
  FormEffects.Vcl.Forms.Mocks;

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

    FWnd: HWND;
    FDC: HDC;

    FWinControl: TWinControl;

  strict private
    procedure PaintCopy;
    procedure NCPrintControl;

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
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Winapi.Messages,
  System.Rtti,
  System.Types,
  System.SysUtils,
{$IFNDEF USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.Pictures,
  FormEffects.Rendering.Ext,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TRenderingExtTests }

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

procedure TRenderingExtTests.Setup;
begin
  FWnd := 111;
  FDC  := 501;

  FWinapiWindowsMocks      := TWinapiWindowsMocks.Create;
  FUtilsPicturesMocks      := TUtilsPicturesMocks.Create;
  FUtilsWindowsMocks       := TUtilsWindowsMocks.Create;
  FRenderingPicturesMocks  := TRenderingPicturesMocks.Create;
end;

procedure TRenderingExtTests.TearDown;
begin
  FWinControl := nil;

  FreeAndNil(FRenderingPicturesMocks);
  FreeAndNil(FUtilsWindowsMocks);
  FreeAndNil(FUtilsPicturesMocks);
  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TRenderingExtTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
