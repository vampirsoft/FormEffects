/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.pas                            *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Messages,
  Winapi.Windows,
  System.Types,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.ComCtrls.Mocks,
  FormEffects.Vcl.StdCtrls.Mocks,
  FormEffects.Vcl.DockTabSet.Mocks,
  FormEffects.Vcl.OleCtrls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{ TRenderingTests }

  [TestFixture]
  TRenderingTests = class
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;

    FWinControl: TWinControl;

  strict private
  {$IFDEF USE_BILLENIUM_EFFECTS}
    function ResolveMessage(const Message: Cardinal): TMessage; inline;
  {$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
    function ResolveMessage(const Message: Cardinal): Cardinal; inline;
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
    procedure GetRegControl_for_Control_Test(
      const ExpectedFlags: DWORD;
      const ExpectNonClientCallback: Boolean = False;
      const ExpectClientCallback: Boolean = False
    );

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

  public
  {$IFDEF USE_BILLENIUM_EFFECTS}
    [Test]
    [TestCase('for greate $C000 message', '$C001')]
    [TestCase('for equal  $C000 message', '$C000')]
    procedure DoesAncestorHandle_should_return_null(const Selector: Cardinal);
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}

    [Test]
    procedure DoesAncestorHandle_should_return_null_if_class_have_not_WM_PRINT_message;

    [Test]
    procedure DoesAncestorHandle_should_return_link_to_class_if_class_have_WM_PRINT_message;

    [Test]
    procedure DoesAncestorHandle_should_return_link_to_first_parent_class_with_WM_NCPAINT_message;

    [Test]
    [TestCase('as no focused, Flags = $00000000',       'False, $00000000, $00000000')]

    [TestCase('as no focused, Flags = RCF_PAINTCOPYNC', 'False, $00000100, $00000100')]
    [TestCase('as    focused, Flags = RCF_PAINTCOPYNC', 'True,  $00000100, $00800000')]

    [TestCase('as no focused, Flags = RCF_PAINTCOPY',   'False, $08000000, $08000000')]
    [TestCase('as    focused, Flags = RCF_PAINTCOPY',   'True,  $08000000, $01000000')]

    [TestCase('as no focused, Flags = RCF_RENDERNC',    'False, $00000001, $00000021')]
    [TestCase('as    focused, Flags = RCF_RENDERNC',    'True,  $00000001, $00000021')]

    [TestCase('as no focused, Flags = RCF_RENDER',      'False, $00100000, $01100000')]
    [TestCase('as    focused, Flags = RCF_RENDER',      'True,  $00100000, $01100000')]
    procedure CompleteFlags_should_return_flags_for_WinControl(const Focused: Boolean; const Flags, Result: DWORD);

    [Test]
    [TestCase('as no focused, Flags = $00000000',       'False, $00000000, $00000000')]

    [TestCase('as no focused, Flags = RCF_PAINTCOPYNC', 'False, $00000100, $00000100')]
    [TestCase('as    focused, Flags = RCF_PAINTCOPYNC', 'True,  $00000100, $00800000')]

    [TestCase('as no focused, Flags = RCF_PAINTCOPY',   'False, $08000000, $08000000')]
    [TestCase('as    focused, Flags = RCF_PAINTCOPY',   'True,  $08000000, $01000000')]

    [TestCase('as no focused, Flags = RCF_RENDERNC',    'False, $00000001, $00000021')]
    [TestCase('as    focused, Flags = RCF_RENDERNC',    'True,  $00000001, $00000021')]

    [TestCase('as no focused, Flags = RCF_RENDER',      'False, $00100000, $00900000')]
    [TestCase('as    focused, Flags = RCF_RENDER',      'True,  $00100000, $00900000')]
    procedure CompleteFlags_should_return_flags_for_HintWindow(const Focused: Boolean; const Flags, Result: DWORD);

    [Test]
    [TestCase('as no focused, Flags = $00000000',       'False, $00000000, $00000000')]

    [TestCase('as no focused, Flags = RCF_PAINTCOPYNC', 'False, $00000100, $00000100')]
    [TestCase('as    focused, Flags = RCF_PAINTCOPYNC', 'True,  $00000100, $00800000')]

    [TestCase('as no focused, Flags = RCF_PAINTCOPY',   'False, $08000000, $08000000')]
    [TestCase('as    focused, Flags = RCF_PAINTCOPY',   'True,  $08000000, $01000000')]

    [TestCase('as no focused, Flags = RCF_RENDERNC',    'False, $00000001, $0000000B')]
    [TestCase('as    focused, Flags = RCF_RENDERNC',    'True,  $00000001, $0000000B')]

    [TestCase('as no focused, Flags = RCF_RENDER',      'False, $00100000, $01100000')]
    [TestCase('as    focused, Flags = RCF_RENDER',      'True,  $00100000, $01100000')]
    procedure CompleteFlags_should_return_flags_for_Form(const Focused: Boolean; const Flags, Result: DWORD);

    [Test]
    [TestCase('as no focused, Flags = $00000000',       'False, $00000000, $00000000')]

    [TestCase('as no focused, Flags = RCF_PAINTCOPYNC', 'False, $00000100, $00000100')]
    [TestCase('as    focused, Flags = RCF_PAINTCOPYNC', 'True,  $00000100, $00800000')]

    [TestCase('as no focused, Flags = RCF_PAINTCOPY',   'False, $08000000, $08000000')]
    [TestCase('as    focused, Flags = RCF_PAINTCOPY',   'True,  $08000000, $01000000')]

    [TestCase('as no focused, Flags = RCF_RENDERNC',    'False, $00000001, $00000009')]
    [TestCase('as    focused, Flags = RCF_RENDERNC',    'True,  $00000001, $00000009')]

    [TestCase('as no focused, Flags = RCF_RENDER',      'False, $00100000, $00900000')]
    [TestCase('as    focused, Flags = RCF_RENDER',      'True,  $00100000, $00900000')]
    procedure CompleteFlags_should_return_flags_for_TabDockPanel(const Focused: Boolean; const Flags, Result: DWORD);

    [Test]
    [TestCase('with message = UNKNOWN',                   '$0000,  0, $01100009')]
    [TestCase('with message = CM_FEFULLRENDER',           '$BC4C,  0, $00180001')]
    [TestCase('with message = CM_FENCPAINT and lag = -1', '$BC4B, -1, $01100201')]
    [TestCase('with message = CM_FENCPAINT and lag =  0', '$BC4B,  0, $01100401')]
    [TestCase('with message = CM_FENCPAINT and lag =  1', '$BC4B,  1, $01100801')]
    [TestCase('with message = CM_FEPAINT   and lag = -1', '$BC4A, -1, $01100009')]
    [TestCase('with message = CM_FEPAINT   and lag =  0', '$BC4A,  0, $01100009')]
    [TestCase('with message = CM_FEPAINT   and lag =  1', '$BC4A,  1, $01100009')]
    procedure GetRegControl_for_window(const Message: UINT; const ResultLag: ShortInt; const Flags: DWORD);

    [Test]
    procedure GetRegControl_for_WinControl;

    [Test]
    procedure GetRegControl_for_CustomListControl;

    [Test]
    procedure GetRegControl_for_CustomListBox;

    [Test]
    procedure GetRegControl_for_Animate;

    [Test]
    procedure GetRegControl_for_ToolBar;

    [Test]
    procedure GetRegControl_for_CommonCalendar;

    [Test]
    procedure GetRegControl_for_CustomEdit;

    [Test]
    procedure GetRegControl_for_OleControl;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.SysUtils;

{$IFDEF FORM_EFFECTS_TESTS}

{ TRenderingTests }

procedure TRenderingTests.CompleteFlags_should_return_flags_for_Form(
  const Focused: Boolean;
  const Flags, Result: DWORD
);
begin
  const WinControlMock = TMock<TForm>.Create;

  WinControlMock.Setup.WillReturn(Focused).When.GetFocused;

  FWinControl := WinControlMock.Instance;

  const Actual = CompleteFlags(FWinControl, Flags);
  Assert.AreEqual(Result, Actual);
end;

procedure TRenderingTests.CompleteFlags_should_return_flags_for_HintWindow(
  const Focused: Boolean;
  const Flags, Result: DWORD
);
begin
  const WinControlMock = TMock<THintWindow>.Create;

  WinControlMock.Setup.WillReturn(Focused).When.GetFocused;

  FWinControl := WinControlMock.Instance;

  const Actual = CompleteFlags(FWinControl, Flags);
  Assert.AreEqual(Result, Actual);
end;

procedure TRenderingTests.CompleteFlags_should_return_flags_for_TabDockPanel(
  const Focused: Boolean;
  const Flags, Result: DWORD
);
begin
  const WinControlMock = TMock<TTabDockPanel>.Create;

  WinControlMock.Setup.WillReturn(Focused).When.GetFocused;

  FWinControl := WinControlMock.Instance;

  const Actual = CompleteFlags(FWinControl, Flags);
  Assert.AreEqual(Result, Actual);
end;

procedure TRenderingTests.CompleteFlags_should_return_flags_for_WinControl(
  const Focused: Boolean;
  const Flags, Result: DWORD
);
begin
  const WinControlMock = TMock<TWinControl>.Create;

  WinControlMock.Setup.WillReturn(Focused).When.GetFocused;

  FWinControl := WinControlMock.Instance;

  const Actual = CompleteFlags(FWinControl, Flags);
  Assert.AreEqual(Result, Actual);
end;

procedure TRenderingTests.DoesAncestorHandle_should_return_link_to_class_if_class_have_WM_PRINT_message;
begin
  const WinControlMock = TMock<THintWindow>.Create;

  FWinControl := WinControlMock.Instance;

  var Message := ResolveMessage(WM_PRINT);

  const Actual = DoesAncestorHandle(FWinControl, Message);
  Assert.IsNotNull(Actual);
end;

procedure TRenderingTests.DoesAncestorHandle_should_return_link_to_first_parent_class_with_WM_NCPAINT_message;
begin
  const WinControlMock = TMock<TForm>.Create;

  FWinControl := WinControlMock.Instance;

  var Message := ResolveMessage(WM_NCPAINT);

  const Actual = DoesAncestorHandle(FWinControl, Message);
  Assert.AreNotEqual(TWinControl, Actual);
  Assert.AreNotEqual(TForm, Actual);
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
procedure TRenderingTests.DoesAncestorHandle_should_return_null(const Selector: Cardinal);
begin
  const WinControlMock = TMock<TWinControl>.Create;

  FWinControl := WinControlMock.Instance;

  var Message := ResolveMessage(Selector);

  const Actual = DoesAncestorHandle(FWinControl, Message);
  Assert.IsNull(Actual);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TRenderingTests.DoesAncestorHandle_should_return_null_if_class_have_not_WM_PRINT_message;
begin
  const WinControlMock = TMock<TWinControl>.Create;

  FWinControl := WinControlMock.Instance;

  var Message := ResolveMessage(WM_PRINT);

  const Actual = DoesAncestorHandle(FWinControl, Message);
  Assert.IsNull(Actual);
end;

procedure TRenderingTests.GetRegControl_for_Animate;
begin
  const WinControlMock = TMock<TAnimate>.Create;

  FWinControl := WinControlMock.Instance;

  GetRegControl_for_Control_Test($00B00009);
end;

procedure TRenderingTests.GetRegControl_for_CommonCalendar;
begin
  const WinControlMock = TMock<TCommonCalendar>.Create;

  FWinControl := WinControlMock.Instance;

  GetRegControl_for_Control_Test($00900021);
end;

procedure TRenderingTests.GetRegControl_for_Control_Test(
  const ExpectedFlags: DWORD;
  const ExpectNonClientCallback, ExpectClientCallback: Boolean
);
var
  Flags: DWORD;
  NonClientCallback: TPaintCallback;
  ClientCallback: TPaintCallback;

begin
  GetRegControl(0, FWinControl, Flags, NonClientCallback, ClientCallback);

  Assert.AreEqual(ExpectedFlags, Flags);
  if ExpectNonClientCallback then Assert.IsTrue(Assigned(NonClientCallback))
  else Assert.IsFalse(Assigned(NonClientCallback));
  if ExpectClientCallback then Assert.IsTrue(Assigned(ClientCallback))
  else Assert.IsFalse(Assigned(ClientCallback));
end;

procedure TRenderingTests.GetRegControl_for_CustomEdit;
begin
  const WinControlMock = TMock<TCustomEdit>.Create;

  FWinControl := WinControlMock.Instance;

  GetRegControl_for_Control_Test($00900021);
end;

procedure TRenderingTests.GetRegControl_for_CustomListBox;
begin
  const WinControlMock = TMock<TCustomListBox>.Create;

  FWinControl := WinControlMock.Instance;

  GetRegControl_for_Control_Test($00900021);
end;

procedure TRenderingTests.GetRegControl_for_CustomListControl;
begin
  const WinControlMock = TMock<TCustomListControl>.Create;

  FWinControl := WinControlMock.Instance;

  GetRegControl_for_Control_Test($00900023);
end;

procedure TRenderingTests.GetRegControl_for_OleControl;
begin
  const WinControlMock = TMock<TOleControl>.Create;

  FWinControl := WinControlMock.Instance;

  GetRegControl_for_Control_Test($82300002);
end;

procedure TRenderingTests.GetRegControl_for_ToolBar;
begin
  const WinControlMock = TMock<TToolBar>.Create;

  FWinControl := WinControlMock.Instance;

  GetRegControl_for_Control_Test($0110000B);
end;

procedure TRenderingTests.GetRegControl_for_WinControl;
begin
  const WinControlMock = TMock<TWinControl>.Create;

  FWinControl := WinControlMock.Instance;

  GetRegControl_for_Control_Test($01100009);
end;

procedure TRenderingTests.GetRegControl_for_window(const Message: UINT; const ResultLag: ShortInt; const Flags: DWORD);
begin
  FWinControl := nil;

  const ID = {$IFDEF USE_BILLENIUM_EFFECTS}BE_ID{$ELSE}FE_ID{$ENDIF};
  FWinapiWindowsMocks.SendMessage_Result(
    function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
    begin
      if Msg = Message then Exit(ID + ResultLag);
      Result := -1;
    end
  );

  GetRegControl_for_Control_Test(Flags);
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
function TRenderingTests.ResolveMessage(const Message: Cardinal): TMessage;
begin
  Result.Msg := Message;
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
function TRenderingTests.ResolveMessage(const Message: Cardinal): Cardinal;
begin
  Result := Message;
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TRenderingTests.Setup;
begin
  FWinapiWindowsMocks := TWinapiWindowsMocks.Create;
end;

procedure TRenderingTests.TearDown;
begin
  FWinControl := nil;

  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TRenderingTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
