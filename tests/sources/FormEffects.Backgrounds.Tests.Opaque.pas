/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Tests.Opaque.pas                   *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Tests.Opaque;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TOpaqueTests }

  [TestFixture]
  TOpaqueTests = class
  public
    [Test]
    [TestCase('Opaque должен вызвать Invalidate, если Control установлен', '')]
    procedure Opaque_should_invalidate_control;

    [Test]
    [TestCase('ParentOpaque должен быть изменён при изменении Opaque', '')]
    procedure ParentOpaque_should_be_updated_when_change_Opaque;

    [Test]
    [TestCase('ParentOpaque должен нотифицировать дочерние элементы', '')]
    procedure ParentOpaque_should_notify_children_about_changes;

    [Test]
    [TestCase('Opaque должен вернуть текущее значение, если ParentOpaque = False', '')]
    procedure Opaque_should_return_self_value_if_ParentOpaque_is_false;

    [Test]
    [TestCase('Opaque должен вернуть текущее значение, если Parent отсутствует', '')]
    procedure Opaque_should_return_self_value_if_Parent_not_assignet;

    [Test]
    [TestCase('Opaque должен вернуть текущее значение, если Control отсутствует', '')]
    procedure Opaque_should_return_self_value_if_Control_not_assignet;

    [Test]
    [TestCase('Opaque должен вернуть текущее значение, если Parent of Control не является Control of Parent', '')]
    procedure Opaque_should_return_self_value_if_parent_of_Control_is_not_control_of_parent;

    [Test]
    [TestCase('Opaque должен вернуть Parent-значение', '')]
    procedure Opaque_should_return_parent_value;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  Winapi.Windows,
  System.Rtti,
  FormEffects.Backgrounds.Tests,
{$IFNDEF USE_BILLENIUM_EFFECTS}
  FormEffects.Constants,
  FormEffects.Backgrounds,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TOpaqueTests }

procedure TOpaqueTests.Opaque_should_invalidate_control;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const ControlMock = TMock<TWinControl>.Create;

  const Options = OptionsMock.Instance;

  const Control = ControlMock.Instance;

  OptionsMock
    .Setup
    .WillReturn(Control)
    .When
    .GetControl;

  ControlMock
    .Setup
    .Expect
    .Once
    .When
    .Invalidate;

  Options.Opaque := not Options.Opaque;

  ControlMock.Verify;
end;

procedure TOpaqueTests.Opaque_should_return_parent_value;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock    = TMock<TWinControl>.Create;
  const ParentControlMock = TMock<TWinControl>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  ParentOptionsMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .GetControl;

  OptionsMock
    .Setup
    .WillReturn(WinControl)
    .When
    .GetControl;
  OptionsMock
    .Setup
    .WillReturn(ParentOptions)
    .When
    .GetParent;

  WinControlMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .GetParent;

  Options.Opaque       := True;
  Options.ParentOpaque := True;
  ParentOptions.Opaque := False;

  Assert.IsFalse(Options.Opaque);
  Assert.AreEqual(ParentOptions.Opaque, Options.Opaque);
end;

procedure TOpaqueTests.Opaque_should_return_self_value_if_Control_not_assignet;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  OptionsMock
    .Setup
    .WillReturn(ParentOptions)
    .When
    .GetParent;
  OptionsMock
    .Setup
    .WillReturnNil
    .When
    .GetControl;

  Options.Opaque       := True;
  Options.ParentOpaque := True;
  ParentOptions.Opaque := False;

  Assert.IsTrue(Options.Opaque);
  Assert.AreNotEqual(ParentOptions.Opaque, Options.Opaque);
end;

procedure TOpaqueTests.Opaque_should_return_self_value_if_ParentOpaque_is_false;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  OptionsMock
    .Setup
    .WillReturn(ParentOptions)
    .When
    .GetParent;

  Options.Opaque       := True;
  ParentOptions.Opaque := False;

  Assert.IsFalse(Options.ParentOpaque);
  Assert.IsTrue(Options.Opaque);
  Assert.AreNotEqual(ParentOptions.Opaque, Options.Opaque);
end;

procedure TOpaqueTests.Opaque_should_return_self_value_if_Parent_not_assignet;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const Options = OptionsMock.Instance;

  OptionsMock
    .Setup
    .WillReturnNil
    .When
    .GetParent;

  Options.Opaque       := True;
  Options.ParentOpaque := True;

  Assert.IsTrue(Options.Opaque);
end;

procedure TOpaqueTests.Opaque_should_return_self_value_if_parent_of_Control_is_not_control_of_parent;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock    = TMock<TWinControl>.Create;
  const ParentControlMock = TMock<TWinControl>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  ParentOptionsMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .GetControl;

  OptionsMock
    .Setup
    .WillReturn(WinControl)
    .When
    .GetControl;

  Options.Opaque       := True;
  Options.ParentOpaque := True;
  ParentOptions.Opaque := False;

  Assert.IsTrue(Options.Opaque);
  Assert.AreNotEqual(ParentOptions.Opaque, Options.Opaque);
end;

procedure TOpaqueTests.ParentOpaque_should_notify_children_about_changes;
begin
  const OptionsMock      = TMock<TBackgroundOptions>.Create;
  const ChildOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock   = TMock<TWinControl>.Create;
  const ChildControlMock = TMock<TWinControl>.Create;

  const OptionsChangeNotifierMock      = TMock<TChangeNotifier>.Create;
  const ChildOptionsChangeNotifierMock = TMock<TChangeNotifier>.Create;

  const Options      = OptionsMock.Instance;
  const ChildOptions = ChildOptionsMock.Instance;

  const WinControl   = WinControlMock.Instance;
  const ChildControl = ChildControlMock.Instance;

  ChildOptions.ParentOpaque := True;

  OptionsChangeNotifierMock
    .Setup
    .Expect
    .Once
    .When
    .OnChange(It0.IsEqualTo(Options));

  ChildOptionsChangeNotifierMock
    .Setup
    .Expect
    .Once
    .When
    .OnChange(It0.IsEqualTo(ChildOptions));

  ChildControlMock
    .Setup
    .WillReturn(LRESULT(ChildOptions))
    .When
    .Perform(It0.IsAny<Cardinal>, It1.IsAny<WPARAM>, It2.IsAny<LPARAM>);

  WinControlMock
    .Setup
    .WillReturn(TValue.From<TControls>([ChildControl]))
    .When
    .GetControls;

  Options.Control := WinControl;

  Options.OnChange := OptionsChangeNotifierMock.Instance.OnChange;

  ChildOptions.OnChange := ChildOptionsChangeNotifierMock.Instance.OnChange;

  Options.ParentOpaque := not Options.ParentOpaque;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TOpaqueTests.ParentOpaque_should_be_updated_when_change_Opaque;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const Options = OptionsMock.Instance;

  Options.ParentOpaque := True;

  Options.Opaque := not Options.Opaque;

  Assert.IsFalse(Options.ParentOpaque);
end;

initialization
  TDUnitX.RegisterTestFixture(TOpaqueTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
