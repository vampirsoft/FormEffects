/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Tests.Glass.pas                    *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Tests.Glass;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TGlassTests }

  [TestFixture]
  TGlassTests = class
  public
    [Test]
    [TestCase('ParentGlass должен нотифицировать дочерние элементы', '')]
    procedure ParentGlass_should_notify_children_about_changes;

    [Test]
    [TestCase('GlassVisible не должен нотифицировать дочерние элементы', '')]
    procedure GlassVisible_should_not_notify_children_about_changes;

    [Test]
    [TestCase('ParentGlass должен быть изменён при изменении GlassTranslucency', '')]
    procedure ParentGlass_should_be_updated_when_change_GlassTranslucency;

    [Test]
    [TestCase('GlassTranslucency должен нотифицировать дочерние элементы', '')]
    procedure GlassTranslucency_should_notify_children_about_changes;

    [Test]
    [TestCase('GlassTranslucency должен вернуть текущее значение, если ParentGlass = False', '')]
    procedure GlassTranslucency_should_return_self_value_if_ParentGlass_is_false;

    [Test]
    [TestCase('GlassTranslucency должен вернуть текущее значение, если Parent отсутствует', '')]
    procedure GlassTranslucency_should_return_self_value_if_Parent_not_assignet;

    [Test]
    [TestCase('GlassTranslucency должен вернуть Parent-значение', '')]
    procedure GlassTranslucency_should_return_parent_value;

    [Test]
    [TestCase('ParentGlass должен быть изменён при изменении GlassColor', '')]
    procedure ParentGlass_should_be_updated_when_change_GlassColor;

    [Test]
    [TestCase('GlassColor должен нотифицировать дочерние элементы', '')]
    procedure GlassColor_should_notify_children_about_changes;

    [Test]
    [TestCase('GlassColor должен вернуть текущее значение, если ParentGlass = False', '')]
    procedure GlassColor_should_return_self_value_if_ParentGlass_is_false;

    [Test]
    [TestCase('GlassColor должен вернуть текущее значение, если Parent отсутствует', '')]
    procedure GlassColor_should_return_self_value_if_Parent_not_assignet;

    [Test]
    [TestCase('GlassColor должен вернуть Parent-значение', '')]
    procedure GlassColor_should_return_parent_value;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  Winapi.Windows,
  System.Rtti,
  Vcl.Graphics,
  FormEffects.Backgrounds.Tests,
{$IFNDEF USE_BILLENIUM_EFFECTS}
  FormEffects.Constants,
  FormEffects.Backgrounds,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TGlassTests }

procedure TGlassTests.GlassColor_should_notify_children_about_changes;
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

  ChildOptions.ParentGlass := True;

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

  Options.GlassColor := not Options.GlassColor;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TGlassTests.GlassColor_should_return_parent_value;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock    = TMock<TWinControl>.Create;
  const ParentControlMock = TMock<TWinControl>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  ParentControlMock
    .Setup
    .WillReturn(LRESULT(ParentOptions))
    .When
    .Perform(It0.IsAny<Cardinal>, It1.IsAny<WPARAM>, It2.IsAny<LPARAM>);

  WinControlMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .GetParent;

  ParentOptions.Control := ParentControl;
  ParentOptions.GlassColor := not ParentOptions.GlassColor;

  Options.Control := WinControl;
  Options.ParentGlass := True;

  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.AreEqual(ParentOptions.GlassColor, Options.GlassColor);
end;

procedure TGlassTests.GlassColor_should_return_self_value_if_ParentGlass_is_false;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock    = TMock<TWinControl>.Create;
  const ParentControlMock = TMock<TWinControl>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  ParentControlMock
    .Setup
    .WillReturn(LRESULT(ParentOptions))
    .When
    .Perform(It0.IsAny<Cardinal>, It1.IsAny<WPARAM>, It2.IsAny<LPARAM>);

  WinControlMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .GetParent;

  Options.Control := WinControl;
  Options.GlassColor := clWhite;

  Assert.IsFalse(Options.ParentGlass);
  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.AreEqual(clWhite, Options.GlassColor);
  Assert.AreNotEqual(ParentOptions.GlassColor, Options.GlassColor);
end;

procedure TGlassTests.GlassColor_should_return_self_value_if_Parent_not_assignet;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const ControlMock = TMock<TWinControl>.Create;

  const Options = OptionsMock.Instance;

  const Control = ControlMock.Instance;

  Options.ParentBackgoundForm := True;
  Options.Control := Control;
  Options.GlassColor := clWhite;

  Assert.IsNull(Options.Parent);
  Assert.AreEqual(clWhite, Options.GlassColor);
end;

procedure TGlassTests.GlassTranslucency_should_notify_children_about_changes;
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

  ChildOptions.ParentGlass := True;

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

  Options.GlassTranslucency := not Options.GlassTranslucency;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TGlassTests.GlassTranslucency_should_return_parent_value;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock    = TMock<TWinControl>.Create;
  const ParentControlMock = TMock<TWinControl>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  ParentControlMock
    .Setup
    .WillReturn(LRESULT(ParentOptions))
    .When
    .Perform(It0.IsAny<Cardinal>, It1.IsAny<WPARAM>, It2.IsAny<LPARAM>);

  WinControlMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .GetParent;

  ParentOptions.Control := ParentControl;
  ParentOptions.GlassTranslucency := not ParentOptions.GlassTranslucency;

  Options.Control := WinControl;
  Options.ParentGlass := True;

  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.AreEqual(ParentOptions.GlassTranslucency, Options.GlassTranslucency);
end;

procedure TGlassTests.GlassTranslucency_should_return_self_value_if_ParentGlass_is_false;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock    = TMock<TWinControl>.Create;
  const ParentControlMock = TMock<TWinControl>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  ParentControlMock
    .Setup
    .WillReturn(LRESULT(ParentOptions))
    .When
    .Perform(It0.IsAny<Cardinal>, It1.IsAny<WPARAM>, It2.IsAny<LPARAM>);

  WinControlMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .GetParent;

  Options.Control := WinControl;
  Options.GlassTranslucency := 0;

  Assert.IsFalse(Options.ParentGlass);
  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.IsTrue(Options.GlassTranslucency = 0);
  Assert.AreNotEqual(ParentOptions.GlassTranslucency, Options.GlassTranslucency);
end;

procedure TGlassTests.GlassTranslucency_should_return_self_value_if_Parent_not_assignet;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const ControlMock = TMock<TWinControl>.Create;

  const Options = OptionsMock.Instance;

  const Control = ControlMock.Instance;

  Options.ParentBackgoundForm := True;
  Options.Control := Control;
  Options.GlassTranslucency := 0;

  Assert.IsNull(Options.Parent);
  Assert.IsTrue(Options.GlassTranslucency = 0);
end;

procedure TGlassTests.GlassVisible_should_not_notify_children_about_changes;
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

  ChildOptions.ParentGlass := True;

  OptionsChangeNotifierMock
    .Setup
    .Expect
    .Once
    .When
    .OnChange(It0.IsEqualTo(Options));

  ChildOptionsChangeNotifierMock
    .Setup
    .Expect
    .Never
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

  Options.GlassVisible := not Options.GlassVisible;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TGlassTests.ParentGlass_should_be_updated_when_change_GlassColor;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const Options = OptionsMock.Instance;

  Options.ParentGlass := True;

  Options.GlassColor := not Options.GlassColor;

  Assert.IsFalse(Options.ParentGlass);
end;

procedure TGlassTests.ParentGlass_should_be_updated_when_change_GlassTranslucency;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const Options = OptionsMock.Instance;

  Options.ParentGlass := True;

  Options.GlassTranslucency := not Options.GlassTranslucency;

  Assert.IsFalse(Options.ParentGlass);
end;

procedure TGlassTests.ParentGlass_should_notify_children_about_changes;
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

  ChildOptions.ParentGlass := True;

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

  Options.ParentGlass := not Options.ParentGlass;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

initialization
  TDUnitX.RegisterTestFixture(TGlassTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
