/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Tests.BackgoundForm.pas            *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Tests.BackgoundForm;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TBackgoundFormTests }

  [TestFixture]
  TBackgoundFormTests = class
  public
    [Test]
    [TestCase('SetBackgoundForm должен нотифицировать дочерние элементы', '')]
    procedure SetBackgoundForm_should_notify_children_about_changes;

    [Test]
    [TestCase('ParentBackgoundForm должен изменяться при задании BackgoundForm, и очищать BackgoundForm, если задать True', '')]
    procedure ParentBackgoundForm_should_update_BackgoundForm;

    [Test]
    [TestCase('ParentBackgoundForm должен нотифицировать дочерние элементы', '')]
    procedure ParentBackgoundForm_should_notify_children_about_changes;

    [Test]
    [TestCase('BackgoundFormVisible не должен нотифицировать дочерние элементы', '')]
    procedure BackgoundFormVisible_should_not_notify_children_about_changes;

    [Test]
    [TestCase('GetBackgoundForm должен вернуть текущую форму, если ParentBackgoundForm = False', '')]
    procedure GetBackgoundForm_should_return_self_value_if_ParentBackgoundForm_is_false;

    [Test]
    [TestCase('GetBackgoundForm должен вернуть текущую форму, если Parent отсутствует', '')]
    procedure GetBackgoundForm_should_return_self_value_if_Parent_not_assignet;

    [Test]
    [TestCase('GetBackgoundForm должен вернуть Parent-форму', '')]
    procedure GetBackgoundForm_should_return_parent_form;

  {$IFNDEF USE_BILLENIUM_EFFECTS}
    [Test]
    [TestCase('GetBackgoundForm должен вернуть null, если форма не установлена', '')]
    procedure GetBackgoundForm_should_return_null;

    [Test]
    [TestCase('GetBackgoundForm должен вернуть форму типа TForm', '')]
    procedure GetBackgoundForm_should_return_form;

  {$IFDEF DEBUG}
    [Test]
    [TestCase('GetBackgoundForm должен выбросить исключение, если ожидаемый тип не соотвествует возвращаемому', '')]
    procedure GetBackgoundForm_should_throw_exception;
  {$ENDIF ~ DEBUG}
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  Winapi.Windows,
  System.Rtti,
  System.SysUtils,
  Vcl.Forms,
  FormEffects.Backgrounds.Tests,
{$IFNDEF USE_BILLENIUM_EFFECTS}
  FormEffects.Constants,
  FormEffects.Backgrounds,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TBackgoundFormTests }

procedure TBackgoundFormTests.SetBackgoundForm_should_notify_children_about_changes;
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

  ChildOptions.ParentBackgoundForm := True;

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

  Options.SetBackgoundForm(TCustomForm);

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TBackgoundFormTests.BackgoundFormVisible_should_not_notify_children_about_changes;
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

  ChildOptions.ParentBackgoundForm := True;

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

  Options.BackgoundFormVisible := not Options.BackgoundFormVisible;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TBackgoundFormTests.GetBackgoundForm_should_return_self_value_if_ParentBackgoundForm_is_false;
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
  Options.SetBackgoundForm(TCustomForm);

  Assert.IsFalse(Options.ParentBackgoundForm);
  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.IsNull(ParentOptions.GetBackgoundForm);
  Assert.IsNotNull(Options.GetBackgoundForm);
end;

procedure TBackgoundFormTests.GetBackgoundForm_should_return_self_value_if_Parent_not_assignet;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const ControlMock = TMock<TWinControl>.Create;

  const Options = OptionsMock.Instance;

  const Control = ControlMock.Instance;

  Options.ParentBackgoundForm := True;
  Options.Control := Control;
  Options.SetBackgoundForm(TCustomForm);

  Assert.IsNull(Options.Parent);
  Assert.IsNotNull(Options.GetBackgoundForm);
end;

procedure TBackgoundFormTests.GetBackgoundForm_should_return_parent_form;
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
  ParentOptions.SetBackgoundForm(TForm);

  Options.Control := WinControl;
  Options.SetBackgoundForm(TCustomForm);
  Options.ParentBackgoundForm := True;

  Assert.IsTrue(Options.GetBackgoundForm is TForm);
  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.IsTrue(ParentOptions.GetBackgoundForm is TForm);
  Assert.AreEqual(ParentOptions.GetBackgoundForm, Options.GetBackgoundForm);
end;

{$IFNDEF USE_BILLENIUM_EFFECTS}

procedure TBackgoundFormTests.GetBackgoundForm_should_return_form;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;
  const FormMock    = TMock<TForm>.Create;

  const Options = OptionsMock.Instance;
  const Form    = FormMock.Instance;

  OptionsMock
    .Setup
    .WillReturn(Form)
    .When
    .GetBackgoundForm;

  Assert.AreEqual(Form, Options.GetBackgoundForm<TForm>);
end;

procedure TBackgoundFormTests.GetBackgoundForm_should_return_null;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const Options = OptionsMock.Instance;

  OptionsMock
    .Setup
    .WillReturnNil
    .When
    .GetBackgoundForm;

  Assert.IsNull(Options.GetBackgoundForm<TForm>);
end;

{$IFDEF DEBUG}

procedure TBackgoundFormTests.GetBackgoundForm_should_throw_exception;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;
  const FormMock    = TMock<TCustomForm>.Create;

  const Options = OptionsMock.Instance;
  const Form    = FormMock.Instance;

  OptionsMock
    .Setup
    .WillReturn(Form)
    .When
    .GetBackgoundForm;

  Assert.WillRaiseWithMessage(
    procedure
    begin
      Options.GetBackgoundForm<TForm>;
    end,
    EAssertionFailed
  );
end;

{$ENDIF ~ DEBUG}

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TBackgoundFormTests.ParentBackgoundForm_should_notify_children_about_changes;
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

  ChildOptions.ParentBackgoundForm := True;

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

  Options.ParentBackgoundForm := True;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TBackgoundFormTests.ParentBackgoundForm_should_update_BackgoundForm;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;
  const ControlMock = TMock<TWinControl>.Create;

  const Options = OptionsMock.Instance;
  const Control = ControlMock.Instance;

  Options.Control := Control;

  Options.ParentBackgoundForm := True;

  Options.SetBackgoundForm(TCustomForm);

  Assert.IsFalse(Options.ParentBackgoundForm);
  Assert.IsNotNull(Options.GetBackgoundForm);

  Options.ParentBackgoundForm := True;

  Assert.IsNull(Options.GetBackgoundForm)
end;

initialization
  TDUnitX.RegisterTestFixture(TBackgoundFormTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
