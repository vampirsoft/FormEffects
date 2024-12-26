/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Tests.Picture.pas                  *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Tests.Picture;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TPictureTests }

  [TestFixture]
  TPictureTests = class
  public
    [Test]
    [TestCase('ParentPicture должен нотифицировать дочерние элементы', '')]
    procedure ParentPicture_should_notify_children_about_changes;

    [Test]
    [TestCase('ParentPicture должен изменяться при задании Picture, и очищать Picture, если задать True', '')]
    procedure ParentPicture_should_update_Picture;

    [Test]
    [TestCase('Picture не должен нотифицировать об изменениях', '')]
    procedure Picture_should_not_notify_about_changes;

    [Test]
    [TestCase('Picture должен вернуть текущее значение, если ParentPicture = False', '')]
    procedure Picture_should_return_self_value_if_ParentPicture_is_false;

    [Test]
    [TestCase('Picture должен вернуть текущее значение, если Parent отсутствует', '')]
    procedure Picture_should_return_self_value_if_Parent_not_assignet;

    [Test]
    [TestCase('Picture должен вернуть Parent-значение', '')]
    procedure Picture_should_return_parent_value;

    [Test]
    [TestCase('PictureMode должен нотифицировать дочерние элементы', '')]
    procedure PictureMode_should_notify_children_about_changes;

    [Test]
    [TestCase('PictureMode должен вернуть текущее значение, если ParentPicture = False', '')]
    procedure PictureMode_should_return_self_value_if_ParentPicture_is_false;

    [Test]
    [TestCase('PictureMode должен вернуть текущее значение, если Parent отсутствует', '')]
    procedure PictureMode_should_return_self_value_if_Parent_not_assignet;

    [Test]
    [TestCase('PictureMode должен вернуть Parent-значение', '')]
    procedure PictureMode_should_return_parent_value;

    [Test]
    [TestCase('PictureTransparentColor должен нотифицировать дочерние элементы', '')]
    procedure PictureTransparentColor_should_notify_children_about_changes;

    [Test]
    [TestCase('PictureTransparentColor должен вернуть текущее значение, если ParentPicture = False', '')]
    procedure PictureTransparentColor_should_return_self_value_if_ParentPicture_is_false;

    [Test]
    [TestCase('PictureTransparentColor должен вернуть текущее значение, если Parent отсутствует', '')]
    procedure PictureTransparentColor_should_return_self_value_if_Parent_not_assignet;

    [Test]
    [TestCase('PictureTransparentColor должен вернуть Parent-значение', '')]
    procedure PictureTransparentColor_should_return_parent_value;
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  Delphi.Mocks,
  Winapi.Windows,
  System.Rtti,
  Vcl.Graphics,
  FormEffects.Constants,
{$IFNDEF USE_BILLENIUM_EFFECTS}
  FormEffects.Backgrounds,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Backgrounds.Tests,
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TPictureTests }

procedure TPictureTests.ParentPicture_should_notify_children_about_changes;
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

  ChildOptions.ParentPicture := True;

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

  Options.ParentPicture := not Options.ParentPicture;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TPictureTests.ParentPicture_should_update_Picture;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;
  const ControlMock = TMock<TWinControl>.Create;
  const PictureMock = TMock<TPicture>.Create;
  const BitmapMock  = TMock<TBitmap>.Create;

  const Options = OptionsMock.Instance;
  const Control = ControlMock.Instance;
  const Picture = PictureMock.Instance;
  const Bitmap  = BitmapMock.Instance;

  Picture.Graphic := Bitmap;

  Options.Control := Control;

  Options.ParentPicture := True;

  Options.Picture := Picture;

  Assert.IsFalse(Options.ParentPicture);
  Assert.IsNotNull(Options.Picture.Graphic);

  Options.ParentPicture := True;

  Assert.IsNull(Options.Picture.Graphic)
end;

procedure TPictureTests.PictureMode_should_notify_children_about_changes;
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

  ChildOptions.ParentPicture := True;

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

  Options.PictureMode := TBackgroundPictureMode.Center.Resolve;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TPictureTests.PictureMode_should_return_parent_value;
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
  ParentOptions.PictureMode := TBackgroundPictureMode.Center.Resolve;

  Options.Control := WinControl;
  Options.ParentPicture := True;

  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.AreEqual(ParentOptions.PictureMode, Options.PictureMode);
end;

procedure TPictureTests.PictureMode_should_return_self_value_if_ParentPicture_is_false;
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
  Options.PictureMode := TBackgroundPictureMode.Center.Resolve;

  Assert.IsFalse(Options.ParentPicture);
  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.AreEqual(TBackgroundPictureMode.Center.Resolve, Options.PictureMode);
  Assert.AreNotEqual(ParentOptions.PictureMode, Options.PictureMode);
end;

procedure TPictureTests.PictureMode_should_return_self_value_if_Parent_not_assignet;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const ControlMock = TMock<TWinControl>.Create;

  const Options = OptionsMock.Instance;

  const Control = ControlMock.Instance;

  Options.ParentBackgoundForm := True;
  Options.Control := Control;
  Options.PictureMode := TBackgroundPictureMode.Center.Resolve;

  Assert.IsNull(Options.Parent);
  Assert.AreEqual(TBackgroundPictureMode.Center.Resolve, Options.PictureMode);
end;

procedure TPictureTests.PictureTransparentColor_should_notify_children_about_changes;
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

  ChildOptions.ParentPicture := True;

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

  Options.PictureTransparentColor := not Options.PictureTransparentColor;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TPictureTests.PictureTransparentColor_should_return_parent_value;
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
  ParentOptions.PictureTransparentColor := not ParentOptions.PictureTransparentColor;

  Options.Control := WinControl;
  Options.ParentPicture := True;

  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.AreEqual(ParentOptions.PictureTransparentColor, Options.PictureTransparentColor);
end;

procedure TPictureTests.PictureTransparentColor_should_return_self_value_if_ParentPicture_is_false;
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
  Options.PictureTransparentColor := not Options.PictureTransparentColor;

  Assert.IsFalse(Options.ParentPicture);
  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.AreNotEqual(ParentOptions.PictureTransparentColor, Options.PictureTransparentColor);
end;

procedure TPictureTests.PictureTransparentColor_should_return_self_value_if_Parent_not_assignet;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const ControlMock = TMock<TWinControl>.Create;

  const Options = OptionsMock.Instance;

  const Control = ControlMock.Instance;

  Options.ParentBackgoundForm := True;
  Options.Control := Control;
  Options.PictureTransparentColor := clBlack;

  Assert.IsNull(Options.Parent);
  Assert.AreEqual(clBlack, Options.PictureTransparentColor);
end;

procedure TPictureTests.Picture_should_not_notify_about_changes;
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

  ChildOptions.ParentPicture := True;

  OptionsChangeNotifierMock
    .Setup
    .Expect
    .Never
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

  Options.Picture := nil;

  ChildOptionsChangeNotifierMock.Verify;
  OptionsChangeNotifierMock.Verify;
end;

procedure TPictureTests.Picture_should_return_parent_value;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock    = TMock<TWinControl>.Create;
  const ParentControlMock = TMock<TWinControl>.Create;

  const BitmapMock  = TMock<TBitmap>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  const Bitmap  = BitmapMock.Instance;

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
  ParentOptions.Picture.Graphic := Bitmap;

  Options.Control := WinControl;
  Options.ParentPicture := True;

  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.AreEqual(ParentOptions.Picture.Graphic, Options.Picture.Graphic);

  ParentOptions.Picture.Graphic := nil;
end;

procedure TPictureTests.Picture_should_return_self_value_if_ParentPicture_is_false;
begin
  const OptionsMock       = TMock<TBackgroundOptions>.Create;
  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControlMock    = TMock<TWinControl>.Create;
  const ParentControlMock = TMock<TWinControl>.Create;

  const BitmapMock  = TMock<TBitmap>.Create;

  const Options       = OptionsMock.Instance;
  const ParentOptions = ParentOptionsMock.Instance;

  const WinControl    = WinControlMock.Instance;
  const ParentControl = ParentControlMock.Instance;

  const Bitmap  = BitmapMock.Instance;

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
  Options.Picture.Graphic := Bitmap;

  Assert.IsFalse(Options.ParentPicture);
  Assert.AreEqual<TObject>(ParentOptions, Options.Parent);
  Assert.AreEqual<TGraphic>(Bitmap, Options.Picture.Graphic);
  Assert.AreNotEqual(ParentOptions.Picture.Graphic, Options.Picture.Graphic);
end;

procedure TPictureTests.Picture_should_return_self_value_if_Parent_not_assignet;
begin
  const OptionsMock = TMock<TBackgroundOptions>.Create;

  const ControlMock = TMock<TWinControl>.Create;

  const BitmapMock  = TMock<TBitmap>.Create;

  const Options = OptionsMock.Instance;

  const Control = ControlMock.Instance;

  const Bitmap  = BitmapMock.Instance;

  Options.ParentBackgoundForm := True;
  Options.Control := Control;
  Options.Picture.Graphic := Bitmap;

  Assert.IsNull(Options.Parent);
  Assert.AreEqual<TGraphic>(Bitmap, Options.Picture.Graphic);
end;

initialization
  TDUnitX.RegisterTestFixture(TPictureTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
