/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Backgrounds.Tests.Control.pas                  *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Backgrounds.Tests.Control;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework;

{$IFDEF FORM_EFFECTS_TESTS}

type

  [TestFixture]
  TControlTests = class
  public
    [Test]
    [TestCase('MainForm как Parent Control для fsMDIChild формы должен вернуть BackgroundOptions для CM_FEGETBACKGRONDOPTIONS', '')]
    procedure MainForm_as_parent_control_should_Perform_options;

  {$IFNDEF USE_BILLENIUM_EFFECTS}
    [Test]
    [TestCase('Parent window control должен иметь BackgroundOptions (должно происходить удаление Child BackgroundOptions у Parent BackgroundOptions)', '')]
    procedure ParentWindow_should_have_options_and_remove_self_from_parent;
  {$ENDIF ~ USE_BILLENIUM_EFFECTS}

    [Test]
    [TestCase('Child BackgroundOptions должны быть добавлены в коллекцию', '')]
    procedure Child_options_should_be_enriched;

  {$IFNDEF USE_BILLENIUM_EFFECTS}
    [Test]
    [TestCase('Child BackgroundOptions должны быть добавлены в коллекцию (должно происходить удаление Parent BackgroundOptions у Child BackgroundOptions)', '')]
    procedure Child_options_should_be_enriched_and_remove_self_from_children;
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
{$IFDEF USE_BILLENIUM_EFFECTS}
  teBkgrnd,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Constants,
  FormEffects.Backgrounds,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Controls.Mocks,
  FormEffects.Vcl.Forms.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{$IFDEF USE_BILLENIUM_EFFECTS}
const CM_GETBACKGRONDOPTIONS = CM_TEGETBKGNDOPTIONS;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
const CM_GETBACKGRONDOPTIONS = CM_FEGETBACKGRONDOPTIONS;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

type
{$IFDEF USE_BILLENIUM_EFFECTS}
  TBackgroundOptions = TFCBackgroundOptions;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{ TBackgroundOptionsHelper }

  TBackgroundOptionsHelper = class helper for TBackgroundOptions
  public
    function GetChildBackgroundOptions(const Index: Integer): TBackgroundOptions;
  end;

  TWinControlMock = class abstract(TWinControl{$IFNDEF USE_BILLENIUM_EFFECTS}, IWithBackgroundOptions{$ENDIF})
  public
    function GetBackgroundOptions: TBackgroundOptions; virtual; abstract;
  end;

{ TControlTests }

procedure TControlTests.Child_options_should_be_enriched;
begin
  const WinControlMock      = TMock<TWinControlMock>.Create;
  const ChildWinControlMock = TMock<TWinControl>.Create;
  const ChildControlMock    = TMock<TControl>.Create;

  const OptionsMock      = TMock<TBackgroundOptions>.Create;
  const ChildOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControl      = WinControlMock.Instance;
  const ChildWinControl = ChildWinControlMock.Instance;
  const ChildControl    = ChildControlMock.Instance;

  const Options      = OptionsMock.Instance;
  const ChildOptions = ChildOptionsMock.Instance;

  WinControlMock
    .Setup
    .Expect
    .Exactly(2)
    .When
    .GetControls;
  WinControlMock
    .Setup
    .WillReturn(TValue.From<TControls>([ChildWinControl]))
    .When
    .GetControls;

  ChildWinControlMock
    .Setup
    .Expect
    .Exactly(2)
    .When
    .GetControls;
  ChildWinControlMock
    .Setup
    .WillReturn(TValue.From<TControls>([ChildControl]))
    .When
    .GetControls;

  ChildControlMock
    .Setup
    .Expect
    .Once
    .When
    .Perform(
      It0.IsEqualTo<Cardinal>(CM_GETBACKGRONDOPTIONS),
      It1.IsEqualTo<WPARAM>(0),
      It2.IsEqualTo<LPARAM>(0)
    );
  ChildControlMock
    .Setup
    .WillReturn(LRESULT(ChildOptions))
    .When
    .Perform(
      It0.IsEqualTo<Cardinal>(CM_GETBACKGRONDOPTIONS),
      It1.IsEqualTo<WPARAM>(0),
      It2.IsEqualTo<LPARAM>(0)
    );

  Options.Control := WinControl;

  Assert.AreEqual<TControl>(WinControl, Options.Control);
  Assert.AreEqual<TBackgroundOptions>(Options, ChildOptions.Parent);
  Assert.AreEqual<TBackgroundOptions>(ChildOptions, Options.GetChildBackgroundOptions(0));
  ChildControlMock.Verify;
  ChildWinControlMock.Verify;
  WinControlMock.Verify;
end;

{$IFNDEF USE_BILLENIUM_EFFECTS}

procedure TControlTests.Child_options_should_be_enriched_and_remove_self_from_children;
begin
  const WinControlMock   = TMock<TWinControl>.Create;
  const ChildControlMock = TMock<TWinControlMock>.Create;

  const OptionsMock      = TMock<TBackgroundOptions>.Create;
  const ChildOptionsMock = TMock<TBackgroundOptions>.Create;

  const WinControl   = WinControlMock.Instance;
  const ChildControl = ChildControlMock.Instance;

  const Options      = OptionsMock.Instance;
  const ChildOptions = ChildOptionsMock.Instance;

  ChildControlMock
    .Setup
    .Expect
    .Once
    .When
    .GetBackgroundOptions;
  ChildControlMock
    .Setup
    .WillReturn(ChildOptions)
    .When
    .GetBackgroundOptions;

  WinControlMock
    .Setup
    .Expect
    .Exactly(2)
    .When
    .GetControls;
  WinControlMock
    .Setup
    .WillReturn(TValue.From<TControls>([ChildControl]))
    .When
    .GetControls;

  Options.Control := WinControl;

  Assert.AreEqual<TControl>(WinControl, Options.Control);
  Assert.AreEqual<TBackgroundOptions>(Options, ChildOptions.Parent);
  Assert.AreEqual<TBackgroundOptions>(ChildOptions, Options.GetChildBackgroundOptions(0));
  ChildControlMock.Verify;
  WinControlMock.Verify;

  Options.Control := nil;

  Assert.IsNull(Options.Control);
  Assert.IsNull(ChildOptions.Parent);
  Assert.IsNull(Options.GetChildBackgroundOptions(0));
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

procedure TControlTests.MainForm_as_parent_control_should_Perform_options;
begin
  const ApplicationMock = TMock<TApplication>.Create;
  const MainFormMock    = TMock<TForm>.Create;
  const ParentFormMock  = TMock<TForm>.Create;
  const WinControlMock  = TMock<TWinControl>.Create;

  const MainFormOptionsMock = TMock<TBackgroundOptions>.Create;
  const OptionsMock         = TMock<TBackgroundOptions>.Create;

  Application := ApplicationMock.Instance;

  const MainForm   = MainFormMock.Instance;
  const ParentForm = ParentFormMock.Instance;
  const WinControl = WinControlMock.Instance;

  const MainFormOptions = MainFormOptionsMock.Instance;
  const Options         = OptionsMock.Instance;

  ApplicationMock
    .Setup
    .Expect
    .Once
    .When
    .GetMainForm;
  ApplicationMock
    .Setup
    .WillReturn(MainForm)
    .When
    .GetMainForm;

  MainFormMock
    .Setup
    .Expect
    .Once
    .When
    .Perform(
      It0.IsEqualTo<Cardinal>(CM_GETBACKGRONDOPTIONS),
      It1.IsEqualTo<WPARAM>(0),
      It2.IsEqualTo<LPARAM>(0)
    );
  MainFormMock
    .Setup
    .WillReturn(LRESULT(MainFormOptions))
    .When
    .Perform(
      It0.IsEqualTo<Cardinal>(CM_GETBACKGRONDOPTIONS),
      It1.IsEqualTo<WPARAM>(0),
      It2.IsEqualTo<LPARAM>(0)
    );

  ParentFormMock
    .Setup
    .Expect
    .Once
    .When
    .GetFormStyle;
  ParentFormMock
    .Setup
    .WillReturn(fsMDIChild)
    .When
    .GetFormStyle;

  ParentFormMock
    .Setup
    .Expect
    .Once
    .When
    .GetParent;

  WinControlMock
    .Setup
    .Expect
    .Exactly({$IFDEF USE_BILLENIUM_EFFECTS}2{$ELSE}1{$ENDIF})
    .When
    .GetParent;
  WinControlMock
    .Setup
    .WillReturn(ParentForm)
    .When
    .GetParent;

  Options.Control := WinControl;

  Assert.AreEqual<TControl>(WinControl, Options.Control);
  Assert.AreEqual<TBackgroundOptions>(MainFormOptions, Options.Parent);
  Assert.AreEqual<TBackgroundOptions>(Options, MainFormOptions.GetChildBackgroundOptions(0));
  WinControlMock.Verify;
  ParentFormMock.Verify;
  MainFormMock.Verify;
  ApplicationMock.Verify;
end;

{$IFNDEF USE_BILLENIUM_EFFECTS}

procedure TControlTests.ParentWindow_should_have_options_and_remove_self_from_parent;
begin
  const VclControlsMock = TMock<TVclControlsMocks>.Create;

  const ParentControlMock = TMock<TWinControlMock>.Create;
  const WinControlMock    = TMock<TWinControl>.Create;

  const ParentOptionsMock = TMock<TBackgroundOptions>.Create;
  const OptionsMock       = TMock<TBackgroundOptions>.Create;

  const ParentWind: HWND = 2999;
  const ParentControl    = ParentControlMock.Instance;
  const WinControl       = WinControlMock.Instance;

  const ParentOptions = ParentOptionsMock.Instance;
  const Options       = OptionsMock.Instance;

  VclControlsMock
    .Setup
    .Expect
    .Once
    .When
    .FindControl(It0.IsEqualTo<HWND>(ParentWind));
  VclControlsMock
    .Setup
    .WillReturn(ParentControl)
    .When
    .FindControl(It0.IsEqualTo<HWND>(ParentWind));

  ParentControlMock
    .Setup
    .Expect
    .Once
    .When
    .GetBackgroundOptions;
  ParentControlMock
    .Setup
    .WillReturn(ParentOptions)
    .When
    .GetBackgroundOptions;

  WinControlMock
    .Setup
    .Expect
    .Once
    .When
    .GetParentWindow;
  WinControlMock
    .Setup
    .WillReturn(ParentWind)
    .When
    .GetParentWindow;

  Options.Control := WinControl;

  Assert.AreEqual<TControl>(WinControl, Options.Control);
  Assert.AreEqual<TBackgroundOptions>(ParentOptions, Options.Parent);
  Assert.AreEqual<TBackgroundOptions>(Options, ParentOptions.GetChildBackgroundOptions(0));
  WinControlMock.Verify;
  ParentControlMock.Verify;
  VclControlsMock.Verify;

  Options.Control := nil;

  Assert.IsNull(Options.Control);
  Assert.IsNull(Options.Parent);
  Assert.IsNull(ParentOptions.GetChildBackgroundOptions(0));
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

function TBackgroundOptionsHelper.GetChildBackgroundOptions(const Index: Integer): TBackgroundOptions;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  Result := ChildBkOptions[Index];
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  if (Index < 0) or (Index >= GetChildren.Count) then
    Result := nil
  else
    Result := GetChildren[Index];
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

initialization
  TDUnitX.RegisterTestFixture(TControlTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
