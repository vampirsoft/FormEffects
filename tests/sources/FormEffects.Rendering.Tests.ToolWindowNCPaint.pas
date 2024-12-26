/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.ToolWindowNCPaint.pas          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.ToolWindowNCPaint;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
  Vcl.ToolWin,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.ToolWin.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TToolWindowNCPaintTests }

  [TestFixture]
  TToolWindowNCPaintTests = class
  strict private
    FWnd: HWND;
    FDC: HDC;
    FBrush: HBRUSH;

    FBrushMock: TMock<TBrush>;
    FToolWindowMock: TMock<TToolWindow>;
    FToolWindow: TToolWindow;

  strict private
    function SetupMocks: TMock<TWinapiWindowsMocks>;
    procedure ToolWindowNCPaint;

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeInner = esNone',       'esNone, $00')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeInner = esRaised',   'esRaised, $04')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeInner = esLowered', 'esLowered, $08')]
    procedure should_DrawEdge_with_EdgeInner(const EdgeInner: TEdgeStyle; const Expected: UINT);

    [Test]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeOuter = esNone',       'esNone, $00')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeOuter = esRaised',   'esRaised, $01')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeOuter = esLowered', 'esLowered, $02')]
    procedure should_DrawEdge_with_EdgeOuter(const EdgeOuter: TEdgeStyle; const Expected: UINT);

    [Test]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeBorder = ebLeft',     'ebLeft, $2001')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeBorder = ebTop',       'ebTop, $2002')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeBorder = ebRight',   'ebRight, $2004')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeBorder = ebBottom', 'ebBottom, $2008')]
    procedure should_DrawEdge_with_EdgeBorders(const EdgeBorder: TEdgeBorder; const Expected: UINT);

    [Test]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для Ctl3D = True',  'True,  $2000')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для Ctl3D = False', 'False, $A000')]
    procedure should_DrawEdge_with_Ctl3D(const Ctl3D: Boolean; const Expected: UINT);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.Types,
  System.Rtti,
  System.SysUtils,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TToolWindowNCPaintTests }

procedure TToolWindowNCPaintTests.ToolWindowNCPaint;
begin
{$IFDEF USE_BILLENIUM_EFFECTS}
  TEAPIHooksDisabled := True;
  teRender.ToolWindowNCPaint(FToolWindow, FDC);
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.ToolWindowNCPaint(FToolWindow, FDC);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

function TToolWindowNCPaintTests.SetupMocks: TMock<TWinapiWindowsMocks>;
begin
  Result := TMock<TWinapiWindowsMocks>.Create;

  FBrushMock      := TMock<TBrush>.Create;
  FToolWindowMock := TMock<TToolWindow>.Create;

  const WindowRect = TRect.InlineCreate(331, 221, 553, 431);
  const ClientRect = TRect.InlineCreate(123, 73,  177, 131);

  FBrushMock
    .Setup
    .WillReturn(FBrush)
    .When
    .GetHandle;

  FToolWindowMock
    .Setup
    .WillReturn(FWnd)
    .When
    .GetHandle;
  FToolWindowMock
    .Setup
    .WillReturn(FBrushMock.Instance)
    .When
    .GetBrush;

  FToolWindow := FToolWindowMock.Instance;

  Result
    .Setup
    .Expect
    .Once
    .When
    .GetClientRect(It0.IsEqualTo<HWND>(FWnd));
  Result
    .Setup
    .WillReturn(ClientRect)
    .When
    .GetClientRect(It0.IsEqualTo<HWND>(FWnd));

  Result
    .Setup
    .Expect
    .Once
    .When
    .GetWindowRect(It0.IsEqualTo<HWND>(FWnd));
  Result
    .Setup
    .WillReturn(WindowRect)
    .When
    .GetWindowRect(It0.IsEqualTo<HWND>(FWnd));

  Result
    .Setup
    .Expect
    .Once
    .When
    .IntersectClipRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(0),
      It2.IsEqualTo<Integer>(0),
      It3.IsEqualTo<Integer>(222),
      It4.IsEqualTo<Integer>(210)
    );

  Result
    .Setup
    .Expect
    .Once
    .When
    .ExcludeClipRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(-261),
      It2.IsEqualTo<Integer>(-201),
      It3.IsEqualTo<Integer>(-207),
      It4.IsEqualTo<Integer>(-143)
    );

  Result
    .Setup
    .Expect
    .Once
    .When
    .FillRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 222, 210)),
      It2.IsEqualTo<HBRUSH>(FBrush)
    );
end;

procedure TToolWindowNCPaintTests.should_DrawEdge_with_Ctl3D(const Ctl3D: Boolean; const Expected: UINT);
begin
  const WinapiWindowsMock = SetupMocks;

  FToolWindowMock
    .Setup
    .WillReturn(Ctl3D)
    .When
    .GetCtl3D;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DrawEdge(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 222, 210)),
      It2.IsEqualTo<UINT>(0),
      It3.IsEqualTo<UINT>(Expected)
    );

  ToolWindowNCPaint;

  WinapiWindowsMock.Verify;
end;

procedure TToolWindowNCPaintTests.should_DrawEdge_with_EdgeBorders(const EdgeBorder: TEdgeBorder; const Expected: UINT);
begin
  const WinapiWindowsMock = SetupMocks;

  const EdgeBorders: TEdgeBorders = [EdgeBorder];

  FToolWindowMock
    .Setup
    .WillReturn(EdgeBorders)
    .When
    .GetEdgeBorders;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DrawEdge(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 222, 210)),
      It2.IsEqualTo<UINT>(0),
      It3.IsEqualTo<UINT>(Expected)
    );

  ToolWindowNCPaint;

  WinapiWindowsMock.Verify;
end;

procedure TToolWindowNCPaintTests.should_DrawEdge_with_EdgeInner(const EdgeInner: TEdgeStyle; const Expected: UINT);
begin
  const WinapiWindowsMock = SetupMocks;

  FToolWindowMock
    .Setup
    .WillReturn(EdgeInner)
    .When
    .GetEdgeInner;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DrawEdge(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 222, 210)),
      It2.IsEqualTo<UINT>(Expected),
      It3.IsEqualTo<UINT>(BF_ADJUST)
    );

  ToolWindowNCPaint;

  WinapiWindowsMock.Verify;
end;

procedure TToolWindowNCPaintTests.should_DrawEdge_with_EdgeOuter(const EdgeOuter: TEdgeStyle; const Expected: UINT);
begin
  const WinapiWindowsMock = SetupMocks;

  FToolWindowMock
    .Setup
    .WillReturn(EdgeOuter)
    .When
    .GetEdgeOuter;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DrawEdge(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 222, 210)),
      It2.IsEqualTo<UINT>(Expected),
      It3.IsEqualTo<UINT>(BF_ADJUST)
    );

  ToolWindowNCPaint;

  WinapiWindowsMock.Verify;
end;

procedure TToolWindowNCPaintTests.Setup;
begin
  FWnd   := 53;
  FDC    := 757;
  FBrush := 833;
end;

procedure TToolWindowNCPaintTests.TearDown;
begin
  FToolWindow := nil;
end;

initialization
{$IFNDEF USE_BILLENIUM_EFFECTS}
  TDUnitX.RegisterTestFixture(TToolWindowNCPaintTests);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
