/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.ToolWindowNCPaint.pas          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
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
  FormEffects.TypeHelpers,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.ToolWin.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TToolWindowNCPaintTests }

  [TestFixture]
  TToolWindowNCPaintTests = class
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;

    FWnd: HWND;
    FDC: HDC;
    FBrush: HBRUSH;

    FBrushMock: TMock<TBrush>;
    FToolWindowMock: TMock<TToolWindow>;
    FToolWindow: TToolWindow;

    FGetWindowRectInvoked: Boolean;
    FGetClientRectInvoked: Boolean;
    FExcludeClipRectInvoked: Boolean;
    FIntersectClipRectInvoked: Boolean;
    FFillRectInvoked: Boolean;

  strict private
    procedure ToolWindowNCPaint;

  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

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
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeOuter = ebLeft',     'ebLeft, $2001')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeOuter = ebTop',       'ebTop, $2002')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeOuter = ebRight',   'ebRight, $2004')]
    [TestCase('ToolWindowNCPaint должен вызвать DrawEdge для EdgeOuter = ebBottom', 'ebBottom, $2008')]
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

procedure TToolWindowNCPaintTests.should_DrawEdge_with_Ctl3D(const Ctl3D: Boolean; const Expected: UINT);
begin
  var DrawEdgeInvoked := False;

  FToolWindowMock
    .Setup
    .WillReturn(Ctl3D)
    .When
    .GetCtl3D;

  FWinapiWindowsMocks.DrawEdge_Result(
    procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
    begin
      DrawEdgeInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 222, 210), Rect);
      Assert.AreEqual<UINT>(0, Edge);
      Assert.AreEqual<UINT>(Expected, Flags);
    end
  );

  ToolWindowNCPaint;
  Assert.IsTrue(FGetWindowRectInvoked);
  Assert.IsTrue(FGetClientRectInvoked);
  Assert.IsTrue(FExcludeClipRectInvoked);
  Assert.IsTrue(FIntersectClipRectInvoked);
  Assert.IsTrue(FFillRectInvoked);
  Assert.IsTrue(DrawEdgeInvoked);
end;

procedure TToolWindowNCPaintTests.should_DrawEdge_with_EdgeBorders(const EdgeBorder: TEdgeBorder; const Expected: UINT);
begin
  var DrawEdgeInvoked := False;

  const EdgeBorders: TEdgeBorders = [EdgeBorder];

  FToolWindowMock
    .Setup
    .WillReturn(EdgeBorders.AsValue)
    .When
    .GetEdgeBorders;

  FWinapiWindowsMocks.DrawEdge_Result(
    procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
    begin
      DrawEdgeInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 222, 210), Rect);
      Assert.AreEqual<UINT>(0, Edge);
      Assert.AreEqual<UINT>(Expected, Flags);
    end
  );

  ToolWindowNCPaint;
  Assert.IsTrue(FGetWindowRectInvoked);
  Assert.IsTrue(FGetClientRectInvoked);
  Assert.IsTrue(FExcludeClipRectInvoked);
  Assert.IsTrue(FIntersectClipRectInvoked);
  Assert.IsTrue(FFillRectInvoked);
  Assert.IsTrue(DrawEdgeInvoked);
end;

procedure TToolWindowNCPaintTests.should_DrawEdge_with_EdgeInner(const EdgeInner: TEdgeStyle; const Expected: UINT);
begin
  var DrawEdgeInvoked := False;

  FToolWindowMock
    .Setup
    .WillReturn(EdgeInner.AsValue)
    .When
    .GetEdgeInner;

  FWinapiWindowsMocks.DrawEdge_Result(
    procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
    begin
      DrawEdgeInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 222, 210), Rect);
      Assert.AreEqual<UINT>(Expected, Edge);
      Assert.AreEqual<UINT>(BF_ADJUST, Flags);
    end
  );

  ToolWindowNCPaint;
  Assert.IsTrue(FGetWindowRectInvoked);
  Assert.IsTrue(FGetClientRectInvoked);
  Assert.IsTrue(FExcludeClipRectInvoked);
  Assert.IsTrue(FIntersectClipRectInvoked);
  Assert.IsTrue(FFillRectInvoked);
  Assert.IsTrue(DrawEdgeInvoked);
end;

procedure TToolWindowNCPaintTests.should_DrawEdge_with_EdgeOuter(const EdgeOuter: TEdgeStyle; const Expected: UINT);
begin
  var DrawEdgeInvoked := False;

  FToolWindowMock
    .Setup
    .WillReturn(EdgeOuter.AsValue)
    .When
    .GetEdgeOuter;

  FWinapiWindowsMocks.DrawEdge_Result(
    procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
    begin
      DrawEdgeInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 222, 210), Rect);
      Assert.AreEqual<UINT>(Expected, Edge);
      Assert.AreEqual<UINT>(BF_ADJUST, Flags);
    end
  );

  ToolWindowNCPaint;
  Assert.IsTrue(FGetWindowRectInvoked);
  Assert.IsTrue(FGetClientRectInvoked);
  Assert.IsTrue(FExcludeClipRectInvoked);
  Assert.IsTrue(FIntersectClipRectInvoked);
  Assert.IsTrue(FFillRectInvoked);
  Assert.IsTrue(DrawEdgeInvoked);
end;

procedure TToolWindowNCPaintTests.Setup;
begin
  FGetWindowRectInvoked     := False;
  FGetClientRectInvoked     := False;
  FExcludeClipRectInvoked   := False;
  FIntersectClipRectInvoked := False;
  FFillRectInvoked          := False;

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

  FWinapiWindowsMocks.GetWindowRect_Result(
    function(const Wnd: HWND): TRect
    begin
      FGetWindowRectInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);

      Result := WindowRect;
    end
  );
  FWinapiWindowsMocks.GetClientRect_Result(
    function(const Wnd: HWND): TRect
    begin
      FGetClientRectInvoked := True;

      Assert.AreEqual<HWND>(FWnd, Wnd);

      Result := ClientRect;
    end
  );
  FWinapiWindowsMocks.ExcludeClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      FExcludeClipRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual(-261, Left);
      Assert.AreEqual(-201, Top);
      Assert.AreEqual(-207, Right);
      Assert.AreEqual(-143, Bottom);
    end
  );
  FWinapiWindowsMocks.IntersectClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      FIntersectClipRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual(  0, Left);
      Assert.AreEqual(  0, Top);
      Assert.AreEqual(222, Right);
      Assert.AreEqual(210, Bottom);
    end
  );
  FWinapiWindowsMocks.FillRect_Result(
    procedure(const DC: HDC; const Rect: TRect; Brush: HBRUSH)
    begin
      FFillRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 222, 210), Rect);
      Assert.AreEqual<HBRUSH>(FBrush, Brush);
    end
  );
end;

procedure TToolWindowNCPaintTests.TearDown;
begin
  FToolWindow := nil;
end;

procedure TToolWindowNCPaintTests.SetupFixture;
begin
  FWinapiWindowsMocks := TWinapiWindowsMocks.Create;

  FWnd   := 53;
  FDC    := 757;
  FBrush := 833;
end;

procedure TToolWindowNCPaintTests.TearDownFixture;
begin
  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
{$IFNDEF USE_BILLENIUM_EFFECTS}
  TDUnitX.RegisterTestFixture(TToolWindowNCPaintTests);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
