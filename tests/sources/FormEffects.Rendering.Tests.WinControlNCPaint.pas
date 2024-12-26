/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.WinControlNCPaint.pas          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.WinControlNCPaint;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.Utils.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Rendering.Ext.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

  TPaintCallback = {$IFDEF USE_BILLENIUM_EFFECTS}TTEPaintCallback{$ELSE}TFEPaintCallback{$ENDIF};

{ TWinControlNCPaintTests}

  [TestFixture]
  TWinControlNCPaintTests = class
  strict private
    FWinapiWindowsMocks: TWinapiWindowsMocks;
    FRenderingExtMocks: TRenderingExtMocks;

    FWinControlWnd: HWND;
    FDC: HDC;
    FBrush: HBRUSH;

    FWinControl: TWinControl;

    FClientRect: TRect;
    FWindowRect: TRect;

  strict private
    procedure WinControlNCPaint(const Themed: Boolean = False);

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

  public
    [Test]
    [TestCase('WinControlNCPaint не должен вызвать PaintThemeBorder, если не определён флаг RCF_THEMEDNC и WinControl не имет csNeedsBorderPaint', 'False,csPannable')]
    [TestCase('WinControlNCPaint должен вызвать PaintThemeBorder, если определён флаг RCF_THEMEDNC, но WinControl не имет csNeedsBorderPaint',      'True,csPannable')]
    [TestCase('WinControlNCPaint должен вызвать PaintThemeBorder, если не определён флаг RCF_THEMEDNC, но WinControl имет csNeedsBorderPaint',     'False,csNeedsBorderPaint')]
    procedure should_paint_theme_border(const Themed: Boolean; const ControlStyleItem: TControlStyleItem);

    [Test]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelKind = Tile', 'bkTile, $2000')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelKind = Soft', 'bkSoft, $3000')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelKind = Flat', 'bkFlat, $6000')]
    procedure should_use_clip_rect_for_Bevel_Kind(const BevelKind: TBevelKind; const Expected: UINT);

    [Test]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelInner = None',       'bvNone, $00')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelInner = Lowered', 'bvLowered, $08')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelInner = Raised',   'bvRaised, $04')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelInner = Space',     'bvSpace, $00')]
    procedure should_use_clip_rect_for_Bevel_Inner(const BevelInner: TBevelCut; const Expected: UINT);

    [Test]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelOuter = None',       'bvNone, $00')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelOuter = Lowered', 'bvLowered, $02')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelOuter = Raised',   'bvRaised, $01')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelOuter = Space',     'bvSpace, $00')]
    procedure should_use_clip_rect_for_Bevel_Outer(const BevelOuter: TBevelCut; const Expected: UINT);

    [Test]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelEdge = Left',     'beLeft, $2001')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelEdge = Top',       'beTop, $2002')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelEdge = Right',   'beRight, $2004')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для BevelEdge = Bottom', 'beBottom, $2008')]
    procedure should_use_clip_rect_for_Bevel_Edges(const BevelEdge: TBevelEdge; const Expected: UINT);

    [Test]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для Ctl3D = True',   'True, $2000')]
    [TestCase('WinControlNCPaint должен использовать Clip Rect для Ctl3D = False', 'False, $A000')]
    procedure should_use_clip_rect_for_Ctl3D(const Ctl3D: Boolean; const Expected: UINT);
  end;

{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.Types,
  System.Rtti,
  System.SysUtils,
  System.Math,
  FormEffects.TypeHelpers,
  FormEffects.TypeHelpers.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

{ TWinControlNCPaintTests }

procedure TWinControlNCPaintTests.WinControlNCPaint(const Themed: Boolean);
begin
  const Wnd: HWND     = 0;
  const StopWnd: HWND = 0;
  const Flags = IfThen(Themed, MAXDWORD, 0);
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender.WinControlNCPaintExt(
    Wnd,
    StopWnd,
    FWinControl,
    Flags,
    nil,
    nil,
    FDC,
    Themed
  );
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering.WinControlNCPaint(
    Wnd,
    StopWnd,
    FWinControl,
    Flags,
    nil,
    nil,
    FDC
  );
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
end;

procedure TWinControlNCPaintTests.should_paint_theme_border(
  const Themed: Boolean;
  const ControlStyleItem: TControlStyleItem
);
begin
  var SaveDCInvokeCount       := 0;
  var RestoreDCInvokeCount    := 0;
  var NCPrintControlInvoked   := False;
  var PaintThemeBorderInvoked := False;

  const SaveDCInvokeMaxCount = 2;
  var   SaveDCInvokeIndex   := 0;

  const ControlStyle: TControlStyle = [ControlStyleItem];

  const WinControlMock = TMock<TWinControl>.Create;

  WinControlMock
    .Setup
    .WillReturn(ControlStyle.AsValue)
    .When
    .GetControlStyle;
  WinControlMock
    .Setup
    .WillReturn(FWinControlWnd)
    .When
    .GetHandle;

  FWinControl := WinControlMock.Instance;

  FWinapiWindowsMocks.SaveDC_Result(
    function(const DC: HDC): Integer
    begin
      Inc(SaveDCInvokeCount);

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.IsTrue(SaveDCInvokeIndex < SaveDCInvokeMaxCount);

      Inc(SaveDCInvokeIndex);

      Result := SaveDCInvokeIndex;
    end
  );
  FWinapiWindowsMocks.RestoreDC_Result(
    procedure(const DC: HDC; const Index: Integer)
    begin
      Inc(RestoreDCInvokeCount);

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual(SaveDCInvokeIndex, Index);

      Dec(SaveDCInvokeIndex);
    end
  );
  FRenderingExtMocks.NCPrintControl_Result(
    procedure(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC)
    begin
      NCPrintControlInvoked := True;

      Assert.AreEqual<HWND>(FWinControlWnd, Wnd);
      Assert.AreEqual<TWinControl>(FWinControl, WinControl);
      Assert.AreEqual<HDC>(FDC, DC);
    end
  );
  FRenderingExtMocks.PaintThemeBorder_Result(
    procedure(const WinControl: TWinControl; const DC: HDC)
    begin
      PaintThemeBorderInvoked := True;

      Assert.AreEqual<TWinControl>(FWinControl, WinControl);
      Assert.AreEqual<HDC>(FDC, DC);
    end
  );

  const Expected = Themed or (ControlStyleItem = csNeedsBorderPaint);

  WinControlNCPaint(Themed);

  Assert.AreEqual(SaveDCInvokeMaxCount, SaveDCInvokeCount);
  Assert.AreEqual(SaveDCInvokeMaxCount, RestoreDCInvokeCount);
  Assert.IsTrue(NCPrintControlInvoked);
  Assert.AreEqual(Expected, PaintThemeBorderInvoked);
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Bevel_Edges(
  const BevelEdge: TBevelEdge;
  const Expected: UINT
);
begin
  var DrawEdgeInvoked          := False;
  var IntersectClipRectInvoked := False;
  var FillRectInvoked          := False;

  const BevelEdges: TBevelEdges = [BevelEdge];
  const BevelWidth              = 10;
  var ExpectedRect := TRect.InlineCreate(64, 34, 114, 56);
  if BevelEdge = beLeft then
    Dec(ExpectedRect.Left, 2 * BevelWidth);
  if BevelEdge = beTop then
    Dec(ExpectedRect.Top, 2 * BevelWidth);
  if BevelEdge = beRight then
    Inc(ExpectedRect.Right, 2 * BevelWidth);
  if BevelEdge = beBottom then
    Inc(ExpectedRect.Bottom, 2 * BevelWidth);

  const BrushMock      = TMock<TBrush>.Create;
  const WinControlMock = TMock<TWinControl>.Create;

  BrushMock
    .Setup
    .WillReturn(FBrush)
    .When
    .GetHandle;

  WinControlMock
    .Setup
    .WillReturn(bkTile.AsValue)
    .When
    .GetBevelKind;
  WinControlMock
    .Setup
    .WillReturn(BevelEdges.AsValue)
    .When
    .GetBevelEdges;
  WinControlMock
    .Setup
    .WillReturn(bvSpace.AsValue)
    .When
    .GetBevelInner;
  WinControlMock
    .Setup
    .WillReturn(bvSpace.AsValue)
    .When
    .GetBevelOuter;
  WinControlMock
    .Setup
    .WillReturn(BevelWidth)
    .When
    .GetBevelWidth;
  WinControlMock
    .Setup
    .WillReturn(BrushMock.Instance)
    .When
    .GetBrush;

  FWinControl := WinControlMock.Instance;

  FWinapiWindowsMocks.GetClientRect_Result(FClientRect);
  FWinapiWindowsMocks.GetWindowRect_Result(FWindowRect);
  FWinapiWindowsMocks.DrawEdge_Result(
    procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
    begin
      DrawEdgeInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(ExpectedRect, Rect);
      Assert.AreEqual<UINT>(0, Edge);
      Assert.AreEqual<UINT>(Expected, Flags);
    end
  );
  FWinapiWindowsMocks.IntersectClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      IntersectClipRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(ExpectedRect, TRect.InlineCreate(Left, Top, Right, Bottom));
    end
  );
  FWinapiWindowsMocks.FillRect_Result(
    procedure(const DC: HDC; const Rect: TRect; Brush: HBRUSH)
    begin
      FillRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 76, 60), Rect);
      Assert.AreEqual<HBRUSH>(FBrush, Brush);
    end
  );

  WinControlNCPaint;

  Assert.IsTrue(DrawEdgeInvoked);
  Assert.IsTrue(IntersectClipRectInvoked);
  Assert.IsTrue(FillRectInvoked);
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Bevel_Inner(
  const BevelInner: TBevelCut;
  const Expected: UINT
);
begin
  var DrawEdgeInvoked          := False;
  var IntersectClipRectInvoked := False;
  var FillRectInvoked          := False;

  const BrushMock      = TMock<TBrush>.Create;
  const WinControlMock = TMock<TWinControl>.Create;

  BrushMock
    .Setup
    .WillReturn(FBrush)
    .When
    .GetHandle;

  WinControlMock
    .Setup
    .WillReturn(bkTile.AsValue)
    .When
    .GetBevelKind;
  WinControlMock
    .Setup
    .WillReturn(BevelInner.AsValue)
    .When
    .GetBevelInner;
  WinControlMock
    .Setup
    .WillReturn(10)
    .When
    .GetBevelWidth;
  WinControlMock
    .Setup
    .WillReturn(BrushMock.Instance)
    .When
    .GetBrush;

  FWinControl := WinControlMock.Instance;

  FWinapiWindowsMocks.GetClientRect_Result(FClientRect);
  FWinapiWindowsMocks.GetWindowRect_Result(FWindowRect);
  FWinapiWindowsMocks.DrawEdge_Result(
    procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
    begin
      DrawEdgeInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(64, 34, 114, 56), Rect);
      Assert.AreEqual<UINT>(Expected, Edge);
      Assert.AreEqual<UINT>($2000, Flags);
    end
  );
  FWinapiWindowsMocks.IntersectClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      IntersectClipRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual( 64, Left);
      Assert.AreEqual( 34, Top);
      Assert.AreEqual(114, Right);
      Assert.AreEqual( 56, Bottom);
    end
  );
  FWinapiWindowsMocks.FillRect_Result(
    procedure(const DC: HDC; const Rect: TRect; Brush: HBRUSH)
    begin
      FillRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 76, 60), Rect);
      Assert.AreEqual<HBRUSH>(FBrush, Brush);
    end
  );

  WinControlNCPaint;

  Assert.IsTrue(DrawEdgeInvoked);
  Assert.IsTrue(IntersectClipRectInvoked);
  Assert.IsTrue(FillRectInvoked);
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Bevel_Kind(
  const BevelKind: TBevelKind;
  const Expected: UINT
);
begin
  var GetClientRectInvoked     := False;
  var GetWindowRectInvoked     := False;
  var ExcludeClipRectInvoked   := False;
  var DrawEdgeInvoked          := False;
  var IntersectClipRectInvoked := False;
  var FillRectInvoked          := False;

  const BrushMock      = TMock<TBrush>.Create;
  const WinControlMock = TMock<TWinControl>.Create;

  BrushMock
    .Setup
    .WillReturn(FBrush)
    .When
    .GetHandle;

  WinControlMock
    .Setup
    .WillReturn(FWinControlWnd)
    .When
    .GetHandle;
  WinControlMock
    .Setup
    .WillReturn(BevelKind.AsValue)
    .When
    .GetBevelKind;
  WinControlMock
    .Setup
    .WillReturn(BrushMock.Instance)
    .When
    .GetBrush;

  FWinControl := WinControlMock.Instance;

  FWinapiWindowsMocks.GetClientRect_Result(
    function(const Wnd: HWND): TRect
    begin
      GetClientRectInvoked := True;

      Assert.AreEqual<HWND>(FWinControlWnd, Wnd);

      Result := FClientRect;
    end
  );
  FWinapiWindowsMocks.GetWindowRect_Result(
    function(const Wnd: HWND): TRect
    begin
      GetWindowRectInvoked := True;

      Assert.AreEqual<HWND>(FWinControlWnd, Wnd);

      Result := FWindowRect;
    end
  );
  FWinapiWindowsMocks.ExcludeClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      ExcludeClipRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual(-389, Left);
      Assert.AreEqual(-419, Top);
      Assert.AreEqual(-339, Right);
      Assert.AreEqual(-397, Bottom);
    end
  );
  FWinapiWindowsMocks.DrawEdge_Result(
    procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
    begin
      DrawEdgeInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(-389, -419, -339, -397), Rect);
      Assert.AreEqual<UINT>(0, Edge);
      Assert.AreEqual<UINT>(Expected, Flags);
    end
  );
  FWinapiWindowsMocks.IntersectClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      IntersectClipRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual(-389, Left);
      Assert.AreEqual(-419, Top);
      Assert.AreEqual(-339, Right);
      Assert.AreEqual(-397, Bottom);
    end
  );
  FWinapiWindowsMocks.FillRect_Result(
    procedure(const DC: HDC; const Rect: TRect; Brush: HBRUSH)
    begin
      FillRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 76, 60), Rect);
      Assert.AreEqual<HBRUSH>(FBrush, Brush);
    end
  );

  WinControlNCPaint;

  Assert.IsTrue(GetClientRectInvoked);
  Assert.IsTrue(GetWindowRectInvoked);
  Assert.IsTrue(ExcludeClipRectInvoked);
  Assert.IsTrue(DrawEdgeInvoked);
  Assert.IsTrue(IntersectClipRectInvoked);
  Assert.IsTrue(FillRectInvoked);
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Bevel_Outer(
  const BevelOuter: TBevelCut;
  const Expected: UINT
);
begin
  var DrawEdgeInvoked          := False;
  var IntersectClipRectInvoked := False;
  var FillRectInvoked          := False;

  const BrushMock      = TMock<TBrush>.Create;
  const WinControlMock = TMock<TWinControl>.Create;

  BrushMock
    .Setup
    .WillReturn(FBrush)
    .When
    .GetHandle;

  WinControlMock
    .Setup
    .WillReturn(bkTile.AsValue)
    .When
    .GetBevelKind;
  WinControlMock
    .Setup
    .WillReturn(BevelOuter.AsValue)
    .When
    .GetBevelOuter;
  WinControlMock
    .Setup
    .WillReturn(10)
    .When
    .GetBevelWidth;
  WinControlMock
    .Setup
    .WillReturn(BrushMock.Instance)
    .When
    .GetBrush;

  FWinControl := WinControlMock.Instance;

  FWinapiWindowsMocks.GetClientRect_Result(FClientRect);
  FWinapiWindowsMocks.GetWindowRect_Result(FWindowRect);
  FWinapiWindowsMocks.DrawEdge_Result(
    procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
    begin
      DrawEdgeInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(64, 34, 114, 56), Rect);
      Assert.AreEqual<UINT>(Expected, Edge);
      Assert.AreEqual<UINT>($2000, Flags);
    end
  );
  FWinapiWindowsMocks.IntersectClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      IntersectClipRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual( 64, Left);
      Assert.AreEqual( 34, Top);
      Assert.AreEqual(114, Right);
      Assert.AreEqual( 56, Bottom);
    end
  );
  FWinapiWindowsMocks.FillRect_Result(
    procedure(const DC: HDC; const Rect: TRect; Brush: HBRUSH)
    begin
      FillRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 76, 60), Rect);
      Assert.AreEqual<HBRUSH>(FBrush, Brush);
    end
  );

  WinControlNCPaint;

  Assert.IsTrue(DrawEdgeInvoked);
  Assert.IsTrue(IntersectClipRectInvoked);
  Assert.IsTrue(FillRectInvoked);
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Ctl3D(const Ctl3D: Boolean; const Expected: UINT);
begin
  var GetWindowLongPtrInvoked  := False;
  var DrawEdgeInvoked          := False;
  var IntersectClipRectInvoked := False;
  var FillRectInvoked          := False;

  const BrushMock      = TMock<TBrush>.Create;
  const WinControlMock = TMock<TWinControl>.Create;

  BrushMock
    .Setup
    .WillReturn(FBrush)
    .When
    .GetHandle;

  WinControlMock
    .Setup
    .WillReturn(FWinControlWnd)
    .When
    .GetHandle;
  WinControlMock
    .Setup
    .WillReturn(bkTile.AsValue)
    .When
    .GetBevelKind;
  WinControlMock
    .Setup
    .WillReturn(Ctl3D)
    .When
    .GetCtl3D;
  WinControlMock
    .Setup
    .WillReturn(10)
    .When
    .GetBorderWidth;
  WinControlMock
    .Setup
    .WillReturn(BrushMock.Instance)
    .When
    .GetBrush;

  FWinControl := WinControlMock.Instance;

  FWinapiWindowsMocks.GetClientRect_Result(FClientRect);
  FWinapiWindowsMocks.GetWindowRect_Result(FWindowRect);
  FWinapiWindowsMocks.GetWindowLongPtr_Result(
    function(const Wnd: HWND; const Index: Integer): LONG_PTR
    begin
      GetWindowLongPtrInvoked := True;

      Assert.AreEqual<HWND>(FWinControlWnd, Wnd);
      Assert.AreEqual(GWL_STYLE, Index);

      Result := -1;
    end
  );
  FWinapiWindowsMocks.DrawEdge_Result(
    procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
    begin
      DrawEdgeInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(-399, -429, -309, -366), Rect);
      Assert.AreEqual<UINT>(0, Edge);
      Assert.AreEqual<UINT>(Expected, Flags);
    end
  );
  FWinapiWindowsMocks.IntersectClipRect_Result(
    procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
    begin
      IntersectClipRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual(-399, Left);
      Assert.AreEqual(-429, Top);
      Assert.AreEqual(-309, Right);
      Assert.AreEqual(-366, Bottom);
    end
  );
  FWinapiWindowsMocks.FillRect_Result(
    procedure(const DC: HDC; const Rect: TRect; Brush: HBRUSH)
    begin
      FillRectInvoked := True;

      Assert.AreEqual<HDC>(FDC, DC);
      Assert.AreEqual<TRect>(TRect.InlineCreate(0, 0, 76, 60), Rect);
      Assert.AreEqual<HBRUSH>(FBrush, Brush);
    end
  );

  WinControlNCPaint;

  Assert.IsTrue(GetWindowLongPtrInvoked);
  Assert.IsTrue(DrawEdgeInvoked);
  Assert.IsTrue(IntersectClipRectInvoked);
  Assert.IsTrue(FillRectInvoked);
end;

procedure TWinControlNCPaintTests.Setup;
begin
  FWinapiWindowsMocks := TWinapiWindowsMocks.Create;
  FRenderingExtMocks  := TRenderingExtMocks.Create;

  FWinControlWnd := 453;
  FDC            := 599;
  FBrush         := 111;

  FClientRect := TRect.InlineCreate(101, 57, 151, 79);
  FWindowRect := TRect.InlineCreate( 37, 23, 113, 83);
end;

procedure TWinControlNCPaintTests.TearDown;
begin
  FWinControl := nil;

  FreeAndNil(FRenderingExtMocks);
  FreeAndNil(FWinapiWindowsMocks);
end;

initialization
  TDUnitX.RegisterTestFixture(TWinControlNCPaintTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
