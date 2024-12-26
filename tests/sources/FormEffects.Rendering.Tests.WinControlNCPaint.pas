/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.Tests.WinControlNCPaint.pas          *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering.Tests.WinControlNCPaint;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks;

{$IFDEF FORM_EFFECTS_TESTS}

type

{ TWinControlNCPaintTests}

  [TestFixture]
  TWinControlNCPaintTests = class
  strict private
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
  Delphi.Mocks,
  System.Types,
  System.Rtti,
  System.SysUtils,
  System.Math,
  FormEffects.TypeHelpers,
{$IFDEF USE_BILLENIUM_EFFECTS}
  teRender,
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
  FormEffects.Rendering,
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  FormEffects.TypeHelpers.Mocks,
  FormEffects.Winapi.Windows.Mocks,
  FormEffects.Rendering.Ext.Mocks;

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
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;
  const RenderingExtMock  = TMock<TRenderingExtMocks>.Create;

  var SaveDCInvokeIndex := 0;

  const ControlStyle: TControlStyle = [ControlStyleItem];

  const WinControlMock = TMock<TWinControl>.Create;

  WinControlMock
    .Setup
    .WillReturn(ControlStyle)
    .When
    .GetControlStyle;
  WinControlMock
    .Setup
    .WillReturn(FWinControlWnd)
    .When
    .GetHandle;

  FWinControl := WinControlMock.Instance;

  RenderingExtMock
    .Setup
    .Expect
    .Once
    .When
    .NCPrintControl(
      It0.IsEqualTo<HWND>(FWinControlWnd),
      It1.IsEqualTo<TWinControl>(FWinControl),
      It2.IsEqualTo<HDC>(FDC)
    );
  RenderingExtMock
    .Setup
    .Expect
    .Exactly(IfThen(Themed or (ControlStyleItem = csNeedsBorderPaint), 1, 0))
    .When
    .PaintThemeBorder(It0.IsEqualTo<TWinControl>(FWinControl), It1.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(2)
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));
  WinapiWindowsMock
    .Setup
    .WillExecute(
      function(const Args: TArray<TValue>; const ReturnType: TRttiType): TValue
      begin
        Inc(SaveDCInvokeIndex);

        Result := SaveDCInvokeIndex;
      end
    )
    .When
    .SaveDC(It0.IsEqualTo<HDC>(FDC));

  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(1)
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(1));
  WinapiWindowsMock
    .Setup
    .Expect
    .Exactly(1)
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsEqualTo<Integer>(2));
  WinapiWindowsMock
    .Setup
    .WillExecute(
      function(const Args: TArray<TValue>; const ReturnType: TRttiType): TValue
      begin
        Assert.AreEqual(SaveDCInvokeIndex, Args[2].AsInteger);

        Dec(SaveDCInvokeIndex);
      end
    )
    .When
    .RestoreDC(It0.IsEqualTo<HDC>(FDC), It1.IsAny<Integer>);

  WinControlNCPaint(Themed);

  RenderingExtMock.Verify;
  WinapiWindowsMock.Verify;
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Bevel_Edges(
  const BevelEdge: TBevelEdge;
  const Expected: UINT
);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

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
    .WillReturn(bkTile)
    .When
    .GetBevelKind;
  WinControlMock
    .Setup
    .WillReturn(BevelEdges)
    .When
    .GetBevelEdges;
  WinControlMock
    .Setup
    .WillReturn(bvSpace)
    .When
    .GetBevelInner;
  WinControlMock
    .Setup
    .WillReturn(bvSpace)
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

  WinapiWindowsMock
    .Setup
    .WillReturn(FClientRect)
    .When
    .GetClientRect(It0.IsAny<HWND>);

  WinapiWindowsMock
    .Setup
    .WillReturn(FWindowRect)
    .When
    .GetWindowRect(It0.IsAny<HWND>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .IntersectClipRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(ExpectedRect.Left),
      It2.IsEqualTo<Integer>(ExpectedRect.Top),
      It3.IsEqualTo<Integer>(ExpectedRect.Right),
      It4.IsEqualTo<Integer>(ExpectedRect.Bottom)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DrawEdge(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(ExpectedRect),
      It2.IsEqualTo<UINT>(0),
      It3.IsEqualTo<UINT>(Expected)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .FillRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 76, 60)),
      It2.IsEqualTo<HBRUSH>(FBrush)
    );

  WinControlNCPaint;

  WinapiWindowsMock.Verify;
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Bevel_Inner(
  const BevelInner: TBevelCut;
  const Expected: UINT
);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const BrushMock      = TMock<TBrush>.Create;
  const WinControlMock = TMock<TWinControl>.Create;

  BrushMock
    .Setup
    .WillReturn(FBrush)
    .When
    .GetHandle;

  WinControlMock
    .Setup
    .WillReturn(bkTile)
    .When
    .GetBevelKind;
  WinControlMock
    .Setup
    .WillReturn(BevelInner)
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

  WinapiWindowsMock
    .Setup
    .WillReturn(FClientRect)
    .When
    .GetClientRect(It0.IsAny<HWND>);

  WinapiWindowsMock
    .Setup
    .WillReturn(FWindowRect)
    .When
    .GetWindowRect(It0.IsAny<HWND>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .IntersectClipRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(64),
      It2.IsEqualTo<Integer>(34),
      It3.IsEqualTo<Integer>(114),
      It4.IsEqualTo<Integer>(56)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DrawEdge(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(64, 34, 114, 56)),
      It2.IsEqualTo<UINT>(Expected),
      It3.IsEqualTo<UINT>($2000)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .FillRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 76, 60)),
      It2.IsEqualTo<HBRUSH>(FBrush)
    );

  WinControlNCPaint;

  WinapiWindowsMock.Verify;
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Bevel_Kind(
  const BevelKind: TBevelKind;
  const Expected: UINT
);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

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
    .WillReturn(BevelKind)
    .When
    .GetBevelKind;
  WinControlMock
    .Setup
    .WillReturn(BrushMock.Instance)
    .When
    .GetBrush;

  FWinControl := WinControlMock.Instance;

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetClientRect(It0.IsEqualTo<HWND>(FWinControlWnd));
  WinapiWindowsMock
    .Setup
    .WillReturn(FClientRect)
    .When
    .GetClientRect(It0.IsEqualTo<HWND>(FWinControlWnd));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetWindowRect(It0.IsEqualTo<HWND>(FWinControlWnd));
  WinapiWindowsMock
    .Setup
    .WillReturn(FWindowRect)
    .When
    .GetWindowRect(It0.IsEqualTo<HWND>(FWinControlWnd));

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .IntersectClipRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(-389),
      It2.IsEqualTo<Integer>(-419),
      It3.IsEqualTo<Integer>(-339),
      It4.IsEqualTo<Integer>(-397)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DrawEdge(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(-389, -419, -339, -397)),
      It2.IsEqualTo<UINT>(0),
      It3.IsEqualTo<UINT>(Expected)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .ExcludeClipRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(-389),
      It2.IsEqualTo<Integer>(-419),
      It3.IsEqualTo<Integer>(-339),
      It4.IsEqualTo<Integer>(-397)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .FillRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 76, 60)),
      It2.IsEqualTo<HBRUSH>(FBrush)
    );

  WinControlNCPaint;

  WinapiWindowsMock.Verify;
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Bevel_Outer(
  const BevelOuter: TBevelCut;
  const Expected: UINT
);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

  const BrushMock      = TMock<TBrush>.Create;
  const WinControlMock = TMock<TWinControl>.Create;

  BrushMock
    .Setup
    .WillReturn(FBrush)
    .When
    .GetHandle;

  WinControlMock
    .Setup
    .WillReturn(bkTile)
    .When
    .GetBevelKind;
  WinControlMock
    .Setup
    .WillReturn(BevelOuter)
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

  WinapiWindowsMock
    .Setup
    .WillReturn(FClientRect)
    .When
    .GetClientRect(It0.IsAny<HWND>);

  WinapiWindowsMock
    .Setup
    .WillReturn(FWindowRect)
    .When
    .GetWindowRect(It0.IsAny<HWND>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .IntersectClipRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(64),
      It2.IsEqualTo<Integer>(34),
      It3.IsEqualTo<Integer>(114),
      It4.IsEqualTo<Integer>(56)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DrawEdge(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(64, 34, 114, 56)),
      It2.IsEqualTo<UINT>(Expected),
      It3.IsEqualTo<UINT>($2000)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .FillRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 76, 60)),
      It2.IsEqualTo<HBRUSH>(FBrush)
    );

  WinControlNCPaint;

  WinapiWindowsMock.Verify;
end;

procedure TWinControlNCPaintTests.should_use_clip_rect_for_Ctl3D(const Ctl3D: Boolean; const Expected: UINT);
begin
  const WinapiWindowsMock = TMock<TWinapiWindowsMocks>.Create;

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
    .WillReturn(bkTile)
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

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .GetWindowLongPtr(It0.IsEqualTo<HWND>(FWinControlWnd), It1.IsEqualTo<Integer>(GWL_STYLE));
  WinapiWindowsMock
    .Setup
    .WillReturn(-1)
    .When
    .GetWindowLongPtr(It0.IsEqualTo<HWND>(FWinControlWnd), It1.IsEqualTo<Integer>(GWL_STYLE));

  WinapiWindowsMock
    .Setup
    .WillReturn(FClientRect)
    .When
    .GetClientRect(It0.IsAny<HWND>);

  WinapiWindowsMock
    .Setup
    .WillReturn(FWindowRect)
    .When
    .GetWindowRect(It0.IsAny<HWND>);

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .IntersectClipRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<Integer>(-399),
      It2.IsEqualTo<Integer>(-429),
      It3.IsEqualTo<Integer>(-309),
      It4.IsEqualTo<Integer>(-366)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .DrawEdge(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(-399, -429, -309, -366)),
      It2.IsEqualTo<UINT>(0),
      It3.IsEqualTo<UINT>(Expected)
    );

  WinapiWindowsMock
    .Setup
    .Expect
    .Once
    .When
    .FillRect(
      It0.IsEqualTo<HDC>(FDC),
      It1.IsEqualTo<TRect>(TRect.InlineCreate(0, 0, 76, 60)),
      It2.IsEqualTo<HBRUSH>(FBrush)
    );

  WinControlNCPaint;

  WinapiWindowsMock.Verify;
end;

procedure TWinControlNCPaintTests.Setup;
begin
  FWinControlWnd := 453;
  FDC            := 599;
  FBrush         := 111;

  FClientRect := TRect.InlineCreate(101, 57, 151, 79);
  FWindowRect := TRect.InlineCreate( 37, 23, 113, 83);
end;

procedure TWinControlNCPaintTests.TearDown;
begin
  FWinControl := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TWinControlNCPaintTests);

{$ENDIF ~ FORM_EFFECTS_TESTS}

end.
