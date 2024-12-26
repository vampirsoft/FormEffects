/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.pas                                  *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  FormEffects.TypeHelpers, FormEffects.FormContainer, FormEffects.Utils.ScrollBars, FormEffects.Utils.Rects
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Winapi.Windows.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

const // Form Effects messages
  // Thir party components interface
  FE_ID           = $41A2;
  FE_BASE         = CM_BASE + $0C4A;
  CM_FEPAINT      = FE_BASE + 0; // Paint client area to Billenium Effects' DC
  CM_FENCPAINT    = FE_BASE + 1; // Paint non client area to Billenium Effects' DC
  CM_FEFULLRENDER = FE_BASE + 2; // Paint whole control to Form Effects' DC

type
  TFEPaintCallback = procedure(const WinControl: TWinControl; const DC: HDC);

  TFERenderFlag = record
  private
    Value: Cardinal;
  end;

const
  feAuto: TFERenderFlag           = (Value: $00000000);
  fePaint: TFERenderFlag          = (Value: $00000001);
  fePrint: TFERenderFlag          = (Value: $00000002);
  feEmulate: TFERenderFlag        = (Value: $00000003);
  feCallback: TFERenderFlag       = (Value: $00000004);
  fePaintCopy: TFERenderFlag      = (Value: $00000005);

  feThemed: TFERenderFlag         = (Value: $10000000);
  feRefreshFocused: TFERenderFlag = (Value: $20000000);
  feOwnCanvas: TFERenderFlag      = (Value: $40000000);
  feNoRender: TFERenderFlag       = (Value: $80000000);

{$MESSAGE 'Need to test RegisterTEControl'}
procedure RegisterTEControl(const ControlClass: TControlClass;
  const NonClientRenderMode, ClientRenderMode: TFERenderFlag; const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil; const ClientCallback: TFEPaintCallback = nil); overload; inline;
{$MESSAGE 'Need to test RegisterTEControl'}
procedure RegisterTEControl(const ControlClassName: string;
  const NonClientRenderMode, ClientRenderMode: TFERenderFlag; const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil; const ClientCallback: TFEPaintCallback = nil); overload;

{$MESSAGE 'Need to test IsMaximizedMDIChild'}
function IsMaximizedMDIChild(const WinControl: TWinControl): Boolean;

{$MESSAGE 'Need to test RenderWindowToDC'}
procedure RenderWindowToDC(const Wnd, StopWnd: HWND; const WinControl: TWinControl; const DC: HDC; const Rect: TRect;
  const ClientCoordinates, CheckVisibility, CheckRegion, Fast: Boolean);

{$IFDEF FORM_EFFECTS_TESTS}
procedure CompleteFlags(const WinControl: TWinControl; var Flags: DWORD); inline;
{$ENDIF ~ FORM_EFFECTS_TESTS}

implementation

uses
  System.Types, System.SysUtils, System.Classes, System.Math, System.Generics.Collections;

{$REGION Internal definitions}

const
  RCF_RENDERNC         = $00000001; // Do render the non-client area
  RCF_REFRESHNC        = $00000002; // Always refresh the non-client area
  RCF_REFRESHFOCUSEDNC = $00000004; // Refresh the non-client area only if the control is focused
  RCF_PRINTNC          = $00000008; // Render non-client area using WM_PRINT message
  RCF_PAINTNC          = $00000010; // Render non-client area using WM_PAINT message
  RCF_EMULNC           = $00000020; // Render non-client with custom code
  RCF_CALLBACKNC       = $00000040; // Render non-client area using a callback method
  RCF_THEMEDNC         = $00000080; // Render non-client area XP themes
  RCF_PAINTCOPYNC      = $00000100; // Render non-client area setting the csPaintCopy control state
  RCF_BENCPREPAINT     = $00000200; // Use BE_NCPAINT, then render non-client area
  RCF_BENCPAINT        = $00000400; // Render non-client area only using BE_NCPAINT
  RCF_BENCPOSTPAINT    = $00000800; // Render non-client area, then use BE_NCPAINT
  RCF_OWNCANVASNC      = $00001000; // Render non-client area in a separate bitmap

  RCF_BEFULLRENDER     = $00080000; // Renders whole window at once using CM_BEFULLRENDER

  RCF_RENDER           = $00100000; // Do render the client area
  RCF_REFRESH          = $00200000; // Always refresh the client area
  RCF_REFRESHFOCUSED   = $00400000; // Refresh the client area only if the control is focused
  RCF_PRINT            = $00800000; // Render client area using WM_PRINT message
  RCF_PAINT            = $01000000; // Render client area using WM_PAINT message
  RCF_EMUL             = $02000000; // Render client with custom code
  RCF_CALLBACK         = $04000000; // Render client area using a callback method
  RCF_PAINTCOPY        = $08000000; // Render client area setting the csPaintCopy control state
  RCF_BEPREPAINT       = $10000000; // Use BE_PAINT, then render client area
  RCF_BEPAINT          = $20000000; // Render client area only using BE_PAINT
  RCF_BEPOSTPAINT      = $40000000; // Render client area, then use BE_PAINT
  RCF_OWNCANVAS        = $80000000; // Render client area in a separate bitmap

  RCF_RENDERMASK       = $FFF00000;
  RCF_RENDERNCMASK     = $00001FFF;

type

{ TRegControl }

  TRegControl = record
  public
    Flags: DWORD;
    NonClientCallback: TFEPaintCallback;
    ClientCallback: TFEPaintCallback;

  public
    class function Create(const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback): TRegControl; static; inline;

  public
    procedure Assign(Source: TRegControl);
    procedure Clear; inline;
  end;

{ TRegControlsManager }

  TRegControlsManager = class
  strict private
    FRegControls: TDictionary<string, TRegControl>;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;

  public
    procedure FindRegControl(const ControlClass: TClass; const RegControl: TRegControl); inline;
    procedure SaveRegControl(const ControlClassName: string; const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback); inline;
  end;

{ TRenderWindowToDCHelper }

  TRenderWindowToDCHelper = record
  strict private
    FWnd: HWND;
    FStopWnd: HWND;
    FParentWnd: HWND;
    FIsMaximizedMDIClient: Boolean;
    FIsMaximizedMDIChild: Boolean;
    FIsMDIClient: Boolean;
    FIsRenderWindow: Boolean;
    FWinControl: TWinControl;
    FWinControlType: TClass;

  strict private
    function IsMDIFormWithMaximizedMDIChild: Boolean; inline;
    function IsMaximizedMDIClient(ClassName: PChar): Boolean;
    procedure LoadWinControlData; inline;

  public
    class function Create(const Wnd, StopWnd: HWND;
      const WinControl: TWinControl): TRenderWindowToDCHelper; overload; static; inline;
    class function Create(const Wnd, StopWnd, ParentWnd: HWND;
      const WinControl: TWinControl): TRenderWindowToDCHelper; overload; static; inline;

  public
    procedure RenderWindowToDC(const DC: HDC; const Rect: TRect;
      const CheckVisibility, CheckRegion, Fast: Boolean; const RegControl: TRegControl);
  end;

var
  RegControlsManager: TRegControlsManager = nil;

function IsInheritsClass(const ClassType: TClass; const ClassName: string): Boolean;
begin
  var Parent := ClassType;
  while Parent <> TObject do
  begin
    if Parent.ClassNameIs(ClassName) then Exit(True);
    Parent := Parent.ClassParent;
  end;
  Result := False;
end;

function ResolveStopWnd(const Wnd, StopWnd: HWND): HWND; inline;
begin
  if IsChild(Wnd, StopWnd) then Exit(StopWnd);
  Result := 0;
end;

function GetWindowClientOffset(const Wnd: HWND): TPoint;
var
  ScreenRect: TRect;

begin
  Result.X := 0;
  Result.Y := 0;

  ClientToScreen(Wnd, Result);
  GetWindowRect(Wnd, ScreenRect);

  Result.X := Result.X - ScreenRect.Left;
  Result.Y := Result.Y - ScreenRect.Top;
end;

{ TRegControl }

procedure TRegControl.Assign(Source: TRegControl);
begin
  Flags             := Source.Flags;
  NonClientCallback := Source.NonClientCallback;
  ClientCallback    := Source.ClientCallback;
end;

procedure TRegControl.Clear;
begin
  Flags             := 0;
  NonClientCallback := nil;
  ClientCallback    := nil;
end;

class function TRegControl.Create(const Flags: DWORD; const NonClientCallback,
  ClientCallback: TFEPaintCallback): TRegControl;
begin
  Result.Flags             := Flags;
  Result.NonClientCallback := NonClientCallback;
  Result.ClientCallback    := ClientCallback;
end;

{ TRegControlsManager }

constructor TRegControlsManager.Create;
begin
  FRegControls := TDictionary<string, TRegControl>.Create;
end;

destructor TRegControlsManager.Destroy;
begin
  FreeAndNil(FRegControls);
end;

procedure TRegControlsManager.FindRegControl(const ControlClass: TClass; const RegControl: TRegControl);
begin
  if ControlClass = nil then Exit;

  for var RegControlPair in FRegControls do
  begin
    if IsInheritsClass(ControlClass, RegControlPair.Key) then
    begin
      RegControl.Assign(RegControlPair.Value);
      Exit;
    end;
  end;
end;

procedure TRegControlsManager.SaveRegControl(const ControlClassName: string; const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);

  if (FRegControls.ContainsKey(ControlClassName)) then FRegControls[ControlClassName].Assign(RegControl)
  else FRegControls.Add(ControlClassName, RegControl);
end;

{ TRenderWindowToDCHelper }

class function TRenderWindowToDCHelper.Create(const Wnd, StopWnd, ParentWnd: HWND;
  const WinControl: TWinControl): TRenderWindowToDCHelper;
begin
  with Result do
  begin
    FWnd       := Wnd;
    FStopWnd   := StopWnd;
    FParentWnd := ParentWnd;

    if Assigned(WinControl) then FWinControl := WinControl else FWinControl := FindControl(Wnd);
    LoadWinControlData;
  end;
end;

procedure TRenderWindowToDCHelper.LoadWinControlData;
var
  ClassName: array[0..63] of Char;

begin
  if Assigned(FWinControl) then
  begin
    FWinControlType := FWinControl.ClassType;
    StrPCopy(ClassName, FWinControl.ClassName);

    if IsMDIFormWithMaximizedMDIChild then
    begin // Edge changing
      SetWindowLong(
        Application.MainForm.ClientHandle,
        GWL_EXSTYLE,
        GetWindowLongPtr(Application.MainForm.ClientHandle, GWL_EXSTYLE) and not WS_EX_CLIENTEDGE
      );
      SetWindowPos(
        Application.MainForm.ClientHandle,
        0,
        0,
        0,
        0,
        0,
        SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER
      );
    end;

    FIsMaximizedMDIClient := False;
    FIsMaximizedMDIChild  := IsMaximizedMDIChild(FWinControl);
  end
  else
  begin
    GetClassName(FWnd, ClassName, Sizeof(ClassName));
    FWinControlType := GetClass(ClassName);

    FIsMaximizedMDIClient := IsMaximizedMDIClient(ClassName);
    FIsMaximizedMDIChild  := False;
  end;

  FIsMDIClient    := StrIComp(ClassName, 'MDICLIENT') = 0;
  FIsRenderWindow := False; {$MESSAGE 'Need to implement `TFERenderWindow` class for `StrIComp(ClassName, 'TFERenderWindow') = 0`'}
end;

procedure TRenderWindowToDCHelper.RenderWindowToDC(const DC: HDC; const Rect: TRect; const CheckVisibility, CheckRegion,
  Fast: Boolean; const RegControl: TRegControl);
begin
  if FIsRenderWindow then Exit;
{$MESSAGE WARN 'Not Implemented TRenderWindowToDCHelper.RenderWindowToDC'}
end;

function TRenderWindowToDCHelper.IsMaximizedMDIClient(ClassName: PChar): Boolean;
begin
  if StrIComp(ClassName, 'MDICLIENT') = 0 then
  begin
    for var I := 0 to Application.MainForm.MDIChildCount - 1 do
    begin
      const Child = Application.MainForm.MDIChildren[I];
      if Child.WindowState = wsMaximized then Exit(True);
    end;
  end;

  Result := False;
end;

function TRenderWindowToDCHelper.IsMDIFormWithMaximizedMDIChild: Boolean;
begin
  Result :=
    (FWinControl is TCustomForm)                                    and
    ((FWinControl as TCustomForm).GetInternalFormStyle = fsMDIForm) and
    IsMaximizedMDIClient('MDICLIENT');
end;

class function TRenderWindowToDCHelper.Create(const Wnd, StopWnd: HWND;
  const WinControl: TWinControl): TRenderWindowToDCHelper;
var
  ParentClassName: array[0..63] of Char;

begin
  var Parent := GetParent(Wnd);
  if Parent <> 0 then
  begin
    GetClassName(Parent, ParentClassName, SizeOf(ParentClassName));
    if CompareText(ParentClassName, TApplication.ClassName) = 0 then Parent := 0;
  end;

  Result := TRenderWindowToDCHelper.Create(Wnd, ResolveStopWnd(Wnd, StopWnd), Parent, WinControl);
end;

function GetRegControlsManager: TRegControlsManager; inline;
begin
  if RegControlsManager = nil then RegControlsManager := TRegControlsManager.Create;
  Result := RegControlsManager;
end;

function GetFlagsFromWindow(const Window: HWND): DWORD; inline;
begin
  if SendMessage(Window, CM_FEFULLRENDER, 0, FE_ID) = FE_ID then Exit(RCF_RENDER or RCF_RENDERNC or RCF_BEFULLRENDER);

  Result := 0;
  case SendMessage(Window, CM_FENCPAINT, 0, FE_ID) of
    FE_ID - 1: Result := RCF_RENDERNC or RCF_BENCPREPAINT;
    FE_ID    : Result := RCF_RENDERNC or RCF_BENCPAINT;
    FE_ID + 1: Result := RCF_RENDERNC or RCF_BENCPOSTPAINT;
  end;
  case SendMessage(Window, CM_FEPAINT, 0, FE_ID) of
    FE_ID - 1: Result := Result or RCF_RENDER or RCF_BEPREPAINT;
    FE_ID    : Result := Result or RCF_RENDER or RCF_BEPAINT;
    FE_ID + 1: Result := Result or RCF_RENDER or RCF_BEPOSTPAINT;
  end;
end;

procedure CompleteFlags(const WinControl: TWinControl; var Flags: DWORD); inline;
begin
{$MESSAGE WARN 'Not Implemented CompleteFlags'}
end;

procedure GetRegControl(const Window: HWND; const WinControl: TWinControl; var RegControl: TRegControl);
begin
  RegControl.Clear;

  var Flags := GetFlagsFromWindow(Window);

  if WinControl = nil then
  begin
    if Flags and RCF_RENDERNCMASK = 0 then Flags := RCF_RENDERNC or RCF_PRINTNC;
    if Flags and RCF_RENDERMASK   = 0 then Flags := Flags or RCF_RENDER or RCF_PAINT;
    RegControl.Flags := Flags;
  end
  else
  begin
    GetRegControlsManager.FindRegControl(WinControl.ClassType, RegControl);
    if Flags and RCF_BEFULLRENDER <> 0 then RegControl.Flags := Flags
    else
    begin
      if Flags and RCF_RENDERNCMASK <> 0 then
      begin
        if Flags and (RCF_BENCPREPAINT or RCF_BENCPOSTPAINT) <> 0 then
        begin
          RegControl.Flags := RegControl.Flags or (Flags and RCF_RENDERNCMASK);
        end
        else
        begin
          RegControl.Flags := (RegControl.Flags and (not RCF_RENDERNCMASK)) or (Flags and RCF_RENDERNCMASK);
        end;
      end;

      if Flags and RCF_RENDERMASK <> 0 then
      begin
        if Flags and (RCF_BEPREPAINT or RCF_BEPOSTPAINT) <> 0 then
        begin
          RegControl.Flags := RegControl.Flags or (Flags and RCF_RENDERMASK)
        end
        else
        begin
          RegControl.Flags := (RegControl.Flags and (not RCF_RENDERMASK)) or (Flags and RCF_RENDERMASK);
        end;
      end;

      CompleteFlags(WinControl, RegControl.Flags);
    end;
  end;
end;

{$ENDREGION Internal definitions}

procedure RegisterTEControl(const ControlClass: TControlClass;
  const NonClientRenderMode, ClientRenderMode: TFERenderFlag; const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil; const ClientCallback: TFEPaintCallback = nil);
begin
  RegisterTEControl(
    ControlClass.ClassName,
    NonClientRenderMode,
    ClientRenderMode,
    RefreshNonClient,
    RefreshClient,
    NonClientCallback,
    ClientCallback
  );
end;

procedure RegisterTEControl(const ControlClassName: string;
  const NonClientRenderMode, ClientRenderMode: TFERenderFlag; const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil; const ClientCallback: TFEPaintCallback = nil);
begin
{$MESSAGE WARN 'Not Implemented RegisterTEControl'}
end;

function IsMaximizedMDIChild(const WinControl: TWinControl): Boolean;
begin
  if
    (WinControl is TCustomForm)                                     and
    ((WinControl as TCustomForm).GetInternalFormStyle = fsMDIChild) and
    (Application.MainForm <> nil)                                   and
    (Application.MainForm.FormStyle = fsMDIForm)
  then
  begin
    if (WinControl as TCustomForm).WindowState = wsMaximized then Exit(True);

    for var I := 0 to Application.MainForm.MDIChildCount - 1 do
    begin
      const Child = Application.MainForm.MDIChildren[I];
      if Child.WindowState = wsMaximized then Exit(True);
    end;
  end;

  Result := False;  
end;

procedure RenderWindowToDC(const Wnd, StopWnd: HWND; const WinControl: TWinControl; const DC: HDC; const Rect: TRect;
  const ClientCoordinates, CheckVisibility, CheckRegion, Fast: Boolean);
begin
  if(Wnd = StopWnd) or (CheckVisibility and (not IsWindowVisible(Wnd))) then Exit;

  var TempRect := Rect;
  var Point: TPoint;

  if ClientCoordinates and not IsMaximizedMDIChild(WinControl) then
  begin
    const ClientOffset = GetWindowClientOffset(Wnd);
    OffsetRect(TempRect, ClientOffset.X, ClientOffset.Y);
    OffsetWindowOrgEx(DC, ClientOffset.x, ClientOffset.y, Point);
  end;

  with TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl) do
  try
    RenderWindowToDC(DC, TempRect, False, CheckRegion, Fast, TRegControl.Create(0, nil, nil));
  finally
    if not EqualRect(Rect, TempRect) then SetWindowOrgEx(DC, Point.x, Point.y, nil);
  end;
end;

initialization

finalization
  if Assigned(RegControlsManager) then FreeAndNil(RegControlsManager);

end.
