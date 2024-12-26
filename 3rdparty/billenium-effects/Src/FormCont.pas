unit FormCont;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, consts, TransEff, teBkgrnd, teRender, ActnList, Windows,
  Messages, Forms, Graphics, Controls, StdCtrls;

type
  TFCFormAlign = (fcfaDefault, fcfaNone, fcfaCenter, fcfaClient, fcfaTopLeft,
    fcfaMainFormCenter);

  TFCFormChangeEvent = procedure(Sender: TObject;
    const OldForm, NewForm: TCustomForm; var CanChange: Boolean) of object;
  TFCFormCreateEvent = procedure(Sender: TObject;
    const Form: TCustomForm) of object;
  TFCFormDestroyEvent = procedure(Sender: TObject;
    const Form: TCustomForm) of object;

  EFormContainerError = class(Exception);

  TFCExtraData = class
  end;

  TFCExtraDataClass = class of TFCExtraData;

  TFCGetExtraDataClass = function: TFCExtraDataClass        of object;
  TFCGetExtraData      = procedure(ExtraData: TFCExtraData) of object;
  TFCSetExtraData      = procedure(ExtraData: TFCExtraData) of object;

  TFCFormData = class
  private
    FFormClass: TCustomFormClass;
    FForm: TCustomForm;

    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
  protected
    DoneShow: Boolean;

    procedure ReadData(AForm: TCustomForm);
    procedure DoHide;
    procedure DoShow;
  public
    FAlign: TFCFormAlign;
    FPosition: TPosition;
    FBorderIcons: TBorderIcons;
    FDescription: String;
    FExtraData: TFCExtraData;

    constructor Create(AForm: TCustomForm);
    destructor  Destroy; override;

    property Align: TFCFormAlign read FAlign write FAlign;
    property Position: TPosition read FPosition write FPosition;
    property BorderIcons: TBorderIcons read FBorderIcons write FBorderIcons;
    property Description: String read FDescription write FDescription;
    property ExtraData: TFCExtraData read FExtraData write FExtraData;
    property FormClass: TCustomFormClass read FFormClass;
    property Form: TCustomForm read FForm;
  end;

  TFormContainer = class(TScrollingWinControl)
  private
    FCanvas: TCanvas;
    FBackgroundOptions: TFCBackgroundOptions;
    FBorderStyle: TBorderStyle;
    FFlickerFree,
    DoCheckOnClose: Boolean;
    FForm: TCustomForm;
    FForms: TList;
    AllFormsData: TList;
    Locked: Boolean;
    FLRUForms: TList;
    FLRUFormIndex,
    NewLRUFormIndex,
    FLRUFormCapacity: Integer;
    FSaveLRUDestroyedForms: Boolean;
    FSafeFormDestroy: Boolean;
    FOnFormChange: TFCFormChangeEvent;
    FOnFormCreate: TFCFormCreateEvent;
    FOnFormDestroy: TFCFormDestroyEvent;

    procedure SetBackgroundOptions(Value: TFCBackgroundOptions);
    procedure SetBorderStyle(Value: TBorderStyle);
    function  GetForms(Index: Integer): TCustomForm;
    procedure SetForm(Value: TCustomForm; DestroyCurrent: Boolean);
    function  GetFormData(Index: Integer): TFCFormData;
    function  GetLRUForm(Index: Integer): TCustomForm;
    function  GetLRUFormData(Index: Integer): TFCFormData;
    procedure SetLRUFormCapacity(Value: Integer);
    function  GetPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
    function  GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    function  GetFormAlignToUse(Form: TCustomForm): TFCFormAlign;
    procedure AdjustForm(AForm: TCustomForm; CheckVisible: Boolean); virtual;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function  CheckFormsData: Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DestroyingLRUForm(FormData: TFCFormData);
    procedure DeleteFormData(FData: TFCFormData);
    function  DeleteLRUForm(F: TCustomForm): Boolean;
    function  DeleteLRUFormByIndex(Index: Integer): Boolean;
    function  FormAlign: TFCFormAlign;
    function  GetPalette: HPalette; override;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
{$ifndef D9UP}
    procedure AddActionList(ActionList: TCustomActionList);
    procedure RemoveActionList(ActionList: TCustomActionList);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
{$endif D9UP}
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;

    property  Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  FormCount: Integer;
    function  LRUFormCount: Integer;
    function  CloseQuery: Boolean;
    function  CloseQueryAll: Boolean;
    function  FormData: TFCFormData;
    function  IndexOf(Value: TCustomForm): Integer;
    function  CheckOnClose(Default: Boolean): Boolean;
    procedure ClearLRUHistory;
    function  CreateForm(AClass: TCustomFormClass): TCustomForm;
    function  CreateShowForm(AClass: TCustomFormClass;
      DestroyCurrent: Boolean = True): TCustomForm;
    function  CreateShowFormEx(AClass: TCustomFormClass;
      DestroyCurrent: Boolean = True;
      Transition: TTransitionEffect = nil;
      BackgrOptions: TFCBackgroundOptions = nil;
      Align: TFCFormAlign = fcfaDefault): TCustomForm;
    procedure ShowForm(AForm: TCustomForm;
      DestroyCurrent: Boolean = True);
    procedure ShowFormEx(AForm: TCustomForm = nil;
      DestroyCurrent: Boolean = True;
      Transition: TTransitionEffect = nil;
      BackgrOptions: TFCBackgroundOptions = nil;
      Align: TFCFormAlign = fcfaDefault);
    function ShowLRUForm(Index: Integer;
      DestroyCurrent: Boolean = True): Boolean;
    function ShowLRUFormEx(Index: Integer;
      DestroyCurrent: Boolean = True;
      Transition: TTransitionEffect = nil;
      BackgrOptions: TFCBackgroundOptions = nil;
      Align: TFCFormAlign = fcfaDefault): Boolean;
    function  HasNextLRUForm: Boolean;
    function  HasPriorLRUForm: Boolean;
    function  ShowNextLRUForm(DestroyCurrent: Boolean = True): Boolean;
    function  ShowNextLRUFormEx(DestroyCurrent: Boolean = True;
      Transition: TTransitionEffect = nil;
      BackgrOptions: TFCBackgroundOptions = nil;
      Align: TFCFormAlign = fcfaDefault): Boolean;
    function  ShowPriorLRUForm(DestroyCurrent: Boolean = True): Boolean;
    function  ShowPriorLRUFormEx(DestroyCurrent: Boolean = True;
      Transition: TTransitionEffect = nil;
      BackgrOptions: TFCBackgroundOptions = nil;
      Align: TFCFormAlign = fcfaDefault): Boolean;
    procedure DestroyForm(F: TCustomForm);
    procedure DestroyAllForms;

    property  Form: TCustomForm read FForm;
    property  Forms[Index: Integer]: TCustomForm read GetForms; default;
    property  FormsData[Index: Integer]: TFCFormData read GetFormData;
    property  LRUFormIndex: Integer read FLRUFormIndex;
    property  LRUForms[Index: Integer]: TCustomForm read GetLRUForm;
    property  LRUFormsData[Index: Integer]: TFCFormData read GetLRUFormData;
  published
    property  Align;
    property  AutoScroll default False;
    property  BackgroundOptions: TFCBackgroundOptions read FBackgroundOptions write SetBackgroundOptions;
    property  BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property  DragCursor;
    property  DragMode;
    property  Enabled;
    property  Color nodefault;
    property  Ctl3D;
    property  FlickerFree: Boolean read FFlickerFree write FFlickerFree default True;
    property  Font;
    property  HorzScrollBar;
    property  LRUFormCapacity: Integer read FLRUFormCapacity write SetLRUFormCapacity default 20;
    property  OnClick;
    property  OnDblClick;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnFormChange: TFCFormChangeEvent read FOnFormChange write FOnFormChange;
    property  OnFormCreate: TFCFormCreateEvent read FOnFormCreate write FOnFormCreate;
    property  OnFormDestroy: TFCFormDestroyEvent read FOnFormDestroy write FOnFormDestroy;
    property  OnMouseDown;
    property  OnMouseMove;
    property  OnMouseUp;
    property  ParentColor;
    property  ParentCtl3D;
    property  ParentFont;
    property  ParentShowHint;
    property  Picture: TPicture read GetPicture write SetPicture stored False; // For backwards compatibility with V1.x
    property  PopupMenu;
    property  SafeFormDestroy: Boolean read FSafeFormDestroy write FSafeFormDestroy default True;
    property  SaveLRUDestroyedForms: Boolean read FSaveLRUDestroyedForms write FSaveLRUDestroyedForms default False;
    property  ShowHint;
    property  TabOrder;
    property  TabStop;
    property  Version: String read GetVersion write SetVersion stored False;
    property  VertScrollBar;
    property  Visible;
    property  Anchors;
    property  BevelEdges;
    property  BevelInner;
    property  BevelOuter;
    property  BevelKind;
    property  BevelWidth;
    property  BiDiMode;
    property  BorderWidth;
    property  Constraints;
    property  DragKind;
    property  ParentBiDiMode;
    property  OnEndDock;
    property  OnStartDock;
    property  OnStartDrag;
    {$ifdef D7UP}
    property  ParentBackground;
    {$endif D7UP}
    {$ifdef D10UP}
    property  Padding;
    {$endif D10UP}
    {$ifdef D14UP}
    property Touch;
    property OnGesture;
    {$endif D14UP}
end;

  TFCGetExtraDataClassEvent = procedure(Sender: TObject;
    var ExtraDataClass: TFCExtraDataClass) of object;
  TFCExtraDataEvent = procedure(Sender: TObject;
    ExtraData: TFCExtraData) of object;

  TFCEmbeddedForm = class(TCustomForm)
  private
    FBackgroundOptions: TFCBackgroundOptions;
    FOnGetExtraDataClass: TFCGetExtraDataClassEvent;
    FOnGetExtraData,
    FOnSetExtraData: TFCExtraDataEvent;
    FAlignment: TFCFormAlign;

    function  GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetBackgroundOptions(Value: TFCBackgroundOptions);

    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function  GetPalette: HPalette; override;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    ParentFormContainer: TFormContainer;
  published
    // TCustomForm properties / events
    property Action;
    property ActiveControl;
    property BiDiMode;
    property Caption;
    property ClientHeight stored True;
    property ClientWidth stored True;
    property Color;
    property Ctl3D;
    property UseDockManager;
    property DockSite;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentFont stored True;
    property Font;
    property Height stored False;
    property HelpFile;
    property KeyPreview;
    property Menu;
//    property OldCreateOrder;
    property ObjectMenuItem;
    {$ifdef D7UP}
    property  ParentBackground;
    {$endif D7UP}
    property ParentBiDiMode;
    property ParentColor stored True;
    property ParentShowHint;
    property PixelsPerInch;
    property PopupMenu;
    property Scaled;
    property ShowHint;
    {$ifdef D14UP}
    property Touch;
    {$endif D14UP}
    property Width stored False;
    property OnCanResize;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnConstrainedResize;
    {$ifdef D6UP}
    property OnContextPopup;
    {$endif D6UP}
    property OnCreate;
    property OnDblClick;
    property OnDestroy;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    {$ifdef D14UP}
    property OnGesture;
    {$endif D14UP}
    property OnGetSiteInfo;
    property OnHide;
    property OnHelp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnStartDock;
    property OnUnDock;

    // Added properties / events
    property Alignment: TFCFormAlign read FAlignment write FAlignment default fcfaCenter;
    property BackgroundOptions: TFCBackgroundOptions read FBackgroundOptions write SetBackgroundOptions;
    property Version: String read GetVersion write SetVersion stored False;

    property OnEnter;
    property OnExit;

    property OnGetExtraDataClass: TFCGetExtraDataClassEvent read FOnGetExtraDataClass write FOnGetExtraDataClass;
    property OnGetExtraData: TFCExtraDataEvent read FOnGetExtraData write FOnGetExtraData;
    property OnSetExtraData: TFCExtraDataEvent read FOnSetExtraData write FOnSetExtraData;
  end;

  TFCEmbeddedFormClass = class of TFCEmbeddedForm;

  procedure FCKeyPreview(WinControl: TWinControl; var Key: Word;
    Shift: TShiftState);
  procedure FCIsShortCut(WinControl: TWinControl; var Msg: TWMKey;
    var Handled: Boolean);

implementation

{$ifdef D7UP}
uses Themes, UxTheme;
{$endif D7UP}

resourcestring
  rsFCLockedFormCont = 'FomContainer is locked';
  rsFCUnknownForm    = 'Form ''%s'' unknown';
  rsFCBorderStyle    = '''BorderStyle'' property of ''%s'' must be ''bsNone''';
  rsFCVisible        = '''Visible'' property of ''%s'' must be ''False'' at design time';
  rsFCState          = '''WindowState'' property of ''%s'' must be ''wsNormal''';
  rsFCStyle          = '''FormStyle'' property of ''%s'' must be ''fsNormal''';

type
  TFCCustomForm = class(TCustomForm);
  TFCWinControl = class(TWinControl);

  TFCOnFormDestroyData = class
  public
    Form: TCustomForm;
    OnFormDestroyBack: TNotifyEvent;

    procedure OnFormDestroy(Sender: TObject);
  end;

var
  OnFormDestroyList: TList = nil;

procedure HideEmbeddedForms(WinControl: TWinControl);
var
  i: Integer;
begin
  for i:=WinControl.ControlCount-1 downto 0 do
  begin
    if WinControl.Controls[i] is TWinControl then
      HideEmbeddedForms(WinControl.Controls[i] as TWinControl);
  end;

  if(WinControl is TFormContainer)             and
    (TFormContainer(WinControl).FormCount > 0) then
    (WinControl as TFormContainer).ShowForm(nil, False);
end;

function GetOnFormDestroyData(Form: TCustomForm): TFCOnFormDestroyData;
var
  i: Integer;
begin
  Result := nil;

  if OnFormDestroyList = nil then
    Exit;

  for i:=0 to OnFormDestroyList.Count-1 do
  begin
    if TFCOnFormDestroyData(OnFormDestroyList[i]).Form = Form then
    begin
      Result := TFCOnFormDestroyData(OnFormDestroyList[i]);
      Break;
    end;
  end;
end;

procedure FCKeyPreview(WinControl: TWinControl; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  ChildWinControl: TWinControl;
begin
  if Key <> 0 then
  begin
    for i:= 0 to WinControl.ControlCount-1 do
    begin
      if WinControl.Controls[i] is TWinControl then
      begin
        ChildWinControl := TWinControl(WinControl.Controls[i]);
        if ChildWinControl.Visible then
        begin
          FCKeyPreview(ChildWinControl, Key, Shift);
          if Key = 0
            then break;
          if(ChildWinControl is TCustomForm)          and
            TCustomForm  (ChildWinControl).KeyPreview then
          begin
            TFCWinControl(ChildWinControl).KeyDown(Key, Shift);
            if Key = 0
              then break;
          end;
        end;
      end;
    end;
  end;
end;

procedure FCIsShortCut(WinControl: TWinControl; var Msg: TWMKey;
 var Handled: Boolean);
var
 i: Integer;
 ChildWinControl: TWinControl;
begin
 for i:= 0 to WinControl.ControlCount-1 do
 begin
   if WinControl.Controls[i] is TWinControl then
   begin
     ChildWinControl := TWinControl(WinControl.Controls[i]);
     if ChildWinControl.Visible then
     begin
       FCIsShortCut(ChildWinControl, Msg, Handled);
       if WinControl.Controls[i] is TCustomForm then
         TFCCustomForm(WinControl.Controls[i]).OnShortCut(Msg,
           Handled);
     end;
   end;
 end;
end;

{ TFCFormData }
constructor TFCFormData.Create(AForm: TCustomForm);
begin
  FFormClass   := TCustomFormClass(AForm.ClassType);
  FAlign       := fcfaDefault;
  FPosition    := TFCCustomForm(AForm).Position;
  FBorderIcons := TFCCustomForm(AForm).BorderIcons;
  FExtraData   := nil;

  ReadData(AForm);
end;

destructor TFCFormData.Destroy;
begin
  ExtraData.Free;

  inherited;
end;

procedure TFCFormData.ReadData(AForm: TCustomForm);
begin
  DoneShow    := False;
  FForm       := AForm;
  Description := AForm.Caption;

  if Assigned(TFCCustomForm(AForm).OnHide) then
  begin
    FOnHide := TFCCustomForm(AForm).OnHide;
    TFCCustomForm(AForm).OnHide := nil;
  end;

  if Assigned(TFCCustomForm(AForm).OnShow) then
  begin
    FOnShow := TFCCustomForm(AForm).OnShow;
    TFCCustomForm(AForm).OnShow := nil;
  end;

  TFCWinControl(AForm).OnEnter := TFCCustomForm(AForm).OnActivate;
  TFCWinControl(AForm).OnExit  := TFCCustomForm(AForm).OnDeactivate;
end;

procedure TFCFormData.DoHide;
begin
  if DoneShow then
  begin
    if Assigned(FOnHide) then
    begin
      try
        FOnHide(Form);
      except
        on Exception do Application.HandleException(Form);
      end;
    end;

    DoneShow := False; // DoShow can be executed now
  end;
end;

procedure TFCFormData.DoShow;
begin
  if not DoneShow then
  begin
    if Assigned(FOnShow) then
    begin
      try
        FOnShow(Form);
      except
        on Exception do Application.HandleException(Form);
      end;
    end;

    DoneShow := True; // DoShow can not be re-executed
  end;
end;

{ TFCOnFormDestroyData }
procedure TFCOnFormDestroyData.OnFormDestroy(Sender: TObject);
begin
  HideEmbeddedForms(Form);
  OnFormDestroyList.Remove(Self);
  if OnFormDestroyList.Count = 0 then
  begin
    OnFormDestroyList.Free;
    OnFormDestroyList := nil;
  end;
  if Assigned(OnFormDestroyBack) then
  begin
    try
      OnFormDestroyBack(Sender);
    except
      on Exception do Application.HandleException(Sender);
    end;
  end;
  Free;
end;

{ TFormContainer }
constructor TFormContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  ControlStyle           :=
    [csAcceptsControls, csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks];
  AutoScroll             := False;
  Locked                 := False;
  FForm                  := nil;
  Width                  := 185;
  Height                 :=  41;
  FBorderStyle           := bsNone;
  FFlickerFree           := True;
  FSaveLRUDestroyedForms := False;
  FLRUFormIndex          :=  0;
  NewLRUFormIndex        :=  0;
  FLRUFormCapacity       := 20;
  FForms                 := TList.Create;
  AllFormsData           := TList.Create;
  FLRUForms              := TList.Create;
  FSafeFormDestroy       := True;
  FBackgroundOptions     := TFCBackgroundOptions.Create;
  DoCheckOnClose         := False;
end;

destructor TFormContainer.Destroy;
var
  i: Integer;
begin
  FBackgroundOptions.Free;
  FBackgroundOptions := nil;
  HideEmbeddedForms(Self);

  if Assigned(FForms) then
    for i:= 1 to FormCount do
      Forms[i].Visible := False;

  if Assigned(AllFormsData) then
    for i:= 0 to AllFormsData.Count-1 do
      TFCFormData(AllFormsData[i]).Free;

  FForms      .Free;
  FLRUForms   .Free;
  AllFormsData.Free;
  FCanvas     .Free;

  inherited Destroy;
end;

function TFormContainer.GetForms(Index: Integer): TCustomForm;
var
  FData: TFCFormData;
begin
  FData := FormsData[Index];
  if FData <> nil
  then Result := FData.Form
  else Result := nil;
end;

function TFormContainer.FormData: TFCFormData;
begin
  Result := FormsData[IndexOf(FForm)];
end;

function TFormContainer.GetFormData(Index: Integer): TFCFormData;
begin
  if(Index >= 1) and (Index <= FormCount)
  then Result := TFCFormData(FForms[Index-1])
  else Result := nil;
end;

function TFormContainer.GetLRUForm(Index: Integer): TCustomForm;
var
  FData: TFCFormData;
begin
  FData := LRUFormsData[Index];
  if FData <> nil
  then Result := FData.Form
  else Result := nil;
end;

function TFormContainer.GetLRUFormData(Index: Integer): TFCFormData;
begin
  if(Index >= 1) and (Index <= LRUFormCount)
  then Result := TFCFormData(FLRUForms[Index-1])
  else Result := nil;
end;

procedure TFormContainer.SetLRUFormCapacity(Value: Integer);
var
  i: Integer;
begin
  if FLRUFormCapacity <> Value then
  begin
    FLRUFormCapacity := Value;

    if FLRUFormCapacity < LRUFormCount then
    begin  // Eliminate LRU forms
      for i := LRUFormCount - FLRUFormCapacity downto 1 do
        DeleteLRUFormByIndex(1);
    end;
    if FLRUFormCapacity < FLRUForms.Capacity then
      FLRUForms.Capacity := FLRUFormCapacity;
  end;
end;

function TFormContainer.FormCount: Integer;
begin
  Result := FForms.Count;
end;

function TFormContainer.LRUFormCount: Integer;
begin
  Result := FLRUForms.Count;
end;

function TFormContainer.IndexOf(Value: TCustomForm): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i:=0 to FForms.Count-1 do
    if TFCFormData(FForms[i]).Form = Value then
    begin
      Result := i + 1;
      Break;
    end;
  if Result = 0 then
    raise EFormContainerError.Create(Format(rsFCUnknownForm, [Value.Name]));
end;

function TFormContainer.GetPicture: TPicture;
begin
  Result := BackgroundOptions.Picture;
end;

procedure TFormContainer.SetPicture(const Value: TPicture);
begin
  BackgroundOptions.Picture := Value;
end;

function TFormContainer.GetVersion: String;
begin
  Result := BilleniumEffectsVersion;
end;

procedure TFormContainer.SetVersion(const Value: String);
begin
end;

function TFormContainer.GetFormAlignToUse(Form: TCustomForm): TFCFormAlign;
var
  FormData: TFCFormData;
  BorderIcons: TBorderIcons;
  Position: TPosition;
begin
  Result := FormAlign;
  if Result = fcfaDefault
  then
  begin
    if Form is TFCEmbeddedForm
    then Result := TFCEmbeddedForm(Form).Alignment
    else
    begin
      FormData    := FormsData[IndexOf(Form)];
      BorderIcons := FormData.BorderIcons;
      Position    := FormData.Position;

      if biMaximize in BorderIcons
      then Result := fcfaClient
      else
        case Position of
          poDesigned: Result := fcfaNone;
          poDesktopCenter,
          poOwnerFormCenter,
          poScreenCenter  : Result := fcfaCenter;
          poMainFormCenter: Result := fcfaMainFormCenter;
          else Result := fcfaTopLeft;
        end;
    end;
  end;
end;

procedure TFormContainer.AdjustForm(AForm: TCustomForm; CheckVisible: Boolean);
var
  LeftNew, TopNew, WidthNew, HeightNew: Integer;
  P: TPoint;
  ParentForm: TCustomForm;
  AlignToUse: TFCFormAlign;
  MarginLeft,
  MarginTop,
  MarginRight,
  MarginBottom: Integer;
begin
  if(AForm <> nil) and (AForm.Visible or (not CheckVisible)) then
  begin
    with AForm do
    begin
      LeftNew   := 0;
      TopNew    := 0;
      WidthNew  := 0;
      HeightNew := 0;

      AlignToUse := GetFormAlignToUse(AForm);

      MarginLeft   := 0;
      MarginTop    := 0;
      MarginRight  := 0;
      MarginBottom := 0;
      {$ifdef D10UP}
      if AForm.AlignWithMargins then
      begin
        MarginLeft   := AForm.Margins.Left;
        MarginTop    := AForm.Margins.Top;
        MarginRight  := AForm.Margins.Right;
        MarginBottom := AForm.Margins.Bottom;
      end;
      {$endif D10UP}
      case AlignToUse of
        fcfaClient: { it큦 sizeable, so we adjust it to fit the Parent }
        begin
          LeftNew   := MarginLeft;
          TopNew    := MarginTop;
          WidthNew  := Self.ClientWidth  - MarginLeft - MarginRight;
          HeightNew := Self.ClientHeight - MarginTop  - MarginBottom;
        end;
        fcfaTopLeft: { it maintains its size, and positions at origin }
        begin
          LeftNew   := MarginLeft;
          TopNew    := MarginTop;
          WidthNew  := Width;
          HeightNew := Height;
        end;
        fcfaCenter: { it maintains its size, and positions at center }
        begin
          WidthNew  := Width;
          HeightNew := Height;

          if(Self.ClientWidth >= WidthNew)
          then { it큦 smaller horizontally, so we center it in the Parent }
            LeftNew := (Self.ClientWidth - WidthNew ) div 2
          else { it큦 bigger horizontally, so we position it at 0 }
            LeftNew := 0;
          if(Self.ClientHeight >= HeightNew)
          then { it큦 smaller vertically, so we center it in the Parent }
            TopNew := (Self.ClientHeight - HeightNew) div 2
          else { it큦 bigger vertically, so we position it at 0 }
            TopNew := 0;
        end;
        fcfaMainFormCenter: { it maintains its size, and positions at center of
                              main form }
        begin
          WidthNew   := Width;
          HeightNew  := Height;

          ParentForm := GetParentForm(Self);
          P.x        := (ParentForm.ClientWidth  - WidthNew ) div 2;
          P.y        := (ParentForm.ClientHeight - HeightNew) div 2;
          P          := ParentForm.ClientToScreen(P);
          P          := Self.ScreenToClient(P);
          LeftNew    := P.x;
          TopNew     := P.y;
        end;
        fcfaNone:
        begin
          LeftNew   := Left;
          TopNew    := Top;
          WidthNew  := Width;
          HeightNew := Height;
        end;
      end;

      if IsScrollBarVisible(Self, Self.Handle, sbHorizontal) then
      begin
        LeftNew  := Left;
        WidthNew := Width;
      end;

      if IsScrollBarVisible(Self, Self.Handle, sbVertical) then
      begin
        TopNew    := Top;
        HeightNew := Height;
      end;

      if(LeftNew  <> Left ) or (TopNew    <> Top   ) or
        (WidthNew <> Width) or (HeightNew <> Height) then
      begin
        SetBounds(LeftNew, TopNew, WidthNew, HeightNew);
        Realign;
      end;
    end;
  end;
end;

function TFormContainer.FormAlign: TFCFormAlign;
begin
  if FForm <> nil
  then Result := FormData.Align
  else Result := fcfaDefault;
end;

function TFormContainer.CloseQuery: Boolean;
begin
  Result := (FForm = nil) or FForm.CloseQuery;
end;

function TFormContainer.CloseQueryAll: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := FormCount downto 1 do
  begin
    Result := Forms[i].CloseQuery;
    if not Result then break;
  end;
end;

procedure TFormContainer.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  AdjustForm(FForm, True);
end;

procedure TFormContainer.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TFormContainer.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

{$ifndef D9UP}
procedure TFormContainer.AddActionList(ActionList: TCustomActionList);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    if TFCCustomForm(Form).FActionLists = nil then
      TFCCustomForm(Form).FActionLists := TList.Create;
    TFCCustomForm(Form).FActionLists.Add(ActionList);
  end;
end;

procedure TFormContainer.RemoveActionList(ActionList: TCustomActionList);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and (TFCCustomForm(Form).FActionLists <> nil) then
    TFCCustomForm(Form).FActionLists.Remove(ActionList);
end;

procedure TFormContainer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  case Operation of
    opInsert:
      if AComponent is TCustomActionList then
        AddActionList(TCustomActionList(AComponent));
    opRemove:
      if AComponent is TCustomActionList then
        RemoveActionList(TCustomActionList(AComponent));
  end;
end;
{$endif D9UP}

procedure TFormContainer.DestroyingLRUForm(FormData: TFCFormData);
var
  aux: TCustomForm;
  Method1,
  Method2: Pointer;
  ExtraDataClass     : TFCExtraDataClass;
  FCGetExtraDataClass: TFCGetExtraDataClass;
  FCSetExtraData     : TFCSetExtraData;
begin
  aux := FormData.FForm;
  FormData.FForm := nil;

  if aux is TFCEmbeddedForm
  then
  begin
    if Assigned(TFCEmbeddedForm(aux).OnGetExtraDataClass) then
    begin
      try
        if FormData.ExtraData = nil then
        begin
          TFCEmbeddedForm(aux).OnGetExtraDataClass(Self, ExtraDataClass);
          FormData.ExtraData := ExtraDataClass.Create;
        end;
        if Assigned(TFCEmbeddedForm(aux).OnSetExtraData) then
          TFCEmbeddedForm(aux).OnSetExtraData(Self, FormData.ExtraData);
      except
        on Exception do Application.HandleException(Self);
      end;
    end
  end
  else
  begin
    Method1 := aux.MethodAddress('FCGetExtraDataClass');
    Method2 := aux.MethodAddress('FCSetExtraData');

    if(Method1 <> nil) and (Method2 <> nil) then
    begin
      with TMethod(FCGetExtraDataClass) do
      begin
        Code := Method1;
        Data := aux;
      end;
      with TMethod(FCSetExtraData) do
      begin
        Code := Method2;
        Data := aux;
      end;

      try
        if FormData.ExtraData = nil then
          FormData.ExtraData := FCGetExtraDataClass.Create;
        FCSetExtraData(FormData.ExtraData);
      except
        on Exception do Application.HandleException(Self);
      end;
    end;
  end;
end;

procedure TFormContainer.SetForm(Value: TCustomForm; DestroyCurrent: Boolean);

  {$ifndef D9UP}
  procedure UpdateActionLists(Operation: TOperation);
  var
    I: Integer;
    Component: TComponent;
  begin
    for I := 0 to FForm.ComponentCount - 1 do
    begin
      Component := FForm.Components[I];
      if Component is TCustomActionList then
        case Operation of
          opInsert: AddActionList(TCustomActionList(Component));
          opRemove: RemoveActionList(TCustomActionList(Component));
        end;
    end;
  end;
  {$endif D9UP}

var
  aux,
  i: Integer;
  SaveFormData: TFCFormData;
  ParentForm: TCustomForm;
  FCOnFormDestroyData: TFCOnFormDestroyData;
  CloseAction: TCloseAction;
begin
  if Locked then
    raise EFormContainerError.Create(rsFCLockedFormCont);

  Locked := True;
  try
    if Value <> FForm then
    begin
      if(FForm <> nil) then
      begin
        aux := IndexOf(FForm)-1;
        SaveFormData := FormData;

        {$ifndef D9UP}
        UpdateActionLists(opRemove);
        {$endif D9UP}

        if Assigned(TFCCustomForm(FForm).OnClose) then
        begin
          if DestroyCurrent
          then CloseAction := caFree
          else CloseAction := caHide;
          try
            TFCCustomForm(FForm).OnClose(FForm, CloseAction);
          except
            on Exception do Application.HandleException(Self);
          end;

          if DoCheckOnClose then
          begin
            case CloseAction of
              caFree: DestroyCurrent := True;
              caHide: DestroyCurrent := False;
            end;
          end;
        end;

        if FForm.Menu <> nil then
        begin
          ParentForm := GetParentForm(Self);
          if ParentForm.Menu <> nil then
            ParentForm.Menu.Unmerge(FForm.Menu);
        end;

        if DestroyCurrent
        then
        begin
          if Assigned(OnFormDestroy) then
          begin
            try
              OnFormDestroy(Self, Form);
            except
              on Exception do Application.HandleException(Self);
            end;
          end;

          HideEmbeddedForms(FForm);
          FForm.Visible := False;
          SaveFormData.DoHide;

          if SaveLRUDestroyedForms
          then
          begin
            if(NewLRUFormIndex = 0)
            then
            begin
              if(LRUFormIndex <> 0) and (LRUFormIndex <> LRUFormCount)
              then
              begin
                for i := LRUFormCount downto LRUFormIndex+1 do
                  DeleteLRUFormByIndex(LRUFormCount);
              end
              else
              begin
                if LRUFormIndex = 0 then
                  FLRUForms.Add(FForms[aux]);
              end;
            end
            else
            begin
              if LRUFormIndex = 0 then
                FLRUForms.Add(FForms[aux]);
            end;
          end
          else
          begin
            if(NewLRUFormIndex = 0) then
            begin
              if(LRUFormIndex <> 0) and (LRUFormIndex <> LRUFormCount) then
              begin
                for i := LRUFormCount downto LRUFormIndex do
                  DeleteLRUFormByIndex(LRUFormCount);
              end;
            end;

            DeleteLRUForm(FForm);
          end;

          FForms.Delete(aux);
          DeleteFormData(SaveFormData);

          if(not(csDestroying in ComponentState)) and
            (GetParentForm(FForm) <> nil) then
          begin
            try
              if SaveLRUDestroyedForms then
                DestroyingLRUForm(SaveFormData);
            except
              on Exception do Application.HandleException(Self);
            end;

            if SafeFormDestroy
            then FForm.Release
            else FForm.Free
          end;
        end
        else
        begin
          if(csDestroying in ComponentState) and
            (Application.MainForm = nil)     and
            (TFCCustomForm(GetParentForm(Self)).FormStyle = fsMDIChild)
          then
          begin
            try
              FForm.Visible := False;
            except
              on E: EInvalidOperation do
              begin
                if E.Message <> SNoMDIForm then
                  raise;
              end;
            end;
          end
          else FForm.Visible := False;
          SaveFormData.DoHide;
          if(NewLRUFormIndex = 0)
          then
          begin
            if(LRUFormIndex <> 0) and (LRUFormIndex <> LRUFormCount)
            then
            begin
              for i := LRUFormCount - LRUFormIndex downto 1 do
                DeleteLRUFormByIndex(LRUFormCount);
            end
            else
            begin
              if LRUFormIndex = 0 then
                FLRUForms.Add(FForms[aux]);
            end;
          end
          else
          begin
            if LRUFormIndex = 0 then
              FLRUForms.Add(FForms[aux]);
          end;
        end;
      end;

      if LRUFormCount > LRUFormCapacity then
        DeleteLRUFormByIndex(1);

      FLRUFormIndex := 0;
      FForm         := Value;

      if FForm <> nil then
      begin
        if NewLRUFormIndex <> 0 then
          FLRUFormIndex := NewLRUFormIndex;
        AdjustForm(FForm, False);
        FormData.DoShow;
        if Assigned(FormData.FOnShow) then
          AdjustForm(FForm, False);
        FForm.Visible := True;

        ParentForm := GetParentForm(Self);
        if(FForm.Menu <> nil) and FForm.Menu.AutoMerge then
        begin
          if ParentForm.Menu <> nil then
            ParentForm.Menu.Merge(FForm.Menu);
        end;

        {$ifndef D9UP}
        UpdateActionLists(opInsert);
        {$endif D9UP}

        if(ParentForm.ParentWindow = 0)            and
          (GetOnFormDestroyData(ParentForm) = nil) then
        begin
          FCOnFormDestroyData                   := TFCOnFormDestroyData.Create;
          FCOnFormDestroyData.Form              := ParentForm;
          FCOnFormDestroyData.OnFormDestroyBack := TFCCustomForm(ParentForm).OnDestroy;

          if OnFormDestroyList = nil then
            OnFormDestroyList := TList.Create;

          OnFormDestroyList.Add(FCOnFormDestroyData);

          TFCCustomForm(ParentForm).OnDestroy := FCOnFormDestroyData.OnFormDestroy;
        end;
      end;
    end;
  finally
    Locked := False;
  end;
end;

function TFormContainer.CreateForm(AClass: TCustomFormClass): TCustomForm;
var
  aux: TCustomForm;
  FData: TFCFormData;
  Method: Pointer;
  FCGetExtraData: TFCGetExtraData;
begin
  if Locked then
    raise EFormContainerError.Create(rsFCLockedFormCont);

  Result := nil;
  Locked := True;
  try
    aux := AClass.CreateParented(Handle);

    if TFCCustomForm(aux).BorderStyle <> bsNone then
    begin
      aux.Release;
      raise EFormContainerError.Create(Format(rsFCBorderStyle, [AClass.ClassName]));
    end;
    if TFCCustomForm(aux).FormStyle <> fsNormal then
    begin
      aux.Release;
      raise EFormContainerError.Create(Format(rsFCStyle, [AClass.ClassName]));
    end;
    if aux.WindowState <> wsNormal then
    begin
      aux.Release;
      raise EFormContainerError.Create(Format(rsFCState, [AClass.ClassName]));
    end;
    if aux.Visible then
    begin
      aux.Release;
      raise EFormContainerError.Create(Format(rsFCVisible, [AClass.ClassName]));
    end;

    if AClass.InheritsFrom(TFCEmbeddedForm) then
    begin
      if TFCEmbeddedForm(aux).ParentFont then
      begin
        TFCEmbeddedForm(aux).Perform(CM_PARENTFONTCHANGED, 0, 0);
        TFCEmbeddedForm(aux).NotifyControls(CM_PARENTFONTCHANGED);
      end;
    end;

    InsertComponent(aux);
    InsertControl(aux);
    aux.BringToFront;

    if NewLRUFormIndex = 0
    then
    begin
      FData := TFCFormData.Create(aux);
      AllFormsData.Add(FData);
    end
    else
    begin
      FData := LRUFormsData[NewLRUFormIndex];
      FData.ReadData(aux);

      if aux is TFCEmbeddedForm
      then
      begin
        if Assigned(TFCEmbeddedForm(aux).OnGetExtraData) then
        begin
          try
            TFCEmbeddedForm(aux).OnGetExtraData(Self, FData.ExtraData);
          except
            on Exception do Application.HandleException(Self);
          end;
        end
      end
      else
      begin
        Method := aux.MethodAddress('FCGetExtraData');

        if Method <> nil then
        begin
          with TMethod(FCGetExtraData) do
          begin
            Code := Method;
            Data := aux;
          end;

          try
            FCGetExtraData(FData.ExtraData);
          except
            on Exception do Application.HandleException(Self);
          end;
        end;
      end;
    end;

    FForms.Add(FData);

    if Assigned(OnFormCreate) then
    begin
      try
        OnFormCreate(Self, aux);
      except
        on Exception do Application.HandleException(Self);
      end;
    end;

    Result := aux;
    AdjustForm(aux, False);
  finally
    Locked := False;
  end;
end;

function TFormContainer.CreateShowForm(AClass: TCustomFormClass;
  DestroyCurrent: Boolean): TCustomForm;
begin
  Result := CreateForm(AClass);
  ShowForm(Result, DestroyCurrent);
end;

function TFormContainer.CreateShowFormEx(AClass: TCustomFormClass;
  DestroyCurrent: Boolean; Transition: TTransitionEffect;
  BackgrOptions: TFCBackgroundOptions; Align: TFCFormAlign): TCustomForm;
begin
  Result := CreateForm(AClass);
  ShowFormEx(Result, DestroyCurrent, Transition, BackgrOptions, Align);
end;

procedure TFormContainer.ShowFormEx(AForm: TCustomForm; DestroyCurrent: Boolean;
  Transition: TTransitionEffect; BackgrOptions: TFCBackgroundOptions;
  Align: TFCFormAlign);

  function Destroying(Control: TControl): Boolean;
  begin
    Result := (csDestroying in Control.ComponentState) or
              ((Control.Parent <> nil) and Destroying(Control.Parent));
  end;

var
  SaveActiveControl: TControl;
  TransitionToUse: TTransitionEffect;
  DestroyTransition: Boolean;
  ParentForm: TCustomForm;
  CanChange,
  IsPrepared: Boolean;
begin
  try
    DestroyTransition := False;
    TransitionToUse := nil;

    if not Destroying(Self) then
    begin
      if AForm <> nil then
        TFCFormData(FForms[IndexOf(AForm)-1]).Align := Align;

      if(Transition = nil) and FlickerFree
      then
      begin
        TransitionToUse   := TFlickerFreeTransition.Create(nil);
        TransitionToUse.FlickerFreeWhenDisabled :=
          FlickerFreeTransition.FlickerFreeWhenDisabled;
        DestroyTransition := True;
      end
      else TransitionToUse := Transition;
    end;

    if TransitionToUse <> nil then
    begin
      if AutoScroll
      then IsPrepared := TransitionToUse.Prepare(Parent, BoundsRect)
      else IsPrepared := TransitionToUse.Prepare(Self  , ClientRect);
      if not IsPrepared then
      begin
        if DestroyTransition then
        begin
          TransitionToUse.Free;
          DestroyTransition := False;
        end;
        TransitionToUse := nil;
      end;
    end;

    try
      CanChange := True;
      if Assigned(OnFormChange) then
      begin
        try
          OnFormChange(Self, Form, AForm, CanChange);
        except
          on Exception do Application.HandleException(Self);
        end;
      end;
    if not CanChange then
        Abort;

      SaveActiveControl := Screen.ActiveControl;

      if(TransitionToUse <> nil)          and
         TransitionToUse.Prepared         and
        (TransitionToUse.Passes(nil) = 2) then
      begin
        SetForm(nil, DestroyCurrent);

        TransitionToUse.Prepare2ndPass;

        SetForm(AForm, False);
      end
      else SetForm(AForm, DestroyCurrent);

      if(BackgrOptions <> nil) and (BackgrOptions <> BackgroundOptions) then
        BackgroundOptions.Assign(BackgrOptions);

      if FForm <> nil then
      begin
        if(Screen.ActiveControl = nil) or
          (Screen.ActiveControl = FForm) or
          (not Screen.ActiveControl.TabStop) or
          ((SaveActiveControl <> nil) and
            (Screen.ActiveControl <> SaveActiveControl) and
            not IsChild(AForm.Handle, Screen.ActiveControl.Handle)) then
        begin
          if(FForm <> nil) and (IsWindowVisible(Handle)) then
          begin
            if(FForm.ActiveControl <> nil) and FForm.ActiveControl.CanFocus
            then FForm.ActiveControl.SetFocus
            else SendMessage(FForm.Handle, WM_NEXTDLGCTL, 0, 0);
          end
          else
          begin
            ParentForm := GetParentForm(Self);
            if(FForm.ActiveControl <> nil) and FForm.ActiveControl.CanFocus
            then ParentForm.ActiveControl := FForm.ActiveControl
            else
              if IsWindowVisible(ParentForm.Handle) then
                SendMessage(FForm.Handle, WM_NEXTDLGCTL, 0, 0);
          end;
        end;
      end;
      if(TransitionToUse <> nil) and TransitionToUse.Prepared and (Form = AForm) then
        TransitionToUse.Execute;
    finally
      if TransitionToUse <> nil then TransitionToUse.UnPrepare;
      if DestroyTransition then TransitionToUse.Free;
    end;
  finally
    DoCheckOnClose := False;
  end;
end;

procedure TFormContainer.ShowForm(AForm: TCustomForm; DestroyCurrent: Boolean);
begin
  ShowFormEx(AForm, DestroyCurrent, nil,
    nil, fcfaDefault);
end;

function TFormContainer.ShowLRUForm(Index: Integer;
  DestroyCurrent: Boolean): Boolean;
begin
  Result := ShowLRUFormEx(Index, DestroyCurrent, nil,
    nil, fcfaDefault);
end;

function TFormContainer.ShowLRUFormEx(Index: Integer;
  DestroyCurrent: Boolean;
  Transition: TTransitionEffect;
  BackgrOptions: TFCBackgroundOptions;
  Align: TFCFormAlign): Boolean;
var
  FData: TFCFormData;
begin
  Result := False;
  if(Index < 1) or (Index > FLRUForms.Count) or (Index = FLRUFormIndex) then
    Exit;

  NewLRUFormIndex := Index;
  try
    FData := LRUFormsData[Index];
    if FData.Form = nil then
      CreateForm(FData.FormClass);

    Result := True;
    ShowFormEx(FData.Form, DestroyCurrent,
      Transition, BackgrOptions,
      Align);
    finally
      NewLRUFormIndex := 0;
    end;
end;

function TFormContainer.HasNextLRUForm: Boolean;
begin
  Result :=
    (LRUFormCount   > 0) and
    (FLRUFormIndex <> 0) and
    (FLRUFormIndex <> LRUFormCount);
end;

function TFormContainer.HasPriorLRUForm: Boolean;
begin
  Result :=
    (LRUFormCount   > 0) and
    (FLRUFormIndex <> 1);
end;

function TFormContainer.ShowNextLRUForm(DestroyCurrent: Boolean): Boolean;
begin
  Result := ShowNextLRUFormEx(DestroyCurrent, nil,
    nil, fcfaDefault);
end;

function TFormContainer.ShowNextLRUFormEx(
  DestroyCurrent: Boolean;
  Transition: TTransitionEffect;
  BackgrOptions: TFCBackgroundOptions;
  Align: TFCFormAlign): Boolean;
begin
  Result := False;
  if not HasNextLRUForm then
    exit;

  Result :=
    ShowLRUFormEx(FLRUFormIndex + 1,
      DestroyCurrent,
      Transition,
      BackgrOptions,
      Align);
end;

function TFormContainer.ShowPriorLRUForm(DestroyCurrent: Boolean): Boolean;
begin
  Result := ShowPriorLRUFormEx(DestroyCurrent, nil,
    nil, fcfaDefault);
end;

function TFormContainer.ShowPriorLRUFormEx(
  DestroyCurrent: Boolean;
  Transition: TTransitionEffect;
  BackgrOptions: TFCBackgroundOptions;
  Align: TFCFormAlign): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if not HasPriorLRUForm then
    exit;

  if FLRUFormIndex = 0
  then Index := LRUFormCount
  else Index := FLRUFormIndex - 1;

  Result :=
    ShowLRUFormEx(Index,
      DestroyCurrent,
      Transition,
      BackgrOptions,
      Align);
end;

procedure TFormContainer.DestroyForm(F: TCustomForm);
var
  Index: Integer;
  FormData: TFCFormData;
begin
  if F = nil then
    Exit;

  if F = FForm
  then ShowForm(nil, True)
  else
  begin
    if Assigned(OnFormDestroy) then
    begin
      try
        OnFormDestroy(Self, F);
      except
        on Exception do Application.HandleException(Self);
      end;
    end;

    Index := IndexOf(F) - 1;
    FormData := FormsData[Index+1];
    if SaveLRUDestroyedForms
    then DestroyingLRUForm(FormData)
    else DeleteLRUForm(F);
    FForms.Delete(Index);
    DeleteFormData(FormData);

    F.Visible := False;
    if SafeFormDestroy
    then F.Release
    else F.Free
  end;

  Assert(CheckFormsData, 'CheckFormsData');
end;

procedure TFormContainer.DestroyAllForms;
var
  i: Integer;
begin
  for i := FormCount downto 1 do
    DestroyForm(Forms[i]);
end;

function TFormContainer.CheckFormsData: Boolean;
var
  i: Integer;
begin
  Result := true;

  for i:= 0 to AllFormsData.Count-1 do
  begin
    if(FForms   .IndexOf(AllFormsData[i]) = -1) and
      (FLRUForms.IndexOf(AllFormsData[i]) = -1) then
    begin
      Result := False;
      break;
    end;
  end;

  if Result then
  begin
    for i:= 0 to FForms.Count-1 do
    begin
      if(AllFormsData.IndexOf(FForms[i]) = -1) then
      begin
        Result := False;
        break;
      end;
    end;
  end;

  if Result then
  begin
    for i:= 0 to FLRUForms.Count-1 do
    begin
      if(AllFormsData.IndexOf(FLRUForms[i]) = -1) then
      begin
        Result := False;
        break;
      end;
    end;
  end;
end;

procedure TFormContainer.DeleteFormData(FData: TFCFormData);
begin
  if(FForms.IndexOf(FData) = -1) and (FLRUForms.IndexOf(FData) = -1) then
  begin
    if AllFormsData.Remove(FData) <> -1 then
      TFCFormData(FData).Free;
  end;
end;

function TFormContainer.DeleteLRUForm(F: TCustomForm): Boolean;
var
  Index,
  Count: Integer;
begin
  Result := False;
  Index := 0;
  Count := FLRUForms.Count;
  while Index < Count do
  begin
    if TFCFormData(FLRUForms[Index]).Form = F
    then
    begin
      if DeleteLRUFormByIndex(Index+1)
      then
      begin
        Result := True;
        Dec(Count);
      end
      else Inc(Index);
    end
    else Inc(Index);
  end;
end;

function TFormContainer.DeleteLRUFormByIndex(Index: Integer): Boolean;
var
  FData: TFCFormData;
begin
  Result := False;

  if(Index >= 1) and (Index <= FLRUForms.Count) then
  begin
    FData := LRUFormsData[Index];
    FLRUForms.Delete(Index-1);
    DeleteFormData(FData);
    Result := True;
    if FLRUFormIndex   >= Index then
      Dec(FLRUFormIndex);
    if NewLRUFormIndex >= Index then
      Dec(NewLRUFormIndex);
  end;
end;

function TFormContainer.GetPalette: HPalette;
begin
  Result := BackgroundOptions.GetPalette;
end;

procedure TFormContainer.SetBackgroundOptions(Value: TFCBackgroundOptions);
begin
  BackgroundOptions.Assign(Value);
end;

procedure Scrolled(Control: TFormContainer);
begin
  if not(csDestroying in Control.ComponentState) then
  begin
    if Control.BackgroundOptions.IsActive then
      Control.BackgroundOptions.ControlChanged(Control);

    {$ifdef D7UP}
    if ThemeServices.ThemesEnabled                  and
       Assigned(Control.Parent)                     and
       (csParentBackground in Control.ControlStyle) then
       Control.Invalidate;
    {$endif D7UP}
  end;
end;

procedure TFormContainer.WMHScroll(var Message: TWMHScroll);
begin
    inherited;
    Scrolled(Self);
end;

procedure TFormContainer.WMVScroll(var Message: TWMVScroll);
begin
    inherited;
    Scrolled(Self);
end;

procedure TFormContainer.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  Invalidate;

  inherited;

  if(not(csDestroying in ComponentState)) and BackgroundOptions.IsActive then
    BackgroundOptions.ControlChanged(Self);
end;

procedure TFormContainer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  {$ifdef D7UP}
  if BEParentBackgroundPainted(Handle) then
    BackgroundOptions.DrawBackGround(Message.DC, nil, Rect(0, 0, 0, 0));
  {$endif D7UP}
  Message.Result := 1;
end;

procedure TFormContainer.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TFormContainer.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TFormContainer.Paint;
var
  R: TRect;
  Flags: Longint;
begin
  BackgroundOptions.DrawBackGround(Canvas.Handle, nil, Canvas.ClipRect);

  if csDesigning in ComponentState then
  begin
    Canvas.Pen  .Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
    Canvas.Pen  .Style := psSolid;

    R := ClientRect;
    Canvas.Brush.Color := clWhite;
    Canvas.Brush.Style := bsSolid;
    Flags := DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS;
    Flags := DrawTextBiDiModeFlags(Flags);
    DrawText(Canvas.Handle, PChar(Name), -1, R, Flags);
  end;
end;

procedure TFormContainer.SetName(const NewName: TComponentName);
begin
  inherited;

  Invalidate;
end;

procedure TFormContainer.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(BackgroundOptions) and (not(csDestroying in ComponentState)) then
    BackgroundOptions.Control := Self;
end;

function TFormContainer.CheckOnClose(Default: Boolean): Boolean;
begin
  DoCheckOnClose := True;
  Result         := Default;
end;

procedure TFormContainer.ClearLRUHistory;
var
  i: Integer;
begin
  for i := LRUFormCount downto 1 do
    DeleteLRUFormByIndex(1);
end;

{ TFCEmbeddedForm }

constructor TFCEmbeddedForm.Create(AOwner: TComponent);

  function FindUniqueName(const Parent: TWinControl;
    const Name: string): string;
  var
    i: Integer;
  begin
    i := 0;
    Result := Name;
    while Parent.FindComponent(Result) <> nil do
    begin
      Inc(i);
      Result := Format('%s_%d', [Name, i]);
    end;
  end;

var
  ParentValue: TWinControl;
begin
  FBackgroundOptions         := TFCBackgroundOptions.Create;
  FBackgroundOptions.Control := Self;
  FAlignment                 := fcfaCenter;

  inherited;

  ParentValue := FindControl(ParentWindow);
  if ParentValue <> nil then
    Name := FindUniqueName(ParentValue, Name);

  BorderStyle  := bsNone;
  ParentFont   := False;
end;

procedure TFCEmbeddedForm.CreateParams(var Params: TCreateParams);
begin
  BorderStyle := bsNone;

  inherited;
end;

destructor TFCEmbeddedForm.Destroy;
begin
  FBackgroundOptions.Free;
  FBackgroundOptions := nil;

  inherited;
end;

function TFCEmbeddedForm.GetPalette: HPalette;
begin
  Result := BackgroundOptions.GetPalette;
end;

function TFCEmbeddedForm.GetVersion: String;
begin
  Result := BilleniumEffectsVersion;
end;

function TFCEmbeddedForm.ParentFormContainer: TFormContainer;
begin
  if Parent = nil
  then Result := nil
  else Result := Parent as TFormContainer;
end;

procedure TFCEmbeddedForm.SetBackgroundOptions(Value: TFCBackgroundOptions);
begin
  BackgroundOptions.Assign(Value);
end;

procedure TFCEmbeddedForm.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(BackgroundOptions) and (not(csDestroying in ComponentState)) then
    BackgroundOptions.Control := Self;
end;

procedure TFCEmbeddedForm.WMPaint(var Message: TWMPaint);
var
  SaveDesigner: {$ifdef D6UP}IDesignerHook{$else}IDesigner{$endif D6UP};
begin
  SaveDesigner := Designer;
  if BackgroundOptions.IsActive then
    Designer := nil;

  inherited;

  Designer := SaveDesigner;
end;

procedure TFCEmbeddedForm.Paint;
var
  R: TRect;
begin
  if BackgroundOptions.IsActive
  then
  begin
    R := Canvas.ClipRect;
    BackgroundOptions.DrawBackGround(Canvas.Handle, nil, R);
  end;

  inherited;
end;

procedure TFCEmbeddedForm.SetVersion(const Value: String);
begin
end;

procedure TFCEmbeddedForm.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if BackgroundOptions.IsActive
  then
  begin
    {$ifdef D7UP}
    if BEParentBackgroundPainted(Handle) then
      BackgroundOptions.DrawBackGround(Message.DC, nil, Rect(0, 0, 0, 0));
    {$endif D7UP}
    Message.Result := 1;
  end
  else inherited;
end;

procedure TFCEmbeddedForm.WMWindowPosChanged(
  var Message: TWMWindowPosChanged);
begin
  inherited;

  if(not(csDestroying in ComponentState)) and
    BackgroundOptions.IsActive then
    BackgroundOptions.ControlChanged(Self);
end;

procedure TFCEmbeddedForm.CMParentFontChanged(var Message: TMessage);
begin
  if csDesigning in ComponentState
  then inherited
  else
  begin
    if ParentFont and (Message.wParam = 0) and (ParentFormContainer <> nil)
    then Font := ParentFormContainer.Font
    else inherited;
  end;
end;

end.
