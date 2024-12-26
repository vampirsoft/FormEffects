/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.Controls.Mocks.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.Forms.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Messages, Winapi.Windows,
  System.Generics.Collections,
  System.Classes,
  Vcl.Graphics,
  Vcl.Menus,
  Vcl.Forms,
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Graphics.Mocks,
  FormEffects.Vcl.Controls.Mocks;

type

  TForm = class;

{ TControlScrollBar }

  TControlScrollBar = class abstract(TPersistent)
  public
    function GetVisible: Boolean; virtual;
    function GetStyle: TScrollBarStyle; virtual; abstract;

  public
    property Visible: Boolean read GetVisible;
    property Style: TScrollBarStyle read GetStyle;
  end;

{ TScrollingWinControl }

  TScrollingWinControl = class abstract(TWinControl)
  strict private
    FAutoScroll: Boolean;

  protected
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll default False;

  public
    function GetVertScrollBar: TControlScrollBar; virtual; abstract;
    function GetHorzScrollBar: TControlScrollBar; virtual; abstract;

  public
    property VertScrollBar: TControlScrollBar read GetVertScrollBar;
    property HorzScrollBar: TControlScrollBar read GetHorzScrollBar;
  end;

{ TCustomForm }

  TCustomForm = class abstract(TScrollingWinControl)
  strict private
    FActiveControl: TWinControl;
    FHelpFile: string;
    FKeyPreview: Boolean;
    FMenu: TMainMenu;
    FObjectMenuItem: TMenuItem;
    FScaled: Boolean;
    FDesigner: IDesignerHook;
    FPosition: TPosition;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnHelp: THelpEvent;
    FOnHide: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FOnShortCut: TShortCutEvent;
    FOnShow: TNotifyEvent;

  public
    function GetActive: Boolean; virtual; abstract;
    function GetMDIChildren: TList<TForm>; virtual; abstract;
    function GetClientHandle: HWND; virtual; abstract;
    function GetFormStyle: TFormStyle; virtual;
    function GetWindowState: TWindowState; virtual;
    function GetBorderStyle: TFormBorderStyle; virtual; abstract;
    function GetBorderIcons: TBorderIcons; virtual; abstract;
    function GetCanvas: TCanvas; virtual; abstract;
    procedure SetBorderStyle(const Value: TFormBorderStyle); virtual;
    procedure SetBorderIcons(const Value: TBorderIcons); virtual;

  strict private
    function GetMDIChildCount: Integer;
    function get_MDIChildren(I: Integer): TForm;

  strict private
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;

  protected
    procedure Paint; dynamic; abstract;

  public
    function CloseQuery: Boolean; virtual; abstract;
    procedure Release; virtual; abstract;

  protected
    property BorderIcons: TBorderIcons read GetBorderIcons write SetBorderIcons;
    property ClientHandle: HWND read GetClientHandle;
    property FormStyle: TFormStyle read GetFormStyle;
    property ObjectMenuItem: TMenuItem read FObjectMenuItem write FObjectMenuItem;
    property MDIChildCount: Integer read GetMDIChildCount;
    property MDIChildren[I: Integer]: TForm read get_MDIChildren;
    property Position: TPosition read FPosition write FPosition default poDefaultPosOnly;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;

  public
    property ActiveControl: TWinControl read FActiveControl write FActiveControl;
    property Active: Boolean read GetActive;
    property BorderStyle: TFormBorderStyle read GetBorderStyle write SetBorderStyle;
    property HelpFile: string read FHelpFile write FHelpFile;
    property KeyPreview: Boolean read FKeyPreview write FKeyPreview default False;
    property Menu: TMainMenu read FMenu write FMenu;
    property WindowState: TWindowState read GetWindowState;
    property Scaled: Boolean read FScaled write FScaled default True;
    property Designer: IDesignerHook read FDesigner write FDesigner;
    property Canvas: TCanvas read GetCanvas;

  public
    property Caption;
  end;

  TCustomFormClass = class of TCustomForm;

{ TForm }

  TForm = class(TCustomForm)
  public
    property FormStyle;
    property ClientHandle;
    property MDIChildCount;
    property MDIChildren;
  end;

{ TApplication }

  TApplication = class(TComponent)
  public
    function GetMainForm: TForm; virtual;

  public
    procedure HandleException(Sender: TObject); virtual;

  public
    property MainForm: TForm read GetMainForm;
  end;

  TScreen = class(TComponent)
  strict private
    FActiveControl: TWinControl;

  public
    property ActiveControl: TWinControl read FActiveControl;
  end;

var
  Application: TApplication;
  Screen: TScreen;

function GetParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;
procedure SetResult_GetParentForm(Form: TCustomForm);

implementation

var
  GetParentFormResult: TCustomForm;

function GetParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;
begin
  Result := GetParentFormResult;
end;

procedure SetResult_GetParentForm(Form: TCustomForm);
begin
  GetParentFormResult := Form;
end;

{ TControlScrollBar }

function TControlScrollBar.GetVisible: Boolean;
begin
  Result := True;
end;

{ TCustomForm }

function TCustomForm.GetFormStyle: TFormStyle;
begin
  Result := fsMDIForm;
end;

function TCustomForm.GetMDIChildCount: Integer;
begin
  Result := GetMDIChildren.Count;
end;

function TCustomForm.GetWindowState: TWindowState;
begin
  Result := wsMaximized;
end;

function TCustomForm.get_MDIChildren(I: Integer): TForm;
begin
  Result := GetMDIChildren[I];
end;

procedure TCustomForm.SetBorderIcons(const Value: TBorderIcons);
begin
end;

procedure TCustomForm.SetBorderStyle(const Value: TFormBorderStyle);
begin
end;

procedure TCustomForm.WMNCPaint(var Message: TWMNCPaint);
begin
end;

{ TApplication }

function TApplication.GetMainForm: TForm;
begin
  Result := nil;
end;

procedure TApplication.HandleException(Sender: TObject);
begin
end;

end.
