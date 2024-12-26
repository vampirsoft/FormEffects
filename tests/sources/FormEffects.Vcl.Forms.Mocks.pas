/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.Controls.Mocks.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.Forms.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Messages, Winapi.Windows,
  System.SysUtils, System.Classes, System.Generics.Collections,
  Vcl.Forms,
  FormEffects.System.Classes.Mocks, FormEffects.Vcl.Controls.Mocks;

type

  TForm = class;

{ TControlScrollBar }

  TControlScrollBar = class abstract(TPersistent)
  public
    function GetVisible: Boolean; virtual; abstract;
    function GetStyle: TScrollBarStyle; virtual; abstract;
    procedure SetVisible(const Value: Boolean); virtual; abstract;
    procedure SetStyle(const value: TScrollBarStyle); virtual; abstract;

  public
    property Visible: Boolean read GetVisible write SetVisible;
    property Style: TScrollBarStyle read GetStyle write SetStyle;
  end;

{ TScrollingWinControl }

  TScrollingWinControl = class abstract(TWinControl)
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
    FActive: Boolean;
    FMDIChildCount: Integer;
    FBorderStyle: TFormBorderStyle;
    FWindowState: TWindowState;
    FMDIChildren: TList<TForm>;

  public
    function GetClientHandle: HWND; virtual; abstract;
    function GetFormStyle: TFormStyle; virtual; abstract;
    procedure SetClientHandle(const Value: HWND); virtual; abstract;
    procedure SetFormStyle(const Value: TFormStyle); virtual; abstract;

  strict private
    function get_MDIChildren(I: Integer): TForm;
    procedure set_MDIChildren(I: Integer; const Value: TForm);

  strict private
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;

  protected
    property FormStyle: TFormStyle read GetFormStyle write SetFormStyle;

  public
    constructor Create(Owner: TComponent); overload; override;
    destructor Destroy; override;

  public
    property Active: Boolean read FActive write FActive;
    property ClientHandle: HWND read GetClientHandle write SetClientHandle;
    property WindowState: TWindowState read FWindowState write FWindowState;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle;
    property MDIChildCount: Integer read FMDIChildCount;
    property MDIChildren[I: Integer]: TForm read get_MDIChildren write set_MDIChildren;
  end;

  TCustomFormClass = class of TCustomForm;

{ TForm }

  TForm = class(TCustomForm)
  public
    property FormStyle;
  end;

{ TApplication }

  TApplication = class(TComponent)
  strict private
    FMainForm: TForm;

  public
    property MainForm: TForm read FMainForm write FMainForm;
  end;

var
  Application: TApplication;

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

{ TCustomForm }

constructor TCustomForm.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FMDIChildren := TList<TForm>.Create;
end;

destructor TCustomForm.Destroy;
begin
  FreeAndNil(FMDIChildren);

  inherited Destroy;
end;

function TCustomForm.get_MDIChildren(I: Integer): TForm;
begin
  Result := FMDIChildren[I];
end;

procedure TCustomForm.set_MDIChildren(I: Integer; const Value: TForm);
begin
  FMDIChildren.Insert(I, Value);
end;

procedure TCustomForm.WMNCPaint(var Message: TWMNCPaint);
begin
end;

initialization
  Application := TApplication.Create(nil);

finalization
  FreeAndNil(Application);

end.
