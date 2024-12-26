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
  System.Generics.Collections,
  Vcl.Forms,
  FormEffects.System.Classes.Mocks,
  FormEffects.Vcl.Controls.Mocks;

type

  TForm = class;

{ TControlScrollBar }

  TControlScrollBar = class abstract(TPersistent)
  public
    function GetVisible: Boolean; virtual; abstract;
    function GetStyle: TScrollBarStyle; virtual; abstract;

  public
    property Visible: Boolean read GetVisible;
    property Style: TScrollBarStyle read GetStyle;
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
  public
    function GetActive: Boolean; virtual; abstract;
    function GetMDIChildren: TList<TForm>; virtual; abstract;
    function GetClientHandle: HWND; virtual; abstract;
    function GetFormStyle: TFormStyle; virtual;
    function GetWindowState: TWindowState; virtual;
    function GetBorderStyle: TFormBorderStyle; virtual; abstract;
    procedure SetBorderStyle(const Value: TFormBorderStyle); virtual; abstract;

  strict private
    function GetMDIChildCount: Integer;
    function get_MDIChildren(I: Integer): TForm;

  strict private
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;

  protected
    property FormStyle: TFormStyle read GetFormStyle;

  public
    property Active: Boolean read GetActive;
    property ClientHandle: HWND read GetClientHandle;
    property WindowState: TWindowState read GetWindowState;
    property BorderStyle: TFormBorderStyle read GetBorderStyle write SetBorderStyle;
    property MDIChildCount: Integer read GetMDIChildCount;
    property MDIChildren[I: Integer]: TForm read get_MDIChildren;
  end;

  TCustomFormClass = class of TCustomForm;

{ TForm }

  TForm = class(TCustomForm)
  public
    property FormStyle;
  end;

{ TApplication }

  TApplication = class(TComponent)
  public
    function GetMainForm: TForm; virtual;

  public
    property MainForm: TForm read GetMainForm;
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

procedure TCustomForm.WMNCPaint(var Message: TWMNCPaint);
begin
end;

{ TApplication }

function TApplication.GetMainForm: TForm;
begin
  Result := nil;
end;

end.
