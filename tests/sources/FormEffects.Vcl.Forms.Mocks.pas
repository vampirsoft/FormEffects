/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Vcl.Controls.Mocks.pas                         *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2024 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Vcl.Forms.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes, System.Generics.Collections,
  Vcl.Forms,
  FormEffects.System.Classes.Mocks, FormEffects.Vcl.Controls.Mocks;

type

  TForm = class;

{ TControlScrollBar }

  TControlScrollBar = class(TPersistent)
  public type
    TKind = (Vertical, Horizontal);

  strict private
    FKind: TKind;
    FVisible: Boolean;
    FStyle: TScrollBarStyle;

  private
    constructor Create(const Kind: TKind); reintroduce;

  public
    property Kind: TKind read FKind;
    property Visible: Boolean read FVisible write FVisible;
    property Style: TScrollBarStyle read FStyle write FStyle;
  end;

{ TScrollingWinControl }

  TScrollingWinControl = class(TWinControl)
  strict private
    FVertScrollBar: TControlScrollBar;
    FHorzScrollBar: TControlScrollBar;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  public
    property VertScrollBar: TControlScrollBar read FVertScrollBar;
    property HorzScrollBar: TControlScrollBar read FHorzScrollBar;
  end;

{ TCustomForm }

  TCustomForm = class(TScrollingWinControl)
  strict private
    FActive: Boolean;
    FClientHandle: HWND;
    FMDIChildCount: Integer;
    FFormStyle: TFormStyle;
    FBorderStyle: TFormBorderStyle;
    FWindowState: TWindowState;
    FMDIChildren: TList<TForm>;

  strict private
    function get_MDIChildren(I: Integer): TForm;
    procedure set_MDIChildren(I: Integer; const Value: TForm);

  public
    constructor Create(Owner: TComponent); overload; override;
    destructor Destroy; override;

  public
    class function Create(const Handle, ClientHandle: HWND): TCustomForm; overload; inline; static;

  public
    property Active: Boolean read FActive write FActive;
    property ClientHandle: HWND read FClientHandle write FClientHandle;
    property FormStyle: TFormStyle read FFormStyle write FFormStyle;
    property WindowState: TWindowState read FWindowState write FWindowState;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle;
    property MDIChildCount: Integer read FMDIChildCount;
    property MDIChildren[I: Integer]: TForm read get_MDIChildren write set_MDIChildren;
  end;

  TCustomFormClass = class of TCustomForm;

{ TForm }

  TForm = class(TCustomForm);

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

{ TControlScrollBar }

constructor TControlScrollBar.Create(const Kind: TKind);
begin
  FKind    := Kind;
  FStyle   := ssRegular;
  FVisible := True;
end;

{ TScrollingWinControl }

constructor TScrollingWinControl.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FVertScrollBar := TControlScrollBar.Create(TControlScrollBar.TKind.Vertical);
  FHorzScrollBar := TControlScrollBar.Create(TControlScrollBar.TKind.Horizontal);
end;

destructor TScrollingWinControl.Destroy;
begin
  FreeAndNil(FHorzScrollBar);
  FreeAndNil(FVertScrollBar);

  inherited Destroy;
end;

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

class function TCustomForm.Create(const Handle, ClientHandle: HWND): TCustomForm;
begin
  Result               := TCustomForm.Create(nil);
  Result.Handle        := Handle;
  Result.FClientHandle := ClientHandle;
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

initialization
  Application := TApplication.Create(nil);

finalization
  FreeAndNil(Application);

end.
