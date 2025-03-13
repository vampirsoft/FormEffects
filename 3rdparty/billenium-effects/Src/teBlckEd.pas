unit teBlckEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teMskEd, teForm, StdCtrls, ExtCtrls, teCtrls, ComCtrls, Buttons, TransEff,
  teTrEfEd;

{$INCLUDE teDefs.inc}

type
  TBlockTransitionEditor = class(TMaskedTransitionEditor)
    LabelPuzzle: TLabel;
    CheckBoxPuzzle: TCheckBox;
    CheckBoxDual: TCheckBox;
    LabelDual: TLabel;
    ComboBoxBlockWidthSel: TComboBox;
    EditBlockWidth: TEdit;
    UpDownBlockWidth: TUpDown;
    ComboBoxBlockHeightSel: TComboBox;
    EditRows: TEdit;
    UpDownRows: TUpDown;
    EditCols: TEdit;
    UpDownCols: TUpDown;
    EditBlockHeight: TEdit;
    UpDownBlockHeight: TUpDown;
    procedure ComboBoxBlockWidthSelChange(Sender: TObject);
    procedure ComboBoxBlockHeightSelChange(Sender: TObject);
  private
    procedure ShowCols(Show: Boolean);
    procedure ShowRows(Show: Boolean);
  public
    procedure Initialize(TransitionValue: TTransitionEffect); override;
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  BlockTransitionEditor: TBlockTransitionEditor;

implementation

uses teBlock, teEditor;

{$R *.DFM}

{ TBlockTransitionEditor }

procedure TBlockTransitionEditor.Initialize(
  TransitionValue: TTransitionEffect);
var
  BlockTransition: TBlockTransition;
begin
  inherited;

  BlockTransition := Transition as TBlockTransition;

  ShowCols(BlockTransition.Cols > 0);
  ShowRows(BlockTransition.Rows > 0);
end;

procedure TBlockTransitionEditor.ReadValues;
var
  BlockTransition: TBlockTransition;
begin
  inherited;

  BlockTransition := Transition as TBlockTransition;

  CheckBoxPuzzle   .Checked  := BlockTransition.Puzzle;
  CheckBoxDual     .Checked  := BlockTransition.Dual;
  UpDownBlockWidth .Position := BlockTransition.BlockWidth;
  UpDownCols       .Position := BlockTransition.Cols;
  UpDownBlockHeight.Position := BlockTransition.BlockHeight;
  UpDownRows       .Position := BlockTransition.Rows;
end;

procedure TBlockTransitionEditor.WriteValues;
var
  BlockTransition: TBlockTransition;
begin
  inherited;

  BlockTransition := Transition as TBlockTransition;

  BlockTransition.Puzzle := CheckBoxPuzzle.Checked;
  BlockTransition.Dual   := CheckBoxDual  .Checked;
  if ComboBoxBlockWidthSel .ItemIndex = 0
  then BlockTransition.BlockWidth  := UpDownBlockWidth.Position
  else BlockTransition.Cols        := UpDownCols      .Position;
  if ComboBoxBlockHeightSel.ItemIndex = 0
  then BlockTransition.BlockHeight := UpDownBlockHeight.Position
  else BlockTransition.Rows        := UpDownRows        .Position;
end;

procedure TBlockTransitionEditor.ShowCols(Show: Boolean);
begin
  if Show
  then
  begin
    if ComboBoxBlockWidthSel.ItemIndex <> 1 then
      ComboBoxBlockWidthSel.ItemIndex := 1;

    EditCols        .Visible := True;
    UpDownCols      .Visible := True;
    EditBlockWidth  .Visible := False;
    UpDownBlockWidth.Visible := False;
  end
  else
  begin
    if ComboBoxBlockWidthSel.ItemIndex <> 0 then
      ComboBoxBlockWidthSel.ItemIndex := 0;

    EditCols        .Visible := False;
    UpDownCols      .Visible := False;
    EditBlockWidth  .Visible := True;
    UpDownBlockWidth.Visible := True;
  end;
end;

procedure TBlockTransitionEditor.ShowRows(Show: Boolean);
begin
  if Show
  then
  begin
    if ComboBoxBlockHeightSel.ItemIndex <> 1 then
      ComboBoxBlockHeightSel.ItemIndex := 1;

    EditRows         .Visible := True;
    UpDownRows       .Visible := True;
    EditBlockHeight  .Visible := False;
    UpDownBlockHeight.Visible := False;
  end
  else
  begin
    if ComboBoxBlockHeightSel.ItemIndex <> 0 then
      ComboBoxBlockHeightSel.ItemIndex := 0;

    EditRows         .Visible := False;
    UpDownRows       .Visible := False;
    EditBlockHeight  .Visible := True;
    UpDownBlockHeight.Visible := True;
  end;
end;

procedure TBlockTransitionEditor.ComboBoxBlockWidthSelChange(
  Sender: TObject);
begin
  ShowCols(ComboBoxBlockWidthSel.ItemIndex = 1);
  TransitionEditor.AutoPreview;
end;

procedure TBlockTransitionEditor.ComboBoxBlockHeightSelChange(
  Sender: TObject);
begin
  ShowRows(ComboBoxBlockHeightSel.ItemIndex = 1);
  TransitionEditor.AutoPreview;
end;

initialization

  RegisterClasses([TBlockTransitionEditor]);

end.
