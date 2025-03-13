unit teRndLis;

interface

{$INCLUDE teDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  teTrLEdi, ComCtrls, Buttons, ExtCtrls, TransEff, teEditor, teModEdit,
  teRandom, teTrEfEd;

type
  TRndTrListEditor = class(TTransitionListEditorForm)
    procedure SpeedButtonDeleteClick(Sender: TObject);
  private
    OldTransitionEditor: TTransitionEditor;
    OldTransitionModalEditor: TTransitionModalEditor;
    OldteEditorInit: TTEEditorInit;

    procedure EditorInit(Control: TWinControl; FirstCall: Boolean);
  protected
    function EditTransition(var Transition: TTransitionEffect): Boolean; override;
  public
    RandomTransition: TRandomTransition;

    procedure Initialize(ATransitionList: TTransitionList;
      AFormDesigner: TTEFormDesignerBase); override;
  end;

var
  RndTrListEditor: TRndTrListEditor;

implementation

{$R *.DFM}

uses teRender;

procedure TRndTrListEditor.EditorInit(Control: TWinControl; FirstCall: Boolean);
var
  ChildModalEditor: TTransitionModalEditor;
  P: TPoint;
begin
  if not FirstCall then
  begin
    if Assigned(Control) then
    begin
      if(Control is TTransitionModalEditor) and
         Assigned(OldTransitionModalEditor) then
      begin
        ChildModalEditor := Control as TTransitionModalEditor;
        ChildModalEditor.Position := poDesigned;
        P := ControlClientOrigin(OldTransitionModalEditor);
        ChildModalEditor.Left     := P.x;
        ChildModalEditor.Top      := P.y;
      end;
      if Assigned(OldTransitionEditor) and Assigned(TransitionEditor) then
      begin
        TransitionEditor.ImageA .Picture.Assign(OldTransitionEditor.ImageA .Picture);
        TransitionEditor.ImageB .Picture.Assign(OldTransitionEditor.ImageB .Picture);
      end;
    end;
  end;
  if Assigned(OldteEditorInit) then
    OldteEditorInit(Control, FirstCall);
end;

function TRndTrListEditor.EditTransition(
  var Transition: TTransitionEffect): Boolean;
begin
  OldTransitionModalEditor := TransitionModalEditor;
  OldTransitionEditor      := TransitionEditor;
  OldteEditorInit          := teEditorInit;
  teEditorInit             := EditorInit;
  try
    Result := inherited EditTransition(Transition);
  finally
    teEditorInit             := OldteEditorInit;
    TransitionEditor         := OldTransitionEditor;
    TransitionModalEditor    := OldTransitionModalEditor;
    OldTransitionModalEditor := nil;
    OldTransitionEditor      := nil;
    OldteEditorInit          := nil;
  end;
end;

procedure TRndTrListEditor.Initialize(ATransitionList: TTransitionList;
  AFormDesigner: TTEFormDesignerBase);
begin
  inherited;

  ListViewTransitions.Columns[1].Width := 163;

  OldTransitionEditor      := nil;
  OldTransitionModalEditor := nil;
  OldteEditorInit          := nil;
end;

procedure TRndTrListEditor.SpeedButtonDeleteClick(Sender: TObject);
begin
  inherited;

  if RandomTransition.TransitionCount > 0
  then RandomTransition.SelectedIndex := ListViewTransitions.Selected.Index
  else RandomTransition.SelectedIndex := -1;
end;

end.
