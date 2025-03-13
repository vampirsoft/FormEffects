unit teTrLEdi;

interface

{$INCLUDE teDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, ComCtrls, TransEff, teEditor, teTrEfEd;

type
  TTransitionListEditorForm = class(TForm)
    PanelButtons: TPanel;
    SpeedButtonAdd: TSpeedButton;
    SpeedButtonDelete: TSpeedButton;
    ListViewTransitions: TListView;
    SpeedButtonEdit: TSpeedButton;
    SpeedButtonUp: TSpeedButton;
    SpeedButtonDown: TSpeedButton;
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonDeleteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure SpeedButtonEditClick(Sender: TObject);
    procedure ListViewTransitionsDblClick(Sender: TObject);
    procedure SpeedButtonUpClick(Sender: TObject);
    procedure SpeedButtonDownClick(Sender: TObject);
    procedure ListViewTransitionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewTransitionsSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
  private
    procedure RefreshItemData(const Index: Integer);
  protected
    procedure CMTENameChanged(var Msg: TCMTENameChanged); message CM_TENAMECHANGED;
    procedure MoveItem(const CurrentIndex, NewIndex: Integer);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function EditTransition(var Transition: TTransitionEffect): Boolean; virtual;
  public
    FormDesigner: TTEFormDesignerBase;
    TransitionList: TTransitionList;
    ShowName: Boolean;

    destructor Destroy; override;
    procedure CheckState;
    procedure Initialize(ATransitionList: TTransitionList;
      AFormDesigner: TTEFormDesignerBase); virtual;
    procedure SelectTransition(Transition: TTransitionEffect);
  end;

var
  TransitionListEditorForm: TTransitionListEditorForm;

implementation

{$R *.DFM}

type
  TTransitionListHack   = class(TTransitionList);
  TTransitionEffectHack = class(TTransitionEffect);

destructor TTransitionListEditorForm.Destroy;
begin
  FormDesigner := nil;

  inherited;
end;

{ TTransitionListEditorForm }

procedure TTransitionListEditorForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if(Operation = opRemove)            and
    (AComponent is TTransitionList)   then
  begin
    TransitionList := nil;
    Close;
  end;
end;

procedure TTransitionListEditorForm.Initialize(ATransitionList:
  TTransitionList; AFormDesigner: TTEFormDesignerBase);
var
  i: Integer;
begin
  TransitionList        := ATransitionList;
  TransitionList.Editor := Self;
  FormDesigner          := AFormDesigner;

  Caption := {TransitionList.Owner.Name + '.' + }TransitionList.Name;
  ShowName := Assigned(FormDesigner);
  if not ShowName then
    ListViewTransitions.Columns.Delete(2);

  ListViewTransitions.Items.BeginUpdate;
  try
    for i:=0 to TransitionList.TransitionCount-1 do
    begin
      ListViewTransitions.Items.Add;
      RefreshItemData(i);
    end;
  finally
    ListViewTransitions.Items.EndUpdate;
  end;
  if TransitionList.TransitionCount > 0 then
    SelectTransition(TransitionList[0]);
end;

procedure TTransitionListEditorForm.SpeedButtonAddClick(Sender: TObject);
var
  NewTransition: TTransitionEffect;
  TransitionName: TComponentName;
begin
  if Assigned(FormDesigner) then
  begin
    TransitionName := FormDesigner.UniqueName('transition');
    TransitionName[1] := 'T';
  end;
  NewTransition := TFlickerFreeTransition.Create(TransitionList.Owner);
  NewTransition.Milliseconds := 1000;
  NewTransition.TransitionList := TransitionList;
  if Assigned(FormDesigner) then
    FormDesigner.SelectComponent(TransitionList);

  try
    try
      ListViewTransitions.Items.Add;
      if Assigned(FormDesigner) then
        NewTransition.Name := TransitionName;
      SelectTransition(NewTransition);

      if EditTransition(NewTransition)
      then NewTransition := nil
      else
      begin
        ListViewTransitions.Items.Delete(ListViewTransitions.Selected.Index);
        FreeAndNil(NewTransition);
      end;
    except
      on Exception do
      begin
        if Assigned(NewTransition) then
          ListViewTransitions.Items.Delete(ListViewTransitions.Selected.Index);
        raise;
      end;
    end;
  finally
    FreeAndNil(NewTransition);
    CheckState;
  end;
end;

function TTransitionListEditorForm.EditTransition(
  var Transition: TTransitionEffect): Boolean;
begin
  try
    Result := ChangeTransition(Transition);
    if Result then
    begin
      RefreshItemData(ListViewTransitions.Selected.Index);
      if Assigned(FormDesigner) then
      begin
        FormDesigner.Modified;
        FormDesigner.MakeSubComponentsLinkable(Transition);
      end;                                    
    end;
  finally
    FormActivate(Self);
  end;
end;

procedure TTransitionListEditorForm.SpeedButtonDeleteClick(
  Sender: TObject);
var
  i,
  Index: Integer;
begin
  if SpeedButtonDelete.Enabled                                        and
    (MessageDlg('Delete?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ListViewTransitions.Items.BeginUpdate;
    try
      Index := ListViewTransitions.Selected.Index;
      TransitionList[ListViewTransitions.Selected.Index].Free;
      ListViewTransitions.Items.Delete(ListViewTransitions.Selected.Index);
      if ListViewTransitions.Items.Count > 0 then
      begin
        for i := Index to TransitionList.TransitionCount - 1 do
          RefreshItemData(i);
        if TransitionList.TransitionCount <= Index then
          Dec(Index);
        SelectTransition(TransitionList[Index]);
      end;
    finally
      ListViewTransitions.Items.EndUpdate;
    end;
    CheckState;
  end;
end;

procedure TTransitionListEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  if TransitionList <> nil then
  begin
    TransitionList.Editor := nil;
    if Assigned(FormDesigner) then
      FormDesigner.SelectComponent(TransitionList);
  end;
end;

procedure TTransitionListEditorForm.FormActivate(Sender: TObject);
begin
  CheckState;
end;

procedure TTransitionListEditorForm.SpeedButtonEditClick(Sender: TObject);
var
  Transition: TTransitionEffect;
begin
  if ListViewTransitions.Selected <> nil then
  begin
    Transition :=
      TTransitionListHack(TransitionList).FTransitions[
        ListViewTransitions.Selected.Index];
    if EditTransition(Transition) then
      TTransitionListHack(TransitionList).FTransitions[
        ListViewTransitions.Selected.Index] := Transition;
  end;
end;

procedure TTransitionListEditorForm.ListViewTransitionsDblClick(
  Sender: TObject);
begin
  SpeedButtonEdit.Click;
end;

procedure TTransitionListEditorForm.CMTENameChanged(var Msg: TCMTENameChanged);
var
  Index: Integer;
begin
  Index := TransitionList.GetTransitionIndex(Msg.Transition);
  if Index <> -1 then
    RefreshItemData(Index);
end;

procedure TTransitionListEditorForm.SpeedButtonUpClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListViewTransitions.Selected.Index;
  MoveItem(Index, Index - 1);
end;

procedure TTransitionListEditorForm.SpeedButtonDownClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListViewTransitions.Selected.Index;
  MoveItem(Index, Index + 1);
end;

procedure TTransitionListEditorForm.ListViewTransitionsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_INSERT: SpeedButtonAdd   .Click;
    VK_DELETE: SpeedButtonDelete.Click;
    VK_RETURN: SpeedButtonEdit  .Click;
    VK_UP    : if ssCtrl in Shift then SpeedButtonUp  .Click;
    VK_DOWN  : if ssCtrl in Shift then SpeedButtonDown.Click;
  end;
end;

procedure TTransitionListEditorForm.CheckState;
begin
  SpeedButtonDelete.Enabled := ListViewTransitions.Selected <> nil;
  SpeedButtonEdit  .Enabled := ListViewTransitions.Selected <> nil;
  SpeedButtonUp    .Enabled :=
    (ListViewTransitions.Selected      <> nil) and
    (ListViewTransitions.Items.Count    > 0  ) and
    (ListViewTransitions.Selected.Index > 0  );
  SpeedButtonDown  .Enabled :=
    (ListViewTransitions.Selected      <> nil) and
    (ListViewTransitions.Items.Count    > 0  ) and
    (ListViewTransitions.Selected.Index < ListViewTransitions.Items.Count-1);

  if Assigned(FormDesigner) then
  begin
    if Assigned(ListViewTransitions.Selected)
    then FormDesigner.SelectComponent(TComponent(TransitionList[ListViewTransitions.Selected.Index]))
    else FormDesigner.SelectComponent(TransitionList);
  end;
end;

procedure TTransitionListEditorForm.ListViewTransitionsSelectItem(
  Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  CheckState;
end;

procedure TTransitionListEditorForm.MoveItem(const CurrentIndex,
  NewIndex: Integer);
begin
  TransitionList[CurrentIndex].Index := NewIndex;
  RefreshItemData(CurrentIndex);
  RefreshItemData(NewIndex);
  SelectTransition(TransitionList[NewIndex]);
end;

procedure TTransitionListEditorForm.RefreshItemData(const Index: Integer);
var
  ListItem: TListItem;
begin
  ListItem          := ListViewTransitions.Items[Index];
  ListItem.Caption  := IntToStr(Index);
  if ListItem.SubItems.Count = 0
  then
  begin
    ListItem.SubItems.Add(TransitionList[Index].Description);
    if ShowName then
      ListItem.SubItems.Add(TransitionList[Index].Name);
  end
  else
  begin
    ListItem.SubItems[0] := TransitionList[Index].Description;
    if ShowName then
      ListItem.SubItems[1] := TransitionList[Index].Name;
  end;
end;

procedure TTransitionListEditorForm.SelectTransition(
  Transition: TTransitionEffect);
begin
  if Transition = nil
  then ListViewTransitions.Selected := nil
  else
  begin
    ListViewTransitions.Selected :=
      ListViewTransitions.Items[TransitionList.GetTransitionIndex(Transition)];
    ListViewTransitions.Selected.Focused := True;
    ListViewTransitions.Selected.MakeVisible(False);
    ListViewTransitions.Update;
  end;
end;

end.
