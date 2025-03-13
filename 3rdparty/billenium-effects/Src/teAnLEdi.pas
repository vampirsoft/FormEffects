unit teAnLEdi;

interface

{$INCLUDE teDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, ComCtrls, teAnim, EditIntf, ToolsAPI,
  {$ifdef D6UP}
  DesignIntf, DesignEditors;
  {$else}
  {$ifdef C6}
  Designide;
  {$else}
  Dsgnintf;
  {$endif C6}
  {$endif D6UP}

type

  TTEAnimationListEditorForm = class(TForm)
    PanelButtons: TPanel;
    SpeedButtonAdd: TSpeedButton;
    SpeedButtonDelete: TSpeedButton;
    ListViewAnimations: TListView;
    SpeedButtonEdit: TSpeedButton;
    SpeedButtonUp: TSpeedButton;
    SpeedButtonDown: TSpeedButton;
    SpeedButtonPreview: TSpeedButton;
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonDeleteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure ListViewAnimationsDblClick(Sender: TObject);
    procedure SpeedButtonUpClick(Sender: TObject);
    procedure SpeedButtonDownClick(Sender: TObject);
    procedure ListViewAnimationsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButtonPreviewClick(Sender: TObject);
    procedure ListViewAnimationsSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
  private
    TheDesigner:{$ifdef D6UP}IDesigner{$else}IFormDesigner{$endif D6UP};
    procedure RefreshItemData(const Index: Integer);
  protected
    procedure CMTENameChanged(var Msg: TMessage); message CM_TENAMECHANGED;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure MoveItem(const CurrentIndex, NewIndex: Integer);
  public
    AnimationList: TTEAnimationList;

    procedure CheckState;
    procedure Initialize(AnimationListValue: TTEAnimationList;
      DesignerValue: {$ifdef D6UP}IDesigner{$else}IFormDesigner{$endif D6UP});
    procedure SelectAnimation(Animation: TTEAnimationEffect);
  end;

var
  teAnimationListEditorForm: TTEAnimationListEditorForm;

implementation

{$R *.DFM}

{$ifdef D7UP}
{$WARN UNIT_DEPRECATED OFF}
{$endif D7UP}
uses
  teModEdit, Exptintf, tezFrAn, teAbout;
{$ifdef D7UP}
{$WARN UNIT_DEPRECATED ON}
{$endif D7UP}

{ TTEEditorForm }

procedure TTEAnimationListEditorForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if(Operation = opRemove)            and
    (AComponent is TTEAnimationList)  then
  begin
    AnimationList := nil;
    Close;
  end;
end;

procedure TTEAnimationListEditorForm.Initialize(
  AnimationListValue: TTEAnimationList;
  DesignerValue: {$ifdef D6UP}IDesigner{$else}IFormDesigner{$endif D6UP});
var
  i: Integer;
begin
  AnimationList        := AnimationListValue;
  AnimationList.Editor := Self;
  TheDesigner          := DesignerValue;

  Caption := {AnimationList.Owner.Name + '.' + }AnimationList.Name;

  ListViewAnimations.Items.BeginUpdate;
  try
    for i:=0 to AnimationList.AnimationCount-1 do
    begin
      ListViewAnimations.Items.Add;
      RefreshItemData(i);
    end;
  finally
    ListViewAnimations.Items.EndUpdate;
  end;
  if AnimationList.AnimationCount > 0 then
    SelectAnimation(AnimationList[0]);
end;

procedure TTEAnimationListEditorForm.SpeedButtonAddClick(Sender: TObject);
var
  NewAnimation: TTEAnimationEffect;
  AnimationName: TComponentName;
begin
  AnimationName := TheDesigner.UniqueName('Animation');
  AnimationName[1] := 'A';
  NewAnimation := TTEZoomFrameAnimation.Create(AnimationList.Owner);
  NewAnimation.AnimationList := AnimationList;
  TheDesigner.SelectComponent(AnimationList);
  ListViewAnimations.Items.Add;
  NewAnimation.Name := AnimationName;
  SelectAnimation(NewAnimation);
  CheckState;
end;

procedure TTEAnimationListEditorForm.SpeedButtonDeleteClick(
  Sender: TObject);
var
  i,
  Index: Integer;
begin
  if SpeedButtonDelete.Enabled                                        and
    (MessageDlg('Delete?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ListViewAnimations.Items.BeginUpdate;
    try
      Index := ListViewAnimations.Selected.Index;
      AnimationList[ListViewAnimations.Selected.Index].Free;
      ListViewAnimations.Items.Delete(ListViewAnimations.Selected.Index);
      if ListViewAnimations.Items.Count > 0 then
      begin
        for i := Index to AnimationList.AnimationCount - 1 do
          RefreshItemData(i);
        if AnimationList.AnimationCount <= Index then
          Dec(Index);
        SelectAnimation(AnimationList[Index]);
      end;
    finally
      ListViewAnimations.Items.EndUpdate;
    end;
    CheckState;
  end;
end;

procedure TTEAnimationListEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  if AnimationList <> nil then
  begin
    AnimationList.Editor := nil;
    TheDesigner.SelectComponent(AnimationList);
  end;
end;

procedure TTEAnimationListEditorForm.FormActivate(Sender: TObject);
begin
  CheckState;
end;

procedure TTEAnimationListEditorForm.ListViewAnimationsDblClick(
  Sender: TObject);
begin
  SpeedButtonPreview.Click;
end;

type
  TCMTENameChanged = packed record
    Msg: Cardinal;
    Animation: TTEAnimationEffect;
    Unused,
    Result: Longint;
  end;

procedure TTEAnimationListEditorForm.CMTENameChanged(var Msg: TMessage);
var
  Index: Integer;
begin
  Index := AnimationList.GetAnimationIndex(TCMTENameChanged(Msg).Animation);
  if Index <> -1 then
    RefreshItemData(Index);
end;

procedure TTEAnimationListEditorForm.SpeedButtonUpClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListViewAnimations.Selected.Index;
  MoveItem(Index, Index - 1);
end;

procedure TTEAnimationListEditorForm.SpeedButtonDownClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListViewAnimations.Selected.Index;
  MoveItem(Index, Index + 1);
end;

procedure TTEAnimationListEditorForm.ListViewAnimationsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_INSERT: SpeedButtonAdd    .Click;
    VK_DELETE: SpeedButtonDelete .Click;
    VK_RETURN: SpeedButtonPreview.Click;
    VK_UP    : if ssCtrl in Shift then SpeedButtonUp  .Click;
    VK_DOWN  : if ssCtrl in Shift then SpeedButtonDown.Click;
  end;
end;

procedure TTEAnimationListEditorForm.CheckState;
begin
  SpeedButtonDelete .Enabled := ListViewAnimations.Selected <> nil;
  SpeedButtonPreview.Enabled := ListViewAnimations.Selected <> nil;
  SpeedButtonUp    .Enabled :=
    (ListViewAnimations.Selected      <> nil) and
    (ListViewAnimations.Items.Count    > 0  ) and
    (ListViewAnimations.Selected.Index > 0  );
  SpeedButtonDown  .Enabled :=
    (ListViewAnimations.Selected      <> nil) and
    (ListViewAnimations.Items.Count    > 0  ) and
    (ListViewAnimations.Selected.Index < ListViewAnimations.Items.Count-1);

  if ListViewAnimations.Selected <> nil
  then TheDesigner.SelectComponent(TComponent(AnimationList[ListViewAnimations.Selected.Index]))
  else TheDesigner.SelectComponent(AnimationList);
end;

procedure TTEAnimationListEditorForm.ListViewAnimationsSelectItem(
  Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  CheckState;
end;

procedure TTEAnimationListEditorForm.MoveItem(const CurrentIndex, NewIndex:
    Integer);
begin
  AnimationList[CurrentIndex].Index := NewIndex;
  RefreshItemData(CurrentIndex);
  RefreshItemData(NewIndex);
  SelectAnimation(AnimationList[NewIndex]);
end;

procedure TTEAnimationListEditorForm.RefreshItemData(const Index: Integer);
var
  ListItem: TListItem;
begin
  ListItem          := ListViewAnimations.Items[Index];
  ListItem.Caption  := IntToStr(Index);
  if ListItem.SubItems.Count = 0
  then
  begin
    ListItem.SubItems.Add(AnimationList[Index].Description);
    ListItem.SubItems.Add(AnimationList[Index].Name);
  end
  else
  begin
    ListItem.SubItems[0] := AnimationList[Index].Description;
    ListItem.SubItems[1] := AnimationList[Index].Name;
  end;
end;

procedure TTEAnimationListEditorForm.SelectAnimation(Animation:
    TTEAnimationEffect);
begin
  if Animation = nil
  then ListViewAnimations.Selected := nil
  else
  begin
    ListViewAnimations.Selected :=
      ListViewAnimations.Items[AnimationList.GetAnimationIndex(Animation)];
    ListViewAnimations.Selected.Focused := True;
    ListViewAnimations.Selected.MakeVisible(False);
    ListViewAnimations.Update;
  end;
end;

procedure TTEAnimationListEditorForm.SpeedButtonPreviewClick(
  Sender: TObject);
var
  AboutForm: TTEAboutForm;
begin
  if ListViewAnimations.Selected <> nil then
  begin
    with AnimationList[ListViewAnimations.Selected.Index] as TTEZoomFrameAnimation do
    begin
      AboutForm := TTEAboutForm.Create(Application);
      try
        AboutForm.DisableTransitions;
        ShowModalFormEx(AboutForm, SpeedButtonPreview.ClientRect, SpeedButtonPreview, True);
      finally
        AboutForm.Free;
      end;
    end;
  end;
end;

end.
