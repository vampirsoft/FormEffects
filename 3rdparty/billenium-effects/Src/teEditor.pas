unit teEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  TransEff, FormCont, teTrEfEd, Buttons, ExtCtrls, teForm, Dialogs, teImage;

{$INCLUDE teDefs.inc}

type
  TTransitionEditor = class(TForm)
    FormContainer: TFormContainer;
    LabelTransitions: TLabel;
    ComboBoxTransitions: TComboBox;
    BitBtnPreview: TButton;
    Bevel: TBevel;
    FormTransitions: TFormTransitions;
    ImageB: TImage;
    ImageA: TImage;
    TEImagePreview: TTEImage;
    CheckBoxAutoPreview: TCheckBox;
    TimerPreview: TTimer;
    LabelAutoPreview: TLabel;
    procedure ComboBoxTransitionsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtnPreviewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxAutoPreviewClick(Sender: TObject);
    procedure TimerPreviewTimer(Sender: TObject);
    procedure FormTransitionsAfterShow(Sender: TObject;
      const FirstTime: Boolean);
  private
    CurrentItemIndex: Integer;
    TempTransition: TTransitionEffect;
  protected
    AllowTransitionChange,
    AllowAutoPreview: Boolean;
    EditorForm: TTransitionEffectEditor;
    OriginalTransition: PTransitionEffect;
    CurrentTransition: TTransitionEffect;
    AVisible: Boolean;

    procedure ShowEditor(Transition: TTransitionEffect; Assigned: Boolean);
    procedure Initialize(Transition: PTransitionEffect);
    procedure Preview;
  public
    ShowPasses: Boolean;

    procedure InitializeForChange(var Transition: TTransitionEffect);
    procedure InitializeForEdition(Transition: TTransitionEffect);
    procedure Apply;
    procedure AutoPreview;
  end;

  {$ifndef TE_NOHLP}
  TTEEditorInit        = procedure(Control: TWinControl; FirstStep: Boolean) of object;
  TTEVerifyDescription = function(Description: String): String;
  {$endif TE_NOHLP}

  function ChangeTransition(var Transition: TTransitionEffect): Boolean;
  function EditTransition(Transition: TTransitionEffect): Boolean;

var
  TransitionEditor: TTransitionEditor;
  {$ifndef TE_NOHLP}
  teEditorInit: TTEEditorInit = nil;
  teVerifyDescription: TTEVerifyDescription = nil;
  teParentWnd: HWnd = 0;
  {$endif TE_NOHLP}

implementation

{$R *.DFM}

uses teModEdit, teRender;

type
  TTransitionEffectHack = class(TTransitionEffect);

resourcestring
  rsTENoEditor = 'Editor not found for transition ''%s''';

function ChangeTransition(var Transition: TTransitionEffect): Boolean;
begin
  if TEParentWnd = 0
  then TransitionModalEditor := TTransitionModalEditor.Create(Application)
  else TransitionModalEditor := TTransitionModalEditor.CreateParented(TEParentWnd);
  try
    TransitionModalEditor.InitializeForChange(Transition);
    Result := TransitionModalEditor.ShowModal = mrOk;
    if Result then
      TransitionModalEditor.Apply;
  finally
    TransitionModalEditor.Free;
  end;
end;

function EditTransition(Transition: TTransitionEffect): Boolean;
begin
  if TEParentWnd = 0
  then TransitionModalEditor := TTransitionModalEditor.Create(Application)
  else TransitionModalEditor := TTransitionModalEditor.CreateParented(TEParentWnd);
  try
    TransitionModalEditor.InitializeForEdition(Transition);
    Result := TransitionModalEditor.ShowModal = mrOk;
    if Result then
      TransitionModalEditor.Apply;
  finally
    TransitionModalEditor.Free;
  end;
end;

procedure TTransitionEditor.FormDestroy(Sender: TObject);
begin
  CurrentTransition.Free;
end;

procedure TTransitionEditor.ShowEditor(Transition: TTransitionEffect;
  Assigned: Boolean);
var
  EditorClass: TCustomFormClass;
  {$ifdef D6UP}
  OldGroup: TPersistentClass;
  {$endif D6UP}
begin
  {$ifdef D6UP}
  OldGroup := ActivateClassGroup(TControl);
  try
  {$endif D6UP}
    EditorClass := TCustomFormClass(GetClass(Transition.GetEditor));
    if EditorClass = nil then
      raise ETransitionEffectError.CreateFmt(rsTENoEditor, [Transition.Description]);
  {$ifdef D6UP}
  finally
    ActivateClassGroup(OldGroup);
  end;
  {$endif D6UP}

  EditorForm := TTransitionEffectEditor(FormContainer.CreateForm(EditorClass));
  TransitionEffectEditor := EditorForm;
  EditorForm.ShowPasses  := ShowPasses;
  EditorForm.Left        := 0;
  EditorForm.Top         := 0;
  EditorForm.Color       := Color;
  if Assigned then
    EditorForm.CheckAssignment(Transition);
  EditorForm.Initialize(Transition);
  EditorForm.ReadValues;

  FormContainer.ShowFormEx(EditorForm, True, nil, nil, fcfaNone);
end;

procedure FixAntialias(Font: TFont);
var
  SaveFont: HGDIOBJ;
  DC: HDC;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TTransitionEditor.Initialize(Transition: PTransitionEffect);
var
  i: Integer;
  TransitionEffectClass: TTransitionEffectClass;
  {$ifdef D6UP}
  OldGroup: TPersistentClass;
  {$endif D6UP}
  R: TRect;
  Bmp: TBitmap;
  Description: String;
begin
  if Assigned(teEditorInit) then
    teEditorInit(GetParentForm(Self), True);

  if(not Assigned(ImageA .Picture.Graphic)) or
    (not Assigned(ImageB .Picture.Graphic)) then
  begin
    Bmp := TBitmap.Create;
    try
      AdjustBmpForTransition(Bmp, 0, TEImagePreview.Width,
        TEImagePreview.Height, pfDevice);
      Bmp.Canvas.Font.Name   := 'Times New Roman';
      Bmp.Canvas.Font.Size   := 110;
      Bmp.Canvas.Font.Style  := [fsBold];
      FixAntialias(Bmp.Canvas.Font);
      R := TEImagePreview.ClientRect;

      if not Assigned(ImageA.Picture.Graphic) then
      begin
        Bmp.Canvas.Brush.Color := clYellow;
        Bmp.Canvas.Brush.Style := bsSolid;
        Bmp.Canvas.FillRect(R);
        DrawText(Bmp.Canvas.Handle, 'A', 1, R,
          DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        ImageA.Picture.Assign(Bmp);
      end;

      if not Assigned(ImageB.Picture.Graphic) then
      begin
        Bmp.Canvas.Brush.Color := clRed;
        Bmp.Canvas.FillRect(R);
        DrawText(Bmp.Canvas.Handle, 'B', 1, R,
          DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        ImageB.Picture.Assign(Bmp);
      end;
    finally
      Bmp.Free;
    end;
  end;

  TEImagePreview.Picture.Assign(ImageA.Picture);
  AVisible := True;

  if TERegisteredTransitions.IndexOf(Transition^.ClassType) = -1
  then ComboBoxTransitions.Enabled := False
  else ComboBoxTransitions.Enabled := AllowTransitionChange;

  if ComboBoxTransitions.Enabled
  then
  begin
    {$ifdef D6UP}
    OldGroup := ActivateClassGroup(TControl);
    try
    {$endif D6UP}
      for i:=0 to TERegisteredTransitions.Count-1 do
        if GetClass(TTransitionEffectClass(TERegisteredTransitions[i]).GetEditor) <> nil then
        begin
          Description :=
            TTransitionEffectClass(TERegisteredTransitions[i]).Description;
          if Assigned(teVerifyDescription) then
            Description := teVerifyDescription(Description);
          if Description <> '' then
            ComboBoxTransitions.Items.AddObject(Description,
              TERegisteredTransitions[i]);
        end;
      ComboBoxTransitions.Sorted := True;
    {$ifdef D6UP}
    finally
      ActivateClassGroup(OldGroup);
    end;
    {$endif D6UP}
  end
  else
  begin
    Description := Transition^.Description;
    if Assigned(teVerifyDescription) then
      Description := teVerifyDescription(Description);
    ComboBoxTransitions.Items.AddObject(Description,
      TObject(Transition^.ClassType));
  end;

  CurrentTransition  := nil;
  OriginalTransition := Transition;

  for i:=0 to ComboBoxTransitions.Items.Count-1 do
  begin
    if TTransitionEffectClass(ComboBoxTransitions.Items.Objects[i]).ClassName = Transition.ClassName then
    begin
      CurrentItemIndex              := i;
      ComboBoxTransitions.ItemIndex := CurrentItemIndex;
      TransitionEffectClass         :=
        TTransitionEffectClass(ComboBoxTransitions.Items.Objects[i]);
      CurrentTransition             := TransitionEffectClass.Create(nil);
      CurrentTransition.Assign(Transition^);
      break;
    end;
  end;

  if CurrentTransition = nil then
    raise ETransitionEffectError.CreateFmt(rsTENoEditor, [Transition.Description]);

  ShowEditor(CurrentTransition, False);

  if Assigned(teEditorInit) then
    teEditorInit(GetParentForm(Self), False);
end;

procedure TTransitionEditor.InitializeForChange(
  var Transition: TTransitionEffect);
begin
  AllowTransitionChange := True;
  Initialize(@Transition);
end;

procedure TTransitionEditor.InitializeForEdition(Transition: TTransitionEffect);
begin
  TempTransition := Transition;
  AllowTransitionChange := False;
  Initialize(@TempTransition);
end;

procedure TTransitionEditor.Apply;
var
  OriginalBak: TTransitionEffect;
  TransitionEffectClass: TTransitionEffectClass;
  Name: TComponentName;
  TheOwner: TComponent;
  Index: Integer;
begin
  if OriginalTransition^.ClassType = CurrentTransition.ClassType
  then
  begin
    EditorForm.WriteValues;
    OriginalTransition^.Assign(CurrentTransition);
  end
  else
  begin
    Name     := OriginalTransition^.Name;
    TheOwner := OriginalTransition^.Owner;
    Index    := OriginalTransition^.Index;

    EditorForm.WriteValues;
    TransitionEffectClass := TTransitionEffectClass(CurrentTransition.ClassType);
    OriginalBak           := OriginalTransition^;
    OriginalTransition^   := TransitionEffectClass.Create(TheOwner);
    OriginalTransition^.TransitionList     := OriginalBak.TransitionList;
    OriginalTransition^.OnStartTransition  := OriginalBak.OnStartTransition;
    OriginalTransition^.OnEndTransition    := OriginalBak.OnEndTransition;
    OriginalTransition^.OnBeforeTransition := OriginalBak.OnBeforeTransition;
    OriginalTransition^.OnAfterTransition  := OriginalBak.OnAfterTransition;
    OriginalTransition^.OnAbortQuery       := OriginalBak.OnAbortQuery;
    OriginalTransition^.Assign(CurrentTransition);
    OriginalTransition^.Index := Index;

    // Destroy transition but maintain links to new one
    try
      OldTransition    := OriginalBak;
      NewTransition    := OriginalTransition^;
      OriginalBak.Free;
    finally
      OldTransition    := nil;
      NewTransition    := nil;
    end;

    OldTransition := nil;
    NewTransition := nil;
    if Name <> '' then
      OriginalTransition^.Name := Name;
  end;
end;

procedure TTransitionEditor.ComboBoxTransitionsChange(Sender: TObject);
var
  TransitionEffectClass: TTransitionEffectClass;
  aux: TTransitionEffect;
  EditorQuestion: String;
  AllowAutoPreviewBak,
  Assigned: Boolean;
begin
  if ComboBoxTransitions.ItemIndex = CurrentItemIndex then
    exit;

  AllowAutoPreviewBak := AllowAutoPreview;
  AllowAutoPreview := False;

  TEImagePreview.AbortTransition;

  EditorQuestion := TTransitionEffectHack(CurrentTransition).EditorQuestion;
  if(TTransitionEffectHack(CurrentTransition).EditorQuestion <> '') and
    (MessageDlg(EditorQuestion, mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
  begin
    ComboBoxTransitions.ItemIndex := CurrentItemIndex;
    exit;
  end;

  EditorForm.WriteValues;
  TransitionEffectClass := TTransitionEffectClass(
    ComboBoxTransitions.Items.Objects[ComboBoxTransitions.ItemIndex]);

  aux := TransitionEffectClass.Create(nil);
  if CurrentTransition <> nil then
  begin
    aux.Assign(CurrentTransition);
    Assigned := True
  end
  else Assigned := False;

  CurrentTransition.Free;
  CurrentTransition := aux;
  CurrentItemIndex := ComboBoxTransitions.ItemIndex;

  ShowEditor(CurrentTransition, Assigned);

  AllowAutoPreview := AllowAutoPreviewBak;
  AutoPreview;
end;

procedure TTransitionEditor.Preview;
begin
  EditorForm.WriteValues;
  TEImagePreview.PrepareTransition(CurrentTransition, True, False);
  AVisible := not AVisible;
  if AVisible
  then TEImagePreview.Picture.Assign(ImageA.Picture)
  else TEImagePreview.Picture.Assign(ImageB.Picture);
  TEImagePreview.ExecuteTransition(False);
end;

procedure TTransitionEditor.BitBtnPreviewClick(Sender: TObject);
begin
  Preview;
end;

procedure TTransitionEditor.AutoPreview;
begin
  TimerPreview.Enabled := False;
  TEImagePreview.AbortTransition;
  if AllowAutoPreview and CheckBoxAutoPreview.Checked then
    TimerPreview.Enabled := True;
end;

procedure TTransitionEditor.FormCreate(Sender: TObject);
begin                  
  AllowAutoPreview := False;
  ShowPasses       := True;
end;

procedure TTransitionEditor.CheckBoxAutoPreviewClick(Sender: TObject);
begin
  AutoPreview;
end;

procedure TTransitionEditor.TimerPreviewTimer(Sender: TObject);
begin
  TimerPreview.Enabled := False;
  Preview;
end;

procedure TTransitionEditor.FormTransitionsAfterShow(Sender: TObject;
  const FirstTime: Boolean);
begin
  AllowAutoPreview := True;
end;

end.
