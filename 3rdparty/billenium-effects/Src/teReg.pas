unit teReg;

interface

{$INCLUDE teDefs.inc}

uses Consts, Classes, Windows, Controls, Graphics, TransEff, FormCont,
  teCtrls, teForm, teTrLEdi, teAllTr, teAnim, teAnLEdi, Forms, teImage,
  teAbout, teEditor, teTrExpo, ToolsAPI,
  {$ifdef D6UP}
  DesignIntf, DesignEditors;
  {$else}
  Dsgnintf;
  {$endif D6UP}

type
  TTransitionListEditor = class(TComponentEditor)
  private
    procedure TransitionEditorInit(Control: TWinControl; FirstCall: Boolean);
    procedure BitBtnAboutClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
  end;

  TTEAnimationListEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
  end;

  TFormContainerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
  end;

  procedure Register;
  procedure GotoWeb(From: String; Order: Boolean);
  procedure ShowAbout;

implementation

uses teModEdit, teTrEfEd, teRender, teXperts, ShellApi, SysUtils;

type
  TTransitionEffectHack = class(TTransitionEffect);

  TProjectNotifier = class(TNotifierObject, IOTAIDENotifier)
  public
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject;
      var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: String; var Cancel: Boolean);
  end;

  TTEFormDesigner = class(TTEFormDesignerBase)
  private
    SubComponents: TList;

    procedure MakeSubComponentLinkable(ComponentClass: TComponentClass);
  public
    TheDesigner:{$ifdef D6UP}IDesigner{$else}IFormDesigner{$endif D6UP};
    Root: TComponent;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Modified; override;
    procedure MakeSubComponentsLinkable(Transition: TTransitionEffect); override;
    procedure SelectComponent(Instance: TPersistent); override;
    function  UniqueName(const BaseName: string): string; override;
  end;

procedure Register;
begin
  {$ifdef D6UP}
  StartClassGroup(TControl);
  GroupDescendentsWith(TFormContainer     , Controls.TControl);
  GroupDescendentsWith(TFCEmbeddedForm    , Controls.TControl);
  GroupDescendentsWith(TEffectsGroupBox   , Controls.TControl);
  GroupDescendentsWith(TEffectsPanel      , Controls.TControl);
  GroupDescendentsWith(TFormTransitions   , Controls.TControl);
  GroupDescendentsWith(TTransitionList    , Controls.TControl);
  GroupDescendentsWith(TTEAnimationList   , Controls.TControl);
  GroupDescendentsWith(TTEImage           , Controls.TControl);
  GroupDescendentsWith(TTETransitionExport, Controls.TControl);
  {$endif D6UP}

  RegisterComponents('Billenium effects', [TFormContainer, TEffectsGroupBox,
    TEffectsPanel, TFormTransitions, TTransitionList, TTEAnimationList,
    TTEImage, TTETransitionExport]);

  RegisterPropertyEditor(TypeInfo(Integer)          , TFCEmbeddedForm, 'Height' , nil);
  RegisterPropertyEditor(TypeInfo(Integer)          , TFCEmbeddedForm, 'Width'  , nil);
  RegisterPropertyEditor(TypeInfo(TControlScrollBar), TFCEmbeddedForm, ''       , nil);
  RegisterComponentEditor(TTransitionList , TTransitionListEditor);
  RegisterComponentEditor(TTEAnimationList, TTEAnimationListEditor);
  RegisterComponentEditor(TFormContainer  , TFormContainerEditor);

  RegisterCustomModule(TFCEmbeddedForm, TCustomModule);
  RegisterPackageWizard(TFCEmbeddedFormExpert.Create);
  RegisterNoIcon([TTransitionEffect, TTEAnimationEffect]);
end;

procedure ShowAbout;
begin
  TEAboutForm := TTEAboutForm.Create(nil);
  try
    TEAboutForm.ShowModal;
  finally
    FreeAndNil(TEAboutForm);
  end;
end;

procedure GotoWeb(From: String; Order: Boolean);
var
  MajorVersion,
  MinorVersion: Char;
  URL,
  Trial: String;
begin
  MajorVersion := BilleniumEffectsVersion[ 9];
  MinorVersion := BilleniumEffectsVersion[11];
  {$ifdef D12UP}
  Assert(CharInSet(MajorVersion, ['0'..'9']));
  Assert(CharInSet(MinorVersion, ['0'..'9']));
  {$else}
  Assert(MajorVersion in ['0'..'9']);
  Assert(MinorVersion in ['0'..'9']);
  {$endif D12UP}
  {$ifdef Trial}
  Trial := 't';
  {$else}
  Trial := 'r';
  {$endif Trial}

   if Order
   then URL := 'http://www.billeniumsoft.com/bef/order.htm'
   else URL := 'http://www.billeniumsoft.com';

   ShellExecute(Application.MainForm.Handle, nil,
    PChar(URL + '?src=be' + MajorVersion + MinorVersion + From + Trial),
    nil, nil, SW_SHOWNORMAL)
end;

procedure TTransitionListEditor.TransitionEditorInit(Control: TWinControl;
  FirstCall: Boolean);
var
  ModalEditor: TTransitionModalEditor;
begin
  if FirstCall and Assigned(Control) and (Control is TTransitionModalEditor) then
  begin
    ModalEditor := Control as TTransitionModalEditor;
    ModalEditor.BitBtnAbout.Visible := True;
    ModalEditor.BitBtnAbout.OnClick := BitBtnAboutClick;
  end;
end;

procedure TTransitionListEditor.ExecuteVerb(Index: Integer);
var
  i: Integer;
  {$ifdef D6UP}
  RealDesigner: IDesigner;
  {$endif D6UP}
  FormDesigner: TTEFormDesigner;
begin
  case Index of
    0: begin
         TransitionListEditorForm := nil;
         for i:=0 to Screen.FormCount-1 do
         begin
           if(Screen.Forms[i] is TTransitionListEditorForm) and
             (TTransitionListEditorForm(Screen.Forms[i]).TransitionList = Component) then
           begin
             TransitionListEditorForm :=
               TTransitionListEditorForm(Screen.Forms[i]);
             break;
           end;
         end;

         if TransitionListEditorForm = nil then
         begin                                                                      
           TransitionListEditorForm := TTransitionListEditorForm.Create(Application);
           FormDesigner := TTEFormDesigner.Create;
           {$ifdef D6UP}
           Designer.QueryInterface(IDesigner, RealDesigner);
           FormDesigner.TheDesigner := RealDesigner;
           FormDesigner.Root        := Component.Owner;
           {$else}
           FormDesigner.TheDesigner := Designer;
           {$endif D6UP}
           TransitionListEditorForm.Initialize(Component as TTransitionList,
             FormDesigner);
           if Component <> nil then
             TransitionListEditorForm.FreeNotification(Component);
           teEditorInit := TransitionEditorInit;
           TransitionListEditorForm.Show;
         end;
         TransitionListEditorForm.BringToFront;
       end;
    1: ShowAbout;
    2: GotoWeb('TL', False);
    3: GotoWeb('TL', True);
  end;
end;

function TTransitionListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Transitions editor...';
    1: Result := BilleniumEffectsVersion;
    2: Result := 'www.billeniumsoft.com';
    3: Result := 'Buy now';
  end;
end;

function TTransitionListEditor.GetVerbCount: Integer;
begin
  {$ifdef Trial}
  Result := 4;
  {$else}
  Result := 3;
  {$endif Trial}
end;

{ TFormContainerEditor }

procedure TFormContainerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowAbout;
    1: ExecuteFCEmbeddedFormExpert;
    2: GotoWeb('FC', False);
    3: GotoWeb('FC', True);
  end;
end;

function TFormContainerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := BilleniumEffectsVersion;
    1: Result := 'Create new EmbeddedForm';
    2: Result := 'www.billeniumsoft.com';
    3: Result := 'Buy now';
  end;
end;

function TFormContainerEditor.GetVerbCount: Integer;
begin
  {$ifdef Trial}
  Result := 4;
  {$else}
  Result := 3;
  {$endif Trial}
end;

{ TTEAnimationListEditor }

procedure TTransitionListEditor.BitBtnAboutClick(Sender: TObject);
begin
  ShowAbout;
end;

procedure TTEAnimationListEditor.ExecuteVerb(Index: Integer);
var
  i: Integer;
  {$ifdef D6UP}
  RealDesigner: IDesigner;
  {$endif D6UP}
begin
  case Index of
    0: begin
         teAnimationListEditorForm := nil;
         for i:=0 to Screen.FormCount-1 do
         begin
           if(Screen.Forms[i] is TTEAnimationListEditorForm) and
             (TTEAnimationListEditorForm(Screen.Forms[i]).AnimationList = Component) then
           begin
             teAnimationListEditorForm :=
               TTEAnimationListEditorForm(Screen.Forms[i]);
             break;
           end;
         end;

         if teAnimationListEditorForm = nil then
         begin
           teAnimationListEditorForm := TTEAnimationListEditorForm.Create(Application);
           {$ifdef D6UP}
           Designer.QueryInterface(IDesigner, RealDesigner);
           teAnimationListEditorForm.Initialize(Component as TTEAnimationList, RealDesigner);
           {$else}
           teAnimationListEditorForm.Initialize(Component as TTEAnimationList, Designer);
           {$endif D6UP}
           if Component <> nil then
             teAnimationListEditorForm.FreeNotification(Component);
           teAnimationListEditorForm.Show;
         end;
         teAnimationListEditorForm.BringToFront;
       end;
    1: ShowAbout;
    2: GotoWeb('AL', False);
    3: GotoWeb('AL', True);
  end;
end;

function TTEAnimationListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Animations editor...';
    1: Result := BilleniumEffectsVersion;
    2: Result := 'www.billeniumsoft.com';
    3: Result := 'Buy now';
  end;
end;

function TTEAnimationListEditor.GetVerbCount: Integer;
begin
  {$ifdef Trial}
  Result := 4;
  {$else}
  Result := 3;
  {$endif Trial}
end;

{ TTEFormDesignerBase }

constructor TTEFormDesigner.Create;
begin
  inherited;

  TheDesigner := nil;
end;

destructor TTEFormDesigner.Destroy;
begin
  TheDesigner := nil;

  inherited;
end;


procedure TTEFormDesigner.MakeSubComponentLinkable(ComponentClass: TComponentClass);
begin
  if SubComponents = nil then
    SubComponents := TList.Create;
  if SubComponents.IndexOf(ComponentClass) = -1 then
    SubComponents.Add(ComponentClass);
end;

procedure TTEFormDesigner.MakeSubComponentsLinkable(Transition:
    TTransitionEffect);

  function GetActiveProject: IOTAProject40;

    function FindModuleInterface(AInterface: TGUID): IUnknown;
    var
      i: Integer;
    begin
      Result := nil;
      with BorlandIDEServices as IOTAModuleServices do
        for i := 0 to ModuleCount - 1 do
          if Modules[i].QueryInterface(AInterface, Result) = S_OK then
            Break;
    end;

  var
    ProjectGroup: IOTAProjectGroup;
  begin
    ProjectGroup := FindModuleInterface(IOTAProjectGroup) as IOTAProjectGroup;
    if Assigned(ProjectGroup)
    then Result := ProjectGroup.ActiveProject
    else Result := FindModuleInterface(IOTAProject) as IOTAProject;
  end;

  procedure UpdateUsesInUnit(Project: IOTAProject40);
  var
    Index: Integer;
  begin
    Index :=
      (BorlandIDEServices as IOTAServices).AddNotifier(TProjectNotifier.Create);
    try
      try
        Project.ProjectBuilder.BuildProject(cmOTAMake, False);
      except
        on E: EAbort do;
      end;
    finally
      (BorlandIDEServices as IOTAServices).RemoveNotifier(Index);
    end;
  end;

  procedure AddUses(Project: IOTAProject40);
  var
    i: Integer;
    TmpComponent: TComponent;
    TmpComponents: TList;
  begin
    TmpComponents := TList.Create;
    try
      for i := 0 to SubComponents.Count-1 do
      begin
        TmpComponent := TheDesigner.CreateComponent(
          TComponentClass(SubComponents[i]), Root, 0, 0, 0, 0);
        TmpComponent.Name := UniqueName('teTmpComponent');
        TmpComponents.Add(TmpComponent);
      end;
      try
        UpdateUsesInUnit(Project);
      finally
          for i := 0 to TmpComponents.Count-1 do
        TComponent(TmpComponents[i]).Free;
      end;
    finally
      FreeAndNil(TmpComponents);
    end;
  end;

  procedure CreateTmpComponents;
  begin
  end;

var
  Project: IOTAProject40;
begin
  Project := GetActiveProject;
  if Assigned(Project) then
  begin
    SubComponents := nil;
    try
      TTransitionEffectHack(Transition).MakeSubComponentsLinkable(
        MakeSubComponentLinkable);
      if Assigned(SubComponents) then
      begin
        AddUses(Project);
      end;
    finally
      FreeAndNil(SubComponents);
    end;
  end;
end;

procedure TTEFormDesigner.Modified;
begin
  TheDesigner.Modified;
end;

procedure TTEFormDesigner.SelectComponent(Instance: TPersistent);
begin
  TheDesigner.SelectComponent(Instance);
end;

function TTEFormDesigner.UniqueName(const BaseName: string): string;
begin
  Result := TheDesigner.UniqueName(BaseName);
end;

procedure TProjectNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TProjectNotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  Cancel := True;
end;

procedure TProjectNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: String; var Cancel: Boolean);
begin
end;

{$ifdef Trial}
{$include trial\taux6.inc}
{$endif Trial}

end.
