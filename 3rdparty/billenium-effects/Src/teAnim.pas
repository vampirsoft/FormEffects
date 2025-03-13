unit teAnim;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, Windows, Messages, Controls, Forms, TransEff;

{$ifndef TE_NOHLP}
const
  CM_TENAMECHANGED = CM_BASE + 533;
{$endif TE_NOHLP}

type
  TTEAnimationList = class;

  TTEAnimationEffect = class(TComponent)
  private
    FAnimationList: TTEAnimationList;
    FEnabled: Boolean;

    function  GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetAnimationList(const Value: TTEAnimationList);
    function  GetIndex: Integer;
    procedure SetIndex(Value: Integer);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    class function Description: String; virtual;

    function  HasParent: Boolean; override;
    function  GetParentComponent: TComponent; override;
    procedure SetParentComponent(AParent: TComponent); override;
    {$ifndef TE_NOHLP}
    property AnimationList: TTEAnimationList read FAnimationList write SetAnimationList;
    property Index: Integer read GetIndex write SetIndex stored False;
    {$endif TE_NOHLP}
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Version: String read GetVersion write SetVersion stored False;
  end;

  TTEAnimationList = class(TComponent)
  private
    FAnimations: TList;

    function  GetAnimation(Index: Integer): TTEAnimationEffect;
    procedure SetAnimation(Index: Integer; const Value: TTEAnimationEffect);
    function  GetVersion: String;
    procedure SetVersion(const Value: String);
    function  GetAnimationCount: Integer;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    {$ifndef TE_NOHLP}
    Editor: TForm;
    {$endif TE_NOHLP}

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddAnimation(Animation: TTEAnimationEffect);
    procedure Clear;
    function GetAnimationIndex(Animation: TTEAnimationEffect): Integer;
    procedure RemoveAnimation(Animation: TTEAnimationEffect);

    property AnimationCount: Integer read GetAnimationCount;
    property Animations[Index: Integer]: TTEAnimationEffect read GetAnimation write SetAnimation; default;
  published
    property Version: String read GetVersion write SetVersion stored False;
  end;

implementation

uses teRender;

{ TTEAnimationEffect }

constructor TTEAnimationEffect.Create(AOwner: TComponent);
begin
  inherited;

  FEnabled       := True;
  FAnimationList := nil;
end;

destructor TTEAnimationEffect.Destroy;
begin
  if Assigned(FAnimationList) then
    FAnimationList.RemoveAnimation(Self);

  inherited;
end;

class function TTEAnimationEffect.Description: String;
begin
  Result := ClassName;
end;

function TTEAnimationEffect.GetIndex: Integer;
begin
  if FAnimationList <> nil
  then Result := FAnimationList.FAnimations.IndexOf(Self)
  else Result := -1;
end;

function TTEAnimationEffect.GetParentComponent: TComponent;
begin
  if FAnimationList <> nil
  then Result := FAnimationList
  else Result := inherited GetParentComponent;
end;

function TTEAnimationEffect.GetVersion: String;
begin
  Result := BilleniumEffectsVersion;
end;

function TTEAnimationEffect.HasParent: Boolean;
begin
  if FAnimationList <> nil
  then Result := True
  else Result := inherited HasParent;
end;

procedure TTEAnimationEffect.ReadState(Reader: TReader);
begin
  inherited;

  if Reader.Parent is TTEAnimationList then
    AnimationList := TTEAnimationList(Reader.Parent);
end;

procedure TTEAnimationEffect.SetAnimationList(const Value: TTEAnimationList);
begin
  if Value <> FAnimationList then
  begin
    if FAnimationList <> nil then
      FAnimationList.RemoveAnimation(Self);
    if Value <> nil then
      Value.AddAnimation(Self);
  end;
end;

procedure TTEAnimationEffect.SetIndex(Value: Integer);
var
  CurIndex,
  Count: Integer;
begin
  CurIndex := GetIndex;
  if CurIndex >= 0 then
  begin
    Count := FAnimationList.FAnimations.Count;
    if Value < 0 then
      Value := 0;
    if Value >= Count then
      Value := Count - 1;
    if Value <> CurIndex then
    begin
      FAnimationList.FAnimations.Delete(CurIndex);
      FAnimationList.FAnimations.Insert(Value, Self);
    end;
  end;
end;

procedure TTEAnimationEffect.SetName(const Value: TComponentName);
begin
  inherited;

  if Assigned(FAnimationList) and Assigned(FAnimationList.Editor) then
    FAnimationList.Editor.Perform(CM_TENAMECHANGED, Longint(Self), 0);
end;

procedure TTEAnimationEffect.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) and (AParent is TTEAnimationList) then
    AnimationList := TTEAnimationList(AParent);
end;

procedure TTEAnimationEffect.SetVersion(const Value: String);
begin
end;

{ TTEAnimationList }

procedure TTEAnimationList.AddAnimation(Animation: TTEAnimationEffect);
begin
  FAnimations.Add(Animation);
  Animation.FAnimationList := Self;
end;

constructor TTEAnimationList.Create(AOwner: TComponent);
begin
  inherited;

  Editor      := nil;
  FAnimations := TList.Create;
end;

destructor TTEAnimationList.Destroy;
begin
  Clear;
  FAnimations.Free;

  inherited;
end;

procedure TTEAnimationList.Clear;
begin
  if Assigned(FAnimations) then
    while FAnimations.Count > 0  do
      TTEAnimationEffect(FAnimations[0]).Free;
end;

function TTEAnimationList.GetAnimation(Index: Integer): TTEAnimationEffect;
begin
  Result := FAnimations[Index];
end;

function TTEAnimationList.GetAnimationCount: Integer;
begin
  if FAnimations = nil
  then Result := 0
  else Result := FAnimations.Count;
end;

procedure TTEAnimationList.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
  Animation: TTEAnimationEffect;
begin
  for I := 0 to FAnimations.Count - 1 do
  begin
    Animation := FAnimations[I];
    if Animation.Owner = Root then
      Proc(Animation);
  end;
end;

function TTEAnimationList.GetAnimationIndex(Animation: TTEAnimationEffect):
    Integer;
begin
  Result := FAnimations.IndexOf(Animation);
end;

function TTEAnimationList.GetVersion: String;
begin
  Result := BilleniumEffectsVersion;
end;

procedure TTEAnimationList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if(Operation = opRemove)             and
    (AComponent is TTEAnimationEffect) then
    RemoveAnimation(TTEAnimationEffect(AComponent));
end;

procedure TTEAnimationList.RemoveAnimation(Animation: TTEAnimationEffect);
begin
  if FAnimations.Remove(Animation) >= 0 then
    Animation.FAnimationList := nil;
end;

procedure TTEAnimationList.SetAnimation(Index: Integer;
  const Value: TTEAnimationEffect);
begin
  Animations[Index].Free;
  AddAnimation(Value);
  Value.Index := Index;
end;

procedure TTEAnimationList.SetVersion(const Value: String);
begin
end;

end.
