unit teRandom;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, Windows, Messages;

{$ifndef TE_NOHLP}
resourcestring
  rsTERndHasTransitions  = 'You''ll loose random transition settings. ¿Continue?';
{$endif TE_NOHLP}

type
  TTERndMode = (termRandom, termCyclical);

  {$ifndef TE_NOHLP}
  TTERndTransitions = class(TTransitionList);
  {$endif TE_NOHLP}

  TRandomTransition = class(TTransitionEffect)
  private
    FMode: TTERndMode;
    FLastTransition: TTransitionEffect;
    FSelectedIndex: Integer;

    function  GetTransitions(Index: Integer): TTransitionEffect;
    procedure SetSelectedIndex(const Value: Integer);
    procedure SetTransitions(Index: Integer; const Value: TTransitionEffect);
    procedure LoadTransitions(Stream: TStream);
    procedure StoreTransitions(Stream: TStream);
  protected
    FTransitions: TTERndTransitions;

    procedure DefineProperties(Filer: TFiler); override;
    function EditorQuestion: string; override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    function MakeSubComponentsLinkable(Proc: TTEMakeSubComponentLinkable): Boolean; override;
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    destructor  Destroy; override;
    procedure AddTransition(Transition: TTransitionEffect);
    class function Description: String; override;
    class function GetEditor: String; override;
    procedure Assign(Source: TPersistent); override;
    function  GetDelegate(Device: TTETransitionDevice;
      const ReturnCopy: Boolean): TTransitionEffect; override;
    procedure RemoveTransition(Transition: TTransitionEffect);
    function SelectedTransition: TTransitionEffect;
    function  TransitionCount: Integer;

    property LastTransition: TTransitionEffect read FLastTransition;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Transitions[Index: Integer]: TTransitionEffect read GetTransitions write SetTransitions;
  published
    property Milliseconds;
    property Mode: TTERndMode read FMode write FMode default termRandom;
  end;

implementation

uses TypInfo;

type
  TTransitionEffectHack = class(TTransitionEffect);

constructor TRandomTransition.Create(AOwner: TComponent);
begin
  inherited;

  FTransitions := TTERndTransitions.Create(Self);
  FLastTransition := nil;
  FSelectedIndex  := -1;
  FMode           := termRandom;

  Randomize;
end;

destructor TRandomTransition.Destroy;
begin
  FTransitions.Free;

  inherited;
end;

procedure TRandomTransition.AddTransition(Transition: TTransitionEffect);
begin
  FTransitions.AddTransition(Transition);
end;

procedure TRandomTransition.Assign(Source: TPersistent);
var
  Transition: TRandomTransition;
begin
  if Source is TRandomTransition
  then
  begin
    inherited;

    Transition := TRandomTransition(Source);
    Mode       := Transition.Mode;
    FTransitions.Assign(Transition.FTransitions);
  end
  else
  begin
    inherited;

    Milliseconds := 0; // We don't want this property to be asigned from other transitions
  end;
end;

class function TRandomTransition.Description: String;
begin
  Result := 'Random';
end;

function TRandomTransition.GetDelegate(Device: TTETransitionDevice;
  const ReturnCopy: Boolean): TTransitionEffect;
begin
  if TransitionCount > 0
  then
  begin
    FLastTransition := SelectedTransition;
    if FMode = termCyclical
    then
    begin
      Inc(FSelectedIndex);
      if FSelectedIndex >= TransitionCount then
        FSelectedIndex := 0;
    end
    else
    begin
      FLastTransition := nil;
      FSelectedIndex := Random(TransitionCount);
    end;
    Result := Transitions[FSelectedIndex].GetDelegate(Device, ReturnCopy);
    if(Milliseconds > 0) and Assigned(Result) then
      Result.Milliseconds := Milliseconds; 
  end
  else Result := TFlickerFreeTransition.Create(Self);
end;

class function TRandomTransition.GetEditor: String;
begin
  Result := 'TRandomTransitionEditor';
end;

function TRandomTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) -
    [
      tetiNeedDstBmp,
      tetiNeedSrcBmp,
      tetiTwoPassesCapable
    ];
end;

function TRandomTransition.SelectedTransition: TTransitionEffect;
begin
  if(FSelectedIndex >= 0) and
    (FSelectedIndex < TransitionCount)
  then Result := Transitions[FSelectedIndex]
  else Result := nil;
end;

function TRandomTransition.GetTransitions(Index: Integer): TTransitionEffect;
begin
  Result := FTransitions.Transitions[Index];
end;

procedure TRandomTransition.RemoveTransition(Transition: TTransitionEffect);
begin
  FTransitions.RemoveTransition(Transition);
  if FLastTransition = Transition then
    FLastTransition := nil;
  if FSelectedIndex = Transition.Index then
    FSelectedIndex := FSelectedIndex - 1;
end;

function TRandomTransition.MakeSubComponentsLinkable(Proc:
    TTEMakeSubComponentLinkable): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to TransitionCount-1 do
  begin
    Result := True;
    Proc(TComponentClass(Transitions[i].ClassType));
    TTransitionEffectHack(Transitions[i]).MakeSubComponentsLinkable(Proc);
  end;
end;

procedure TRandomTransition.SetSelectedIndex(const Value: Integer);
begin
  if Value < TransitionCount then
    FSelectedIndex := Value;
end;

procedure TRandomTransition.SetTransitions(Index: Integer;
  const Value: TTransitionEffect);
begin
  FTransitions.Transitions[Index] := Value;
end;

function TRandomTransition.TransitionCount: Integer;
begin
  Result := FTransitions.TransitionCount;
end;

procedure TRandomTransition.LoadTransitions(Stream: TStream);
begin
  FTransitions.Free;
  FTransitions := TTERndTransitions(Stream.ReadComponent(nil));
end;

procedure TRandomTransition.StoreTransitions(Stream: TStream);
begin
  Stream.WriteComponent(FTransitions);
end;

procedure TRandomTransition.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil
    then // check Ancestor for an inherited value
    begin
      if TRandomTransition(Filer.Ancestor).TransitionCount > 0
      then Result := TransitionCount > 0
      else
      begin
        if TransitionCount = 0
        then Result := True
        else Result := False;
      end
    end
    else Result := TransitionCount > 0; // no inherited value
  end;

begin
  inherited;

  Filer.DefineBinaryProperty('Transitions', LoadTransitions, StoreTransitions,
    DoWrite);
end;

function TRandomTransition.EditorQuestion: string;
begin
  if TransitionCount > 0
  then Result := rsTERndHasTransitions
  else Result := inherited EditorQuestion;
end;

procedure TRandomTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  i,
  Count: Integer;
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'Mode'];
  if Value = 'Random'
  then Mode := termRandom
  else if Value = 'Cyclical' then
    Mode := termCyclical;

  Count := 0;
  Value := List.Values[Prefix + 'Count'];
  if Value <> '' then
    Count := StrToInt(Value);

  for i := 1 to Count do
    AddTransition(InternalLoadTransitionFromStrings(List, 'Rnd' + IntToStr(i) + '_'));
end;

procedure TRandomTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
var
  i: Integer;
  Prop: String;
  PropInfo: PPropInfo;
begin
  inherited;

  Prop := 'Mode';
  if OmitDefaultValues
  then PropInfo := GetPropInfo(Self, Prop, [tkEnumeration])
  else PropInfo := nil;
  if(not OmitDefaultValues) or
    (PropInfo.Default <> Ord(Mode)) then
  begin
    case Mode of
      termRandom  : List.Values[Prefix + Prop] := 'Random';
      termCyclical: List.Values[Prefix + Prop] := 'Cyclical';
    end;
  end;

  Prop := Prefix + 'Count';
  if(not OmitDefaultValues) or
    (TransitionCount <> 0) then
    List.Values[Prop] := IntToStr(TransitionCount);

  for i := 1 to TransitionCount do
    TTransitionEffectHack(Transitions[i-1]).SaveToStrings(
      List, OmitDefaultValues, Prefix + 'Rnd' + IntToStr(i) + '_');
end;

initialization

  TERegisterTransition(TRandomTransition);
  RegisterClasses([TRandomTransition, TTERndTransitions]);

end.
