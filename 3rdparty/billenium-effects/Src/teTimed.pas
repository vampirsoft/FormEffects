unit teTimed;

interface

{$INCLUDE teDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, TransEff, teChrono, Graphics;

type
  TTimedTransitionEffect = class(TTransitionEffect)
  protected
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  published
    property Milliseconds;
  end;

implementation

uses MMSystem;

{ TTimedTransitionEffect }

procedure TTimedTransitionEffect.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'Milliseconds'];
  if Value <> '' then
    Milliseconds := StrToInt(Value);
end;

procedure TTimedTransitionEffect.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
begin
  inherited;

  if(not OmitDefaultValues) or (Milliseconds <> 0) then
    List.Values[Prefix + 'Milliseconds'] := IntToStr(Milliseconds);
end;

end.
