unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TransEff, teTimed, teRoll, teForm, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    FormTransitions1: TFormTransitions;
    TransitionList1: TTransitionList;
    Transition1: TRollTransition;
    Label1: TLabel;
    BitBtn1: TBitBtn;
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

end.
 