unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TransEff, teTimed, teBlend, teAllTr, teEditor;

type
  TFormMain = class(TForm)
    ButtonEditEffect: TButton;
    MemoTxt: TMemo;
    ButtonSave: TButton;
    ButtonLoad: TButton;
    procedure ButtonEditEffectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
  private
    Transition: TTransitionEffect;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses IniFiles;

procedure TFormMain.ButtonEditEffectClick(Sender: TObject);
begin
  if Assigned(Transition) then
    ChangeTransition(Transition);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Transition := nil;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  IniFile: TIniFile;
  List: TStringList;
  i: Integer;
begin
  if Assigned(Transition) then
  begin
    IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Persistence.ini');
    try
      List := TStringList.Create;
      try
        List.NameValueSeparator := '=';
        List.QuoteChar          := '"';
//        List.Delimiter          := ';';
        TESaveTransitionToStrings(Transition, List, True);
        IniFile.EraseSection('Transition1');
        for i:=0 to List.Count-1 do
          IniFile.WriteString('Transition1', List.Names[i], List.ValueFromIndex[i]);
        MemoTxt.Lines.Assign(List);
      finally
        List.Free;
      end;
    finally
      IniFile.Free;
    end;
  end;
end;

procedure TFormMain.ButtonLoadClick(Sender: TObject);
var
  IniFile: TIniFile;
  List: TStringList;
begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Persistence.ini');
  try
    FreeAndNil(Transition);
    List := TStringList.Create;
    try
      List.NameValueSeparator := '=';
      List.QuoteChar          := '"';
//      List.Delimiter          := ';';
      IniFile.ReadSectionValues('Transition1', List);
      MemoTxt.Lines.Assign(List);
      Transition := TELoadTransitionFromStrings(List);
    finally
      List.Free;
    end;
  finally
    IniFile.Free;
  end;
end;

end.
