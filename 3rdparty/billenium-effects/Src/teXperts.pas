unit teXperts;

interface

{$INCLUDE teDefs.inc}

uses
  Windows, SysUtils, Classes, Forms, ToolsAPI;

type

  TFCEmbeddedFormExpert = class(
    TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTAFormWizard)
  public
    function GetName: string;
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;         
    function GetGlyph: {$ifdef D6UP} Cardinal; {$else} HICON; {$endif D6UP}
    function GetState: TWizardState;
    function GetIDString: string;

    procedure Execute;
  end;

  procedure ExecuteFCEmbeddedFormExpert;

implementation

type
  TFCEmbeddedFormModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TFCEmbeddedFormSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const Source: string);
  end;

function IntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  with Stream do
  try
    WriteString('//---------------------------------------------------------------------------' + #13#10);
    WriteString(Format('#ifndef %sH', [ModuleIdent]) + #13#10);
    WriteString(Format('#define %sH', [ModuleIdent]) + #13#10);
    WriteString('//---------------------------------------------------------------------------' + #13#10);
    WriteString('#include <Classes.hpp>' + #13#10);
    WriteString('#include <Controls.hpp>' + #13#10);
    WriteString('#include <StdCtrls.hpp>' + #13#10);
    WriteString('#include <Forms.hpp>' + #13#10);
    WriteString('#include <FormCont.hpp>' + #13#10);
    WriteString('//---------------------------------------------------------------------------' + #13#10);
    WriteString(Format('class T%s : public T%s', [FormIdent, AncestorIdent]) + #13#10);
    WriteString('{' + #13#10);
    WriteString('__published:' + #13#10);
    WriteString('private:' + #13#10);
    WriteString('public:' + #13#10);
    WriteString(Format('    __fastcall T%s(TComponent* Owner);', [FormIdent]) + #13#10);
    WriteString('};' + #13#10);
    WriteString('//---------------------------------------------------------------------------' + #13#10);
    WriteString(Format('extern PACKAGE T%s *%0:s;', [FormIdent]) + #13#10);
    WriteString('//---------------------------------------------------------------------------' + #13#10);
    WriteString('#endif' + #13#10);

    Result := DataString;
  finally
    Free;
  end;
end;

function ImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): string;
var
  Stream: TStringStream;
  IsBCB: Boolean;
begin
{$ifdef D10UP}
  IsBCB := CompareStr(PersonalityServices.CurrentPersonality, sCBuilderPersonality) = 0;
{$else}
  {$ifdef BCB}
  IsBCB := True;
  {$else}
  IsBCB := False;
  {$endif BCB}
{$endif D10UP}

  Stream := TStringStream.Create('');
  with Stream do
  try
    if IsBCB
    then
    begin
      WriteString('//---------------------------------------------------------------------------' + #13#10);
      WriteString('#include <vcl.h>' + #13#10);
      WriteString('#pragma hdrstop' + #13#10);
      WriteString(#13#10);
      WriteString(Format('#include "%s.h"', [ModuleIdent]) + #13#10);
      WriteString('//---------------------------------------------------------------------------' + #13#10);
      WriteString('#pragma package(smart_init)' + #13#10);
      WriteString('#pragma resource "*.dfm"' + #13#10);
      WriteString(Format('T%s *%0:s;', [FormIdent]) + #13#10);
      WriteString('//---------------------------------------------------------------------------' + #13#10);
      WriteString(Format('__fastcall T%s::T%0:s(TComponent* Owner)', [FormIdent]) + #13#10);
      WriteString(Format('    : T%s(Owner)', [AncestorIdent]) + #13#10);
      WriteString('{' + #13#10);
      WriteString('}' + #13#10);
      WriteString('//---------------------------------------------------------------------------' + #13#10);
    end
    else
    begin
      WriteString(Format('unit %s;', [ModuleIdent]) + #13#10);
      WriteString(#13#10);
      WriteString('interface' + #13#10);
      WriteString(#13#10);
      WriteString('uses' + #13#10);
      WriteString('  Windows, Messages, SysUtils, ' +
        {$ifdef D6UP}
        'Variants, ' +
        {$endif D6UP}
        'Classes, Graphics, Controls, Forms,' + #13#10);
      WriteString('  Dialogs, FormCont;' + #13#10);
      WriteString(#13#10);
      WriteString('type' + #13#10);
      WriteString(Format('  T%s = class(T%s)', [FormIdent, AncestorIdent]) + #13#10);
      WriteString('  private' + #13#10);
      WriteString('  public' + #13#10);
      WriteString('  end;' + #13#10);
      WriteString(#13#10);
      WriteString('var' + #13#10);
      WriteString(Format('  %s: T%0:s;', [FormIdent]) + #13#10);
      WriteString(#13#10);
      WriteString('implementation' + #13#10);
      WriteString(#13#10);
      WriteString('{$R *.DFM}' + #13#10);
      WriteString(#13#10);
      WriteString('end.' + #13#10);
    end;
    Result := DataString;
  finally
    Free;
  end;
end;

{ TFCEmbeddedFormModuleCreator }

procedure TFCEmbeddedFormModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TFCEmbeddedFormModuleCreator.GetAncestorName: string;
begin
  Result := 'FCEmbeddedForm';
end;

function TFCEmbeddedFormModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TFCEmbeddedFormModuleCreator.GetExisting: Boolean;
begin
  Result := False
end;

function TFCEmbeddedFormModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TFCEmbeddedFormModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TFCEmbeddedFormModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TFCEmbeddedFormModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TFCEmbeddedFormModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TFCEmbeddedFormModuleCreator.GetOwner: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  NewModule: IOTAModule;
begin
  Result := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Module := ModuleServices.CurrentModule;

  if Module <> nil then
  begin
    if Module.QueryInterface(IOTAProject, NewModule) = S_OK
    then Result := NewModule
    else
    begin
    {$ifndef D6UP}
      if Module.GetOwnerCount > 0 then
      begin
        NewModule := Module.GetOwner(0);
    {$else}
      if Module.OwnerModuleCount > 0 then
      begin
        NewModule := Module.OwnerModules[0];
    {$endif D6UP}
        if NewModule <> nil then
          if NewModule.QueryInterface(IOTAProject, Result) <> S_OK then
            Result := nil;
      end;
    end;
  end;
end;

function TFCEmbeddedFormModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TFCEmbeddedFormModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TFCEmbeddedFormModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TFCEmbeddedFormModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TFCEmbeddedFormModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TFCEmbeddedFormSourceFile.Create(ImplSource(ModuleIdent, FormIdent, AncestorIdent));
end;

function TFCEmbeddedFormModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
{$ifdef D10UP}
  if CompareStr(PersonalityServices.CurrentPersonality, sCBuilderPersonality) = 0
  then Result := TFCEmbeddedFormSourceFile.Create(IntfSource(ModuleIdent, FormIdent, AncestorIdent))
  else Result := nil;
{$else}
  {$ifdef BCB}
  Result := TFCEmbeddedFormSourceFile.Create(IntfSource(ModuleIdent, FormIdent, AncestorIdent));
  {$else}
  Result := nil;
  {$endif BCB}
{$endif D10UP}
end;

{ TFCEmbeddedFormSourceFile }

constructor TFCEmbeddedFormSourceFile.Create(const Source: string);
begin
  FSource := Source;
end;

function TFCEmbeddedFormSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TFCEmbeddedFormSourceFile.GetSource: string;
begin
  Result := FSource;
end;

{ TFCEmbeddedFormExpert }
function TFCEmbeddedFormExpert.GetName: string;
begin
  Result := 'Embedded form';
end;

function TFCEmbeddedFormExpert.GetComment: string;
begin
  Result := 'Creates a new TFCEmbeddedForm for embedding into a FormContainer';
end;

function TFCEmbeddedFormExpert.GetGlyph: {$ifdef D6UP} Cardinal; {$else} HICON; {$endif D6UP}
begin
  Result := 0;
end;

function TFCEmbeddedFormExpert.GetState: TWizardState; 
begin
  Result := [];
end;

function TFCEmbeddedFormExpert.GetIDString: string;
begin
  Result := 'BilleniumEffects.EmbeddedFormExpert';
end;

function TFCEmbeddedFormExpert.GetAuthor: string;
begin
  Result := 'BilleniumSoft';
end;

function TFCEmbeddedFormExpert.GetPage: string;
begin
  {$ifndef D9UP}
  Result := 'New';
  {$else}
  Result := sCategoryDelphiNewFiles;
  {$endif D9UP}
end;

procedure TFCEmbeddedFormExpert.Execute;
begin
  ExecuteFCEmbeddedFormExpert;
end;

procedure ExecuteFCEmbeddedFormExpert;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TFCEmbeddedFormModuleCreator.Create);
end;

end.
