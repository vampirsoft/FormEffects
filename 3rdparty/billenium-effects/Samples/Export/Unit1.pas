unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, teImage, jpeg, ExtCtrls, TransEff, teTimed, teMasked, teBlock,
  ComCtrls, teTrExpo;

type
  TForm1 = class(TForm)
    TransitionList1: TTransitionList;
    ButtonExport: TButton;
    TEImageSrc: TTEImage;
    Label1: TLabel;
    TEImageTgt: TTEImage;
    Label2: TLabel;
    Preview: TTEImage;
    LabelPreview: TLabel;
    TETransitionExport: TTETransitionExport;
    Transition1: TBlockTransition;
    CheckBoxExportSrc: TCheckBox;
    CheckBoxExportTgt: TCheckBox;
    Label3: TLabel;
    ComboBoxPixelFormat: TComboBox;
    Label4: TLabel;
    EditFps: TEdit;
    UpDownFps: TUpDown;
    Label5: TLabel;
    LabelMilliseconds: TLabel;
    EditMilliseconds: TEdit;
    UpDownMilliseconds: TUpDown;
    Bevel1: TBevel;
    Label6: TLabel;
    ButtonAbort: TButton;
    procedure ButtonExportClick(Sender: TObject);
    procedure TETransitionExportFrame(Sender: TObject; Frame: TBitmap;
      FrameIndex, TotalFrames: Integer; var Abort: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ButtonAbortClick(Sender: TObject);
  private
    OutputDir: String;
    Aborted: Boolean;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  OutputDir := ExtractFilePath(Application.ExeName) + 'Output';
  CreateDir(OutputDir);

  ComboBoxPixelFormat.Items.AddObject('15 bits per pixel', TObject(pf15bit));
  ComboBoxPixelFormat.Items.AddObject('16 bits per pixel', TObject(pf16bit));
  ComboBoxPixelFormat.Items.AddObject('24 bits per pixel', TObject(pf24bit));
  ComboBoxPixelFormat.Items.AddObject('32 bits per pixel', TObject(pf32bit));
  ComboBoxPixelFormat.ItemIndex := 0;
end;

procedure TForm1.ButtonExportClick(Sender: TObject);

  procedure RemoveOldFiles;
  var
    DirInfo: TSearchRec;
    r : Integer;
    FileName: String;
  begin
    r := FindFirst(OutputDir + '\*.bmp', FaAnyfile, DirInfo);
    while r = 0 do  begin
      if ((DirInfo.Attr and FaDirectory <> FaDirectory) and
          (DirInfo.Attr and FaVolumeId  <> FaVolumeID)) then
      begin
        FileName := OutputDir + '\' + DirInfo.Name;
        if(DirInfo.Attr and SysUtils.faReadOnly) <> 0 then
          SetFileAttributes(PChar(FileName), DirInfo.Attr and (not SysUtils.faReadOnly));
        DeleteFile(PChar(FileName));
      end;
      r := FindNext(DirInfo);
    end;
    SysUtils.FindClose(DirInfo);
  end;

begin
  Screen.Cursor := crHourGlass;
  try
    RemoveOldFiles;
    Aborted := False;
    Transition1.Milliseconds := StrToInt(EditMilliseconds.Text);

    TETransitionExport.ExportSourceFrame := CheckBoxExportSrc.Checked;
    TETransitionExport.ExportTargetFrame := CheckBoxExportTgt.Checked;
    TETransitionExport.PixelFormat       :=
      TPixelFormat(ComboBoxPixelFormat.Items.Objects[ComboBoxPixelFormat.ItemIndex]);
    TETransitionExport.Fps               := StrToInt(EditFps.Text);

    ButtonExport.Enabled := False;
    ButtonAbort .Visible := True;
    try
      if not TETransitionExport.Execute(TEImageSrc.Picture.Graphic,
        TEImageTgt.Picture.Graphic) then
        ShowMessage('Export aborted');
    finally
      ButtonExport.Enabled := True;
      ButtonAbort .Visible := False;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.TETransitionExportFrame(Sender: TObject; Frame: TBitmap;
  FrameIndex, TotalFrames: Integer; var Abort: Boolean);
begin
  Application.ProcessMessages;
  Abort := Aborted;

  Preview.Picture.Assign(Frame);
  LabelPreview.Caption := Format('Frame %d of %d', [FrameIndex, TotalFrames]);
  Preview.Update;
  LabelPreview.Update;
  Frame.SaveToFile(OutputDir + '\frame' + Format('%.4d', [FrameIndex]) + '.bmp');
end;

procedure TForm1.ButtonAbortClick(Sender: TObject);
begin
  Aborted := True;
end;

end.
