unit teTrExpo;

interface

{$INCLUDE teDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, Consts, Graphics, TransEff;

type
  TTEFrameEvent = procedure(Sender: TObject; Frame: TBitmap;
    FrameIndex, TotalFrames: Integer; var Abort: Boolean) of object;

  TTETransitionExport = class(TComponent)
  private
    FExportSourceFrame: Boolean;
    FExportTargetFrame: Boolean;
    FFps: Word;
    FOnFrame: TTEFrameEvent;
    FPixelFormat: TPixelFormat;
    FTransition: TTransitionEffect;

    function GetVersion: String;
    procedure SetPixelFormat(const Value: TPixelFormat);
    procedure SetVersion(const Value: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(SourceGraphic, TargetGraphic: TGraphic): Boolean;
  published
    property ExportSourceFrame: Boolean read FExportSourceFrame write FExportSourceFrame default True;
    property ExportTargetFrame: Boolean read FExportTargetFrame write FExportTargetFrame default True;
    property Fps: Word read FFps write FFps default 0;
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat default pf24bit;
    property Transition: TTransitionEffect read FTransition write FTransition default nil;
    property Version: String read GetVersion write SetVersion stored False;

    property OnFrame: TTEFrameEvent read FOnFrame write FOnFrame;
  end;

implementation

uses
  teChrono, Controls, teRender;

resourcestring
  rsTEBadPixelFormat = 'PixelFormat not supported';
  rsTENilFrameEvent  = 'OnFrame event must be implemented';

type
  TTransitionEffectHack = class(TTransitionEffect);

  TTETransitionExportDevice = class(TTETransitionDevice)
  private
    FOnFrame: TTEFrameEvent;
  protected
    FPixelFormat: TPixelFormat;
    FrameBmp: TBitmap;
    FrameIndex,
    TotalFrames: Integer;
    FFps: Integer;
    MsStep,
    MsDone: Double;
    FExportSrc,
    FExportTgt: Boolean;

    procedure CustomExecute; override;
    function GetExtTimingData(FrameRendered: Boolean): Integer; override;
    procedure TransitionInitialized; override;
    class function TransitionIsDisabled(Transition: TTransitionEffect;
        NoFlickerFreeWhenDisabled: Boolean): Boolean; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    function  HasPalette: Boolean; override;
    function  PixelFormat: TPixelFormat; override;
    procedure Prepare(SourceGraphic, TargetGraphic: TGraphic;
      APixelFormat: TPixelFormat; Fps: Word; ExportSrc, ExportTgt: Boolean;
      OnFrameEvent: TTEFrameEvent);
    procedure UnPrepare;
    function  TwoPassesCapable: Boolean; override;
  end;

constructor TTETransitionExportDevice.Create;
begin
  inherited;

  FrameBmp := nil;
  FOnFrame := nil;
end;

destructor TTETransitionExportDevice.Destroy;
begin
  UnPrepare;

  inherited;
end;

function TTETransitionExportDevice.HasPalette: Boolean;
begin
  Result := False;
end;

function TTETransitionExportDevice.PixelFormat: TPixelFormat;
begin
  Result := FPixelFormat;
end;

class function TTETransitionExportDevice.TransitionIsDisabled(Transition:
    TTransitionEffect; NoFlickerFreeWhenDisabled: Boolean): Boolean;
begin
  Result := False;
end;

function TTETransitionExportDevice.TwoPassesCapable: Boolean;
begin
  Result := True;
end;

procedure TTETransitionExportDevice.CustomExecute;
var
  OldPalette: HPalette;
  TotalMilliseconds: Integer;
begin
  if(Pass2Image = nil) and (DelegateTransition.Passes(Self) = 2) then
  begin
    if DelegateTransition.Pass2Options.SolidColor = clNone then
      DelegateTransition.Pass2Options.SolidColor := clWhite;
    Get2ndPassBmp;
  end;

  FrameIndex := 1;

  if Pass2Image = nil
  then
  begin
    Data.SrcBmp := SrcImage;
    Data.DstBmp := DstImage;
    GetOffScreenBmp(OldPalette);
    BitBlt(FrameBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
      Data.SrcBmp.Canvas.Handle, 0, 0, cmSrcCopy);
    ExePass(1, nil, DelegateTransition.Milliseconds);
  end
  else
  begin
    TotalMilliseconds := DelegateTransition.Milliseconds;
    if DelegateTransition.Pass2Options.DistributedTime and
      (DelegateTransition.Milliseconds <> 0) then
      DelegateTransition.Milliseconds := TotalMilliseconds div 2;

    Data.SrcBmp := SrcImage;
    Data.DstBmp := Pass2Image;
    GetOffScreenBmp(OldPalette);
    BitBlt(FrameBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
      Data.SrcBmp.Canvas.Handle, 0, 0, cmSrcCopy);
    ExePass(1, nil, TotalMilliseconds);

    FreeAndNil(SrcImage);

    Data.SrcBmp := Pass2Image;
    Data.DstBmp := DstImage;
    GetOffScreenBmp(OldPalette);
    BitBlt(FrameBmp.Canvas.Handle, 0, 0, Data.Width, Data.Height,
      Data.SrcBmp.Canvas.Handle, 0, 0, cmSrcCopy);
    ExePass(2, nil, TotalMilliseconds);
  end;
end;

procedure TTETransitionExportDevice.Prepare(
  SourceGraphic, TargetGraphic: TGraphic; APixelFormat: TPixelFormat;
  Fps: Word; ExportSrc, ExportTgt: Boolean; OnFrameEvent: TTEFrameEvent);
begin
  if not Assigned(OnFrameEvent) then
    raise ETransitionEffectError.Create(rsTENilFrameEvent);

  FPixelFormat   := APixelFormat;
  FExportSrc     := ExportSrc;
  FExportTgt     := ExportTgt;
  FFps           := Fps;
  FOnFrame       := OnFrameEvent;
  RenderSrcFrame := ExportSrc;
  RenderDstFrame := ExportTgt;

  if Transition = nil then
    raise ETransitionEffectError.Create(rsTEDevTrIsNil);

  try
    Initialize;

    if FFps = 0 then
      DelegateTransition.Milliseconds := 0;

    if SourceGraphic.Width  < TargetGraphic.Width
    then Data.Width  := SourceGraphic.Width
    else Data.Width  := TargetGraphic.Width;
    if SourceGraphic.Height < TargetGraphic.Height
    then Data.Height := SourceGraphic.Height
    else Data.Height := TargetGraphic.Height;

    FrameBmp := TBitmap.Create;
    FrameBmp.Canvas.Lock;
    AdjustBmpForTransition(FrameBmp, 0, Data.Width, Data.Height, PixelFormat);

    Data.DeviceCanvas        := FrameBmp.Canvas;
    Data.ExternalTiming      := True;
    Data.AlwaysShowLastFrame := False;

    SrcImage := TBitmap.Create;
    SrcImage.Canvas.Lock;
    AdjustBmpForTransition(SrcImage, 0,
      TTransitionEffectHack(DelegateTransition).GetBitmapsWidth(Data),
      Data.Height,
      TTransitionEffectHack(DelegateTransition).GetPixelFormat(Self));
    SrcImage.Canvas.Draw(0, 0, SourceGraphic);
    DstImage := TBitmap.Create;
    DstImage.Canvas.Lock;
    AdjustBmpForTransition(DstImage, 0,
      TTransitionEffectHack(DelegateTransition).GetBitmapsWidth(Data),
      Data.Height,
      TTransitionEffectHack(DelegateTransition).GetPixelFormat(Self));
    DstImage.Canvas.Draw(0, 0, TargetGraphic);
  except
    on Exception do
    begin
      UnPrepare;
      raise;
    end;
  end;
end;

procedure TTETransitionExportDevice.UnPrepare;
begin
  if Assigned(Data) then
    Finalize;
  FreeAndNil(FrameBmp);
  FreeAndNil(SrcImage);
  FreeAndNil(Pass2Image);
  FreeAndNil(DstImage);
end;

procedure TTETransitionExportDevice.TransitionInitialized;
var
  MsTotal: Integer;
begin
  inherited;

  if(Data.Pass = 1) then
  begin
    if(FFps = 0) or (DelegateTransition.Milliseconds = 0)
    then TotalFrames := Data.TotalFrames
    else
    begin
      MsTotal     := DelegateTransition.Milliseconds;
      TotalFrames := Round((FFps * MsTotal) / 1000);
      MsStep      := MsTotal / TotalFrames;
      if not FExportTgt then
        DelegateTransition.Milliseconds := MsTotal + Round(MsStep / 2);
    end;
  end;
end;

function TTETransitionExportDevice.GetExtTimingData(
  FrameRendered: Boolean): Integer;
var
  Aborted: Boolean;
begin
  Aborted := False;
  {$ifdef Trial}
  FrameBmp.Canvas.Pen.Color  := clRed;
  FrameBmp.Canvas.MoveTo(0, 0);
  FrameBmp.Canvas.LineTo(FrameBmp.Width, FrameBmp.Height);
  FrameBmp.Canvas.MoveTo(FrameBmp.Width, 0);
  FrameBmp.Canvas.LineTo(0, FrameBmp.Height);
  FrameBmp.Canvas.Font.Size  := 8;
  FrameBmp.Canvas.Font.Color := clRed;
  FrameBmp.Canvas.TextOut(0, 0, 'Billenium Effects trial version');
  {$endif Trial}
  if FFps = 0
  then
  begin
    FOnFrame(Self, FrameBmp, FrameIndex, TotalFrames, Aborted);
    if Aborted then
      Abort;
    Inc(FrameIndex);
    Result := 0;
  end
  else
  begin
    if FrameRendered
    then
    begin
      MsDone := MsDone + MsStep;
      Result := Round(MsDone);
      if MsDone <= DelegateTransition.Milliseconds then
      begin
        FOnFrame(Self, FrameBmp, FrameIndex, TotalFrames, Aborted);
        if Aborted then
          Abort;
        Inc(FrameIndex);
      end;
    end
    else
    begin
      if FExportSrc
      then Result := 0
      else if FExportTgt
      then Result := Round(MsStep)
      else Result := Round(MsStep / 2);
    end;
  end;
end;

{ TTETransitionExport }

constructor TTETransitionExport.Create(AOwner: TComponent);
begin
  inherited;

  FExportSourceFrame := True;
  FExportTargetFrame := True;
  FFps               := 0;
  FPixelFormat       := pf24bit;
  FTransition        := nil;
  FOnFrame           := nil;
end;

function TTETransitionExport.Execute(SourceGraphic,
  TargetGraphic: TGraphic): Boolean;
var
  Device: TTETransitionExportDevice;
begin
  Device := TTETransitionExportDevice.Create;
  try
    Device.Transition := FTransition;
    Device.Prepare(SourceGraphic, TargetGraphic, FPixelFormat, FFps,
      FExportSourceFrame, FExportTargetFrame, FOnFrame);
    try
      Device.Execute(True);
      Result := not Device.Aborted;
    finally
      Device.UnPrepare;
    end;
  finally
    Device.Free;
  end;
end;

function TTETransitionExport.GetVersion: String;
begin
  Result := BilleniumEffectsVersion;
end;

procedure TTETransitionExport.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if(AComponent = FTransition) and Assigned(FTransition) then
    begin
      if(FTransition = OldTransition) and Assigned(NewTransition)
      then FTransition := NewTransition
      else FTransition := nil;
    end;
  end;
end;

procedure TTETransitionExport.SetPixelFormat(const Value: TPixelFormat);
begin
  if FPixelFormat <> Value then
  begin
    if Value in [pf15bit, pf16bit, pf24bit, pf32bit]
    then FPixelFormat := Value
    else raise Exception.Create(rsTEBadPixelFormat);
  end;
end;

procedure TTETransitionExport.SetVersion(const Value: String);
begin
end;

end.
