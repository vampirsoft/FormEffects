unit teDrip;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teChrono, teTimed, Windows, Messages, Graphics;

type
  TDripTransition = class(TTimedTransitionEffect)
  private
    BmpScanLines,
    DstScanLines: TList;
    SaveStretchBltMode: Integer;
  protected
    R, RemainingRect: TRect;

    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure Finalize(Data: TTETransitionData); override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;
  published
    property Direction default tedLeft;
    property Pass2Options;
    property PassSetting;
    property Reversed;
  end;

implementation

uses teRender;

constructor TDripTransition.Create(AOwner: TComponent);
begin
  inherited;

  AllowedDirections := [tedRight, tedLeft, tedDown, tedUp, tedRandom];
  Direction := tedLeft;
end;

class function TDripTransition.Description: String;
begin
  Result := 'Drip';
end;

procedure TDripTransition.Initialize(Data: TTETransitionData; var TotalFrames:
  Longint);
var
  i: Integer;
begin
  inherited;

  SaveStretchBltMode := SetStretchBltMode(Data.Canvas.Handle, COLORONCOLOR);
  case DirectionToUse of
    tedDown:
      begin
        TotalFrames := Data.Height-1;
        R := Rect(0, Data.Height, Data.Width, Data.Height);
      end;
    tedUp:
      begin
        TotalFrames := Data.Height-1;
        R := Rect(0, 0, Data.Width, 0);
      end;
    tedRight:
      begin
        TotalFrames := Data.Width-1;
        R := Rect(Data.Width, 0, Data.Width, Data.Height);
      end;
    tedLeft:
      begin
        TotalFrames := Data.Width-1;
        R := Rect(0, 0, 0, Data.Height);
      end;
  end;

  RemainingRect := Rect(0, 0, Data.Width, Data.Height);

  BmpScanLines := nil;
  DstScanLines := nil;

  if DirectionToUse in [tedDown, tedUp] then
  begin
    BmpScanLines := TList.Create;
    DstScanLines := TList.Create;
    BmpScanLines.Capacity := Data.Height;
    DstScanLines.Capacity := Data.Height;
    for i:=0 to Data.Height-1 do
    begin
      BmpScanLines.Add(Data.Bitmap.ScanLine[i]);
      DstScanLines.Add(Data.DstBmp.ScanLine[i]);
    end;
  end;
end;

procedure TDripTransition.Finalize(Data: TTETransitionData);
begin
  SetStretchBltMode(Data.Canvas.Handle, SaveStretchBltMode);

  FreeAndNil(BmpScanLines);
  FreeAndNil(DstScanLines);

  inherited;
end;

procedure TDripTransition.ExecuteFrame(Data: TTETransitionData; CurrentFrame,
  Step, LastExecutedFrame: Longint);
var
  SrcRect: TRect;
  i: Integer;
  SrcAux,
  DstAux: Pointer;
  ScanLineWidth: Longint;
begin
  case DirectionToUse of
    tedDown:
      begin
        R.Bottom := R.Top;
        R.Top    := R.Top - Step;
      end;
    tedUp:
      begin
        R.Top    := R.Bottom;
        R.Bottom := R.Bottom + Step;
      end;
    tedRight:
      begin
        R.Right := R.Left;
        R.Left  := R.Left - Step;
      end;
    tedLeft:
      begin
        R.Left  := R.Right;
        R.Right := R.Right + Step;
      end;
  end;

  case DirectionToUse of
    tedDown , tedUp  : SrcRect := Rect(R.Left, R.Top, R.Right , R.Top+1);
    tedRight, tedLeft: SrcRect := Rect(R.Left, R.Top, R.Left+1, R.Bottom);
  end;

  SubtractRect(RemainingRect, RemainingRect, R);
  BitBlt(Data.Canvas.Handle, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
    Data.DstBmp.Canvas.Handle, R.Left, R.Top, cmSrcCopy);
  Windows.UnionRect(Data.UpdateRect, R, Data.UpdateRect);

  case DirectionToUse of
    tedDown , tedUp  :
    begin
      ScanLineWidth := GetBytesPerScanline(Data.DstBmp, Data.PixelFormat, 32);
      SrcAux        := DstScanLines[SrcRect.Top];
      for i:=RemainingRect.Top to RemainingRect.Bottom-1 do
      begin
        DstAux := BmpScanLines[i];
        Move(SrcAux^, DstAux^, ScanLineWidth);
      end;
    end;
    tedRight, tedLeft:
      StretchBlt(Data.Canvas.Handle, RemainingRect.Left, RemainingRect.Top,
        RemainingRect.Right-RemainingRect.Left,
        RemainingRect.Bottom-RemainingRect.Top, Data.DstBmp.Canvas.Handle,
        SrcRect.Left, SrcRect.Top, SrcRect.Right-SrcRect.Left,
        SrcRect.Bottom-SrcRect.Top, cmSrcCopy);
  end;
  Windows.UnionRect(Data.UpdateRect, RemainingRect, Data.UpdateRect);
end;

function TDripTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiOffScreenBmpCapable,
      tetiStaticSrcPixels,
      tetiThreadSafe
    ] -
    [
      tetiNeedSrcBmp
    ];
  if DirectionToUse in [tedDown, tedUp] then
    Include(Result, tetiNeedOffScreenBmp);
end;

initialization

  TERegisterTransition(TDripTransition);

end.
