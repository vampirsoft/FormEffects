unit tePush;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teTimed, Windows, Messages, Graphics;

type
  TPushTransition = class(TTimedTransitionEffect)
  private
  protected
    DoScroll: Boolean;

    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    function  GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;

  published
    property Direction default tedDown;
    property Pass2Options;
    property PassSetting;
    property Reversed;
  end;

implementation

constructor TPushTransition.Create(AOwner: TComponent);
begin
  inherited;

  AllowedDirections := [tedRight, tedLeft, tedDown, tedUp, tedRandom];
  Direction := tedDown;
end;

class function TPushTransition.Description: String;
begin
  Result := 'Push';
end;

procedure TPushTransition.Initialize(Data: TTETransitionData; var TotalFrames:
  Longint);
begin
  inherited;

  case DirectionToUse of
    tedDown : TotalFrames := Data.Height-1;
    tedUp   : TotalFrames := Data.Height-1;
    tedRight: TotalFrames := Data.Width -1;
    tedLeft : TotalFrames := Data.Width -1;
  end;

  DoScroll := Assigned(Data.Bitmap);
end;

procedure TPushTransition.ExecuteFrame(Data: TTETransitionData; CurrentFrame,
  Step, LastExecutedFrame: Longint);
var
  ScrollRect,
  ClipRect,
  SrcRect,
  UpdRect: TRect;
  xDesp,
  yDesp,
  DstXDesp,
  DstYDesp,
  SrcXDesp,
  SrcYDesp: Integer;
begin
  xDesp    := 0;
  yDesp    := 0;
  DstXDesp := 0;
  DstYDesp := 0;
  SrcXDesp := 0;
  SrcYDesp := 0;

  case DirectionToUse of
    tedDown:
      begin
        yDesp    := Step;
        DstYDesp := Data.Height - CurrentFrame;
        if not DoScroll
        then
        begin
          SrcRect := Rect(0, CurrentFrame, Data.Width, Data.Height );
          UpdRect := Rect(0, 0, Data.Width, CurrentFrame);
        end
        else UpdRect := Rect(0, 0, Data.Width, Step);
      end;
    tedUp:
      begin
        yDesp := -Step;
        if not DoScroll
        then
        begin
          SrcRect := Rect(0, 0, Data.Width, Data.Height - CurrentFrame);
          UpdRect := Rect(0, Data.Height - CurrentFrame, Data.Width,
            Data.Height);
          SrcYDesp   := CurrentFrame;
        end
        else
        begin
          UpdRect  := Rect(0, Data.Height - Step, Data.Width, Data.Height);
          DstYDesp := CurrentFrame;
        end;
      end;
    tedRight:
      begin
        xDesp    := Step;
        DstXDesp := Data.Width - CurrentFrame;
        if not DoScroll
        then
        begin
          SrcRect := Rect(CurrentFrame, 0, Data.Width, Data.Height);
          UpdRect := Rect(0, 0, CurrentFrame, Data.Height);
        end
        else UpdRect := Rect(0, 0, Step, Data.Height);
      end;
    tedLeft:
      begin
        xDesp := -Step;
        if not DoScroll
        then
        begin
          SrcRect := Rect(0, 0, Data.Width - CurrentFrame, Data.Height);
          UpdRect := Rect(Data.Width - CurrentFrame, 0, Data.Width,
            Data.Height);
          SrcXDesp   := CurrentFrame;
        end
        else
        begin
          UpdRect  := Rect(Data.Width - Step, 0, Data.Width, Data.Height);
          DstXDesp := CurrentFrame;
        end;
      end;
  end;

  if DoScroll
  then
  begin
    ScrollRect := Rect(0, 0, Data.Width, Data.Height);
    ClipRect   := Rect(0, 0, Data.Width, Data.Height);
    ScrollDC(Data.Canvas.Handle, xDesp, yDesp, ScrollRect, ClipRect, 0, nil);
  end
  else
    BitBlt(Data.Canvas.Handle, SrcRect.Left, SrcRect.Top,
      SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
      Data.SrcBmp.Canvas.Handle, SrcXDesp, SrcYDesp, cmSrcCopy);

  BitBlt(Data.Canvas.Handle, UpdRect.Left, UpdRect.Top,
    UpdRect.Right - UpdRect.Left, UpdRect.Bottom - UpdRect.Top,
    Data.DstBmp.Canvas.Handle, DstXDesp, DstYDesp, cmSrcCopy);
  Data.UpdateRect := Rect(0, 0, Data.Width, Data.Height);
end;

function TPushTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiMillisecondsCapable,
      tetiOffScreenBmpCapable,
      tetiThreadSafe
    ];
end;

function TPushTransition.GetPixelFormat(
  Device: TTETransitionDevice): TPixelFormat;
begin
  // Timing doesn't work fine with ScrollDC and DDBs
  Result := Device.PixelFormat;
end;

initialization

  TERegisterTransition(TPushTransition);

end.
