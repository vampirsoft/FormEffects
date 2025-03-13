unit teBkgrnd;

interface

{$INCLUDE teDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  {$ifndef NoVCL}
  Forms, Controls,
  {$endif NoVCL}
  teRender;

{$ifndef NoVCL}
{$ifndef TE_NOHLP}
const
  CM_TEBASE            = CM_BASE   + 532;
  CM_TEGETBKGNDOPTIONS = CM_TEBASE +   0;
{$endif TE_NOHLP}
{$endif NoVCL}

type
  TFCPictureMode = (fcpmCenter, fcpmCenterStretch, fcpmStretch, fcpmTile,
    fcpmZoom, fcpmTopLeft, fcpmTopRight, fcpmBottomLeft, fcpmBottomRight);
  TFCTranslucency = 0..255;

  {$ifndef NoVCL}
  TFCBackgroundOptions = class(TPersistent)
  private
    FControl: TControl;
    FChildBkOptions: TList;
    FParent: TFCBackgroundOptions;
    FOpaque,
    FParentOpaque,
    FParentPicture: Boolean;
    FPicture: TPicture;
    FPictureVisible: Boolean;
    FPictureMode: TFCPictureMode;
    FPictureTranspColor: TColor;
    FParentBkgrndForm: Boolean;
    FBkgrndForm: TCustomForm;
    FBkgrndFormVisible: Boolean;
    FParentGlass: Boolean;
    FGlassColor: TColor;
    FGlassTranslucency: TFCTranslucency;
    FGlassVisible,
    OpaqueActive,
    BkFormActive,
    GlassActive,
    PictureActive,
    FThemesDisabled: Boolean;
    
    FOnChange: TNotifyEvent;

    function GetChildBkOptions(Index: Integer): TFCBackgroundOptions;

    procedure Insert(Child: TFCBackgroundOptions);
    procedure Remove(Child: TFCBackgroundOptions);
    procedure SetControl(const Value: TControl);

    function  GetOpaque: Boolean;
    procedure SetOpaque(Value: Boolean);
    function  GetParentOpaque: Boolean;
    procedure SetParentOpaque(const Value: Boolean);
    function  IsOpaqueActive: Boolean;
    function  IsBkFormActive: Boolean;
    function  IsGlassActive: Boolean;
    function  XRayActive(PictureBkOptions: TFCBackgroundOptions;
      R: TRect): Boolean;
    function  IsPictureActive: Boolean;
    function  GetParentPicture: TFCBackgroundOptions;
    procedure SetParentPicture(const Value: Boolean);
    function  IsPictureStored: Boolean;
    function  GetPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
    procedure SetPictureVisible(Value: Boolean);
    function  GetPictureMode: TFCPictureMode;
    procedure SetPictureMode(Value: TFCPictureMode);
    function  GetPictureTranspColor: TColor;
    procedure SetPictureTranspColor(Value: TColor);

    function  GetParentBkgrndForm: TFCBackgroundOptions;
    procedure SetParentBkgrndForm(const Value: Boolean);
    function  GetBkgrndForm: TCustomForm;
    procedure SetBkgrndFormVisible(Value: Boolean);

    function  GetParentGlass: TFCBackgroundOptions;
    procedure SetParentGlass(const Value: Boolean);
    function  IsGlassStored: Boolean;
    function  GetGlassColor: TColor;
    procedure SetGlassColor(const Value: TColor);
    function  GetGlassTranslucency: TFCTranslucency;
    procedure SetGlassTranslucency(const Value: TFCTranslucency);
    procedure SetGlassVisible(Value: Boolean);
    {$ifdef D7UP}
    procedure SetThemesDisabled(const Value: Boolean);
    {$endif D7UP}
  protected
    procedure Changed;
    procedure OpaqueChanged(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure PicChanged(Sender: TObject; Propagate: Boolean);
    procedure BkgrndFormChanged(Sender: TObject; Propagate: Boolean);
    procedure GlassChanged(Sender: TObject; Propagate: Boolean);
    function  GlassTranslucencyToUse: TFCTranslucency;

    property ChildBkOptions[Index: Integer]: TFCBackgroundOptions read GetChildBkOptions;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure ControlChanged(Sender: TObject);
    procedure DrawBackGround(DC: HDC; DstBmp: TBitmap;
      R: TRect);
    function  GetPalette: HPalette;
    procedure SetBkgrndForm(Value: TCustomFormClass);
    function  IsActive: Boolean;

    property BkgrndForm: TCustomForm read GetBkgrndForm;
    property Control: TControl read FControl write SetControl;
    property Parent: TFCBackgroundOptions read FParent;
  published
    property Opaque: Boolean read GetOpaque write SetOpaque default True;
    property ParentOpaque: Boolean read FParentOpaque write SetParentOpaque default False;
    property BkgrndFormVisible: Boolean read FBkgrndFormVisible write SetBkgrndFormVisible default True;
    property ParentBkgrndForm: Boolean read FParentBkgrndForm write SetParentBkgrndForm default False;
    property ParentPicture: Boolean read FParentPicture write SetParentPicture default False;
    property Picture: TPicture read GetPicture write SetPicture stored IsPictureStored;
    property PictureMode: TFCPictureMode read GetPictureMode write SetPictureMode stored IsPictureStored default fcpmTile;
    property PictureTranspColor: TColor read GetPictureTranspColor write SetPictureTranspColor stored IsPictureStored default clNone;
    property PictureVisible: Boolean read FPictureVisible write SetPictureVisible default True;
    property GlassColor: TColor read GetGlassColor write SetGlassColor stored IsGlassStored default clBlack;
    property GlassTranslucency: TFCTranslucency read GetGlassTranslucency write SetGlassTranslucency stored IsGlassStored default 255;
    property GlassVisible: Boolean read FGlassVisible write SetGlassVisible default True;
    property ParentGlass: Boolean read FParentGlass write SetParentGlass default False;
    {$ifdef D7UP}
    property ThemesDisabled: Boolean read FThemesDisabled write SetThemesDisabled default False;
    {$endif D7UP}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$ifdef D7UP}
  function BEParentBackgroundPainted(Handle: HWND): Boolean;
  {$endif D7UP}
  function PictureRect(Pic: TGraphic; PictureMode: TFCPictureMode; Margin: Word;
    CtrlThis: TControl; CtrlOrg: TWinControl; var DrawRect: TRect): TRect;
  procedure DrawPicture(Pic: TGraphic; PictureMode: TFCPictureMode;
    PictureTranspColor: TColor; PicCtrl: TWinControl; Bmp: TBitmap; R: TRect;
    Margin: Word; Ctrl: TControl); overload;
  {$endif NoVCL}

  {$ifndef TE_NOHLP}
  procedure DrawPicture(Pic: TGraphic; PictureMode: TFCPictureMode;
    PictureTranspColor: TColor; Bmp: TBitmap; R, PicRect, DrawRect: TRect;
    Margin: Word); overload;
  function TEGetPictureModeDesc(PictureMode: TFCPictureMode): String;
  function TEGetPictureModeFromDesc(Description: String): TFCPictureMode;
  {$endif TE_NOHLP}

implementation

uses
  {$ifndef NoVCL}
  {$ifdef D7UP}Themes, UxTheme, {$endif D7UP}
  TypInfo, 
  {$endif NoVCL}
  teBlndWk;

{$ifndef NoVCL}
type
  TFCControl = class(TControl);
  TFCCustomForm = class(TCustomForm);

{$ifdef D7UP}
var
  BEDrawParentBackgroundList: TList = nil;
{$endif D7UP}

function PictureRect(Pic: TGraphic; PictureMode: TFCPictureMode; Margin: Word;
  CtrlThis: TControl; CtrlOrg: TWinControl; var DrawRect: TRect): TRect;
var
  MaxWidth,
  MaxHeight,
  ClientWidth,
  ClientHeight,
  OffsetH,
  OffsetV: Integer;
  ScrollInfoH,
  ScrollInfoV: TScrollInfo;
  CtrlOrgHandle,
  CtrlThisHandle: HWND;
  IsMDIClient,
  ScrollBarVisible: Boolean;
begin
  IsMDIClient    :=
    (CtrlOrg is TCustomForm) and
    (TFCCustomForm(CtrlOrg).FormStyle = fsMDIForm);
  CtrlThisHandle := TWinControl(CtrlThis).Handle;
  if not IsMDIClient
  then CtrlOrgHandle := CtrlOrg.Handle
  else
  begin
    CtrlOrgHandle  := TFCCustomForm(CtrlOrg).ClientHandle;
    if CtrlThis = CtrlOrg then
      CtrlThisHandle := CtrlOrgHandle;
  end;
  GetClientRect(CtrlOrgHandle, DrawRect);
  ClientWidth  := DrawRect.Right  - DrawRect.Left;
  ClientHeight := DrawRect.Bottom - DrawRect.Top;

  if(Pic = nil) or (Pic.Width = 0) or (Pic.Height = 0)
  then Result := Rect(0, 0, 0, 0)
  else
  begin
    OffsetH := 0;
    OffsetV := 0;

    if not IsMDIClient
    then ScrollBarVisible := IsScrollBarVisible(CtrlOrg, CtrlOrgHandle, sbHorizontal)
    else ScrollBarVisible := IsScrollBarVisible(nil    , CtrlOrgHandle, sbHorizontal);
    if ScrollBarVisible then
    begin
      ScrollInfoH.cbSize := SizeOf(ScrollInfoH);
      ScrollInfoH.fMask  := SIF_ALL;
      GetScrollInfo(CtrlOrgHandle, SB_HORZ, ScrollInfoH);
      OffsetH := ScrollInfoH.nPos;
    end;

    if not IsMDIClient
    then ScrollBarVisible := IsScrollBarVisible(CtrlOrg, CtrlOrgHandle, sbVertical)
    else ScrollBarVisible := IsScrollBarVisible(nil    , CtrlOrgHandle, sbVertical);
    if ScrollBarVisible then
    begin
      ScrollInfoV.cbSize := SizeOf(ScrollInfoV);
      ScrollInfoV.fMask  := SIF_ALL;
      GetScrollInfo(CtrlOrgHandle, SB_VERT, ScrollInfoV);
      OffsetV := ScrollInfoV.nPos;
    end;

    DrawRect   := Rect(0, 0, ClientWidth, ClientHeight);
    InflateRect(DrawRect, -Margin, -Margin);
    ClientWidth  := DrawRect.Right  - DrawRect.Left;
    ClientHeight := DrawRect.Bottom - DrawRect.Top;

    case PictureMode of
      fcpmCenter:
      begin
        Result :=
          Rect(
            ((ClientWidth  - Pic.Width ) DIV 2) + Margin,
            ((ClientHeight - Pic.Height) DIV 2) + Margin,
            ((ClientWidth  - Pic.Width ) DIV 2) + Margin + Pic.Width,
            ((ClientHeight - Pic.Height) DIV 2) + Margin + Pic.Height);
      end;
      fcpmCenterStretch:
      begin
        if(ClientWidth / ClientHeight) > (Pic.Width / Pic.Height)
        then
        begin
          MaxHeight := ClientHeight;
          MaxWidth  := (Pic.Width * MaxHeight) DIV Pic.Height;
        end
        else
        begin
          MaxWidth  := ClientWidth;
          MaxHeight := (Pic.Height * MaxWidth) DIV Pic.Width;
        end;
        Result.Left   := ((ClientWidth  - MaxWidth ) DIV 2) + Margin;
        Result.Top    := ((ClientHeight - MaxHeight) DIV 2) + Margin;
        Result.Right  := (Result.Left + MaxWidth );
        Result.Bottom := (Result.Top  + MaxHeight);
      end;
      fcpmStretch,
      fcpmTile,
      fcpmZoom:
      begin
        Result := DrawRect;
      end;
      fcpmTopLeft:
      begin
        IntersectRect(
          Result,
          DrawRect,
          Rect(Margin, Margin, Pic.Width + Margin, Pic.Height + Margin));
      end;
      fcpmTopRight:
      begin
        Result.Left   := ClientWidth - Pic.Width - Margin;
        Result.Top    := Margin;
        Result.Right  := ClientWidth - Margin;
        Result.Bottom := Pic.Height + Margin;
      end;
      fcpmBottomLeft:
      begin
        Result.Left   := Margin;
        Result.Top    := ClientHeight - Pic.Height - Margin;
        Result.Right  := Pic.Width + Margin;
        Result.Bottom := ClientHeight - Margin;
      end;
      fcpmBottomRight:
      begin
        Result.Left   := ClientWidth - Pic.Width - Margin;
        Result.Top    := ClientHeight - Pic.Height - Margin;
        Result.Right  := ClientWidth - Margin;
        Result.Bottom := ClientHeight - Margin;
      end;
    end;

    if CtrlOrgHandle <> CtrlThisHandle then
    begin
      ClientToScreen(CtrlOrgHandle , Result.TopLeft);
      ClientToScreen(CtrlOrgHandle , Result.BottomRight);
      ScreenToClient(CtrlThisHandle, Result.TopLeft);
      ScreenToClient(CtrlThisHandle, Result.BottomRight);
      OffsetRect(Result, -OffsetH, -OffsetV);

      if IsScrollBarVisible(CtrlThis, CtrlThisHandle, sbHorizontal) then
      begin
        ScrollInfoH.cbSize := SizeOf(ScrollInfoH);
        ScrollInfoH.fMask  := SIF_POS;
        GetScrollInfo(CtrlThisHandle, SB_HORZ, ScrollInfoH);
        OffsetRect(Result, ScrollInfoH.nPos, 0);
      end;
      if IsScrollBarVisible(CtrlThis, CtrlThisHandle, sbVertical) then
      begin
        ScrollInfoV.cbSize := SizeOf(ScrollInfoV);
        ScrollInfoV.fMask  := SIF_POS;
        GetScrollInfo(CtrlThisHandle, SB_VERT, ScrollInfoV);
        OffsetRect(Result, 0, ScrollInfoV.nPos);
      end;
    end;
  end;
end;

procedure DrawPicture(Pic: TGraphic; PictureMode: TFCPictureMode;
  PictureTranspColor: TColor; PicCtrl: TWinControl; Bmp: TBitmap; R: TRect;
  Margin: Word; Ctrl: TControl);
var
  aux,
  DrawRect,
  PicRect: TRect;
begin
  if Assigned(Ctrl) and Assigned(PicCtrl)
  then
  begin
    PicRect := PictureRect(Pic, PictureMode, Margin, Ctrl, PicCtrl, DrawRect);

    IntersectRect(aux, DrawRect, R);
    if IsRectEmpty(aux) then
      exit;
  end
  else
  begin
    PicRect  := R;
    DrawRect := R;
    InflateRect(DrawRect, -Margin, -Margin);
  end;

  DrawPicture(Pic, PictureMode, PictureTranspColor, Bmp, R, PicRect, DrawRect, Margin);
end;

{$endif NoVCL}

procedure DrawPicture(Pic: TGraphic; PictureMode: TFCPictureMode;
  PictureTranspColor: TColor; Bmp: TBitmap; R, PicRect, DrawRect: TRect; Margin: Word);

  procedure TileBitmap(Pic: TGraphic; Bmp: TBitmap; PicRect, R: TRect);
  var
    TileStart: TPoint;
    i,
    j,
    Cols,
    Rows,
    xPos,
    yPos: Integer;
  begin
    TileStart.X := R.Left - ((R.Left - PicRect.Left) mod Pic.Width );
    TileStart.Y := R.Top  - ((R.Top  - PicRect.Top ) mod Pic.Height);
    Cols        := (R.Right  - TileStart.X) div Pic.Width;
    if (R.Right  - TileStart.X) mod Pic.Width  <> 0 then
      Inc(Cols);
    Rows        := (R.Bottom - TileStart.Y) div Pic.Height;
    if (R.Bottom - TileStart.Y) mod Pic.Height <> 0 then
      Inc(Rows);

    xPos := TileStart.X;
    for i := 0 to Cols-1 do
    begin
      yPos := TileStart.Y;
      for j := 0 to Rows-1 do
      begin
        Bmp.Canvas.Draw(xPos, yPos, Pic);
        Inc(yPos, Pic.Height);
      end;
      Inc(xPos, Pic.Width);
    end;
  end;

  procedure ZoomBitmap(Pic: TGraphic; Bmp: TBitmap; PicRect, R: TRect);

    function Ceil(const X: Extended): Integer;
    begin
      Result := Integer(Trunc(X));
      if Frac(X) > 0 then
        Inc(Result);
    end;

  var
    FullPicRect: TRect;
    Ratio,
    ZoomLevel: Double;
    NewSize,
    aux: Integer;
  begin
    FullPicRect := PicRect;
    Ratio := Pic.Width / Pic.Height;
    if Ratio > ((PicRect.Right - PicRect.Left) / (PicRect.Bottom - PicRect.Top))
    then // Zoomed picture is wider than the target canvas
    begin
      ZoomLevel := (PicRect.Bottom - PicRect.Top) / Pic.Height;
      NewSize   := Ceil(Pic.Width * ZoomLevel);
      aux       := (NewSize - (FullPicRect.Right - FullPicRect.Left)) div 2;
      FullPicRect.Left  := FullPicRect.Left - aux;
      FullPicRect.Right :=
        FullPicRect.Right + (NewSize - (FullPicRect.Right - FullPicRect.Left));
    end
    else // Zoomed picture is taller than the target canvas
    begin
      ZoomLevel := (PicRect.Right - PicRect.Left) / Pic.Width;
      NewSize   := Ceil(Pic.Height * ZoomLevel);
      aux       := (NewSize - (FullPicRect.Bottom - FullPicRect.Top)) div 2;
      FullPicRect.Top    := FullPicRect.Top - aux;
      FullPicRect.Bottom :=
        FullPicRect.Bottom + (NewSize - (FullPicRect.Bottom - FullPicRect.Top));
    end;
    Bmp.Canvas.StretchDraw(FullPicRect, Pic);
  end;

var
  SaveClipRgn: HRgn;
  UseClipRgn,
  ExistsClipRgn: Boolean;
begin
  if IsRectEmpty(PicRect) then
    exit;

  if PictureTranspColor = clNone
  then Pic.Transparent := False
  else
  begin
    Pic.Transparent := True;
    if Pic is TBitmap then
      TBitmap(Pic).TransparentColor := PictureTranspColor;
  end;

  SaveClipRgn   := 0;
  ExistsClipRgn := False;
  UseClipRgn :=
    (Margin <> 0) and
    (PictureMode in [fcpmCenter, fcpmTile, fcpmZoom, fcpmTopLeft, fcpmTopRight,
      fcpmBottomLeft, fcpmBottomRight]);
  if UseClipRgn then
  begin
    // Remember current clipping region
    SaveClipRgn   := CreateRectRgn(0,0,0,0);
    ExistsClipRgn := GetClipRgn(Bmp.Canvas.Handle, SaveClipRgn) = 1;
  end;
  try
    if UseClipRgn then
    begin
      IntersectClipRect(Bmp.Canvas.Handle, DrawRect.Left, DrawRect.Top,
        DrawRect.Right, DrawRect.Bottom);
    end;

    case PictureMode of
      fcpmCenter,
      fcpmTopLeft,
      fcpmTopRight,
      fcpmBottomLeft,
      fcpmBottomRight  : Bmp.Canvas.Draw(PicRect.Left, PicRect.Top, Pic);
      fcpmCenterStretch: Bmp.Canvas.StretchDraw(PicRect, Pic);
      fcpmStretch      : Bmp.Canvas.StretchDraw(PicRect, Pic);
      fcpmTile         : TileBitmap(Pic, Bmp, PicRect, R);
      fcpmZoom         : ZoomBitmap(Pic, Bmp, PicRect, R);
    end;
  finally
    if UseClipRgn then
    begin
      if ExistsClipRgn
      then SelectClipRgn(Bmp.Canvas.Handle, SaveClipRgn)
      else SelectClipRgn(Bmp.Canvas.Handle, 0);
      DeleteObject(SaveClipRgn);
    end;
  end;
end;

function TEGetPictureModeDesc(PictureMode: TFCPictureMode): String;
begin
  Result := '';
  case PictureMode of
    fcpmCenter       : Result := 'Center';
    fcpmCenterStretch: Result := 'Center stretch';
    fcpmStretch      : Result := 'Stretch';
    fcpmTile         : Result := 'Tile';
    fcpmZoom         : Result := 'Zoom';
    fcpmTopLeft      : Result := 'Top left';
    fcpmTopRight     : Result := 'Top right';
    fcpmBottomLeft   : Result := 'Bottom left';
    fcpmBottomRight  : Result := 'Bottom right';
  end;
end;

function TEGetPictureModeFromDesc(Description: String): TFCPictureMode;
var
  i: Integer;
begin
  Result := fcpmCenter;
  for i := 0 to Integer(High(TFCPictureMode)) do
  begin
    if TEGetPictureModeDesc(TFCPictureMode(i)) = Description then
    begin
      Result := TFCPictureMode(i);
      break;
    end;
  end;
end;

{$ifndef NoVCL}
{ TFCBackgroundOptions }

constructor TFCBackgroundOptions.Create;
begin
  inherited Create;

  FChildBkOptions     := TList.Create;
  FOpaque             := True;
  FParentOpaque       := False;
  FParentBkgrndForm   := False;
  FBkgrndForm         := nil;
  FBkgrndFormVisible  := True;
  FParentPicture      := False;
  FPicture            := TPicture.Create;
  FPicture.OnChange   := PictureChanged;
  FPictureVisible     := True;
  FPictureMode        := fcpmTile;
  FPictureTranspColor := clNone;
  FParentGlass        := False;
  FGlassColor         := clBlack;
  FGlassTranslucency  := 255;
  FGlassVisible       := True;
  FThemesDisabled     := False;
end;

destructor TFCBackgroundOptions.Destroy;
begin
  if Assigned(Control)                            and
    (not(csDestroying in Control.ComponentState)) and
    IsActive then
    Changed;
    
  if Parent <> nil then
    Parent.Remove(Self);

  FPicture.Free;

  while FChildBkOptions.Count > 0 do
    Remove(TFCBackgroundOptions(FChildBkOptions[0]));
  FChildBkOptions.Free;

  inherited;
end;

procedure TFCBackgroundOptions.Assign(Source: TPersistent);
var
  aux: TFCBackgroundOptions;
begin
  if Source is TFCBackgroundOptions
  then
  begin
    aux := (Source as TFCBackgroundOptions);

    ParentOpaque := aux.ParentOpaque;
    if not ParentOpaque then
      Opaque := aux.Opaque;

    PictureVisible := aux.PictureVisible;
    ParentPicture  := aux.ParentPicture;
    if not ParentPicture then
    begin
      Picture.Assign(aux.Picture);
      PictureMode        := aux.PictureMode;
      PictureTranspColor := aux.PictureTranspColor;
    end;

    BkgrndFormVisible := aux.BkgrndFormVisible;
    ParentBkgrndForm  := aux.ParentBkgrndForm;
    if not ParentBkgrndForm then
    begin
      if Assigned(aux.BkgrndForm) then
        SetBkgrndForm(TCustomFormClass(aux.FBkgrndForm.ClassType));
    end;

    GlassVisible := aux.GlassVisible;
    ParentGlass  := aux.ParentGlass;
    if not ParentGlass then
    begin
      GlassColor        := aux.GlassColor;
      GlassTranslucency := aux.GlassTranslucency;
    end;
  end
  else inherited Assign(Source);
end;

procedure TFCBackgroundOptions.Insert(Child: TFCBackgroundOptions);
begin
  if Child <> nil then
  begin
    if Child.Parent <> nil then
      Child.Parent.Remove(Child);

    FChildBkOptions.Add(Child);
    Child.FParent := Self;
  end;
end;

procedure TFCBackgroundOptions.Remove(Child: TFCBackgroundOptions);
begin
  FChildBkOptions.Remove(Child);
  Child.FParent := nil;
end;

procedure TFCBackgroundOptions.Changed;

  procedure DoPaletteChange;
  var
    ParentForm: TCustomForm;
    Tmp: TGraphic;
  begin
    Tmp := Picture.Graphic;
    if IsPictureActive                            and
      (not (csLoading in Control.ComponentState)) and
      (Tmp <> nil)                                and
      (Tmp.PaletteModified)                       then
    begin
      if Tmp.Palette = 0
      then Tmp.PaletteModified := False
      else
      begin
        ParentForm := GetParentForm(Control);
        if Assigned(ParentForm) and ParentForm.Active and ParentForm.HandleAllocated then
        begin
          SendMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
          Tmp.PaletteModified := False;
        end;
      end;
    end;
  end;

begin
  if Assigned(Control) then
  begin
    DoPaletteChange;
    Control.Invalidate;
  end;
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TFCBackgroundOptions.GetPalette: HPalette;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
    Result := FPicture.Graphic.Palette;
end;

function TFCBackgroundOptions.GetChildBkOptions(
  Index: Integer): TFCBackgroundOptions;
begin
   Result := TFCBackgroundOptions(FChildBkOptions.Items[Index]);
end;

procedure TFCBackgroundOptions.SetControl(const Value: TControl);

  function GetParentBkOptions(Ctrl: TControl): TFCBackgroundOptions;
  type
    TFCGetBackgroundOptions = function: TFCBackgroundOptions of object;
  var
    Info: PPropInfo;
    CtrlParent: TWinControl;
  begin
    Result := nil;
    if Ctrl <> nil
    then
    begin
      if Ctrl.Parent <> nil
      then CtrlParent := Ctrl.Parent
      else
      begin
        CtrlParent := FindControl((Ctrl as TWinControl).ParentWindow);
        if CtrlParent = nil then
        begin
          if(Ctrl is TForm) and (TForm(Ctrl).FormStyle = fsMDIChild) then
            CtrlParent := Application.MainForm;
        end;
      end;
    end
    else CtrlParent := nil;

    if CtrlParent <> nil then
    begin
      Info := GetPropInfo(CtrlParent.ClassInfo, 'BackgroundOptions');
      if(Info <> nil) and (Info^.PropType^^.Name <> 'TFCBackgroundOptions') then
        Info := nil;

      if Info = nil
      then
      begin
        Result := TFCBackgroundOptions(
          CtrlParent.Perform(CM_TEGETBKGNDOPTIONS, 0, 0));
      end
      else Result := TFCBackgroundOptions(GetOrdProp(CtrlParent, Info));

      if Result = nil then
        Result := GetParentBkOptions(CtrlParent);
    end;
  end;

  procedure CheckChildBkOptions(Ctrl: TWinControl);
  type
    TFCGetBackgroundOptions = function: TFCBackgroundOptions of object;
  var
    Info: PPropInfo;
    i: Integer;
    Child: TControl;
    BkOptions: TFCBackgroundOptions;
  begin
    for i:=0 to Ctrl.ControlCount-1 do
    begin
      Child := Ctrl.Controls[i];
      Info := GetPropInfo(Child.ClassInfo, 'BackgroundOptions');

      if Info = nil
      then
      begin
        if Child is TWinControl
        then BkOptions := TFCBackgroundOptions(
               (Child as TWinControl).Perform(CM_TEGETBKGNDOPTIONS, 0, 0))
        else BkOptions := TFCBackgroundOptions(
               Child.Perform(CM_TEGETBKGNDOPTIONS, 0, 0));
      end
      else
      begin
        BkOptions := TFCBackgroundOptions(GetOrdProp(Child, Info));
      end;

      if BkOptions <> nil
      then Insert(BkOptions)
      else
      begin
        if Child is TWinControl then
          CheckChildBkOptions(TWinControl(Child));
      end;
    end;
  end;

var
  ParentBkOptions: TFCBackgroundOptions;
begin
  if FControl <> Value then
  begin
    if Parent <> nil then
      Parent.Remove(Self);
    FControl := Value;
    ParentBkOptions := GetParentBkOptions(FControl);
    if ParentBkOptions <> nil then
      ParentBkOptions.Insert(Self);
    if FControl is TWinControl then
    begin
      while FChildBkOptions.Count > 0 do
        Remove(ChildBkOptions[0]);
      CheckChildBkOptions(TWinControl(FControl));
    end;
  end;
end;

function TFCBackgroundOptions.GetOpaque: Boolean;
begin
  Result := GetParentOpaque;
end;

procedure TFCBackgroundOptions.SetOpaque(Value: Boolean);
begin
  FParentOpaque := False;
  
  if FOpaque <> Value then
  begin
    FOpaque := Value;
    if Assigned(Control) then
      Control.Invalidate;
  end;
end;

function TFCBackgroundOptions.GetParentOpaque: Boolean;
begin
  Result := FOpaque;
  if ParentOpaque                      and
     Assigned(Parent)                  and
     Assigned(Control)                 and
     (Parent.Control = Control.Parent) then
    Result := Parent.GetParentOpaque;
end;

procedure TFCBackgroundOptions.SetParentOpaque(const Value: Boolean);
begin
  if FParentOpaque <> Value then
  begin
    FParentOpaque := Value;
    OpaqueChanged(Self);
  end;
end;

function TFCBackgroundOptions.GetParentPicture: TFCBackgroundOptions;
begin
  Result := Self;
  if ParentPicture and Assigned(Parent) then
    Result := Parent.GetParentPicture;
end;

function TFCBackgroundOptions.GetParentBkgrndForm: TFCBackgroundOptions;
begin
  Result := Self;
  if ParentBkgrndForm and Assigned(Parent) then
    Result := Parent.GetParentBkgrndForm;
end;

function TFCBackgroundOptions.GetParentGlass: TFCBackgroundOptions;
begin
  Result := Self;
  if ParentGlass and Assigned(Parent) then
    Result := Parent.GetParentGlass;
end;

procedure TFCBackgroundOptions.SetParentPicture(const Value: Boolean);
begin
  if FParentPicture <> Value then
  begin
    if Value then
      Picture.Graphic := nil;

    FParentPicture := Value;

    PicChanged(Self, True);
  end;
end;

procedure TFCBackgroundOptions.SetParentBkgrndForm(const Value: Boolean);
begin
  if FParentBkgrndForm <> Value then
  begin
    if Value then
    begin
      FBkgrndForm.Free;
      FBkgrndForm := nil;
    end;

    FParentBkgrndForm := Value;

    BkgrndFormChanged(Self, True);
  end;
end;

procedure TFCBackgroundOptions.SetParentGlass(const Value: Boolean);
begin
  if FParentGlass <> Value then
  begin
    FParentGlass := Value;

    GlassChanged(Self, True);
  end;
end;

function TFCBackgroundOptions.GetPicture: TPicture;
begin
  Result := GetParentPicture.FPicture;
end;

function TFCBackgroundOptions.GetBkgrndForm: TCustomForm;
begin
  Result := GetParentBkgrndForm.FBkgrndForm;
end;

function TFCBackgroundOptions.GetGlassColor: TColor;
begin
  Result := GetParentGlass.FGlassColor;
end;

function TFCBackgroundOptions.GetGlassTranslucency: TFCTranslucency;
begin
  Result := GetParentGlass.FGlassTranslucency;
end;

procedure TFCBackgroundOptions.SetPicture(const Value: TPicture);
begin
  FParentPicture := False;
  FPicture.Assign(Value);
end;

procedure TFCBackgroundOptions.SetBkgrndForm(Value: TCustomFormClass);
begin
  FParentBkgrndForm := False;
  FBkgrndForm.Free;
  FBkgrndForm := nil;
  if Value <> nil then
  begin
    FBkgrndForm := Value.Create(Control);
    TFCCustomForm(FBkgrndForm).BorderStyle := bsNone;
    FBkgrndForm.Left := 0;
    FBkgrndForm.Top  := 0;
    FBkgrndForm.SetBounds(-1, -1, Control.ClientWidth, Control.ClientHeight);
    SetWindowRgn(FBkgrndForm.Handle, CreateRectRgn(0, 0, 1, 1), False);
    ShowWindow(FBkgrndForm.Handle, SW_SHOWNOACTIVATE);
    FBkgrndForm.Visible := True;
  end;

  BkgrndFormChanged(Self, True);
end;

function TFCBackgroundOptions.GlassTranslucencyToUse: TFCTranslucency;
begin
  Result := GlassTranslucency;
end;

procedure TFCBackgroundOptions.SetGlassColor(const Value: TColor);
begin
  FParentGlass := False;
  FGlassColor  := Value;
  GlassChanged(Self, True);
end;

procedure TFCBackgroundOptions.SetGlassTranslucency(const Value: TFCTranslucency);
begin
  FParentGlass       := False;
  FGlassTranslucency := Value;
  GlassChanged(Self, True);
end;

function TFCBackgroundOptions.IsPictureStored: Boolean;
begin
  Result := not ParentPicture;
end;

function TFCBackgroundOptions.IsGlassStored: Boolean;
begin
  Result := not ParentGlass;
end;

procedure TFCBackgroundOptions.SetPictureVisible(Value: Boolean);
begin
  if FPictureVisible <> Value then
  begin
    FPictureVisible := Value;
    PicChanged(Self, False);
  end;
end;

procedure TFCBackgroundOptions.SetBkgrndFormVisible(Value: Boolean);
begin
  if FBkgrndFormVisible <> Value then
  begin
    FBkgrndFormVisible := Value;
    BkgrndFormChanged(Self, False);
  end;
end;

procedure TFCBackgroundOptions.SetGlassVisible(Value: Boolean);
begin
  if FGlassVisible <> Value then
  begin
    FGlassVisible := Value;
    GlassChanged(Self, False);
  end;
end;

function TFCBackgroundOptions.GetPictureMode: TFCPictureMode;
begin
  Result := GetParentPicture.FPictureMode;
end;

procedure TFCBackgroundOptions.SetPictureMode(Value: TFCPictureMode);
begin
  if FPictureMode <> Value then
  begin
    FPictureMode := Value;
    PicChanged(Self, True);
  end;
end;

function TFCBackgroundOptions.GetPictureTranspColor: TColor;
begin
  Result := GetParentPicture.FPictureTranspColor;
end;

procedure TFCBackgroundOptions.SetPictureTranspColor(Value: TColor);
begin
  if FPictureTranspColor <> Value then
  begin
    FPictureTranspColor := Value;
    PicChanged(Self, True);
  end;
end;

procedure TFCBackgroundOptions.OpaqueChanged(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to FChildBkOptions.Count-1 do
  begin
    if TFCBackgroundOptions(ChildBkOptions[i]).ParentOpaque then
      TFCBackgroundOptions(ChildBkOptions[i]).OpaqueChanged(Self);
  end;

  Changed;
end;

procedure TFCBackgroundOptions.PictureChanged(Sender: TObject);
begin
  PicChanged(Sender, True);
end;

procedure TFCBackgroundOptions.PicChanged(Sender: TObject; Propagate: Boolean);
var
  i: Integer;
begin
  if Propagate then
  begin
    for i:= 0 to FChildBkOptions.Count-1 do
    begin
      if TFCBackgroundOptions(ChildBkOptions[i]).ParentPicture then
        TFCBackgroundOptions(ChildBkOptions[i]).PicChanged(Self, True);
    end;
  end;

  Changed;
end;

procedure TFCBackgroundOptions.BkgrndFormChanged(Sender: TObject;
  Propagate: Boolean);
var
  i: Integer;
begin
  if Propagate then
  begin
    for i:= 0 to FChildBkOptions.Count-1 do
    begin
      if TFCBackgroundOptions(ChildBkOptions[i]).ParentBkgrndForm then
        TFCBackgroundOptions(ChildBkOptions[i]).BkgrndFormChanged(Self, True);
    end;
  end;

  Changed;
end;

procedure TFCBackgroundOptions.GlassChanged(Sender: TObject;
  Propagate: Boolean);
var
  i: Integer;
begin
  if Propagate then
  begin
    for i:= 0 to FChildBkOptions.Count-1 do
    begin
      if TFCBackgroundOptions(ChildBkOptions[i]).ParentGlass then
        TFCBackgroundOptions(ChildBkOptions[i]).GlassChanged(Self, True);
    end;
  end;

  Changed;
end;

function TFCBackgroundOptions.IsOpaqueActive: Boolean;
begin
  Result := Opaque;
end;

function TFCBackgroundOptions.IsPictureActive: Boolean;
var
  Pic: TPicture;
begin
  Result := PictureVisible;
  if Result then
  begin
    Pic := Picture;
    Result :=
      (Pic.Graphic <> nil)     and
      (not Pic.Graphic.Empty)  and
      (Pic.Graphic.Width  > 0) and
      (Pic.Graphic.Height > 0);
  end;
end;

function TFCBackgroundOptions.IsBkFormActive: Boolean;
begin
  Result := BkgrndFormVisible and (BkgrndForm <> nil);
end;

function TFCBackgroundOptions.IsGlassActive: Boolean;
begin
  Result := GlassVisible and (GlassTranslucencyToUse < 255);
end;

function TFCBackgroundOptions.XRayActive(
  PictureBkOptions: TFCBackgroundOptions; R: TRect): Boolean;

  function CoveredByPic: Boolean;
  var
    aux,
    PicRect: TRect;
  begin
    Result := PictureActive and (PictureTranspColor = clNone);
    if Result then
    begin
      PicRect := PictureRect(PictureBkOptions.Picture.Graphic,
        PictureBkOptions.PictureMode, 0, Control,
        TWinControl(PictureBkOptions.Control), aux);
      UnionRect(aux, PicRect, R);
      Result  := EqualRect(PicRect, aux);
    end;
  end;

begin
  Result :=
    Assigned(Control.Parent) and
    IsActive                 and
    (not OpaqueActive)       and
    (not BkFormActive)       and
    (not CoveredByPic);
end;

function TFCBackgroundOptions.IsActive: Boolean;
begin
  Result := Assigned(Control);
  if Result then
  begin
    OpaqueActive  := IsOpaqueActive;
    GlassActive   := IsGlassActive;
    PictureActive := IsPictureActive;
    BkFormActive  := IsBkFormActive;
    Result :=
      (not OpaqueActive) or GlassActive or IsBkFormActive or PictureActive;
  end;
end;

procedure TFCBackgroundOptions.ControlChanged(Sender: TObject);
begin
  PicChanged       (Sender, True);
  BkgrndFormChanged(Sender, True);
  GlassChanged     (Sender, True);
end;

procedure DrawXRay(BkOptions: TFCBackgroundOptions; var Bmp: TBitmap;
  R, DrawR: TRect; BmpWidth, BmpHeight: Integer; PixelFormat: TPixelFormat);
var
  WndHandle,
  Limit: HWnd;
  RAux,
  RAux2: TRect;
  P: TPoint;
  SaveClipRgn,
  ClipRgn: HRGN;
  ExistsClipRgn,
  HasUpdateRect,
  SaveTEXPRenderDisabled: Boolean;
begin
  if BkOptions.Control is TWinControl
  then WndHandle := TWinControl(BkOptions.Control).Handle
  else WndHandle := 0;
  RAux := R;
  ClientToScreen(WndHandle, RAux.TopLeft);
  ScreenToClient(BkOptions.Control.Parent.Handle, RAux.TopLeft);
  ClientToScreen(WndHandle, RAux.BottomRight);
  ScreenToClient(BkOptions.Control.Parent.Handle, RAux.BottomRight);

  RAux2 := DrawR;
  LPToDP(Bmp.Canvas.Handle, RAux2, 2);
  SaveClipRgn   := CreateRectRgn(0, 0, 0, 0);
  ExistsClipRgn := GetClipRgn(Bmp.Canvas.Handle, SaveClipRgn) = 1;
  ClipRgn       := CreateRectRgn(RAux2.Left, RAux2.Top, RAux2.Right, RAux2.Bottom);
  SelectClipRgn(Bmp.Canvas.Handle, ClipRgn);
  DeleteObject(ClipRgn);
  try
    OffsetWindowOrgEx(Bmp.Canvas.Handle, RAux.Left-R.Left-(DrawR.Left-R.Left),
      RAux.Top-R.Top-(DrawR.Top-R.Top), P);
    try
      Limit := WndHandle;
      HasUpdateRect:= GetUpdateRect(TWinControl(BkOptions.Control).Handle,
        TRect(nil^), False);
      SaveTEXPRenderDisabled := TEXPRenderDisabled;
      TEXPRenderDisabled     := True;
      try
        RenderWindowToDC(BkOptions.Control.Parent.Handle, Limit,
          BkOptions.Control.Parent, Bmp.Canvas.Handle, RAux, True, False, False,
          True);
      finally
        TEXPRenderDisabled := SaveTEXPRenderDisabled;
        if not HasUpdateRect then
          ValidateRect(TWinControl(BkOptions.Control).Handle, nil);
      end;
    finally
      SetWindowOrgEx(Bmp.Canvas.Handle, P.x, P.y, nil);
    end;
  finally
    if ExistsClipRgn
    then SelectClipRgn(Bmp.Canvas.Handle, SaveClipRgn)
    else SelectClipRgn(Bmp.Canvas.Handle, 0);
    DeleteObject(SaveClipRgn);
  end;
end;

procedure DrawBkgrndForm(BkOptions: TFCBackgroundOptions; Control: TControl;
  var Bmp: TBitmap; R, DrawR: TRect; BmpWidth, BmpHeight: Integer;
  PixelFormat: TPixelFormat);
var
  WndHandle: HWnd;
  RAux,
  RAux2: TRect;
  P: TPoint;
  SaveClipRgn,
  ClipRgn: HRGN;
  SaveTEXPRenderDisabled,
  ExistsClipRgn: Boolean;
  BkForm: TCustomForm;
  ClientWidth,
  ClientHeight,
  OffSetH,
  OffsetV: Integer;
  ScrollInfoH,
  ScrollInfoV: TScrollInfo;
begin
  if Control is TWinControl
  then WndHandle := (Control as TWinControl).Handle
  else WndHandle := Control.Parent.Handle;

  BkForm := BkOptions.BkgrndForm;

  ClientWidth  := BkOptions.Control.ClientWidth;
  ClientHeight := BkOptions.Control.ClientHeight;
  OffSetH      := 0;
  OffSetV      := 0;

  if IsScrollBarVisible(BkOptions.Control, TWinControl(BkOptions.Control).Handle, sbHorizontal) then
  begin
    ScrollInfoH.cbSize := SizeOf(ScrollInfoH);
    ScrollInfoH.fMask  := SIF_ALL;
    GetScrollInfo(TWinControl(BkOptions.Control).Handle, SB_HORZ, ScrollInfoH);
    ClientWidth  := ScrollInfoH.nMax;
    OffSetH      := -ScrollInfoH.nPos;
  end;
  if IsScrollBarVisible(BkOptions.Control, TWinControl(BkOptions.Control).Handle, sbVertical) then
  begin
    ScrollInfoV.cbSize := SizeOf(ScrollInfoV);
    ScrollInfoV.fMask  := SIF_ALL;
    GetScrollInfo(TWinControl(BkOptions.Control).Handle, SB_VERT, ScrollInfoV);
    ClientHeight := ScrollInfoV.nMax;
    OffsetV      := -ScrollInfoV.nPos;
  end;

  RAux := Rect(0, 0, ClientWidth, ClientHeight);
  RAux.TopLeft     := ControlClientToScreen(BkOptions.Control, RAux.TopLeft);
  RAux.BottomRight := ControlClientToScreen(BkOptions.Control, RAux.BottomRight);
  OffsetRect(RAux, OffSetH, OffsetV);
  if not EqualRect(RAux, BkForm.BoundsRect) then
    BkForm.SetBounds(RAux.Left, RAux.Top, RAux.Right - RAux.Left,
      RAux.Bottom - RAux.Top);

  RAux := R;
  ClientToScreen(WndHandle    , RAux.TopLeft);
  ScreenToClient(BkForm.Handle, RAux.TopLeft);
  ClientToScreen(WndHandle    , RAux.BottomRight);
  ScreenToClient(BkForm.Handle, RAux.BottomRight);
  if BkOptions.Control <> Control then
  begin
    OffsetRect(RAux, -OffsetH, -OffsetV);
    if IsScrollBarVisible(Control, TWinControl(Control).Handle, sbHorizontal) then
    begin
      ScrollInfoH.cbSize := SizeOf(ScrollInfoH);
      ScrollInfoH.fMask  := SIF_POS;
      GetScrollInfo(TWinControl(Control).Handle, SB_HORZ, ScrollInfoH);
      OffsetRect(RAux, -ScrollInfoH.nPos, 0);
    end;
    if IsScrollBarVisible(Control, TWinControl(Control).Handle, sbVertical) then
    begin
      ScrollInfoV.cbSize := SizeOf(ScrollInfoV);
      ScrollInfoV.fMask  := SIF_POS;
      GetScrollInfo(TWinControl(Control).Handle, SB_VERT, ScrollInfoV);
      OffsetRect(RAux, 0, -ScrollInfoV.nPos);
    end;
  end;

  RAux2 := DrawR;
  LPToDP(Bmp.Canvas.Handle, RAux2, 2);
  SaveClipRgn := CreateRectRgn(0, 0, 0, 0);
  ExistsClipRgn := GetClipRgn(Bmp.Canvas.Handle, SaveClipRgn) = 1;
  ClipRgn := CreateRectRgn(RAux2.Left, RAux2.Top, RAux2.Right, RAux2.Bottom);
  SelectClipRgn(Bmp.Canvas.Handle, ClipRgn);
  DeleteObject(ClipRgn);
  try
    OffsetWindowOrgEx(Bmp.Canvas.Handle, RAux.Left-R.Left+OffSetH,
      RAux.Top-R.Top+OffsetV, P);
    SaveTEXPRenderDisabled := TEXPRenderDisabled;
    TEXPRenderDisabled     := True;
    try
      RenderWindowToDC(BkForm.Handle, 0, BkForm, Bmp.Canvas.Handle, RAux,
        True, True, False, True);
    finally
      TEXPRenderDisabled := SaveTEXPRenderDisabled;
      SetWindowOrgEx(Bmp.Canvas.Handle, P.x, P.y, nil);
    end;
  finally
    if ExistsClipRgn
    then SelectClipRgn(Bmp.Canvas.Handle, SaveClipRgn)
    else SelectClipRgn(Bmp.Canvas.Handle, 0);
    DeleteObject(SaveClipRgn);
  end;
end;

procedure BlendBkgrnd(BkOptions: TFCBackgroundOptions; Bmp: TBitmap;
  LocalBmp: Boolean; R: TRect; RWidth, RHeight: Integer;
  PixelFormat: TPixelFormat);
var
  BrushBmp: TBitmap;
  BrushAlign: TPoint;
  ParentControl: TControl;
  Level: Integer;
  BmpRect: TRect;
  P: TPoint;
begin
  if PixelFormat = pf8bit
  then
  begin
    Level := Round((BkOptions.GlassTranslucencyToUse * 63) / 255);
    BrushBmp := TBitmap.Create;
    try
      BrushBmp.Canvas.Lock;
      BrushBmp.Width      := 8;
      BrushBmp.Height     := 8;
      BrushBmp.Monochrome := True;

      BrushAlign := ControlClientToScreen(BkOptions.Control, R.TopLeft);

      ParentControl := BkOptions.Control;
      while ParentControl.Parent <> nil do
        ParentControl := ParentControl.Parent;

      Dec(BrushAlign.x, ParentControl.Left);
      Dec(BrushAlign.y, ParentControl.Top);
      SetBrushOrgEx(Bmp.Canvas.Handle, -BrushAlign.x, -BrushAlign.y, @P);
      try
        BlendBmp(Bmp, BrushBmp, PixelFormat, BkOptions.GlassColor, R, Level);
      finally
        SetBrushOrgEx(Bmp.Canvas.Handle, P.x, P.y, nil);
      end;
      BrushBmp.Canvas.Unlock;
    finally
      BrushBmp.Free;
    end;
  end
  else
  begin
    Level := BkOptions.GlassTranslucencyToUse;
    if LocalBmp
    then BmpRect := Rect(0, 0, Bmp.Width, Bmp.Height)
    else
    begin
      BmpRect := R;
      LPToDP(Bmp.Canvas.Handle, BmpRect, 2);
    end;
    if not IsRectEmpty(BmpRect) then
      BlendBmp(Bmp, nil, PixelFormat, BkOptions.GlassColor, BmpRect, Level);
  end;
end;

procedure DrawStandardBackground(Control: TFCControl; DC: HDC; R: TRect;
  ThemesDisabled: Boolean);
var
  Brush: HBrush;
begin
  {$ifdef D7UP}
  with ThemeServices do
  begin
    if(not ThemesDisabled) and ThemesEnabled and Assigned(Control.Parent) and
      (csParentBackground in Control.ControlStyle) then
      DrawParentBackground(TWinControl(Control).Handle, DC, nil, False, @R)
    else
    begin
        Brush := CreateSolidBrush(Graphics.ColorToRGB(TFCControl(Control).Color));
        FillRect(DC, R, Brush);
        DeleteObject(Brush);
    end;
  end;
  {$else}
    Brush := CreateSolidBrush(ColorToRGB(TFCControl(Control).Color));
    FillRect(DC, R, Brush);
    DeleteObject(Brush);
  {$endif D7UP}
end;

procedure TFCBackgroundOptions.DrawBackGround(DC: HDC; DstBmp: TBitmap; R: TRect);
var
  Bmp: TBitmap;
  LocalBmp: Boolean;
  PixelFormat: TPixelFormat;
  RWidth,
  RHeight: Integer;
  PictureBkOptions,
  BkFormBkOptions,
  GlassBkOptions: TFCBackgroundOptions;
  ScrollInfoH,
  ScrollInfoV: TScrollInfo;
  DrawR: TRect;
begin
  if IsActive
  then
  begin
    if IsRectEmpty(R) then
      R := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
    DrawR := R;

    if IsScrollBarVisible(Control, TWinControl(Control).Handle, sbHorizontal) then
    begin
      ScrollInfoH.cbSize := SizeOf(ScrollInfoH);
      ScrollInfoH.fMask  := SIF_POS;
      GetScrollInfo(TWinControl(Control).Handle, SB_HORZ, ScrollInfoH);
      OffsetRect(DrawR, ScrollInfoH.nPos, 0);
    end;
    if IsScrollBarVisible(Control, TWinControl(Control).Handle, sbVertical) then
    begin
      ScrollInfoV.cbSize := SizeOf(ScrollInfoV);
      ScrollInfoV.fMask  := SIF_POS;
      GetScrollInfo(TWinControl(Control).Handle, SB_VERT, ScrollInfoV);
      OffsetRect(DrawR, 0, ScrollInfoV.nPos);
    end;

    PixelFormat := DevicePixelFormat(False);
    RWidth      := R.Right  - R.Left;
    RHeight     := R.Bottom - R.Top;
    LocalBmp    :=
      (
        (DstBmp = nil) or
        (DstBmp.PixelFormat = pfDevice)
      ); {and
      (
        (TECurBmp = nil)                  or
        (TECurBmp.PixelFormat = pfDevice) or
        (TECurBmp.Canvas.Handle <> DC)    or
        (GlassActive and ControlClientAreaHasRegion(TWinControl(Control)))
      );}

    if not LocalBmp
    then
    begin
      Bmp := DstBmp;
      Bmp.Canvas.Lock;
//        if DstBmp <> nil
//        then Bmp := DstBmp
//        else Bmp := TECurBmp;
      PixelFormat := Bmp.PixelFormat;
    end
    else
    begin
      Bmp := TBitmap.Create;
//          TECurBmp := Bmp;
      Bmp.Canvas.Lock;
      AdjustBmpForTransition(Bmp, 0, RWidth, RHeight, PixelFormat);
      SetWindowOrgEx(Bmp.Canvas.Handle, DrawR.Left, DrawR.Top, nil);
    end;
    try
      if PictureActive
      then PictureBkOptions := GetParentPicture
      else PictureBkOptions := nil;
      if BkFormActive
      then BkFormBkOptions  := GetParentBkgrndForm
      else BkFormBkOptions  := nil;
      if GlassActive
      then GlassBkOptions   := GetParentGlass
      else GlassBkOptions   := nil;

      if GlassActive and (GlassBkOptions.GlassTranslucencyToUse = 0)
      then
      begin
        Bmp.Canvas.Brush.Color := GlassBkOptions.GlassColor;
        Bmp.Canvas.FillRect(DrawR);
      end
      else
      begin
        if XRayActive(PictureBkOptions, DrawR)
        then DrawXRay(Self, Bmp, R, DrawR, RWidth, RHeight, PixelFormat)
        else
        begin
          if BkFormActive
          then DrawBkgrndForm(BkFormBkOptions, Control, Bmp, R, DrawR, RWidth, RHeight, PixelFormat)
          else
          begin
            if PictureActive
            then DrawStandardBackground(
                   TFCControl(PictureBkOptions.Control), Bmp.Canvas.Handle,
                   DrawR, FThemesDisabled)
            else DrawStandardBackground(TFCControl(Control),
                   Bmp.Canvas.Handle, DrawR, FThemesDisabled);
          end;
        end;
        if PictureActive then
          DrawPicture(PictureBkOptions.Picture.Graphic,
            PictureBkOptions.PictureMode, PictureBkOptions.PictureTranspColor,
            TWinControl(PictureBkOptions.Control), Bmp, DrawR, 0, Control);

        if GlassActive then
          BlendBkgrnd(GlassBkOptions, Bmp, LocalBmp, DrawR, RWidth, RHeight, PixelFormat);
      end;

      if LocalBmp then
        BitBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
          Bmp.Canvas.Handle, DrawR.Left, DrawR.Top, cmSrcCopy);
    finally
      Bmp.Canvas.Unlock;
      if LocalBmp then
        Bmp.Free;
    end;
  end
  else DrawStandardBackground(TFCControl(Control), DC, R, FThemesDisabled);
end;

{$ifdef D7UP}
procedure TFCBackgroundOptions.SetThemesDisabled(const Value: Boolean);
begin
  if FThemesDisabled <> Value then
  begin
    FThemesDisabled := Value;
    Changed;
  end;
end;

var
  OldDrawThemeParentBackground:
    function(hwnd: HWND; hdc: HDC; prc: PRECT): HRESULT; stdcall;

function BEDrawThemeParentBackground(hwnd: HWND; hdc: HDC;
  prc: PRECT): HRESULT; stdcall;
begin
  BEDrawParentBackgroundList.Add(Pointer(GetParent(hwnd)));
  try
    Result := OldDrawThemeParentBackground(hwnd, hdc, prc);
  finally
    BEDrawParentBackgroundList.Delete(BEDrawParentBackgroundList.Count-1);
  end;
end;

procedure ThemesSupport;
begin
  ThemeServices;
  if Assigned(DrawThemeParentBackground) then
  begin
    OldDrawThemeParentBackground := DrawThemeParentBackground;
    DrawThemeParentBackground    := BEDrawThemeParentBackground;
    BEDrawParentBackgroundList   := TList.Create;
  end;
end;

function BEParentBackgroundPainted(Handle: HWND): Boolean;
begin
  Result :=
    (BEDrawParentBackgroundList <> nil)    and
    (BEDrawParentBackgroundList.Count > 0) and
    (BEDrawParentBackgroundList.Items[BEDrawParentBackgroundList.Count-1] = Pointer(Handle));
end;

initialization
  ThemesSupport;

finalization
  BEDrawParentBackgroundList.Free;
{$endif NoVCL}
{$endif D7UP}

end.
