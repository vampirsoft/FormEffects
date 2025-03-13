unit teBlock;

interface

{$RANGECHECKS OFF}
{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teMasked, Windows, Messages, Graphics, teRender;

type
  TBlockTransition = class(TMaskedTransition)
  private
    FPuzzle: Boolean;
    FCols,
    FRows: Integer;
    FBlockHeight,
    FBlockWidth: Integer;
    FDual: Boolean;

    procedure SetBlockHeight(Value: Integer);
    procedure SetBlockWidth(Value: Integer);
    procedure SetCols(Value: Integer);
    procedure SetRows(Value: Integer);
  protected
    ColsToUse,
    RowsToUse: Integer;

    procedure ExecuteFrame(Data: TTETransitionData;
      CurrentFrame, Step, LastExecutedFrame: Longint); override;
    procedure MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step, LastExecutedFrame:
      Longint; Data: TTETransitionData; Draw, CalcDirtyRects: Boolean); override;
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    function  Smooth(Device: TTETransitionDevice): Boolean; override;
    function  CalcTotalFrames(Data: TTETransitionData): Longint; override;
    function  CalculateReversedSubStyle(
      const StyleValue, SubStyleValue: Word): Word; override;
    function  GetBlockBounds(Data: TTETransitionData; CheckPuzzle: Boolean;
      Col, Row: Longint): TRect;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure StyleBlockByArray(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure StyleBlockByArrayReversed(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style2_1Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style2_2Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style2_3Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style2_4Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style2_5Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style2_6Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style2_7Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style2_8Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style3_1Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style3_2Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style3_3Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure Style3_4Block(Data: TTETransitionData;
      CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
    procedure LoadFromStrings(List: TStrings; Prefix: String); override;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function Description: String; override;

    procedure Assign(Source: TPersistent); override;
    class function GetEditor: String; override;
    function  CountOfSubStyles(StyleValue: Word): Word; override;

    property CountOfStyles;
  published
    property BlockHeight: Integer read FBlockHeight write SetBlockHeight default 50;
    property BlockWidth: Integer read FBlockWidth write SetBlockWidth default 50;
    property Cols: Integer read FCols write SetCols default 0;
    property Dual: Boolean read FDual write FDual default False;
    property Puzzle: Boolean read FPuzzle write FPuzzle default False;
    property Rows: Integer read FRows write SetRows default 0;
    property Reversed;
    property Style;
    property SubStyle;
  end;

implementation

uses teMskWk, TypInfo;

type
  TStyleBlock = procedure (Data: TTETransitionData;
    CurrentFrame, TotFrames: Longint; var Col, Row: Integer;
    Dual: Boolean) of object;

  TBlockData = class(TTECustomData)
  public
    StyleBlockProc: TStyleBlock;
    ExtSize: Integer;
    IsPixel: Boolean;
    BlocksOrder: PDWordArray;
    PuzzleV_1,
    PuzzleV_2,
    PuzzleV_3,
    PuzzleV_4,
    PuzzleH_1,
    PuzzleH_2,
    PuzzleH_3,
    PuzzleH_4: TBitmap;

    destructor Destroy; override;
  end;

constructor TBlockTransition.Create(AOwner: TComponent);
begin
  inherited;

  FDual          := False;
  FPuzzle        := False;
  FBlockHeight   := 50;
  FBlockWidth    := 50;
  FCountOfStyles :=  4;
  FCols          :=  0;
  FRows          :=  0;
end;

class function TBlockTransition.Description: String;
begin
  Result := 'Blocks';
end;

procedure TBlockTransition.SetBlockHeight(Value: Integer);
begin
  if FBlockHeight <> Value then
  begin
    FBlockHeight := Value;
    if FBlockHeight < 1 then
      FBlockHeight := 1;
    FRows := 0;
  end;
end;

procedure TBlockTransition.SetBlockWidth(Value: Integer);
begin
  if FBlockWidth <> Value then
  begin
    FBlockWidth := Value;
    if FBlockWidth < 1 then
      FBlockWidth := 1;
    FCols := 0;
  end;
end;

procedure TBlockTransition.SetCols(Value: Integer);
begin
  if FCols <> Value then
  begin
    FCols := Value;
    if FCols < 1 then
      FCols := 1;
    FBlockWidth  := 0;
  end;
end;

procedure TBlockTransition.SetRows(Value: Integer);
begin
  if FRows <> Value then
  begin
    FRows := Value;
    if FRows < 1 then
      FRows := 1;
    FBlockHeight := 0;
  end;
end;

procedure TBlockTransition.Initialize(Data: TTETransitionData; var TotalFrames:
  Longint);
var
  BlockData: TBlockData;

  function CalcLCM(const Number1, Number2: Longint): Longint;
  var
    i,
    j: Integer;
    Multiple1,
    Multiple2: Longint;
  begin
    i := 1;
    j := 1;

    repeat
      Multiple1 := Number1 * i;
      Multiple2 := Number2 * j;
      if Multiple1 > Multiple2
      then Inc(j)
      else if Multiple1 < Multiple2
      then Inc(i)
    until Multiple1 = Multiple2;

    Result := Multiple1 + 1;
  end;

  function CreatePuzzle(const Vertical: Boolean; const Item: Integer;
    Monochrome: Boolean; Color: Byte): TBitmap;
  var
    Width,
    Height: Integer;
  begin
    Width  := Data.Width  div ColsToUse;
    if Item mod 2 = 0 then
      Inc(Width);

    Height := Data.Height div RowsToUse;
    if Item > 2 then
      Inc(Height);

    Result := TBitmap.Create;
    Result.Canvas.Lock;

    if Vertical
    then
    begin
      Result.Width  := Width;
      Result.Height := Height + (2 * BlockData.ExtSize);

      if Monochrome
      then
      begin
        Result.PixelFormat := pf1bit;
        Result.Canvas.Pen  .Color := clBlack;
        Result.Canvas.Brush.Color := clBlack;
        FillRect(Result.Canvas.Handle, Rect(0, 0, Result.Width, Result.Height),
          GetStockObject(WHITE_BRUSH));
      end
      else
      begin
        Result.PixelFormat := pf8bit;
        Result.Palette     := CreateGrayScalePalette;
        Result.Canvas.Pen  .Color := $02000000 or RGB(Color, Color, Color);
        Result.Canvas.Brush.Color := $02000000 or RGB(Color, Color, Color);
        FillRect(Result.Canvas.Handle, Rect(0, 0, Result.Width, Result.Height),
          GetStockObject(BLACK_BRUSH));
      end;

      Result.Canvas.Ellipse((Result.Width - BlockData.ExtSize) div 2, 0,
        (Result.Width - BlockData.ExtSize) div 2 + BlockData.ExtSize,
        BlockData.ExtSize);
      Result.Canvas.FillRect(Rect((Result.Width - BlockData.ExtSize) div 2,
        BlockData.ExtSize div 2,
        (Result.Width - BlockData.ExtSize) div 2 + BlockData.ExtSize,
        BlockData.ExtSize));
      Result.Canvas.FillRect(Rect(0, BlockData.ExtSize, Result.Width,
        Result.Height - BlockData.ExtSize));
      Result.Canvas.Ellipse((Result.Width - BlockData.ExtSize) div 2,
        Result.Height - BlockData.ExtSize,
        (Result.Width - BlockData.ExtSize) div 2 + BlockData.ExtSize,
        Result.Height);
      Result.Canvas.FillRect(Rect((Result.Width - BlockData.ExtSize) div 2,
        Result.Height - BlockData.ExtSize,
        (Result.Width - BlockData.ExtSize) div 2 + BlockData.ExtSize,
        Result.Height - (BlockData.ExtSize div 2)));
      if Monochrome
      then
      begin
        Result.Canvas.Pen  .Color := clWhite;
        Result.Canvas.Brush.Color := clWhite;
      end
      else
      begin
        Result.Canvas.Pen  .Color := clBlack;
        Result.Canvas.Brush.Color := clBlack;
      end;
      Result.Canvas.Ellipse(0, (Result.Height - BlockData.ExtSize) div 2,
        BlockData.ExtSize,
        ((Result.Height - BlockData.ExtSize) div 2) + BlockData.ExtSize);
      Result.Canvas.FillRect(Rect(0, (Result.Height - BlockData.ExtSize) div 2,
        BlockData.ExtSize div 2,
        (Result.Height - BlockData.ExtSize) div 2 + BlockData.ExtSize));
      Result.Canvas.Ellipse(Result.Width - BlockData.ExtSize,
        (Result.Height - BlockData.ExtSize) div 2, Result.Width,
        ((Result.Height - BlockData.ExtSize) div 2) + BlockData.ExtSize);
      Result.Canvas.FillRect(Rect(Result.Width - (BlockData.ExtSize div 2),
        (Result.Height - BlockData.ExtSize) div 2, Result.Width,
        (Result.Height - BlockData.ExtSize) div 2 + BlockData.ExtSize));
    end
    else
    begin
      Result.Width  := Width + (2 * BlockData.ExtSize);
      Result.Height := Height;

      if Monochrome
      then
      begin
        Result.PixelFormat := pf1bit;
        Result.Canvas.Pen  .Color := clBlack;
        Result.Canvas.Brush.Color := clBlack;
        FillRect(Result.Canvas.Handle, Rect(0, 0, Result.Width, Result.Height),
          GetStockObject(WHITE_BRUSH));
      end
      else
      begin
        Result.PixelFormat := pf8bit;
        Result.Palette     := CreateGrayScalePalette;
        Result.Canvas.Pen  .Color := $02000000 or RGB(Color, Color, Color);
        Result.Canvas.Brush.Color := $02000000 or RGB(Color, Color, Color);
        FillRect(Result.Canvas.Handle, Rect(0, 0, Result.Width, Result.Height),
           GetStockObject(BLACK_BRUSH));
      end;

      Result.Canvas.Ellipse(0, (Result.Height - BlockData.ExtSize) div 2,
        BlockData.ExtSize,
        (Result.Height - BlockData.ExtSize) div 2 + BlockData.ExtSize);
      Result.Canvas.FillRect(Rect(BlockData.ExtSize div 2,
        (Result.Height - BlockData.ExtSize) div 2, BlockData.ExtSize,
        (Result.Height - BlockData.ExtSize) div 2 + BlockData.ExtSize));
      Result.Canvas.FillRect(Rect(BlockData.ExtSize, 0,
        Result.Width - BlockData.ExtSize, Result.Height));
      Result.Canvas.Ellipse(Result.Width - BlockData.ExtSize,
        (Result.Height - BlockData.ExtSize) div 2, Result.Width,
        (Result.Height - BlockData.ExtSize) div 2 + BlockData.ExtSize);
      Result.Canvas.FillRect(Rect(Result.Width - BlockData.ExtSize,
        (Result.Height - BlockData.ExtSize) div 2,
        Result.Width - (BlockData.ExtSize div 2),
        (Result.Height - BlockData.ExtSize) div 2 + BlockData.ExtSize));
      if Monochrome
      then
      begin
        Result.Canvas.Pen  .Color := clWhite;
        Result.Canvas.Brush.Color := clWhite;
      end
      else
      begin
        Result.Canvas.Pen  .Color := clBlack;
        Result.Canvas.Brush.Color := clBlack;
      end;
      Result.Canvas.Ellipse((Result.Width - BlockData.ExtSize) div 2, 0,
        ((Result.Width - BlockData.ExtSize) div 2) + BlockData.ExtSize,
        BlockData.ExtSize);
      Result.Canvas.FillRect(Rect((Result.Width - BlockData.ExtSize) div 2, 0,
        (Result.Width - BlockData.ExtSize) div 2 + BlockData.ExtSize,
        BlockData.ExtSize div 2));
      Result.Canvas.Ellipse((Result.Width - BlockData.ExtSize) div 2,
        Result.Height - BlockData.ExtSize,
        ((Result.Width - BlockData.ExtSize) div 2) + BlockData.ExtSize,
        Result.Height);
      Result.Canvas.FillRect(Rect((Result.Width - BlockData.ExtSize) div 2,
        Result.Height - (BlockData.ExtSize div 2),
        (Result.Width - BlockData.ExtSize) div 2 + BlockData.ExtSize,
        Result.Height));
    end;
  end;

  procedure Style1BlocksOrder;
  var
    Index: DWord;
    TotalBlocks,
    i,
    aux,
    Col,
    Row: Longint;
  begin
    Randomize;
    TotalBlocks := ColsToUse * RowsToUse;
    GetMem(BlockData.BlocksOrder, (TotalBlocks+1) * 4);
    Index := 1;
    for Row := 1 to RowsToUse do
    begin
      aux := Row shl 16;
      for Col := 1 to ColsToUse do
      begin
        BlockData.BlocksOrder[Index] := aux + Col;
        Inc(Index);
      end;
    end;
    for i := 1 to TotalBlocks do
    begin
      aux                          := BlockData.BlocksOrder[i];
      Index                        := Random(TotalBlocks) + 1;
      BlockData.BlocksOrder[i]     := BlockData.BlocksOrder[Index];
      BlockData.BlocksOrder[Index] := aux;
    end;
  end;

  procedure Style4_1BlocksOrder;
  var
    TotalBlocks,
    Col,
    Row,
    PrevCol,
    PrevRow,
    IncCol,
    IncRow: Longint;
    i: Longint;
    R : TRect;
  begin
    TotalBlocks := ColsToUse * RowsToUse;
    Col         := 1;
    Row         := 1;
    PrevCol     := Col;
    PrevRow     := Row;
    IncCol      := 1;
    IncRow      := 0;
    R           := Rect(0, 1, ColsToUse + 1, RowsToUse + 1);
    GetMem(BlockData.BlocksOrder, (TotalBlocks+1) * 4);
    BlockData.BlocksOrder[1] := (Row shl 16) + Col;
    for i := 2 to TotalBlocks do
    begin
      if IncCol <> 0 then
      begin
        if IncCol > 0 then
        begin
          if PrevCol + 1 = R.Right then
          begin
            IncCol := 0;
            IncRow := 1;
            Dec(R.Right);
          end;
        end
        else
        begin
          if PrevCol - 1 = R.Left then
          begin
            IncCol :=  0;
            IncRow := -1;
            Inc(R.Left);
          end;
        end;
      end
      else
      begin
        if IncRow > 0 then
        begin
          if PrevRow + 1 = R.Bottom then
          begin
            IncCol := -1;
            IncRow :=  0;
            Dec(R.Bottom);
          end;
        end
        else
        begin
          if PrevRow - 1 = R.Top then
          begin
            IncCol := 1;
            IncRow := 0;
            Inc(R.Top);
          end;
        end;
      end;

      Col     := PrevCol + IncCol;
      Row     := PrevRow + IncRow;
      PrevCol := Col;
      PrevRow := Row;
      BlockData.BlocksOrder[i] := (Row shl 16) + Col;
    end;
  end;

  procedure Style4_3BlocksOrder;
  var
    TotalBlocks,
    Col,
    Row,
    PrevCol,
    PrevRow,
    IncCol,
    IncRow: Longint;
    i: Longint;
    R : TRect;
  begin
    TotalBlocks    := ColsToUse * RowsToUse;
    Col            := ColsToUse;
    Row            := 1;
    PrevCol        := Col;
    PrevRow        := Row;
    IncCol         := -1;
    IncRow         :=  0;
    R              := Rect(0, 1, ColsToUse + 1, RowsToUse + 1);
    GetMem(BlockData.BlocksOrder, (TotalBlocks+1) * 4);
    BlockData.BlocksOrder[1] := (Row shl 16) + Col;
    for i := 2 to TotalBlocks do
    begin
      if IncCol <> 0 then
      begin
        if IncCol > 0 then
        begin
          if PrevCol + 1 = R.Right then
          begin
            IncCol  :=  0;
            IncRow  := -1;
            R.Right := PrevCol;
          end;
        end
        else
        begin
          if PrevCol - 1 = R.Left then
          begin
            IncCol := 0;
            IncRow := 1;
            R.Left := PrevCol;
          end;
        end;
      end
      else
      begin
        if IncRow > 0 then
        begin
          if PrevRow + 1 = R.Bottom then
          begin
            IncCol   := 1;
            IncRow   := 0;
            R.Bottom := PrevRow;
          end;
        end
        else
        begin
          if PrevRow - 1 = R.Top then
          begin
            IncCol := -1;
            IncRow :=  0;
            R.Top  := PrevRow;
          end;
        end;
      end;

      Col     := PrevCol + IncCol;
      Row     := PrevRow + IncRow;
      PrevCol := Col;
      PrevRow := Row;
      BlockData.BlocksOrder[i] := (Row shl 16) + Col;
    end;
  end;

var
  BlockBounds: TRect;
begin
  BlockData   := TBlockData.Create(Data);
  Data.Custom := BlockData;
  BlockData.StyleBlockProc := nil;
  BlockData.BlocksOrder    := nil;

  case StyleToUse of
    1: BlockData.StyleBlockProc := StyleBlockByArray;
    2: case SubStyleToUse of
         1: BlockData.StyleBlockProc := Style2_1Block;
         2: if Dual
            then BlockData.StyleBlockProc := Style2_1Block
            else BlockData.StyleBlockProc := Style2_2Block;
         3: BlockData.StyleBlockProc := Style2_3Block;
         4: if Dual
            then BlockData.StyleBlockProc := Style2_3Block
            else BlockData.StyleBlockProc := Style2_4Block;
         5: BlockData.StyleBlockProc := Style2_5Block;
         6: if Dual
            then BlockData.StyleBlockProc := Style2_5Block
            else BlockData.StyleBlockProc := Style2_6Block;
         7: BlockData.StyleBlockProc := Style2_7Block;
         8: if Dual
            then BlockData.StyleBlockProc := Style2_7Block
            else BlockData.StyleBlockProc := Style2_8Block;
       end;
    3: case SubStyleToUse of
         1: BlockData.StyleBlockProc := Style3_1Block;
         2: if Dual
            then BlockData.StyleBlockProc := Style3_1Block
            else BlockData.StyleBlockProc := Style3_2Block;
         3: BlockData.StyleBlockProc := Style3_3Block;
         4: if Dual
            then BlockData.StyleBlockProc := Style3_3Block
            else BlockData.StyleBlockProc := Style3_4Block;
       end;
    4: case SubStyleToUse of
         1,3: BlockData.StyleBlockProc := StyleBlockByArray;
         2,4: BlockData.StyleBlockProc := StyleBlockByArrayReversed;
       end;
  end;

  ColsToUse := Cols;
  if ColsToUse = 0 then
    ColsToUse := Round(Data.Width / BlockWidth);
  if ColsToUse < 1 then
    ColsToUse := 1;
  if Puzzle and (Data.Width div ColsToUse < 15) then
    ColsToUse := Trunc(Data.Width / 15);
  if ColsToUse > Data.Width then
    ColsToUse := Data.Width;

  RowsToUse := Rows;
  if RowsToUse = 0 then
    RowsToUse := Round(Data.Height / BlockHeight);
  if RowsToUse < 1 then
    RowsToUse := 1;
  if Puzzle and (Data.Height div RowsToUse < 15) then
    RowsToUse := Trunc(Data.Height / 15);
  if RowsToUse > Data.Height then
    RowsToUse := Data.Height;

  BlockBounds := GetBlockBounds(Data, True, 1, 1);

  inherited;

  BlockData.IsPixel :=
    (ColsToUse = Data.Width) and
    (RowsToUse = Data.Height);

  case StyleToUse of
    1: Style1BlocksOrder;
    4: case SubStyleToUse of
         1,2: Style4_1BlocksOrder;
         3,4: Style4_3BlocksOrder;
       end;
  end;

  if Puzzle and (not BlockData.IsPixel) then
  begin
    if(Data.Width div ColsToUse) <= (Data.Height div RowsToUse)
    then BlockData.ExtSize := Round((Data.Width  div ColsToUse) / 3.5)
    else BlockData.ExtSize := Round((Data.Height div RowsToUse) / 3.5);

    BlockData.PuzzleV_1 := CreatePuzzle(True , 1, True, 0);
    BlockData.PuzzleV_2 := CreatePuzzle(True , 2, True, 0);
    BlockData.PuzzleV_3 := CreatePuzzle(True , 3, True, 0);
    BlockData.PuzzleV_4 := CreatePuzzle(True , 4, True, 0);
    BlockData.PuzzleH_1 := CreatePuzzle(False, 1, True, 0);
    BlockData.PuzzleH_2 := CreatePuzzle(False, 2, True, 0);
    BlockData.PuzzleH_3 := CreatePuzzle(False, 3, True, 0);
    BlockData.PuzzleH_4 := CreatePuzzle(False, 4, True, 0);
  end;
  Data.DirtyRects.CheckBounds := True;
  Data.DirtyRects.Bounds := Rect(0, 0, Data.Width, Data.Height);
end;

function TBlockTransition.Smooth(Device: TTETransitionDevice): Boolean;
begin
  Result := False;
end;

function TBlockTransition.CalcTotalFrames(Data: TTETransitionData): Longint;
begin
  Result := RowsToUse * ColsToUse - 1;
  if Dual and (StyleToUse <> 1) then
    Result := ((Result + 2) div 2) - 1;
end;

procedure TBlockTransition.Assign(Source: TPersistent);
var
  Transition: TBlockTransition;
begin
  if Source is TBlockTransition
  then
  begin
    inherited;

    Transition := TBlockTransition(Source);
    Puzzle     := Transition.Puzzle;
    Dual       := Transition.Dual;
    if Transition.Rows <> 0
    then Rows := Transition.Rows
    else BlockHeight := Transition.BlockHeight;
    if Transition.Cols <> 0
    then Cols := Transition.Cols
    else BlockWidth := Transition.BlockWidth;
  end
  else inherited;
end;

class function TBlockTransition.GetEditor: String;
begin
  Result := 'TBlockTransitionEditor';
end;

function TBlockTransition.CountOfSubStyles(StyleValue: Word): Word;
begin
  if StyleValue = 0
  then Result := 0
  else
  begin
    Result := 1;

    case StyleValue of
      1: Result := 1;
      2: Result := 8;
      3: Result := 4;
      4: Result := 4;
    end;
  end;
end;

function TBlockTransition.CalculateReversedSubStyle(
  const StyleValue, SubStyleValue: Word): Word;
begin
  Result := SubStyleValue;

  case StyleValue of
    2: case SubStyleValue of
         1: Result := 2;
         2: Result := 1;
         3: Result := 4;
         4: Result := 3;
         5: Result := 6;
         6: Result := 5;
         7: Result := 8;
         8: Result := 7;
       end;
    3: case SubStyleValue of
         1: Result := 2;
         2: Result := 1;
         3: Result := 4;
         4: Result := 3;
       end;
    4: case SubStyleValue of
         1: Result := 2;
         2: Result := 1;
         3: Result := 4;
         4: Result := 3;
       end;
  end;
end;

function TBlockTransition.GetBlockBounds(Data: TTETransitionData;
  CheckPuzzle: Boolean; Col, Row: Integer): TRect;
var
  Left,
  Top,
  Right,
  Bottom: Integer;
  BlockData: TBlockData;
begin
  BlockData := TBlockData(Data.Custom);

  if Col = 1
  then Left := 0
  else
  begin
    Left := Round(Data.Width / ColsToUse * (Col - 1));
    if CheckPuzzle and Puzzle and (((Col + Row) mod 2) = 1) then
      Dec(Left, BlockData.ExtSize);
  end;

  if Row = 1
  then Top := 0
  else
  begin
    Top := Round(Data.Height / RowsToUse * (Row - 1));
    if CheckPuzzle and Puzzle and (((Col + Row) mod 2) = 0) then
      Dec(Top, BlockData.ExtSize);
  end;

  if Col = ColsToUse
  then Right := Data.Width
  else
  begin
    Right := Round(Data.Width / ColsToUse * Col);
    if CheckPuzzle and Puzzle and (((Col + Row) mod 2) = 1) then
      Inc(Right, BlockData.ExtSize);
  end;

  if Row = RowsToUse
  then Bottom := Data.Height
  else
  begin
    Bottom := Round(Data.Height / RowsToUse * Row);
    if CheckPuzzle and Puzzle and (((Col + Row) mod 2) = 0) then
      Inc(Bottom, BlockData.ExtSize);
  end;

  Result := Rect(Left, Top, Right, Bottom);
end;

procedure TBlockTransition.ExecuteFrame(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Longint);
var
  i: Integer;
begin
  Data.AllowDeviceUpdate :=
     Puzzle      or
    (Step > 100) or
    (Data.Device.UsingThread);

  inherited;

  if Data.AllowDeviceUpdate and (Frame1bppMaskBmp <> nil) then
  begin
    if Data.DirtyRects.Count > 1
    then
    begin
      for i := 0 to Data.DirtyRects.Count-1 do
        FillRect(Frame1bppMaskBmp.Canvas.Handle, Data.DirtyRects[i],
          GetStockObject(WHITE_BRUSH));
    end
    else FillRect(Frame1bppMaskBmp.Canvas.Handle, Data.UpdateRect,
           GetStockObject(WHITE_BRUSH));
  end;
end;

procedure TBlockTransition.MaskFrame(MaskBmp: TBitmap; CurrentFrame, Step,
  LastExecutedFrame: Longint; Data: TTETransitionData; Draw, CalcDirtyRects:
  Boolean);
const
  ColMasks1bit: array[0..31] of DWord =
    ($FFFFFF7F, $FFFFFFBF, $FFFFFFDF, $FFFFFFEF,
     $FFFFFFF7, $FFFFFFFB, $FFFFFFFD, $FFFFFFFE,
     $FFFF7FFF, $FFFFBFFF, $FFFFDFFF, $FFFFEFFF,
     $FFFFF7FF, $FFFFFBFF, $FFFFFDFF, $FFFFFEFF,
     $FF7FFFFF, $FFBFFFFF, $FFDFFFFF, $FFEFFFFF,
     $FFF7FFFF, $FFFBFFFF, $FFFDFFFF, $FFFEFFFF,
     $7FFFFFFF, $BFFFFFFF, $DFFFFFFF, $EFFFFFFF,
     $F7FFFFFF, $FBFFFFFF, $FDFFFFFF, $FEFFFFFF);

  ColMasks2bit: array[0..30] of DWord =
    ($FFFFFF3F, $FFFFFF9F, $FFFFFFCF, $FFFFFFE7,
     $FFFFFFF3, $FFFFFFF9, $FFFFFFFC, $FFFFFFFE,
     $FFFF3FFF, $FFFF9FFF, $FFFFCFFF, $FFFFE7FF,
     $FFFFF3FF, $FFFFF9FF, $FFFFFCFF, $FFFFFE7F,
     $FF3FFFFF, $FF9FFFFF, $FFCFFFFF, $FFE7FFFF,
     $FFF3FFFF, $FFF9FFFF, $FFFCFFFF, $FFFE7FFF,
     $3FFFFFFF, $9FFFFFFF, $CFFFFFFF, $E7FFFFFF,
     $F3FFFFFF, $F9FFFFFF, $FCFFFFFF);
var
  i,
  TotFrames: Longint;
  Col,
  Row,
  Value: Integer;
  BlockBounds,
  DirtyRect: TRect;
  PuzzleMask: TBitmap;
  Mask: PDWordArray;
  aux: PDWord;
  MaskScanLineSize: Integer;
  Monochrome,
  DualToUse: Boolean;
  BlockData: TBlockData;
begin
  BlockData  := TBlockData(Data.Custom);
  Monochrome := MaskBmp.PixelFormat = pf1bit;
  TotFrames  := RowsToUse * ColsToUse;
  if StyleToUse = 1
  then
  begin
    DualToUse       := False;
    Data.UpdateRect := Rect(0, 0, Data.Width, Data.Height);
  end
  else
  begin
    DualToUse       := Dual;
    Data.UpdateRect := Rect(0, 0, 0, 0);
    if DualToUse then
    begin
      CurrentFrame := CurrentFrame * 2;
      Step         := Step * 2;
    end;
  end;
  CalcDirtyRects := CalcDirtyRects and (Step <= 650);
  if BlockData.IsPixel
  then
  begin
    MaskScanLineSize := GetBytesPerScanline(MaskBmp, pf1bit, 32) div 4;
    Mask := PDWordArray(MaskBmp.ScanLine[MaskBmp.Height-1]);
  end
  else
  begin
    MaskScanLineSize := 0;
    Mask := nil;
  end;

  if Data.AllowDeviceUpdate and
     BlockData.IsPixel      and
     Draw                   and
     Monochrome             and
     (StyleToUse = 1)
  then
  begin
    for i := CurrentFrame-Step+1 to CurrentFrame do
    begin
      Value := BlockData.BlocksOrder[i];
      Row   := Value shr 16;
      Col   := (Value and $0000FFFF) - 1;
      if CalcDirtyRects then
        Data.DirtyRects.AddRect(Rect(Col, Row-1, Col+1, Row));
      aux  := @Mask[((RowsToUse - Row) * MaskScanLineSize) + (Col shr 5)];
      aux^ := aux^ and ColMasks1bit[Col and $1F];
    end;
  end
  else
  begin
    for i := CurrentFrame-Step+1 to CurrentFrame do
    begin
      BlockData.StyleBlockProc(Data, i, TotFrames, Col, Row, DualToUse);
      if not Data.AllowDeviceUpdate
      then
      begin
        if BlockData.IsPixel
        then
        begin
          Dec(Col);
          aux  := @Mask[((RowsToUse - Row) * MaskScanLineSize) + (Col shr 5)];
          aux^ := aux^ and ColMasks1bit[Col and $1F];
          if Data.Device.IsRGB
          then SetPixelV(Data.DeviceCanvas.Handle, Col, Row-1,
                 GetPixel(Data.DstBmp.Canvas.Handle, Col, Row-1))
          else BitBlt(Data.DeviceCanvas.Handle, Col, Row-1, 1, 1,
                 Data.DstBmp.Canvas.Handle, Col, Row-1, cmSrcCopy);
        end
        else
        begin
          BlockBounds := GetBlockBounds(Data, False, Col, Row);
          MaskBmp.Canvas.FillRect(BlockBounds);
          BitBlt(Data.DeviceCanvas.Handle, BlockBounds.Left, BlockBounds.Top,
            BlockBounds.Right-BlockBounds.Left, BlockBounds.Bottom-BlockBounds.Top,
            Data.DstBmp.Canvas.Handle, BlockBounds.Left, BlockBounds.Top, cmSrcCopy);
        end;
      end
      else
      begin
        if BlockData.IsPixel
        then
        begin
          if Draw then
          begin
            Dec(Col);
            if Monochrome
            then
            begin
              aux  := @Mask[((RowsToUse - Row) * MaskScanLineSize) + (Col shr 5)];
              aux^ := aux^ and ColMasks1bit[Col and $1F];
            end
            else SetPixelV(MaskBmp.Canvas.Handle, Col, Row-1, 0);
          end;
          DirtyRect := Rect(Col, Row-1, Col+1, Row);
          if StyleToUse <> 1 then
            Windows.UnionRect(Data.UpdateRect, DirtyRect, Data.UpdateRect);
        end
        else
        begin
          DirtyRect   := GetBlockBounds(Data, Puzzle, Col, Row);
          BlockBounds := GetBlockBounds(Data, False , Col, Row);
          if StyleToUse <> 1 then
            Windows.UnionRect(Data.UpdateRect, DirtyRect, Data.UpdateRect);
          if Draw then
          begin
            if Puzzle
            then
            begin
              if((Col + Row) mod 2) = 0
              then
              begin
                if BlockBounds.Right - BlockBounds.Left = BlockData.PuzzleV_1.Width
                then
                begin
                  if BlockBounds.Bottom - BlockBounds.Top = BlockData.PuzzleH_1.Height
                  then PuzzleMask := BlockData.PuzzleV_1
                  else PuzzleMask := BlockData.PuzzleV_3;
                end
                else
                begin
                  if BlockBounds.Bottom - BlockBounds.Top = BlockData.PuzzleH_2.Height
                  then PuzzleMask := BlockData.PuzzleV_2
                  else PuzzleMask := BlockData.PuzzleV_4;
                end;

                BitBlt(MaskBmp.Canvas.Handle, BlockBounds.Left,
                  BlockBounds.Top - BlockData.ExtSize, PuzzleMask.Width,
                  PuzzleMask.Height, PuzzleMask.Canvas.Handle, 0, 0, cmSrcAnd);
                if Col = 1 then
                  MaskBmp.Canvas.FillRect(Rect(BlockBounds.Left,
                    BlockBounds.Top, BlockBounds.Left + BlockData.ExtSize,
                    BlockBounds.Bottom));
                if Col = ColsToUse then
                  MaskBmp.Canvas.FillRect(Rect(BlockBounds.Right - BlockData.ExtSize,
                    BlockBounds.Top, BlockBounds.Right, BlockBounds.Bottom));
              end
              else
              begin
                if BlockBounds.Right - BlockBounds.Left = BlockData.PuzzleV_1.Width
                then
                begin
                  if BlockBounds.Bottom - BlockBounds.Top = BlockData.PuzzleH_1.Height
                  then PuzzleMask := BlockData.PuzzleH_1
                  else PuzzleMask := BlockData.PuzzleH_3;
                end
                else
                begin
                  if BlockBounds.Bottom - BlockBounds.Top = BlockData.PuzzleH_2.Height
                  then PuzzleMask := BlockData.PuzzleH_2
                  else PuzzleMask := BlockData.PuzzleH_4;
                end;

                BitBlt(MaskBmp.Canvas.Handle, BlockBounds.Left - BlockData.ExtSize,
                  BlockBounds.Top, PuzzleMask.Width, PuzzleMask.Height,
                  PuzzleMask.Canvas.Handle, 0, 0, cmSrcAnd);
                if Row = 1 then
                  MaskBmp.Canvas.FillRect(Rect(BlockBounds.Left,
                    BlockBounds.Top, BlockBounds.Right,
                    BlockBounds.Top + BlockData.ExtSize));
                if Row = RowsToUse then
                  MaskBmp.Canvas.FillRect(Rect(BlockBounds.Left,
                    BlockBounds.Bottom - BlockData.ExtSize, BlockBounds.Right,
                    BlockBounds.Bottom));
              end;
            end
            else MaskBmp.Canvas.FillRect(BlockBounds);
          end;
        end;
        if CalcDirtyRects and Data.AllowDeviceUpdate then
          Data.DirtyRects.AddRect(DirtyRect);
      end;
    end;
  end;
end;

procedure GetDualData(var Frame: Longint; Frames: Longint);
begin
  if Frame and $1 = $1
  then Frame := (Frame shr 1) + 1
  else Frame := Frames - (Frame shr 1) + 1;
end;

function TBlockTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiThreadSafe,
      tetiUseDirtyRects
    ];
end;

procedure TBlockTransition.StyleBlockByArray(Data: TTETransitionData;
  CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
var
  Value: Longint;
begin
  if Dual then
    GetDualData(CurrentFrame, TotFrames);
  Value := TBlockData(Data.Custom).BlocksOrder[CurrentFrame];
  Row   := Value shr 16;
  Col   := Value and $0000FFFF;
end;

procedure TBlockTransition.StyleBlockByArrayReversed(Data: TTETransitionData;
  CurrentFrame, TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
var
  Value: Longint;
begin
  if Dual then
    GetDualData(CurrentFrame, TotFrames);
  Value := TBlockData(Data.Custom).BlocksOrder[TotFrames - CurrentFrame + 1];
  Row   := Value shr 16;
  Col   := Value and $0000FFFF;
end;

procedure TBlockTransition.Style2_1Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  if Dual then
    GetDualData(CurrentFrame, TotFrames);

  Col := ((CurrentFrame - 1) mod ColsToUse) + 1;
  Row := ((CurrentFrame - 1) div ColsToUse) + 1;
end;

procedure TBlockTransition.Style2_2Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  Style2_1Block(Data, TotFrames - CurrentFrame + 1, TotFrames, Col, Row, False);
end;

procedure TBlockTransition.Style2_3Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  if Dual then
    GetDualData(CurrentFrame, TotFrames);

  Col := ((CurrentFrame - 1) mod ColsToUse) + 1;
  Row := RowsToUse - (((CurrentFrame - 1) div ColsToUse));
end;

procedure TBlockTransition.Style2_4Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  Style2_3Block(Data, TotFrames - CurrentFrame + 1, TotFrames, Col, Row, False);
end;

procedure TBlockTransition.Style2_5Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  if Dual then
    GetDualData(CurrentFrame, TotFrames);

  Col := ((CurrentFrame - 1) div RowsToUse) + 1;
  Row := ((CurrentFrame - 1) mod RowsToUse) + 1;
end;

procedure TBlockTransition.Style2_6Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  Style2_5Block(Data, TotFrames - CurrentFrame + 1, TotFrames, Col, Row, False);
end;

procedure TBlockTransition.Style2_7Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  if Dual then
    GetDualData(CurrentFrame, TotFrames);

  Col := ColsToUse - (((CurrentFrame - 1) div RowsToUse));
  Row := ((CurrentFrame - 1) mod RowsToUse) + 1;
end;

procedure TBlockTransition.Style2_8Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  Style2_7Block(Data, TotFrames - CurrentFrame + 1, TotFrames, Col, Row, False);
end;

procedure TBlockTransition.Style3_1Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  if Dual then
    GetDualData(CurrentFrame, TotFrames);

  Row := ((CurrentFrame - 1) div ColsToUse) + 1;
  if(Row mod 2) = 1
  then Col := ((CurrentFrame - 1) mod ColsToUse) + 1
  else Col := ColsToUse - ((CurrentFrame - 1) mod ColsToUse)
end;

procedure TBlockTransition.Style3_2Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  Style3_1Block(Data, TotFrames - CurrentFrame + 1, TotFrames, Col, Row, False);
end;

procedure TBlockTransition.Style3_3Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  if Dual then
    GetDualData(CurrentFrame, TotFrames);

  Col := ((CurrentFrame - 1) div RowsToUse) + 1;
  if(Col mod 2) = 1
  then Row := ((CurrentFrame - 1) mod RowsToUse) + 1
  else Row := RowsToUse - ((CurrentFrame - 1) mod RowsToUse);
end;

procedure TBlockTransition.Style3_4Block(Data: TTETransitionData; CurrentFrame,
  TotFrames: Longint; var Col, Row: Integer; Dual: Boolean);
begin
  Style3_3Block(Data, TotFrames - CurrentFrame + 1, TotFrames, Col, Row, False);
end;

{ TBlockData }

destructor TBlockData.Destroy;
begin
  if Assigned(PuzzleV_1) then
  begin
    PuzzleV_1.Canvas.Unlock;
    PuzzleV_1.Free;
  end;
  if Assigned(PuzzleV_2) then
  begin
    PuzzleV_2.Canvas.Unlock;
    PuzzleV_2.Free;
  end;
  if Assigned(PuzzleV_3) then
  begin
    PuzzleV_3.Canvas.Unlock;
    PuzzleV_3.Free;
  end;
  if Assigned(PuzzleV_4) then
  begin
    PuzzleV_4.Canvas.Unlock;
    PuzzleV_4.Free;
  end;
  if Assigned(PuzzleH_1) then
  begin
    PuzzleH_1.Canvas.Unlock;
    PuzzleH_1.Free;
  end;
  if Assigned(PuzzleH_2) then
  begin
    PuzzleH_2.Canvas.Unlock;
    PuzzleH_2.Free;
  end;
  if Assigned(PuzzleH_3) then
  begin
    PuzzleH_3.Canvas.Unlock;
    PuzzleH_3.Free;
  end;
  if Assigned(PuzzleH_4) then
  begin
    PuzzleH_4.Canvas.Unlock;
    PuzzleH_4.Free;
  end;

  FreeMem(BlocksOrder);

  inherited;
end;

procedure TBlockTransition.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'BlockHeight'];
  if Value <> '' then
    BlockHeight := StrToInt(Value);

  Value := List.Values[Prefix + 'BlockWidth'];
  if Value <> '' then
    BlockWidth := StrToInt(Value);

  Value := List.Values[Prefix + 'Cols'];
  if Value <> '' then
    Cols := StrToInt(Value);

  Value := List.Values[Prefix + 'Dual'];
  if Value <> '' then
    Dual := SameText(Value, BoolToStr(True));

  Value := List.Values[Prefix + 'Puzzle'];
  if Value <> '' then
    Puzzle := SameText(Value, BoolToStr(True));

  Value := List.Values[Prefix + 'Rows'];
  if Value <> '' then
    Rows := StrToInt(Value);
end;

procedure TBlockTransition.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
var
  Prop: String;
begin
  inherited;

  Prop := Prefix + 'BlockHeight';
  if(not OmitDefaultValues) or
    (
      (BlockHeight <> 50) and
      (Rows = 0)
    ) then
    List.Values[Prop] := IntToStr(BlockHeight);

  Prop := Prefix + 'BlockWidth';
  if(not OmitDefaultValues) or
    (
      (BlockWidth <> 50) and
      (Cols = 0)
    ) then
    List.Values[Prop] := IntToStr(BlockWidth);

  Prop := Prefix + 'Cols';
  if(not OmitDefaultValues) or
    (
      (Cols <> 0) and
      (BlockWidth = 0)
    ) then
    List.Values[Prop] := IntToStr(Cols);

  Prop := Prefix + 'Dual';
  if(not OmitDefaultValues) or
    (Dual <> False) then
    List.Values[Prop] := BoolToStr(Dual);

  Prop := Prefix + 'Puzzle';
  if(not OmitDefaultValues) or
    (Puzzle <> False) then
    List.Values[Prop] := BoolToStr(Puzzle);

  Prop := Prefix + 'Rows';
  if(not OmitDefaultValues) or
    (
      (Rows <> 0) and
      (BlockHeight = 0)
    ) then
    List.Values[Prop] := IntToStr(Rows);
end;

initialization

  TERegisterTransition(TBlockTransition);

end.
