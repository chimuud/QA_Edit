unit uDetails;

interface

uses
  Vcl.Grids, Vcl.StdCtrls, SysUtils, Classes, ComCtrls, Controls, System.Types, Graphics,
  uIPage;

type
  TDetails = class(TInterfacedObject, IPage)
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Populate(Content: string; page: TPageControl);
    function GetContent(aPage: TPageControl): string;
    procedure ReplaceSSN(Primary: Boolean; SSN: string);
    procedure SearchDisplay(SearchText: string);
  private
    FStrings: TStrings;
    FPage: TPageControl;
    function AddRecordTab(rectype: string): TStringGrid;
    procedure ParseDetails(SG: TStringGrid; line: string);
    procedure SGClick(Sender: TObject);
    procedure SGKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    function CreateNewGrid(rectype: string; Tab: TTabsheet): TStringGrid;
    procedure SGDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure SGSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure SGKeyPress(Sender: TObject; var Key: Char);
  end;

implementation

uses
  StrUtils, Forms, Dialogs,
  uConfig, uFrmMain;

{ TDetails }

constructor TDetails.Create;
begin
  FStrings := TStringList.Create;
end;

destructor TDetails.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TDetails.GetContent(aPage: TPageControl): string;
var
  I: Integer;
  Tab: TTabSheet;
  SG: TStringGrid;
  name: string;
  Strings: TStrings;

  function CollectValue: string;
  var
    row: Integer;
  begin
    Result := '';
    for row := 1 to SG.RowCount - 1 do
    begin
      if SG.Cells[2, row].Trim <> '' then
        Result := Result + SG.Cells[5, row].PadRight(SG.Cells[2, row].ToInteger);
    end;
  end;

begin
  Strings := TStringList.Create;
  try
    for I := 0 to aPage.PageCount - 1 do
    begin
      Tab := aPage.Pages[I];
      name := Tab.Name;
      SG := TStringGrid(Tab.Components[0]);
      Strings.Add(CollectValue());
    end;
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

{function GetRecType(line: string): string;
begin
  Result := LeftStr(line, 2).Trim;
end;}

procedure TDetails.Populate(Content: string; page: TPageControl);
var
  line: string;
  rectype: string;
  col: Integer;
  SG: TStringGrid;
begin
  FStrings.Text := Content;
  FPage := page;

  for line in FStrings do begin
    SG := AddRecordTab(GetRecType(line));
    if SG = nil then Continue;

    ParseDetails(SG, line);
  end;
  if FPage.PageCount > 1 then
    FPage.ActivePageIndex := 1;
end;

procedure TDetails.ReplaceSSN(Primary: Boolean; SSN: string);
var
  I: Integer;
  Tab: TTabSheet;
  name: string;
  SG: TStringGrid;
  row: Integer;
  startSSN: string;
  S: string;
begin
  startSSN := 'PRIMARY';
  if not Primary then startSSN := 'SECONDARY';

  for I := 0 to FPage.PageCount - 1 do
  begin
    Tab := FPage.Pages[I];
    S := Tab.Name;
    S := name.Substring(3, 1);
    if (S = 'H') or (S = 'T') then Continue;

    SG := TStringGrid(Tab.Components[0]);
    for row := 1 to SG.RowCount - 1 do
    begin
      S := Uppercase(SG.Cells[0, row]);
      if S.StartsWith(startSSN) and S.Contains('SSN') then
      begin
        SG.Cells[5, row] := SSN;
        Break;
      end;
    end;
  end;
end;

procedure TDetails.Clear;
var
  Tab: TTabSheet;
  name: string;
  SG: TStringGrid;
begin
  if FPage = nil then Exit;

  while FPage.ActivePage <> nil do begin
    Tab := FPage.ActivePage;
    name := ReplaceStr(Tab.Name, 'tab', 'grd');
    SG := TStringGrid(Tab.FindChildControl(name));
    FreeAndNil(SG);
    FreeAndNil(Tab);
  end;
end;

function TDetails.AddRecordTab(rectype: string): TStringGrid;
var
  Tab: TTabSheet;
  sg1: TStringGrid;
begin
  if rectype.Trim = '' then begin
    ShowMessage('Illegal RecordType detected');
    Exit;
  end;

  Tab := TTabSheet.Create(FPage);
  Tab.PageControl := FPage;
  Tab.Parent := FPage;
  Tab.Name := 'tab' + rectype + 'Record';
  Tab.Caption := rectype + ' Record';

  Result := CreateNewGrid(rectype, Tab);
end;

function TDetails.CreateNewGrid(rectype: string; Tab: TTabsheet): TStringGrid;
begin
  Result := TStringGrid.Create(Tab);
  Result.Parent := Tab;
  Result.Name := 'grd' + rectype + 'Record';
  Result.Options := [goEditing, {goFixedVertLine, }goFixedHorzLine, goVertLine, goHorzLine, goColSizing];
  Result.ColCount := 6;
  Result.RowCount := 2;
  Result.FixedCols := 5;
  Result.FixedRows := 1;
  Result.Align := alClient;
  Result.ScrollBars := ssBoth;
  Result.Cells[0, 0] := 'Field Type';
  Result.Cells[1, 0] := 'Pos';
  Result.Cells[2, 0] := 'Len';
  Result.Cells[5, 0] := 'Value';
  Result.ColWidths[0] := 200;
  Result.ColWidths[1] := 40;
  Result.ColWidths[2] := 40;
  Result.ColWidths[3] := 0;
  Result.ColWidths[4] := 0;
  Result.ColWidths[5] := FrmMain.pageDetails.Width - 320;
  Result.OnDrawCell := SGDrawCell;
  Result.OnSelectCell := SGSelectCell;
  Result.OnKeyPress := SGKeyPress;
  Result.OnKeyUp := SGKeyUp;
end;

procedure TDetails.ParseDetails(SG: TStringGrid; line: string);
var
  SectionList: TStrings;
  rec: TRecord;
  I: Integer;
  fldtype: string;
  row: Integer;
begin
  SectionList := Config.GetSection(GetRecType(line));
  if SectionList = nil then Exit;

  for I := 0 to SectionList.Count - 1 do begin
    fldtype := SectionList[I];
    rec := TRecord(SectionList.Objects[I]);
    row := SG.RowCount - 1;
    SG.Cells[0, row] := fldtype;
    SG.Cells[1, row] := rec.FPos.ToString;
    SG.Cells[2, row] := rec.FLen.ToString;
    SG.Cells[3, row] := rec.FType.Replace('<LF>', #13#10).Replace('<COMMA>', ',');
    SG.Cells[4, row] := rec.FRequire.Replace('<LF>', #13#10).Replace('<COMMA>', ',');
    SG.Cells[5, row] := line.Substring(rec.FPos - 1, rec.FLen);
    SG.RowCount := SG.RowCount + 1;
  end;
  SG.Options := [goColSizing, goEditing, goFixedVertLine, goFixedHorzLine,
    goVertLine, goHorzLine];
end;

{ TStringGrid Events >>> }

procedure TDetails.SearchDisplay(SearchText: string);
var
  Tab: TTabSheet;
  SG: TStringGrid;
  row: Integer;
  S: string;
  found: Boolean;
  currow: Integer;
  V: string;
begin
  found := False;
  SearchText := SearchText.ToUpper;

  if FPage = nil then Exit;

  Tab := FPage.ActivePage;
  if Tab = nil then Exit;

  SG := TStringGrid(Tab.Components[0]);
  currow := SG.Row;
  for row := SG.Row + 1 to SG.RowCount - 1 do
  begin
    S := Uppercase(SG.Cells[0, row]);
    V := Uppercase(SG.Cells[5, row]);
    if S.Contains(SearchText) or V.Contains(SearchText) then
    begin
      found := True;
      SG.Row := row;
      Break;
    end;
  end;
  if not found then
    SG.Row := 1;
end;

procedure TDetails.SGClick(Sender: TObject);
begin

end;

procedure TDetails.SGKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  SG: TStringGrid;
begin
  SG := Sender as TStringGrid;
  FrmMain.StatusBar.Panels[1].Text := 'Trimmed value length = ' + SG.Cells[5, SG.Row].Trim.Length.ToString;
end;

procedure TDetails.SGDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  SG: TStringGrid;
begin
  SG := Sender as TStringGrid;
  if (ARow = 0) or (ACol < 3) then begin
    SG.Canvas.Brush.Color := clBtnFace;
    SG.Canvas.FillRect(Rect);
    SG.Canvas.Font.Color := clBlack;
    SG.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, SG.Cells[ACol, ARow]);
  end;
end;

procedure TDetails.SGSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  SG: TStringGrid;
begin
  SG := Sender as TStringGrid;
  FrmMain.StatusBar.Panels[0].Text := SG.Cells[0, ARow];
  FrmMain.StatusBar.Panels[1].Text := 'Trimmed value length = ' + SG.Cells[5, ARow].Trim.Length.ToString;
  FrmMain.mmDataType.Text := SG.Cells[3, ARow];
  FrmMain.mmRequirment.Text := SG.Cells[4, ARow];
end;

procedure TDetails.SGKeyPress(Sender: TObject; var Key: Char);
var
  SG: TStringGrid;
begin
  if (Key in [#8]) then Exit;

  SG := Sender as TStringGrid;
  if SG.Cells[0, SG.Row] = '' then begin
    Key := #0;
    Exit;
  end;

  Application.ProcessMessages;
  if SG.Cells[5, SG.Row].Trim.Length + 1 > StrToInt(SG.Cells[2, SG.Row]) then begin
    Key := #0;
    Exit;
  end;
end;

{ <<< TStringGrid Event }

end.
