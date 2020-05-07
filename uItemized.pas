unit uItemized;

interface

uses
  Classes, ComCtrls, Forms, VCL.Controls, VCL.StdCtrls,
  uConfig, uIPage;

type
  TXPos = (xCol1 = 20, xCol2 = 236, xCol3 = 515, xCol4 = 731);

  TItemized = class(TInterfacedObject, IPage)
  private
    FStrings: TStrings;
    FPage: TPageControl;
    FStatusBar: TStatusBar;
    FEdited: Boolean;

    procedure Edit_OnEnter(Sender: TObject);
    procedure Edit_Change(Sender: TObject);
    procedure Edit_OnExit(Sender: TObject);

    function AddRecordTab(aLine: string): Boolean;
    procedure AddComponents(Scroll: TScrollBox; aLine: string);
    procedure AddEditBox(Scroll: TScrollBox; col, row: Integer; rec: TRecord);
    procedure PopulateValues(aContent: string; aPage: TPageControl);
    procedure GetScrollBox1(aLine: string; aPage: TPageControl);
    function GetScrollBox2(aPage: TPageControl; recType: string): TScrollBox;
    procedure ParseEditBox(aLine: string; Scroll: TScrollBox);
    function GetScrollBox(Tab: TTabSheet): TScrollBox;
  public
    constructor Create(aStatusBar: TStatusBar);
    destructor Destroy; override;

    procedure Clear;
    procedure Populate(aContent: string; aPage: TPageControl);
    function GetContent(aPage: TPageControl): string;
    procedure ReplaceSSN(Primary: Boolean; SSN: string);
    procedure SearchDisplay(SearchText: string);
  end;

implementation

uses
  System.StrUtils, SysUtils, Dialogs, VCL.Graphics
  ;

{ TItemized }

constructor TItemized.Create(aStatusBar: TStatusBar);
begin
  FStrings := TStringList.Create;
  FStatusBar := aStatusBar;
end;

destructor TItemized.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TItemized.Edit_Change(Sender: TObject);
begin
  FEdited := True;
end;

procedure TItemized.Edit_OnEnter(Sender: TObject);
var
  S: string;
begin
  FEdited := False;
  S := TEdit(Sender).HelpKeyword;
  FStatusBar.Panels[0].Text := S.Split([';'])[0];
  FStatusBar.Panels[1].Text := S.Split([';'])[1];
end;

procedure TItemized.Edit_OnExit(Sender: TObject);
var
  Edit: TEdit;
  name: string;
begin
  Edit := TEdit(Sender);
  name := Uppercase(Edit.Name);
  if FEdited and name.Contains('PRIMARY') and name.Contains('SSN') then
    ReplaceSSN(True, Edit.Text);

  FEdited := False;
end;

function TItemized.GetContent(aPage: TPageControl): string;
var
  Scroll: TScrollBox;
  I: Integer;
  Tab: TTabSheet;
  name: string;
  Strings: TStrings;
  rectype: string;

  function CollectValues: string;
  var
    J: Integer;
    Edit: TEdit;
    name2: string;
    value: string;
  begin
    Result := rectype;
    for J := 0 to Scroll.ComponentCount - 1 do
    begin
      Edit := TEdit(Scroll.Components[J]);
      name2 := Edit.Name;
      if name2.StartsWith('value', True) then
      begin
        value := Edit.Text;
        Result := Result + value.PadRight(Edit.MaxLength);
      end;
    end;
  end;

begin
  Strings := TStringList.Create;
  try
    for I := 0 to aPage.PageCount - 1 do
    begin
      Tab := aPage.Pages[I];
      name := Tab.Name;
      rectype := name.Replace('tab', '').Replace('Record', '').PadRight(2);
      name := name.Replace('tab', 'scroll');
      Scroll := TScrollBox(Tab.FindComponent(name));
      Strings.Add(CollectValues());
    end;
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

procedure TItemized.Populate(aContent: string; aPage: TPageControl);
var
  line: string;
begin
  FStrings.Text := aContent;
  FPage := aPage;
  Clear;

  for line in FStrings do
    AddRecordTab(line);
  PopulateValues(aContent, aPage);
  if FPage.PageCount > 1 then
    FPage.ActivePageIndex := 1;
end;

procedure TItemized.Clear;
var
  Tab: TTabSheet;
  I, J: Integer;
  name: string;
  Scroll: TScrollBox;
  Edit: TEdit;
begin
  FPage.Visible := False;
  while FPage.PageCount > 0 do
  begin
    Tab := FPage.Pages[0];
    name := Tab.Name;
    name := name.Replace('tab', 'scroll');
    Scroll := TScrollBox(Tab.FindComponent(name));
    while Scroll.ComponentCount > 0 do
    begin
      Edit := TEdit(Scroll.Components[0]);
      Edit.Free;
    end;
    Scroll.Free;
    Tab.Free;
  end;
  FPage.Visible := True;
end;

function TItemized.GetScrollBox(Tab: TTabSheet): TScrollBox;
var
  name: string;
begin
  name := Tab.Name;
  name := name.Replace('tab', 'scroll');
  Result := TScrollBox(Tab.FindComponent(name));
end;

function TItemized.AddRecordTab(aLine: string): Boolean;
var
  Tab: TTabSheet;
  Scroll: TScrollBox;
  rectype: string;
begin
  rectype := GetRecType(aLine);
  if rectype.Trim = '' then begin
    ShowMessage('Unknown RecordType detected');
    Exit;
  end;

  Tab := TTabSheet.Create(FPage);
  Tab.PageControl := FPage;
  Tab.Parent := FPage;
  Tab.Name := 'tab' + rectype + 'Record';
  Tab.Caption := rectype + ' Record';

  Scroll := TScrollBox.Create(Tab);
  Scroll.Parent := Tab;
  Scroll.Name := 'scroll' + rectype + 'Record';
  Scroll.Align := alClient;

  AddComponents(Scroll, aLine);
end;

procedure TItemized.AddComponents(Scroll: TScrollBox; aLine: string);
var
  SectionList: TStrings;
  I: Integer;
  fldtype: string;
  rec: TRecord;
  Edit: TEdit;
  col, row: Integer;
begin
  SectionList := Config.GetSection(GetRecType(aLine));
  if SectionList = nil then Exit;

  for I := 2 to SectionList.Count - 1 do begin
    fldtype := SectionList[I];
    rec := TRecord(SectionList.Objects[I]);
    col := (I - 2) mod 2;
    row := Round((I - 2) / 3);
    AddEditBox(Scroll, col, row, rec);
  end;
end;

procedure TItemized.AddEditBox(Scroll: TScrollBox; col, row: Integer; rec: TRecord);

  procedure Add(LE: string);
  var
    Edit: TEdit;
    hint: string;
  begin
    Edit := TEdit.Create(Scroll);
    Edit.Parent := Scroll;
    Edit.Top := 15 + row * 30;
    hint := rec.FFieldType + ' (' + rec.FPos.ToString + ',' + rec.FLen.ToString + ')';

    if LE.Contains('LABEL') then
    begin
      Edit.Name := 'labelEdit' + rec.FPos.ToString;
      Edit.Width := 210;
      Edit.ParentColor := True;
      Edit.Text := hint;
      Edit.Hint := hint;
      Edit.ShowHint := True;
      Edit.ReadOnly := True;
      Edit.TabStop := False;
      Edit.BorderStyle := bsNone;

      case col of
        0: Edit.Left := Integer(xCol1);
        1: Edit.Left := Integer(xCol3);
      end;
    end
    else
    begin
      Edit.Name := 'valueEdit' + rec.FPos.ToString;
      Edit.Width := 260;
      Edit.ParentColor := False;
      Edit.MaxLength := rec.FLen;
      Edit.Hint := hint;
      Edit.ShowHint := True;
      Edit.HelpKeyword := rec.FType + ';' + rec.FRequire;
      Edit.OnEnter := Edit_OnEnter;

      if rec.FRequire.ToUpper.Contains('REQUIRE') then
        Edit.Color := clInfoBk;

      case col of
        0: Edit.Left := Integer(xCol2);
        1: Edit.Left := Integer(xCol4);
      end;
    end;
  end;

begin
  Add('LABEL');
  Add('EDIT');
end;

procedure TItemized.PopulateValues(aContent: string; aPage: TPageControl);
var
  line: string;
begin
  FStrings.Text := aContent;

  for line in FStrings do begin
    GetScrollBox1(line, aPage);
  end;
end;

procedure TItemized.ReplaceSSN(Primary: Boolean; SSN: string);
var
  I: Integer;
  Scroll: TScrollBox;
  J: Integer;
  Edit: TEdit;
  name: string;
  value: string;
  startSSN: string;
begin
  startSSN := 'PRIMARY';
  if not Primary then startSSN := 'SECONDARY';

  for I := 0 to FPage.PageCount - 1 do
  begin
    Scroll := TScrollBox(FPage.Pages[I].Components[0]);
    name := Scroll.Name;
    name := name.Substring(6, 1);
    if (name[1] = 'H') or (name[1] = 'T') then Continue;

    for J := 0 to Scroll.ComponentCount - 1 do
    begin
      Edit := TEdit(Scroll.Components[J]);
      name := Edit.Name;
      value := Edit.Text;
      if name.StartsWith('label') and value.ToUpper.StartsWith(startSSN) and value.Contains('SSN') then
      begin
        name := name.Replace('label', 'value');
        Edit := TEdit(Scroll.FindComponent(name));
        Edit.Text := SSN;
        Break;
      end;
    end;
  end;
end;

procedure TItemized.SearchDisplay(SearchText: string);
var
  compName: string;
  Tab: TTabSheet;
  Scroll: TScrollBox;
  I: Integer;
  Edit: TEdit;
  S: string;
begin
  Tab := FPage.ActivePage;
  if Tab = nil then Exit;

  compName := Tab.Name;
  compName := compName.Replace('tab', 'scroll');
  Scroll := TScrollBox(Tab.FindComponent(compName));
  for I := 0 to Scroll.ComponentCount - 1 do
  begin
    Edit := TEdit(Scroll.Components[I]);
    S := Edit.Text;
    if S.ToUpper.Contains(SearchText.ToUpper) then
      Edit.Font.Style := [fsBold]
    else
      Edit.Font.Style := [];
  end;
end;

procedure TItemized.GetScrollBox1(aLine: string; aPage: TPageControl);
var
  rectype: string;
  Scroll: TScrollBox;
begin
  rectype := GetRecType(aLine);
  Scroll := GetScrollBox2(FPage, rectype);
  if Scroll <> nil then
  begin
    ParseEditBox(aLine, Scroll);
  end;
end;

function TItemized.GetScrollBox2(aPage: TPageControl; recType: string): TScrollBox;
var
  compName: string;
  Tab: TTabSheet;
begin
  Result := nil;
  compName := 'tab' + recType + 'Record';
  Tab := TTabsheet(aPage.FindComponent(compName));
  if Tab <> nil then begin
    compName := 'scroll' + rectype + 'Record';
    Result := TScrollBox(Tab.FindComponent(compName));
  end;
end;

procedure TItemized.ParseEditBox(aLine: string; Scroll: TScrollBox);
var
  SectionList: TStrings;
  I: Integer;
  rectype: string;
  rec: TRecord;
  value: string;
  Edit: TEdit;
begin
  SectionList := Config.GetSection(GetRecType(aLine));
  if SectionList = nil then Exit;

  rectype := GetRecType(aLine);
  for I := 2 to SectionList.Count - 1 do begin
    rec := TRecord(SectionList.Objects[I]);
    value := aLine.Substring(rec.FPos - 1, rec.FLen);
    Edit := TEdit(Scroll.FindComponent('valueEdit' + rec.FPos.ToString));
    if Edit <> nil then
      Edit.Text := value;
  end;

end;

end.
