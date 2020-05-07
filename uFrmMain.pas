unit uFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtCtrls, IOUtils,
  Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.Grids,
  uFrmDemo, uConfig, uDetails, uItemized, Vcl.Buttons, Vcl.ToolWin;

type
  TSearchOption = (soIgnoreCase, soFromStart, soWrap);
  TSearchOptions = set of TSearchOption;

  TFrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnuNew: TMenuItem;
    mnuOpen: TMenuItem;
    mnuExit: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    btnOpen: TButton;
    ActionList1: TActionList;
    actUndo: TAction;
    cbxFileList: TComboBox;
    PageControl1: TPageControl;
    tabContent: TTabSheet;
    tabDetailed: TTabSheet;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    edStartPos: TEdit;
    edSelected: TEdit;
    edContent: TEdit;
    btnReplace: TButton;
    pnlRecord: TPanel;
    Panel3: TPanel;
    btnSave: TButton;
    pageDetails: TPageControl;
    Panel4: TPanel;
    Panel5: TPanel;
    mmDataType: TMemo;
    Label5: TLabel;
    Panel6: TPanel;
    mmRequirment: TMemo;
    Label6: TLabel;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    SaveDialog: TSaveDialog;
    cmbFieldType: TComboBox;
    btnOpenFile: TButton;
    tabItemized: TTabSheet;
    pageItemized: TPageControl;
    Label7: TLabel;
    edSearch: TEdit;
    GroupBox1: TGroupBox;
    rbtnPSSN: TRadioButton;
    rbtnSSSN: TRadioButton;
    edSSN: TEdit;
    sbtnRefresh: TSpeedButton;
    sbtnApply: TSpeedButton;
    Panel7: TPanel;
    sbtnSearch: TSpeedButton;
    Memo1: TMemo;
    ToolBar1: TToolBar;
    tbtnNew: TToolButton;
    procedure mnuNewClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnReplaceClick(Sender: TObject);
    procedure edStartPosKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edSelectedKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edStartPosExit(Sender: TObject);
    procedure edSelectedExit(Sender: TObject);
    procedure lbxFileListDblClick(Sender: TObject);
    procedure edSelectedChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure cbxFileListChange(Sender: TObject);
    procedure edContentKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PageControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cmbFieldTypeClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject;
      var AllowChange: Boolean);
    procedure edSearchKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sbtnApplyClick(Sender: TObject);
    procedure sbtnRefreshClick(Sender: TObject);
    procedure cbxFileListDropDown(Sender: TObject);
    procedure sbtnSearchClick(Sender: TObject);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tbtnNewClick(Sender: TObject);
  private
    FPath: string;
    FDemoFile: string;
    FFileName: string;
    FFrmDemo: TFrmDemo;
    FUndoList: TStringList;
    FDetails: TDetails;
    FItemized: TItemized;
    FPrevFieldType: string;
    FFromTab: TTabSheet;
    startpos: Integer;

    procedure CreateDemo(fileName: string);
    procedure OpenDemo(fileName: string);
    procedure PopulateFileList;
    procedure OpenFile(fileName: string);
    procedure ReplaceClicked;
    function GetRecord(lineno: Integer): string;
    procedure SetMemoCaret(Key: Word = 13);
    procedure SaveClicked;
    procedure PopulateDetails;
    procedure CleanupDetails;
    procedure SetDataType(Value: string);
//    procedure SaveDetails;
    function GetRowsAsLine(SG: TStringGrid): string;
    function WrapText(content: string): string;
    procedure ParseRecordGridToMemo;
    function RecordGridsAsString: string;
    procedure OpenAuto;
    procedure FindDetailItem(rectype: string; X: Integer);
    procedure PopulateFieldType(FieldType: string);
    procedure PopulateItemized;
    procedure SearchKey(srch: string);
    procedure ReplaceSSN(Primary: Boolean; SSN: string);
    procedure SearchDisplay(SearchText: string);

    property DemoFile: string read FDemoFile write FDemoFile;
    property Demo: TFrmDemo read FFrmDemo write FFrmDemo;
    property DataType: string write SetDataType;
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  XMLIntf, XMLDoc, StrUtils, ShellApi,
  uFrmTPG, TPG.TokenizerService;

{$R *.dfm}


{ Singletons }
{$Region}

function IsNumber(S: string): Boolean;
var
  value: Double;
  code: Integer;
begin
  val(s, value, code);
  Result := code = 0;
end;

function IsTPGFile(fName: string): Boolean;
var
  fn: string;
  ext: string;
  tmp: string;
begin
  Result := False;
  fn := ExtractFileName(fName);
  ext := ExtractFileExt(fName);
  tmp := Copy(fn, 2,6);
  if ( (fn[1] in ['F', 'Y', 'R']) and
       (IsNumber(tmp)) and
       (IsNumber(ext))
     ) or
     (
       ext = '.ACK'
     )
  then
    Result := True;
end;

function GetEditVal(Edit: TEdit): Integer;
var
  value, code: Integer;
begin
  Result := 0;
  Val(Edit.Text, value, code);
  if code = 0 then
    Result := value;
end;

{$EndRegion}

{ Events }
{$Region Events}

procedure TFrmMain.FormActivate(Sender: TObject);
begin
  PopulateFileList;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  rec: TRecord;
begin
  FFrmDemo := TFrmDemo.Create(nil);
  FUndoList := TStringList.Create;
  FDetails := TDetails.Create;
  FItemized := TItemized.Create(StatusBar);
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  try
    FUndoList.Clear;
    FUndoList.Free;
    FDetails.Free;
    FItemized.Free;
  except
  end;
end;

procedure TFrmMain.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
//
end;

procedure TFrmMain.FormDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
//
end;

procedure TFrmMain.FormResize(Sender: TObject);
begin
  StatusBar.Panels[1].Width := 200;
  StatusBar.Panels[0].Width := Width - StatusBar.Panels[1].Width - 5;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  if ParamStr(1) <> '' then
    OpenAuto;
end;

procedure TFrmMain.Memo1Click(Sender: TObject);
var
  Coordinate: TPoint;
begin
  Coordinate := Memo1.CaretPos;
  pnlRecord.Caption := GetRecord(Coordinate.Y);
  if pnlRecord.Caption = '' then Exit;

  if FPrevFieldType <> pnlRecord.Caption then begin
    FPrevFieldType := pnlRecord.Caption;
    PopulateFieldType(FPrevFieldType);
  end;

  edStartPos.Text := (Coordinate.X + 1).ToString;
  edSelected.Text := Memo1.SelLength.ToString;
  FindDetailItem(pnlRecord.Caption, Coordinate.X + 1);
  startpos := 0;
end;

procedure TFrmMain.Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Memo1Click(Sender);
end;

procedure TFrmMain.mnuNewClick(Sender: TObject);
var
  frm: TFrmTPG;
  jira: string;
begin
  try
    frm := TFrmTPG.Create(nil);
    frm.ShowModal;
    if frm.ModalResult = mrCancel then Exit;
  finally
    frm.Free;
  end;

//  if FileExists(DemoFile) then
//      if MessageDlg('File already exists. Do you want to overwrite?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;

//  CreateDemo(DemoFile);
//  OpenDemo(DemoFile);
end;

procedure TFrmMain.mnuOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute() then
    OpenDemo(OpenDialog.FileName);
end;

procedure TFrmMain.btnReplaceClick(Sender: TObject);
begin
  ReplaceClicked;
end;

procedure TFrmMain.btnSaveClick(Sender: TObject);
begin
  if MessageDlg('Do you want to save?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;

  SaveClicked;
end;

procedure TFrmMain.btnOpenFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    FPath := ExtractFilePath(OpenDialog.FileName);
    if FPath = '' then
      FPath := IncludeTrailingBackslash(GetCurrentDir);

    FFileName := ExtractFileName(OpenDialog.FileName);
    OpenFile(FFileName);
  end;
end;

procedure TFrmMain.cbxFileListChange(Sender: TObject);
begin
  btnOpen.Click;
end;

procedure TFrmMain.cbxFileListDropDown(Sender: TObject);
begin
  PopulateFileList;
end;

procedure TFrmMain.actUndoExecute(Sender: TObject);
begin
  Memo1.Perform(EM_UNDO, 0, 0);
end;

procedure TFrmMain.btnOpenClick(Sender: TObject);
begin
  if cbxFileList.ItemIndex = -1 then begin
    FFileName := '';
    Exit;
  end;

  FFileName := cbxFileList.Items[cbxFileList.ItemIndex];
  OpenFile(FFileName);
end;

procedure TFrmMain.lbxFileListDblClick(Sender: TObject);
begin
  btnOpen.Click;
end;

procedure TFrmMain.edContentKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    btnReplace.Click;
end;

procedure TFrmMain.edSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    SearchKey(edSearch.Text);
end;

procedure TFrmMain.edSelectedChange(Sender: TObject);
var
  len: Integer;
  S: string;
begin
  len := GetEditVal(edSelected);
  if len > 0 then begin
    edContent.MaxLength := len;
    S := edContent.Text;
    if S.Length > len then
      edContent.Text := S.Substring(1, len);
  end;
end;

procedure TFrmMain.edSelectedExit(Sender: TObject);
begin
  if (edSelected.Text <> '') and (edSelected.Text <> '0') then
    SetMemoCaret;
end;

procedure TFrmMain.edSelectedKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) and (edSelected.Text <> '') then begin
    edStartPosKeyUp(Sender, Key, Shift);
  end;
end;

procedure TFrmMain.edStartPosExit(Sender: TObject);
begin
  if ActiveControl.ClassName = 'TTabSheet' then Exit;

  if (edStartPos.Text <> '') then
    SetMemoCaret;
end;

procedure TFrmMain.edStartPosKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 13) and (edStartPos.Text <> '') then
    SetMemoCaret;
end;

procedure TFrmMain.PageControl1Change(Sender: TObject);
begin
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  if PageControl1.ActivePage = tabContent then
  begin
    //ParseRecordGridToMemo;
    //CleanupDetails;
  end
  else
  if PageControl1.ActivePage = tabDetailed then
  begin
    PopulateDetails;
  end
  else
  if PageControl1.ActivePage = tabItemized then
  begin
    PopulateItemized;
  end;
end;

procedure TFrmMain.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  if PageControl1.ActivePage = tabItemized then
  begin
    Memo1.Text := FItemized.GetContent(pageItemized);
    FItemized.Clear;
  end;

  if PageControl1.ActivePage = tabDetailed then
  begin
    Memo1.Text := FDetails.GetContent(pageDetails);
    FDetails.Clear;
  end;
end;

procedure TFrmMain.sbtnApplyClick(Sender: TObject);
begin
  if Length(edSSN.Text) < 9 then Exit;

  if PageControl1.ActivePage = tabContent then
    ReplaceSSN(rbtnPSSN.Checked, edSSN.Text);

  if PageControl1.ActivePage = tabItemized then
    FItemized.ReplaceSSN(rbtnPSSN.Checked, edSSN.Text);

  if PageControl1.ActivePage = tabDetailed then
    FDetails.ReplaceSSN(rbtnPSSN.Checked, edSSN.Text);
end;

procedure TFrmMain.sbtnRefreshClick(Sender: TObject);
var
  I: Integer;
  SSN: string;
  TokenizerService : TTokenizerService;
begin
  SSN := '';
  Randomize;
  for I := 1 to 9 do
    SSN := SSN + Random(9).ToString;

  TokenizerService := TTokenizerService.Create(SSN);
  try
    edSSN.Text := TokenizerService.Tokenize;
    edSSN.Hint := SSN;
  finally
    TokenizerService.Free;
  end;
end;

procedure TFrmMain.sbtnSearchClick(Sender: TObject);
begin
  SearchKey(edSearch.Text);
end;

{$EndRegion}

procedure TFrmMain.ReplaceSSN(Primary: Boolean; SSN: string);
var
  line: string;
  rectype: string;
  Strings: TStringList;
  S: string;
  startSSN: string;
  rec: TRecord;
  I: Integer;
  temp: string;
begin
  startSSN := 'PRIMARY';
  if not Primary then startSSN := 'SECONDARY';

  for I := 0 to Memo1.Lines.Count - 1 do
  begin
    line := Memo1.Lines[I];
    rectype := GetRecType(line);
    if (rectype = 'H') or (rectype = 'T') then Continue;

    Strings := Config.GetSection(rectype);
    for S in Strings do
    begin
      if S.StartsWith(startSSN, True) and S.ToUpper.Contains('SSN') then
      begin
        rec := Config.GetRecordByIdent(rectype, S);
        temp := line.Substring(rec.FPos - 1, 9);
        Memo1.Lines[I] := line.Replace(temp, SSN);
        Break;
      end;
    end;
  end;

  Memo1.CaretPos := TPoint.Create(rec.FPos - 1, 1);
  Memo1.SelLength := 9;
  if Memo1.CanFocus then
    Memo1.SetFocus;
end;

procedure TFrmMain.PopulateFileList;
var
  fname: string;
begin
  FPath := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
  cbxFileList.Clear;
  for fname in TDirectory.GetFiles(FPath) do begin
    if IsTPGFile(fname) then
      cbxFileList.Items.Add(ExtractFileName(fname));
  end;
  cbxFileList.OnChange := cbxFileListChange;
end;

procedure TFrmMain.PopulateFieldType(FieldType: string);
begin
  if FieldType = '' then Exit;
  try
    cmbFieldType.Items := TStrings(Config.GetSection(FieldType.Trim));
  except
    StatusBar.Panels[0].Text := 'Bad field type: ' + FieldType.Trim;
  end;
end;

procedure TFrmMain.OpenAuto;
var
  dir: string;
  fn: string;
begin
  if ParamStr(1) = '' then Exit;

  FPath := ExtractFileDir(ParamStr(1));
  if FPath = '' then
    FPath := GetCurrentDir;

  FPath := IncludeTrailingBackslash(FPath);
  FFileName := ExtractFileName(ParamStr(1));
  if not FileExists(FPath + FFileName) then Exit;

  OpenFile(FFileName);
end;

procedure TFrmMain.CreateDemo(fileName: string);
begin
  //To do: set display to zero
  Demo.CreateDemo(DemoFile);
end;

procedure TFrmMain.SetMemoCaret(Key: Word = 13);
var
  Y: Integer;
begin
  if (Key = 13) and (GetEditVal(edStartPos) > 0) then begin
    Y := Memo1.CaretPos.Y;
    Memo1.CaretPos := TPoint.Create(StrToInt(edStartPos.Text) - 1, Y);
    if (edSelected.Text <> '') and (edSelected.Text <> '0') then
      Memo1.SelLength := StrToInt(edSelected.Text);

    Memo1.Perform(EM_SCROLLCARET, 0, 0);
    Memo1.SetFocus;
  end;
end;

procedure TFrmMain.tbtnNewClick(Sender: TObject);
begin
  mnuNewClick(Sender);
end;

procedure TFrmMain.OpenDemo(fileName: string);
begin
  FFrmDemo.OpenDemo(fileName);
  FFrmDemo.PanelDemo.Parent := FrmMain;
end;

procedure TFrmMain.OpenFile(fileName: string);
var
  fn: string;
begin
  fn := FPath + fileName;
  if not FileExists(fn) then Exit;

  FrmMain.Caption := fn;
  Memo1.OnClick := Memo1Click;
  PageControl1.ActivePageIndex := 0;
  Memo1.Lines.LoadFromFile(FPath + fileName);
  Memo1.Lines.Text := WrapText(Memo1.Lines.Text);
  if Memo1.CanFocus then
    Memo1.SetFocus;
  Memo1.OnClick(self);
end;

function TFrmMain.WrapText(content: string): string;
var
  len: Integer;
  S: string;
begin
  Result := '';
  content := content.Replace(#13#10, '');
  len := content.Length;
  while len >= 512 do begin
    Result := Result + LeftStr(content, 512) + #13#10;
    content := content.Remove(0, 512);
    len := content.Length;
  end;
end;

procedure TFrmMain.ReplaceClicked;
var
  Coordinate: TPoint;
  at: Integer;
  len: Integer;
  S: string;
begin
  at := GetEditVal(edStartPos);
  len := GetEditVal(edSelected);
  S := edContent.Text;
  if (at = 0) or (len = 0) then Exit;

  if S.Length = 0 then
    S := string.Create(' ', len)
  else
  if S.Length < len then
    S := S.PadRight(len, ' ')
  else
  if S.Length > len then
    S := LeftStr(S, len);

  Coordinate := Memo1.CaretPos;
  Memo1.Lines[ Coordinate.Y ] := StuffString(Memo1.Lines[ Coordinate.Y ], at, S.Length, S);
  SetMemoCaret;
end;

function TFrmMain.GetRecord(lineno: Integer): string;
begin
  Result := LeftStr(Memo1.Lines[lineno], 2);
end;

procedure TFrmMain.PopulateDetails;
begin
  if Memo1.Lines.Count = 0 then Exit;

  FDetails.Populate(Memo1.Text, pageDetails);
end;

procedure TFrmMain.PopulateItemized;
begin
  FItemized.Populate(Memo1.Text, pageItemized);
end;

procedure TFrmMain.CleanupDetails;
begin
  FDetails.Clear;
end;

procedure TFrmMain.cmbFieldTypeClick(Sender: TObject);
var
  rec: TRecord;
begin
  if cmbFieldType.ItemIndex = -1 then Exit;
  rec := Config.GetRecordByIdent(Trim(pnlRecord.Caption), cmbFieldType.Text);
  if rec = nil then Exit;

  edStartPos.Text := rec.FPos.ToString;
  edSelected.Text := rec.FLen.ToString;
  SetMemoCaret;
end;

procedure TFrmMain.SetDataType(Value: string);
begin
  mmDataType.Lines.Text := Value;
end;

procedure TFrmMain.SaveClicked;
var
  fn: string;
  bak: string;
begin
  if FFileName = '' then Exit;

  fn := FPath + FFileName;
  TFile.Copy(fn, ChangeFileExt(fn, '.bak'), True);

  if PageControl1.ActivePage = tabItemized then
    Memo1.Text := FItemized.GetContent(pageItemized);

  if PageControl1.ActivePage = tabDetailed then
    Memo1.Text := FDetails.GetContent(pageDetails);

  Memo1.Lines.SaveToFile(fn);
end;

//procedure TFrmMain.SaveDetails;
//var
//  I: Integer;
//  Tab: TTabSheet;
//  SG: TStringGrid;
//  content: string;
//  Strings: TStrings;
//begin
//  content := RecordGridsAsString;
//
//  Strings := TStringList.Create;
//  try
//    Strings.Text := content;
//    Strings.SaveToFile(FPath + FFileName);
//  finally
//    Strings.Free;
//  end;
//end;

function TFrmMain.RecordGridsAsString: string;
var
  I: Integer;
  Tab: TTabSheet;
  SG: TStringGrid;
begin
  Result := '';
  for I := 0 to pageDetails.PageCount - 1 do begin
    Tab := pageDetails.Pages[I];
    if Tab.Controls[0] = nil then begin
      Continue;
    end;

    SG := TStringGrid(Tab.Controls[0]);
    Result := Result + GetRowsAsLine(SG) + #13#10;
  end;
end;

function TFrmMain.GetRowsAsLine(SG: TStringGrid): string;
var
  I: Integer;
  maxlen: Integer;
  S: string;
begin
  Result := '';
  for I := 1 to SG.RowCount - 1 do begin
    if SG.Cells[0, I] = '' then Continue;
    maxlen := SG.Cells[2, I].ToInteger;
    S := SG.Cells[5, I].TrimRight;
    if S.Length < maxlen then
      S := S.PadRight(maxlen, ' ')
    else
    if S.Length > maxlen then
      S := S.Substring(0, maxlen);
    Result := Result + S;
  end;
end;

procedure TFrmMain.ParseRecordGridToMemo;
begin
  Memo1.Text := RecordGridsAsString;
end;

procedure TFrmMain.FindDetailItem(rectype: string; X: Integer);
var
  rec: TRecord;
  I: Integer;
begin
  rec := Config.GetRecordByPos(rectype.Trim, X);
  if rec = nil then Exit;

  I := cmbFieldType.Items.IndexOf(rec.FFieldType);
  if I > -1 then
    cmbFieldType.ItemIndex := I;
end;

function SearchMemoText(
    Control: TMemo;
    Search: string;
    SearchOptions: TSearchOptions): Boolean;
var
  Text: string;
  Index: Integer;
begin
  if soIgnoreCase in SearchOptions then
  begin
    Search := UpperCase(Search);
    Text := UpperCase(Control.Text);
  end
  else
    Text := Control.Text;

  Index := 0;
  if not (soFromStart in SearchOptions) then
    Index := PosEx(Search, Text,
         Control.SelStart + Control.SelLength + 1);

  if (Index = 0) and
      ((soFromStart in SearchOptions) or
       (soWrap in SearchOptions)) then
    Index := PosEx(Search, Text, 1);

  Result := Index > 0;
  if Result then
  begin
    Control.SelStart := Index - 1;
    Control.SelLength := Length(Search);
    Control.SetFocus;
  end;
end;

procedure TFrmMain.SearchDisplay(SearchText: string);
var
  I: Integer;
  L: Integer;
begin
  SearchMemoText(Memo1, SearchText, []);
//  startpos := Pos(SearchText, Memo1.Text, startpos);
//  if startpos > 0 then begin
//    L := SendMessage(Memo1.Handle, EM_LINEFROMCHAR, I - 1, 0);
//    Memo1.SelStart := I - 1;
//    Memo1.SelLength := Length(SearchText);
//    Memo1.SetFocus;
//  end;
end;

procedure TFrmMain.SearchKey(srch: string);
begin
  if srch = '' then Exit;

  if PageControl1.ActivePage = tabContent then
    SearchDisplay(srch);

  if PageControl1.ActivePage = tabItemized then
    FItemized.SearchDisplay(srch);

  if PageControl1.ActivePage = tabDetailed then
    FDetails.SearchDisplay(srch);
end;

{ TMemo }

//procedure TMemo.CreateWnd;
//begin
//  inherited;
//  DragAcceptFiles(Handle, True);
//end;
//
//procedure TMemo.DestroyWnd;
//begin
//  inherited;
//
//end;
//
//procedure TMemo.WMDropFiles(var Message: TWMDropFiles);
//var
//  NumFiles : longint;
//  i : longint;
//  buffer : array[0..255] of char;
//begin
// {How many files are being dropped}
////  NumFiles := DragQueryFile(Message.Drop,
////                            -1,
////                            nil,
////                            0);
// {Accept the dropped files}
////  for i := 0 to (NumFiles - 1) do begin
//    DragQueryFile(Message.Drop,
//                  i,
//                  @buffer,
//                  sizeof(buffer));
//    Lines.Text := buffer;
////  end;
//end;
//
//procedure TMemo.OnEndDrag(Sender, Target: TObject; X, Y: Integer);
//begin
////
//end;

end.
