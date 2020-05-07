unit uFrmTPG;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ADODB, Vcl.ExtCtrls,
  Vcl.Buttons;

type
  TColumnType = (lblCol, edCol);
  TLabelColumns = (lblCol1 = 20, lblCol2 = 400, lblCol3 = 780);
  TEditColumns = (edCol1 = 206, edCol2 = 586, edCol3 = 966);

  TEfin = class
    TransId: Integer;
    MasterEfin: string;
  end;

  TTemplate = class

  end;

  TFrmTPG = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    cmbSystemYear: TComboBox;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    cmbServer: TComboBox;
    Label2: TLabel;
    cmbTransEfin: TComboBox;
    Label4: TLabel;
    cmbMasterEfin: TComboBox;
    Label5: TLabel;
    cmbOfficeId: TComboBox;
    Label6: TLabel;
    cmbRecordType: TComboBox;
    Label7: TLabel;
    cmbSubType: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    edPrimarySSN: TEdit;
    edSecondarySSN: TEdit;
    sbtnTogglePSSN: TSpeedButton;
    sbtnRandPSSN: TSpeedButton;
    sbtnToggleSSSN: TSpeedButton;
    sbtnRandSSSN: TSpeedButton;
    ScrollBox: TScrollBox;
    CheckBox1: TCheckBox;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    sbtnCreate: TSpeedButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmbServerChange(Sender: TObject);
    procedure cmbTransEfinChange(Sender: TObject);
    procedure cmbMasterEfinChange(Sender: TObject);
    procedure cmbTransEfinKeyPress(Sender: TObject; var Key: Char);
    procedure cmbMasterEfinKeyPress(Sender: TObject; var Key: Char);
    procedure cmbRecordTypeClick(Sender: TObject);
    procedure sbtnCreateClick(Sender: TObject);
  private
    { Private declarations }
    connTPGSys: TAdoConnection;
    Query: TAdoQuery;

    procedure PopulateTransmitterEfin;
    procedure PopulateSystemYear;
    procedure PopulateMasterEfin(yyyy: string; Trans_Id: Integer);
    procedure PopulateOfficeId(yyyy: string; Trans_Id: Integer; MasterEfin: string);
    procedure LoadRTTemplate;
    procedure LoadYTemplate;
    procedure CreateFile;
  public
    { Public declarations }
  end;

var
  FrmTPG: TFrmTPG;

implementation

uses
  DateUtils,
  DBUtil, ConfigXML, uConfig;

{$R *.dfm}

procedure TFrmTPG.FormCreate(Sender: TObject);
var
  sConfigXML : IXMLConfigType;
  i: Integer;
  V: OleVariant;
  fServer: string;
  fDatabase: string;
  fFDdriverID: string;
  fFDodbcDriver: string;
begin
  PopulateSystemYear;
  connTPGSys := TADOConnection.Create(nil);
  Query := TAdoQuery.Create(nil);
  Query.Connection := connTPGSys;

  connTPGSys.ConnectionString := DBUtil.TDBConnection.CreateConnectionString(_DefaultConfigurationFileName, 'TPGSys');
  connTPGSys.Connected := True;
  if connTPGSys.Connected then
  begin
    cmbServer.ItemIndex := 0;
    cmbServerChange(Sender);
  end;
end;

procedure TFrmTPG.FormDestroy(Sender: TObject);
begin
  connTPGSys.Free;
end;

procedure TFrmTPG.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFrmTPG.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFrmTPG.cmbServerChange(Sender: TObject);
begin
  PopulateTransmitterEfin;
end;

procedure TFrmTPG.cmbTransEfinChange(Sender: TObject);
var
  Efin: TEfin;
  i: Integer;
begin
  if (Length(cmbTransEfin.Text) = 6) and (Length(cmbSystemYear.Text) = 4) then
  begin
    try
      Efin := TEfin(cmbTransEfin.Items.Objects[cmbTransEfin.ItemIndex]);
      PopulateMasterEfin(cmbSystemYear.Text, Efin.TransId);
    except
    end;
  end;
end;

procedure TFrmTPG.cmbTransEfinKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #8 then Exit;

  if not ('0123456789').Contains(Key) then
    Key := #0;
end;

procedure TFrmTPG.cmbRecordTypeClick(Sender: TObject);
begin
  if cmbRecordType.Text = 'R' then
    LoadRTTemplate
  else
  if cmbRecordType.Text = 'Y' then
    LoadYTemplate;
end;

procedure TFrmTPG.cmbMasterEfinChange(Sender: TObject);
var
  Efin: TEfin;
begin
  if (Length(cmbTransEfin.Text) = 6) and (Length(cmbSystemYear.Text) = 4) then
  begin
    try
      Efin := TEfin(cmbMasterEfin.Items.Objects[cmbTransEfin.ItemIndex]);
      PopulateOfficeId(cmbSystemYear.Text, Efin.TransId, Efin.MasterEfin);
    except
    end;
  end;
end;

procedure TFrmTPG.cmbMasterEfinKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #8 then Exit;
  if not ('0123456789').Contains(Key) then
    Key := #0;
end;

procedure TFrmTPG.PopulateSystemYear;
var
  yyyy: Word;
begin
  yyyy := YearOf(Today);
  cmbSystemYear.Items.Add(yyyy.ToString);
  cmbSystemYear.Items.Add((yyyy - 1).ToString);
  cmbSystemYear.Items.Add((yyyy - 2).ToString);
  cmbSystemYear.ItemIndex := 0;
end;

procedure TFrmTPG.PopulateTransmitterEfin;
var
  currYear: Integer;
  Efin: TEfin;
  yyyy: Integer;
begin
  currYear := YearOf(Today);
  yyyy := Trim(cmbSystemYear.Text).ToInteger;
  if not ((currYear - 3 <= yyyy) and (yyyy <= currYear)) then Exit;

  if connTPGSys.Connected then
  begin
    try
      try
        Query.SQL.Text :=
          ' SELECT Id, TransmitterEfin FROM Transmitter '+
          ' WHERE LastEnrollYear = ' + QuotedStr(cmbSystemYear.Text) +
          ' ORDER BY TransmitterEfin ';
        Query.Open;

        while not Query.Eof do
        begin
          Efin := TEfin.Create;
          Efin.TransId := Query.FieldByName('Id').AsInteger;
          cmbTransEfin.Items.AddObject(Query.FieldByName('TransmitterEfin').AsString, Efin);
          Query.Next;
        end;
        cmbTransEfin.ItemIndex := 0;
//        cmbTransEfinChange(nil);
      except
        on e: Exception do
          ShowMessage(e.Message);
      end;
    finally
      Query.Close;
    end;
  end;
end;

procedure TFrmTPG.sbtnCreateClick(Sender: TObject);
begin
  CreateFile;
end;

procedure TFrmTPG.PopulateMasterEfin(yyyy: string; Trans_Id: Integer);
var
  sql: string;
  Efin: TEfin;
begin
  sql :=
    ' SELECT DISTINCT MasterEfin FROM EroLinking EL '+
    ' JOIN MasterInfo M ON EL.Master_ID = M.Id '+
    ' WHERE '+
    '   Transmitter_ID = ' + Trans_Id.ToString +
    '   AND M.LastEnrollYear = ' + QuotedStr(yyyy) +
    ' ORDER BY MasterEfin ';

  if connTPGSys.Connected then
  begin
    try
      try
        Query.SQL.Text := sql;
        Query.Open;

        cmbMasterEfin.Clear;
        while not Query.Eof do
        begin
          Efin := TEfin.Create;
          Efin.TransId := Trans_Id;
          Efin.MasterEfin := Query.FieldByName('MasterEfin').AsString;
          cmbMasterEfin.Items.AddObject(Efin.MasterEfin, Efin);
          Query.Next;
        end;
        cmbMasterEfin.ItemIndex := 0;
//        cmbMasterEfinChange(nil);
      except
        on e: Exception do
          ShowMessage(e.Message);
      end;
    finally
      Query.Close;
    end;
  end;
end;

procedure TFrmTPG.PopulateOfficeId(yyyy: string; Trans_Id: Integer; MasterEfin: string);
var
  sql: string;
begin
  sql :=
    ' SELECT DISTINCT JHOfficeID FROM EroLinking EL '+
    ' JOIN MasterInfo M ON EL.Master_ID = M.Id '+
    ' WHERE '+
    ' 	Transmitter_ID = ' + Trans_Id.ToString +
    ' 	AND M.LastEnrollYear = ' + QuotedStr(yyyy) +
    ' 	AND M.MasterEFIN = ' + QuotedStr(MasterEfin) +
    ' ORDER BY JHOfficeID ';

  if connTPGSys.Connected then
  begin
    try
      try
        Query.SQL.Text := sql;
        Query.Open;

        cmbOfficeId.Clear;
        while not Query.Eof do
        begin
          cmbOfficeId.Items.Add(Query.FieldByName('JHOfficeID').AsString);
          Query.Next;
        end;
      except
        on e: Exception do
          ShowMessage(e.Message);
      end;
    finally
      cmbOfficeId.ItemIndex := 0;
      Query.Close;
    end;
  end;
end;

procedure TFrmTPG.LoadRTTemplate;
var
  strLabel, strComp, strSection,
  strField, strValue: string;
  row, col, top: Integer;
  lblEdit: TEdit;
  SL: TStrings;

  function GetColumn(coltype: TColumnType): Integer;
  begin
    if coltype = lblCol then
      case col of
        1: Result := Integer(lblCol1);
        2: Result := Integer(lblCol2);
        3: Result := Integer(lblCol3);
      end
    else
      case col of
        1: Result := Integer(edCol1);
        2: Result := Integer(edCol2);
        3: Result := Integer(edCol3);
      end;
  end;

  procedure CreateLabel;
  begin
    lblEdit := TEdit.Create(ScrollBox);
    lblEdit.Parent := ScrollBox;
    lblEdit.Top := top;
    lblEdit.Left := GetColumn(lblCol);
    lblEdit.Width := 180;
    lblEdit.Text := strLabel;
    lblEdit.ReadOnly := True;
    lblEdit.ParentColor := True;
    lblEdit.Name := 'lblEdit' + col.ToString + '_' + row.ToString;
  end;

  procedure CreateCheckBox;
  var
    chkbx: TCheckBox;
    LabelColumns: TLabelColumns;
  begin
    CreateLabel;

    chkbx := TCheckBox.Create(ScrollBox);
    chkbx.Parent := ScrollBox;
    chkbx.Top := top;
    chkbx.Left := GetColumn(edCol);
    chkbx.Width := 15;
    chkbx.Hint := strField;
    chkbx.Checked := strValue = 'X';
    chkbx.Name := 'chkbx' + col.ToString + '_' + row.ToString;
  end;

  procedure CreateComboBox(strValue: string);
  var
    cmbbx: TComboBox;
  begin
    CreateLabel;

    cmbbx := TComboBox.Create(ScrollBox);
    cmbbx.Parent := ScrollBox;
    cmbbx.Top := top;
    cmbbx.Left := GetColumn(edCol);
    cmbbx.Width := 180;
    cmbbx.Items.DelimitedText := strValue;
    cmbbx.TextHint := strField;
    cmbbx.Name := 'cmbbx' + col.ToString + '_' + row.ToString;
    if cmbbx.Items.Count > 0 then
      cmbbx.ItemIndex := 0;
  end;

  function GetMaxLength: Integer;
  var
    rectype: string;
    rec: TRecord;
  begin
    Result := 0;
    SL.Delimiter := '.';
    SL.DelimitedText := strField;
    if SL.Count < 2 then
      raise Exception.Create('.ini file has invalid field: ' + strField);

    rectype := SL[0].Replace('Record', '', [rfIgnoreCase]);
    rec := Config.GetRecord(rectype, SL[1]);
    Result := rec.FLen;
  end;

  procedure CreateEditBox(strLabel, strField, strValue: string);
  var
    lblEdit: TEdit;
    edEdit: TEdit;
    LabelColumns: TLabelColumns;
    rec: TRecord;
  begin
    CreateLabel;

//    strField
//    rec := Config.GetRecordByPos(rectype.Trim, X);

    edEdit := TEdit.Create(ScrollBox);
    edEdit.Parent := ScrollBox;
    edEdit.Top := top;
    edEdit.Left := GetColumn(edCol);
    edEdit.Width := 180;
    edEdit.MaxLength := GetMaxLength;
    edEdit.TextHint := strField;
    edEdit.Text := strValue;
    edEdit.Name := 'edEdit' + col.ToString + '_' + row.ToString;
  end;

  procedure CreateComponent(S: string);
  begin
    SL.StrictDelimiter := True;
    SL.Delimiter := '=';
    SL.DelimitedText := S;
    strLabel := SL[0];

    SL.Delimiter := '^';
    SL.DelimitedText := SL[1];
    if SL.Count < 2 then
    begin
      ShowMessage('.ini file has error: ' + S);
      Exit;
    end;
    strField := SL[0];
    strValue := SL[1];

    if (strValue.StartsWith('[')) and (strValue.EndsWith(']')) and (strValue.Contains(',')) then
    begin
      strValue := strValue.Replace('[','').Replace(']','').Trim.ToUpper;
      CreateComboBox(strValue);
    end
    else
    if (strValue.StartsWith('[')) and (strValue.EndsWith(']')) and (not strValue.Contains(',')) then
    begin
      strValue := strValue.Replace('[','').Replace(']','').Trim.ToUpper;
      CreateCheckBox;
    end
    else
      CreateEditBox(strLabel, strField, strValue);
  end;

  procedure LoadRTInit;
  var
    Strings: TStringList;
    S: string;
  begin
    row := 1; col := 1;
    top := 16;
    Strings := Config.GetTemplate('RTTemplate');
    try
      for S in Strings do
      begin
        CreateComponent(S);
        Inc(col);
        if (col - 1) mod 3 = 0 then
        begin
          Inc(row);
          top := 16 + (row - 1) * 30;
          col := 1;
        end;
      end;
    finally
      Strings.Free;
    end;
  end;

begin
  SL := TStringList.Create;
  try
    cmbSubType.Items.DelimitedText := 'T,P,I,D,L';
    LoadRTInit;
  finally
    SL.Free;
  end;
end;

procedure TFrmTPG.LoadYTemplate;
begin
  cmbSubType.Items.DelimitedText := 'S,M';
end;

procedure TFrmTPG.CreateFile;
var
  ext: string;
  num: Integer;
  line: string;

  function CreateFileName: string;
  begin
    ext := num.ToString.PadLeft(3, '0');
    Result := IncludeTrailingBackslash(GetCurrentDir) + 'F' + cmbTransEfin.Text + '.' + ext;
  end;

  function FindNextFile: string;
  begin
    num := 1;
    Result := CreateFileName;
    while FileExists(Result) do
    begin
      Inc(num);
      Result := CreateFileName;
    end;
  end;

  function GetScreenValue(rectype, Ident: string): string;
  var
    idx: Integer;
    S: string;
    fld: string;
    srchHint: string;
    edit: TEdit;
    chk: TCheckBox;
    cmb: TComboBox;
  begin
    Result := '';
    srchHint := rectype.ToUpper + '.' + Ident.ToUpper;
    for idx := 0 to ScrollBox.ComponentCount - 1 do
    begin
      S := ScrollBox.Components[idx].Name;
      if S.StartsWith('lblEdit') then
        Continue
      else
      if S.StartsWith('edEdit') then
        edit := TEdit(ScrollBox.Components[idx]);
        if edit.Hint.ToUpper.Equals(srchHint) then
        begin
          Result := edit.Text;
          Break;
        end
      else
      if S.StartsWith('chkbx') then
      begin
        chk := TCheckBox(ScrollBox.Components[idx]);
        if chk.Hint.ToUpper.Equals(srchHint) then
        begin
          if chk.Checked then
            Result := 'X';
          Break;
        end;
      end
      else
      if S.StartsWith('cmbbx') then
      begin
        cmb := TComboBox(ScrollBox.Components[idx]);
        Result := cmb.Text;
        Break;
      end;
    end;
  end;

var
  FileLineList: TStrings;

  procedure CreateHeader;
  var
    SectionList: TStringList;
    I: Integer;
    rec: TRecord;
    value: string;
  begin
    SectionList := Config.GetSection('H');
    line := '';
    for I := 0 to SectionList.Count - 1 do
    begin
      rec := Config.GetRecord('H', SectionList[I]);
      if I = 0 then
        value := 'H'
      else
        value := GetScreenValue('HRecord', SectionList[I]);

      line := line + value.PadRight(rec.FLen, rec.FPad);
    end;
    FileLineList.Add(line);
  end;

begin
  FileLineList := TStringList.Create;
  try
    if cmbRecordType.Text = 'R' then
    begin
      CreateHeader;
    end;
  finally
    FileLineList.SaveToFile(FindNextFile);
    FileLineList.Free;
  end;
end;

end.
