unit uConfig;

interface

uses
  IniFiles, Classes;

type
  TRecord = class
    FFieldType: string;
    FPos,
    FLen: Integer;
    FType,
    FRequire: string;
    FPad: Char;
  end;

//  TConfigList = class(TStringList)
//  public
//    function GetRecord(section, ident: string): TRecord;
//  end;

  TConfig = class(TStringList)
    constructor Create;
    destructor Destroy; override;
  public
    function GetSection(section: string): TStringList;
    function GetRecord(section, ident: string): TRecord;
    function GetRecordByPos(section: string; X: Integer): TRecord;
    function GetRecordByIdent(section, Ident: string): TRecord;
    function GetTemplate(section: string): TStringList;
  private
    FIni: TIniFile;
    //FConfigList: TConfigList;

    procedure ReadIni;
    function NewSection(section: string): TStringList;
    function GetType(field: string): string;
    function NewRecord(section, ident: string): TRecord;
    function Valid(section, ident: string; fields: TArray<string>): Boolean;
    function IsNumber(S: string): Boolean;
  end;

  function GetRecType(line: string): string;

var
  Config: TConfig;

implementation

uses
  SysUtils, Dialogs, StrUtils;

function GetRecType(line: string): string;
begin
  Result := LeftStr(line, 2).Trim;
end;

{ TConfig }

constructor TConfig.Create;
var
  fn: string;
begin
//  inherited Create;
  fn := ChangeFileExt(ParamStr(0), '.ini');
  FIni := TIniFile.Create(fn);
//  FConfigList := TConfigList.Create;
  ReadIni;
end;

destructor TConfig.Destroy;
begin
//  FConfigList.Free;
  FIni.Free;
  inherited;
end;

procedure TConfig.ReadIni;
var
  SectionList: TStrings;
  section: string;
begin
  try
    SectionList := TStringList.Create;
    FIni.ReadSections(SectionList);
    for section in SectionList do begin
      try
        Self.AddObject(section.ToUpper, NewSection(section))
      except

      end;
    end;
  finally
    SectionList.Free;
  end;
end;

function TConfig.NewSection(section: string): TStringList;
var
  SectionList,
  IdentList: TStrings;
  ident: string;
begin
  try
    Result := TStringList.Create;
    SectionList := TStringList.Create;
    FIni.ReadSection(section, SectionList);
    for ident in SectionList do
      Result.AddObject(ident.ToUpper, NewRecord(section, ident))
  finally
    SectionList.Free;
  end;
end;

function TConfig.NewRecord(section, ident: string): TRecord;
var
  fields: TArray<string>;
  value: string;
  X: Integer;
begin
  Result := TRecord.Create;
  value := FIni.ReadString(section, ident, '');
  fields := value.Split([',']);
  if not Valid(section, ident, fields) then Exit;

  Result.FFieldType := ident;
  Result.FPos := 0;
  if TryStrToInt(fields[0], X) then
    Result.FPos := X;

  Result.FLen := 0;
  if TryStrToInt(fields[1], X) then
    Result.FLen := X;

  Result.FType := GetType(fields[2]);
  Result.FRequire := fields[3];

  Result.FPad := ' ';
  if (Length(fields) = 5) and (fields[4].Length > 0) then
    Result.FPad := fields[4][1];
end;

function TConfig.IsNumber(S: string): Boolean;
var
  value: Integer;
begin
  Result := False;
  if TryStrToInt(S, value) then
    Result := True;
end;

function TConfig.Valid(section, ident: string; fields: TArray<string>): Boolean;
const
  _lf = #13#10;
begin
  Result := True;
  if (Length(fields) < 4) or
     (not IsNumber(fields[0])) or
     (not IsNumber(fields[1]))
  then begin
    Result := False;
    raise Exception.Create('Invalid format in .ini:' + _lf + section + _lf + ident);
  end;
end;

//function TConfig.GetRecord(section, ident: string): TRecord;
//begin
//  Result := FConfigList.GetRecord(section, ident);
//end;

function TConfig.GetTemplate(section: string): TStringList;
begin
  Result := TStringList.Create;
//  FIni.ReadSection(section, Result);
  FIni.ReadSectionValues(section, Result);
end;

function TConfig.GetType(field: string): string;
begin
  Result := field.Replace('<COMMA>', ',', [rfReplaceAll]);
end;

function TConfig.GetSection(section: string): TStringList;
var
  index: Integer;
begin
  Result := nil;
  index := Self.IndexOf(section.ToUpper + 'RECORD');
  if index > -1 then
    Result := TStringList(Self.Objects[index])
  else
    //ShowMessage(section.ToUpper + 'RECORD section is not defined in .ini file');
    ;
end;

function TConfig.GetRecord(section, ident: string): TRecord;
var
  index: Integer;
  sl: TStrings;
begin
  Result := nil;
  sl := GetSection(section);
  if sl <> nil then begin
    index := sl.IndexOf(ident.ToUpper);
    if index > -1 then
      Result := TRecord(sl.Objects[index]);
  end;
end;

function TConfig.GetRecordByPos(section: string; X: Integer): TRecord;
var
  sl: TStringList;
  I: Integer;
  rec: TRecord;
begin
  sl := GetSection(section);
  if sl = nil then Exit;

  for I := 0 to sl.Count - 1 do begin
    rec := TRecord(sl.Objects[I]);
    if (rec.FPos <= X) and (X < rec.FPos + rec.FLen) then begin
      Result := rec;
      Break;
    end;
  end;
end;

function TConfig.GetRecordByIdent(section, Ident: string): TRecord;
var
  sl: TStringList;
  I: Integer;
begin
  sl := GetSection(section);
  I := sl.IndexOf(Ident);
  if I > -1 then
    Result := TRecord(sl.Objects[I]);
end;

initialization
  Config := TConfig.Create;

finalization
  Config.Free;

end.
