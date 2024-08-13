unit uModel;

interface

uses
  Classes,
  SysUtils;
  //System.Generics.Collections;

type
  TToDoItem = class(TObject)
  private
    FCreatedDate: TDateTime;
    FDescription: string;
    FCompleted: boolean;
    FCompletedDate: TDateTime;
    FPriority: Char;
    procedure SetCompleted(const Value: boolean);
    procedure SetCompletedDate(const Value: TDateTime);
    procedure SetCreatedDate(const Value: TDateTime);
    procedure SetDescription(const Value: string);
    procedure CheckCreatedDate;
    procedure SetPriority(const Value: Char);
  public
    constructor Create; overload;
    constructor Create(AString: string); overload;
    function AsString: string;
    function IsValidCreatedDate: boolean;
    property Completed: boolean read FCompleted write SetCompleted;
    property Priority: Char read FPriority write SetPriority;
    property CompletedDate: TDateTime read FCompletedDate write SetCompletedDate;
    property CreatedDate: TDateTime read FCreatedDate write SetCreatedDate;
    property Description: string read FDescription write SetDescription;
  end;

implementation

uses
  Character;

function StrToken(var S: string; Separator: Char): string;
var
  NPos: integer;
begin
  NPos := Pos(Separator, S);
  if NPos >= 1 then
  begin
    Result := Copy(S, 1, NPos-1);
    S := Trim(Copy(S, NPos, MaxInt));
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

{ TToDoItem }

procedure TToDoItem.CheckCreatedDate;
begin
  if FCreatedDate < EncodeDate(1980, 1, 1) then
    FCreatedDate := Date;
end;

constructor TToDoItem.Create(AString: string);
var
  WorkString: string;
  Token: string;
  Settings: TFormatSettings;
begin
  self.Create;
  if AString = '' then
    Exit;

  //Settings := TFormatSettings.Create;
  Settings.ShortDateFormat := 'yyyy-mm-dd';
  Settings.DateSeparator := '-';

  WorkString := AString;
  FCompleted := WorkString.StartsWith('x ');
  if FCompleted then
    WorkString := WorkString.Substring(2);

  if WorkString[1] = '(' then
  begin
    Token := StrToken(WorkString, ' ');
    FPriority := Token[2];
  end;


  if WorkString[1].IsDigit then
  begin
    Token := StrToken(WorkString, ' ');
    FCreatedDate := StrToDate(Token, Settings);
  end;

  if WorkString[1].IsDigit then
  begin
    Token := StrToken(WorkString, ' ');
    FCompletedDate := FCreatedDate;
    FCreatedDate := StrToDate(Token, Settings);
  end;

  self.FDescription := WorkString;
end;

function TToDoItem.IsValidCreatedDate: boolean;
begin
  Result := FCreatedDate > EncodeDate(1980, 1, 1);
end;

constructor TToDoItem.Create;
begin
  FCreatedDate := 0;
  FPriority := '-';
  FDescription := 'Empty';
  FCompleted := False;
  FCompletedDate := 0;
end;

procedure TToDoItem.SetCompleted(const Value: boolean);
begin
  FCompleted := Value;
  if FCompleted then
  begin
    FCompletedDate := Now;
    CheckCreatedDate;
  end;
end;

procedure TToDoItem.SetCompletedDate(const Value: TDateTime);
begin
  FCompletedDate := Value;
  FCompleted := True;
  CheckCreatedDate;
end;

procedure TToDoItem.SetCreatedDate(const Value: TDateTime);
begin
  FCreatedDate := Value;
end;

procedure TToDoItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TToDoItem.SetPriority(const Value: Char);
begin
  FPriority := Value;
end;

function TToDoItem.AsString: string;

  function DateValToStr(ADate: TDateTime): string; begin
    Result := FormatDateTime('yyyy-mm-dd', ADate);
  end;

begin
  Result := '';
  if self.Completed then
    Result := Result + 'x ';
  if self.FPriority <> '-' then
    Result := Result + '(' + self.FPriority + ') ';
  if self.FCompletedDate > 1 then
    Result := Result + DateValToStr(self.FCompletedDate) + ' ';
  if self.FCreatedDate > 1 then
    Result := Result + DateValToStr(self.FCreatedDate) + ' ';
  Result := Result + self.FDescription;
end;

end.
