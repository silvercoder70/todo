unit uDatabase;

interface

uses
  Classes,
  SysUtils;

type
  TOpenMode = (openReadOnly, openReadWrite);

  TToDoDatabase = class
  private
    FMode: TOpenMode;
    FDatabase: string;
    FRows: TStringlist;

    procedure CreateEmptyFile;
  public
    constructor Create(ADatabase: string; AMode: TOpenMode); overload;
    destructor Destroy; override;
    procedure Append(AItem: string);
    function IDToRowIndex(AIndex: string): integer;
    property Rows: TStringList read FRows;
  end;

implementation

{ TToDoDatabase }

procedure TToDoDatabase.Append(AItem: string);
begin
  FRows.Add(AItem);
end;

constructor TToDoDatabase.Create(ADatabase: string; AMode: TOpenMode);
begin
  FDatabase := ADatabase;
  FMode := AMode;
  FRows := TStringlist.Create;
  if not FileExists(FDatabase) then
    CreateEmptyFile
  else
    FRows.LoadFromFile(FDatabase);
end;

procedure TToDoDatabase.CreateEmptyFile;
var
  LMode: Word;
begin
  LMode := fmOpenWrite;
  if not FileExists(FDatabase) then
    LMode := LMode or fmCreate;

  var todoFS := TFileStream.Create(FDatabase, LMode);
  todoFS.Free;
end;

destructor TToDoDatabase.Destroy;
begin
  if FMode = openReadWrite then
    FRows.SaveToFile(FDatabase);
  inherited;
end;

function TToDoDatabase.IDToRowIndex(AIndex: string): integer;
begin
  var LIdx := AIndex.ToInteger - 1;
  if (LIdx >= 0) and (LIdx < FRows.Count) then
    Result := LIdx
  else
    Result := -1;

end;

end.
