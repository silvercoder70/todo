unit uCommands;

interface

uses
  Classes,
  SysUtils,
  uDatabase;

type
  TToDoCommand = record
  const
    DropBoxDir = 'dropbox';
  private
    FDatabasePath: string;
    FDatabaseFile: string;
    function CsvToList(AItems: string): TStringList;
  public
    class function Create: TToDoCommand; static;
    procedure AddItem(AItem: string);
    procedure List(AShowAll: boolean);
    procedure Complete(AItems: string);
    procedure Delete(AItems: string);
    procedure Touch;
  end;

implementation

{ TToDoCommand }

uses uModel,
  System.IOUtils;


class function TToDoCommand.Create: TToDoCommand;
begin
  Result.FDatabasePath :=
    TPath.Combine(GetEnvironmentVariable('USERPROFILE'), DropBoxDir);
  if not DirectoryExists(Result.FDatabasePath) then
    Result.FDatabaseFile := ''
  else
    Result.FDatabaseFile := TPath.Combine(Result.FDatabasePath, 'todo.txt');
end;

function TToDoCommand.CsvToList(AItems: string): TStringList;
begin
  Result := TStringList.Create;
  Result.CommaText := AItems;

  {ensure the list is in some sort of order for delete operations
  and even if the list is in ascending order, that is good enough
  at the moment}
  Result.Sort;
end;

procedure TToDoCommand.Delete(AItems: string);
var
  todoDb: TToDoDatabase;
begin
  todoDb := TToDoDatabase.Create(FDatabaseFile, openReadWrite);

  var LDeleteList := CsvToList(AItems);

  for var I := LDeleteList.Count-1 downto 0 do
  begin
    var LIdx := todoDb.IDToRowIndex(LDeleteList[I]);
    if (LIdx >= 0) then
    begin
      var LItemStr := todoDb.Rows[LIdx];
      var LItem := TToDoItem.Create(LItemStr);
      if not LItem.Completed then
        todoDb.Rows.Delete(LIdx);
      LItem.Free;
    end;
  end;

  LDeleteList.Free;
  todoDb.Free;
end;

procedure TToDoCommand.AddItem(AItem: string);
var
  todoDb: TToDoDatabase;
  Item: TToDoItem;
  ItemStr: string;
begin
  Item := TToDoItem.Create;
  Item.Description := AItem;
  ItemStr := Item.AsString + sLineBreak;
  Item.Free;

  todoDb := TToDoDatabase.Create(FDatabaseFile, openReadWrite);
  todoDb.Append(ItemStr);
  todoDb.Free;
end;

procedure TToDoCommand.List(AShowAll: boolean);
var
  todoDb: TToDoDatabase;
begin
  todoDb := TToDoDatabase.Create(FDatabaseFile, openReadOnly);
  for var I := 0 to todoDb.Rows.Count-1 do
  begin
    var LItemStr := todoDb.Rows[I];
    var DisplayFlag := (LItemStr <> '');
    if DisplayFlag and (not AShowAll) then
    begin
      var LItem := TToDoItem.Create(LItemStr);
      DisplayFlag := not LItem.Completed;
      LItem.Free;
    end;
    if DisplayFlag then
      WriteLn(I+1:4, ': ' + LItemStr);
  end;
  todoDb.Free;
end;

procedure TToDoCommand.Touch;
var
  todoDb: TToDoDatabase;
begin
  todoDb := TToDoDatabase.Create(FDatabaseFile, openReadWrite);
  for var I := 0 to todoDb.Rows.Count-1 do
  begin
    var LItemStr := todoDb.Rows[I];
    var LItem := TToDoItem.Create(LItemStr);
    if (not LItem.Completed) and
      (not LItem.IsValidCreatedDate) then
    begin
      LItem.CreatedDate := Now;
      todoDb.Rows[I] := LItem.AsString;
    end;
    LItem.Free;
  end;
  todoDb.Free;
end;

procedure TToDoCommand.Complete(AItems: string);
var
  todoDb: TToDoDatabase;
begin
  todoDb := TToDoDatabase.Create(FDatabaseFile, openReadWrite);

  var LUpdateList := CsvToList(AItems);

  for var LItemID in LUpdateList do
  begin
    var LIdx := todoDb.IDToRowIndex(LItemID);
    if (LIdx > 0) then
    begin
      var LItem := TToDoItem.Create(todoDb.Rows[LIdx]);
      LItem.Completed := True;
      todoDb.Rows[LIdx] := LItem.AsString;
      LItem.Free;
    end;
  end;

  LUpdateList.Free;
  todoDb.Free;
end;

end.


