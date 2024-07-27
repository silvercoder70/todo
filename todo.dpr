program todo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uCommands in 'uCommands.pas',
  uModel in 'uModel.pas',
  uDatabase in 'uDatabase.pas';

function FindCmdLineSwitch2(AShortSwitch,
                            ALongSwitch: string): boolean;
begin
  Result := False;
  if (AShortSwitch <> '') then
    Result := FindCmdLineSwitch(AShortSwitch);
  if (not Result) and (ALongSwitch <> '') then
    Result := FindCmdLineSwitch(ALongSwitch);
end;


function FindCmdLineSwitch3(AShortSwitch,
                            ALongSwitch: string;
                            var AValue: string): boolean;
begin
  Result := False;
  if (AShortSwitch <> '') then
    Result := FindCmdLineSwitch(AShortSwitch, AValue);
  if (not Result) and (ALongSwitch <> '') then
    Result := FindCmdLineSwitch(ALongSwitch, AValue);
end;


var
  LArgText: string;
  LToDo: TToDoCommand;

begin
  try
    LToDo := TToDoCommand.Create;

    if FindCmdLineSwitch3('i', 'add', LArgText) then
    begin
      if LArgText = '' then
      begin
        WriteLn('Missing description of item to add');
        Exit;
      end;

      LToDo.AddItem(LArgText)
    end
    else if FindCmdLineSwitch2('l', 'list') then
      LToDo.List(False)
    else if FindCmdLineSwitch2('la', 'listall') then
      LToDo.List(True)
    else if FindCmdLineSwitch3('x', 'done', LArgText) then
    begin
      if LArgText = '' then
      begin
        WriteLn('Missing items to update');
        Exit;
      end;
      LToDo.Complete(LArgText);
    end
    else if FindCmdLineSwitch3('d', 'delete', LArgText) then
    begin
      if LArgText = '' then
      begin
        WriteLn('Missing items to delete');
        Exit;
      end;
      LToDo.Delete(LArgText);
    end
    else if FindCmdLineSwitch2('t', 'touch') then
      LToDo.Touch;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
