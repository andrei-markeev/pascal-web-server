unit TaskManager;

{$mode objfpc}

interface

uses
    {$ifdef UNIX}cthreads,{$endif} classes, sysutils, lnet;

const
    MAX_THREAD_POOL_SIZE = 100;

type
    TTaskStatus = (tsPending, tsExecuting, tsCancelled, tsDone);
    TTask = class
    public
        status: TTaskStatus;
        procedure Execute; virtual; abstract;
        procedure Finalize; virtual; abstract;
    end;

    TTaskManager = class
    private
        workerThreadIds: array [0..MAX_THREAD_POOL_SIZE - 1] of ptrint;
        threadPoolSize: integer;
    public
        constructor Create;
        destructor Destroy; override;
        procedure SocketDisconnected(socket: TLSocket);
        procedure Enqueue(task: TTask);
        procedure FinalizeTasks;
    end;

implementation

var
    tasks: TThreadList;
    terminateWorkers: array [0..MAX_THREAD_POOL_SIZE - 1] of integer;
    newTasksAvailable: integer = 0;
    doneTasksAvailable: integer = 0;

function Worker(p: pointer): ptrint;
var
    task: TTask;
    i: integer;
    tasksAvailable: integer;
    idx: integer;
    needTerminate: integer;
begin
    idx := integer(p);
    repeat
        tasksAvailable := InterlockedExchange(newTasksAvailable, 0);

        if tasksAvailable > 0 then
        begin
            // if there were more than one task available,
            // let's return those available tasks back so
            // that they can be consumed by other threads
            for i := 1 to tasksAvailable - 1 do
                InterlockedIncrement(newTasksAvailable);

            task := nil;

            with tasks.LockList do
            begin
                for i := 0 to Count - 1 do
                begin
                    task := TTask(Items[i]);
                    if task.status = tsPending then
                    begin
                        task.status := tsExecuting;
                        break;
                    end;
                end;
            end;
            tasks.UnlockList;

            if task <> nil then
            begin
                task.Execute;
                task.status := tsDone;
                InterlockedIncrement(doneTasksAvailable);
            end;
        end;

        Sleep(15);

        needTerminate := InterlockedExchange(terminateWorkers[idx], 0);

    until needTerminate > 0;

    Worker := 0;
end;

constructor TTaskManager.Create;
var
    i: integer;
begin
    tasks := TThreadList.Create;
    FillChar(terminateWorkers, sizeof(terminateWorkers), 0);
    newTasksAvailable := 0;
    doneTasksAvailable := 0;
    threadPoolSize := 4;
    for i := 0 to threadPoolSize - 1 do
        workerThreadIds[i] := BeginThread(@Worker, pointer(i));
end;

destructor TTaskManager.Destroy;
var
    i: integer;
    retCode: dword;
begin
    tasks.Free;
    for i := 0 to threadPoolSize - 1 do
    begin
        InterlockedIncrement(terminateWorkers[i]);
        retCode := WaitForThreadTerminate(workerThreadIds[i], 50);
        if retCode <> 0 then
        begin
            WriteLn('Timeout expired while waiting worker thread to terminate! Killing.');
            KillThread(workerThreadIds[i]);
        end;
    end;
    inherited;
end;

procedure TTaskManager.SocketDisconnected(socket: TLSocket);
begin
    if socket.UserData <> nil then
    begin
        with tasks.LockList do
        begin
            TTask(socket.UserData).status := tsCancelled;
            Remove(socket.UserData);
        end;
        tasks.UnlockList;
    end;
end;

procedure TTaskManager.Enqueue(task: TTask);
var
    i: integer;
    tasksInQueue: integer;
    newPoolSize: integer;
begin
    tasks.Add(task);
    tasksInQueue := InterlockedIncrement(newTasksAvailable);

    if tasksInQueue > threadPoolSize then
    begin
        newPoolSize := threadPoolSize * 2;
        if newPoolSize > MAX_THREAD_POOL_SIZE then
            newPoolSize := MAX_THREAD_POOL_SIZE;

        for i := threadPoolSize to newPoolSize - 1 do
            workerThreadIds[i] := BeginThread(@Worker, pointer(i));

        threadPoolSize := newPoolSize;
        WriteLn('Grew pool to ', newPoolSize);
    end;
end;

procedure TTaskManager.FinalizeTasks;
var
    i: integer;
    tasksAvailable: integer;
    task: TTask;
    tasksToFinalize: array [0..99] of TTask;
    tasksToFinalizeCount: integer;
begin

    tasksAvailable := InterlockedExchange(doneTasksAvailable, 0);

    if tasksAvailable > 0 then
    begin
        tasksToFinalizeCount := 0;

        with tasks.LockList do
        begin
            for i := 0 to Count - 1 do
            begin
                task := TTask(Items[i]);
                if (task.status = tsDone) and (tasksToFinalizeCount < 100) then
                begin
                    tasksToFinalize[tasksToFinalizeCount] := task;
                    inc(tasksToFinalizeCount);
                end;
            end;

            for i := 0 to tasksToFinalizeCount - 1 do
                Remove(tasksToFinalize[i]);

        end;
        tasks.UnlockList;

        for i := 0 to tasksToFinalizeCount - 1 do
            tasksToFinalize[i].Finalize;

    end
    else if newTasksAvailable = 0 then
    begin
        // time to shrink
        if threadPoolSize > 16 then
        begin

            WriteLn('Idle. Shrinking the pool...');

            for i := threadPoolSize - 1 downto 16 do
                InterlockedIncrement(terminateWorkers[i]);

            threadPoolSize := 16;
            WriteLn('Shrinked pool to 16');

        end;
    end;

end;

end.