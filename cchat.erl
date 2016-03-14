% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

%% Sends jobs to the server
%
% Delegate work to channels
% Uses the assign_tasks function to spread the work over several clients.
% The computation of the function parameter will be done in each client.
% If there is no clients connected to the server the delegation will fail and report
% that no clients are available.
% If some clients for some reason fail to respond, the delegation will have failed as well
% and report that all clients did not answer. 
%
send_job(Server, Func, List) ->
    Data = get_workers,
    try genserver:request(list_to_atom(Server), Data, infinity) of
        {ok, Workers} -> 
            Tasks = assign_tasks(Workers, List),
            Reply = [genserver:request(Pid, {work, Func, Item}, 5000) || {Pid, Item} <- Tasks],
            if
                length(Reply) == length(List) ->
                    [Val||{ok, Val} <- Reply];
                true ->
                    io:fwrite("All clients did not answer.")
            end;
        {error, _, Msg} ->
            io:fwrite("Error: ~p~n", [Msg])
    catch
        _:_ ->
            io:fwrite("Server not reached! ~p~n", [Server])
    end.

%
% Assigns tasks to the clients connected to the server
%
assign_tasks([], _) -> [] ;
assign_tasks(Workers, Tasks) ->
  [  {lists:nth(((N-1) rem length(Workers)) + 1, Workers), Task} || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].
