%%%%%%%%%%%%%%%% 
%% Made by Josefin Ondrus and Emma Gustafsson group 17 in TDA383, 2016
%%
%% Server module handling requests from the client module. 
%% Includes functionality of clients connecting and disconnecting 
%% to/from the server as well as redirect channel functionality 
%% to the channel module.
%%%%%%%%%%%%%%%%

-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%
% Produce initial state 
%
initial_state(ServerName) ->
    #server_st{name = ServerName, users = maps:new(), channels = maps:new()}.

%%
%% handle/2 handles requests from clients
%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.
%%

%
% Connect to server
% If the user is not connected, adds the user and returns the process Pid.
% If the user already is connected returns error user_already_connected.
%
handle(St, {connect, Nick, Pid}) ->
	Member = maps:is_key(Nick, St#server_st.users),
	if
		Member ->
			{reply, getError(nick_taken), St};
		true ->
            NewSt = St#server_st {users = maps:put(Nick, Pid, St#server_st.users)},
            {reply, {ok, self()}, NewSt}
	end;

%
% Disconnect from server
% Removes the user from the server.
%
handle(St, {disconnect, Nick}) ->
	NewState = St#server_st {users = maps:remove(Nick, St#server_st.users)},
	{reply, ok, NewState};

%
% Join channel
% If the channel exists, sends a request to the channel process to add the user t the channel.
% If the channel does not exist, create a new channel process and add the user to it.
%
handle(St, {join, Channel, Pid}) ->
	case maps:find(Channel, St#server_st.channels) of
		{ok, ChannelPid} ->
			Reply = genserver:request(ChannelPid, {join, Pid}),
			{reply, Reply, St};
		error ->
			ChannelPid = genserver:start(Channel, channel:initial_state(Channel), fun channel:handle/2),
			Reply = genserver:request(ChannelPid, {join, Pid}),
			NewState = St#server_st {channels = maps:put(Channel, ChannelPid, St#server_st.channels)},
			{reply, Reply, NewState}
	end;
%
% Leave channel
% If the channel exists, sends a request to the channel process to remove the user from it.
% If the channel does not exist, returns error user_not_joined.
%
handle(St, {leave, Channel, Pid}) ->
	case maps:find(Channel, St#server_st.channels) of
		{ok, ChannelPid} ->
			Reply = genserver:request(ChannelPid, {leave, Pid}),
			{reply, Reply, St};
		error ->
			{reply, getError(user_not_joined), St}
	end;


%
% Delegate work to channels
% Uses the assign_tasks function to spread the work over several clients.
% The computation of the function parameter will be done in each client.
% If there is no clients connected to the server the delegation will fail and report
% that no clients are available.
% If some clients for some reason fail to respond, the delegation will have failed as well
% and report that all clients did not answer. 
%
handle(St, get_workers) ->
	Users = maps:values(St#server_st.users),
	if
		length(Users) == 0 ->
			{reply, getError(no_clients), St};
		true ->
			{reply, {ok, Users}, St}
	end;

%
% Handles unknown requests
%
handle(St, _) ->
	{reply, getError(unknown_request), St}.

%
% Assigns tasks to the clients connected to the server
%
% assign_tasks([], _) -> [] ;
% assign_tasks(Users, Tasks) ->
%   [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task} || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].


%
% Returns the error tuple of each error atom
%
getError(Error) ->
	case Error of
		delegation_failed ->
			{error, delegation_failed, "All clients did not answer."};
		no_clients ->
			{error, no_clients, "No clients available."};
		nick_taken ->
			{error, nick_taken, "Nick occupied by other user"};
		user_not_joined ->
			{error, user_not_joined, "Channel does not exist"};
		unknown_request ->
			{error, unknown_request, "Something went really wrong"}
	end.
