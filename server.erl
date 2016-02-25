-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{ name = ServerName, users = maps:new(), channels = maps:new()}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.
handle(St, {connect, Nick, Pid}) ->
	Is_Member = maps:is_key(Nick, St#server_st.users),
	%io:fwrite("Server received: ~p~n", Is_member),
	if
		Is_Member ->
			Response = {error, user_already_connected, "Nick occupied by other user"},
			{reply, Response, St};
		true ->
            NewSt = St#server_st {users = maps:put(Nick, Pid, St#server_st.users)},
            io:fwrite("Server is sending: ~p~n", [self()]),
            {reply, {ok, self()}, NewSt}
        % Atom server_not_reached is returned when 
        % the server process cannot be reached for any reason.
	end;

%
% Disconnect from server
%
%
handle(St, {disconnect, Nick}) ->
	NewState = St#server_st {users = maps:remove(Nick, St#server_st.users)},
	{reply, ok, NewState};

%
% Join channel
%
% NEED TO CHECK THAT NO ONE TRIES TO JOIN THE SAME CHANNEL WHILE SOMEONE CREATES IT
handle(St, {join, Channel, Nick}) ->
	Channels = St#server_st.channels,
	case maps:find(Channel, Channels) of
		{ok, Members} ->
			IsMember = lists:member(maps:get(Nick,St#server_st.users),Members),
			if 
				IsMember ->
					NewMembers = Members,
					{reply, {error, user_already_joined, "You're already in this channel"}, St};
				true ->
					NewMembers = [maps:get(Nick,St#server_st.users)|Members]
			end;
		error ->
			NewMembers = [maps:get(Nick,St#server_st.users)]
	end,
	NewState = St#server_st {channels = maps:put(Channel, NewMembers, Channels)},
	{reply, ok, NewState};
%
% Leave channel
%
% NEED TO CHECK THAT NO ONE TRIES TO LEAVE BEFORE JOINED
handle(St, {leave, Channel, Nick}) ->
	Channels = St#server_st.channels,
	case maps:find(Channel, Channels) of
		{ok, Members} ->
			Pid = maps:get(Nick,St#server_st.users),
			IsMember = lists:member(Pid,Members),
			if 
				IsMember ->
					NewMembers = lists:delete(Pid, Members),
					NewState = St#server_st {channels = maps:put(Channel, NewMembers, Channels)},
					{reply, ok, NewState};
				true ->
					{reply, {error, user_not_joined, "You're not in the channel"}, St}
			end;
		error ->
			{reply, {error, user_not_joined, "Channel does not exist"}, St}
	end;

handle(St, {msg_from_client, Channel, Nick, Msg}) ->
	% Fold of all Pids from channel as St and then send the messages to them
	Channels = St#server_st.channels,
	case maps:find(Channel, Channels) of
		{ok, Members} ->
			Pids = lists:delete(maps:get(Nick, St#server_st.users), Members),	
			Response = lists:map(fun(P) ->
							genserver:request(P, {incoming_msg, atom_to_list(Channel), atom_to_list(Nick), Msg})
						  end, Pids),
			{reply, ok, St};
		error ->
			{reply, {error, user_not_joined, " BASJ: You're not in the channel"}, St}
	end;

handle(St, Request) ->
	{reply, {error, user_not_joined, "Wierd request to server"}, St}.





