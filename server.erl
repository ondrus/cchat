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
	if
		Is_Member ->
			Response = {error, user_already_connected, "Nick occupied by other user"},
			{reply, Response, St};
		true ->
            NewSt = St#server_st {users = maps:put(Nick, Pid, St#server_st.users)},
            {reply, {ok, self()}, NewSt}
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
% NEED TO CHECK THAT NO ONE TRIES TO JOIN THE SAME CHANNEL WHILE SOMEONE CREATES IT?
handle(St, {join, Channel, Nick}) ->
	Channels = St#server_st.channels,
	{ok, UserPid} = maps:find(Nick, St#server_st.users),
	case maps:find(Channel, Channels) of
		{ok, Pid} ->
			Response = genserver:request(Pid, {join, UserPid}),
			{reply, Response, St};
		error ->
			ChannelPid = genserver:start(Channel, channel:initial_state(Channel), fun channel:handle/2),
			Response = genserver:request(ChannelPid, {join, UserPid}),
			NewState = St#server_st {channels = maps:put(Channel, ChannelPid, Channels)}, % Perhaps could be done at the same time as above.
			{reply, Response, NewState}
	end;
%
% Leave channel
%
% NEED TO CHECK THAT NO ONE TRIES TO LEAVE BEFORE JOINED
handle(St, {leave, Channel, Pid}) ->
	Channels = St#server_st.channels,
	%io:fwrite("Server sent channelPid to Channel: ~p~n", [ChannelPidd]),
	case maps:find(Channel, Channels) of
		{ok, ChannelPid} ->
			Response = genserver:request(ChannelPid, {leave, Pid}),
			{reply, Response, St};
		error ->
			{reply, {error, user_not_joined, "Channel does not exist"}, St}
	end;

%
% Send message
%
%
handle(St, {msg_from_client, Channel, Nick, Msg}) ->
	case maps:find(Channel, St#server_st.channels) of
		{ok, ChannelPid} ->
			{ok, UserPid} = maps:find(Nick, St#server_st.users),
			Response = genserver:request(ChannelPid, {msg_from_client, UserPid, Nick, Msg}),
			{reply, Response, St};
		error ->
			{reply, {error, user_not_joined, "Channel does not exist"}, St}
			
	end;

handle(St, Request) ->
	{reply, {error, user_not_joined, [Request]}, St}.





