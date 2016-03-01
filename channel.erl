-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ChannelName) ->
    #channel_st{ name = ChannelName, users = []}.

%% handle/2 handles requests from server

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the server
%% and NewState is the new state of the server.

%
% Join channel
%
% NEED TO CHECK THAT NO ONE TRIES TO JOIN THE SAME CHANNEL WHILE SOMEONE CREATES IT?
handle(St, {join, Pid}) ->
	Users = St#channel_st.users,
	IsMember = lists:member(Pid, Users),
	if 
		IsMember ->
			{reply, {error, user_already_joined, "You're already in this channel"}, St};
		true ->
			NewState = St#channel_st {users = [Pid|St#channel_st.users]}, % Perhaps could be done at the same time as below
			{reply, ok, NewState}
	end;
%
% Leave channel
%
% NEED TO CHECK THAT NO ONE TRIES TO LEAVE BEFORE JOINED
handle(St, {leave, Pid}) ->
	Users = St#channel_st.users,
	IsMember = lists:member(Pid, Users),
	if
		IsMember ->
			NewState = St#channel_st {users = lists:delete(Pid,Users)}, % Perhaps could be done at the same time as below
			{reply, ok, NewState};
		true ->
			{reply, {error, user_not_joined, "You're not in this channel bajs"}, St}		
	end;

%
% Send message
%
%
handle(St, {msg_from_client, Pid, Nick, Msg}) ->
	IsMember = lists:member(Pid, St#channel_st.users),
	if
		IsMember ->
			{reply, ok, St},
			Pids = lists:delete(Pid, St#channel_st.users),
			lists:foreach(fun(P) ->
							genserver:request(P, {incoming_msg, atom_to_list(St#channel_st.name), atom_to_list(Nick), Msg})
				  		  end, Pids);
		true ->
			{reply, {error, user_not_joined, "You can't write to this channel"}, St}
	end;
		

handle(St, Request) ->
	{reply, {error, user_not_joined, [Request]}, St}.
