-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{ name = ServerName, users = [], channels = maps:new().

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Nick}) ->
	Is_member = lists:member(Nick, St#server_st.users),
	%io:fwrite("Server received: ~p~n", Is_member),
	if
		Is_member ->
			Response = {error, user_already_connected, "You're already connected"},
			{reply, Response, St} ;
		true ->
            NewSt = St#server_st { users = [Nick|St#server_st.users] },
            {reply, ok, NewSt}
        % Atom server_not_reached is returned when 
        % the server process cannot be reached for any reason.
	end;

%
% Disconnect from server
%
%
handle(St, {disconnect, Nick}) ->
	NewState = St#server_st { users = lists:delete(Nick, St#server_st.users) },
	{reply, ok, NewState};

%
% Join channel
%
%
handle(St, {join, Channel, Ref}) ->
	ChannelExists = lists:member(Channel, St#server_st.channels),
	if
		ChannelExists ->
			{reply, ok, St};
		true ->
			NewState = St#server_st { channels = [Channel|St#server_st.channels] },
			{reply, ok, NewState}	
	end;


handle(St, {msg_from_client, Channel, Nick, Msg}) ->
	genserver:request(St, {incoming_msg, Channel, Nick, Msg});
	%NewState = St#server_st { users = lists:delete(Nick, St#server_st.users) },
	%{reply, ok, NewState};

handle(St, Request) ->
   io:fwrite("Server received: ~p~n", [Request]),
   Response = "hi!",
   io:fwrite("Server is sending: ~p~n", [Response]),
   {reply, Response, St}.
    %io:fwrite("Server received: ~p~n", [Request]),
    %Response = "hi!",
    %io:fwrite("Server is sending: ~p~n", [Response]),
    
