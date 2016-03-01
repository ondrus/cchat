-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { nick = list_to_atom(Nick), gui = GUIName, server = null, channels = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.
%% Connect to server
handle(St, {connect, Server}) ->
    if 
        St#client_st.server == null ->
            Data = {connect, St#client_st.nick, self()},
            try genserver:request(list_to_atom(Server), Data) of
                {ok, Connection} ->
                    NewSt = St#client_st {server = Connection},
                    {reply, ok, NewSt};
                Response ->
                    {reply, Response, St}
            catch
                _:_ ->
                    {reply, {error, server_not_reached, "couldn't reach to server"}, St}
            end;
        true -> % probably not needed on client side?
             {reply, {error, user_already_connected, "You're already connected to a server"}, St}
    end;

%% Disconnect from server
handle(St, disconnect) ->
    if 
        St#client_st.server == null ->
            {reply, {error, user_not_connected, "You're not connected to a server"}, St} ;
        St#client_st.channels /= [] ->
            {reply, {error, leave_channels_first, "Leave all the channels!"}, St} ;
        true ->
            Data = {disconnect, St#client_st.nick},
            try genserver:request(St#client_st.server, Data) of
                ok ->
                    NewState = St#client_st {server = null},
                    {reply, ok, NewState}
            catch
                _:_ ->
                    {reply, {error, server_not_reached, "couldn't reach server"}, St}
            end
    end;

% Join channel
handle(St, {join, Channel}) ->
    if
        St#client_st.server == null ->
            {reply, {error, user_not_connected, "You're not connected to a server"}, St} ;
        true ->
            ChannelAtom = list_to_atom(Channel),
            Data = {join, ChannelAtom, St#client_st.nick},
            try genserver:request(St#client_st.server, Data) of
                ok ->
                    NewState = St#client_st {channels = [ChannelAtom|St#client_st.channels]},
                    {reply, ok, NewState};
                Response ->
                    {reply, Response, St}
            catch
                _:_ ->
                  {reply, {error, server_not_reached, "couldn't reach server"}, St}
            end
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    ChannelAtom = list_to_atom(Channel),
    Data = {leave, ChannelAtom, self()},
    try genserver:request(St#client_st.server, Data) of
        ok ->
            NewState = St#client_st {channels = lists:delete(ChannelAtom, St#client_st.channels)},
            {reply, ok, NewState};
        Response ->
            {reply, Response, St}
    catch
        _:_ ->
            {reply, {error, server_not_reached, "couldn't reach server"}, St}
    end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    Data = {msg_from_client, list_to_atom(Channel), St#client_st.nick, Msg},
    try genserver:request(St#client_st.server, Data) of
        Response ->
        {reply, Response, St}
    catch
        _:_ ->
            {reply, {error, server_not_reached, "couldn't reach server"}, St}
    end;

%% Get current nick
handle(St, whoami) ->
    Nick = atom_to_list(St#client_st.nick),
    {reply, Nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    if 
        St#client_st.server /= null -> 
            {reply, {error, user_already_connected, "Can't change nick while connected"}, St} ;
        true ->
            NickAtom = list_to_atom(Nick),
            NewSt = St#client_st { nick = NickAtom },
            {reply, ok, NewSt}
    end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
