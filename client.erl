
%%%%%%%%%%%%%%%% 
%% Made by Josefin Ondrus and Emma Gustafsson group 17 in TDA383, 2016
%%
%% Client module handling requests from the GUI. 
%% Includes request handling of the accepted commands in CCHAT.
%% 
%%%%%%%%%%%%%%%%

-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%
% Produce initial state 
%
initial_state(Nick, GUIName) ->
    #client_st { nick = list_to_atom(Nick), gui = GUIName, server = null, channels = maps:new()}.

%%
%% handle/2 handles each kind of request from GUI
%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.
%%

%
% Connect to server
% If the user is not connected, tries to connect to the server.
% If the user already is connected returns error user_already_connected.
%
handle(St, {connect, Server}) ->
    Data = {connect, St#client_st.nick, self()},
    try genserver:request(list_to_atom(Server), Data) of
        {ok, ServerPid} ->
            NewState = St#client_st {server = ServerPid},
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, St}
    catch
        _:_ ->
            {reply, getError(server_not_reached), St}
    end;

%
% Disconnect form server
% If the user is connected and not in any channels, disconnects the user.
% If the user is connected but in at least one channel, returns error leave_channels_first.
% If the user not connected, returns error user_not_connected.
%
handle(St, disconnect) ->
    MapSize= maps:size(St#client_st.channels),
    if 
        St#client_st.server == null ->
            {reply, getError(user_not_connected), St};
         MapSize /= 0 ->
            {reply, getError(leave_channels_first), St};
        true ->
            Data = {disconnect, St#client_st.nick},
            try genserver:request(St#client_st.server, Data) of
                ok ->
                    NewState = St#client_st {server = null},
                    {reply, ok, NewState}
            catch
                _:_ ->
                    {reply, getError(server_not_reached), St}
            end
    end;

%
% Join channel
% Sends a request to the server to connect the user to the specified channel
% If the server is not reached, (e.g. user not connected to a server),
% returns error server_not_reached.
%
handle(St, {join, Channel}) ->
    ChannelAtom = list_to_atom(Channel),
    Data = {join, ChannelAtom, self()},
    try genserver:request(St#client_st.server, Data) of
        {ok,ChannelPid} ->
            NewState = St#client_st {channels = maps:put(ChannelAtom, ChannelPid, St#client_st.channels)},
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, St}
    catch
        _:_ ->
          {reply, getError(server_not_reached), St}
    end;

%
% Leave channel
% Sends a request to the server remove the user from the specified channel
% If the server is not reached, (e.g. user not connected to a server),
% returns error server_not_reached.
%
handle(St, {leave, Channel}) ->
   ChannelAtom = list_to_atom(Channel),
   Data = {leave, ChannelAtom, self()},
   try genserver:request(St#client_st.server, Data) of
       ok ->
           NewState = St#client_st {channels = maps:remove(ChannelAtom,St#client_st.channels)},
           {reply, ok, NewState};
       Reply ->
           {reply, Reply, St}
   catch
        _:_ ->
           {reply, getError(server_not_reached), St}
   end;

%
% Send messages
% Sends a request directly to the channel to send a message to that specified channel.
% If the user has not joined that channel error message user_not_joined is returned.
%
handle(St, {msg_from_GUI, Channel, Msg}) ->
    Data = {msg_from_client, self(), St#client_st.nick, Msg},
    case maps:find(list_to_atom(Channel),St#client_st.channels) of
        {ok,ChannelPid} ->
            Reply = genserver:request(ChannelPid, Data),
            {reply, Reply, St};
        error -> 
             {reply, getError(user_not_joined), St}
    end;

%
% Get current nick
% Returns the current nick of the specified client.
%
handle(St, whoami) ->
    Nick = atom_to_list(St#client_st.nick),
    {reply, Nick, St} ;

%
% Change nick
% If the user isn't connected, changes the nick of the user.
% If the user is connected to a server, returns the error user_already_connected.
%
handle(St, {nick, Nick}) ->
    if 
        St#client_st.server /= null -> 
            {reply, getError(user_already_connected), St} ;
        true ->
            NickAtom = list_to_atom(Nick),
            NewState = St#client_st { nick = NickAtom },
            {reply, ok, NewState}
    end;

%
% Incoming message
% Sends the incoming message to the GUI.
%
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St};

handle(St, {work, Func, Val}) ->
    {reply, Func(Val), St};

%
% Handles unknown requests
%
handle(St, _) ->
    {reply, getError(unknown_request), St}.
%
% Returns the error tuple of each error atom
%
getError(Error) ->
    case Error of
        server_not_reached ->
            {error, server_not_reached, "couldn't reach server"};
        user_not_joined ->
            {error, user_not_joined, "Channel does not exist"};
        unknown_request ->
            {error, unknown_request, "Something went really wrong"};
        user_not_connected ->
            {error, user_not_connected, "You're not connected to a server"};
        leave_channels_first ->
            {error, leave_channels_first, "Leave all the channels!"};
        user_already_connected ->
            {error, user_already_connected, "Can't change nick while connected"}
    end.