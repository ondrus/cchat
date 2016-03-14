%%%%%%%%%%%%%%%% 
%% Made by Josefin Ondrus and Emma Gustafsson group 17 in TDA383, 2016
%%
%% Channel module handling requests from the server module. 
%% Includes functionality of clients joining and leaving channels as well as 
%% sending messages to all clients connected to its own channel process.
%%%%%%%%%%%%%%%%

-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%
% Produce initial state 
%
initial_state(ChannelName) ->
    #channel_st{name = ChannelName, users = []}.

%%
%% handle/2 handles requests from server
%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the server
%% and NewState is the new state of the channel.
%%

%
% Join channel
% If the user is not in the channel, adds the user
% If the user is in the channels returns error user_already_joined.
%
handle(St, {join, Pid}) ->
    Member = isMember(St, Pid),
    if 
        Member ->
            {reply, getError(user_already_joined), St};
        true ->
            NewState = St#channel_st {users = [Pid|St#channel_st.users]},
            {reply, {ok,self()}, NewState}
    end;

%
% Leave channel
% If the user is in the channel, removes the user
% If the user is not in the channels returns error user_not_joined.
%
handle(St, {leave, Pid}) ->
    Member = isMember(St, Pid),
    if
        Member ->
            NewState = St#channel_st {users = lists:delete(Pid,St#channel_st.users)},
            {reply, ok, NewState};
        true ->
            {reply, getError(user_not_joined), St}      
    end;

%
% Send message
% Sends the message to all users connected to the current channel process
% (except the sending user).
% If the sending user is not in the channel, returns error user_not_joined.
%
handle(St, {msg_from_client, Pid, Nick, Msg}) ->
    Member = isMember(St, Pid),
    if
        Member ->
            Pids = lists:delete(Pid, St#channel_st.users),
            Data = {incoming_msg, atom_to_list(St#channel_st.name), atom_to_list(Nick), Msg},
            [spawn(fun() -> genserver:request(P, Data) end) || P <- Pids],
            {reply, ok, St};
        true ->
            {reply, getError(user_not_joined), St}
    end;

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
        user_already_joined -> 
            {error, user_already_joined, "You're already in this channel"};
        user_not_joined -> 
            {error, user_not_joined, "You're not in this channel"};
        unknown_request ->
            {error, unknown_request, "Something went really wrong"}
    end.

%
% Checks if the user is a member of this channel.
% Returns true if the user is a member of this channel process.
%
isMember(St, User) -> 
    lists:member(User, St#channel_st.users).


