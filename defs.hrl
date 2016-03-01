% This record defines the structure of the client process.
% It contains the following fields:
%	- nick: the nickname of the client (not the Pid).
%	- gui: the name (or Pid) of the GUI process.
%	- server: the Pid of the server that the client is connected to.
%	- channels: a list of the channels that the client is connected to.
-record(client_st, {nick, gui, server, channels}).

% This record defines the structure of the server process.
% It contains the following fields:
%	- name: the name of the server.
%	- users: the connected users nicknames mapping to their Pids.
%	- channels: the name of the channels on the servers mapping to their Pids.
-record(server_st, {name, users, channels}).

% This record defines the structure of a channel process.
% It contains the following fields:
%	- name: the name of the channel.
%	- users: the Pids of the users that are members of the channel.
-record(channel_st, {name, users}).
