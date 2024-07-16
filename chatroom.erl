%% Name: Kailey Totland and Yousaf Rajput
%% Pledge: I pledge my honor that I have abided by the Stevens Honor System.

-module(chatroom).

-include_lib("./defs.hrl").

-export([start_chatroom/1]).

-spec start_chatroom(_ChatName) -> _.
-spec loop(_State) -> _.
-spec do_register(_State, _Ref, _ClientPID, _ClientNick) -> _NewState.
-spec do_unregister(_State, _ClientPID) -> _NewState.
-spec do_update_nick(_State, _ClientPID, _NewNick) -> _NewState.
-spec do_propegate_message(_State, _Ref, _ClientPID, _Message) -> _NewState.

start_chatroom(ChatName) ->
    loop(#chat_st{name = ChatName,
		  registrations = maps:new(), history = []}),
    ok.

loop(State) ->
    NewState =
	receive
	    %% Server tells this chatroom to register a client
	    {_ServerPID, Ref, register, ClientPID, ClientNick} ->
		do_register(State, Ref, ClientPID, ClientNick);
	    %% Server tells this chatroom to unregister a client
	    {_ServerPID, _Ref, unregister, ClientPID} ->
		do_unregister(State, ClientPID);
	    %% Server tells this chatroom to update the nickname for a certain client
	    {_ServerPID, _Ref, update_nick, ClientPID, NewNick} ->
		do_update_nick(State, ClientPID, NewNick);
	    %% Client sends a new message to the chatroom, and the chatroom must
	    %% propegate to other registered clients
	    {ClientPID, Ref, message, Message} ->
		do_propegate_message(State, Ref, ClientPID, Message);
	    {TEST_PID, get_state} ->
		TEST_PID!{get_state, State},
		loop(State)
end,
    loop(NewState).

%% This function should register a new client to this chatroom
do_register(State, Ref, ClientPID, ClientNick) ->
    %% io:format("chatroom:do_register(...): IMPLEMENT ME~n"),

	%% STEP 8
	%% chatrooms receives msg from server
	%% update local record of registered client
	%% AND tell client about itself by sending message to client
	%% msg = {self(), Ref, connect, State#chat st.history} [D]
	%% state is the chatroom chat_st
	UpdatedRegistrations = maps:put(ClientPID, ClientNick, State#chat_st.registrations),
	%% is this order ok?
	ClientPID!{self(), Ref, connect, State#chat_st.history},
	State#chat_st{registrations=UpdatedRegistrations}.

%% This function should unregister a client from this chatroom
do_unregister(State, ClientPID) ->
    %%io:format("chatroom:do_unregister(...): IMPLEMENT ME~n"),
    
	%% STEP 6
	%% server sends msg to chatroom
	%% msg = {self(), Ref, unregister, ClientPID} [C]
	%% AND chatroom removes client from record of registetered clients
	UpdatedRegistrations = maps:remove(ClientPID, State#chat_st.registrations),
	State#chat_st{registrations = UpdatedRegistrations}.

	%%State.

%% This function should update the nickname of specified client.
do_update_nick(State, ClientPID, NewNick) ->
    %% io:format("chatroom:do_update_nick(...): IMPLEMENT ME~n"),
	UpdatedRegistrations = maps:update(ClientPID, NewNick, State#chat_st.registrations),
	State#chat_st{registrations = UpdatedRegistrations}.

	%%State.

propegate_helper([], _State, _Ref, _ClientPID, _Message) ->
	pass;

propegate_helper([H|T], State, Ref, ClientPID, Message) ->
	case H == ClientPID of
		%% STEP 4
		%% chatroom sends back to sending client ack message {self(), Ref, ack msg} [D] 
		% chatroom behavior continued to receiving clients
		true -> ClientPID!{self(), Ref, ack_msg},
			propegate_helper(T, State, Ref, ClientPID, Message);
			%% STEP 1
			%% chatroom sends msg to all receiving clients registered to chatroom
			%% EXCEPT for the sending client
			%% msg = {request, self(), Ref, {incoming msg, CliNick, State#chat st.name, Message}} [D] 
		false ->
			CliNick = maps:get(ClientPID, State#chat_st.registrations),
			H!{request, self(), Ref, {incoming_msg, CliNick, State#chat_st.name, Message}},
			propegate_helper(T, State, Ref, ClientPID, Message)
	end. 


%% This function should update all clients in chatroom with new message
%% (read assignment specs for details)
do_propegate_message(State, Ref, ClientPID, Message) ->
    %io:format("chatroom:do_propegate_message(...): IMPLEMENT ME~n"),
    ListPIDs = maps:keys(State#chat_st.registrations),
	propegate_helper(ListPIDs, State, Ref, ClientPID, Message),
	%%%%%%%%% RECEIVING CLIENT - chatroom behavior %%%%%%%%%%	
	%% STEP 2
	%% chatroom appends new Message to its own chat history
	%% by appending {CliNick,Message} to the history field of record holding its state

	%% chat history is a tuple of {clinick, message}
	CliNick = maps:get(ClientPID, State#chat_st.registrations),
	AddMessage = {CliNick, Message},
	UpdatedHistory = State#chat_st.history ++ [AddMessage],
	State#chat_st{history=UpdatedHistory}.

	%State.
