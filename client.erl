%% Name: Kailey Totland and Yousaf Rajput
%% Pledge: I pledge my honor that I have abided by the Stevens Honor System.

-module(client).

-export([main/1, initial_state/2]).

-include_lib("./defs.hrl").

-spec main(_InitialState) -> _.
-spec listen(_State) -> _.
-spec initial_state(_Nick, _GuiName) -> _InitialClientState.
-spec loop(_State, _Request, _Ref) -> _.
-spec do_join(_State, _Ref, _ChatName) -> _.
-spec do_leave(_State, _Ref, _ChatName) -> _.
-spec do_new_nick(_State, _Ref, _NewNick) -> _.
-spec do_new_incoming_msg(_State, _Ref, _SenderNick, _ChatName, _Message) -> _.

%% Receive messages from GUI and handle them accordingly
%% All handling can be done in loop(...)
main(InitialState) ->
    %% The client tells the server it is connecting with its initial nickname.
    %% This nickname is guaranteed unique system-wide as long as you do not assign a client
    %% the nickname in the form "user[number]" manually such that a new client happens
    %% to generate the same random number as you assigned to your client.
    whereis(server)!{self(), connect, InitialState#cl_st.nick},
    %% if running test suite, tell test suite that client is up
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{client_up, self()}
    end,
    %% Begins listening
    listen(InitialState).

%% This method handles all incoming messages from either the GUI or the
%% chatrooms that are not directly tied to an ongoing request cycle.
listen(State) ->
    receive
        {request, From, Ref, Request} ->
   %% the loop method will return a response as well as an updated
   %% state to pass along to the next cycle
            {Response, NextState} = loop(State, Request, Ref),
   case Response of
{dummy_target, Resp} ->
   io:format("Use this for whatever you would like~n"),
   From!{result, self(), Ref, {dummy_target, Resp}},
   listen(NextState);
%% if shutdown is received, terminate
shutdown ->
   ok_shutdown;

%% do nothing if GUI receives a quit instruction
ack_quit ->
   From ! {self(), Ref, ack_quit},
   listen(NextState);

%% if ok_msg_received, then we don't need to reply to sender.
ok_msg_received ->
   listen(NextState);

%% otherwise, reply to sender with response
_ ->
   From!{result, self(), Ref, Response},
   listen(NextState)
   end
    end.

%% This function just initializes the default state of a client.
%% This should only be used by the GUI. Do not change it, as the
%% GUI code we provide depends on it.
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, con_ch = maps:new() }.

%% ------------------------------------------
%% loop handles each kind of request from GUI
%% ------------------------------------------
loop(State, Request, Ref) ->
    case Request of
	%% GUI requests to join a chatroom with name ChatName
	{join, ChatName} ->
	    do_join(State, Ref, ChatName);

	%% GUI requests to leave a chatroom with name ChatName
	{leave, ChatName} ->
	    do_leave(State, Ref, ChatName);

	%% GUI requests to send an outgoing message Message to chatroom ChatName
	{outgoing_msg, ChatName, Message} ->
	    do_msg_send(State, Ref, ChatName, Message);

	%% GUI requests the nickname of client
	whoami ->
        {State#cl_st.nick, State};

	%% GUI requests to update nickname to Nick
	{nick, Nick} ->
            do_new_nick(State, Ref, Nick);

	%% GUI requesting to quit completely
	quit ->
	    do_quit(State, Ref);

	%% Chatroom with name ChatName has sent an incoming message Message
	%% from sender with nickname SenderNick
	{incoming_msg, SenderNick, ChatName, Message} ->
	    do_new_incoming_msg(State, Ref, SenderNick, ChatName, Message);

	{get_state} ->
	    {{get_state, State}, State};

	%% Somehow reached a state where we have an unhandled request.
	%% Without bugs, this should never be reached.
	_ ->
	    io:format("Client: Unhandled Request: ~w~n", [Request]),
	    {unhandled_request, State}
    end.

%% executes `/join` protocol from client perspective
do_join(State, Ref, ChatName) ->
    %%io:format("client:do_join(...): IMPLEMENT ME~n"),

	%% STEP 1
	%% gui sends message {request, self(), Ref, {join, ChatName}} to the client [A]

	%% STEP 2
	%% client checks cl_st record if in chatroom
	ClientChannels = maps:is_key(ChatName, State#cl_st.con_ch),
	case ClientChannels of
		%% if client is in chatroom, send msg back to GUI and skip steps
		%% msg = {result, self(), Ref, err} [A]
		%% if client not in chatroom, step 3
		true -> 
			{err, State};	
			%%%whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, err};
		%% STEP 3
		%% client asks server to join chatroom
		%% sends msg = {self(), Ref, join, ChatName} [B]
		false -> whereis(server)!{self(), Ref, join, ChatName},
			%% STEP 9
			%% client receives message from chatroom, updates record of connected chatrooms,
			%% and sends msg to GUI
			%% msg = {result, self(), Ref, History} [A]
			%% where History is the chatroom history received from the chatroom process
			receive
				{PID, Ref, connect, History} ->
					%% update chatrooms records
					UpdatedChatrooms = maps:put(ChatName, PID, State#cl_st.con_ch),
					
					%%whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, History},
					{History, State#cl_st{con_ch=UpdatedChatrooms}}
			end
		end.

		%% STEP 10
		%% gui handles it, its already done for you

    %%{{dummy_target, dummy_response}, State}.

%% executes `/leave` protocol from client perspective
do_leave(State, Ref, ChatName) ->
    %%io:format("client:do_leave(...): IMPLEMENT ME~n"),

	%% STEP 1
	%% gui sends msg {request, self(), Ref, {leave, ChatName}} [A] to client

	%% STEP 2
	%% client checks if in chatroom with chatname
	%% if chatroom not founded in client list of connected chatrooms
	%% clients sends err message to gui
	%% and skips all following steps
	%% error msg = {result, self(), Ref, err} [A]
	ChatroomFound = maps:is_key(ChatName, State#cl_st.con_ch),
	case ChatroomFound of
		false -> %%whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, err};
			{err, State};
		true -> 
			%% STEP 3
			%% chatroom is found so client sends msg to server
			%% msg = {self(), Ref, leave, ChatName} [B]
			whereis(server)!{self(), Ref, leave, ChatName},

			receive
				{_PID, Ref, ack_leave} ->
					%% STEP 8
					%% client removes chatroom from list of chatrooms
					UpdatedChatrooms = maps:remove(ChatName, State#cl_st.con_ch),
					%% STEP 9
					%% client sends msg ok back to gui 
					%% ok = {result, self(), Ref, ok} [A] 
					%%whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, ok},
					{result, self(), Ref, ok},
					{ok, State#cl_st{con_ch=UpdatedChatrooms}}
			end
	end.
	
	%% STEP 10
	%% gui will handle

    %%{{dummy_target, dummy_response}, State}.

%% executes `/nick` protocol from client perspective
do_new_nick(State, Ref, NewNick) ->
    %% io:format("client:do_new_nick(...): IMPLEMENT ME~n"),

	%% STEP 1
	%% gui sends msg to client {request, self (), Ref, {nick, Nick}} [A] 

	%% STEP 2
	%% client checks Nick against current nickname
	CurrentNickname = State#cl_st.nick,
	case CurrentNickname == NewNick of
		%% if Nick is the same, client sends error msg to GUI
		%% msg = {result, self(), Ref, err same} [A],
		%% and skips all following steps
		true -> %%whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, err_same};
			{err_same, State};
		false -> 
			%% STEP 3
			%% if Nick is not the same, client sends msg back to server
			%% msg = {self(), Ref, nick, Nick} [B]
			whereis(server)!{self(), Ref, nick, NewNick},
			receive
				%% STEP 4
				%% server checks if Nick is used by another client
				%% if yes, send err msg to client
				%% err msg = {self(), Ref, err nick used} [B]
				%% and client sends msg back to gui
				%% cli msg = {result, self(), Ref, err nick used}
				%% and skip all following steps
				{_PID, Ref, err_nick_used} ->
					%%whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, err_nick_used};
					{err_nick_used, State};
				{_PID, Ref, ok_nick} ->
					%% STEP 7
					%% server sends msg {self(), Ref, ok nick} [B] to the client
					%% STEP 8
					%% client sends message to GUI {result, self(), Ref, ok nick}
					%%whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, ok_nick},
					{ok_nick, State#cl_st{nick=NewNick}}
				end
	end.

	%% STEP 9
	%% gui takes care of it

    %{{dummy_target, dummy_response}, State}.

%% executes send message protocol from client perspective
do_msg_send(State, Ref, ChatName, Message) ->
    %% io:format("client:do_new_nick(...): IMPLEMENT ME~n"),

	%% STEP 1
	%% GUI sends msg to client. {request, self(), Ref, {outgoing msg, ChatName, Message}}

	%% STEP 2
	%% sending client looks up PID of chatroom in list of connected chats
	FoundChat = maps:find(ChatName, State#cl_st.con_ch),
	case FoundChat of
		%% STEP 3
		%% sending client sends msg {self(), Ref, message, Message} [D] to chatroom
		{ok, PID} -> PID!{self(), Ref, message, Message};
		error -> throw(error) end,

	%% STEP 4
	%% chatroom sends back to sending client ack message {self(), Ref, ack msg} [D] 
	% chatroom behavior continued to receiving clients
	receive
		{_PID, Ref, ack_msg} ->
			%% STEP 5
			%% sending client sends msg back to gui. {result, self(), Ref, {msg sent, St#cl st.nick}} [A] 
			%%whereis(list_to_atom(State#cl_st.gui))! {result, self(), Ref, {msg_sent, State#cl_st.nick}}
			{{msg_sent, State#cl_st.nick}, State}
	end.

	%% STEP 6
	%% gui takes care of rest

    %%{{dummy_target, dummy_response}, State}.

%% executes new incoming message protocol from client perspective
do_new_incoming_msg(State, _Ref, CliNick, ChatName, Msg) ->
	%% STEP 3
	%% receiving client receives msg and makes function call
	%% gen server:call(list to atom(State#cl st.gui), {msg to GUI, ChatName, CliNick, Msg}).
    gen_server:call(list_to_atom(State#cl_st.gui), {msg_to_GUI, ChatName, CliNick, Msg}),
    {ok_msg_received, State}.
	%% STEP 4
	%% gui receives msg {msg to GUI, ChatName, CliNick, Msg}
	%% and posts it to appropriate chatroom

%% executes quit protocol from client perspective
do_quit(State, Ref) ->
    %%io:format("client:do_new_nick(...): IMPLEMENT ME~n"),
	%% STEP 1
	%% gui sends message to client  {request, self(), Ref, quit} [A]
	%% STEP 2
	%% client sends message to server {self(), Ref, quit} [B]
	whereis(server)!{self(), Ref, quit},

	%% STEP 4
	%% server sends msg to client {self(), Ref, ack quit} [B]
	receive
		{_PID, Ref, ack_quit} ->
			%% STEP 5
			%% client sends message to GUI {self(), Ref, ack quit} [A]
			%%whereis(list_to_atom(State#cl_st.gui))!{self(), Ref, ack_quit}
			{ack_quit, State}
	end.
	%% STEP 6
	%% client cleanly exits and gui exits cleanly after receiving msg from client

    %%{{dummy_target, dummy_response}, State}.
