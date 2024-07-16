%% Name: Kailey Totland and Yousaf Rajput
%% Pledge: I pledge my honor that I have abided by the Stevens Honor System.

-module(server).
-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    %%io:format("server:do_join(...): IMPLEMENT ME~n"),

	%% STEP 4
	%% server receives and checks if chatroom exists yet
	%% use chatrooms elem of serv_st.
	%% if does not exist - spawns it
	ChatroomExists = maps:is_key(ChatName, State#serv_st.chatrooms),
	case ChatroomExists of
		false -> % spawn chatroom and continue
			ChatRoom = spawn(chatroom, start_chatroom, [ChatName]),
			OtherPIDs = [];
		true -> %% continue get the chatpid
			ChatRoom = maps:get(ChatName, State#serv_st.chatrooms),
			OtherPIDs = maps:get(ChatName, State#serv_st.registrations)
		end,
		%% STEP 5
		%% server looks up client nickname from serv_st record
		NicknameFound = maps:get(ClientPID, State#serv_st.nicks),
		
		%% STEP 6
		%% if PID is found OR new chatroom spawned,
		%% server then tells chatroom that client is joining the chatroom
		%% by sending {self(), Ref, register, ClientPID, ClientNick} [C] to chatroom
		ChatRoom!{self(), Ref, register, ClientPID, NicknameFound},

		%% STEP 7
		%% server updates record of chatroom registrations to include client in list of clients registered
		AddPIDs = [ClientPID | OtherPIDs],
		UpdatedChatrooms = maps:put(ChatName, ChatRoom, State#serv_st.chatrooms),
		UpdatedRegistrations = maps:put(ChatName, AddPIDs, State#serv_st.registrations),
		State#serv_st{chatrooms=UpdatedChatrooms, registrations=UpdatedRegistrations}.


%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    %% io:format("server:do_leave(...): IMPLEMENT ME~n"),
	%% STEP 4
	%% server lookup chatroom PID from servers state serv_st
	ChatRoomPID = maps:get(ChatName, State#serv_st.chatrooms),
	AllPIDs = maps:get(ChatName, State#serv_st.registrations),
	RemovePID = lists:delete(ClientPID, AllPIDs),
	%% STEP 6
	%% server sends msg to chatroom
	%% msg = {self(), Ref, unregister, ClientPID} [C]
	%% AND chatroom removes client from record of registetered clients
	UpdatedRegistrations = maps:update(ChatName, RemovePID, State#serv_st.registrations),
	ChatRoomPID!{self(), Ref, unregister, ClientPID},
	ClientPID ! {self, Ref, ack_leave},
	State#serv_st{registrations = UpdatedRegistrations}.

updateAllChatrooms([], _State, _Ref, _ClientPID, _NewNick) ->
	[];

%% STEP 6
		%% server updates all chatrooms that the client belongs to
		%% with the msg {self(), Ref, update nick, ClientPID, NewNick} [C] to the chatrooms
updateAllChatrooms([H|T], State, Ref, ClientPID, NewNick) ->
	FindClient = maps:get(H, State#serv_st.registrations),
	case lists:member(ClientPID, FindClient) of
		true -> 
			FindChatroom = maps:get(H, State#serv_st.chatrooms),
			FindChatroom!{self(), Ref, update_nick, ClientPID, NewNick},
			[H] ++ [updateAllChatrooms(T, State, Ref, ClientPID, NewNick)];
		false ->
			updateAllChatrooms(T, State, Ref, ClientPID, NewNick)
	end.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    %%io:format("server:do_new_nick(...): IMPLEMENT ME~n"),
	%% STEP 4
	%% server checks if Nick is used by another client
	%% if yes, send err msg to client
	%% err msg = {self(), Ref, err nick used} [B]
	NicknamesList = maps:values(State#serv_st.nicks),
	case lists:member(NewNick, NicknamesList) of
		true -> ClientPID!{self(), Ref, err_nick_used};
		false -> 
			%% STEP 5
			%% server updates record of nickname - points client PID to new nickname
			UpdatedNicks = maps:update(ClientPID, NewNick, State#serv_st.nicks),
			%% step 6 - method above
			ListChatrooms = maps:keys(State#serv_st.registrations),
			updateAllChatrooms(ListChatrooms, State, Ref, ClientPID, NewNick),
			%% STEP 7
			%% server sends msg {self(), Ref, ok nick} [B] to the client
			ClientPID!{self(), Ref, ok_nick},
			State#serv_st{nicks=UpdatedNicks}
	end.


%%State.

% quit_helper([], State, _Ref, _ClientPID) ->
% 	State;

%% server:
	%% removes client from nicknames
	%% tells each chatroom client is registered that client is leaving
	%% by sending {self(), Ref, unregister, ClientPID} [C]  to each chatroom
	%% this is same msg as when client asks to leave
	%% remove client from server copy of all chat registrations

quit_helper([], _State, _Ref, _ClientPID) ->
	pass;
quit_helper([H|T], State, Ref, ClientPID) ->
	Clientlist = maps:get(H, State#serv_st.registrations),
	case lists:member(ClientPID, Clientlist) of
		true ->
			Chatroom = maps:get(H, State#serv_st.chatrooms),
			Chatroom!{self(), Ref, unregister, ClientPID},
			quit_helper(T, State, Ref, ClientPID);
		false ->
			quit_helper(T, State, Ref, ClientPID)
	end.


%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    %% io:format("server:do_client_quit(...): IMPLEMENT ME~n"),
	%% STEP 3
	%% server removes client from nicknames and tells each chatroom client that the client is leaving
	UpdatedNicks = maps:remove(ClientPID, State#serv_st.nicks),
	ChatroomNames = maps:keys(State#serv_st.registrations),
	quit_helper(ChatroomNames,State, Ref, ClientPID ),

	UpdatedRegistrations = maps:map( fun(_H, Clientlist) -> 
		case lists:member(ClientPID, Clientlist) of
			true -> lists:delete(ClientPID, Clientlist);
			false -> Clientlist
		end
	end,
	State#serv_st.registrations
	),
		%% STEP 4
	%% server sends msg to client {self(), Ref, ack quit} [B]
	ClientPID!{self(), Ref, ack_quit},
	State#serv_st{nicks = UpdatedNicks, registrations = UpdatedRegistrations}.


	
