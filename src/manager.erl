-module(manager).
-behavior(gen_server).

-export([start/0, getclients/0, connect/3, disconnect/1, broadcast/1,
	 broadcast/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-include("client.hrl").

-record(state, {clients}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    API                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

getclients() ->
    gen_server:call(?MODULE, list).

connect(Socket, Nick, Application) ->
    gen_server:call(?MODULE, {connect, Socket, Nick, Application}).

disconnect(Client) ->
    gen_server:cast(?MODULE, {disconnect, Client}).

broadcast(Message) ->
    gen_server:cast(?MODULE, {broadcast, Message}).

broadcast(Sender, Message) ->
    gen_server:cast(?MODULE, {broadcast, Sender, Message}).

stop() ->
    gen_server:cast(?MODULE, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Callbacks                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #state{clients=[]}}.

handle_call({connect, Socket, Nick, App}, _, State = #state{clients=Clients}) ->
    Address = utils:socket_to_address(Socket),
    case verify(Nick, Address, Clients) of
	ok ->
	    NewClient = #client{socket=Socket, address=Address,	nick=Nick,
				application=App},
	    utils:log("[CONNECTED] " ++ utils:client_to_string(NewClient)),
	    Users = string:join([Client#client.nick || Client <- Clients], ":"),
	    send(Socket, "OK:" ++ Users),
	    send_to_all(Clients, "JOIN:" ++ Nick),
	    {reply, {ok, NewClient}, State#state{clients=[NewClient|Clients]}};
	{error, nick_in_use} ->
	    send(Socket, "ERROR:Nick in use"),
	    {reply, {error, nick_in_use}, State}
    end;
handle_call(list, _From, State = #state{clients=Clients}) ->
    {reply, Clients, State}.

handle_cast({disconnect, Client}, State = #state{clients=Clients}) ->
    utils:log("[DISCONNECTED] " ++ utils:client_to_string(Client)),
    Others = lists:delete(Client, Clients),
    send_to_all(Others, "LEAVE:" ++ Client#client.nick),
    {noreply, State#state{clients=Others}};
handle_cast({broadcast, Message}, State = #state{clients=Clients}) ->
    utils:log("BROADCAST: " ++ Message),
    send_to_all(Clients, "BROADCAST:" ++ Message),
    {noreply, State};
handle_cast({broadcast, Sender, Message}, State = #state{clients=Clients}) ->
    utils:log(Sender#client.nick ++ ": " ++ Message),
    Others = lists:delete(Sender, Clients),
    send_to_all(Others, "MSG:" ++ Sender#client.nick ++ ":" ++ Message),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Info, State) ->
    io:format("Client Manager: Unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(normal, _State) ->
    ok;
terminate(shutdown, _State) ->
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Internal functions                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: Check nick length, illegal characters, bans, timeouts etc
verify(Nick, _Address, Clients) ->
    case lists:keysearch(Nick, #client.nick, Clients) of
	false ->
	    ok;
	_ ->
	    {error, nick_in_use}
    end.

send(Socket, Message) ->
    gen_tcp:send(Socket, Message ++ "\n").

send_to_all(Clients, Message) ->
    [gen_tcp:send(Client#client.socket, Message ++ "\n") || Client <- Clients].

