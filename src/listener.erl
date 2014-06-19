-module(listener).
-behavior(gen_server).

-export([start/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-include("client.hrl").

-record(state, {port, listensocket=null}).

-define(TCP_OPTIONS, [{packet, line}, {active, false}, {reuseaddr, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    API                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

stop() ->
    gen_server:cast(?MODULE, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Callbacks                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Port) ->
    process_flag(trap_exit, true),
    State = #state{port=Port},
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
	{ok, ListenSocket} ->
	    {ok, spawn_accept(State#state{listensocket=ListenSocket})};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State) ->
    {noreply, spawn_accept(State)};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Listener: Unexpected message: ~p~n", [Info]),
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

spawn_accept(State = #state{listensocket=ListenSocket}) ->
    proc_lib:spawn(fun() -> accept(ListenSocket) end),
    State.

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    gen_server:cast(?MODULE, {accepted, self()}),
    handle_request(Socket).

handle_request(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    case request_to_list(clean(Data)) of
		["CONNECT", Nick, Application] ->
		    case manager:connect(Socket, Nick, Application) of
			{ok, Client} ->
			    handle_client(Client);
			{error, _} ->
			    handle_request(Socket)
		    end;
		_ ->
		    handle_request(Socket)
	    end;
	{error, closed} ->
	    ok;
	{error, Reason} ->
	    io:format("Listener recv error: ~s~n", [Reason])
    end.

handle_client(Client) ->
    case gen_tcp:recv(Client#client.socket, 0) of
	{ok, Data} ->
	    case splitat($:, clean(Data)) of
		{"SAY", Message} ->
		    manager:broadcast(Client, Message);
		{"DISCONNECT", _} ->
		    manager:disconnect(Client),
		    exit(normal);
		_ -> %% Why is this needed?
		    ok
	    end,
	    handle_client(Client);
	{error, closed} ->
	    manager:disconnect(Client)
    end.

clean(Data) ->
    string:strip(Data, right, $\n).

request_to_list(Message) ->    
    case string:tokens(Message, ":") of
	[] ->
	    [""];
	List ->
	    List
    end.

splitat(Char, String) ->
    case lists:splitwith(fun(E) -> E =/= Char end, String) of
	{First, [Char|Rest]} ->
	    {First, Rest};
	{First, ""} ->
	    {First, ""}
    end.

