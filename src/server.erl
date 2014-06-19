-module(server).

-export([start/1, clients/0, stop/0]).

-include("client.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    API                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Port) ->
    process_flag(trap_exit, true),

    io:format("Starting client manager... "),
    manager:start(),
    io:format("ok!~n"),

    io:format("Starting listener... "),
    listener:start(Port),
    io:format("ok!~n"),

    cli_start().

clients() ->
    case manager:getclients() of
	[] ->
	    io:format("No clients connected.~n");
	Clients ->
	    io:format("~p client(s):~n~n", [length(Clients)]),
	    [io:format("~s~n", [utils:client_to_string(Client)])
	     || Client <- Clients],
	    io:format("~n")
    end.

stop() ->
    io:format("Stopping server... "),
    manager:broadcast("EXIT:Shutdown"),
    listener:stop(),
    manager:stop(),
    io:format("server stopped.~n"),
    exit(normal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Internal functions                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cli_start() ->
    io:format("~n"),
    io:format("##########################~n"),
    io:format("#     KGNChat Server     #~n"),
    io:format("##########################~n"),    
    print_help(),
    cli().

print_help() ->
    io:format("~n"),
    io:format("Available commands~n"),
    io:format("------------------~n"),
    io:format("/clients \tList connected clients~n"),
    io:format("/help \t\tShow this dialog~n"),
    io:format("/stop \t\tStop the server~n"),
    io:format("text \t\tBroadcast 'text' to all clients~n"),
    io:format("~n").

cli() ->
    Input = io:get_line("> "),
    InputClean = string:strip(Input, right, $\n),
    Message = string:strip(InputClean),
    case Message of
	[] ->
	    ok;
	_ ->
	    case hd(Message) of
		$/ ->
		    handle_command(tl(Message));
		_ ->
		    manager:broadcast(Message)
	    end
    end,
    cli().

handle_command("clients") ->
    clients();
handle_command("help") ->
    print_help();
handle_command("stop") ->
    stop();
handle_command(_) ->
    io:format("Unknown command. Type '/help' to see available options.~n").

