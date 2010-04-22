-module(cdb_listener).
-author('flatter@gmail.com').
-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([
	init/1, 				handle_call/3, 	handle_cast/2, 
	handle_info/2, 	terminate/2, 		code_change/3
]).

start_link(Port) when is_integer(Port) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).
	
init([Port]) ->
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	case gen_udp:open(Port, [binary]) of
		{ok, Socket} 		-> {ok, Socket};
		{error, Reason} -> {stop, Reason}
	end.
	
handle_info({udp, _Socket, _IPtuple, _InPortNo, Packet}, State) -> 
	try
			Result = cdb_packet:unpack(Packet),
			io:format("~p~n", [Result]),
			cdb_store_couchdb:save(Result)
	catch
		% Catch errors and send a report but prevent server termination.
		error:Reason ->
			error_logger:error_report([
				"A packet could not be unpacked and/or stored.",
				{packet, Packet},
				{error, Reason}
			])
	end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.
	
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.
