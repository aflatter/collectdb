-module(cdb_store_couchdb).
-author('flatter@gmail.com').
-behaviour(cdb_store).
-include_lib("couchbeam/include/couchbeam.hrl").

-export([
	init/1, 				handle_call/3, 	handle_cast/2, 
	handle_info/2, 	terminate/2, 		code_change/3,
	start_link/2,   save/1
]).

-record(state, {
	connection,
	db
}).

start_link(Params, DbName) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Params, DbName], []).

init([Params = #couchdb_params{}, {DbHandle, DbName}]) ->
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	couchbeam:start(),
	#couchdb_params{name = Connection} = Params,
	couchbeam_server:start_connection_link(Params),
	couchbeam_db:open_or_create(Connection, {DbHandle, DbName}),
	{ok, #state{connection = Connection, db = DbHandle}}.

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

save(_Packet) ->
  io:format("Work to do.~n"),
	ok.
	
% to_doc(Data) ->
%  to_doc(data, record_info(fields, data)),
%  Doc = {
%    [
%      {<<"somefield">>, <<"somevalue">>}
%    ]
%  }.

%to_doc(Data = #data{}) ->
%  [ X || X <- record_info(fields, data)],
%%  Doc = {[
%    {<<"host">>, Data#data{}}
%  ]}