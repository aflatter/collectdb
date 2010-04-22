-module(cdb_app).
-author('flatter@gmail.com').
-behaviour(application).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,   25826).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    Port 	    = get_app_env(port,       undefined),
		Store     = get_app_env(store,      undefined),
		StoreArgs = get_app_env(store_args, undefined),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, Store, StoreArgs]).

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Store, StoreArgs]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % UDP Listener
              {   cdb_listener_sup,                        	% Id       = internal id
                  {cdb_listener,start_link,[Port]}, 		    % StartFun = {M, F, A}
                  permanent,                               	% Restart  = permanent | transient | temporary
                  2000,                                    	% Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  	% Type     = worker | supervisor
                  [cdb_listener]                          	% Modules  = [Module] | dynamic
              },
              % Datastore
              {   cdb_store_sup,                        		% Id       = internal id
                  {Store,start_link,StoreArgs}, 						% StartFun = {M, F, A}
                  permanent,                               	% Restart  = permanent | transient | temporary
                  2000,                                    	% Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  	% Type     = worker | supervisor
                  [Store]                          					% Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case application:get_env(cdb, Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.