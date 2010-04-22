{application, cdb,
 [
  {description, "collectdb is an Erlang application that stores your collectd packets in a database"},
  {vsn, "0.1"},
  {id, "cdb"},
  {modules,      [cdb_listener]},
  {registered,   [cdb_listener_sup, cdb_listener]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {cdb_app, []}},
  {env, [
		{port, 25826},
		{store, cdb_store_couchdb},
		{store_args, [
  		%% store_args' first element is a couchdb_params record:
  		%% -record(couchdb_params, {
      %%    host        = "127.0.0.1" :: string(),
      %%    port        = 5984 :: integer(),
      %%    ssl         = false :: boolean(),
      %%    prefix      = "/" :: string(),
      %%    username    = nil :: string(),
      %%    password    = nil :: string(),
      %%    name        = default :: term(),
      %%    timeout     = infinity :: integer() | infinity
      %% }).
      %% The name field will be overwritten with collectdb.
		  {couchdb_params, "127.0.0.1", 5984, false, "/", nil, nil, collectdb, infinity},
		  %% The second element is a tuple of database handle and name
		  {collectdb, "collectdb"}
		]}
	]}
 ]
}.