-module(cdb_store).
-author('flatter@gmail.com').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{save,1}] ++ gen_server:behaviour_info(callbacks);
behaviour_info(_Other) ->
    undefined.