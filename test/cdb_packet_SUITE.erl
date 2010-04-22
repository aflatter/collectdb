-module(cdb_packet_SUITE).
-compile(export_all).
-include("ct.hrl").
-include("cdb_packet.hrl").
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.
%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.
%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.
%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.
%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.
%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [].
%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> [
	unpack,
	unpack_counter_value,	unpack_gauge_value, unpack_derive_value, unpack_absolute_value,
	unpack_string_parts, 	unpack_time_part,	  unpack_value_part
].
%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
unpack(_Config) ->
	_Result = cdb_packet:unpack(real_packet()).

unpack_counter_value(_Config) ->
	{counter, 133} = cdb_packet:unpack_value(counter, <<133:64>>).

unpack_gauge_value(_Config) ->
	{gauge, 133} = cdb_packet:unpack_value(gauge, <<133:64/little>>).

unpack_derive_value(_Config) ->
	{derive, 133} = cdb_packet:unpack_value(derive, <<133:64/signed>>).

unpack_absolute_value(_Config) ->
	{absolute, 133} = cdb_packet:unpack_value(absolute, <<133:64>>).

unpack_string_parts(_Config) ->
  Test = fun(Elem, Acc) ->
    Elem1 = list_to_binary(integer_to_list(Elem)),
    Bin = <<110,111,110,97,109,101,32,Elem1/binary,0>>,
    cdb_packet:unpack_part_value(Elem, Bin, Acc)
  end,
  Result = lists:foldl(Test, #data{}, [0,2,3,4,5,100]),
	#data{
    host = "noname 0", plugin = "noname 2", plugin_instance = "noname 3",
	  type = "noname 4", type_instance = "noname 5", message = "noname 100"
	} = Result.

unpack_time_part(_Config) ->
	Bin = <<12345:64>>,
	Result = cdb_packet:unpack_part_value(1, Bin, #data{}),
	#data{time = 12345} = Result.
	
unpack_value_part(_Config) ->
	Bin = <<0,2,0,0,0,0,0,0,0,0,50,81,0,0,0,0,0,0,29,100>>,
	Result = cdb_packet:unpack_part_value(6, Bin, #data{}),
	#data{values = [{counter,7524},{counter,12881}]} = Result.

real_packet() -> 
	<<0,0,0,11,110,111,110,97,109,101,0,0,1,0,12,0,0,
    0,0,75,129,58,72,0,7,0,12,0,0,0,0,0,0,0,10,0,2,
    0,14,105,110,116,101,114,102,97,99,101,0,0,4,0,
    15,105,102,95,112,97,99,107,101,116,115,0,0,5,0,
    9,101,116,104,48,0,0,6,0,24,0,2,0,0,0,0,0,0,0,0,
    50,81,0,0,0,0,0,0,29,100,0,4,0,14,105,102,95,
    101,114,114,111,114,115,0,0,6,0,24,0,2,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,9,115,119,97,
    112,0,0,4,0,9,115,119,97,112,0,0,5,0,9,117,115,
    101,100,0,0,6,0,15,0,1,1,0,0,0,0,0,0,0,0,0,5,0,
    9,102,114,101,101,0,0,6,0,15,0,1,1,0,0,0,0,26,
    251,230,65,0,5,0,11,99,97,99,104,101,100,0,0,6,
    0,15,0,1,1,0,0,0,0,0,0,0,0,0,4,0,12,115,119,97,
    112,95,105,111,0,0,5,0,7,105,110,0,0,6,0,15,0,1,
    2,0,0,0,0,0,0,0,0,0,5,0,8,111,117,116,0,0,6,0,
    15,0,1,2,0,0,0,0,0,0,0,0,0,2,0,7,100,102,0,0,4,
    0,7,100,102,0,0,5,0,9,114,111,111,116,0,0,6,0,
    24,0,2,1,1,0,0,0,0,13,223,247,65,0,0,0,16,229,
    86,55,66,0,5,0,16,108,105,98,45,105,110,105,116,
    45,114,119,0,0,6,0,24,0,2,1,1,0,0,0,0,0,0,0,0,0,
    0,0,0,176,186,199,65,0,5,0,8,100,101,118,0,0,6,
    0,24,0,2,1,1,0,0,0,0,0,64,17,65,0,0,0,0,0,118,
    99,65,0,5,0,12,100,101,118,45,115,104,109,0,0,6,
    0,24,0,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,176,186,
    199,65,0,2,0,14,112,114,111,99,101,115,115,101,
    115,0,0,4,0,13,112,115,95,115,116,97,116,101,0,
    0,5,0,12,114,117,110,110,105,110,103,0,0,6,0,15,
    0,1,1,0,0,0,0,0,0,0,0,0,5,0,13,115,108,101,101,
    112,105,110,103,0,0,6,0,15,0,1,1,0,0,0,0,0,64,
    93,64,0,5,0,12,122,111,109,98,105,101,115,0,0,6,
    0,15,0,1,1,0,0,0,0,0,0,0,0,0,5,0,12,115,116,111,
    112,112,101,100,0,0,6,0,15,0,1,1,0,0,0,0,0,0,0,
    0,0,5,0,11,112,97,103,105,110,103,0,0,6,0,15,0,
    1,1,0,0,0,0,0,0,0,0,0,5,0,12,98,108,111,99,107,
    101,100,0,0,6,0,15,0,1,1,0,0,0,0,0,0,0,0,0,1,0,
    12,0,0,0,0,75,129,58,81,0,2,0,10,117,115,101,
    114,115,0,0,4,0,10,117,115,101,114,115,0,0,5,0,
    5,0,0,6,0,15,0,1,1,0,0,0,0,0,0,8,64,0,2,0,8,99,
    112,117,0,0,3,0,6,48,0,0,4,0,8,99,112,117,0,0,5,
    0,9,117,115,101,114,0,0,6,0,15,0,1,0,0,0,0,0,0,
    0,25,67,0,5,0,9,110,105,99,101,0,0,6,0,15,0,1,0,
    0,0,0,0,0,0,2,195,0,5,0,11,115,121,115,116,101,
    109,0,0,6,0,15,0,1,0,0,0,0,0,0,0,7,232,0,5,0,9,
    105,100,108,101,0,0,6,0,15,0,1,0,0,0,0,0,0,1,86,
    228,0,5,0,9,119,97,105,116,0,0,6,0,15,0,1,0,0,0,
    0,0,0,0,23,0,0,5,0,14,105,110,116,101,114,114,
    117,112,116,0,0,6,0,15,0,1,0,0,0,0,0,0,0,0,16,0,
    5,0,12,115,111,102,116,105,114,113,0,0,6,0,15,0,
    1,0,0,0,0,0,0,0,0,46,0,5,0,10,115,116,101,97,
    108,0,0,6,0,15,0,1,0,0,0,0,0,0,0,0,0>>.