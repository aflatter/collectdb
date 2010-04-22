-module(cdb_packet).
-author('flatter@gmail.com').

-ifdef(debug). 
-compile(export_all). 
-else.
-export([unpack/1]).
-endif.

-include("cdb_packet.hrl").

unpack(Binary) -> unpack(Binary, #data{}, []).

unpack(<<>>, _Data = #data{}, Result) -> Result;
unpack(Binary, Data = #data{values = []}, Result) ->
	{PartID, Length, Rem1} = unpack_part_header(Binary),
  {Data1,  Rem2}         = unpack_part_value(Rem1, PartID, Length, Data),
  unpack(Rem2, Data1, Result);
% Values field is set - add data to the result and unset values.
unpack(Binary, Data = #data{}, Result) ->
  Data1 = Data#data{values = []},
  unpack(Binary, Data1, [Data|Result]).

%% Unpacks part id and length from Binary. Return value is a triplet
%% of those plus the Binary remainder.
unpack_part_header(Binary) ->
  <<PartID:16, Length:16, Rem/binary>> = Binary,
  % Length includes the 4 header bytes; we need to get rid of them.
  {PartID, Length - 4, Rem}.

%% Takes Length bytes from Binary and transforms them to an Erlang
%% expression using the part data type. The result is a tuple of
%% the updated data record and the Binary remainder.
unpack_part_value(Binary, PartID, Length, Data = #data{}) ->
  <<Value:Length/binary-unit:8, Rem/binary>> = Binary,
  Data1 = unpack_part_value(PartID, Value, Data),
  {Data1, Rem}.

%% I thought about doing this using record_info(fields, data),
%% but this is just easier.
unpack_part_value(  0, Value, Data = #data{}) ->
  Data#data{host            = unpack_value(string,  Value)};
unpack_part_value(  1, Value, Data = #data{}) ->
  Data#data{time            = unpack_value(numeric, Value)};
unpack_part_value(  2, Value, Data = #data{}) ->
  Data#data{plugin          = unpack_value(string,  Value)};
unpack_part_value(  3, Value, Data = #data{}) ->
  Data#data{plugin_instance = unpack_value(string,  Value)};
unpack_part_value(  4, Value, Data = #data{}) ->
  Data#data{type            = unpack_value(string,  Value)};
unpack_part_value(  5, Value, Data = #data{}) ->
  Data#data{type_instance   = unpack_value(string,  Value)};
unpack_part_value(  6, Value, Data = #data{}) ->
  Data#data{values          = unpack_value(other,   Value)};
unpack_part_value(  7, Value, Data = #data{}) ->
  Data#data{interval        = unpack_value(numeric, Value)};
unpack_part_value(100, Value, Data = #data{}) ->
  Data#data{message         = unpack_value(string,  Value)};
unpack_part_value(101, Value, Data = #data{}) ->
  Data#data{severity        = unpack_value(numeric, Value)}.

%% @doc Unpacks multiple values from a values part.
unpack_values(0, <<>>, <<>>, Result) -> Result;
unpack_values(No, Types, Values, Result) ->
	% Extract the first type id and the first value.
	<<TypeID:8, TypesRem/binary>> = Types,
	<<Value:64/binary-unit:1, ValuesRem/binary>> = Values,
	{TypeID, Name} = lists:keyfind(TypeID, 1, ?VALUE_TYPES),
	unpack_values(
		No - 1, TypesRem, ValuesRem, [unpack_value(Name, Value)|Result]
	).

unpack_value(string, Value) ->
	% Remove null byte.
	Length = size(Value) - 1,
	<<String:Length/binary-unit:8, 0:8>> = Value,
	binary_to_list(String);
unpack_value(numeric, <<Value:64>>) -> 
  Value;
unpack_value(other, Value) ->
	<<No:16,Rem/binary>> = Value,
	% Split the binary into types and values.
	{Types, Values} = split_binary(Rem, No),
	unpack_values(No, Types, Values, []);

unpack_value(counter,  <<Value:64>>) ->
	{counter, Value};
unpack_value(gauge,    <<Value:64/little>>) ->
	{gauge, Value};
unpack_value(derive,   <<Value:64/signed>>) ->
	{derive, Value};
unpack_value(absolute, <<Value:64>>) ->
	{absolute, Value}.