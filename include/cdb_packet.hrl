-record(data, {
  % Comments in each line are the corresponding part ids.
  host            = nil, %   0
  time            = nil, %   1
  plugin          = nil, %   2
  plugin_instance = nil, %   3
  type            = nil, %   4
  type_instance   = nil, %   5
  values          = [],  %   6
  interval        = nil, %   7
  message         = nil, % 100
  severity        = nil  % 101
}).

-define(VALUE_TYPES, [
	{0,counter}, {1,gauge}, {2,derive}, {3,absolute}
]).