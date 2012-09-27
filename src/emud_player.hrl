-type room() :: pid() | 'no_room'.
-type item() :: pid().
-type output_server() :: pid() | none.
-type player() :: pid().

-type strength() :: integer().

-record(state, {name="",
		strength = 0 :: integer(), 
		room = no_room :: room(), 
		items=[] :: list(item()), 
		output_server = none:: output_server(),
		description="",
		short_description=""}).

