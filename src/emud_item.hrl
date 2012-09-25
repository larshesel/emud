%% TODO: Move records away from hrl files if possible
%% annoying that all internal state is exposed.
-type picked_up_state() :: picked_up | not_picked_up.

-record(item_state, {description, 
		     short_description, 
		     interaction_names, 
		     picked_up_state = not_picked_up :: picked_up_state(),
		     pickup_requirements=[]}).
