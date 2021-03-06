-module(emud_create_player).
-export([create_player/0]).

-include("emud.hrl").

create_player() ->
    Name = binary:list_to_bin(string:strip(io:get_line(standard_io, 'name> '), right, $\n)),
    case emud_player_db:player_exists(Name) of 
	true ->
	    {existing_player, Name, load_player(Name)};
	false -> 
	    %% new player
	    PData = #player_creation_data{name = Name},
	    {new_player, Name, set_stats(select_race(PData))}
    end.

set_stats(PData) ->
    %% FIXME : create much better stats, depending on race etc.
    PData#player_creation_data{strength = 57}.
    
load_player(Name) ->
    [{Name, State}] = emud_player_db:get_player(Name),
    State.

select_race(PData) ->
    Races = [orc, human, elf],
    io:fwrite("Choose a race. ~p.~n", [Races]),
    Race = string:strip(io:get_line(standard_io, 'race> '), right, $\n),
    StringRaces = [ atom_to_list(X) || X <- Races ],
    RaceExists = lists:any(fun(X) -> 
		      X == Race 
	      end, StringRaces),
    case RaceExists of 
	true ->
	   select_class(PData#player_creation_data{race = list_to_atom(Race)});
	false ->
	    io:fwrite("Unknown race~n", []),
	    select_race(PData)
    end.

select_class(PData) ->
    Classes = [mage, warrior, thief],
    io:fwrite("Choose a class. ~p.~n", [Classes]),
    Class = string:strip(io:get_line(standard_io, 'class> '), right, $\n),
    StringRaces = [ atom_to_list(X) || X <- Classes ],
    ClassExists = lists:any(fun(X) -> 
		      X == Class 
	      end, StringRaces),
    case ClassExists of 
	true ->
	   PData#player_creation_data{class = list_to_atom(Class)};
	false ->
	    io:fwrite("Unknown class~n", []),
	    select_class(PData)
    end.
