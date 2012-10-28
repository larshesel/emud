-module(emud_create_player).
-export([create_player/0]).

-include("emud.hrl").

create_player() ->
    Name = o:get_line(standard_io, 'name> '),
    case player_exists(Name) of 
	true ->
	    load_player();
	false -> 
	    %% new player
	    PData = #player_creation_data{name = Name},
	    select_race(PData)
    end.

load_player() ->
    ok.

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

player_exists(_Name) ->
    false.
