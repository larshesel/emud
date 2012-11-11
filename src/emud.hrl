%%-type in() :: string().
-type room() :: pid() | no_room.
-type player() :: pid().
-type item() :: pid().

-type race() :: orc | human  | elf.
-type class() :: mage | warrior | thief.

-record(player_creation_data, {name, attributes, class :: class(), race :: race(), strength}).
