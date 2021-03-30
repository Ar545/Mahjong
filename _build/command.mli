(** Parsing Player's Commands *)

(** The type [player_command] is a player command phrase where each
    element is word within the phrase. A word is defined to be a
    sequence of non-space characters *)
type player_command = string list

(** The type [command] is a parsed player command that is composed of a
    verb and, depending on the command, followed by a player command *)
type command =
  | Continue
  | Help
  | Pung
  | Chow of player_command
  | Kong
  | Quit

(** [Invalid] is an exception that is raised when a command is invalid
    and can not be parsed *)
exception Invalid of string

(** [parse str] translates a player's input command into the
    corresponding [command] if valid. If [str] is empty, then the game
    will continue without further action. Otherwise, the function will
    parse the first word as a verb and the rest, if not empty, will be
    the player_command

    Requires: [str] to be composed of only alphanumeric characters and
    spaces.

    Raises: [Invalid] when the player's input command is invalid, which
    corresponds to the following criteria: *)
val parse : string -> command
