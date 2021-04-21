(** The type [player_command] is a player command phrase where each
    element is word within the phrase. A word is defined to be a
    sequence of non-space characters Do not expose to outside, mli. *)
type player_command = string list

type command =
  | Quit
  | Restart
  | Played
  | Help
  | Kong
  | Mahjong
  | Discard of int
  | Continue
  | Pung
  | Chow of (int * int)

exception Invalid of string

let remove_space (str_list : player_command) : player_command =
  List.fold_left
    (fun acc str -> if str = "" then acc else str :: acc)
    [] (List.rev str_list)

let to_lower (lst : player_command) : player_command =
  List.map String.lowercase_ascii lst

let raise_invalid comment =
  raise (Invalid ("Invalid" ^ comment ^ "command"))

let parse str =
  match String.split_on_char ' ' str |> remove_space |> to_lower with
  | [] -> Continue
  | "help" :: t -> (
      match t with [] -> Help | _ -> raise_invalid "help")
  | "pung" :: t | "peng" :: t -> (
      match t with [] -> Pung | _ -> raise_invalid "pung")
  | "chow" :: t | "chi" :: t -> (
      match t with
      | [] -> raise_invalid "chow need two int"
      | [ hd ] -> raise_invalid "chow need two int, not one"
      | [ fst; snd ] -> (
          match int_of_string_opt fst with
          | None -> raise_invalid "chow '_->int' '_'"
          | Some index_1 -> (
              match int_of_string_opt snd with
              | None -> raise_invalid "chow '_' '_->int'"
              | Some index_2 -> Chow (index_1, index_2)))
      | _ -> raise_invalid "chow only two int, not more")
  | "kong" :: t
  | "gang" :: t
  | "angang" :: t
  | "hiddenkong" :: t
  | "ankong" :: t
  | "hidden" :: "kong" :: t
  | "an" :: "gong" :: t -> (
      match t with [] -> Kong | _ -> raise_invalid "kong")
  | "played" :: t | "view" :: "played" :: t -> (
      match t with [] -> Played | _ -> raise_invalid "played")
  | "mahjong" :: t | "hu" :: t -> (
      match t with [] -> Mahjong | _ -> raise_invalid "mahjong")
  | "quit" :: t -> (
      match t with [] -> Quit | _ -> raise_invalid "quit")
  | "discard" :: t | "play" :: t -> (
      match t with
      | [] -> raise_invalid "discard with a index"
      | [ index_str ] -> (
          match int_of_string_opt index_str with
          | None -> raise_invalid "non valid index"
          | Some index -> Discard index)
      | _ -> raise_invalid "enter discard and a index")
  | "restart" :: t | "new" :: "round" :: t -> (
      match t with [] -> Restart | _ -> raise_invalid "new round")
  | _ -> raise_invalid "??"
