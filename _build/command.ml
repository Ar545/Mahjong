type player_command = string list

type command =
  | Continue
  | Help
  | Pung
  | Chow of player_command
  | Kong
  | Mahjong
  | Played
  | Quit

exception Invalid of string

let remove_space str_list =
  List.fold_left
    (fun acc str -> if str = "" then acc else str :: acc)
    [] (List.rev str_list)

let to_lower lst = List.map String.lowercase_ascii lst

let raise_invalid comment =
  raise (Invalid ("Invalid" ^ comment ^ "command"))

let parse str =
  match String.split_on_char ' ' str |> remove_space |> to_lower with
  | [] -> Continue
  | "help" :: t -> (
      match t with [] -> Help | _ -> raise_invalid "help")
  | "pung" :: t -> (
      match t with [] -> Pung | _ -> raise_invalid "pung")
  | "chow" :: t -> (
      match t with [] -> raise_invalid "chow" | _ -> Chow t)
  | "kong" :: t -> (
      match t with [] -> Kong | _ -> raise_invalid "kong")
  | "played" :: t -> (
      match t with [] -> Played | _ -> raise_invalid "played")
  | "mahjong" :: t -> (
      match t with [] -> Mahjong | _ -> raise_invalid "mahjong")
  | "quit" :: t -> (
      match t with [] -> Quit | _ -> raise_invalid "quit")
  | _ -> raise_invalid ""
