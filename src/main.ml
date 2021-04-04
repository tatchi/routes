type 'a conv =
  { to_string : 'a -> string
  ; of_string : string -> 'a option
  ; label : string
  }

let conv to_string of_string label = { to_string; of_string; label }

type ('a, 'b) path =
  | End : ('a, 'a) path
  | Wildcard : (string -> 'a, 'a) path
  | Match : string * ('a, 'b) path -> ('a, 'b) path
  | Conv : 'c conv * ('a, 'b) path -> ('c -> 'a, 'b) path

type slash_kind =
  | Trailing
  | NoSlash

type ('a, 'b) target =
  { slash_kind : slash_kind
  ; path : ('a, 'b) path
  }

type 'b route = Route : ('a, 'c) target * 'a * ('c -> 'b) -> 'b route

let int_conv = conv string_of_int int_of_string_opt ":int"
let int r = Conv (int_conv, r)
let empty = { slash_kind = NoSlash; path = End }

let testa =
  { slash_kind = Trailing
  ; path = Match ("user", Conv (int_conv, Match ("product", Conv (int_conv, End))))
  }
;;

let handler x y = Format.sprintf "userId: %i, productId: %i" x y

let _res =
  Route
    ( testa
    , (fun userId productId ->
        Format.sprintf "userId: %i, productId: %i" userId productId)
    , fun x -> x )
;;

let parse_route { slash_kind; path } handler params =
  let rec match_target : type a b. (a, b) path -> a -> string list -> b option =
   fun t f s ->
    match t with
    | End ->
      (match s, slash_kind with
      | [ "" ], Trailing -> Some f
      | [], NoSlash -> Some f
      | _ -> None)
    | Wildcard ->
      let x = String.concat "/" s in
      Some (f x)
    | Match (x, fmt) ->
      (match s with
      | x' :: xs when x = x' -> match_target fmt f xs
      | _ -> None)
    | Conv ({ of_string; _ }, fmt) ->
      (match s with
      | [] -> None
      | x :: xs ->
        (match of_string x with
        | None -> None
        | Some x' -> match_target fmt (f x') xs))
  in
  match_target path handler params
;;

let res = parse_route testa handler [ "user"; "5"; "product"; "6"; "" ];;

print_endline (Option.value ~default:"None" res)
