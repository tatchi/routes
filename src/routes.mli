(** Typed routing for OCaml.
    [Routes] provides combinators for adding typed routing
    to OCaml applications. The core library will be independent
    of any particular web framework or runtime.
*)

(** [path] represents a sequence of path parameter patterns that are expected in a route. *)
type ('a, 'b) path

(** [route] is a combination of a path sequence, with a function that will be
    called on a successful match. When a path sequence matches, the patterns
    that are extracted are forwarded to said function with the types that the user
    defined. Note that because of {{: https://caml.inria.fr/pub/docs/manual-ocaml/polymorphism.html#ss:valuerestriction} value restriction}, the route definitions will be assigned a weak type by the compiler.
    This causes problems if one intends to re-use the same route definition in multiple contexts,
    like using a single definition for both matching a target url, and serializing to use in a client call.
    To avoid such problems one can use eta-expansion (i.e. add an explicit argument to the route definition).

    Example:

    {[
      let route () = Routes.(s "foo" / str / int /? nil @-->
        (fun (a : string) (b : int) ->
            Printf.sprintf "%s %d" a b))
    ]}

*)
type 'b route

(** [router] is a collection of multiple routes. It transforms a list of routes
    into a trie like structure, that is then used for matching an input target url.*)
type 'b router

(** [int] matches a path segment if it can be successfully coerced into an integer. *)
val int : ('a, 'b) path -> (int -> 'a, 'b) path

(** [int32] matches a path segment if it can be successfully coerced into a 32 bit integer. *)
val int32 : ('a, 'b) path -> (int32 -> 'a, 'b) path

(** [int64] matches a path segment if it can be successfully coerced into a 64 bit integer. *)
val int64 : ('a, 'b) path -> (int64 -> 'a, 'b) path

(** [str] matches any path segment and forwards it as a string. *)
val str : ('a, 'b) path -> (string -> 'a, 'b) path

(** [bool] matches a path segment if it can be successfully coerced into a boolean. *)
val bool : ('a, 'b) path -> (bool -> 'a, 'b) path

(** [s word] matches a path segment if it exactly matches [word]. The matched path param is then discarded. *)
val s : string -> ('a, 'b) path -> ('a, 'b) path

(** [nil] is used at the end of a path pattern sequence to indicate that a route
    should only match if it ends without a trailing slash. This can also be used
    to match against the root url '/'. *)
val nil : ('a, 'a) path

(** [trail] is used at the end of a path pattern sequence to indicate that a route
    should only match if it ends with a trailing slash. This should only be used
    at the end of a route. If a route needs to match the root url '/' please use [nil]
    instead. *)
val trail : ('a, 'a) path

(** [pattern] accepts two functions, one for converting a user provided type to
    a string representation, and another to potentially convert a string to the said type.
    With these two functions, it creates a pattern that can be used for matching a path segment.
    This is useful when there is a need for types that aren't provided out of the box
    by the library. It also accepts a string label that will be used when pretty printing
    the route pattern. it is recommended to use a string value starting with `:` character
    for the label, example: ':shape', ':float' etc

    Example:

    {[
      type shape =
        | Square
        | Circle

      let shape_of_string = function
        | "square" -> Some Square
        | "circle" -> Some Circle
        | _ -> None

      let shape_to_string = function
        | Square -> "square"
        | Circle -> "circle"

      let shape = Routes.pattern shape_to_string shape_of_string

      (* Now the shape pattern can be used just like any
         of the built in patterns like int, bool etc *)
      let route () = s "shape" / shape / s "create" /? nil
    ]}

*)
val pattern
  :  ('c -> string)
  -> (string -> 'c option)
  -> string
  -> ('a, 'b) path
  -> ('c -> 'a, 'b) path

(** [l / r] joins two path match patterns [l] and [r] into a pattern sequence, parse l followed by parse r.
    Example: If we want to define a route that matches a string followd by
    a constant "foo" and then an integer, we'd use the [/] operator like below:
    {[
      let route () = Routes.(str / s "foo" / int /? nil)
    ]} *)
val ( / ) : (('a, 'b) path -> 'c) -> ('d -> ('a, 'b) path) -> 'd -> 'c

(** [l /? r] is used to express the sequence of, parse l followed by parse r and then stop parsing.
    This is used at the end of the route pattern to define how a route should end. The right hand parameter
    [r] should be a pattern definition that cannot be used in further chains joined by [/] (One such operator is [nil]). *)
val ( /? ) : (('a, 'b) path -> 'c) -> ('a, 'b) path -> 'c

(** [r @--> h] is used to connect a route pattern [r] to a function [h] that gets called
    if this pattern is successfully matched.*)
val ( @--> ) : ('a, 'b) path -> 'a -> 'b route

(** [one_of] accepts a list of tuples comprised of route definitions
    of type ['b route] where 'b is the type that a successful route match will return.

    It transforms the input list of routes into a trie like structure that can later be used
    to perform route matches. *)
val one_of : 'b route list -> 'b router

(** [match'] accepts a router and the target url to match. *)
val match' : 'a router -> target:string -> 'a option

(** [sprintf] takes a route pattern as an input, and returns a string with the result
    of formatting the pattern into a URI path. *)
val sprintf : ('a, string) path -> 'a

(** [pp_path] can be used to pretty-print a path sequence. This can be useful
    to get a human readable output that indicates the kind of pattern
    that a route will match. When creating a custom pattern matcher
    using [pattern], a string label needs to be provided. This label
    is used by [pp_path] when preparing the pretty-print output.

    Example:
    {[
      let r () = Routes.(s "foo" / int / s "add" / bool);;
      Format.asprintf "%a" Routes.pp_path r;;
      -: "foo/:int/add/:bool"
    ]}
*)
val pp_path : Format.formatter -> ('a, 'b) path -> unit

(** [pp_route] is similar to [pp_path], except it takes a route (combination of path sequence
    and a handler) as input, instead of just a path sequence. *)
val pp_route : Format.formatter -> 'a route -> unit
