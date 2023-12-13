val last : 'a list -> 'a
val print_char_list : char list -> unit
val print_int_list : int list -> unit
val print_pair : int * int -> unit
val get : 'a list -> int -> 'a
val range : int -> int -> int list
val to_pairs : 'a list -> ('a * 'a) list
val sum : int list -> int
val product : int list -> int
val print : int list -> string -> unit
val slice_from : 'a list -> int -> 'a list
val slice_to : 'a list -> int -> 'a list
val slice : 'a list -> int -> int -> 'a list
val insert : 'a list -> int -> 'a -> 'a list
val transpose : 'a list list -> 'a list list
