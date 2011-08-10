val random_split : 'a list * int -> ('a list * int) * ('a list * int)
val shuffle : 'a list -> 'a list
val at : 'a list -> int -> 'a
val change_to : 'a list -> int -> 'a -> 'a list
val remove : 'a list -> 'a -> 'a list
val nth_map : ('a -> 'a) -> 'a list -> int -> 'a list
