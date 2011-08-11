type limit = { action : int; money : int; buy : int; }
module Phase :
  sig type t = Action | Treasure | Purchase | Cleanup val next : t -> t end
module Deck :
  sig
    type t = {
      deck : Card.t list;
      hand : Card.t list;
      playing : Card.t list;
      trash : Card.t list;
      aside : Card.t list;
    }
    type info_t = { d : int; h : int; p : int; t : int; }
    type hand_t = Card.t list
    exception Hand_Not_Exist
    val init_deck : unit -> t
    val draw : t -> t
    val play : t -> Card.t -> t
    val obtain : t -> Card.t -> t
    val obtain_in_hand : t -> Card.t -> t
    val trash : t -> Card.t -> t
    val put_on_top : t -> Card.t -> t
    val discard : t -> Card.t -> t
    val return_to_top : t -> Card.t -> t
    val cleanup : t -> t
    val count_victory : t -> int
    val get_hand : t -> Card.t list
    val toInfo : t -> info_t
    val toString : t -> string
    val string_of_hand : t -> string
  end
module Supply :
  sig
    type t = (Card.t * int) list
    exception Supply_Not_Exist
    val create : 'a -> (Card.t * int) list
    val exist : ('a * 'b) list -> 'a -> bool
    val decrease : ('a * int) list -> 'a -> ('a * int) list
    val lookup : ('a * 'b) list -> 'a -> 'b
    val endgame : (Card.t * int) list -> bool
    val toString : (Card.t * int) list -> string
  end
type game = Deck.t list * (Phase.t * int) * Supply.t
