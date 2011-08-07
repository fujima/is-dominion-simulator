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
    }
    type info_t = { d : int; h : int; p : int; t : int; }
    type hand_t = Card.t list
    val init_deck : unit -> t
    val draw : t -> t
    val play : t -> Card.t -> t
    val obtain : t -> Card.t -> t
    val cleanup : t -> t
    val count_victory : t -> int
    val get_hand : t -> Card.t list
    val toInfo : t -> info_t
    val toString : t -> string
  end
module Supply :
  sig
    type t = (Card.t * int) list
    val create : 'a -> (Card.t * int) list
    val exist : ('a * 'b) list -> 'a -> bool
    val decrease : ('a * int) list -> 'a -> ('a * int) list
    val lookup : ('a * 'b) list -> 'a -> 'b
    val endgame : (Card.t * int) list -> bool
    val toString : (Card.t * int) list -> string
  end
type game = Deck.t list * (Phase.t * int) * Supply.t
