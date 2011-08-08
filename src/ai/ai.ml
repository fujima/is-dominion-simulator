open Card
open DominionLib

module type Interface =
  sig
    val name : string
    val init        : unit -> unit
    val select_card : Phase.t -> limit -> Deck.info_t list -> Deck.hand_t -> Supply.t -> Card.t option
  end
