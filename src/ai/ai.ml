open Card
open DominionLib

class type interface =
  object
    method name : string
    method init : unit -> unit
    method select_card : Phase.t -> limit -> Deck.info_t list -> Deck.hand_t -> Supply.t -> Card.t option
  end
