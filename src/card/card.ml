type t =
  | Copper
  | Silver
  | Gold
  | Estate
  | Duchy
  | Province
  | Curse
  | Smithy

module Cardtype =
  struct
    type t =
      | Treasure of int
      | Victory of int
      | Action
      | Attack
      | Reaction
      | Curse of int
  end

let cost_of_card = function
  | Copper   -> 0
  | Silver   -> 3
  | Gold     -> 6
  | Estate   -> 2
  | Duchy    -> 5
  | Province -> 8
  | Curse    -> 0
  | Smithy   -> 4
      
let string_of_card = function
  | Copper   -> "Copper"
  | Silver   -> "Silver"
  | Gold     -> "Gold"
  | Estate   -> "Estate"
  | Duchy    -> "Duchy"
  | Province -> "Province"
  | Curse    -> "Curse"
  | Smithy    -> "Smithy"

let cardtype_of_card = function
  | Copper   -> Cardtype.Treasure 1
  | Silver   -> Cardtype.Treasure 2
  | Gold     -> Cardtype.Treasure 3
  | Estate   -> Cardtype.Victory 1
  | Duchy    -> Cardtype.Victory 3
  | Province -> Cardtype.Victory 6
  | Curse    -> Cardtype.Curse 1
  | Smithy   -> Cardtype.Action