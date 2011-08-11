type t =
  | Copper
  | Silver
  | Gold
  | Estate
  | Duchy
  | Province
  | Curse
  | Smithy
  | Village
  | Woodcutter
  | Laboratory
  | Festival
  | Market
  | Gardens
  | Council_room
  | Witch
  | Moneylender
  | Duke

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
  | Village  -> 3
  | Woodcutter -> 3
  | Laboratory -> 5
  | Festival -> 5
  | Market   -> 5
  | Gardens  -> 4
  | Council_room -> 5
  | Witch -> 5
  | Moneylender -> 4
  | Duke -> 5
      
let string_of_card = function
  | Copper   -> "Copper"
  | Silver   -> "Silver"
  | Gold     -> "Gold"
  | Estate   -> "Estate"
  | Duchy    -> "Duchy"
  | Province -> "Province"
  | Curse    -> "Curse"
  | Smithy    -> "Smithy"
  | Village  -> "Village"
  | Woodcutter -> "Woodcutter"
  | Laboratory -> "Laboratory"
  | Festival -> "Festival"
  | Market   -> "Market"
  | Gardens  -> "Gardens"
  | Council_room -> "Council room"
  | Witch -> "Witch"
  | Moneylender -> "Moneylender"
  | Duke -> "Duke"

let cardtype_of_card = function
  | Copper   -> Cardtype.Treasure 1
  | Silver   -> Cardtype.Treasure 2
  | Gold     -> Cardtype.Treasure 3
  | Estate   -> Cardtype.Victory 1
  | Duchy    -> Cardtype.Victory 3
  | Province -> Cardtype.Victory 6
  | Curse    -> Cardtype.Curse 1
  | Smithy   -> Cardtype.Action
  | Village  -> Cardtype.Action
  | Woodcutter -> Cardtype.Action
  | Laboratory -> Cardtype.Action
  | Festival -> Cardtype.Action
  | Market   -> Cardtype.Action
  | Gardens  -> Cardtype.Victory 0
  | Council_room -> Cardtype.Action
  | Witch -> Cardtype.Action
  | Moneylender -> Cardtype.Action
  | Duke -> Cardtype.Victory 0

exception Action_Limit_Exceeded
exception Card_Not_Playable
