
open Card
open DominionLib

let print_line () =
  print_endline "--------------------------------------------------"

let print_hand decks player =
  print_endline (Deck.string_of_hand (ListUtil.at decks player))

let print_info (decks, (_, player, _), _, _, turns) name =
  print_endline ("Turn " ^ (string_of_int turns));
  print_endline ("Player" ^ (string_of_int player) ^ " [" ^ name ^ "]");
  print_line ()

let print_status (decks, (phase, player, limit), _, _, _) =
  match phase with
    | Phase.Cleanup ->
        print_endline "  Clean up\n";
    | Phase.Action ->
        print_hand decks player;
    | Phase.Purchase ->
        print_endline ("Coin: "^(string_of_int limit.money));
    | _ -> ()

let print_play phase card =
  match phase with
    | Phase.Cleanup ->
        print_endline "";
    | Phase.Action ->
        print_endline ("  Action   - " ^ (Card.string_of_card card));
    | Phase.Purchase ->
        print_endline ("  buy      - " ^ (Card.string_of_card card));
    | Phase.Treasure ->
        print_endline ("  Treasure - " ^ (Card.string_of_card card));
