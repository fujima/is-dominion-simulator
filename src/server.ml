
open Card
open DominionLib


exception End_of_Game of Deck.t list

(* AI configuration *)

module AI1 = Yano.GardenChuu;;
module AI0 = Fujima.Pikachu;;

let verbose = ref false

let ask player phase limit decks supply =
  let hand = Deck.get_hand (ListUtil.at decks player) in
  let deckinfos = List.map Deck.toInfo decks in
    begin
      match player with
	| 0 -> AI0.select_card
	| 1 -> AI1.select_card
	| _ -> assert false
    end phase limit deckinfos hand supply

let play card game =
  let (decks, (phase, player, limit), supply, numplayer, turn) = game in
  let newdecks = ListUtil.change_to decks player (Deck.play (ListUtil.at decks player) card) in
  let newgame = (newdecks,
		 (phase, 
		  player, 
		  limit), 
		 supply, 
		 numplayer, 
		 turn) in
  CardEffect.onPlay card newgame

let get_ai_name player =
  match player with
    | 0 -> AI0.name
    | 1 -> AI1.name
    | _ -> assert false

let print_line () =
  print_endline "--------------------------------------------------"

let print_hand decks player =
  print_endline (Deck.string_of_hand (ListUtil.at decks player))

let print_info (decks, (_, player, _), _, _, turns) =
  if !verbose then
    begin
      print_endline ("Turn " ^ (string_of_int turns));
      print_endline ("Player" ^ (string_of_int player) ^ " [" ^ (get_ai_name player) ^ "]");
      print_line ()
    end
  else
    ()

let print_status (decks, (phase, player, limit), _, _, _) =
  if !verbose then
    match phase with
      | Phase.Cleanup ->
          print_endline "  Clean up\n";
      | Phase.Action ->
          print_hand decks player;
      | Phase.Purchase ->
          print_endline ("Coin: "^(string_of_int limit.money));
      | _ -> ()
    else
      ()

let print_play phase card =
  if !verbose then
    match phase with
      | Phase.Cleanup ->
          print_endline "";
      | Phase.Action ->
          print_endline ("  Action   - " ^ (Card.string_of_card card));
      | Phase.Purchase ->
          print_endline ("  buy      - " ^ (Card.string_of_card card));
      | Phase.Treasure ->
          print_endline ("  Treasure - " ^ (Card.string_of_card card));
    else
      ()
      

let get_args () =
  let ret = ref false in
    for i = 0 to Array.length Sys.argv - 1 do
      if Sys.argv.(i) = "-v" then ret := true
    done;
    !ret
  
let _ =
  Random.self_init ();
  AI0.init ();
  AI1.init ();

  let options = get_args () in
  let numplayer = 2 in
  let deck0 = Deck.init_deck () in
  let deck1 = Deck.init_deck () in
    let rec loop game =
      print_status game;
      let (decks, (phase, player, limit), supply, numplayer, turn) = game in
	if phase = Phase.Cleanup then
	  if Supply.endgame supply || turn >= 100 then
	    raise (End_of_Game decks)
	  else 
	    let newdecks = 
	      ListUtil.change_to 
		decks 
		player 
		(Deck.cleanup (ListUtil.at decks player))
	    in
	    let newgame =
		  (newdecks,
		   ((Phase.next phase), 
		    (player + 1) mod numplayer, 
		    {action = 1; money = 0; buy = 1}), 
		   supply,
		   numplayer,
		   turn + 1)
	    in
	      print_info newgame;
	      loop newgame
	else
	  match ask player phase limit decks supply with
	    | None ->
	        loop (decks, (Phase.next phase, player, limit), supply, numplayer, turn)
	    | Some card ->
	        print_play phase card;
	        begin
		  match phase with
		    | Phase.Action ->
			if limit.action = 0 then assert false
			else
			  begin
			    match cardtype_of_card card with
			      | Cardtype.Action
			      | Cardtype.Attack
			      | Cardtype.Reaction ->
				  loop
				    (play
				       card
				       (decks,
					(Phase.Action,
					 player, 
					 {action = limit.action-1; money = limit.money; buy = limit.buy}),
					supply,
					numplayer,
					turn))
			      | _ -> assert false
			  end
		    | Phase.Treasure -> 
			begin
			  match cardtype_of_card card with
			    | Cardtype.Treasure x
			      -> loop
				(play
				   card
				   (decks,
				    (Phase.Treasure,
				     player, 
				     {action = limit.action; money = limit.money; buy = limit.buy}),
				    supply,
				    numplayer,
				    turn))
			    | _ -> assert false
			end
		    | Phase.Purchase ->
			if limit.buy = 0 || 
			   not (Supply.exist supply card) || 
			   (cost_of_card card) > limit.money
			then assert false
			else 
			  let newsupply = Supply.decrease supply card in
			  let newdeck = Deck.obtain (ListUtil.at decks player) card in
			    loop ((ListUtil.change_to
				     decks
				     player
				     newdeck),
				  (Phase.Purchase,
				   player,
				   { action = limit.action; 
				     money = limit.money - (cost_of_card card);
				     buy = limit.buy - 1}),
				  newsupply,
				  numplayer,
				  turn)
		    | _ -> assert false
		end
    in
      try
        verbose := options;
        let initial_game =          
	      ([deck0; deck1], 
	       (Phase.Action, 
	        0, 
	        {action = 1; money = 0; buy = 1}), 
	       (Supply.create numplayer),
	       numplayer,
	       0)
        in
          print_info initial_game;
          loop initial_game
      with End_of_Game decks ->
	
	let rec print_points points iter =
	  match points with
	    | [] -> ()
	    | x::xs -> 
		print_endline ("player" ^ (string_of_int iter) ^ " : " ^ (string_of_int x)); 
		print_points xs (iter+1)
	in
	  print_points (List.map Deck.count_victory decks) 0;;
