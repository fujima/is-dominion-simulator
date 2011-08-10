
open Card
open DominionLib
open DominionIOLib

exception End_of_Game of Deck.t list

(* AI configuration *)

let ai0 = new Fujima.likeDuchy_too;;
let ai1 = new Fujima.pikachu;;

let verbose = ref false

let ask player phase limit decks supply =
  let hand = Deck.get_hand (ListUtil.at decks player) in
  let deckinfos = List.map Deck.toInfo decks in
    begin
      match player with
	| 0 -> ai0#select_card
	| 1 -> ai1#select_card
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
    | 0 -> ai0#name
    | 1 -> ai1#name
    | _ -> assert false      

let get_args () =
  let ret = ref false in
    for i = 0 to Array.length Sys.argv - 1 do
      if Sys.argv.(i) = "-v" then ret := true
    done;
    !ret
  
let _ =
  Random.self_init ();
  ai0#init ();
  ai1#init ();

  let options = get_args () in
  let numplayer = 2 in
  let deck0 = Deck.init_deck () in
  let deck1 = Deck.init_deck () in
    let rec loop game =
      (if !verbose then print_status game);
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
	    let nextplayer = (player + 1) mod numplayer in
	    let newgame =
		  (newdecks,
		   ((Phase.next phase), 
		    nextplayer, 
		    {action = 1; money = 0; buy = 1}), 
		   supply,
		   numplayer,
		   turn + 1)
	    in
	      (if !verbose then print_info newgame (get_ai_name nextplayer));
	      loop newgame
	else
	  match ask player phase limit decks supply with
	    | None ->
	        loop (decks, (Phase.next phase, player, limit), supply, numplayer, turn)
	    | Some card ->
	        (if !verbose then print_play phase card);
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
	  begin
            if !verbose then print_info initial_game (get_ai_name 0);
	    loop initial_game
	  end
      with End_of_Game decks ->
	
	let rec print_points points iter =
	  match points with
	    | [] -> ()
	    | x::xs -> 
		print_endline ("player" ^ (string_of_int iter) ^ " : " ^ (string_of_int x)); 
		print_points xs (iter+1)
	in
	  print_points (List.map Deck.count_victory decks) 0;;
