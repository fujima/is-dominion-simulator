
open Card

type limit = {action : int; money : int; buy : int}

module Phase =
  struct
    type t = 
      | Action
      | Treasure
      | Purchase
      | Cleanup

    let next = function
      | Action -> Treasure
      | Treasure -> Purchase
      | Purchase -> Cleanup
      | Cleanup -> Action
  end;;

module Deck =
  struct
    type t = { deck : Card.t list; hand : Card.t list; playing : Card.t list; trash : Card.t list }
    type info_t = {d : int; h : int; p : int; t : int}
    type hand_t = Card.t list

    let init_deck () =
      let deck = ListUtil.shuffle 
	[Copper; Copper; Copper; Copper; Copper; Copper; Copper; 
	 Estate; Estate; Estate]
      in
	match deck with
	  | a::b::c::d::e::xs -> 
	      {deck = xs;
	       hand = [a; b; c; d; e];
	       playing = [];
	       trash = []}
	  | _ -> assert false

    let rec draw set =
      match set.deck with
	| [] -> begin 
	    match set.trash with
	      | [] -> {deck = []; hand = set.hand; playing = set.playing; trash = []}
	      | xs -> draw {deck = ListUtil.shuffle set.trash; 
			    hand = set.hand; 
			    playing = set.playing; 
			    trash = []}
	  end
	| x::xs -> {deck = xs; hand = x::set.hand; playing = set.playing; trash = set.trash}

    let play set card =
      try 
	let newhand = ListUtil.remove set.hand card in
	  {deck = set.deck; hand = newhand; playing = card::set.playing; trash = set.trash}
      with _ -> assert false

    let obtain set card =
      { deck = set.deck; hand = set.hand; playing = set.playing; trash = card::set.trash }

    let cleanup set =
      Util.repeat 
	draw 
	5 
	{deck = set.deck; hand = []; playing = []; trash = set.trash @ set.hand @ set.playing}

    let count_victory set =
      let rec count_victory' cards sum =
	match cards with
	  | [] -> sum
	  | x::xs -> 
	      count_victory' xs begin
		match cardtype_of_card x with
		  | Cardtype.Victory v -> sum + v
		  | Cardtype.Curse v   -> sum - v
		  | _         -> sum
	      end
      in
	count_victory' (set.deck @ set.hand @ set.playing @ set.trash) 0
      
    let get_hand set =
      set.hand

    let toInfo set =
      let l = List.length in
	{d = l set.deck; h = l set.hand; p = l set.playing; t = l set.trash}
	    
    let toString set =
      let rec string_of_hand = function
	| []    -> ""
	| x::xs -> (string_of_card x) ^ ", " ^ string_of_hand xs
      in
      "deck: " ^ (string_of_int (List.length set.deck)) ^ "\n"
      ^ "hand: " ^ (string_of_hand set.hand) ^ "\n"
      ^ "trash: " ^ (string_of_int (List.length set.trash))

  end;;

module Supply =
  struct
    type t = (Card.t * int) list
	
    let create numplayer =
      [Copper  , 30;
       Silver  , 30;
       Gold    , 30;
       Estate  ,  8;
       Duchy   ,  8;
       Province,  8;
       Curse   , 10;
       Smithy  , 10]

    let rec exist supply card =
      match supply with
	| [] -> false
	| (name,num)::xs -> if name = card then true else exist xs card

    let rec decrease supply card =
      match supply with
	| [] -> assert false
	| (name,num)::xs -> if name = card then (name,(num-1))::xs else (name, num) :: (decrease xs card)

    let rec lookup supply card =
      match supply with
	| [] -> assert false
	| (name,num)::xs -> if name = card then num else lookup xs card

    let endgame supply =
      let rec endgame' supply num =
	if num >= 3 then true
	else 
	  match supply with
	    | [] -> false
	    | (name,numcard)::xs -> 
		if numcard = 0 then
		  endgame' xs (num + (if name = Province then 3 else 1))
		else endgame' xs num
      in
	endgame' supply 0

    let rec toString = function
      | [] -> ""
      | (name,numcard)::xs -> 
	  (string_of_card name) ^ " : " ^ (string_of_int numcard) ^ "\n" ^ (toString xs) 
      
  end;;

type game = Deck.t list * (Phase.t * int) * Supply.t