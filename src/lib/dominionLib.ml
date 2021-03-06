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
    type t = { deck : Card.t list; hand : Card.t list; playing : Card.t list; trash : Card.t list; aside : Card.t list}
    type info_t = {d : int; h : int; p : int; t : int}
    type hand_t = Card.t list

    exception Hand_Not_Exist

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
	       trash = [];
               aside = []}
	  | _ -> assert false
           
    let rec draw set =
      match set.deck with
	| [] -> begin 
	    match set.trash with
	      | [] -> {deck = []; hand = set.hand; playing = set.playing; trash = []; aside = set.aside}
	      | xs -> draw {deck = ListUtil.shuffle set.trash; 
			    hand = set.hand; 
			    playing = set.playing; 
			    trash = [];
                            aside = set.aside}
	  end
	| x::xs -> {deck = xs; hand = x::set.hand; playing = set.playing; trash = set.trash; aside = set.aside}

    (* 手札からプレイ *)
    let play set card =
      try 
	let newhand = ListUtil.remove set.hand card in
	  {deck = set.deck; hand = newhand; playing = card::set.playing; trash = set.trash; aside = set.aside}
      with _ -> raise Hand_Not_Exist

    (* 獲得 *)
    let obtain set card =
      { deck = set.deck; hand = set.hand; playing = set.playing; trash = card::set.trash; aside = set.aside}

    (* 手札に獲得 *)
    let obtain_in_hand set card =
      { deck = set.deck; hand = card::set.hand; playing = set.playing; trash = set.trash; aside = set.aside}

    (* 手札から破棄 *)
    let trash set card =
      try
        let newhand = ListUtil.remove set.hand card in
          {deck = set.deck; hand = newhand; playing = set.playing; trash = set.trash; aside = set.aside}
      with e -> raise e 

    (* デッキトップに置く *)
    let put_on_top set card =
      { deck = card::set.deck; hand = set.hand; playing = set.playing; trash = set.trash; aside = set.aside}

    (* 手札から捨てる *)
    let discard set card =
      try
        obtain (trash set card) card
      with e -> raise e

    (* 手札からデッキトップに戻す *)
    let return_to_top set card =
      try
        put_on_top (trash set card) card
      with e -> raise e

    let cleanup set =
      Util.repeat 
	draw 
	5 
	{deck = set.deck; hand = []; playing = []; trash = set.trash @ set.hand @ set.playing @ set.aside; aside = []}

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
      let rec count_gardens cards sum =
        match cards with
          | [] -> sum
          | x::xs ->
              count_gardens xs begin
                if x = Gardens then sum + 1 else sum
              end
      in
      let rec count_duke cards (duke_num,duchy_num) =
        match cards with
          | [] -> duke_num * duchy_num
          | x::xs ->
              count_duke xs begin
                if x = Duke then (duke_num + 1,duchy_num) 
	else if x = Duchy then (duke_num,duchy_num + 1)
	else (duke_num,duchy_num)		  
              end
      in
      let all_cards = (set.deck @ set.hand @ set.playing @ set.trash @ set.aside) in
	(count_victory' all_cards 0) + (List.length all_cards) / 10 * (count_gardens all_cards 0)
	  + (count_duke all_cards (0,0))
  
	      
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

    let string_of_hand set =
      let rec string_of_hand = function
	    | []    -> ""
	    | x::xs -> (string_of_card x) ^ ", " ^ string_of_hand xs
      in
        "hand: " ^ (string_of_hand set.hand)

  end;;

module Supply =
  struct
    type t = (Card.t * int) list
	
    exception Supply_Not_Exist

    let create numplayer =
      [Copper  , 30;
       Silver  , 30;
       Gold    , 30;
       Estate  ,  8;
       Duchy   ,  8;
       Province,  8;
       Curse   , 10;
       Smithy  , 10;
       Village , 10;
       Woodcutter, 10;
       Laboratory, 10;
       Festival, 10;
       Market, 10;
       Gardens, 8;
       Duke, 8;
       ]

    let rec exist supply card =
      match supply with
	| [] -> false
	| (name,num)::xs -> if name = card then true else exist xs card

    let rec decrease supply card =
      match supply with
	| [] -> raise Supply_Not_Exist
	| (name,num)::xs ->
	    if name = card then
	      begin
	        if num = 0 then raise Supply_Not_Exist
	        else (name,(num-1))::xs
	      end
	    else (name, num) :: (decrease xs card)

    let rec lookup supply card =
      match supply with
	| [] -> raise Supply_Not_Exist
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
