open Card
open DominionLib

class provinceLover : Ai.interface =
  object
    method name = "ProvinceLover"

    method init () =
      ()

    method select_card phase limit deckinfos myhand supply =
      match phase with
	| Phase.Action -> None
	| Phase.Treasure -> 
	    begin
	      try
		Some 
		  (List.hd 
		     (List.filter 
			(fun x -> 
			   match cardtype_of_card x with 
			     | Cardtype.Treasure _ -> true 
			     | _ -> false) 
			myhand))
	      with Failure "hd" -> None
	    end
	| Phase.Purchase -> 
	    begin
	      if limit.buy = 0 then None
	      else match limit.money with
		| 0 | 1 | 2 -> None
		| 3 | 4 | 5 -> Some Silver
		| 6 | 7     -> Some Gold
		| _         -> Some Province
	    end
	| Phase.Cleanup -> assert false

  end

class likeDuchy_too : Ai.interface =
  object

    method name = "LikeDuchytoo"

    method init () =
      ()

    method select_card phase limit deckinfos myhand supply =
      match phase with
	| Phase.Action -> None
	| Phase.Treasure -> 
	    begin
	      try
		Some 
		  (List.hd 
		     (List.filter 
			(fun x -> 
			   match cardtype_of_card x with 
			     | Cardtype.Treasure _ -> true 
			     | _ -> false) 
			myhand))
	      with Failure "hd" -> None
	    end
	| Phase.Purchase -> 
	    begin
	      if limit.buy = 0 then None
	      else match limit.money with
		| 0 | 1 | 2 -> None
		| 3 | 4     -> Some Silver
		| 5         -> if Supply.lookup supply Province < 5 then Some Duchy else Some Silver
		| 6 | 7     -> Some Gold
		| _         -> Some Province
	    end
	| Phase.Cleanup -> assert false
	    
  end

class pikachu : Ai.interface =
  object

    val mutable has_smithy = false

    method name = "Pikachu"

    method init () =
      has_smithy <- false

    method select_card phase limit deckinfos myhand supply =
      match phase with
	| Phase.Action -> 
	    if limit.action = 0 then None else (try let act = List.find (fun x -> x = Smithy) myhand in Some act with Not_found -> None)
	| Phase.Treasure -> 
	    begin
	      try
		Some 
		  (List.hd 
		     (List.filter 
			(fun x -> 
			   match cardtype_of_card x with 
			     | Cardtype.Treasure _ -> true 
			     | _ -> false) 
			myhand))
	      with Failure "hd" -> None
	    end
	| Phase.Purchase -> 
	    begin
	      if limit.buy = 0 then None
	      else match limit.money with
		| 0 | 1 | 2 -> None
		| 3         -> Some Silver
		| 4         -> if has_smithy then Some Silver else (has_smithy <- true; Some Smithy)
		| 5         -> if Supply.lookup supply Province < 5 then Some Duchy else if has_smithy then Some Silver else (has_smithy <- true; Some Smithy)
		| 6 | 7     -> Some Gold
		| _         -> Some Province
	    end
	| Phase.Cleanup -> assert false
	    
  end
