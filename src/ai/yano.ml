open Card
open DominionLib

class fushigidane : Ai.interface =
  object
    method name = "Fushigidane"

    val mutable num_smithy = 0

    method init () =
      num_smithy <- 0

    method select_card phase limit deckinfos myhand supply =
      match phase with
	| Phase.Action -> 
	    if limit.action = 0 then None else
	      (try
	         let act = List.find (fun x -> x = Festival) myhand in Some act
	       with Not_found ->
	         try
	            let act = List.find (fun x -> x = Market) myhand in Some act
	          with Not_found ->
	            try
	              let act = List.find (fun x -> x = Smithy) myhand in Some act
	            with Not_found -> None
	      )
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
		| 0 | 1     -> None
		| 2         -> if Supply.lookup supply Province < 4 then Some Estate else None
		| 3         -> if Supply.lookup supply Province < 3 then Some Estate else Some Silver
		| 4         -> if Supply.lookup supply Province < 3 then Some Estate else if num_smithy < 3 then (num_smithy <- num_smithy + 1;Some Smithy) else Some Silver
		| 5         -> if Supply.lookup supply Province < 5 then Some Duchy else Some Festival
		| 6 | 7     -> if Supply.lookup supply Province < 4 then Some Duchy else Some Gold
		| _         -> Some Province
	    end
	| Phase.Cleanup -> assert false
	    
  end

class gardenChuu : Ai.interface =
  object

    method name = "GardenChuu"

    val mutable num_woodcutter = 0

    method init () =
      num_woodcutter <- 0

    method select_card phase limit deckinfos myhand supply =
      match phase with
	| Phase.Action -> 
	    if limit.action = 0 then None else
	      (try
	         let act = List.find (fun x -> x = Market) myhand in Some act
	       with Not_found ->
	         try
	            let act = List.find (fun x -> x = Woodcutter) myhand in Some act
	          with Not_found -> None
	      )
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
		| 0 | 1     -> if Supply.lookup supply Province < 7 then Some Copper else None
		| 2         -> if Supply.lookup supply Province < 7 then Some Estate else None
		| 3         -> if Supply.lookup supply Province < 7 then Some Estate else if num_woodcutter < 5 then (num_woodcutter <- num_woodcutter + 1; Some Woodcutter) else Some Silver
		| 4         -> if Supply.lookup supply Province < 8 && Supply.lookup supply Gardens > 0 then Some Gardens else if num_woodcutter < 5 then (num_woodcutter <- num_woodcutter + 1; Some Woodcutter) else Some Silver
		| 5         -> if Supply.lookup supply Province < 5 then (if Supply.lookup supply Gardens > 0 then Some Gardens else Some Duchy) else Some Market
		| 6 | 7     -> if Supply.lookup supply Province < 5 then (if Supply.lookup supply Gardens > 0 then Some Gardens else Some Duchy) else Some Market
		| _         -> Some Province
	    end
	| Phase.Cleanup -> assert false
	    
  end
