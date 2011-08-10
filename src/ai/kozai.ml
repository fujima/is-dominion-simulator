open Card
open DominionLib

module DukeLover : Ai.Interface =
  struct
    let name = "DukeLover"

    let num_duchy = ref 0
    let num_duke = ref 0
    let num_gold = ref 0

    let init () =
      num_duchy := 0; num_duke := 0; num_gold := 0

    let select_card phase limit deckinfos myhand supply =
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
		| 0 | 1       -> None
		| 2           -> if Supply.lookup supply Province < 4 then Some Estate else None
		| 3 | 4       -> if Supply.lookup supply Province < 4 then Some Estate else Some Silver
		| 5           -> if !num_gold < 2 then Some Silver else if !num_duchy < !num_duke + 3 then (num_duchy := !num_duchy + 1; Some Duchy) else (num_duke := !num_duke + 1; Some Duke) 
		| 6 | 7 | _   -> if !num_gold < 2 then (num_gold := !num_gold + 1; Some Gold) else if !num_duchy < !num_duke + 3 then (num_duchy := !num_duchy + 1; Some Duchy) else (num_duke := !num_duke + 1; Some Duke) 
	    end
	| Phase.Cleanup -> assert false

  end
