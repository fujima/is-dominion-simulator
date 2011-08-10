

open Card
open DominionLib

let onPlay card game =
  let (decks, (phase, player, limit), supply, numplayer, turn) = game in
    match card with
      | Copper ->
	  (decks, 
	   (phase, player, {action = limit.action; money = limit.money + 1; buy = limit.buy}), 
	   supply, 
	   numplayer, 
	   turn)
      | Silver ->
	  (decks, 
	   (phase, player, {action = limit.action; money = limit.money + 2; buy = limit.buy}), 
	   supply, 
	   numplayer, 
	   turn)
      | Gold ->
	  (decks, 
	   (phase, player, {action = limit.action; money = limit.money + 3; buy = limit.buy}), 
	   supply, 
	   numplayer, 
	   turn)
      | Smithy -> (* 鍛冶屋 *)
	  let newdecks = ListUtil.change_to decks player (Util.repeat Deck.draw 3 (ListUtil.at decks player)) in
	  (newdecks, 
	   (phase, player, {action = limit.action; money = limit.money; buy = limit.buy}), 
	   supply, 
	   numplayer, 
	   turn)
      | Village -> (* 村 *)
	  let newdecks = ListUtil.change_to decks player (Util.repeat Deck.draw 1 (ListUtil.at decks player)) in
	  (newdecks, 
	   (phase, player, {action = limit.action + 2; money = limit.money; buy = limit.buy}), 
	   supply, 
	   numplayer, 
	   turn)
      | Woodcutter -> (* 木こり *)
	  (decks, 
	   (phase, player, {action = limit.action; money = limit.money + 2; buy = limit.buy + 1}), 
	   supply,
	   numplayer, 
	   turn)
      | Laboratory -> (* 研究所 *)
	  let newdecks = ListUtil.change_to decks player (Util.repeat Deck.draw 2 (ListUtil.at decks player)) in
	    (newdecks, 
	     (phase, player, {action = limit.action + 1; money = limit.money; buy = limit.buy}), 
	     supply, 
	     numplayer, 
	     turn)
      | Festival -> (* 祝祭 *)
	  (decks, 
	   (phase, player, {action = limit.action + 2; money = limit.money + 2; buy = limit.buy + 1}), 
	   supply,
	   numplayer, 
	   turn)
      | Market   -> (* 市場 *)
	  let newdecks = ListUtil.change_to decks player (Util.repeat Deck.draw 1 (ListUtil.at decks player)) in
	  (newdecks, 
	   (phase, player, {action = limit.action + 1; money = limit.money + 1; buy = limit.buy + 1}), 
	   supply, 
	   numplayer, 
	   turn)
      | Council_room -> (* 議事堂 *)
	  let (newdecks, _) = List.fold_left begin
	    fun (lst, i) deck -> 
	      (lst @ [(Util.repeat Deck.draw (if i = player then 4 else 1) (ListUtil.at decks i))], i+1)
	  end ([], 0) decks in
	    (newdecks, 
	     (phase, player, {action = limit.action; money = limit.money; buy = limit.buy + 1}), 
	     supply, 
	     numplayer, 
	     turn)
      | Witch -> (* 魔女 *)
	  let (newdecks, _) = List.fold_left begin
	    fun (lst, i) deck -> 
	      (lst @ 
		 [if i = player then 
		    Util.repeat Deck.draw 2 (ListUtil.at decks i) 
		  else
		    Deck.obtain (ListUtil.at decks i) Curse
		 ], 
	       i+1)
	  end ([], 0) decks in
	    (newdecks, 
	     (phase, player, {action = limit.action; money = limit.money; buy = limit.buy}), 
	     supply, 
	     numplayer, 
	     turn)
      | Moneylender -> (* 金貸し *)
	  let (newdecks, plusmoney) = 
	    begin
	      try
		ListUtil.change_to decks player (Deck.trash (ListUtil.at decks player) Copper), 3
	      with Invalid_argument _ -> decks, 0
	    end
	  in
	    (newdecks, 
	     (phase, player, {action = limit.action; money = limit.money + plusmoney; buy = limit.buy}), 
	     supply, 
	     numplayer, 
	     turn)
      | Gardens
      | Duke
      | Estate
      | Duchy
      | Province
      | Curse -> assert false
