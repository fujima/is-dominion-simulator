

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
      | Laboratory ->
	  let newdecks = ListUtil.change_to decks player (Util.repeat Deck.draw 2 (ListUtil.at decks player)) in
	    (newdecks, 
	     (phase, player, {action = limit.action + 1; money = limit.money + 3; buy = limit.buy}), 
	     supply, 
	     numplayer, 
	     turn)
      | Estate
      | Duchy
      | Province
      | Curse -> assert false
