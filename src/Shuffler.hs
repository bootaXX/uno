module Shuffler where

import Common
import System.Random
import System.Random.Shuffle

shuffleDeck :: State -> IO State
-- TODO: Implement a random shuffling algorithm
shuffleDeck state@State{
					players = plyrs,
					deck = dck,
					d_stack = dstck}
				= return State {
						players = plyrs,
						deck = shuffles seed,
						d_stack = dstck}

seed = mkStdGen 1000

-- Shuffle function
shuffles :: (RandomGen gen) => gen -> [Card]
shuffles = shuffle' fullDeck 108

-- Deal Cards
dealCards :: State -> State
dealCards state@State{
				players = plyrs,
				deck = dck,
				d_stack = dstck
				} = State{
						players = dealp dck plyrs,
						deck = dealc dck plyrs,
						d_stack = dstck
					}

dealp :: Deck -> [Player] -> [Player]
dealp [] _ = []
dealp _ [] = []
dealp (c1:c2:cs) (p:ps) = p{hand=[c1,c2]} : dealp cs ps

dealc :: Deck -> [Player] -> Deck
dealc [] _ = []
dealc deck [] = deck
dealc (c1:c2:cs) (p:ps) = dealc cs ps
							
