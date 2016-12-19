module Shuffler where

import Common
import System.Random
import System.Random.Shuffle
import Data.List

shuffleDeck :: State -> IO State
-- TODO: Implement a random shuffling algorithm
shuffleDeck state@State{
players = plyrs, e_players = eplayers, deck = dck, d_stack = dstck, cur_player = curplayer} = return State {
players = plyrs,
e_players = eplayers,
deck = shuffles seed,
d_stack = dstck,
cur_player = curplayer}

seed = mkStdGen 1000

-- Shuffle function
shuffles :: (RandomGen gen) => gen -> [Card]
shuffles = shuffle' fullDeck 108

-- Deal Cards
dealCards :: State -> State
dealCards state@State{
players = plyrs,
e_players = eplayers,
deck = dck,
d_stack = dstck,
cur_player = curplayer
} = State{
players = dealp dck plyrs,
e_players = eplayers,
deck = dealc dck plyrs,
d_stack = setup_discard dstck dck,
cur_player = curplayer
}

dealp :: Deck -> [Player] -> [Player]
dealp [] _ = []
dealp _ [] = []
dealp (c1:c2:c3:c4:c5:c6:c7:cs) (p:ps) = p{hand=[c1,c2,c3,c4,c5,c6,c7]} : dealp cs ps

dealc :: Deck -> [Player] -> Deck
dealc [ ] _ = [ ]
dealc deck [ ] = drop 1 deck
dealc deck (p:ps) = dealc (drop 7 deck) ps

setup_discard :: D_Stack -> Deck -> D_Stack
setup_discard dstack [ ] = dstack
setup_discard dstack deck = dstack ++ (take 1 deck)