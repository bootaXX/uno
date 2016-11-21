module Game where

import Common
import Shuffler
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

initialCardCount :: Int
initialCardCount = 7

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players = generateHumanPlayers n,
                     deck = Common.fullDeck,
                     d_stack = [ ] }
-- MY CODES----------------------------------------------------------
generateHumanPlayers :: Int -> [Player]
generateHumanPlayers n 
		| (n > 0) = [HPlayer {name = "Player" ++ show n, hand = [ ]}] ++ generateHumanPlayers (n-1)
		| otherwise = [ ]
---------------------------------------------------------------------

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = do
				curr <- shuffleDeck gs
				return (dealCards(curr))
