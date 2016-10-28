module Game where

import Common
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players = generateHumanPlayers n,
                     deck = Common.fullDeck,
                     d_stack = [ ] }
-- MY CODES----------------------------------------------------------
generateHumanPlayers :: Int -> [Player]
generateHumanPlayers n 
		| (n > 0) = [HPlayer {name = "Player" ++ show n, hand = [Card {color = Black, value = One}]}] ++ generateHumanPlayers (n-1)
		| otherwise = [ ]
---------------------------------------------------------------------

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = return (gs)
