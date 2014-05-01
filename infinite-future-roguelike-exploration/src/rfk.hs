import Control.Monad

import Model
import Input
import Update
import LevelGeneration
import Rendering

instance GameView GameState where
  -- |view breaks up a GameState into UiElements
  -- >>> view Playing { robot = (3,4), message = "Hi!", over = False, level = [NKI 'x' (5,6) "a duck", Kitten 'k' (7,8)] }
  -- [Sprite 'x' (5,6),Sprite 'k' (7,8),Sprite '#' (3,4),Message "Hi!"]
  view Playing { robot = r, message = m, level = l } = (map itemView l) ++ [Sprite '#' r, Message m] 

itemView (Kitten representation position) = Sprite representation position
itemView (NKI representation position _) = Sprite representation position

-- TODO this is, frankly, also a bit of a hack
transitions :: [a] -> [(a, a)]
transitions list = zip ([head list] ++ list) list

-- Here be IO Monad dragons

main :: IO ()
main = do
    level <- generateLevel
    [robotPos] <- takeRandomPositions 1
    let gameState = Playing { robot = robotPos, message = "", over = False, level = level }
    initScreen gameState
    userInput <- getContents
    forM_ (transitions $ playGame (parseInput userInput) gameState) updateScreen
    putStrLn ""
