import Control.Monad

import Model
import Input
import Update
import LevelGeneration
import Rendering

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
