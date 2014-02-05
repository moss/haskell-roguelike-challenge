module IAmThatMerryWandererOfTheNight where
import Control.Monad
import System.Console.ANSI
import System.IO

type Position = (Int, Int)
data Command = Left | Right | Quit | Unknown deriving (Eq)

initScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen

parseInput :: [Char] -> [Command]
parseInput chars = takeWhile (/= Quit) $ map parseCommand chars

parseCommand :: Char -> Command
parseCommand 'q' = Quit
parseCommand _ = Unknown

draw (xpos, ypos) = do
    setCursorPosition xpos ypos
    putChar '@'
    setCursorPosition 26 0

clear (xpos, ypos) = do
    setCursorPosition xpos ypos
    putChar ' '
    setCursorPosition 26 0

advance (xpos, ypos) = do
    (xpos, ypos + 1)

main :: IO ()
main = do
    initScreen
    draw (12, 40)
    userInput <- getContents
    foldM_ updateScreen (12, 40) (parseInput userInput) where
      updateScreen curState command = do
        let newState = advance curState
        clear curState
        draw newState
        return newState
