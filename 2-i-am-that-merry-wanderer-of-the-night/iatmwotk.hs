module IAmThatMerryWandererOfTheNight where
import Control.Monad
import System.Console.ANSI
import System.IO

initScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen

parseInput = takeWhile (/= 'q')

type Position = (Int, Int)

main :: IO ()
main = do
    initScreen
    let initialState = (12, 40)
    userInput <- getContents
    foldM_ updateScreen (12, 40) (parseInput userInput) where
      updateScreen (xpos, ypos) command = do
        setCursorPosition xpos ypos
        putChar '@'
        setCursorPosition 26 0
        return (xpos, ypos + 1)
