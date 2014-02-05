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

main :: IO ()
main = do
    initScreen
    setCursorPosition 12 40
    putChar '@'
    setCursorPosition 26 0
    userInput <- getContents
    foldM_ updateScreen 1 (parseInput userInput) where
      updateScreen age command = do
        setCursorPosition 12 40
        putChar ' '
        setCursorPosition 26 0
        print age
        return (age + 1)
