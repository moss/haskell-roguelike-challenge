module IAmThatMerryWandererOfTheNight where
import System.Console.ANSI
import System.IO

initScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen

main :: IO ()
main = do
    initScreen
    setCursorPosition 12 40
    putChar '@'
    setCursorPosition 26 0
    command <- getChar
    return ()
