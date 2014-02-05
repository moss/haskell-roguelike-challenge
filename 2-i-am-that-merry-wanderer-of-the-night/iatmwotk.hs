module IAmThatMerryWandererOfTheNight where
import System.Console.ANSI
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    setCursorPosition 12 40
    putChar '@'
    setCursorPosition 26 0
    command <- getChar
    return ()
