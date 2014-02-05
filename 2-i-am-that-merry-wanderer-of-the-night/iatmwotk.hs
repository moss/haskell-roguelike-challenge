module IAmThatMerryWandererOfTheNight where
import System.Console.ANSI

main :: IO ()
main = do
    clearScreen
    setCursorPosition 12 40
    putChar '@'
    setCursorPosition 26 0
