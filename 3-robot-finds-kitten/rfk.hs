module RobotFindsKitten where
import Control.Monad
import System.Console.ANSI
import System.IO

type Position = (Int, Int)
data Command = MoveLeft
             | MoveDown
             | MoveUp
             | MoveRight
             | Quit
             | Unknown
             deriving (Eq)

parseInput :: [Char] -> [Command]
parseInput chars = takeWhile (/= Quit) $ map parseCommand chars

parseCommand :: Char -> Command
parseCommand 'q' = Quit
parseCommand 'h' = MoveLeft
parseCommand 'j' = MoveDown
parseCommand 'k' = MoveUp
parseCommand 'l' = MoveRight
parseCommand _ = Unknown

advance MoveLeft (row, col) = (row, col - 1)
advance MoveUp (row, col) = (row - 1, col)
advance MoveDown (row, col) = (row + 1, col)
advance MoveRight (row, col) = (row, col + 1)
advance _ state = state

-- Here be IO Monad dragons

initScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen

draw (row, col) = do
    setCursorPosition row col
    putChar '@'
    setCursorPosition 26 0

clear (row, col) = do
    setCursorPosition row col
    putChar ' '
    setCursorPosition 26 0

main :: IO ()
main = do
    initScreen
    draw (12, 40)
    userInput <- getContents
    foldM_ updateScreen (12, 40) (parseInput userInput) where
      updateScreen curState command = do
        let newState = advance command curState
        clear curState
        draw newState
        return newState
