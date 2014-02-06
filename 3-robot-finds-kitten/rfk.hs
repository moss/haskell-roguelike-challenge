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

advance :: Command -> Position -> Position
advance MoveLeft (row, col) = (row, col - 1)
advance MoveUp (row, col) = (row - 1, col)
advance MoveDown (row, col) = (row + 1, col)
advance MoveRight (row, col) = (row, col + 1)
advance _ state = state

-- Here be IO Monad dragons

initScreen robot kitten = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    drawR robot
    drawK kitten

draw char (row, col) = do
    setCursorPosition row col
    putChar char
    setCursorPosition 26 0

drawR = draw '#'
drawK = draw 'k'
clear = draw ' '

main :: IO ()
main = do
    let robot = (12, 40)
    let kitten = (13, 17)
    initScreen robot kitten
    userInput <- getContents
    foldM_ updateScreen robot (parseInput userInput) where
      updateScreen curState command = do
        let newState = advance command curState
        clear curState
        drawR newState
        return newState
