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

advance :: Position -> Command -> Position
advance (row, col) MoveLeft = (row, col - 1)
advance (row, col) MoveUp = (row - 1, col)
advance (row, col) MoveDown = (row + 1, col)
advance (row, col) MoveRight = (row, col + 1)
advance state _ = state

playGame :: [Char] -> Position -> [Position]
playGame userInput initState =
    scanl advance initState $ parseInput userInput

transitions :: [a] -> [(a, a)]
transitions list = zip ([head list] ++ list) list

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
    forM_ (transitions (playGame userInput robot)) updateScreen where
      updateScreen (oldState, newState) = do
        clear oldState
        drawR newState
        return newState
