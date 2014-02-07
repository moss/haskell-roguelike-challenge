module RobotFindsKitten where
import Control.Monad
import System.Console.ANSI
import System.IO

type Position = (Int, Int)
data GameState = Playing { robot :: Position } | Over deriving (Eq)
data Command = MoveLeft
             | MoveDown
             | MoveUp
             | MoveRight
             | Quit
             | Unknown
             deriving (Eq)

parseInput :: [Char] -> [Command]
parseInput chars = map parseCommand chars

parseCommand :: Char -> Command
parseCommand 'q' = Quit
parseCommand 'h' = MoveLeft
parseCommand 'j' = MoveDown
parseCommand 'k' = MoveUp
parseCommand 'l' = MoveRight
parseCommand _ = Unknown

moveRobot :: (Int, Int) -> GameState -> GameState
moveRobot (rowDelta, colDelta) Playing { robot = (row, col) } =
    Playing { robot = (row + rowDelta, col + colDelta) }

advance :: GameState -> Command -> GameState
advance state MoveLeft = moveRobot (0, -1) state
advance state MoveUp = moveRobot (-1, 0) state
advance state MoveDown = moveRobot (1, 0) state
advance state MoveRight = moveRobot (0, 1) state
advance _ Quit = Over
advance state _ = state

playGame :: [Char] -> GameState -> [GameState]
playGame userInput initState = takeWhile (/= Over) $
    scanl advance initState $
    parseInput userInput

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

updateScreen (oldState, newState) = do
  clear (robot oldState)
  drawR (robot newState)
  when (robot newState == (13, 17))
    (putStr "Oh no robot! Where did the kitten go?")
  return newState

main :: IO ()
main = do
    let robot = (12, 40)
    let kitten = (13, 17)
    let gameState = Playing robot
    initScreen robot kitten
    userInput <- getContents
    forM_ (transitions (playGame userInput gameState)) updateScreen
