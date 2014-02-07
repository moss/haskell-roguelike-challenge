module RobotFindsKitten where
import Control.Monad
import System.Console.ANSI
import System.IO

type Position = (Int, Int)
data GameState = Playing { robot :: Position, kitten :: Position }
               | FoundKitten
               | Over deriving (Eq, Show)
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
moveRobot (rowDelta, colDelta) Playing { robot = (row, col), kitten = k } =
    let newR = (row + rowDelta, col + colDelta) in
    if (k == newR) then FoundKitten else Playing { robot = newR, kitten = k }

advance :: GameState -> Command -> GameState
advance state MoveLeft = moveRobot (0, -1) state
advance state MoveUp = moveRobot (-1, 0) state
advance state MoveDown = moveRobot (1, 0) state
advance state MoveRight = moveRobot (0, 1) state
advance _ Quit = Over
advance state _ = state

-- |Play a game
-- >>> playGame ['h', 'q'] Playing { robot = (2,2), kitten = (5,5) }
-- [Playing {robot = (2,2), kitten = (5,5)},Playing {robot = (2,1), kitten = (5,5)},Over]
--
-- >>> playGame ['h', 'h', 'h', 'h'] Playing {robot = (2,2), kitten = (2,1)}
-- [Playing {robot = (2,2), kitten = (2,1)},FoundKitten]
playGame :: [Char] -> GameState -> [GameState]
playGame userInput initState = takeThrough (flip elem [Over, FoundKitten]) $
    scanl advance initState $
    parseInput userInput

-- |takeThrough, applied to a predicate @p@ and a list @xs@, returns the
-- prefix (possibly empty) of @xs@ up through the first that satisfies @p@:
-- >>> takeThrough (== 3) [1,2,3,4,5]
-- [1,2,3]
-- >>> takeThrough (== 700) [1,2,3,4,5]
-- [1,2,3,4,5]
takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough _ [] = []
takeThrough p (x:xs)
              | not (p x) = x : takeThrough p xs
              | otherwise = [x]

transitions :: [a] -> [(a, a)]
transitions list = zip ([head list] ++ list) list

-- Here be IO Monad dragons

initScreen (Playing robot kitten) = do
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

updateScreen (_, Over) = do putStrLn "Goodbye!"
updateScreen (_, FoundKitten) = do putStrLn "Aww! You found a kitten!"
updateScreen (oldState, newState) = do
  clear (robot oldState)
  drawR (robot newState)

main :: IO ()
main = do
    let gameState = Playing (12, 40) (13, 17)
    initScreen gameState
    userInput <- getContents
    forM_ (transitions (playGame userInput gameState)) updateScreen
