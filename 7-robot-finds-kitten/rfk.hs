module RobotFindsKitten where
import Control.Monad
import Data.List
import System.Console.ANSI
import System.IO
import System.Random

type Position = (Int, Int)

data GameState = Playing { robot :: Position }
               -- TODO these things shouldn't all be one single GameState
               | FoundKitten
               | FoundNKI
               | Over deriving (Eq)

data Command = MoveLeft
             | MoveDown
             | MoveUp
             | MoveRight
             | Quit
             | Unknown
             deriving (Eq)

data Item = Kitten { representation :: Char, position :: Position }
          | NKI { representation :: Char, position :: Position }
          deriving (Eq)

type Level = [Item]

parseInput :: [Char] -> [Command]
parseInput chars = map parseCommand chars

parseCommand :: Char -> Command
parseCommand 'q' = Quit
parseCommand 'h' = MoveLeft
parseCommand 'j' = MoveDown
parseCommand 'k' = MoveUp
parseCommand 'l' = MoveRight
parseCommand _ = Unknown

itemAt :: Position -> [Item] -> Maybe Item
itemAt pos = find (\ item -> (position item) == pos)

moveRobot :: Level -> (Int, Int) -> GameState -> [GameState]
moveRobot level (rowDelta, colDelta) Playing { robot = (row, col) } =
    let newR = (row + rowDelta, col + colDelta) in
    let itemInTheWay = itemAt newR in
    case itemAt newR level of
      Just (Kitten _ _) -> [FoundKitten]
      Just (NKI _ _) -> [FoundNKI, Playing (row, col)]
      Nothing -> [Playing newR]

positions :: [Item] -> [Position]
positions = map position

-- TODO duplication!
advance :: Level -> GameState -> Command -> [GameState]
advance level state MoveLeft = moveRobot level (0, -1) state
advance level state MoveUp = moveRobot level (-1, 0) state
advance level state MoveDown = moveRobot level (1, 0) state
advance level state MoveRight = moveRobot level (0, 1) state
advance _ _ Quit = [Over]
advance _ state _ = [state]

-- TODO this is a bit of a hack
-- |like scanl, but one trip through the function can produce multiple
-- >>> chunkyscanl (\ latest new -> [latest + new]) 0 [1,2,3]
-- [0,1,3,6]
-- >>> chunkyscanl (\ latest new -> [9, latest + new]) 0 [1,2,3]
-- [0,9,1,9,3,9,6]
chunkyscanl :: (a -> b -> [a]) -> a -> [b] -> [a]
chunkyscanl f q ls =  q : (case ls of
                     []   -> []
                     x:xs -> let nextchunk = f q x in
                             (init nextchunk) ++ chunkyscanl f (last nextchunk) xs)

-- TODO this is also a bit of a hack
-- |Show a simple representation of a series of gameplay states
diagram :: [GameState] -> String
diagram = intercalate " -> " . map (\ state -> case state of
                                                Playing { robot = robot } -> show robot
                                                FoundKitten -> "Kitten"
                                                FoundNKI -> "NKI"
                                                Over -> "Over"
                                                )

-- |Play a game
-- >>> diagram $ playGame [Kitten 'k' (5,5)] ['h', 'q'] (Playing (2,2))
-- "(2,2) -> (2,1) -> Over"
--
-- >>> diagram $ playGame [Kitten 'k' (2,1)] ['h', 'l'] (Playing (2,2))
-- "(2,2) -> Kitten"
--
-- >>> diagram $ playGame [NKI 's' (2,1)] ['h', 'l'] (Playing (2,2))
-- "(2,2) -> NKI -> (2,2) -> (2,3)"
playGame :: Level -> [Char] -> GameState -> [GameState]
playGame level userInput initState = takeThrough (flip elem [Over, FoundKitten]) $
    chunkyscanl (advance level) initState $
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

-- TODO this is, frankly, also a bit of a hack
transitions :: [a] -> [(a, a)]
transitions list = zip ([head list] ++ list) list

-- Here be IO Monad dragons

initScreen level (Playing robot) = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    drawR robot
    mapM_ drawItem level

drawItem (Kitten representation position) = draw representation position
drawItem (NKI representation position) = draw representation position

draw char (row, col) = do
    setCursorPosition row col
    putChar char
    setCursorPosition 26 0

drawR = draw '#'
drawK = draw 'k'
drawS = draw 's'
clear = draw ' '

clearState Playing { robot = robotPosition } = do clear robotPosition
clearState _ = do return ()

drawState Playing { robot = robotPosition } = do drawR robotPosition
drawState Over = do putStrLn "Goodbye!"
drawState FoundKitten = do putStrLn "Aww! You found a kitten!"
drawState FoundNKI = do putStr "Just a useless gray rock."

updateScreen (oldState, newState) = do
  clearState oldState
  drawState newState

takeRandom count range = do
    g <- newStdGen
    return $ take count $ randomRs range g

generateLevel = do
    [kittenChar, stoneChar, otherStoneChar] <- takeRandom 3 ('A', 'z')
    randomRows <- takeRandom 2 (0, 25)
    randomCols <- takeRandom 2 (0, 80)
    let [kittenPos, stonePos] = zip randomRows randomCols
    return [ Kitten kittenChar kittenPos
           , NKI stoneChar stonePos
           , NKI otherStoneChar (6, 42)
           ]

main :: IO ()
main = do
    level <- generateLevel
    let gameState = Playing (12, 40)
    initScreen level gameState
    userInput <- getContents
    forM_ (transitions $ playGame level userInput gameState) updateScreen
