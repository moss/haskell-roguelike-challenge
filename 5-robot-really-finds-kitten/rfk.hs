module RobotFindsKitten where
import Control.Monad
import Data.List
import System.Console.ANSI
import System.IO

type Position = (Int, Int)

data GameState = Playing { robot :: Position, items :: [Item] }
               | FoundKitten
               | FoundNKI
               | Over deriving (Eq)

instance Show GameState where
    show (Playing robot items) = "Playing{"
                               ++ "#@"
                               ++ (show robot)
                               ++ ";"
                               ++ (intercalate ", " (map show items))
                               ++ "}"
    show FoundKitten = "FoundKitten"
    show FoundNKI = "FoundNKI"
    show Over = "Over"

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

instance Show Item where
    show (Kitten representation position) = "Kitten "
                                          ++ [representation]
                                          ++ "@"
                                          ++ (show position)
    show (NKI representation position) = "NKI "
                                          ++ [representation]
                                          ++ "@"
                                          ++ (show position)

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

moveRobot :: (Int, Int) -> GameState -> [GameState]
moveRobot (rowDelta, colDelta) Playing { robot = (row, col), items = is } =
    let newR = (row + rowDelta, col + colDelta) in
    let itemInTheWay = itemAt newR in
    case itemAt newR is of
      Just (Kitten _ _) -> [FoundKitten]
      Just (NKI _ _) -> [FoundNKI, Playing (row, col) is]
      Nothing -> [Playing { robot = newR, items = is }]

positions :: [Item] -> [Position]
positions = map position

advance :: GameState -> Command -> [GameState]
advance state MoveLeft = moveRobot (0, -1) state
advance state MoveUp = moveRobot (-1, 0) state
advance state MoveDown = moveRobot (1, 0) state
advance state MoveRight = moveRobot (0, 1) state
advance _ Quit = [Over]
advance state _ = [state]

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


-- |Play a game
-- >>> playGame ['h', 'q'] (Playing (2,2) [Kitten 'k' (5,5)])
-- [Playing{#@(2,2);Kitten k@(5,5)},Playing{#@(2,1);Kitten k@(5,5)},Over]
--
-- >>> playGame ['h', 'l'] (Playing (2,2) [Kitten 'k' (2,1)])
-- [Playing{#@(2,2);Kitten k@(2,1)},FoundKitten]
--
-- >>> playGame ['h', 'l'] (Playing (2,2) [NKI 's' (2,1)])
-- [Playing{#@(2,2);NKI s@(2,1)},FoundNKI,Playing{#@(2,2);NKI s@(2,1)},Playing{#@(2,3);NKI s@(2,1)}]
playGame :: [Char] -> GameState -> [GameState]
playGame userInput initState = takeThrough (flip elem [Over, FoundKitten]) $
    chunkyscanl advance initState $
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

initScreen (Playing robot items) = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    drawR robot
    mapM_ drawItem items

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

updateScreen (_, Over) = do putStrLn "Goodbye!"
updateScreen (_, FoundKitten) = do putStrLn "Aww! You found a kitten!"
updateScreen (_, FoundNKI) = do putStr "Just a useless gray rock."
updateScreen (FoundNKI, _) = do return ()
updateScreen (oldState, newState) = do
  clear (robot oldState)
  drawR (robot newState)

main :: IO ()
main = do
    let gameState = Playing (12, 40) [ Kitten 'k' (13, 17)
                                     , NKI 's' (15, 20)
                                     ]
    initScreen gameState
    userInput <- getContents
    forM_ (transitions (playGame userInput gameState)) updateScreen
