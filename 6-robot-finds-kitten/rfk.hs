module RobotFindsKitten where
import Control.Monad
import Data.List
import System.Console.ANSI
import System.IO
import System.Random

type Position = (Int, Int)

data GameState = Playing { robot :: Position }
               | FoundKitten
               | FoundNKI
               | Over deriving (Eq)

instance Show GameState where
    show (Playing robot) = "Playing " ++ (show robot)
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

type Level = [Item]

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

moveRobot :: Level -> (Int, Int) -> GameState -> [GameState]
moveRobot level (rowDelta, colDelta) Playing { robot = (row, col) } =
    let newR = (row + rowDelta, col + colDelta) in
    let itemInTheWay = itemAt newR in
    case itemAt newR level of
      Just (Kitten _ _) -> [FoundKitten]
      Just (NKI _ _) -> [FoundNKI, Playing (row, col)]
      Nothing -> [Playing { robot = newR }]

positions :: [Item] -> [Position]
positions = map position

advance :: Level -> GameState -> Command -> [GameState]
advance level state MoveLeft = moveRobot level (0, -1) state
advance level state MoveUp = moveRobot level (-1, 0) state
advance level state MoveDown = moveRobot level (1, 0) state
advance level state MoveRight = moveRobot level (0, 1) state
advance _ _ Quit = [Over]
advance _ state _ = [state]

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

-- |Show a simple representation of a series of gameplay states
diagram :: [GameState] -> String
diagram = intercalate " -> " . map (\ state -> case state of
                                                Playing robot -> show robot
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

updateScreen (_, Over) = do putStrLn "Goodbye!"
updateScreen (_, FoundKitten) = do putStrLn "Aww! You found a kitten!"
updateScreen (_, FoundNKI) = do putStr "Just a useless gray rock."
updateScreen (FoundNKI, _) = do return ()
updateScreen (oldState, newState) = do
  clear (robot oldState)
  drawR (robot newState)

takeRandom count range = do
    g <- newStdGen
    return $ take count $ randomRs range g

main :: IO ()
main = do
    [kittenChar, stoneChar] <- takeRandom 2 ('A', 'z')
    [kittenRow] <- takeRandom 1 (0, 25)
    [kittenCol] <- takeRandom 1 (0, 80)
    let level = [Kitten kittenChar (kittenRow, kittenCol), NKI stoneChar (15, 20)]
    let gameState = Playing (12, 40)
    initScreen level gameState
    userInput <- getContents
    forM_ (transitions (playGame level userInput gameState)) updateScreen
