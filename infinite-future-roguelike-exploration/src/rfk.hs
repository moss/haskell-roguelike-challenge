import Control.Monad
import Data.Array.IO
import Data.List
import System.Console.ANSI
import System.IO
import System.Random

type Position = (Int, Int)

data GameState = Playing { robot :: Position, message :: String, over :: Bool }
               deriving (Eq)

data Command = MoveLeft
             | MoveDown
             | MoveUp
             | MoveRight
             | Quit
             | Unknown
             deriving (Eq)

data Item = Kitten { representation :: Char, position :: Position }
          | NKI { representation :: Char, position :: Position, description :: String }
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

moveRobot :: Level -> (Int, Int) -> GameState -> GameState
moveRobot level (rowDelta, colDelta) curState =
    let (row, col) = robot curState in
    let newR = (row + rowDelta, col + colDelta) in
    let itemInTheWay = itemAt newR in
    case itemAt newR level of
      Just (Kitten _ _) -> curState { message = "You found kitten! Way to go, robot!", over = True }
      Just (NKI _ _ description) -> curState { message = description }
      Nothing -> curState { robot = newR, message = "" }

positions :: [Item] -> [Position]
positions = map position

-- TODO duplication!
advance :: Level -> GameState -> Command -> GameState
advance level state MoveLeft = moveRobot level (0, -1) state
advance level state MoveUp = moveRobot level (-1, 0) state
advance level state MoveDown = moveRobot level (1, 0) state
advance level state MoveRight = moveRobot level (0, 1) state
advance _ state Quit = state { message = "Goodbye!", over = True }
advance _ state _ = state

-- TODO this is also a bit of a hack
-- |Show a simple representation of a series of gameplay states
diagram :: [GameState] -> String
diagram = intercalate " -> " . map (\ state -> case state of
                          Playing { robot = robot, message = "" } -> show robot
                          Playing { message = message } -> message
                          )

playing robotPosition = Playing { robot = robotPosition, message = "", over = False }

-- |Play a game
-- >>> diagram $ playGame [Kitten 'k' (5,5)] ['h', 'q'] (playing (2,2))
-- "(2,2) -> (2,1) -> Goodbye!"
--
-- >>> diagram $ playGame [Kitten 'k' (2,1)] ['h', 'l'] (playing (2,2))
-- "(2,2) -> You found kitten! Way to go, robot!"
--
-- >>> diagram $ playGame [NKI 's' (2,1) "Alf."] ['h', 'l'] (playing (2,2))
-- "(2,2) -> Alf. -> (2,3)"
playGame :: Level -> [Char] -> GameState -> [GameState]
playGame level userInput initState = takeThrough over $
    scanl (advance level) initState $
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

initScreen level Playing {robot = robot} = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    drawR robot
    mapM_ drawItem level

drawItem (Kitten representation position) = draw representation position
drawItem (NKI representation position message) = draw representation position

draw char (row, col) = do
    setCursorPosition row col
    putChar char

drawR = draw '#'
clear = draw ' '

clearState Playing { robot = robotPosition } = do
    clear robotPosition
    setCursorPosition 26 0
    clearLine

drawState Playing { robot = robotPosition, message = message } = do
    drawR robotPosition
    setCursorPosition 26 0
    putStr message

updateScreen (oldState, newState) = do
  clearState oldState
  drawState newState

takeRandom count range = do
    g <- newStdGen
    return $ take count $ randomRs range g

takeRandomPositions count = do
    randomRows <- takeRandom count (0, 25)
    randomCols <- takeRandom count (0, 80)
    return $ zip randomRows randomCols

-- | Randomly shuffle a list
--   /O(N)/
-- Cribbed from http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

takeRandomDescriptions count = do
    shuffled <- shuffle nonKittenItemDescriptions
    return $ take count shuffled

takeRandomItems count = do
    representations <- takeRandom count ('A', 'z')
    positions <- takeRandomPositions count
    descriptions <- takeRandomDescriptions count
    let makeNKI (r,p,d) = NKI r p d
    return $ map makeNKI $ zip3 representations positions descriptions

generateLevel = do
    [kittenChar] <- takeRandom 1 ('A', 'z')
    [kittenPos] <- takeRandomPositions 1
    [randomItemCount] <- takeRandom 1 (3, 15)
    nonKittenItems <- takeRandomItems randomItemCount
    return $ (Kitten kittenChar kittenPos):nonKittenItems

main :: IO ()
main = do
    level <- generateLevel
    [robotPos] <- takeRandomPositions 1
    let gameState = Playing { robot = robotPos, message = "", over = False }
    initScreen level gameState
    userInput <- getContents
    forM_ (transitions $ playGame level userInput gameState) updateScreen
    putStrLn ""

nonKittenItemDescriptions =
    [ "Just a useless gray rock."
    , "A giant statue of a kitten."
    , "Five pounds of flax."
    , "I pity the fool who mistakes me for kitten!\", sez Mr. T."
    , "That's just an old tin can."
    , "It's an altar to the horse god."
    , "A box of dancing mechanical pencils. They dance! They sing!"
    , "It's an old Duke Ellington record."
    , "A box of fumigation pellets."
    , "A digital clock. It's stuck at 2:17 PM."
    , "That's just a charred human corpse."
    , "I don't know what that is, but it's not kitten."
    , "An empty shopping bag. Paper or plastic?"
    , "Could it be... a big ugly bowling trophy?"
    , "A coat hanger hovers in thin air. Odd."
    , "Not kitten, just a packet of Kool-Aid(tm)."
    , "A freshly-baked pumpkin pie."
    , "A lone, forgotten comma, sits here, sobbing."
    , "ONE HUNDRED THOUSAND CARPET FIBERS!!!!!"
    , "It's Richard Nixon's nose!"
    , "It's Lucy Ricardo. \"Aaaah, Ricky!\", she says."
    , "You stumble upon Bill Gates' stand-up act."
    , "Just an autographed copy of the Kama Sutra."
    , "It's the Will Rogers Highway. Who was Will Rogers, anyway?"
    , "It's another robot, more advanced in design than you but strangely immobile."
    , "Leonard Richardson is here, asking people to lick him."
    , "It's a stupid mask, fashioned after a beagle."
    , "Your State Farm Insurance(tm) representative!"
    , "It's the local draft board."
    , "Seven 1/4\" screws and a piece of plastic."
    , "An 80286 machine."
    , "One of those stupid \"Homes of the Stars\" maps."
    , "A signpost saying \"TO KITTEN\". It points in no particular direction."
    ]
