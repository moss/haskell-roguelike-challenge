module Update where

import Data.List

import Model

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
-- >>> diagram $ playGame [Kitten 'k' (5,5)] [MoveLeft, Quit] (playing (2,2))
-- "(2,2) -> (2,1) -> Goodbye!"
--
-- >>> diagram $ playGame [Kitten 'k' (2,1)] [MoveLeft, MoveRight] (playing (2,2))
-- "(2,2) -> You found kitten! Way to go, robot!"
--
-- >>> diagram $ playGame [NKI 's' (2,1) "Alf."] [MoveLeft, MoveRight] (playing (2,2))
-- "(2,2) -> Alf. -> (2,3)"
playGame :: Level -> [Command] -> GameState -> [GameState]
playGame level userInput initState = takeThrough over $
    scanl (advance level) initState $
    userInput

