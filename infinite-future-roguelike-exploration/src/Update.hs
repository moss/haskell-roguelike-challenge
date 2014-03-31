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

moveRobot :: (Int, Int) -> GameState -> GameState
moveRobot (rowDelta, colDelta) curState =
    let (row, col) = robot curState in
    let newR = (row + rowDelta, col + colDelta) in
    let itemInTheWay = itemAt newR (level curState) in
    case itemInTheWay of
      Just (Kitten _ _) -> curState { message = "You found kitten! Way to go, robot!", over = True }
      Just (NKI _ _ description) -> curState { message = description }
      Nothing -> curState { robot = newR, message = "" }

positions :: [Item] -> [Position]
positions = map position

-- TODO duplication!
advance :: GameState -> Command -> GameState
advance state MoveLeft = moveRobot (0, -1) state
advance state MoveUp = moveRobot (-1, 0) state
advance state MoveDown = moveRobot (1, 0) state
advance state MoveRight = moveRobot (0, 1) state
advance state Quit = state { message = "Goodbye!", over = True }
advance state _ = state

-- TODO this is also a bit of a hack
-- |Show a simple representation of a series of gameplay states
diagram :: [GameState] -> String
diagram = intercalate " -> " . map (\ state -> case state of
                          Playing { robot = robot, message = "" } -> show robot
                          Playing { message = message } -> message
                          )

playing robotPosition level = Playing { robot = robotPosition, message = "", over = False, level = level }

-- |Play a game
-- >>> diagram $ playGame [MoveLeft, Quit] (playing (2,2) [Kitten 'k' (5,5)])
-- "(2,2) -> (2,1) -> Goodbye!"
--
-- >>> diagram $ playGame [MoveLeft, MoveRight] (playing (2,2) [Kitten 'k' (2,1)])
-- "(2,2) -> You found kitten! Way to go, robot!"
--
-- >>> diagram $ playGame [MoveLeft, MoveRight] (playing (2,2) [NKI 's' (2,1) "Alf."])
-- "(2,2) -> Alf. -> (2,3)"
playGame :: [Command] -> GameState -> [GameState]
playGame userInput initState = takeThrough over $
    scanl advance initState $
    userInput

