module Model where

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
