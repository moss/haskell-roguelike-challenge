module Rendering where

import System.Console.ANSI
import System.IO

import Model

initScreen level startingState = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    mapM_ drawElement $ (view startingState) ++ (map itemView level)

itemView (Kitten representation position) = Sprite representation position
itemView (NKI representation position _) = Sprite representation position

draw char (row, col) = do
    setCursorPosition row col
    putChar char

goToMessage = setCursorPosition 26 0

updateScreen (oldState, newState) = do
    mapM_ clearElement $ view oldState
    mapM_ drawElement $ view newState

drawElement :: UiElement -> IO ()
drawElement (Sprite rep pos) = draw rep pos
drawElement (Message m) = do
    goToMessage
    putStr m

clearElement :: UiElement -> IO ()
clearElement (Sprite _ pos) = draw ' ' pos
clearElement (Message _) = do
    goToMessage
    clearLine

-- |view breaks up a GameState into UiElements
-- >>> view Playing { robot = (3,4), message = "Hi!", over = False }
-- [Sprite '#' (3,4),Message "Hi!"]
view :: GameState -> [UiElement]
view Playing { robot = r, message = m} = [Sprite '#' r, Message m]

data UiElement = Sprite Char Position
               | Message String
               deriving (Show, Eq)
