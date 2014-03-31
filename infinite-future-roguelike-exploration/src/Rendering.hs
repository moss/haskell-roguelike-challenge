module Rendering where

import System.Console.ANSI
import System.IO

import Model

initScreen startingState = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    renderWith drawElement startingState

draw char (row, col) = do
    setCursorPosition row col
    putChar char

goToMessage = setCursorPosition 26 0

updateScreen (oldState, newState) = do
    renderWith clearElement oldState
    renderWith drawElement newState

type Renderer = UiElement -> IO ()

renderWith :: Renderer -> GameState -> IO ()
renderWith renderer gameState = mapM_ renderer $ view gameState

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
-- >>> view Playing { robot = (3,4), message = "Hi!", over = False, level = [NKI 'x' (5,6) "a duck", Kitten 'k' (7,8)] }
-- [Sprite 'x' (5,6),Sprite 'k' (7,8),Sprite '#' (3,4),Message "Hi!"]
view :: GameState -> [UiElement]
view Playing { robot = r, message = m, level = l } = (map itemView l) ++ [Sprite '#' r, Message m] 

itemView (Kitten representation position) = Sprite representation position
itemView (NKI representation position _) = Sprite representation position

data UiElement = Sprite Char Position
               | Message String
               deriving (Show, Eq)
