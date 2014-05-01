module Rendering where

import System.Console.ANSI
import System.IO
import Model

class GameView a where
    view :: a -> [UiElement]

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

renderWith :: GameView a => Renderer -> a -> IO ()
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

data UiElement = Sprite Char Position
               | Message String
               deriving (Show, Eq)
