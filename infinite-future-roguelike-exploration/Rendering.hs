module Rendering where

import System.Console.ANSI
import System.IO

import Model

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
