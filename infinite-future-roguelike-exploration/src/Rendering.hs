module Rendering where

import System.Console.ANSI
import System.IO

import Model

initScreen level Playing {robot = robot} = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    drawRobot robot
    mapM_ drawItem level

drawItem (Kitten representation position) = draw representation position
drawItem (NKI representation position message) = draw representation position

draw char (row, col) = do
    setCursorPosition row col
    putChar char

drawRobot = draw '#'
clearRobot = draw ' '

goToMessage = setCursorPosition 26 0
clearMessage = do goToMessage; clearLine
drawMessage m = do goToMessage; putStr m

clearState Playing { robot = robotPosition } = do
    clearRobot robotPosition
    clearMessage

drawState Playing { robot = robotPosition, message = message } = do
    drawRobot robotPosition
    drawMessage message

updateScreen (oldState, newState) = do
  clearState oldState
  drawState newState
