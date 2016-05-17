{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( app
    ) where

import SDL
import Linear
import Linear.Affine
import Control.Monad (unless)
import Data.Word
import Control.Concurrent
import System.Random
import System.Clock
import World as World
    
windowWidth = World.width
windowHeight = World.height
    
app :: IO ()
app = appInit

appInit :: IO ()
appInit = do
    initializeAll
    window <- createWindow "Herbivores and Carnivores" defaultWindow
           { windowInitialSize = V2 windowWidth windowHeight }
    renderer <- createRenderer window (-1) defaultRenderer
    world <- World.initWorldRandom
    appLoop renderer world

appLoop :: Renderer -> World -> IO ()
appLoop r w = do
        events <- pollEvents
        let eventIsKeyPress key event =
              case eventPayload event of
                KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == key
                _ -> False
            keyPressed key = not (null (filter (eventIsKeyPress key) events))
            qPressed = keyPressed KeycodeQ
        draw r w
        w' <- World.updateWorld w
        threadDelay 16500
        unless qPressed $ appLoop r w'

draw :: Renderer -> World -> IO ()
draw renderer world = do
        rendererDrawColor renderer $= V4 0 0 0 255
        clear renderer
        drawWorld renderer world
        present renderer

drawWorld :: Renderer -> World -> IO ()
drawWorld renderer (World grass) = do
    drawGrass renderer grass

drawGrass :: Renderer -> [Grass] -> IO ()
drawGrass renderer grass = do
    mapM_ (drawBladeOfGrass renderer) grass

drawBladeOfGrass :: Renderer -> Grass -> IO ()
drawBladeOfGrass renderer (Grass point height) = do
    rendererDrawColor renderer $= V4 0 height 0 255
    drawPoint renderer point
