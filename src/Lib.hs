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
    
windowWidth = 500
windowHeight = 500
    
app :: IO ()
app = appInit

appInit :: IO ()
appInit = do
    initializeAll
    window <- createWindow "Herbivores and Carnivores" defaultWindow
           { windowInitialSize = V2 windowWidth windowHeight }
    renderer <- createRenderer window (-1) defaultRenderer
    gen <- getStdGen
    -- world <- World.worldInit
    appLoop renderer gen -- world

appLoop :: Renderer -> StdGen -> IO ()
appLoop renderer gen = do
        (a,g) <- return $ randomR (0 :: Int, 10 :: Int) gen
        events <- pollEvents
        let eventIsKeyPress key event =
              case eventPayload event of
                KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == key
                _ -> False
            keyPressed key = not (null (filter (eventIsKeyPress key) events))
            qPressed = keyPressed KeycodeQ
        rendererDrawColor renderer $= V4 0 0 0 255
        clear renderer
        present renderer
        threadDelay 16500
        unless qPressed $ appLoop renderer g
