module Main where

import           Animation                        (Animation, Direction (..),
                                                   Env (..), St (..),
                                                   UserInput (..),
                                                   defaultBrickList, defaultEnv,
                                                   defaultSt, directionFromInt,
                                                   next, render, runAnimation)

import           Control.Concurrent               (threadDelay)
import           Data.IORef
import           System.Random                    (randomRIO)

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)

putInitialState :: Animation Env St ()
putInitialState = do
  (Env (width, height) _ baselength _) <- ask
  posX <- lift $ lift $ randomRIO (0, width)
  posY <- lift $ lift $ randomRIO (0, height)
  dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
  dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
  lift $
    put $
    defaultSt
      { position = (div width 2, div height 2)
      , direction = (Positive, Positive)
      , bXPosition = div (width - baselength) 3
      }

animate :: Animation Env St ()
animate = do
  render
  next
  lift $ lift $ threadDelay 500000
  animate

--  render
mainAnimation :: Animation Env St ()
mainAnimation = do
  putInitialState
  animate

main :: IO ()
main = do
  ref <-
    newIORef
      [Start, MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, Stop]
  runAnimation (defaultEnv {userInputReference = ref}) defaultSt mainAnimation
