module Main where

import           Animation                        (Animation, Direction (..),
                                                   Env (..), St (..),
                                                   defaultBrickList, defaultEnv,
                                                   defaultSt, directionFromInt,
                                                   next, render, runAnimation)

import           Control.Concurrent               (threadDelay)
import           System.Random                    (randomRIO)

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)

putInitialState :: Animation Env St ()
putInitialState = do
  (Env (width, height) _ baselength) <- ask
  posX <- lift $ lift $ randomRIO (0, width)
  posY <- lift $ lift $ randomRIO (0, height)
  dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
  dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
  lift $
    put $
    St
      (div width 2, div height 2)
      (Positive, Positive)
      (div (width - baselength) 3)
      Negative
      defaultBrickList
      0

animate :: Animation Env St ()
animate = do
  render
  next
  lift $ lift $ threadDelay 50000
  animate

--  render
mainAnimation :: Animation Env St ()
mainAnimation = do
  putInitialState
  animate

main :: IO ()
main = runAnimation defaultEnv defaultSt mainAnimation
