module Main where

import           Animation                        (Animation, Direction (..),
                                                   Env (..), St (..),
                                                   defaultEnv, defaultSt,
                                                   directionFromInt, next,
                                                   render, runAnimation)

import           Control.Concurrent               (threadDelay)
import           System.Random                    (randomRIO)

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)

putInitialState :: Animation Env St ()
putInitialState = do
  (Env (width, height) _) <- ask
  posX <- lift $ lift $ randomRIO (0, width)
  posY <- lift $ lift $ randomRIO (0, height)
  dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (0, 2)
  dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (0, 2)
  lift $ put defaultSt

animate :: Animation Env St ()
animate = render
  --next
  --lift $ lift $ threadDelay 100000
  --animate

--  render
mainAnimation :: Animation Env St ()
mainAnimation = do
  putInitialState
  animate

main :: IO ()
main = runAnimation defaultEnv defaultSt mainAnimation
