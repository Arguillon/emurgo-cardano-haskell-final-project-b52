module Main where

import           Animation                        (Animation, Direction (..),
                                                   Env (..), St (..),
                                                   bricksInPlace,
                                                   defaultBrickList, defaultEnv,
                                                   defaultSt, directionFromInt,
                                                   next, render, runAnimation)

import           Control.Concurrent               (threadDelay)
import           System.Random                    (randomRIO)

import           Animation.Type                   (GameStatus (..))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)
import           Data.IORef

-- Function tu put the initial state
-- Takes the default animation, environment and state
-- It is here that you can define the number of bricks
putInitialState :: Animation Env St ()
putInitialState = do
  (Env (width, height) _ baselength bricklength _ _) <- ask
  posX <- lift $ lift $ randomRIO (div width 3, (*) 2 $ div width 3)
  posY <- lift $ lift $ randomRIO (div height 3, (*) 2 $ div height 3)
  dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
  dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
  randNumBlocks <-
    let maxDesiredBlocks = 4
     in lift $ lift $ randomRIO (0, maxDesiredBlocks)
  randDistBlocks <-
    sequence $
    replicate randNumBlocks $ randomRIO (1, (width * (height - 4)) :: Int)
  lift $
    put $
    St
      (posX, posY)
      (dirX, dirY)
      (div (width - baselength) 2)
      (bricksInPlace width randDistBlocks 1)
      0
      Paused

animate :: Animation Env St ()
animate = do
  render
  event <- lift get
  case status event of
    Restarted -> putInitialState
    _         -> next
  lift $ lift $ threadDelay 200000
  animate

mainAnimation :: Animation Env St ()
mainAnimation = do
  putInitialState
  animate

main :: IO ()
main = do
  runAnimation defaultEnv defaultSt mainAnimation
