module Animation.State where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)

import           Animation.Env                    (Env (..))
import           Animation.Type                   (Animation)

data Direction
  = Positive
  | Negative
  | Neutral

data UserInput
  = Left
  | Right

data Brick =
  Brick
    { brickPosition :: (Int, Int)
    , life          :: Int
    }

directionFromInt :: Int -> Direction
directionFromInt 0 = Neutral
directionFromInt 1 = Positive
directionFromInt 2 = Negative
directionFromInt _ = error "Boooooo....."

directionToMultiplier :: Direction -> Int
directionToMultiplier Positive = 1
directionToMultiplier Negative = -1
directionToMultiplier Neutral  = 0

data St =
  St
    { ballPosition  :: (Int, Int)
    , ballDirection :: (Direction, Direction)
    , bricks        :: [Brick]
    }

-- TODO add random brick generation
defaultBrickList :: [Brick]
defaultBrickList = [Brick (5, 5) 1, Brick (6, 5) 2, Brick (6, 6) 3]

defaultSt :: St
defaultSt = St (0, 0) (Neutral, Neutral) defaultBrickList

next :: Animation Env St ()
next = do
  env <- ask
  prevSt <- lift get
  lift (put (nextInternal env prevSt))

nextInternal :: Env -> St -> St
nextInternal (Env (width, height) velocity) (St (prevX, prevY) (prevXDir, prevYDir) _) =
  St
    { ballPosition = (newX, newY)
    , ballDirection = (newXDir, newYDir)
    , bricks = defaultBricks
    }
  where
    defaultBricks = [Brick (5, 5) 1]
    newXUnbounded = prevX + directionToMultiplier prevXDir * velocity
    newYUnbounded = prevY + directionToMultiplier prevYDir * velocity
    newX =
      case prevXDir of
        Neutral  -> newXUnbounded
        Positive -> min newXUnbounded width
        Negative -> max newXUnbounded 0
    newY =
      case prevYDir of
        Neutral  -> newYUnbounded
        Positive -> min newYUnbounded height
        Negative -> max newYUnbounded 0
    newXDir =
      case prevXDir of
        Neutral -> Neutral
        Positive ->
          if newXUnbounded > width
            then Negative
            else Positive
        Negative ->
          if newXUnbounded < 0
            then Positive
            else Negative
    newYDir =
      case prevYDir of
        Neutral -> Neutral
        Positive ->
          if newYUnbounded > height
            then Negative
            else Positive
        Negative ->
          if newYUnbounded < 0
            then Positive
            else Negative
