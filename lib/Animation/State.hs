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
    { position     :: (Int, Int)
    , direction    :: (Direction, Direction)
    , bXPosition   :: Int
    , baseMovement :: Direction
    , bricks       :: [Brick]
    , numberDeaths :: Int
    }

defaultBrickList :: [Brick]
defaultBrickList = [Brick (5, 5) 1, Brick (6, 5) 2, Brick (6, 6) 3]

defaultSt :: St
defaultSt = St (0, 0) (Neutral, Neutral) 0 Neutral defaultBrickList 0

next :: Animation Env St ()
next = do
  env <- ask
  prevSt <- lift get
  lift (put (nextInternal env prevSt))

nextInternal :: Env -> St -> St
nextInternal (Env (width, height) velocity baselength) (St (prevX, prevY) (prevXDir, prevYDir) prevBXPos prevMov prevBricks prevNbDeath) =
  St
    { position = (newX, newY)
    , direction = (newXDir, newYDir)
    , bXPosition = newBXPos
    , baseMovement = newMov
    , bricks = newBricks
    , numberDeaths = newNbDeath
    }
  where
    ballInBottom =
      if prevY == height
        then 1
        else 0
    newNbDeath = prevNbDeath + ballInBottom
    newXUnbounded = prevX + directionToMultiplier prevXDir * velocity
    newYUnbounded = prevY + directionToMultiplier prevYDir * velocity
    baseCollision = newBXPos <= newX && ((newBXPos + baselength) >= newX)
    brickCollisionY =
      elem (newX, newYUnbounded + directionToMultiplier prevYDir) $
      map brickPosition prevBricks
    brickCollisionX =
      elem (newXUnbounded + directionToMultiplier prevXDir, newY) $
      map brickPosition prevBricks
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
          if newXUnbounded >= width || brickCollisionX
            then Negative
            else Positive
        Negative ->
          if newXUnbounded <= 0 || brickCollisionX
            then Positive
            else Negative
    newYDir =
      case prevYDir of
        Neutral -> Neutral
        Positive ->
          if baseCollision
            then if newYUnbounded >= (height - 2)
                   then Negative
                   else Positive
            else if (newYUnbounded >= height) || brickCollisionY
                   then Negative
                   else Positive
        Negative ->
          if newYUnbounded <= 0 || brickCollisionY
            then Positive
            else Negative
    newBXPos =
      case prevMov of
        Neutral  -> prevBXPos
        Positive -> prevBXPos + 1
        Negative -> prevBXPos - 1
    newMov =
      case prevMov of
        Neutral -> Neutral
        Positive ->
          if (prevBXPos + baselength) < (width - 1)
            then Positive
            else Negative
        Negative ->
          if prevBXPos > 1
            then Negative
            else Positive
    newBricks
      | brickCollisionY =
        let filterCond =
              (==) (newX, newYUnbounded + directionToMultiplier prevYDir) .
              brickPosition
            target = head $ filter filterCond prevBricks
            brickTail =
              filter ((/=) (brickPosition target) . brickPosition) prevBricks
            brickHurt = Brick (brickPosition target) (life target - 1)
         in if life target > 0
              then brickHurt : brickTail
              else brickTail
      | brickCollisionX =
        let filterCond =
              (==) (newXUnbounded + directionToMultiplier prevXDir, newY) .
              brickPosition
            target = head $ filter filterCond prevBricks
            brickTail =
              filter ((/=) (brickPosition target) . brickPosition) prevBricks
            brickHurt = Brick (brickPosition target) (life target - 1)
         in if life target > 0
              then brickHurt : brickTail
              else brickTail
      | otherwise = prevBricks
