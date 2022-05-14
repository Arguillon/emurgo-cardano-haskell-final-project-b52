module Animation.State where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)
import           Data.IORef

import           Animation.Env                    (Env (..))
import           Animation.Type                   (Animation, GameStatus (..),
                                                   UserInput (..))

data Direction
  = Positive
  | Negative
  | Neutral

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
    , bricks       :: [Brick]
    , numberDeaths :: Int
    , status       :: GameStatus
    }

-- TODO MODIFY BRICK GENERATION
defaultBrickList :: [Brick]
defaultBrickList =
  [ Brick (5, 5) 1
  , Brick (6, 5) 2
  , Brick (6, 6) 3
  , Brick (7, 7) 3
  , Brick (9, 8) 2
  , Brick (10, 1) 2
  , Brick (6, 4) 2
  , Brick (11, 15) 2
  , Brick (4, 1) 2
  , Brick (1, 1) 2
  , Brick (1, 2) 2
  , Brick (1, 3) 2
  , Brick (1, 4) 2
  , Brick (1, 5) 2
  ]

defaultSt :: St
defaultSt = St (0, 0) (Neutral, Neutral) 0 defaultBrickList 0 Stopped

pullUserInput :: IORef [UserInput] -> IO (Maybe UserInput)
pullUserInput x = do
  listOfUserInputs <- readIORef x
  if null listOfUserInputs
    then return Nothing
    else do
      let mostAncient = head listOfUserInputs
      writeIORef x (tail listOfUserInputs)
      return (Just mostAncient)

next :: Animation Env St ()
next = do
  env <- ask
  input <- lift $ lift (pullUserInput (userInputReference env))
  prevSt <- lift get
  lift (put (nextInternal env input prevSt))

nextInternal :: Env -> Maybe UserInput -> St -> St
nextInternal (Env (width, height) velocity baselength _) userInput prevSt@(St (prevX, prevY) (prevXDir, prevYDir) prevBXPos prevBricks prevNbDeath prevStatus) =
  case prevStatus of
    Paused ->
      case userInput of
        Just Start -> prevSt {status = Playing}
        Just Stop  -> prevSt {status = Stopped}
        _          -> prevSt
    Stopped ->
      case userInput of
        Just Start -> prevSt {status = Playing}
        _          -> prevSt
    Playing ->
      case userInput of
        Just Stop -> prevSt {status = Stopped}
        Just Pause -> prevSt {status = Paused}
        Just MoveLeft ->
          St
            { position = (newX, newY)
            , direction = (newXDir, newYDir)
            , bXPosition = newBXPosition (-1)
            , bricks = newBricks
            , numberDeaths = newNbDeath
            , status = newStatus
            }
        Just MoveRight ->
          St
            { position = (newX, newY)
            , direction = (newXDir, newYDir)
            , bXPosition = newBXPosition 1
            , bricks = newBricks
            , numberDeaths = newNbDeath
            , status = newStatus
            }
        _ ->
          St
            { position = (newX, newY)
            , direction = (newXDir, newYDir)
            , bXPosition = newBXPosition 0
            , bricks = newBricks
            , numberDeaths = newNbDeath
            , status = newStatus
            }
  where
    newStatus = Playing
    ballInBottom =
      if prevY == height
        then 1
        else 0
    newNbDeath = prevNbDeath + ballInBottom
    newXUnbounded = prevX + directionToMultiplier prevXDir * velocity
    newYUnbounded = prevY + directionToMultiplier prevYDir * velocity
    baseCollision =
      bXPosition prevSt <= newX && ((bXPosition prevSt + baselength) >= newX)
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
    newBXPosition i =
      let newBxPos = prevBXPos + i
       in if newBxPos > width
            then width
            else if newBxPos < 0
                   then 0
                   else newBxPos
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
