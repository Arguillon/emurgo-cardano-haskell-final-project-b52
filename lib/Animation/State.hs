module Animation.State where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)
import           System.IO                        (BufferMode (NoBuffering),
                                                   Handle (..), hReady,
                                                   hSetBuffering, hSetEcho,
                                                   stdin)

import           Animation.Env                    (Env (..))
import           Animation.Type                   (Animation, Brick (..),
                                                   GameStatus (..),
                                                   UserInput (..))

data Direction
  = Positive
  | Negative
  | Neutral

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
    { position   :: (Int, Int)
    , direction  :: (Direction, Direction)
    , bXPosition :: Int
    , bricks     :: [Brick]
    , points     :: Int
    , status     :: GameStatus
    }

defaultBrickList :: [Brick]
defaultBrickList =
  [Brick (x, y) 1 | x <- [6, 9], y <- [5 .. 13]] ++
  [Brick (x, y) 1 | x <- [39, 42], y <- [5 .. 13]] ++
  [Brick (x, y) 1 | x <- [6,9 .. 42], y <- [14 .. 15]]

bricksInPlace :: Int -> [Int] -> Int -> [Brick]
bricksInPlace width positions life =
  map (\x -> Brick (findPosition x width 0) life) positions
  where
    findPosition x width level =
      if x < width
        then (x, level)
        else findPosition (x - width) width (level + 1)

defaultSt :: St
defaultSt = St (0, 0) (Neutral, Neutral) 0 defaultBrickList 0 Stopped

getUserInput :: IO (Maybe UserInput)
getUserInput = go Nothing
  where
    go a = do
      hSetBuffering stdin NoBuffering -- ^ This makes it work
      hSetEcho stdin False
      ready <- hReady stdin
      if not ready
        then return Nothing
        else do
          key <- getChar
          if key == 'a' || (key == 'A')
            then return (Just MoveLeft)
            else if key == 'd' || (key == 'D')
                   then return (Just MoveRight)
                   else if key == 'p' || (key == 'P')
                          then return (Just Pause)
                          else if key == 'q' || (key == 'Q')
                                 then return (Just Stop)
                                 else if key == 's' || (key == 'S')
                                        then return (Just Start)
                                        else if key == 'r' ||
                                                (key == 'R') || (key == ' ')
                                               then return (Just Restart)
                                               else return Nothing

next :: Animation Env St ()
next = do
  env <- ask
  input <- lift $ lift getUserInput
  prevSt <- lift get
  lift (put (nextInternal env input prevSt))

nextInternal :: Env -> Maybe UserInput -> St -> St
nextInternal (Env (width, height) velocity baselength bricklength _ _) userInput prevSt@(St (prevX, prevY) (prevXDir, prevYDir) prevBXPos prevBricks prevPoints prevStatus) =
  case prevStatus of
    Paused ->
      case userInput of
        Just Start -> prevSt {status = Playing}
        Just Stop  -> prevSt {status = Stopped}
        _          -> prevSt
    Stopped ->
      case userInput of
        Just Restart -> prevSt {status = Restarted}
        _            -> prevSt
    LevelComplete ->
      case userInput of
        Just Restart -> prevSt {status = Restarted}
        _            -> prevSt
    GameOver ->
      case userInput of
        Just Restart -> prevSt {status = Restarted}
        _            -> prevSt
    Playing ->
      if prevBricks /= []
        then case userInput of
               Just Stop -> prevSt {status = Stopped}
               Just Pause -> prevSt {status = Paused}
               Just MoveLeft ->
                 St
                   { position = (newX, newY)
                   , direction = (newXDir, newYDir)
                   , bXPosition = newBXPosition (-2)
                   , bricks = newBricks
                   , points = newPoints
                   , status = newStatus
                   }
               Just MoveRight ->
                 St
                   { position = (newX, newY)
                   , direction = (newXDir, newYDir)
                   , bXPosition = newBXPosition 2
                   , bricks = newBricks
                   , points = newPoints
                   , status = newStatus
                   }
               _ ->
                 St
                   { position = (newX, newY)
                   , direction = (newXDir, newYDir)
                   , bXPosition = prevBXPos
                   , bricks = newBricks
                   , points = newPoints
                   , status = newStatus
                   }
        else prevSt {status = LevelComplete}
  where
    newXUnbounded = prevX + directionToMultiplier prevXDir * velocity
    newYUnbounded = prevY + directionToMultiplier prevYDir * velocity
    baseCollision = prevBXPos <= newX && ((prevBXPos + baselength) >= newX)
    addPositions (u, v) brl = zip [u .. (u + brl - 1)] $ replicate brl v
    completePositions =
      foldl (\x y -> x ++ addPositions (brickPosition y) bricklength) []
    targetY = (newX, newYUnbounded + directionToMultiplier prevYDir)
    brickCollisionY = elem targetY $ completePositions prevBricks
    targetX = (newXUnbounded + directionToMultiplier prevXDir, newY)
    brickCollisionX = elem targetX $ completePositions prevBricks
    cornerTarget =
      ( newXUnbounded + directionToMultiplier prevXDir
      , newYUnbounded + directionToMultiplier prevYDir)
    cornerCollision =
      not brickCollisionX &&
      not brickCollisionY && elem cornerTarget (completePositions prevBricks)
    targetBlockY = identify targetY prevBricks
    targetBlockX = identify targetX prevBricks
    targetBlockC = identify cornerTarget prevBricks
    identify target =
      head .
      filter
        (\u ->
           snd target == snd (brickPosition u) &&
           (fst target - fst (brickPosition u)) < bricklength &&
           (fst target - fst (brickPosition u)) >= 0)
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
          if newXUnbounded >= width || brickCollisionX || cornerCollision
            then Negative
            else Positive
        Negative ->
          if newXUnbounded <= 0 || brickCollisionX || cornerCollision
            then Positive
            else Negative
    newYDir =
      case prevYDir of
        Neutral -> Neutral
        Positive ->
          if brickCollisionY ||
             cornerCollision ||
             ((newYUnbounded >= (height - 2)) && baseCollision)
            then Negative
            else Positive
        Negative ->
          if newYUnbounded <= 0 || brickCollisionY || cornerCollision
            then Positive
            else Negative
    newBXPosition i =
      let newBxPos = prevBXPos + i
       in if newBxPos + baselength > width
            then prevBXPos
            else if newBxPos <= 0
                   then 0
                   else newBxPos
    newStatus =
      if newY /= height
        then Playing
        else GameOver
    newPoints =
      (+) prevPoints $
      fromEnum $ brickCollisionY || brickCollisionX || cornerCollision
    newBricks -- Case 1: Collision in Y axis AND X axis
      | brickCollisionX && brickCollisionY =
        let brickTail =
              filter
                (\x ->
                   (brickPosition targetBlockY /= brickPosition x) &&
                   (brickPosition targetBlockX /= brickPosition x))
                prevBricks
            brickHurtY =
              Brick (brickPosition targetBlockY) (life targetBlockY - 1)
            brickHurtX =
              Brick (brickPosition targetBlockX) (life targetBlockX - 1)
                      -- Case 1.1: Both blocks in X axis and Y axis have remaining lifes
         in if (life targetBlockY > 0) && (life targetBlockX > 0)
              then brickHurtY : brickHurtX : brickTail
                      -- Case 1.2: Just block in Y axis has remaining lifes
              else if life targetBlockY > 0
                     then brickHurtY : brickTail
                      -- Case 1.3: Just block in X axis has remaining lifes
                     else if life targetBlockX > 0
                            then brickHurtX : brickTail
                      -- Case 1.4: No block has remaining lifes
                            else brickTail
                -- Case 2: Collision in Y axis
      | brickCollisionY = changeBricks targetBlockY
                -- Case 3: Collision in X axis
      | brickCollisionX = changeBricks targetBlockX
                -- Case 4: Collision with a corner
      | cornerCollision = changeBricks targetBlockC
                -- Case 5: No collision
      | otherwise = prevBricks
    changeBricks x =
      let brickTail = filter ((/=) (brickPosition x) . brickPosition) prevBricks
          brickHurt = Brick (brickPosition x) (life x - 1)
       in if life x > 0
            then brickHurt : brickTail
            else brickTail
