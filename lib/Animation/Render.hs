module Animation.Render where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)

import           Animation.Env                    (Env (..))
import           Animation.State                  (Brick (..), St (..))
import           Animation.Type                   (Animation)
import           Data.Char                        (intToDigit)

render :: Animation Env St ()
render = do
  val <- renderVal
  lift (lift (putStrLn val))

renderVal :: Animation Env St String
renderVal = do
  env <- ask
  st <- lift get
  return (renderInternal env st)

renderInternal :: Env -> St -> String
renderInternal env st = makeBox (size env) (ballPosition st) (bricks st)

makeLine :: Char -> Char -> Int -> Maybe Int -> [Brick] -> String
makeLine endChar innerChar numCols mBrickXPosition bricks =
  let positions = [0 .. numCols]
      renderBrick x =
        case mBrickXPosition of
          Nothing ->
            if x `elem` brickXPositions
              then findBrickLife x
              else innerChar
          Just b ->
            if x == b
              then 'O'
              else if x `elem` brickXPositions
                     then '='
                     else innerChar
   in [endChar] ++ map renderBrick positions ++ [endChar]
  where
    brickXPositions = map (fst . brickPosition) bricks
    findBrickLife z =
      intToDigit $
      head $
      map (life) $ filter (\brick -> fst (brickPosition brick) == z) bricks

makeBox :: (Int, Int) -> (Int, Int) -> [Brick] -> String
makeBox (numRows, numCols) (ballX, ballY) bricks =
  unlines
    ([makeLine '-' '-' numRows Nothing []] ++
     mappedPositions ++ [makeLine '-' '-' numRows Nothing []])
  where
    positions = [0 .. numCols]
    mappedPositions = map lineMaker positions
    lineMaker y =
      let brickPositions =
            filter (\brick -> snd (brickPosition brick) == y) bricks
       in if y == ballY
            then makeLine '|' ' ' numRows (Just ballX) brickPositions
            else makeLine '|' ' ' numRows Nothing brickPositions
