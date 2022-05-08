module Animation.Render where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)

import           Animation.Env                    (Env (..))
import           Animation.State                  (Brick (..), St (..))
import           Animation.Type                   (Animation)

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

makeLine :: Char -> Char -> Int -> Maybe Int -> [Int] -> String
makeLine endChar innerChar i mb brickXPositions =
  let positions = [0 .. i]
      renderBrick x =
        case mb of
          Nothing ->
            if x `elem` brickXPositions
              then '='
              else innerChar
          Just b ->
            if x == b
              then 'O'
              else if x `elem` brickXPositions
                     then '='
                     else innerChar
   in [endChar] ++ map renderBrick positions ++ [endChar]

makeBox :: (Int, Int) -> (Int, Int) -> [Brick] -> String
makeBox (c, r) (x, y) bricks =
  unlines
    ([makeLine '-' '-' c Nothing []] ++
     mappedPositions ++ [makeLine '-' '-' c Nothing []])
  where
    positions = [0 .. r]
    mappedPositions = map lineMaker positions
    lineMaker x =
      let brickPositions =
            map (fst . brickPosition) $
            filter (\brick -> snd (brickPosition brick) == x) bricks
       in if x == y
            then makeLine '|' ' ' c (Just x) brickPositions
            else makeLine '|' ' ' c Nothing brickPositions
