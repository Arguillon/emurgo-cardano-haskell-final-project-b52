module Animation.Render where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get, put)

import           Animation.Env                    (Env (..))
import           Animation.State                  (Brick (..), St (..))
import           Animation.Type                   (Animation)
import           Data.Char                        (intToDigit)

data Object
  = Ball Int
            -- | Brick Int
  | Base Int Int

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
renderInternal env st =
  makeBox
    (size env)
    (baselength env)
    (bXPosition st)
    (position st)
    (bricks st)
    (numberDeaths st)

makeLine ::
     Char -> Char -> Int -> Maybe Object -> Maybe Object -> [Brick] -> String
makeLine endChar innerChar i mb mba bricks =
  let positions = [0 .. i]
      renderPixel x =
        case mb of
          Nothing ->
            case mba of
              Nothing ->
                if x `elem` brickXPositions
                  then findBrickLife x
                  else innerChar
              Just (Base bl ba) ->
                if x `elem` [ba .. (ba + bl)]
                  then ':'
                  else innerChar
          Just (Ball b) ->
            case mba of
              Nothing ->
                if x == b
                  then 'O'
                  else if x `elem` brickXPositions
                         then findBrickLife x
                         else innerChar
              Just (Base bl ba) ->
                if x `elem` [ba .. (ba + bl)]
                  then 'O'
                  else if x `elem` [ba .. (ba + bl)]
                         then ':'
                         else innerChar
   in [endChar] ++ map renderPixel positions ++ [endChar]
  where
    brickXPositions = map (fst . brickPosition) bricks
    findBrickLife z =
      intToDigit $
      head $ map (life) $ filter ((==) z . fst . brickPosition) bricks

makeBox :: (Int, Int) -> Int -> Int -> (Int, Int) -> [Brick] -> Int -> String
makeBox (numRows, numCols) baseL baseX (ballX, ballY) bricks nbDeaths =
  unlines
    ([makeLine '-' '-' numRows Nothing Nothing []] ++
     mappedPositions ++
     [makeLine '-' '-' numRows Nothing Nothing []] ++
     [ "BaseX: " ++
       show (baseX + div baseL 2) ++
       " | Ball: (" ++
       show ballX ++
       "," ++
       show ballY ++
       ") | BallOverBase: " ++
       show (ballX >= baseX && (ballX <= (baseX + baseL))) ++
       " | Number of deaths: " ++ show nbDeaths
     ])
  where
    positions = [0 .. numCols]
    mappedPositions = map lineMaker positions
    lineMaker y =
      let brickPositions = filter ((==) y . snd . brickPosition) bricks
       in if y == ballY
            then makeLine
                   '|'
                   ' '
                   numRows
                   (Just (Ball ballX))
                   Nothing
                   brickPositions
            else if y == numCols - 1
                   then makeLine
                          '|'
                          ' '
                          numRows
                          Nothing
                          (Just (Base baseL baseX))
                          brickPositions
                   else makeLine '|' ' ' numRows Nothing Nothing brickPositions
