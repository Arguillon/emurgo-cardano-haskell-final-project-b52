module Animation.Render where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (get)

import           Animation.Env                    (Env (..))
import           Animation.State                  (St (..))
import           Animation.Type                   (Animation, Brick (..),
                                                   GameStatus (..), Object (..))

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
    (bricklength env)
    (bricks st)
    (status st)
    (points st)

makeLine ::
     Char
  -> Char
  -> Int
  -> Maybe Object
  -> Maybe Object
  -> [Brick]
  -> Int
  -> String
makeLine endChar innerChar i mb mba bricks bricklength =
  let positions = [0 .. i]
      renderPixel x =
        case mb of
          Nothing ->
            case mba of
              Nothing -> printBlock x
              Just (Base bl ba) ->
                if x `elem` [ba .. (ba + bl)]
                  then ':'
                  else innerChar
          Just (Ball b) ->
            case mba of
              Nothing ->
                if x == b
                  then 'O'
                  else printBlock x
              Just (Base bl ba) ->
                if x == b
                  then 'O'
                  else if x `elem` [ba .. (ba + bl)]
                         then ':'
                         else innerChar
   in [endChar] ++ map renderPixel positions ++ [endChar]
  where
    brickXPositions = map (fst . brickPosition) bricks
    printBlock x =
      if x `elem`
         foldl (\u v -> u ++ [v .. (v + bricklength - 1)]) [] brickXPositions
        then if life $ pixelOwner x > 0
               then '='
               else '-'
        else innerChar
    pixelOwner x =
      head $
      filter
        (\u ->
           (x - fst (brickPosition u) < bricklength) &&
           (x - fst (brickPosition u) >= 0))
        bricks

makeBox ::
     (Int, Int)
  -> Int
  -> Int
  -> (Int, Int)
  -> Int
  -> [Brick]
  -> GameStatus
  -> Int
  -> String
makeBox (numRows, numCols) baseL baseX (ballX, ballY) bricklength bricks status points =
  unlines
    (["            BRICK BREAKER VIDEOGAME"] ++
     [" "] ++
     case status of
       LevelComplete -> [celebratrionCartoon]
       _ ->
         [makeLine '-' '-' numRows Nothing Nothing [] bricklength] ++
         mappedPositions ++
         [makeLine '-' '-' numRows Nothing Nothing [] bricklength] ++
         [ "Status: " ++
           show status ++
           if ballY /= numCols
             then " | Score: " ++ show points
             else " | ***** GAME OVER ***** | Your Score is " ++ show points
         ] ++ -- Define menu according to status
         [ case status of
             Stopped -> "Press (R) to Restart"
             Paused  -> "Press (S) to Play | Controls: (A) Left / (D) Right"
             Playing -> "(P) Pause / (Q) Stop / (A) Left / (D) Right"
             _       -> ""
         ]
-- Uncomment these lines for debugging purposes
--                            ++ [  "BaseX: " ++ show (baseX + div baseL 2)
--                               ++ " | Ball: (" ++ show ballX ++ "," ++ show ballY ++ ")"
--                               ++ " | BallOverBase: "   ++ show (ballX >= baseX && (ballX <= (baseX + baseL)))
--                               ]
     )
  where
    positions = [0 .. numCols]
    mappedPositions = map lineMaker positions
    lineMaker y =
      let brickYPositions = filter ((==) y . snd . brickPosition) bricks
       in if y == ballY
            then if y == numCols - 1
                   then makeLine
                          '|'
                          ' '
                          numRows
                          (Just (Ball ballX))
                          (Just (Base baseL baseX))
                          brickYPositions
                          bricklength
                   else makeLine
                          '|'
                          ' '
                          numRows
                          (Just (Ball ballX))
                          Nothing
                          brickYPositions
                          bricklength
            else if y == numCols - 1
                   then makeLine
                          '|'
                          ' '
                          numRows
                          Nothing
                          (Just (Base baseL baseX))
                          brickYPositions
                          bricklength
                   else makeLine
                          '|'
                          ' '
                          numRows
                          Nothing
                          Nothing
                          brickYPositions
                          bricklength
    celebratrionCartoon =
      "                        .-." ++
      "\n                _.--¨¨¨¨.o/         .-.-._" ++
      "\n             __'   .¨¨¨; {        _J ,__  `.       Level Complete" ++
      "\n            ; o`.-.`._.'J;       ; /  `- /  ;" ++
      "\n            `--i`¨. `¨ .';       `._ __.'   |     ¡CONGRATULATIONS!" ++
      "\n                `  `¨¨¨   `         `;      :" ++
      "\n                 `.¨-.     ;     ____/     /     Your Score: " ++
      show points ++
      " points" ++
      "\n                   `-.`     `-.-'    `¨-..'" ++
      "\n     ___              `;__.-'¨           `." ++
      "\n  .-{_  `--._         /.-¨                 `-." ++
      "\n /    ¨¨T    ¨¨---...'  _.-¨¨   ¨¨¨-.         `." ++
      "\n;       /                 __.-¨¨.    `.         `,             _.." ++
      "\n `     /            __.-¨¨       '.    `          `.,__      .'L' }" ++
      "\n  `---¨`-.__    __.¨    .-.       j     `.         :   `.  .' ,' /" ++
      "\n            ¨¨¨¨       /   `     :        `.       |     F' `   ;" ++
      "\n                      ;     `-._,L_,-¨¨-.   `-,    ;     `   ; /" ++
      "\n                       `.       |        `-._  `.__/_        `/" ++
      "\n                         `     _;            `  _.'  `-.     /" ++
      "\n                          `---¨ `.___,,      ;¨¨        `  .'" ++
      "\n                                    _/       ;           `¨" ++
      "\n      Bring me                   .-¨     _,-' " ++
      "\n     more bricks!               {       ¨¨;            Next Level - Press SPACE" ++
      "\n                                 ;-.____.'`." ++
      "\n      I am not done yet!          `.  ` '.  :" ++
      "\n                                    `  : : /" ++
      "\n                                     `':/ `"
