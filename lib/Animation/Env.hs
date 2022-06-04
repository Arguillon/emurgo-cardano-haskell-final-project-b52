module Animation.Env where

-- import Data.IORef
import           Animation.Type (UserInput (..))

-- An environment is defined as :
-- Size of the box of the game
-- Velocity of the ball
-- Base length of bricks
-- Total number of bricks
-- Position of bricks
data Env =
  Env
    { size        :: (Int, Int)
    , velocity    :: Int
    , baselength  :: Int
    , bricklength :: Int
    , numOfBricks :: Int
    , posOfBricks :: [Int]
    }

defaultEnv :: Env
defaultEnv =
  Env
    { size = (50, 20)
    , velocity = 1
    , baselength = 10
    , bricklength = 3
    , numOfBricks = 0
    , posOfBricks = []
    }
