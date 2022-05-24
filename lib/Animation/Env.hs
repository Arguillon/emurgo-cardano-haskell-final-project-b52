module Animation.Env where

-- import Data.IORef
import           Animation.Type (UserInput (..))

data Env =
  Env
    { size        :: (Int, Int)
    , velocity    :: Int
    , baselength  :: Int
    , bricklength :: Int
    , numOfBricks :: Int
    , posOfBricks :: [Int]
    }

--      , userInputReference :: IORef [UserInput]
defaultEnv :: Env
defaultEnv =
  Env
    { size = (50, 20)
    , velocity = 1
    , baselength = 10
    , bricklength = 3
    , numOfBricks = 0
    , posOfBricks = []
--      , userInputReference = error "input reference not initialized"
    }
