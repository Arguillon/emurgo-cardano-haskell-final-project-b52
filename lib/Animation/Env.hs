module Animation.Env where

import           Animation.Type             (UserInput (..))
import           Control.Monad.Trans.Reader (ask)
import           Data.IORef

data Env =
  Env
    { size               :: (Int, Int)
    , velocity           :: Int
    , baselength         :: Int
    , userInputReference :: IORef [UserInput]
    }

defaultEnv :: Env
defaultEnv =
  Env
    { size = (20, 10)
    , velocity = 1
    , baselength = 3
    , userInputReference = error "input reference not initialized"
    }
