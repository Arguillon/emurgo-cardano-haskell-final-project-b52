module Animation.Env where

import           Control.Monad.Trans.Reader (ask)

data Env =
  Env
    { size       :: (Int, Int)
    , velocity   :: Int
    , baselength :: Int
    }

defaultEnv :: Env
defaultEnv = Env {size = (20, 10), velocity = 1, baselength = 3}
