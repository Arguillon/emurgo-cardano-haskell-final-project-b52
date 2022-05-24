module Animation.Type where

import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State.Strict (StateT (..), evalStateT)

type Animation env st a = ReaderT env (StateT st IO) a

data Object
  = Ball Int
  | Base Int Int

data Brick =
  Brick
    { brickPosition :: (Int, Int)
    , life          :: Int
    }
  deriving (Eq)

data GameStatus
  = Paused
  | Playing
  | Stopped
  | LevelComplete
  | Restarted
  deriving (Show) -- , Eq)

data UserInput
  = MoveLeft
  | MoveRight
  | Pause
  | Stop
  | Start
  | Restart

runAnimation :: env -> st -> Animation env st a -> IO a
runAnimation env st action = evalStateT (runReaderT action env) st
