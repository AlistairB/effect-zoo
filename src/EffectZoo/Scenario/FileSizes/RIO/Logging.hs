module EffectZoo.Scenario.FileSizes.RIO.Logging where

import RIO hiding ( HasLogFunc(..), LogFunc(..) )
import qualified EffectZoo.Scenario.FileSizes.Shared as Shared

newtype LogFunc = LogFunc ( String -> IO () )

class HasLogFunc env where
  logFuncL :: Lens' env LogFunc

logMsg :: HasLogFunc env => String -> RIO env ()
logMsg msg =
  view logFuncL >>= \(LogFunc f) -> liftIO ( f msg )

logToIORef :: IORef [String] -> LogFunc
logToIORef r =
  LogFunc ( Shared.logToIORef r )
