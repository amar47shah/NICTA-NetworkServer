module Network.Server.Chat.Chat where

import Network.Server.Common.Line
import Network.Server.Chat.Loop

import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (fromMaybe)
import Data.Foldable (msum)
import Data.IORef (atomicModifyIORef)

type Chat a      = IORefLoop Integer a
data ChatCommand = Chat String
                 | Incr
                 | Unknown String
                 deriving (Eq, Show)

incr :: Chat Integer
incr = readEnvval >>= liftIO . flip atomicModifyIORef (succ &&& succ)

chat :: IO a
chat = iorefLoop 0 (readIOEnvval >>= pPutStrLn . show) $ process . chatCommand

-- |
--
-- >>> chatCommand "CHAT hi"
-- Chat "hi"
--
-- >>> chatCommand "Chat bye"
-- Chat "bye"
--
-- >>> chatCommand "INCR"
-- Incr
--
-- >>> chatCommand "Nothing"
-- UNKNOWN "Nothing"
chatCommand :: String -> ChatCommand
chatCommand z = Unknown z `fromMaybe` msum [ Chat <$> trimPrefixThen "CHAT" z
                                           , Incr <$  trimPrefixThen "INCR" z
                                           ]

process :: ChatCommand -> Chat ()
process (Chat _)    = error "Chat.process (Chat _)"
process Incr        = void incr
process (Unknown _) = error "Chat.process (Unknown _)"
