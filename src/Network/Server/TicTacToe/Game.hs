module Network.Server.TicTacToe.Game where

import Data.TicTacToe
import Network.Server.Common.Env
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Ref
import Network.Server.TicTacToe.Loop
import Data.Char(isSpace, toLower, toUpper)
import Data.Function(on)
import Data.IORef(readIORef, atomicModifyIORef)
import Data.Maybe(fromMaybe)
import Data.Foldable(asum, find)
import Data.Set(Set)
import Control.Applicative((<$), (<$>))
import System.IO(hGetLine, hPutStrLn)

type FinishedGames =
  [FinishedBoard]

type Game a =
  IORefLoop Board (Board, FinishedGames) a

data Command =
  Move Position
  | Current
  | Finished
  | Chat String
  | Turn
  | At Position
  | Unknown String
  deriving (Eq, Show)

-- |
--
-- >>> command "MOVE ne"
-- Move NE
--
-- >>> command "MOVE 2"
-- Move N
--
-- >>> command "GAME"
-- Current
--
-- >>> command "FiniSHED"
-- Finished
--
-- >>> command "CHAT hi"
-- Chat "hi"
--
-- >>> command "Turn"
-- Turn
--
-- >>> command "At 4"
-- At W
--
-- >>> command "At C"
-- At C
--
-- >>> command "At X"
-- Unknown "At X"
--
-- >>> command "Move i"
-- Unknown "Move i"
command :: String -> Command
command z =
  fromMaybe (Unknown z) (interpreted z)
      where
  trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  p = ((trim <$>) .) . prefixThen ((==) `on` toLower)
  interpreted z' = asum [ Move <$> (p z' "MOVE " >>= sPosition)
                        , Current <$ p z' "GAME"
                        , Finished <$ p z' "FINISHED"
                        , Chat <$> p z' "CHAT"
                        , Turn <$ p z' "TURN"
                        , At <$> (p z' "AT" >>= sPosition)
                        ]

-- |
--
-- >>> sPosition "1"
-- Just NW
--
-- > sPosition "E"
-- Just E
--
-- > sPosition "sw"
-- Just SW
--
-- > sPosition "x"
-- Nothing
sPosition :: String -> Maybe Position
sPosition s =
  snd <$> find (match s) table
      where
  match s' (t, _) = toUppers s' `elem` fmap toUppers t
  toUppers = map toUpper
  table = [ (["1", "NW"], NW)
          , (["2", "N" ], N )
          , (["3", "NE"], NE)
          , (["4",  "W"],  W)
          , (["5", "C" ], C )
          , (["6",  "E"],  E)
          , (["7", "SW"], SW)
          , (["8", "S" ], S )
          , (["9", "SE"], SE)
          ]

currentBoard ::
  Game Board
currentBoard =
  initLoop $ \env ->
    readIORef (envvalL `getL` env)

withCurrentBoard ::
  (Board -> (Board, a))
  -> Game a
withCurrentBoard f =
  initLoop $ \env ->
    atomicModifyIORef (envvalL `getL` env) f

lastBoard ::
  Game Board
lastBoard =
  Loop $ \_ (s, t) ->
    return (s, (s, t))

putBoard ::
  Board
  -> Game ()
putBoard s =
  Loop $ \_ (_, t) ->
      return ((), (s, t))

modifyFinishedGames ::
  (FinishedGames -> FinishedGames)
  -> Game ()
modifyFinishedGames f =
  Loop $ \_ (s, t) -> return ((), (s, f t))

finishedGames ::
  Game FinishedGames
finishedGames =
  Loop $ \_ (s, t) -> return (t, (s, t))

eGetLine ::
  Game String
eGetLine =
  initLoop (hGetLine . getL handleL)

ePutStrLn ::
  String
  -> Game ()
ePutStrLn s =
  initLoop (\env -> (hPutStrLn (handleL `getL` env) s))

allClients ::
  Game (Set Ref)
allClients =
  initLoop $ \env -> (readIORef (clientsL `getL` env))

process ::
  Command
  -> Game ()
process =
  error "Game.process"

game ::
  Game x -- client accepted (post)
  -> (String -> Game w) -- read line from client
  -> IO a
game =
  error "Game.game"

play ::
  IO a
play =
  game (currentBoard >>= pPutStrLn . show) (process . command)
