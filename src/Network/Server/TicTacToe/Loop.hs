{-# LANGUAGE TupleSections #-}
module Network.Server.TicTacToe.Loop where

import Network.Server.Common.Accept
import Network.Server.Common.Env
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Ref

import Prelude hiding (mapM_)

import Control.Arrow (first)
import Control.Concurrent (forkIO)
import Control.Exception (finally, try, catch, Exception)
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Foldable (mapM_)
import Data.Set (Set)
import Network (PortID(..), sClose, withSocketsDo, listenOn)
import System.IO (BufferMode(..))

import qualified Data.Set as S

data Loop      v s f a = Loop (Env v -> s -> f (a, s))
type IOLoop    v s   a = Loop v s IO a
type IORefLoop v s   a = IOLoop (IORef v) s a

execLoop :: Functor f => Loop v s f a -> Env v -> s -> f a
execLoop (Loop l) env s = fst <$> l env s

initLoop :: Functor f => (Env v -> f a) -> Loop v s f a
initLoop f = Loop $ \env s -> (, s) <$> f env

instance Functor f => Functor (Loop v s f) where
  fmap f (Loop l) = Loop $ \env s -> first f <$> l env s

instance Monad f => Applicative (Loop v s f) where
  pure a = Loop . const $ pure . (a, )
  Loop lf <*> Loop lx =
    Loop $ \env s ->
      lf env s >>= \(f, s') ->
        first f <$> lx env s'

instance Monad f => Monad (Loop v s f) where
  Loop l >>= k =
    Loop $ \env s ->
      l env s >>= \(a, s') ->
        let Loop l' = k a in l' env s'

instance MonadTrans (Loop v s) where
  lift mx = Loop . const $ \s -> (, s) <$> mx

instance MonadIO f => MonadIO (Loop v s f) where
  liftIO = lift . liftIO

etry ::
  Exception e =>
  (Env v -> IO a)
  -> IOLoop v s (Either e a)
etry k =
  initLoop $ try . k

server ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> s -- initial state
  -> IOLoop v s () -- per-client
  -> IO a
server i r t l =
  let hand s w c = forever $
                     do q <- accept' s
                        lSetBuffering q NoBuffering
                        _ <- atomicModifyIORef_ c (S.insert (refL `getL` q))
                        x <- r w
                        forkIO (execLoop l (Env q c x) t)
  in withSocketsDo $ do
       s <- listenOn (PortNumber 6060)
       w <- i
       c <- newIORef S.empty
       hand s w c `finally` sClose s

perClient ::
  IOLoop v s x -- client accepted (post)
  -> (String -> IOLoop v s a) -- read line from client
  -> IOLoop v s ()
perClient q f =
  let lp = do k <- etry lGetLine
              case k of Left e -> xprint e
                        Right [] -> lp
                        Right l -> f l >> lp
  in do _ <- q
        lp

loop ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> s -- initial state
  -> IOLoop v s x -- client accepted (post)
  -> (String -> IOLoop v s w) -- read line from client
  -> IO a
loop i r s q f =
  server i r s (perClient q f)

iorefServer ::
  v -- server initialise
  -> s -- initial state
  -> IORefLoop v s () -- per-client
  -> IO a
iorefServer x s =
  server (newIORef x) return s

iorefLoop ::
  v -- server initialise
  -> s -- initial state
  -> IORefLoop v s x -- client accepted (post)
  -> (String -> IORefLoop v s w) -- read line from client
  -> IO a
iorefLoop x s q f =
  iorefServer x s (perClient q f)

pPutStrLn ::
  String
  -> IOLoop v s ()
pPutStrLn s =
  initLoop (`lPutStrLn` s)

(!) ::
  Foldable t =>
  IOLoop v s (t Ref)
  -> String
  -> IOLoop v s ()
clients ! msg =
 clients >>= purgeClients (\y -> liftIO (lPutStrLn y msg))

infixl 2 !

purgeClients ::
  Foldable t =>
  (Ref -> IOLoop s v ())
  -> t Ref
  -> IOLoop s v ()
purgeClients a =
  mapM_ (\y ->
    ecatch (a y)
      (\x -> do _ <- modifyClients (S.delete y)
                xprint x)
        )

readEnv ::
  Applicative f =>
  Loop v s f (Env v)
readEnv =
  initLoop $ pure

readEnvval ::
  Applicative f =>
  Loop v s f v
readEnvval =
  fmap (envvalL `getL`) readEnv

readIOEnvval ::
  IORefLoop a s a
readIOEnvval =
  initLoop $ \env ->
    readIORef (envvalL `getL` env)

allClientsButThis ::
  IOLoop v s (Set Ref)
allClientsButThis =
  initLoop $ \env ->
    fmap (S.delete ((acceptL .@ refL) `getL` env)) (readIORef (clientsL `getL` env))

-- Control.Monad.CatchIO
ecatch ::
  Exception e =>
  IOLoop v s a
  -> (e -> IOLoop v s a)
  -> IOLoop v s a
ecatch (Loop k) f =
  Loop $ \env s -> k env s `catch` (\e -> let Loop l = f e in l env s)

modifyClients ::
  (Set Ref -> Set Ref)
  -> IOLoop v s (Set Ref)
modifyClients f =
  initLoop $ \env ->
    atomicModifyIORef_ (clientsL `getL` env) f
