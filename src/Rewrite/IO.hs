module Rewrite.IO ( ProgramIO(..), ProgramIOState(..)
                  , RewritePure, evalOnePathIOPure, evalAllPathsIOPure
                  , RewriteIO, evalOnePathIO
                  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Text.Read (readMaybe)

import Rewrite.Class
import Rewrite.Basic


-- | The ProgramIO typeclass enables Rewrites to perfom IO actions.
-- This module provides two instances of MonadRewrite that are also instances
-- of ProgramIO:
-- 1.   'RewritePure' that provides a "mock" IO, and
-- 2.   'RewriteIO' that forwards the calls to operating system's IO.
--
class Monad m => ProgramIO m where
  -- | Print a line to stdout
  printConsole :: Integer -> m ()
  -- | Read a line from stdin
  readConsole  :: m (Maybe Integer)

-------------------------------------------------------------------------------

-- | 'RewritePure' implements both MonadRewrite and a mock ProgramIO, by making
-- the states for each sub-components of a larger state and re-using
-- RewriteBasic's implementation.

data ProgramIOState = ProgramIOState { input  :: [Integer]
                                     , output :: ![Integer]
                                     }
    deriving (Show, Eq)

newtype RewritePure s a =
        RewritePure { unwrap :: (RewriteBasic (s, ProgramIOState) a) }
    deriving (Functor, Applicative, Monad, MonadFail)

instance ProgramIO (RewritePure s) where
    {-# INLINE printConsole #-}
    printConsole str = RewritePure $ do (s, pis@ProgramIOState{output=o}) <- get
                                        put (s, pis {output=(str:o)})
    {-# INLINE readConsole #-}
    readConsole = RewritePure $
        do (s, pis@ProgramIOState{input}) <- get
           case input of
               []   -> pure Nothing
               i:is -> do put (s, pis {input=is})
                          pure $ Just i

instance MonadRewrite (RewritePure s) s where
    get   = RewritePure $ do (s, _) <- get
                             pure s
    put s' = RewritePure $ do (_, io) <- get
                              put (s', io)

{-# INLINE evalOnePathIOPure #-}
evalOnePathIOPure :: [RewritePure s ()] -> s -> [Integer] -> (s, ProgramIOState)
evalOnePathIOPure rewrites state input
    = evalOnePath (map unwrap rewrites) (state, ProgramIOState input [])

evalAllPathsIOPure :: [RewritePure s ()] -> s -> [Integer] -> [(s, ProgramIOState)]
evalAllPathsIOPure rewrites state input
    = evalAllPaths (map unwrap rewrites) (state, ProgramIOState input [])

-------------------------------------------------------------------------------

-- | 'RewriteIO' also implements both MonadRewrite and ProgramIO, by calling
-- through to the IO monad. Note that this does not provide all-path evaluation,
-- since the IO monad does not allow backtracking.
--
-- Eventually, this instance could be extended soundly allow
-- more advanced features such as mutable datastructures. e.g. an optimized
-- stack implementation for EVM.
--
-- TODO     Much of the code below is duplicated from RewriteBasic.
-- Could this be avoided if we used the MaybeT monad transformer?
-- One aspect to keep in mind is that `IO (Maybe s)` prohibits implementing
-- all-path reachability since backtracking cannot be performed in the IO
-- monad.

newtype RewriteIO s a = RewriteIO { rewriterIO :: s -> IO (Maybe (a, s)) }
    deriving Functor

instance Applicative (RewriteIO s) where
    pure x = RewriteIO $ \s -> pure $ Just (x, s)
    (<*>) = ap

instance Monad (RewriteIO s) where
    {-# INLINE (>>=) #-}
    p >>= q = RewriteIO $
        \s -> do ps <- ((rewriterIO p) s)
                 case ps of
                    Just (a', s') -> ((rewriterIO $ q a') s')
                    Nothing       -> pure Nothing

-- MonadFail allows us to have binding patterns that fail in do notation.
instance MonadFail (RewriteIO s) where
    fail _ = RewriteIO $ \_ -> pure Nothing

instance Alternative (RewriteIO s) where
    empty = RewriteIO $ \_ -> pure Nothing
    {-# INLINE (<|>) #-}
    r1 <|> r2 = RewriteIO $ \s -> do r1s <- ((rewriterIO r1) s)
                                     r2s <- ((rewriterIO r2) s)
                                     pure $ r1s <|> r2s

instance MonadRewrite (RewriteIO s) s where
    get    = RewriteIO $ \s -> pure $ Just (s, s)
    put s' = RewriteIO $ \_ -> pure $ Just ((), s')
    matchFail = RewriteIO $ \_ -> pure Nothing

instance ProgramIO (RewriteIO s) where
    {-# INLINE printConsole #-}
    printConsole v = RewriteIO $ \s -> do print v
                                          pure $ Just ((), s)

    {-# INLINE readConsole #-}
    readConsole = RewriteIO $ \s -> do l <- getLine
                                       case readMaybe @Integer l of
                                        Just v  -> pure $ Just (Just v, s)
                                        Nothing -> pure $ Just (Nothing, s)

{-# INLINE evalOnePathIO #-}
evalOnePathIO :: forall s. [RewriteIO s ()] -> s -> IO s
evalOnePathIO rewrites state = do s <- applyRewrite eval' state
                                  pure $ unwrap s
  where
    eval' :: RewriteIO s ()
    eval' =     (next >> eval') -- If next succeeds, recurse
            <|> pure ()         -- otherwise return the previous state

    next :: RewriteIO s ()
    next = asum rewrites -- Choose first rewrite that applies

    unwrap :: Maybe a -> a
    unwrap = fromMaybe undefined -- always returns a Just.

    applyRewrite :: RewriteIO s () -> s -> IO (Maybe s)
    applyRewrite r s = do rhs <- rewriterIO r s
                          pure $ fmap snd rhs
