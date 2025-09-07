module Rewrite.IO ( ProgramIO(..), ProgramIOState(..)
                  , RewritePure, evalOnePathIOPure, evalAllPathsIOPure
                  , RewriteIO, evalOnePathIO
                  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe

import Rewrite.Class
import Rewrite.Basic

class Monad m => ProgramIO m where
  -- |Print a line to stdout
  printConsole :: String -> m ()
  -- |Read a line from stdin
  readConsole  :: m (Maybe String)


data ProgramIOState = ProgramIOState { input  :: [String]
                                     , output :: ![String]
                                     }
    deriving (Show, Eq)

newtype RewritePure s a =
        RewritePure { unwrap :: (RewriteBasic (s, ProgramIOState) a) }
    deriving (Functor, Applicative, Monad, MonadFail)

instance ProgramIO (RewritePure s) where
    printConsole str = RewritePure $ do (s, pis@ProgramIOState{output=o}) <- get
                                        put (s, pis {output=(str:o)})
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

evalOnePathIOPure :: [RewritePure s ()] -> s -> [String] -> (s, ProgramIOState)
evalOnePathIOPure rewrites state input
    = evalOnePath (map unwrap rewrites) (state, ProgramIOState input [])

evalAllPathsIOPure :: [RewritePure s ()] -> s -> [String] -> [(s, ProgramIOState)]
evalAllPathsIOPure rewrites state input
    = evalAllPaths (map unwrap rewrites) (state, ProgramIOState input [])

-------------------------------------------------------------------------------

newtype RewriteIO s a = RewriteIO { getFunIO :: s -> IO (Maybe (a, s)) }
    deriving Functor

instance Applicative (RewriteIO s) where
    pure x = RewriteIO $ \s -> pure $ Just (x, s)
    (<*>) = ap

instance Monad (RewriteIO s) where
    {-# INLINE (>>=) #-}
    p >>= q = RewriteIO $
        \s -> do ps <- ((getFunIO p) s)
                 case ps of
                    Just (a', s') -> ((getFunIO $ q a') s')
                    Nothing       -> pure Nothing

-- MonadFail allows us to have binding patterns that fail in do notation.
instance MonadFail (RewriteIO s) where
    fail _ = RewriteIO $ \_ -> pure Nothing

instance Alternative (RewriteIO s) where
    empty = RewriteIO $ \_ -> pure Nothing
    {-# INLINE (<|>) #-}
    r1 <|> r2 = RewriteIO $ \s -> do r1s <- ((getFunIO r1) s)
                                     r2s <- ((getFunIO r2) s)
                                     pure $ r1s <|> r2s

instance MonadRewrite (RewriteIO s) s where
    get   = RewriteIO $ \s -> pure $ Just (s, s)
    put s = RewriteIO $ \_ -> pure $ Just ((), s)
    matchFail = RewriteIO $ \_ -> pure Nothing

instance ProgramIO (RewriteIO s) where
    printConsole str = RewriteIO $ \s -> do putStrLn str
                                            pure $ Just ((), s)

    -- TODO: Handle case where exceptions
    readConsole = RewriteIO $ \s -> do str <- getLine
                                       pure $ Just (Just str, s)

{-# INLINE evalOnePathIOPure #-}
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
    applyRewrite r s = do rhs <- getFunIO r s
                          pure $ fmap snd rhs
