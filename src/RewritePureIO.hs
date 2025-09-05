module RewritePureIO (Console(..), evalOnePathPureConsole) where

--- import Control.Monad (ap)
import           Tarnyq hiding (evalOnePath)
import qualified Tarnyq        (evalOnePath)


class Monad m => Console m where
  printConsole :: String -> m ()

newtype RewritePureIO s a = RewritePureIO { inner :: (RewriteM (s, [String]) a) }
    deriving (Functor, Applicative, Monad, MonadFail)

instance Console (RewritePureIO s) where
    printConsole str = RewritePureIO $ do (s, output) <- get
                                          put (s, (str:output))

instance MonadRewrite (RewritePureIO s) s where
    get   = RewritePureIO $ RewriteM $ \(s, o) -> Just (s, (s, o))
    put s = RewritePureIO $ RewriteM $ \(_, o) -> Just ((), (s, o))
    matchFail = RewritePureIO $ RewriteM $ \_ -> Nothing



evalOnePathPureConsole :: [RewritePureIO s ()] -> s -> (s, [String])
evalOnePathPureConsole rewrites state
    = Tarnyq.evalOnePath (map inner rewrites) (state, [])

