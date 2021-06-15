module Main where

import Control.Arrow
import Control.Monad.Writer.Strict -- (MonadReader(..), Reader, ReaderT(..), ask, withReaderT, mapReaderT)
import Control.Monad.Trans.Maybe
import Data.Monoid (Sum(..))

data P = P !Int !Int

type CounterT w = WriterT w (MaybeT IO)

type F w = Int -> Maybe (w, Int)

-- | https://hackage.haskell.org/package/monad-extras-0.6.0/docs/src/Control-Monad-Extra.html#iterateM
-- | Monadic equivalent to 'iterate', which uses Maybe to know when to terminate.
iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM 0 _ _ = pure []
iterateM n f x = do
  x' <- f x
  (x':) `liftM` iterateM (n - 1) f x'

liftCounter :: Monoid w => F w -> (Int -> CounterT w Int)
liftCounter f a = do
  (w, a) <- lift . MaybeT . pure $ f a
  tell w
  pure a

collatz :: Int -> Maybe (Sum Int, Int)
collatz 1 = Nothing
collatz n = if even n then Just (Sum 1, n `div` 2) else Just (Sum 1, (3 * n + 1))

main = print =<< runMaybeT (runWriterT (iterateM 100 (liftCounter collatz) 500))

{-

:t (\(f :: F) -> lift . MaybeT . pure . f) :: F -> CounterT

-}
