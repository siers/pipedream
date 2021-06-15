{-# LANGUAGE NumericUnderscores #-}

import Control.Parallel.Strategies (withStrategy, parList, rseq)

tl :: Int -> Int
tl n = length (foldr1 (\a b -> (+) <$> a <*> b) (map Just [1..n]))

b = 1_000_000
main = traverse print $ (withStrategy (parList rseq)) (map tl [b..(b + 20)])

-- ghc -threaded -O trash/2021-05-10-parallel.hs -o trash/2021-05-10-parallel && time ./trash/2021-05-10-parallel +RTS -N8

--

{-
import Control.Concurrent
import Control.Monad (forM_, when)
import Data.List (transpose, intercalate)
import System.IO (hFlush, stdout)

-- | Evaluates the projected list items (of type @b@) to WHNF before returning.
parSeq :: Bool -> (a -> b) -> [a] -> IO ()
parSeq showProgress projection items = do
    numThreads <- max 1 . subtract 1 <$> getNumCapabilities
    let chunkify _ [] = []
        chunkify n l = uncurry (:) (fmap (chunkify n) (splitAt n l))
        jobLists = transpose (chunkify numThreads items)
    signalmvar <- newEmptyMVar
    chan <- newChan
    -- print (map length jobLists)
    forM_ jobLists $ \l -> forkIO $ do
        readMVar signalmvar
        foldr seq () (map projection l) `seq` writeChan chan ()
    putMVar signalmvar ()
    -- putStrLn "waiting on channel..."
    let statusprefix = "(" ++ intercalate "," (map (show . length) jobLists) ++ " "
        statuslength = length statusprefix + 5
        statuspct n = statusprefix ++ leftPad 3 ' ' (show n) ++ "%)\x1B[" ++ show statuslength ++ "D"
    when showProgress $ do
        putStr (statuspct (0 :: Int))
        hFlush stdout
    forM_ [1 .. length jobLists] $ \i -> do
        readChan chan
        when showProgress $ do
            putStr (statuspct (100 * i `div` length jobLists))
            hFlush stdout
    when showProgress $ do
        putStr (replicate statuslength ' ' ++ "\x1B[" ++ show statuslength ++ "D")
        hFlush stdout
-}
