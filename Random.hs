module Random where

import System.Random

randVal :: [a] -> IO a
randVal range = do
    let num = length range
    d <- rand (0, num - 1)
    return $ range !! d

randStream :: (Int, Int) -> IO [Int]
randStream range = randomRs range <$> newStdGen

rand :: (Int, Int) -> IO Int 
rand range = do
    r <- randomRs range <$> newStdGen
    return $ head r