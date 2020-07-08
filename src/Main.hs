{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}

module Main where

-- import qualified Finz as F
import Debug.Trace
import Control.DeepSeq (($!!))
import Data.List (foldl')

data RunTotal = RunTotal 
    {   sum :: !Int
    ,   count :: !Int 
    }

prnAvg :: RunTotal -> IO ()
prnAvg (RunTotal sm cnt) 
    | cnt == 0 = error "No values"
    | otherwise = print (fromIntegral sm / fromIntegral cnt :: Double ) 

prnListAvg :: [Int] -> IO ()
prnListAvg = go (RunTotal 0 0) where 
    go rt [] = prnAvg rt 
    go (RunTotal sm ct) (x:xs) = go (RunTotal (sm+x) (ct+1)) xs

mySum :: [Int] -> Int 
mySum = foldl' (+) 0

main :: IO ()
main = do

    prnListAvg [1..1000_000]

    print $ mySum [1..1000_000] -- foldl' >> foldr > foldl



