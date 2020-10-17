{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Strict #-}

module Finance.Bonds 
  ( twrr, twrrN, tBillR, tBillD, holdPerYd, effAnnYd
  , moneyMktYd

  ) where

import Finance.Utilities.Numeric ( DVec)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!),(//))
import Control.Monad.ST

twrr :: DVec -> DVec -> Double
twrr bv bi = (\x-> x**(1/(fromIntegral n)) - 1) $ U.product $ runST $ do 
  U.forM (U.fromList [0..(n-1)]) $ \i -> do return $ bv!(i+1)/(bv!i + bi!i)
  where n = U.length bv - 1

twrrN :: Double -> DVec -> DVec -> Double
twrrN n bv bi = (\x-> x**(1/n) - 1) $ U.product $ runST $ do 
  U.forM (U.fromList [0..(U.length bv - 2)]) $ \i -> do 
    return $ bv!(i+1)/(bv!i + bi!i)

tBillR :: Double -> Double -> Double -> Double
tBillR t p0 f = (1.0 - p0/f) * 360.0/t

tBillD :: Double -> Double -> Double -> Double
tBillD r t f = r*t*f/360.0

holdPerYd :: Double -> Double -> Double -> Double
holdPerYd p0 p1 d1 = (p1+d1)/p0-1.0

effAnnYd :: Double -> Double -> Double -> Double -> Double
effAnnYd t p0 p1 d1 = ((p1+d1)/p0)**(365.0/t) - 1.0

moneyMktYd :: Double -> Double -> Double -> Double -> Double
moneyMktYd t p0 p1 d1 = ((p1+d1)/p0-1.0)*360/t

