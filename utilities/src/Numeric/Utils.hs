{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict #-}


module Numeric.Utils
( dot
, (+^), (-^), (*^), (/^), interp
, grad
, negGrad
, DVec
) where

import Data.Vector.Unboxed ((!),(//))
import qualified Data.Vector.Unboxed as U
import Control.Monad.ST
-- import Data.Time (Day)
-- import qualified Data.HashMap.Strict as Hm
-- import Data.Hashable

import Debug.Trace (trace)
debug = flip trace

type DVec = U.Vector Double

dot x y = U.sum $ U.zipWith (*) x y

(+^) x y = U.zipWith (+) x y
(-^) x y = U.zipWith (-) x y
(*^) x s = U.map (s*) x
(/^) x s = U.map (/s) x

interp :: DVec -> DVec -> Double -> DVec
interp x y s = U.zipWith (\xi yi -> (1.0-s)*xi + s*yi) x y

grad :: (DVec -> Double) -> DVec -> DVec
grad f v = runST $ do
  let f0 = f v
  U.forM (U.fromList [0..((U.length v) - 1)]) $ \i -> do
    let vi = v ! i; dvm = vi*1e-8; z = v // [(i, vi + dvm)] 
    return (((f z) - f0)/dvm) -- `debug` ("hi " ++ show z)

negGrad :: (DVec -> Double) -> DVec -> DVec
negGrad f v = runST $ do
  let f0 = f v
  U.forM (U.fromList [0..((U.length v) - 1)]) $ \i -> do
    let vi = v ! i; dvm = vi*1e-8; z = v // [(i, vi - dvm)] 
    return (((f z) - f0)/dvm) -- `debug` ("hi " ++ show z)
