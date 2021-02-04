{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict #-}


module Numeric.Utils
( dot
, (+^), (-^), (*^), (/^), interp, between, tolerance
, grad, negGrad
, DVec
, mround, dround
) where

import Data.Vector.Unboxed ((!),(//),Unbox)
import qualified Data.Vector.Unboxed as U
import Control.Monad.ST
-- import Data.Time (Day)
-- import qualified Data.HashMap.Strict as Hm
-- import Data.Hashable

import Debug.Trace (trace)
debug = flip trace

type DVec = U.Vector Double

infixl 7 *^, /^
infixl 6 +^, -^
infix 4 `between` 

dot :: (Unbox a, Num a) => U.Vector a -> U.Vector a -> a
dot x y = U.sum $ U.zipWith (*) x y; {-# INLINE dot #-}

(+^) :: (Unbox c, Num c) => U.Vector c -> U.Vector c -> U.Vector c
(+^) = U.zipWith (+); {-# INLINE (+^) #-}

(-^) :: (Unbox c, Num c) => U.Vector c -> U.Vector c -> U.Vector c
(-^) = U.zipWith (-); {-# INLINE (-^) #-}

(*^) :: (Unbox b, Fractional b) => U.Vector b -> b -> U.Vector b
(*^) x s = U.map (s*) x; {-# INLINE (*^) #-}

(/^) :: (Unbox b, Fractional b) => U.Vector b -> b -> U.Vector b
(/^) x s = U.map (/s) x; {-# INLINE (/^) #-}

interp :: DVec -> DVec -> Double -> DVec
interp x y s = U.zipWith (\xi yi -> (1.0-s)*xi + s*yi) x y

grad :: (DVec -> Double) -> DVec -> DVec
grad f v = runST $ do
  let f0 = f v
  U.forM (U.fromList [0..(U.length v - 1)]) $ \i -> do
    let vi = v ! i; dvm = vi*1e-8; z = v // [(i, vi + dvm)] 
    return ((f z - f0)/dvm) -- `debug` ("hi " ++ show z)

negGrad :: (DVec -> Double) -> DVec -> DVec
negGrad f v = runST $ do
  let f0 = f v
  U.forM (U.fromList [0..(U.length v - 1)]) $ \i -> do
    let vi = v ! i; dvm = vi*1e-8; z = v // [(i, vi - dvm)] 
    return ((f z - f0)/dvm) -- `debug` ("hi " ++ show z)

mround :: RealFloat a => a -> a -> a
mround m = (*m) . fromInteger . round . (/m)

dround :: RealFloat  a => Int -> a -> a
dround n = mround (1/(10 ^^ n))

between :: Ord a => a -> (a, a) -> Bool   
between x (y,z) = y <= x && x < z

tolerance :: (Ord a, Num a) => a -> a -> a -> Bool 
tolerance t x y = y `between` (x-t,x+t)