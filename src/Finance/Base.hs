{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Strict #-}


module Finance.Base
  ( timeMul, timeMulSub
  , rate, period
  , pv, pvm, pvc, pvAnnuity, pvAnnuityCont, fvAnnuity, pmt 
  , fmt, fv, fvm, fvc 
  , effectiveRate, effectiveRateCont
  , npv, npvN, npvT, npvNT, irr, xirr, twrr, twrrN
  , tBillR, tBillD, holdPerYd, effAnnYd, moneyMktYd
  , sharpe
  ) where

import Utilz.Numeric (dot,(+^), (-^), (*^), (/^), DVec)
import qualified Utilz.Numeric.Optima as Op
import Data.Approx
import Control.Monad.ST

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!),(//))

timeMul :: Double -> Double -> Double
timeMul r n = (1+r)**n 

timeMulSub :: Double -> Double -> Double -> Double
timeMulSub r n m = timeMul (r/m) (n*m)

rate :: Double -> Double -> Double -> Double
rate f p n = (f/p)**(1/n) - 1

period :: Double -> Double -> Double -> Double
period f p r = (log (f/p))/(log (1+r))

pv :: Double -> Double -> Double -> Double
pv ft r n = ft/(1+r)**n 

pvm :: Double -> Double -> Double -> Double -> Double
pvm ft r n m = ft/(1+r/m)**(n*m)

pvc :: Double -> Double -> Double -> Double
pvc ft r n = ft/(exp (r*n))

pvAnnuity :: Double -> Double -> Double -> Double -> Double
pvAnnuity pt r n m = pt/(r/m)*(1 - 1/(1 + r/m)**(n*m))

pvAnnuityCont :: Double -> Double -> Double
pvAnnuityCont pt r = pt/r

fvAnnuity :: Double -> Double -> Double -> Double -> Double
fvAnnuity pt r n m = pt/(r/m) * ((1 + r/m)**(n*m) - 1)

pmt :: Double -> Double -> Double -> Double -> Double
pmt pt r n m = pt * (r/m) / (1 - 1/(1 + r/m)**(n*m))

fmt :: Double -> Double -> Double -> Double -> Double
fmt ft r n m = ft * (r/m) / ((1 + r/m)**(n*m) - 1)

fv :: Double -> Double -> Double -> Double
fv pt r n = pt*(1+r)**n

fvm :: Double -> Double -> Double -> Double -> Double
fvm pt r n m = pt*(1+r/m)**(n*m)

fvc :: Double -> Double -> Double -> Double
fvc ft r n = ft*(exp (r*n))

effectiveRate :: Double -> Double -> Double
effectiveRate r m = (1 + r/m)**m - 1

effectiveRateCont :: Double -> Double 
effectiveRateCont r = (exp r) - 1

npv :: Double -> DVec -> DVec -> Double -> Double
npv t0 tim cf r =(U.sum $ U.zipWith (\c t->c/r1**t) cf tim)/r1**t0 where r1=1+r

npvT :: DVec -> DVec -> Double -> Double
npvT tim cf r = (U.sum $ U.zipWith (\c t->c/r1**t) cf tim) where r1=1+r

npvN :: Double -> DVec -> Double -> Double
npvN t0 cf r = (U.sum $ U.zipWith (\c t->c/r1**t) cf (U.fromList [0.0,1.0..fromIntegral $ U.length cf - 1]))/r1**t0 where r1 = 1+r

npvNT :: DVec -> Double -> Double
npvNT cf r = (U.sum $ U.zipWith (\c t->c/r1**t) cf (U.fromList [0.0,1.0..fromIntegral $ U.length cf - 1])) where r1 = 1+r

irr :: DVec -> Maybe Double
irr cf = Op.newtRP (npvNT cf) 0.0

xirr :: DVec -> DVec -> Maybe Double
xirr tim cf = Op.newtRP (npvT tim cf) 0.0

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

sharpe :: Double -> Double -> Double -> Double
sharpe rf rp sp = (rp - rf)/sp

