{-|
Module      : Finance.Base
Description : Implement __Base__ modules for the __finz__ library
Copyright   : (c) 2020 Kishaloy Neogi
License     : MIT
Maintainer  : Kishaloy Neogi
Email       : nkishaloy@yahoo.com

The module describes the base modules of Finance like npv, xnpv, irr, xirr, time value of money etc. 

PV is mentioned as PV, Future value as FV and Terminal value as TV

You may see the github repository at <https://github.com/n-kishaloy/finz>

-}


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Strict #-} 


module Finance.Base
  ( timeMul, timeMulSub
  , rate, period, yearFrac
  , pv, pvm, pvc, pvAnnuity, pvAnnuityCont, fvAnnuity, pmt 
  , fmt, fv, fvm, fvc 
  , effectiveRate, effectiveRateCont
  , npv, npvN, npvT, npvNT, irr, virr, sharpe
  , DayData, TrendData, DTrend, listToDTrend, xnpv, xirr, xpv
  ) where

import Numeric.Utils (dot,(+^), (-^), (*^), (/^), DVec)
import qualified Numeric.Optima as Op
import Data.Approx
import Control.Monad.ST
import Data.Time (Day, fromGregorian, toGregorian, diffDays)

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!),(//))

import qualified Data.Vector as V

type DayData a = (Day, a)
type TrendData a = V.Vector (DayData a)

type DTrend = TrendData Double

{-|@timeMul n r = Time Multiplier@

*n = nos of time periods
*r = rate of increase / time period
-}
timeMul :: Double -> Double -> Double
timeMul n r = (1+r)**n 

{-|@timeMulSub n m r = Time Multiplier@

*n = nos of time periods
*m = nos of period / time period
*r = rate of increase / time period
-}
timeMulSub :: Double -> Double -> Double -> Double
timeMulSub n m r = timeMul (n*m) (r/m) 

{-|@rate f p n = Rate which gives a growth over a specified period@

*f = FV
*p = PV
*n = nos of time periods
-}
rate :: Double -> Double -> Double -> Double
rate f p n = (f/p)**(1/n) - 1

{-|
@period f p r = Number of time periods to increase from p to f@

*f = FV
*p = PV
*r = rate of return
-}
period :: Double -> Double -> Double -> Double
period f p r = (log (f/p))/(log (1+r))

{-|@pv f n r = PV of a Future cash flow@

*f = Future cash flow
*n = number of periods
*r = rate of return
-}
pv :: Double -> Double -> Double -> Double
pv f n r = f/(1+r)**n 

{-|@pvm f n m r = PV of a Future cash flow with multiple compounding per period@

*f = Future cash flow
*n = number of periods
*m = number of compounding per period
*r = rate of return
-}
pvm :: Double -> Double -> Double -> Double -> Double
pvm f n m r = f/(1+r/m)**(n*m)

{-|@pvc f n r = PV of continuous expontial growth@

*f = FV
*n = number of periods
*r = rate of return
-}
pvc :: Double -> Double -> Double -> Double
pvc f n r = f/(exp (r*n))

{-|@pvAnnuity p n m r = PV of an annuity with multiple payments per period@

*p = payment made in each transaction
*n = number of periods (/say/, years)
*m = number of payments per period (/say/, monthly where @m = 12@)
*r = rate of return
-}
pvAnnuity :: Double -> Double -> Double -> Double -> Double
pvAnnuity p n m r = p/(r/m)*(1 - 1/(1 + r/m)**(n*m))

{-|@pvAnnuityCont p r = PV of infinite payments@

*p = payments made at a time
*r = rate of return between /payments/
-}
pvAnnuityCont :: Double -> Double -> Double
pvAnnuityCont p r = p/r

{-|@fvAnnuity p n m r = FV of an annuity with multiple payments per period@

*p = payment made in each transaction
*n = number of periods (/say/, years)
*m = number of payments per period (/say/, monthly where @m = 12@)
*r = rate of return
-}
fvAnnuity :: Double -> Double -> Double -> Double -> Double
fvAnnuity p n m r = p/(r/m) * ((1 + r/m)**(n*m) - 1)

{-|@pmt p n m r = Payment to cover the PV of an Annuity@

*p = PV of Annuity
*n = number of periods (/say/, years)
*m = number of payments per period (/say/, monthly where @m = 12@)
*r = rate of return
-}
pmt :: Double -> Double -> Double -> Double -> Double
pmt p n m r = p * (r/m) / (1 - 1/(1 + r/m)**(n*m))

{-|@fmt f n m r = Payment to cover the FV of an Annuity@

*f = FV of Annuity
*n = number of periods (/say/, years)
*m = number of payments per period (/say/, monthly where @m = 12@)
*r = rate of return
-}
fmt :: Double -> Double -> Double -> Double -> Double
fmt f n m r = f * (r/m) / ((1 + r/m)**(n*m) - 1)

{-|@fv p n r = FV of a Present cash flow@

*p = Present cash flow
*n = number of periods
*r = rate of return
-}
fv :: Double -> Double -> Double -> Double
fv p n r = p*(1+r)**n

{-|@fvm p n m r = FV of a Future cash flow with multiple compounding per period@

*p = Present cash flow
*n = number of periods
*m = number of compounding per period
*r = rate of return
-}
fvm :: Double -> Double -> Double -> Double -> Double
fvm p n m r = p*(1+r/m)**(n*m)

{-|@fvc p n r = FV of continuous expontial growth@

*p = PV
*n = number of periods
*r = rate of return
-}
fvc :: Double -> Double -> Double -> Double
fvc p n r = p*(exp (r*n))

{-|@effectiveRate m r = real rate of return for multiple compounding per period@

*m = number of compounding per period
*r = rate of return in a period
-}
effectiveRate :: Double -> Double -> Double
effectiveRate m r = (1 + r/m)**m - 1

{-|@effectiveRateCont r = real rate of return for continuous exponential compounding@

*r = rate of return in a period
-}
effectiveRateCont :: Double -> Double 
effectiveRateCont r = (exp r) - 1

{-|@listToDTrend dt cf = List of Day and Double ==> DTrend@

*dt = List of Days
*cf = List of Doubles
-}
listToDTrend :: [Day] -> [Double] -> DTrend
listToDTrend dt cf = V.fromList $ zipWith (\d c -> (d,c)) dt cf

-- {-|@dayToYear dt = Convert Day to fraction of a year expressed as Double@

-- The following formula is used:

-- @y + [ (m-1) + d \/ daysInMonth ] \/ 12 , /where/ daysInMonth = /nos of days in the month/@

-- This formula is valid between for years in the range (1900,2100) as it 
-- uses a quick (__@y \`div\` 4 == 0@__) check for leap year to speed 
-- calculations. Appropriately, Feb has 29 or 28 days. 

-- This is eseentially the 30/360 formula.

-- For reference, <https://lists.oasis-open.org/archives/office-formula/200806/msg00040.html>
-- -}
-- dayToYear :: Day -> Double
-- dayToYear dt = (fromIntegral y) + (dateFrac y m d) 
--   where
--   (y,m,dI) = toGregorian dt
--   d = fromIntegral dI

--   dateFrac :: Integer -> Int -> Double -> Double
--   dateFrac y 4 d  = 0.25 + d/360.0
--   dateFrac y 6 d  = 5.0/12.0 + d/360.0
--   dateFrac y 9 d  = 8.0/12.0 + d/360.0
--   dateFrac y 11 d = 10.0/12.0 + d/360.0 
--   dateFrac y 2 d  = 1.0/12.0 + d/(if y `div` 4 == 0 then 348.0 else 336.0)
--   dateFrac y m d  = (fromIntegral (m-1))/12.0 + d/372.0
-- {-# INLINE dayToYear #-}

{-|@yearFrac d1 d0 = Day difference (d1 - d0) in fraction of a year@

*d1 = target date
*d0 = reference date

The formula divides the day by 365.25 instead of 365 to adjust for leap years
over multiple years period.
-}
yearFrac :: Day -> Day -> Double
yearFrac d1 d0 = (fromIntegral (d1 `diffDays` d0)) / 365.25
{-# INLINE yearFrac #-}

{-|@xpv dtData d0 r = Value of cash flow given by ddyData on date d0@

*dtData = (d1, f) where d1 is date and f is amount of cash flow
*d0 = date on which cash flow value is desired
*r = annual rate of return
-}
xpv :: DayData Double -> Day -> Double -> Double
xpv (d1, f) d0 r = f / (1+r)**(d1 `yearFrac` d0)
{-# INLINE xpv #-}

{-|@xnpv tr d0 r = NPV of cash flows against dates@

*tr = Vector of (Date, Cash Flow)
*d0 = Date at which NPV is sought. Essentially, NPV(di - d0) 
*r = annual rate of return
-}
xnpv :: DTrend -> Day -> Double -> Double
xnpv tr d0 r = V.foldl' (\p (d1,cf) -> p + cf/r1**(d1 `yearFrac` d0)) 0.0 tr 
  where r1 = 1+r

xirr :: DTrend -> Maybe Double
xirr tr = Op.newtRP (xnpv tr (fst (V.head tr))) 0.1

{-|@npv tim cf t0 r = NPV of cash flows against time given in periods@

*tim = time of cash flows given as periods
*cf = corresponding cash flows
*t0 = time period at which the NPV is sought. Essentially, NPV(ti - t0)
*r = rate of return across the periods
-}
npv :: DVec -> DVec -> Double -> Double -> Double
npv tim cf t0 r = (npvT tim cf r)*r1**t0 where r1=1+r

{-|@npvT tim cf r = NPV of cash flows against time given in periods@

*tim = time of cash flows given as periods
*cf = corresponding cash flows
*t0 = 0.0 (refer npv)
*r = rate of return across the periods
-}
npvT :: DVec -> DVec -> Double -> Double
npvT tim cf r = U.sum $ U.zipWith (\c t->c/r1**t) cf tim where r1=1+r

{-|@npvN cf t0 r = NPV of cash flows against time given in periods@

*tim = [0.0, 1.0 ..] (refer npv)
*cf = corresponding cash flows
*t0 = time period at which the NPV is sought. Essentially, NPV(ti - t0)
*r = rate of return across the periods
-}
npvN :: DVec -> Double -> Double -> Double
npvN cf t0 r = (npvNT cf r)*r1**t0 where r1 = 1+r

{-|@npvNT cf r = NPV of cash flows against time given in periods@

*tim = [0.0, 1.0 ..] (refer npv)
*cf = corresponding cash flows
*t0 = 0.0 (refer npv)
*r = rate of return across the periods
-}
npvNT :: DVec -> Double -> Double
npvNT cf r = U.sum $ U.zipWith (\c t->c/r1**t) cf (U.fromList [0.0,1.0..fromIntegral $ U.length cf - 1]) where r1 = 1+r

{-|@irr cf = IRR of cash flows assumed to be spread equally in time@

*cf =  cash flows
-}
irr :: DVec -> Maybe Double
irr cf = Op.newtRP (npvNT cf) 0.0

{-|@irr cf = IRR of cash flow against time given in periods@

*tim = time of cash flows given as periods
*cf = corresponding cash flows
-}
virr :: DVec -> DVec -> Maybe Double
virr tim cf = Op.newtRP (npvT tim cf) 0.0

{-|@sharpe rf ra sa = (ra - rf)/sa@

Sharpe ratio where:

*rf = risk free rate
*ra = rate of return of portfolio 'a'
*sa = std dev of portfolio 'a'
-}
sharpe :: Double -> Double -> Double -> Double
sharpe rf ra sa = (ra - rf)/sa

