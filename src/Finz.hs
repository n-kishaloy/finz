{-# LANGUAGE NumericUnderscores #-}

module Finz 
    (   (~=)
    ,   timeMul
    ,   timeMulSub
    ,   rate
    ,   period
    ,   pv
    ,   pvm
    ,   pvc
    ,   pvAnnuity
    ,   pvAnnuityCont
    ,   fvAnnuity
    ,   pmt 
    ,   fmt 
    ,   fv
    ,   fvm
    ,   fvc 
    ,   effectiveRate
    ,   effectiveRateCont
    ) where


import qualified Data.Vector.Unboxed as V

(~=) :: Double -> Double -> Bool 
(~=) x y = 
    if (mx < 1e-5) || (abs (x-y)) / mx < 1e-7 then True else False where 
        mx = (max (abs x) (abs y))

timeMul r n = (1+r)**n 
timeMulSub r n m = timeMul (r/m) (n*m) 

rate f p n = (f/p)**(1/n) - 1
period f p r = (log (f/p))/(log (1+r))

pv fv r n = fv/(1+r)**n
pvm fv r n m = fv/(1+r/m)**(n*m)
pvc fv r n = fv/(exp (r*n))

pvAnnuity pmt r n m = pmt/(r/m)*(1 - 1/(1 + r/m)**(n*m))
pvAnnuityCont pmt r = pmt/r
fvAnnuity pmt r n m = pmt/(r/m) * ((1 + r/m)**(n*m) - 1)
pmt pv r n m = pv * (r/m) / (1 - 1/(1 + r/m)**(n*m))
fmt fv r n m = fv * (r/m) / ((1 + r/m)**(n*m) - 1)

fv pv r n = pv*(1+r)**n
fvm pv r n m = pv*(1+r/m)**(n*m)
fvc fv r n = fv*(exp (r*n))

effectiveRate r m = (1 + r/m)**m - 1
effectiveRateCont r = (exp r) - 1
