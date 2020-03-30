{-# LANGUAGE NumericUnderscores #-}



module Main where


import Finz ((~=))

import qualified Data.Map as M 
import qualified Finz as F

-- import Test.Hspec
import Test.QuickCheck 



main :: IO ()
main = do

    print "Float equality test"
    quickCheck $ 0.0 ~= (-1e-8)
    quickCheck $ 0.0 ~= (-0.0)
    quickCheck $ 0.0 ~= 0.000
    quickCheck $ 1e+7 ~= 10_000_000.05

    putStr "timeMul "
    quickCheck $ F.timeMul (0.06/12) (-120) ~= 0.54963273336

    putStr "timeMulSub "
    quickCheck $ F.timeMulSub 0.06 (-10) 12 ~= 0.54963273336

    putStr "Rate "; quickCheck $ F.rate 7.35 8.52 5 ~= (-0.02911107103)
    putStr "Period"; quickCheck $ F.period 100 50 0.07 ~= 10.244768351058712

    print "PV "
    quickCheck $ F.pv 10_000_000 0.09 5 ~= 6_499_313.862983452
    quickCheck $ F.pvm 12_704_891.6109538 0.06 4 12 ~= 10_000_000
    quickCheck $ F.pvc 11_735.108709918102 0.08 2 ~= 10_000

    print "FV "
    quickCheck $ F.fv 6_499_313.862983452 0.09 5 ~= 10_000_000
    quickCheck $ F.fvm 10_000_000 0.06 4 12 ~= 12_704_891.6109538
    quickCheck $ F.fvc 10_000 0.08 2 ~= 11_735.108709918102

    print "PV Annuity "
    quickCheck $ F.pvAnnuity 7.33764573879378 0.08 30 12 ~= 1000
    quickCheck $ F.pvAnnuityCont 100 0.05 ~= 2000
    quickCheck $ F.fvAnnuity 2000 0.24 5 3 ~= 54_304.2278549568
    quickCheck $ F.pmt 1000 0.08 30 12 ~= 7.33764573879378
    quickCheck $ F.fmt 54_304.2278549568 0.24 5 3 ~= 2000

    quickCheck $ F.pv (F.pvAnnuity 1e+6 0.05 30 1) 0.05 9 ~= 9_909_218.99605011

    print "Effective rates"
    quickCheck $ F.effectiveRate 0.08 2 ~= 0.0816
    quickCheck $ F.effectiveRateCont 0.08 ~= 0.08328706767495864