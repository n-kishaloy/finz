{-# LANGUAGE NumericUnderscores, OverloadedStrings, Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Main where


import Numeric.Utils (dot,(+^), (-^), (*^), (/^))
import qualified Numeric.Utils as Nu
import qualified Numeric.Optima as Op
import Data.Approx ( Approx((=~), (/~)) )

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import qualified Finance.Base as F
import qualified Finance.Statements as S
import qualified Finance.Bonds as B
 -- pollution
import Finance.Statements (BsTyp (..), PlTyp (..), CfTyp (..), GetRecords (..)) 
import Finance.Statements (GetAccount (..), GetStatement (..)  )
-- pollution
import Finance.Statements (HasRec(..), HasBalanceSheetBegin (..), HasProfitLoss (..), HasBalanceSheetEnd (..), HasDateBegin (..), HasDatez (..), HasDocs (..))
import Finance.Statements (eqlRec, notEqlRec)

import Data.Time (fromGregorian)

import qualified Data.HashMap.Strict as Hm
import Data.HashMap.Strict ((!))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing)

import Control.Lens ((^.),(.~),(&),(?~))

import Data.List (foldl')

qTest :: Show a => a -> [Bool] -> IO ()
qTest nam x = putStrLn $ lJus 30 '.' nam ++" => Tests :"++ rJus 3 ' ' (p+f) ++fl f where
  (p,f)=foldl' (\(u,v) b -> if b then (u+1,v) else (u,v+1)) (0,0) x :: (Int,Int)
  fl 0 = " => Ok"
  fl z = " => +++++ << FAILED : " ++ show z ++ " >> +++++" 
  lJus n c xr = st ++ replicate (n - length st) c where st = show xr
  rJus n c xr = replicate (n - length st) c ++ st where st = show xr 

infix 3 `qCheck`
qCheck :: Show a => a -> Bool -> IO ()
qCheck nam False = putStrLn $ "*** Error *** : " ++ show nam ++ "\n"
qCheck _ _ = putStr ""

main :: IO ()
main = do

  ("Newton Raphson"::String) `qTest` 
    [ Op.newtRaph (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
    , Op.newtRaph (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
    , Op.newtRP (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
    , Op.newtRP (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
    , Op.newtRaph (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
    , Op.newtRaph (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
    , Op.newtRP (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
    , Op.newtRP (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
    ]

  let v1 = U.fromList [1.2,3.4,4.5] :: U.Vector Double
  let v2 = U.fromList [2.5,3.6,1.2] :: U.Vector Double

  ("Vector maths"::String) `qTest` 
    [ v1 `dot` v2 =~ 20.64

    , v1 +^ v2 =~ U.fromList [3.7,7.0,5.7]
    , v1 +^ v2 /~ U.fromList [3.75,7.0,5.7]

    , v1 -^ v2 =~ U.fromList [-1.3,-0.2,3.3]
    , v1 -^ v2 /~ U.fromList [-1.3,-0.2,3.2]

    , v1 *^ (-2.0) =~ U.fromList [-2.4,-6.8,-9.0]

    , v1 /^ 0.5 =~ U.fromList [2.4,6.8,9.0]

    , Nu.interp (U.fromList [2.0,3.0]) (U.fromList [10.0,15.0]) 0.25 =~ U.fromList [4.0,6.0]

    , Nu.grad (\x -> (x U.! 0) - (x U.! 1)*5.0 + (x U.! 2)**2) (U.fromList [5.0,2.0,-4.0]) =~ U.fromList [1.0,-5.0,-8.0]

    , Nu.negGrad (\x -> (x U.! 0) - (x U.! 1)*5.0 + (x U.! 2)**2) (U.fromList [5.0,2.0,-4.0]) =~ U.fromList [-1.0,5.0,8.0]
    ]

  ("Optima"::String) `qTest` 
    [ Op.bPhase (
    \x -> ((x U.! 0)-2.5)**2.0/25.0 + ((x U.! 1)-4.5)**2.0/100.0) 
      (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5]) =~ Just (0.4096,1.6384)

    , Op.lineSearch (
    \x -> ((x U.! 0)-2.5)**2.0/25.0 + ((x U.! 1)-4.5)**2.0/100.0)
    (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5]) 0.512 2.048 =~ Just (U.fromList [2.323530954719283,3.0882345226403585])

    , Op.lineOptima (
    \x -> ((x U.! 0)-2.5)**2.0/25.0 + ((x U.! 1)-4.5)**2.0/100.0)
      (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5]) =~ Just (U.fromList [2.3235300091300894,3.0882349954349553])

    , Op.conjGradPR (
    \x -> ((x U.! 0) - 3.0)**4.0 + ((x U.! 1) - 4.0)**2.0 + ((x U.! 2) - 2.0)**2.0 + ((x U.! 2) - 2.0)**4.0 + 10.0 ) (U.fromList [4.2,2.0,0.75]) =~ Just (U.fromList [2.9959575,4.0,2.0])
    ]

  ("Time calculations"::String) `qTest` 
    [ F.timeMul (-120) (0.06/12) =~ 0.54963273336

    , F.timeMulSub (-10) 12 0.06 =~ 0.54963273336

    , F.rate 7.35 8.52 5 =~ (-0.02911107103)
    , F.period 100 50 0.07 =~ 10.244768351058712
    ]

  ("PV"::String) `qTest` 
    [ F.pv 10_000_000 5 0.09 =~ 6_499_313.862983452
    , F.pvm 12_704_891.6109538 4 12 0.06 =~ 10_000_000
    , F.pvc 11_735.108709918102 2 0.08 =~ 10_000
  
    , F.xpv (fromGregorian 2020 8 31, 258) (fromGregorian 2019 2 25) 0.08 =~ 229.6228419865806

    , F.fv 6_499_313.862983452 5 0.09 =~ 10_000_000
    , F.fvm 10_000_000 4 12 0.06 =~ 12_704_891.6109538
    , F.fvc 10_000 2 0.08 =~ 11_735.108709918102

    , F.pvAnnuity 7.33764573879378 30 12 0.08 =~ 1000
    , F.pvAnnuityCont 100 0.05 =~ 2000
    , F.fvAnnuity 2000 5 3 0.24 =~ 54_304.2278549568
    , F.pmt 1000 30 12 0.08 =~ 7.33764573879378
    , F.fmt 54_304.2278549568 5 3 0.24 =~ 2000

    , F.pv (F.pvAnnuity 1e+6 30 1 0.05) 9 0.05 =~ 9_909_218.99605011
    ]

  ("Effective rates"::String) `qTest` 
    [ F.effectiveRate 2 0.08 =~ 0.0816
    , F.effectiveRateCont 0.08 =~ 0.08328706767495864
    ]

  ("NPV"::String) `qTest` 
    [ F.npv (U.fromList [0.25,6.25,3.5,4.5,1.25]) (U.fromList [-6.25,1.2,1.25,3.6,2.5]) (-0.45) 0.08 =~ 0.36962283798505946

    , F.npvT (U.fromList [0.25,6.25,3.5,4.5,1.25]) (U.fromList [-6.25,1.2,1.25,3.6,2.5]) 0.08 =~ 0.3826480347907877

    , F.npvN (U.fromList [1000,2000,4000,5000,6000]) (-1.45) 0.05 =~ 14_709.923338335733

    , F.npvNT (U.fromList [-2,0.5,0.75,1.35]) 0.1 =~ 0.08865514650638584726040
    ]

  ("IRR"::String) `qTest` 
    [ F.irr (U.fromList [-2.0, 0.5, 0.75, 1.35]) =~ Just 0.12129650314520722

    , F.virr (U.fromList [0.125,0.29760274,0.49760274,0.55239726,0.812671233]) (U.fromList [-10.25,-2.5,3.5,9.5,1.25]) =~ Just  0.31813386476788824
    ]

  let y0 = (`F.yearFrac` fromGregorian 2018 2 12)

  let cf = 
        F.listToDTrend 
        [ fromGregorian 2019 8 25, fromGregorian 2020 2 18
        , fromGregorian 2020 7 25, fromGregorian 2020 8 31
        , fromGregorian 2020 9 28
        ] [-6000, 825, 125, 5698, 1589]

  let ct = F.listToDTrend
        [ fromGregorian 1995 1 12, fromGregorian 1998 3 28
        , fromGregorian 1999 2 15, fromGregorian 1999 8 22
        , fromGregorian 2000 5 25, fromGregorian 2003 9 16
        , fromGregorian 2004 11 18
        ] [ -1900.00, 256.00, 825.00, 965.00, 158.00, 874.00, 2158.00 ]
        
  ("XNPV/XIRR"::String) `qTest` 
    [ y0 (fromGregorian 2026 2 12) =~ 8.0
    , F.xnpv cf (fromGregorian 2019 2 25) 0.08 =~ 1_578.89009324049
    , F.xirr cf =~ Just 0.38474739096115906
    , F.xnpv ct (fromGregorian 1994 8 5) 0.08 =~ 1105.0039750472524
    , F.xirr ct =~ Just 0.1607722112422215
    ]

  ("Money market"::String) `qTest` 
    [ B.twrr (U.fromList [4,6,5.775,6.72,5.508])(U.fromList [1,-0.5,0.225,-0.6]) =~ 0.06159232319186159

    , B.twrrN 1.0 (U.fromList [100, 112, 142.64]) (U.fromList [0,20]) =~ 0.21027878787878795

    , B.tBillR 150 98_000 100_000 =~ 0.048
    , B.tBillD 0.048 150 100_000 =~ 2_000
    , B.holdPerYd 98 95 5 =~ 0.020408163265306145
    , B.moneyMktYd 150 98 95 5 =~ 0.04897959183673475
    ]

  ("Sharpe"::String) `qTest` [F.sharpe 1.58 9.26 22.36 =~ 0.3434704830053667]

  let ch = S.Checker 5 
  let sh = S.Shaker 7
  let cs = S.CheckShake ch sh 3 9

  let xs = cs&(S.chk.S.statuz) .~ 42
  let ys = ch&S.statuz .~ 23

  -- print $ (cs^.S.chk.S.statuz, xs^.S.chk.S.statuz, cs^.shk.statuz, cs^.statuz, cs^.S.checker)

  -- print $ ("Hi", ys)
  -- print $ ("Hi", xs)

  ("FinTypes"::String) `qTest` 
    [ Cash =~ Cash
    , Cash /~ CurrentAdvances
    , OperatingRevenue =~ OperatingRevenue
    , CashFlowFinancing /~ InterestFin
    ]
  
  ("String to Types"::String) `qTest` 
    [ S.stringToTyp ("Cash"::Text) == Just S.Cash
    , S.stringToTyp ("ComonStock"::Text) == (Nothing::Maybe BsTyp) 
    , S.stringToTyp ("BondsPayable"::Text) == Just S.BondsPayable

    , S.stringToTyp ("OtherIncome"::Text) == Just S.OtherIncome
    , S.stringToTyp ("OthIncome"::Text) == (Nothing::Maybe PlTyp)
    , S.stringToTyp ("GainsLossesActurial"::Text) == Just S.GainsLossesActurial

    , S.stringToTyp ("Fcfd"::Text) == Just S.Fcfd
    , S.stringToTyp ("FcFd"::Text) == (Nothing::Maybe CfTyp)
    , S.stringToTyp ("DisPpe"::Text) == Just S.DisPpe
    ]


  ("Calc checks"::String) `qTest`
    [ S.isCalc Cash =~ False
    , S.isCalc Pat =~ True 

    ]

  -- print $ "Balance Sheet"

  let bs = S.BalanceSheet {
      S.balanceSheetDatez = fromGregorian 2018 3 31,
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (Cash,                30.45)
          ,   (CurrentReceivables,  80.56) 
          ]
    }

  let xs = bs & S.datez .~ fromGregorian 2018 3 30
  let bs = xs !!~ [ 
                      (Cash,                24.45)
                  ,   (CurrentLoans,        34.56) 
                  ]

  -- print bs

  let xs = bs
  let bs = xs !!+ (CurrentLoans, 10.0)

  let xs = bs
  let bs = xs !!+ (CurrentAdvances, 25.0)
  -- print bs

  -- print $ bs^.S.rec.at S.Cash
  ("Balance Sheet"::String) `qTest` 
    [ (bs^.S.rec) ! Cash =~ 24.45
    , bs !!> CurrentReceivables =~ 0.0
    , bs !!> Cash =~ 24.45
    , bs !!> CurrentAdvances =~ 25.0
    , bs !!> CurrentLoans =~ 44.56
    , isNothing $ bs !!? CurrentNotesPayable 
    , bs !!? Cash  =~ Just 24.45
    ]

  let pl = S.ProfitLoss {
      S.profitLossDateBegin = fromGregorian 2018 3 31,
      S.profitLossDateEnd = fromGregorian 2018 3 31,
      S.profitLossStatuz = S.Unset,
      S.profitLossRec = 
        Hm.fromList [ 
                (S.OperatingRevenue,    58.35)
            ,   (S.OtherExpenses,       41.58) 
          ]
    }

  ("Profit and Loss"::String) `qTest` 
    [ pl !!> OperatingRevenue =~ 58.35
    , pl !!> GainsLossesActurial =~ 0.0
    , pl !!? OperatingRevenue =~ Just 58.35
    , isNothing $ pl !!? GainsLossesActurial
    ]

  let cf = S.CashFlow {
      S.cashFlowDateBegin = fromGregorian 2018 3 31,
      S.cashFlowDateEnd = fromGregorian 2018 3 31,
      S.cashFlowStatuz = S.Unset,
      S.cashFlowRec = 
        Hm.fromList [ 
              (S.OtherCfOperations ,  38.35)
          ,   (S.OtherCfInvestments,  48.58) 
        ]
    }

  let xs = cf
  let cf = xs `addToItems` [
          (DisPpe, 23.58), (OtherCfInvestments, 20.00)
          ]

  -- print cf

  ("Cash Flow"::String) `qTest` 
    [ cf !!> OtherCfOperations  =~ 38.35
    , cf !!> InterestFin =~ 0.0
    , cf !!? OtherCfOperations  =~ Just 38.35
    , isNothing $ cf !!? InterestFin 
    , cf !!> DisPpe =~ 23.58
    , cf !!> OtherCfInvestments =~ 68.58
    ]

  let xs = cf
  let cf = xs `updateItems` [
          (DisPpe, 22.25), (CfInvestmentDividends, 78.58)
          ]

  ("recToList"::String) `qTest` [(S.recToList cf :: [(CfTyp,Double)]) =~ [
    (DisPpe,22.25),(CfInvestmentDividends,78.58),(OtherCfOperations ,38.35),
    (OtherCfInvestments,68.58)]]

  -- print "Account"

  let acz = S.Account {
      S.accountDateBegin = fromGregorian 2018 3 31
    , S.accountDateEnd   = fromGregorian 2019 3 31
    , S.accountBalanceSheetBegin = Nothing
    , S.accountBalanceSheetEnd = 
      Just $ Hm.fromList [
        (Cash,                30.45)
      , (CurrentReceivables,  80.65)
      ]
    , S.accountProfitLoss = 
      Just $ Hm.fromList [
        (S.OperatingRevenue,    58.35)
      , (S.OtherExpenses,       41.58) 
      ]
    , S.accountCashFlow = 
      Just $ Hm.fromList [ 
        (S.OtherCfOperations ,  38.35)
      , (S.OtherCfInvestments,  48.58) 
      ]
    }

  -- print acz

  ("Updating records"::String) `qTest` 
    [ (acz !>> Cash) =~ Just 30.45
    , (acz !>> CurrentLeasesLiability) =~ Just 0.0
    , (acz !^> Cash) =~ Nothing
    , (acz !^> OperatingRevenue) =~ Just 58.35
    , (acz !>> GainsLossesActurial) =~ Just 0.0
    ]

  let yz = acz `S.fromListEnd`  [ 
          (Cash,                24.45)
        , (CurrentLoans,        34.56) 
        -- , (CurrentAssets,      58.25) -- will fail as isCalc == True
        ]

  let xz = acz `S.fromListNoCalcEnd`  [ 
          (Cash,                24.45)
        , (CurrentLoans,        34.56) 
        , (CurrentAssets,      58.25) -- removes this during Cleaning
        ]

  ("fromListNoCalcEnd"::String) `qTest` [ (xz !>> CurrentAssets ) =~ Just 0.0  ]

  -- print "yz = "; print yz
  -- print "xz = "; print xz

  ("Updating records"::String) `qTest` 
    [ (xz !>> Cash) =~ Just 24.45
    , (xz !>> OperatingRevenue) =~ Just 58.35

    , (xz `addToBeginItems` [(Cash, 2.34), (AccumulatedDepreciation, 5.67)]) =~ Nothing
    , (xz !^+ (Cash, 2.34)) =~ Nothing
    , (xz `updateBeginItems` [(Cash, 2.34), (AccumulatedDepreciation, 5.67)]) =~ Nothing
    , (xz !^% (Cash, 2.34)) =~ Nothing
    , S.balShBegin xz =~ Nothing
    ]

  let acz = xz `S.fromListBegin` [
            (Cash,                          88.68)
          , (AccumulatedDepreciation,       52.47) 
          ]

  -- print "acz = "; print acz
  let pz = acz

  let Just acz = do  -- Using do notation but not so useful
        x <- (!>+ (Cash, 10.0)) pz
        x <- (!>+ (CurrentAdvances, 15.0)) x
        (!>+ (OperatingRevenue, -5.0)) x
  
  let Just acz = Just pz >>= (!>+ (Cash, 10.0)) >>= 
        (!>+ (CurrentAdvances, 15.0)) >>= 
        (!>+ (OperatingRevenue, -5.0)) >>=
        (!>+ (OtherCfOperations , 15.0)) >>=
        (!>+ (InterestFin, 25.0)) >>=
        (!^+ (Cash, 1.32)) >>=
        (!^+ (Cash, 0.00)) >>=
        -- (!^+ (CurrentAssets, 0.00)) >>= -- isCalc == True
        -- (!>+ (CurrentAssets, 0.00)) >>= -- isCalc == True
        (!^+ (CurrentPayables, 0.00)) >>=
        (!^+ (CurrentAdvances, 32.5)) >>=
        (!^+ (InterestFin, 5.0))

  ("Updating records"::String) `qTest` 
    [ acz !^> Cash =~ Just 90.0
    , acz !>> Cash =~ Just 34.45
    , acz !^> CurrentLoans =~ Just 0.0
    , acz !^> OperatingRevenue =~ Just 53.35
    ]

  -- print "acz = "; print acz

  let Just xz = (acz `addToBeginItems` [(Cash, 10.0), (CurrentReceivables, 15.0)]) >>= 
        (`addToBeginItems` [(OperatingRevenue, -3.35), (ExciseStaxLevy, 2.58)]) >>=
        (`addToBeginItems` [(Cash, 15.0), (AccumulatedDepreciation, -2.47)]) >>=
        (`subToBeginItems` [(StockRepurchase, -12.0), (InterestFin, 5.0)]) >>=
        (`addToEndItems` [(Cash, 0.55), (CurrentAdvances, -2.5), (AccumulatedDepreciation, 12.7)]) >>=
        (`addToEndItems` [(StockRepurchase, -2.0), (DonorContribution, 2.73)]) >>=
        (`addToEndItems` [(OperatingRevenue, -15.0), (ResearchNDevelopment, 3.58)])

  ("Add List of entries"::String) `qTest` 
    [ xz !>> Cash =~ Just 35.0
    , xz !^> Cash =~ Just 115.0
    , xz !^> OperatingRevenue =~ Just 35.0
    , xz !>> OtherExpenses =~ Just 41.58
    , xz !^> ExciseStaxLevy =~ Just 2.58
    , xz !>> StockRepurchase =~ Just 10.0
    , xz !^> Pbitx =~ Just 0.0
    , xz !^> AccumulatedDepreciation =~ Just 50.0
    ]

  -- print "xz = "; print xz

  let Just acz = Just xz >>= (!>% (Cash, 15)) >>= (!^% (Cash, 34.0)) >>=
        (!^% (AccumulatedAmortization, 45.0)) >>=
        (!>% (AccumulatedOci, 24.25)) >>=
        (!^% (AccumulatedOci, 22.56)) >>=
        (!^% (OperatingRevenue, 93.57)) >>=
        (!^% (TaxesDeferred, 23.65)) >>=
        (!^% (AcqEquityAssets, 15.89)) >>=
        (!>% (StockRepurchase, -3.50))

  let Just xz = (acz `updateBeginItems` [(GainsLossesActurial, 22.65), (Depreciation, 56.58)]) >>=
        (`updateEndItems` [(CurrentLoans, 78.02), (IntangibleAssets, 65.43)]) >>=
        (`subToEndItems` [(Cash, 5.0), (AccumulatedOci, 1.0)]) >>=
        (`updateEndItems` [(NonOperatingRevenue, 67.65), (Amortization, 54.32)]) >>=
        (`updateBeginItems` [(OtherCfInvestments, 84.56), (StockSales, 22.54)])

  ("Update List of entries"::String) `qTest` 
    [ xz !>> Cash =~ Just 10.0
    , xz !^> Cash =~ Just 34.0
    , xz !>> StockSales =~ Just 22.54
    , xz !^> AcqEquityAssets =~ Just 15.89
    , xz !>> AccumulatedOci =~ Just 23.25
    , xz !^> DisPpe =~ Just 0.0
    , xz !^> AccumulatedOci =~ Just 22.56
    , xz !>> OperatingRevenue =~ Just 93.57
    ]

  -- print "xz = "; print xz

  let 
    b1 = Just $ S.BalanceSheet {
      S.balanceSheetDatez = fromGregorian 2018 3 31,
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (Cash,                30.45)
          ,   (CurrentReceivables,  80.56) 
          ]
    }
 
    b2 = Just $ S.BalanceSheet {
      S.balanceSheetDatez = fromGregorian 2019 3 31,
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (Cash,                22.96)
          ,   (CurrentReceivables,  90.88) 
          ]
    }
 
    pl = Just $ S.ProfitLoss {
      S.profitLossDateBegin = fromGregorian 2018 3 31,
      S.profitLossDateEnd = fromGregorian 2019 3 31,
      S.profitLossStatuz = S.Unset,
      S.profitLossRec = 
        Hm.fromList [ 
                (S.OperatingRevenue,    58.35)
            ,   (S.OtherExpenses,       41.58) 
          ]
    }

    cf = Just $ S.CashFlow {
      S.cashFlowDateBegin = fromGregorian 2018 3 31,
      S.cashFlowDateEnd = fromGregorian 2019 3 31,
      S.cashFlowStatuz = S.Unset,
      S.cashFlowRec = 
        Hm.fromList [ 
              (S.OtherCfOperations ,  38.35)
          ,   (S.OtherCfInvestments,  48.58) 
        ]
    }

  let Just mka = S.mkAccount b1 b2 pl cf
  -- print $ "spl"; print $ S.splitAccount mka

  let Just ska = mka `updateBeginStatement` S.BalanceSheet {
      S.balanceSheetDatez = fromGregorian 2018 3 31,
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (S.CurrentAdvances,                 87.58)
          ,   (S.AccountPayables,                 25.98) 
          ]
    }

  qCheck "Err " $ (ska !^> S.CurrentAdvances) =~ Just 87.58
  qCheck "Err " $ (ska !^> S.Cash) =~ Just 0.0
  qCheck "Err " $ (ska !>> S.Cash) =~ Just 22.96

  let Just ska = mka `updateEndStatement` S.BalanceSheet {
      S.balanceSheetDatez = fromGregorian 2019 3 31,
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (S.CurrentAdvances,                 87.58)
          ,   (S.AccountPayables,                 25.98) 
          ]
    }

  qCheck "Err " $ (ska !>> S.CurrentAdvances) =~ Just 87.58
  qCheck "Err " $ (ska !>> S.Cash) =~ Just 0.0
  qCheck "Err " $ (ska !^> S.Cash) =~ Just 30.45


  let Just ska = mka `updateStatement` S.ProfitLoss {
      S.profitLossDateBegin = fromGregorian 2018 3 31,
      S.profitLossDateEnd = fromGregorian 2019 3 31,
      S.profitLossStatuz = S.Unset,
      S.profitLossRec = 
        Hm.fromList [ 
              (S.OperatingRevenue,                62.58)
          ,   (S.GainsLossesActurial,                             12.57) 
          ]
    }

  qCheck "Err " $ (ska !^> S.OperatingRevenue) =~ Just 62.58

  let Just ska = mka `updateBeginStatement` S.CashFlow {
      S.cashFlowDateBegin = fromGregorian 2018 3 31,
      S.cashFlowDateEnd = fromGregorian 2019 3 31,
      S.cashFlowStatuz = S.Unset,
      S.cashFlowRec = 
        Hm.fromList [ 
              (S.InterestFin,               63.45)
          ,   (S.Fcfd,                              72.12) 
          ]
    }

  qCheck "Err " $ (ska !^> S.Fcfd) =~ Just 72.12

  -- print "mka"; print mka
  -- print "ska"; print ska

  -- print "JSON"
  let (Just b1, Just b2, Just pl, Just cf) = S.splitAccount xz

  let b1j = S.recToJSON $ b1 ^. rec
  let b1jMod = T.replace "34.0" "-57.58" b1j
  let Just b1Rec = S.jsonToRec b1jMod

  qCheck "Err " $ Hm.lookup Cash b1Rec =~ Just (-57.58)

  let b1jMod = T.replace "Cash" "Csah" b1j
  qCheck "Err " $ isNothing (S.jsonToRec b1jMod :: Maybe S.BsMap) 

  let plj = S.recToJSON $ pl ^. rec
  let Just plRec = S.jsonToRec $ T.replace "54.32" "-28.78" plj 

  qCheck "Err " $ Hm.lookup Amortization plRec =~ Just (-28.78)
  qCheck "Err " $ isNothing (S.jsonToRec $ T.replace "TaxesDeferred" "Ptb" plj :: Maybe S.PlMap) 

  let cfj = S.recToJSON $ cf ^. rec
  let Just cfRec = S.jsonToRec $ T.replace "54.32" "-28.78" cfj 
  
  -- print "cfj"; print $ cfj
  qCheck "Err 1" $ Hm.lookup InterestFin cfRec =~ Just 25.0
  -- print cfj
  qCheck "Err 2" $ isNothing (S.jsonToRec $ T.replace "StockSales" "Fcfr" cfj :: Maybe S.CfMap) 

  let pz = xz & balanceSheetBegin .~ Nothing
  -- print "pz = "; print pz

  let jz = S.accountToJson pz
  -- print "pz json ="; print jz

  -- let gz = T.replace "cashFlow" "csahFl" jz

  qCheck "Err 3" $ S.jsonToAccount (T.replace "cashFlow" "csahFl" jz)=~ Nothing
  -- print jz
  qCheck "Err 4" $ S.jsonToAccount (T.replace "OtherCfOperations" "FcFd" jz) =~ Nothing
  qCheck "Err 5" $ (S.jsonToAccount (T.replace "AcqEquityAssets" "ChangeLiabilities" $ jz) >>= \x -> x !^> ChangeLiabilities) =~ Just 15.89
  qCheck "Err 6" $ (S.jsonToAccount (T.replace "3.58" "-8.95" $ jz) >>= \x -> x !^> ResearchNDevelopment) =~ Just (-8.95)
  qCheck "Err 7" $ S.jsonToAccount (T.replace "3.58" "-8.9x5" $ jz) =~ Nothing
  qCheck "Err 8" $ isNothing (pz ^. balanceSheetBegin) 



  -- print $ "Equality of records / account etc"

  let rb = Hm.fromList [(Cash, 10.23), (CurrentAdvances, 56.25), (AccountPayables, 0.0)] :: S.BsMap
  let tb = Hm.fromList [(Cash, 10.23), (CurrentAdvances, 56.25)] :: S.BsMap

  qCheck "Err " $ rb `eqlRec` tb
  qCheck "Err " $ tb `eqlRec` rb

  let rb = Hm.fromList [(Cash, 10.23), (AccumulatedDepreciation, 56.25), (AccountPayables, 0.0)] :: S.BsMap

  qCheck "Err " $ rb `notEqlRec` tb
  qCheck "Err " $ tb `notEqlRec` rb

  let pz = xz; tz = xz
  qCheck "Err " $ pz =~ xz

  let Just pz = xz !^% (Cash, 34.0)
  qCheck "Err " $ pz =~ xz

  let Just pz = xz !^% (TaxesDeferred, 34.0)
  qCheck "Err " $ pz /~ xz

  let Just pz = xz !^% (ChangeLiabilities, 2.65)
  qCheck "Err " $ pz /~ xz

  let Just pz = xz !^% (ChangeLiabilities, 0.0)
  qCheck "Err " $ pz =~ xz

  let pz = xz & balanceSheetBegin .~ Nothing
  let az = xz & balanceSheetEnd .~ Nothing
  let bz = xz & balanceSheetBegin .~ Nothing
  qCheck "Err " $ pz /~ xz
  qCheck "Err " $ pz =~ bz
  qCheck "Err " $ az /~ bz

  let xz = pz & dateBegin .~ (let Just x = S.getEOMonth "2018-03-15" in x)
  qCheck "Err " $ pz =~ xz
  qCheck "Err " $ xz =~ xz

  let xz = pz & dateBegin .~ (let Just x = S.getEOMonth "2018-05-15" in x)
  qCheck "Err " $ pz /~ xz

  let (bBg, Just bEd, Just pl, Just cf) = S.splitAccount xz

  qCheck "Err " $ bBg =~ Nothing

  let b1 = bEd
  qCheck "Err " $ b1 =~ bEd

  let b1 = bEd & datez .~ fromGregorian 2015 03 31
  qCheck "Err " $ b1 /~ bEd

  -- print $ b1
  qCheck "Err " $ b1 /~ (b1 !!% (Cash,34.0))
  qCheck "Err " $ b1 =~ (b1 !!% (Cash,10.0))

  let p1 = pl
  qCheck "Err " $ p1 =~ pl

  let p1 = pl !!+ (Salaries, -5.0)
  qCheck "Err " $ p1 /~ pl
  let p1 = pl & dateBegin .~ fromGregorian 2015 03 31
  qCheck "Err " $ p1 /~ pl

  let c1 = cf
  qCheck "Err " $ cf =~ c1

  let c1 = cf !!+ (ChangeLiabilities, 2.3)
  qCheck "Err " $ c1 /~ cf
  let c1 = cf & dateBegin .~ fromGregorian 2015 03 31
  qCheck "Err " $ c1 /~ cf

  -- print $ "xz = "; print $ xz

  let Just cz = xz `updateEndItems` [
          (CurrentReceivables, -89.58)
        , (PlantPropertyEquipment, 45.25)
        , (AccumulatedDepreciation, 45.32)
        , (CurrentPayables, 32.32)
        , (FinishedGoods, 0.0)
        ]  

  let Just xz = cz `updateBeginItems` [
          (OperatingRevenue, 76.34)
        , (CostMaterial, 45.34)
        , (TaxesCurrent, 0.0)
        ]

  let Just cz = xz `updateEndItems` [
          (ChangeInventories, 34.34)
        , (ChangeReceivables, 23.21)
        , (ChangeLiabilities, 0.0)
        ]

  -- print $ "cz = "; print $ cz

  -- print $ "cz = "; print $ S.cleanAccount cz



  let Just xz = S.jsonToAccount "{\"dateBegin\":\"2007-03-31\",\"dateEnd\":\"2008-03-31\",\"balanceSheetBegin\":null,\"balanceSheetEnd\":{\"OtherCurrentLiabilities\":67620.0,\"RawMaterials\":24220.0,\"AccountReceivables\":11310.0,\"Cash\":23970.0,\"PlantPropertyEquipment\":108310.0,\"LongTermLoanAssets\":62810.0,\"CommonStock\":3860.0,\"LongTermInvestments\":48080.0,\"AccumulatedDepreciation\":54440.0,\"RetainedEarnings\":74540.0,\"OtherLongTermAssets\":-3910.0,\"WorkInProgress\":50650.0,\"CurrentInvestments\":49100.0,\"CurrentPayables\":48470.0},\"profitLoss\":{\"Pbitda\":26580.0,\"OperatingRevenue\":285380.0,\"OtherExpenses\":20704.0,\"Depreciation\":6520.0,\"InterestExpense\":4260.0,\"CostMaterial\":183748.0,\"DirectExpenses\":15528.0,\"Pbt\":25760.0,\"Pat\":20290.0,\"OtherIncome\":9960.0,\"Salaries\":12940.0,\"TaxesCurrent\":5409.6},\"cashFlow\":{\"ChangeInvestments\":-8150.0,\"InvestmentsCapDevp\":2090.0,\"CfInvestmentInterest\":1260.0,\"CfInvestmentDividends\":1440.0,\"DebtRepay\":-28310.0,\"OtherCfInvestments\":3940.0,\"CashFlowOperations\":61650.0,\"InterestFin\":-5670.0,\"InvestmentsLoans\":-530.0,\"OtherCfFinancing\":19700.0,\"DebtIssue\":32330.0,\"InvestmentsPpe\":-50360.0,\"Dividends\":-6750.0,\"AcqEquityAssets\":-6940.0}}" 


  let Just cz = S.jsonToAccount "{\"dateBegin\":\"2008-03-31\",\"dateEnd\":\"2009-03-31\",\"balanceSheetBegin\":{\"OtherCurrentLiabilities\":67620.0,\"RawMaterials\":24220.0,\"AccountReceivables\":11310.0,\"Cash\":23970.0,\"PlantPropertyEquipment\":108310.0,\"LongTermLoanAssets\":62810.0,\"CommonStock\":3860.0,\"LongTermInvestments\":48080.0,\"AccumulatedDepreciation\":54440.0,\"RetainedEarnings\":74540.0,\"OtherLongTermAssets\":-3910.0,\"WorkInProgress\":50650.0,\"CurrentInvestments\":49100.0,\"CurrentPayables\":48470.0},\"balanceSheetEnd\":{\"OtherCurrentLiabilities\":70860.0,\"RawMaterials\":22300.0,\"AccountReceivables\":12060.0,\"Cash\":11420.0,\"PlantPropertyEquipment\":139050.0,\"LongTermLoanAssets\":131660.0,\"CommonStock\":5140.0,\"LongTermInvestments\":61080.0,\"AccumulatedDepreciation\":62600.0,\"RetainedEarnings\":117160.0,\"OtherLongTermAssets\":-11430.0,\"WorkInProgress\":69470.0,\"CurrentInvestments\":129680.0,\"CurrentPayables\":46200.0},\"profitLoss\":{\"Pbitda\":11560.0,\"OperatingRevenue\":251500.0,\"OtherExpenses\":21594.6,\"Depreciation\":8750.0,\"InterestExpense\":8110.0,\"CostMaterial\":177555.6,\"DirectExpenses\":16795.8,\"Pbt\":10140.0,\"Pat\":10010.0,\"OtherIncome\":15430.0,\"Salaries\":14396.400000000001,\"TaxesCurrent\":101.4},\"cashFlow\":{\"ChangeInvestments\":15620.0,\"CfInvestmentInterest\":1370.0,\"CfInvestmentDividends\":4580.0,\"StockSales\":41100.0,\"DebtRepay\":-31790.0,\"OtherCfInvestments\":-1470.0,\"CashFlowOperations\":12840.0,\"InterestFin\":1210.0,\"DebtIssue\":76960.0,\"InvestmentsPpe\":-124430.0,\"Dividends\":-6420.0,\"AcqEquityAssets\":-1510.0}}"

  let Just bs0 = cz ^. balanceSheetEnd

  let bs1 = bs0 `S.transactSeries` [
          (LongTermLoanAssets, CurrentPayables, 500)
        , (CurrentPayables, CurrentTaxPayables, 300)
        , (RetainedEarnings, AccumulatedDepreciation, 200)
        , (RawMaterials, Cash, 100)
        , (OtherCurrentLiabilities, Cash, 500)
        , (RetainedEarnings, InterestPayable, 300)

        ]

  qCheck bs1 $ bs1 ! LongTermLoanAssets =~ 132160
  qCheck "Err " $ bs1 ! CurrentPayables =~ 46400
  "Err" `qCheck` bs1 ! CurrentTaxPayables =~ 300
  qCheck "Err " $ bs1 ! Cash =~ 10820
  qCheck "Err " $ bs1 ! RetainedEarnings =~ 116660
  qCheck "Err " $ bs1 ! InterestPayable =~ 300

  let Just bz = (cz & balanceSheetEnd ?~ bs1 `S.transactSeries` [
          (LongTermLoanAssets, CurrentPayables, 200.2578978459)
        , (CurrentPayables, CurrentTaxPayables, 2.578914584)
        , (RetainedEarnings, AccumulatedDepreciation, 3.000000008)

        ]) !^% (ChangeLiabilities, -0.3) >>= (!^% (GainsLossesActurial, -0.03)) >>= (!^% (CommonStock, 0.09))

  let cz = S.cleanAccount bz

  qCheck "cleaner" $ cz !>> LongTermLoanAssets == Just 132360

  let cz = "{\"code\":\"TATAMOTORS\",\"affiliated\":null,\"consolidated\":false,\"docs\":[{\"dateBegin\":\"2007-03-31\",\"dateEnd\":\"2008-03-31\",\"balanceSheetBegin\":null,\"balanceSheetEnd\":{\"LongTermLoanAssets\":62810.0,\"CurrentPayables\":48470.0,\"RawMaterials\":24220.0,\"Cash\":23970.0,\"OtherLongTermAssets\":-3910.0,\"AccountReceivables\":11310.0,\"PlantPropertyEquipment\":108310.0,\"LongTermInvestments\":48080.0,\"OtherCurrentLiabilities\":67620.0,\"AccumulatedDepreciation\":54440.0,\"WorkInProgress\":50650.0,\"CommonStock\":3860.0,\"CurrentInvestments\":49100.0,\"RetainedEarnings\":74540.0},\"profitLoss\":{\"OtherExpenses\":46584.0,\"Pbt\":25760.0,\"Salaries\":12940.0,\"TaxesCurrent\":5409.6,\"OperatingRevenue\":285380.0,\"Depreciation\":6520.0,\"Pbitda\":36540.0,\"Pat\":20350.4,\"CostMaterial\":183748.0,\"OtherIncome\":9960.0,\"DirectExpenses\":15528.0,\"InterestExpense\":4260.0},\"cashFlow\":{\"ChangeInvestments\":-8150.0,\"InvestmentsCapDevp\":2090.0,\"CfInvestmentInterest\":1260.0,\"CfInvestmentDividends\":1440.0,\"DebtRepay\":-28310.0,\"OtherCfInvestments\":3940.0,\"CashFlowOperations\":61650.0,\"InterestFin\":-5670.0,\"InvestmentsLoans\":-530.0,\"OtherCfFinancing\":19700.0,\"DebtIssue\":32330.0,\"InvestmentsPpe\":-50360.0,\"Dividends\":-6750.0,\"AcqEquityAssets\":-6940.0}},{\"dateBegin\":\"2009-03-31\",\"dateEnd\":\"2010-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":131660.0,\"CurrentPayables\":46200.0,\"RawMaterials\":22300.0,\"Cash\":11420.0,\"OtherLongTermAssets\":-11430.0,\"AccountReceivables\":12060.0,\"PlantPropertyEquipment\":139050.0,\"LongTermInvestments\":61080.0,\"OtherCurrentLiabilities\":70860.0,\"AccumulatedDepreciation\":62600.0,\"WorkInProgress\":69470.0,\"CommonStock\":5140.0,\"CurrentInvestments\":129680.0,\"RetainedEarnings\":117160.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":165950.0,\"CurrentPayables\":61160.0,\"RawMaterials\":29360.0,\"Cash\":17530.0,\"OtherLongTermAssets\":-6300.0,\"AccountReceivables\":23920.0,\"PlantPropertyEquipment\":184170.0,\"LongTermInvestments\":52170.0,\"OtherCurrentLiabilities\":127650.0,\"AccumulatedDepreciation\":72130.0,\"WorkInProgress\":52320.0,\"CommonStock\":5710.0,\"CurrentInvestments\":223370.0,\"RetainedEarnings\":143950.0},\"profitLoss\":{\"OtherExpenses\":61710.100000000006,\"Pbt\":28300.0,\"Salaries\":16239.5,\"TaxesCurrent\":5943.0,\"OperatingRevenue\":350250.0,\"Depreciation\":10340.0,\"Pbitda\":25450.0,\"Pat\":22400.0,\"CostMaterial\":224105.09999999998,\"OtherIncome\":25640.0,\"DirectExpenses\":22735.300000000003,\"InterestExpense\":12460.0},\"cashFlow\":{\"ChangeInvestments\":16250.0,\"InvestmentsCapDevp\":160.0,\"CfInvestmentInterest\":1360.0,\"CfInvestmentDividends\":590.0,\"StockSales\":17940.0,\"DebtRepay\":-95110.0,\"OtherCfInvestments\":-6620.0,\"CashFlowOperations\":64000.0,\"InterestFin\":6500.0,\"OtherCfFinancing\":-2030.0,\"DebtIssue\":131490.0,\"InvestmentsPpe\":-129300.0,\"Dividends\":-3450.0,\"AcqEquityAssets\":-1300.0}},{\"dateBegin\":\"2010-03-31\",\"dateEnd\":\"2011-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":165950.0,\"CurrentPayables\":61160.0,\"RawMaterials\":29360.0,\"Cash\":17530.0,\"OtherLongTermAssets\":-6300.0,\"AccountReceivables\":23920.0,\"PlantPropertyEquipment\":184170.0,\"LongTermInvestments\":52170.0,\"OtherCurrentLiabilities\":127650.0,\"AccumulatedDepreciation\":72130.0,\"WorkInProgress\":52320.0,\"CommonStock\":5710.0,\"CurrentInvestments\":223370.0,\"RetainedEarnings\":143950.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":159910.0,\"CurrentPayables\":43770.0,\"RawMaterials\":38910.0,\"Cash\":24290.0,\"OtherLongTermAssets\":27790.0,\"AccountReceivables\":26030.0,\"PlantPropertyEquipment\":218830.0,\"LongTermInvestments\":26470.0,\"OtherCurrentLiabilities\":138130.0,\"AccumulatedDepreciation\":84660.0,\"WorkInProgress\":37990.0,\"CommonStock\":6350.0,\"CurrentInvestments\":226240.0,\"RetainedEarnings\":193760.0},\"profitLoss\":{\"OtherExpenses\":72124.2,\"Pbt\":21970.0,\"Salaries\":21213.0,\"TaxesCurrent\":3954.6,\"OperatingRevenue\":470880.0,\"Depreciation\":13610.0,\"Pbitda\":46630.0,\"Pat\":18120.0,\"CostMaterial\":305467.2,\"OtherIncome\":2780.0,\"DirectExpenses\":25455.6,\"InterestExpense\":13840.0},\"cashFlow\":{\"ChangeInvestments\":3320.0,\"InvestmentsCapDevp\":340.0,\"CfInvestmentInterest\":2020.0,\"CfInvestmentDividends\":1810.0,\"StockSales\":32530.0,\"DebtRepay\":-70400.0,\"OtherCfInvestments\":-440.0,\"CashFlowOperations\":15060.0,\"InterestFin\":-8680.0,\"InvestmentsLoans\":-1740.0,\"OtherCfFinancing\":-3270.0,\"DebtIssue\":76210.0,\"InvestmentsPpe\":-28450.0,\"Dividends\":-9900.0,\"AcqEquityAssets\":-2040.0}},{\"dateBegin\":\"2011-03-31\",\"dateEnd\":\"2012-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":159910.0,\"CurrentPayables\":43770.0,\"RawMaterials\":38910.0,\"Cash\":24290.0,\"OtherLongTermAssets\":27790.0,\"AccountReceivables\":26030.0,\"PlantPropertyEquipment\":218830.0,\"LongTermInvestments\":26470.0,\"OtherCurrentLiabilities\":138130.0,\"AccumulatedDepreciation\":84660.0,\"WorkInProgress\":37990.0,\"CommonStock\":6350.0,\"CurrentInvestments\":226240.0,\"RetainedEarnings\":193760.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":158810.0,\"CurrentPayables\":56150.0,\"RawMaterials\":45880.0,\"Cash\":18410.0,\"OtherLongTermAssets\":25980.0,\"AccountReceivables\":27080.0,\"PlantPropertyEquipment\":249850.0,\"LongTermInvestments\":29760.0,\"OtherCurrentLiabilities\":133980.0,\"AccumulatedDepreciation\":99660.0,\"WorkInProgress\":40370.0,\"CommonStock\":6350.0,\"CurrentInvestments\":204940.0,\"RetainedEarnings\":187330.0},\"profitLoss\":{\"OtherExpenses\":75238.5,\"Pbt\":13410.0,\"Salaries\":25079.5,\"TaxesCurrent\":938.7,\"OperatingRevenue\":543070.0,\"Depreciation\":16070.0,\"Pbitda\":41480.0,\"Pat\":12420.0,\"CostMaterial\":366160.7,\"OtherIncome\":190.0,\"DirectExpenses\":35111.3,\"InterestExpense\":12190.0},\"cashFlow\":{\"ChangeInvestments\":42630.0,\"InvestmentsCapDevp\":160.0,\"CfInvestmentInterest\":3310.0,\"CfInvestmentDividends\":1810.0,\"DebtRepay\":-70170.0,\"OtherCfInvestments\":130.0,\"CashFlowOperations\":36540.0,\"InterestFin\":-14820.0,\"InvestmentsLoans\":-870.0,\"OtherCfFinancing\":-11470.0,\"DebtIssue\":68730.0,\"InvestmentsPpe\":-45200.0,\"Dividends\":-14620.0,\"AcqEquityAssets\":-470.0}},{\"dateBegin\":\"2012-03-31\",\"dateEnd\":\"2013-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":158810.0,\"CurrentPayables\":56150.0,\"RawMaterials\":45880.0,\"Cash\":18410.0,\"OtherLongTermAssets\":25980.0,\"AccountReceivables\":27080.0,\"PlantPropertyEquipment\":249850.0,\"LongTermInvestments\":29760.0,\"OtherCurrentLiabilities\":133980.0,\"AccumulatedDepreciation\":99660.0,\"WorkInProgress\":40370.0,\"CommonStock\":6350.0,\"CurrentInvestments\":204940.0,\"RetainedEarnings\":187330.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":167990.0,\"CurrentPayables\":46980.0,\"RawMaterials\":44550.0,\"Cash\":4630.0,\"OtherLongTermAssets\":20690.0,\"AccountReceivables\":18180.0,\"PlantPropertyEquipment\":270670.0,\"LongTermInvestments\":32370.0,\"OtherCurrentLiabilities\":115530.0,\"AccumulatedDepreciation\":116110.0,\"WorkInProgress\":47530.0,\"CommonStock\":6380.0,\"CurrentInvestments\":199340.0,\"RetainedEarnings\":184970.0},\"profitLoss\":{\"OtherExpenses\":51831.6,\"Pbt\":1750.0,\"Salaries\":25915.8,\"TaxesCurrent\":-1277.5,\"OperatingRevenue\":447660.0,\"Depreciation\":18180.0,\"Pbitda\":15720.0,\"Pat\":3020.0,\"CostMaterial\":319628.2,\"OtherIncome\":18080.0,\"DirectExpenses\":34554.4,\"InterestExpense\":13880.0},\"cashFlow\":{\"ChangeInvestments\":-3050.0,\"InvestmentsCapDevp\":440.0,\"CfInvestmentInterest\":4040.0,\"CfInvestmentDividends\":16610.0,\"DebtRepay\":-123650.0,\"OtherCfInvestments\":7460.0,\"CashFlowOperations\":22580.0,\"InterestFin\":-18090.0,\"InvestmentsLoans\":-1940.0,\"OtherCfFinancing\":-28490.0,\"DebtIssue\":144370.0,\"InvestmentsPpe\":-13020.0,\"Dividends\":-14600.0,\"AcqEquityAssets\":210.0}},{\"dateBegin\":\"2013-03-31\",\"dateEnd\":\"2014-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":167990.0,\"CurrentPayables\":46980.0,\"RawMaterials\":44550.0,\"Cash\":4630.0,\"OtherLongTermAssets\":20690.0,\"AccountReceivables\":18180.0,\"PlantPropertyEquipment\":270670.0,\"LongTermInvestments\":32370.0,\"OtherCurrentLiabilities\":115530.0,\"AccumulatedDepreciation\":116110.0,\"WorkInProgress\":47530.0,\"CommonStock\":6380.0,\"CurrentInvestments\":199340.0,\"RetainedEarnings\":184970.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":150530.0,\"CurrentPayables\":52650.0,\"RawMaterials\":38630.0,\"Cash\":2260.0,\"OtherLongTermAssets\":-8810.0,\"AccountReceivables\":12170.0,\"PlantPropertyEquipment\":287910.0,\"LongTermInvestments\":52560.0,\"OtherCurrentLiabilities\":102400.0,\"AccumulatedDepreciation\":135510.0,\"WorkInProgress\":63550.0,\"CommonStock\":6440.0,\"CurrentInvestments\":184580.0,\"RetainedEarnings\":185330.0},\"profitLoss\":{\"OtherExpenses\":21433.800000000003,\"Pbt\":-10260.0,\"Salaries\":32150.7,\"TaxesCurrent\":-13645.8,\"OperatingRevenue\":342880.0,\"Depreciation\":20700.0,\"Pbitda\":-14350.0,\"Pat\":3350.0,\"CostMaterial\":271494.8,\"OtherIncome\":38330.0,\"DirectExpenses\":32150.7,\"InterestExpense\":13530.0},\"cashFlow\":{\"ChangeInvestments\":44240.0,\"InvestmentsCapDevp\":-400.0,\"CfInvestmentInterest\":1820.0,\"CfInvestmentDividends\":16030.0,\"DebtRepay\":-109120.0,\"OtherCfInvestments\":2410.0,\"CashFlowOperations\":24630.0,\"InterestFin\":-17500.0,\"InvestmentsLoans\":1520.0,\"OtherCfFinancing\":-11080.0,\"DebtIssue\":93850.0,\"InvestmentsPpe\":-36720.0,\"Dividends\":-6490.0,\"AcqEquityAssets\":-3250.0}},{\"dateBegin\":\"2014-03-31\",\"dateEnd\":\"2015-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":150530.0,\"CurrentPayables\":52650.0,\"RawMaterials\":38630.0,\"Cash\":2260.0,\"OtherLongTermAssets\":-8810.0,\"AccountReceivables\":12170.0,\"PlantPropertyEquipment\":287910.0,\"LongTermInvestments\":52560.0,\"OtherCurrentLiabilities\":102400.0,\"AccumulatedDepreciation\":135510.0,\"WorkInProgress\":63550.0,\"CommonStock\":6440.0,\"CurrentInvestments\":184580.0,\"RetainedEarnings\":185330.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":211340.0,\"CurrentPayables\":54010.0,\"RawMaterials\":48020.0,\"Cash\":9450.0,\"OtherLongTermAssets\":-15820.0,\"AccountReceivables\":11140.0,\"PlantPropertyEquipment\":318140.0,\"LongTermInvestments\":58520.0,\"OtherCurrentLiabilities\":85450.0,\"AccumulatedDepreciation\":160310.0,\"WorkInProgress\":60410.0,\"CommonStock\":6440.0,\"CurrentInvestments\":169870.0,\"RetainedEarnings\":142190.0},\"profitLoss\":{\"OtherExpenses\":30096.0,\"Pbt\":-39750.0,\"Salaries\":33858.0,\"TaxesCurrent\":7552.5,\"OperatingRevenue\":363020.0,\"Depreciation\":26030.0,\"Pbitda\":-13180.0,\"Pat\":-47390.0,\"CostMaterial\":278388.0,\"OtherIncome\":15580.0,\"DirectExpenses\":33858.0,\"InterestExpense\":16120.0},\"cashFlow\":{\"ChangeInvestments\":18710.0,\"InvestmentsCapDevp\":-50.0,\"CfInvestmentInterest\":800.0,\"CfInvestmentDividends\":16980.0,\"DebtRepay\":-68900.0,\"OtherCfInvestments\":-480.0,\"CashFlowOperations\":-22140.0,\"InterestFin\":-18450.0,\"OtherCfFinancing\":-8010.0,\"DebtIssue\":128170.0,\"InvestmentsPpe\":-31910.0,\"Dividends\":-6490.0,\"AcqEquityAssets\":-1590.0}},{\"dateBegin\":\"2015-03-31\",\"dateEnd\":\"2016-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":211340.0,\"CurrentPayables\":54010.0,\"RawMaterials\":48020.0,\"Cash\":9450.0,\"OtherLongTermAssets\":-15820.0,\"AccountReceivables\":11140.0,\"PlantPropertyEquipment\":318140.0,\"LongTermInvestments\":58520.0,\"OtherCurrentLiabilities\":85450.0,\"AccumulatedDepreciation\":160310.0,\"WorkInProgress\":60410.0,\"CommonStock\":6440.0,\"CurrentInvestments\":169870.0,\"RetainedEarnings\":142190.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":164730.0,\"CurrentPayables\":58700.0,\"RawMaterials\":51180.0,\"Cash\":7880.0,\"OtherLongTermAssets\":-23520.0,\"AccountReceivables\":20460.0,\"PlantPropertyEquipment\":393050.0,\"LongTermInvestments\":73500.0,\"OtherCurrentLiabilities\":110710.0,\"AccumulatedDepreciation\":182300.0,\"WorkInProgress\":56870.0,\"CommonStock\":6790.0,\"CurrentInvestments\":169630.0,\"RetainedEarnings\":225830.0},\"profitLoss\":{\"OtherExpenses\":76748.59999999999,\"Pbt\":-670.0,\"Salaries\":28275.8,\"TaxesCurrent\":-46.900000000000006,\"OperatingRevenue\":428450.0,\"Depreciation\":23290.0,\"Pbitda\":24520.0,\"Pat\":-620.0,\"CostMaterial\":274679.19999999995,\"OtherIncome\":14020.0,\"DirectExpenses\":24236.399999999998,\"InterestExpense\":15920.0},\"cashFlow\":{\"ChangeInvestments\":-8370.0,\"InvestmentsCapDevp\":-350.0,\"CfInvestmentInterest\":2540.0,\"CfInvestmentDividends\":10420.0,\"StockSales\":74330.0,\"DebtRepay\":-90130.0,\"OtherCfInvestments\":-3010.0,\"CashFlowOperations\":27030.0,\"InterestFin\":-20860.0,\"InvestmentsLoans\":-780.0,\"DebtIssue\":35870.0,\"InvestmentsPpe\":-33100.0}},{\"dateBegin\":\"2008-03-31\",\"dateEnd\":\"2009-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":62810.0,\"CurrentPayables\":48470.0,\"RawMaterials\":24220.0,\"Cash\":23970.0,\"OtherLongTermAssets\":-3910.0,\"AccountReceivables\":11310.0,\"PlantPropertyEquipment\":108310.0,\"LongTermInvestments\":48080.0,\"OtherCurrentLiabilities\":67620.0,\"AccumulatedDepreciation\":54440.0,\"WorkInProgress\":50650.0,\"CommonStock\":3860.0,\"CurrentInvestments\":49100.0,\"RetainedEarnings\":74540.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":131660.0,\"CurrentPayables\":46200.0,\"RawMaterials\":22300.0,\"Cash\":11420.0,\"OtherLongTermAssets\":-11430.0,\"AccountReceivables\":12060.0,\"PlantPropertyEquipment\":139050.0,\"LongTermInvestments\":61080.0,\"OtherCurrentLiabilities\":70860.0,\"AccumulatedDepreciation\":62600.0,\"WorkInProgress\":69470.0,\"CommonStock\":5140.0,\"CurrentInvestments\":129680.0,\"RetainedEarnings\":117160.0},\"profitLoss\":{\"OtherExpenses\":31192.199999999997,\"Pbt\":10140.0,\"Salaries\":14396.400000000001,\"TaxesCurrent\":101.4,\"OperatingRevenue\":251500.0,\"Depreciation\":8750.0,\"Pbitda\":11560.0,\"Pat\":10010.0,\"CostMaterial\":177555.6,\"OtherIncome\":15430.0,\"DirectExpenses\":16795.8,\"InterestExpense\":8110.0},\"cashFlow\":{\"ChangeInvestments\":15620.0,\"CfInvestmentInterest\":1370.0,\"CfInvestmentDividends\":4580.0,\"StockSales\":41100.0,\"DebtRepay\":-31790.0,\"OtherCfInvestments\":-1470.0,\"CashFlowOperations\":12840.0,\"InterestFin\":1210.0,\"DebtIssue\":76960.0,\"InvestmentsPpe\":-124430.0,\"Dividends\":-6420.0,\"AcqEquityAssets\":-1510.0}},{\"dateBegin\":\"2016-03-31\",\"dateEnd\":\"2017-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":164730.0,\"CurrentPayables\":58700.0,\"RawMaterials\":51180.0,\"Cash\":7880.0,\"OtherLongTermAssets\":-23520.0,\"AccountReceivables\":20460.0,\"PlantPropertyEquipment\":393050.0,\"LongTermInvestments\":73500.0,\"OtherCurrentLiabilities\":110710.0,\"AccumulatedDepreciation\":182300.0,\"WorkInProgress\":56870.0,\"CommonStock\":6790.0,\"CurrentInvestments\":169630.0,\"RetainedEarnings\":225830.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":193570.0,\"CurrentPayables\":79330.0,\"RawMaterials\":55530.0,\"Cash\":3270.0,\"OtherLongTermAssets\":-21650.0,\"AccountReceivables\":21280.0,\"PlantPropertyEquipment\":398860.0,\"LongTermInvestments\":76960.0,\"OtherCurrentLiabilities\":104250.0,\"AccumulatedDepreciation\":191130.0,\"WorkInProgress\":72710.0,\"CommonStock\":6790.0,\"CurrentInvestments\":172960.0,\"RetainedEarnings\":204830.0},\"profitLoss\":{\"OtherExpenses\":59952.200000000004,\"Pbt\":-23530.0,\"Salaries\":34258.4,\"TaxesCurrent\":705.9000000000001,\"OperatingRevenue\":443160.0,\"Depreciation\":30370.0,\"Pbitda\":14940.0,\"Pat\":-24300.0,\"CostMaterial\":304043.30000000005,\"OtherIncome\":7590.0,\"DirectExpenses\":29976.100000000002,\"InterestExpense\":15690.0},\"cashFlow\":{\"ChangeInvestments\":-5370.0,\"InvestmentsCapDevp\":200.0,\"CfInvestmentInterest\":2590.0,\"CfInvestmentDividends\":6730.0,\"StockSales\":50.0,\"DebtRepay\":-74730.0,\"OtherCfInvestments\":3610.0,\"CashFlowOperations\":14530.0,\"InterestFin\":-19360.0,\"DebtIssue\":106870.0,\"InvestmentsPpe\":-36360.0,\"Dividends\":-730.0}},{\"dateBegin\":\"2017-03-31\",\"dateEnd\":\"2018-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":193570.0,\"CurrentPayables\":79330.0,\"RawMaterials\":55530.0,\"Cash\":3270.0,\"OtherLongTermAssets\":-21650.0,\"AccountReceivables\":21280.0,\"PlantPropertyEquipment\":398860.0,\"LongTermInvestments\":76960.0,\"OtherCurrentLiabilities\":104250.0,\"AccumulatedDepreciation\":191130.0,\"WorkInProgress\":72710.0,\"CommonStock\":6790.0,\"CurrentInvestments\":172960.0,\"RetainedEarnings\":204830.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":184640.0,\"CurrentPayables\":94110.0,\"RawMaterials\":56700.0,\"Cash\":7950.0,\"OtherLongTermAssets\":-17500.0,\"AccountReceivables\":34800.0,\"PlantPropertyEquipment\":431660.0,\"LongTermInvestments\":74530.0,\"OtherCurrentLiabilities\":111660.0,\"AccumulatedDepreciation\":215620.0,\"WorkInProgress\":51970.0,\"CommonStock\":6790.0,\"CurrentInvestments\":167640.0,\"RetainedEarnings\":194920.0},\"profitLoss\":{\"OtherExpenses\":77876.40000000001,\"Pbt\":-9470.0,\"Salaries\":38938.200000000004,\"TaxesCurrent\":852.3000000000001,\"OperatingRevenue\":578970.0,\"Depreciation\":31020.0,\"Pbitda\":22700.0,\"Pat\":-10350.0,\"CostMaterial\":411632.39999999997,\"OtherIncome\":16290.0,\"DirectExpenses\":27813.0,\"InterestExpense\":17440.0},\"cashFlow\":{\"ChangeInvestments\":10260.0,\"InvestmentsCapDevp\":600.0,\"CfInvestmentInterest\":3990.0,\"CfInvestmentDividends\":10550.0,\"DebtRepay\":-74100.0,\"OtherCfInvestments\":-1110.0,\"CashFlowOperations\":41340.0,\"InterestFin\":-20980.0,\"DebtIssue\":64060.0,\"InvestmentsPpe\":-30950.0,\"Dividends\":-30.0,\"AcqEquityAssets\":-440.0}},{\"dateBegin\":\"2018-03-31\",\"dateEnd\":\"2019-03-31\",\"balanceSheetBegin\":{\"LongTermLoanAssets\":184640.0,\"CurrentPayables\":94110.0,\"RawMaterials\":56700.0,\"Cash\":7950.0,\"OtherLongTermAssets\":-17500.0,\"AccountReceivables\":34800.0,\"PlantPropertyEquipment\":431660.0,\"LongTermInvestments\":74530.0,\"OtherCurrentLiabilities\":111660.0,\"AccumulatedDepreciation\":215620.0,\"WorkInProgress\":51970.0,\"CommonStock\":6790.0,\"CurrentInvestments\":167640.0,\"RetainedEarnings\":194920.0},\"balanceSheetEnd\":{\"LongTermLoanAssets\":186400.0,\"CurrentPayables\":104090.0,\"RawMaterials\":46620.0,\"Cash\":13070.0,\"OtherLongTermAssets\":-15650.0,\"AccountReceivables\":32510.0,\"PlantPropertyEquipment\":453970.0,\"LongTermInvestments\":78150.0,\"OtherCurrentLiabilities\":96990.0,\"AccumulatedDepreciation\":231100.0,\"WorkInProgress\":62870.0,\"CommonStock\":6790.0,\"CurrentInvestments\":168670.0,\"RetainedEarnings\":214830.0},\"profitLoss\":{\"OtherExpenses\":103464.0,\"Pbt\":23990.0,\"Salaries\":38799.0,\"TaxesCurrent\":3838.3999999999996,\"OperatingRevenue\":692030.0,\"Depreciation\":30990.0,\"Pbitda\":45370.0,\"Pat\":20210.0,\"CostMaterial\":472054.5,\"OtherIncome\":27540.0,\"DirectExpenses\":32332.5,\"InterestExpense\":17940.0},\"cashFlow\":{\"ChangeInvestments\":9520.0,\"InvestmentsCapDevp\":-20.0,\"CfInvestmentInterest\":3270.0,\"CfInvestmentDividends\":15690.0,\"DebtRepay\":-89770.0,\"OtherCfInvestments\":-10760.0,\"CashFlowOperations\":62930.0,\"InterestFin\":-23550.0,\"DebtIssue\":88050.0,\"InvestmentsPpe\":-55910.0,\"Dividends\":-30.0}}],\"shareprices\":null,\"rate\":null,\"beta\":null}"  :: Text


  let Just tz1 = S.jsonToCompany cz
  let _cz1 = S.companyToJson tz1
  -- print (S.jsonToCompany cz1) 

  -- print $ "cz = "; print $ S.sortAccountVec (tz1 ^. docs)
  let Just acz = S.sortCheckCompany tz1

  ("Sort Company docs"::String) `qTest`
    [ not $ S.checkCompany tz1
    , (S.checkCompany . S.sortCompanyDocs) tz1
    , acz =~ S.sortCompanyDocs tz1
    ]

  let a0 = (acz ^. docs) V.! 0
  let a1 = (acz ^. docs) V.! 1

  -- print $ a0  

  -- -- print $ let Just x = S.setAccount a0 in x ^. profitLoss

  -- print $ S.setAccount a0

  -- print acz

  -- print $ S.setCompany acz

  putStrLn "Bye"

