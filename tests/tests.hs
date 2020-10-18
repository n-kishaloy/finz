{-# LANGUAGE NumericUnderscores, OverloadedStrings, Strict #-}



module Main where


import Finance.Utilities.Numeric (dot,(+^), (-^), (*^), (/^), DVec)
import qualified Finance.Utilities.Numeric as Nu
import qualified Finance.Utilities.Numeric.Optima as Op
import Data.Approx

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import qualified Finance.Base as F
import qualified Finance.Statements as S
import qualified Finance.Bonds as B
import Finance.Statements (HasStatuz(..), HasChk (..), HasShk(..)) -- pollution
import Finance.Statements (BsTyp (..), PlTyp (..), CfTyp (..), GetRecords (..)) 
import Finance.Statements (GetAccountz(..), GetStatementz (..)  )
-- pollution
import Finance.Statements (HasRec(..), HasBalanceSheetBegin (..), HasBalanceSheetEnd (..), HasDateBegin (..), HasDatez (..))
import Finance.Statements (BalanceSheet (..), ProfitLoss (..), CashFlow (..))
import Finance.Statements (eqlRec, notEqlRec, maybeEqlRec, notMaybeEqlRec)

import Data.Time (Day, fromGregorian)

import qualified Data.HashMap.Strict as Hm
import Data.HashMap.Strict ((!), lookupDefault)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens ((^.),(.~),(&),at)

-- import Test.Hspec
import Test.QuickCheck 



main :: IO ()
main = do

  print ""; print $ "Newton Raphson soln"
  quickCheck $ Op.newtRaph (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
  quickCheck $ Op.newtRaph (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
  quickCheck $ Op.newtRP (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
  quickCheck $ Op.newtRP (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667

  print ""; print $ "Vector maths"

  let v1 = U.fromList [1.2,3.4,4.5] :: U.Vector Double
  let v2 = U.fromList [2.5,3.6,1.2] :: U.Vector Double

  quickCheck $ v1 `dot` v2 =~ 20.64

  quickCheck $ v1 +^ v2 =~ U.fromList [3.7,7.0,5.7]
  quickCheck $ v1 +^ v2 /~ U.fromList [3.75,7.0,5.7]

  quickCheck $ v1 -^ v2 =~ U.fromList [-1.3,-0.2,3.3]
  quickCheck $ v1 -^ v2 /~ U.fromList [-1.3,-0.2,3.2]

  quickCheck $ v1 *^ (-2.0) =~ U.fromList [-2.4,-6.8,-9.0]

  quickCheck $ v1 /^ 0.5 =~ U.fromList [2.4,6.8,9.0]

  quickCheck $ Nu.interp (U.fromList [2.0,3.0]) (U.fromList [10.0,15.0]) 0.25 =~   (U.fromList [4.0,6.0])

  quickCheck $ Nu.grad (\x -> (x U.! 0) - (x U.! 1)*5.0 + (x U.! 2)**2) (U.fromList [5.0,2.0,-4.0]) =~ U.fromList [1.0,-5.0,-8.0]

  quickCheck $ Nu.negGrad (\x -> (x U.! 0) - (x U.! 1)*5.0 + (x U.! 2)**2) (U.fromList [5.0,2.0,-4.0]) =~ U.fromList [-1.0,5.0,8.0]

  quickCheck $ (Op.bPhase (
    \x -> (((x U.! 0)-2.5)**2.0/25.0 + ((x U.! 1)-4.5)**2.0/100.0)) 
      (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5])) =~ Just (0.4096,1.6384)

  quickCheck $ ((Op.lineSearch (
    \x -> (((x U.! 0)-2.5)**2.0/25.0 + ((x U.! 1)-4.5)**2.0/100.0)) 
    (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5])) 0.512 2.048) =~ (Just $ U.fromList [2.323530954719283,3.0882345226403585])

  quickCheck $ (Op.lineOptima (
    \x -> (((x U.! 0)-2.5)**2.0/25.0 + ((x U.! 1)-4.5)**2.0/100.0)) 
      (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5])) =~ (Just $ U.fromList [2.3235300091300894,3.0882349954349553])

  quickCheck $ Op.conjGradPR (
    \x -> ((x U.! 0) - 3.0)**4.0 + ((x U.! 1) - 4.0)**2.0 + ((x U.! 2) - 2.0)**2.0 + ((x U.! 2) - 2.0)**4.0 + 10.0 ) (U.fromList [4.2,2.0,0.75]) =~ (Just $ U.fromList [2.971601975980278,3.999995704367093,1.999991592551973])

  quickCheck $ F.timeMul (-120) (0.06/12) =~ 0.54963273336

  quickCheck $ F.timeMulSub (-10) 12 0.06 =~ 0.54963273336

  quickCheck $ F.rate 7.35 8.52 5 =~ (-0.02911107103)
  quickCheck $ F.period 100 50 0.07 =~ 10.244768351058712

  -- print $ "Year calculations"
  -- print $ F.dayToYear (fromGregorian 2020 11 5)

  print "PV "
  quickCheck $ F.pv 10_000_000 5 0.09 =~ 6_499_313.862983452
  quickCheck $ F.pvm 12_704_891.6109538 4 12 0.06 =~ 10_000_000
  quickCheck $ F.pvc 11_735.108709918102 2 0.08 =~ 10_000
  
  quickCheck $ F.xpv ((fromGregorian 2020 8 31), 258) (fromGregorian 2019 2 25) 0.08 =~ 229.6228419865806

  print "FV "
  quickCheck $ F.fv 6_499_313.862983452 5 0.09 =~ 10_000_000
  quickCheck $ F.fvm 10_000_000 4 12 0.06 =~ 12_704_891.6109538
  quickCheck $ F.fvc 10_000 2 0.08 =~ 11_735.108709918102

  print "PV Annuity "
  quickCheck $ F.pvAnnuity 7.33764573879378 30 12 0.08 =~ 1000
  quickCheck $ F.pvAnnuityCont 100 0.05 =~ 2000
  quickCheck $ F.fvAnnuity 2000 5 3 0.24 =~ 54_304.2278549568
  quickCheck $ F.pmt 1000 30 12 0.08 =~ 7.33764573879378
  quickCheck $ F.fmt 54_304.2278549568 5 3 0.24 =~ 2000

  quickCheck $ F.pv (F.pvAnnuity 1e+6 30 1 0.05) 9 0.05 =~ 9_909_218.99605011

  print "Effective rates"
  quickCheck $ F.effectiveRate 2 0.08 =~ 0.0816
  quickCheck $ F.effectiveRateCont 0.08 =~ 0.08328706767495864

  print "NPV calculations"

  quickCheck $ F.npv (U.fromList [0.25,6.25,3.5,4.5,1.25]) (U.fromList [-6.25,1.2,1.25,3.6,2.5]) (-0.45) 0.08 =~ 0.36962283798505946

  quickCheck $ F.npvT (U.fromList [0.25,6.25,3.5,4.5,1.25]) (U.fromList [-6.25,1.2,1.25,3.6,2.5]) 0.08 =~ 0.3826480347907877

  quickCheck $ F.npvN (U.fromList [1000,2000,4000,5000,6000]) (-1.45) 0.05 =~ 14_709.923338335733

  quickCheck $ F.npvNT (U.fromList [-2,0.5,0.75,1.35]) 0.1 =~ 0.08865514650638584726040

  quickCheck $ F.irr (U.fromList [-2.0, 0.5, 0.75, 1.35]) =~ (Just 0.12129650314520722)

  quickCheck $ F.virr (U.fromList [0.125,0.29760274,0.49760274,0.55239726,0.812671233]) (U.fromList [-10.25,-2.5,3.5,9.5,1.25]) =~ (Just  0.31813386476788824)

  let y0 = (`F.yearFrac` (fromGregorian 2018 2 12))
  quickCheck $ y0 (fromGregorian 2026 2 12) =~ 8.0

  let cf = 
        (F.listToDTrend 
          [ fromGregorian 2019 8 25, fromGregorian 2020 2 18
          , fromGregorian 2020 7 25, fromGregorian 2020 8 31
          , fromGregorian 2020 9 28
          ] [-6000, 825, 125, 5698, 1589])

  quickCheck $ (F.xnpv cf (fromGregorian 2019 2 25) 0.08) =~ 1_578.89009324049

  quickCheck $ F.xirr cf =~ (Just 0.38474739096115906)

  let cf = 
        ( F.listToDTrend
            [ (fromGregorian 1995 1 12), (fromGregorian 1998 3 28)
            , (fromGregorian 1999 2 15), (fromGregorian 1999 8 22)
            , (fromGregorian 2000 5 25), (fromGregorian 2003 9 16)
            , (fromGregorian 2004 11 18)
            ] [ -1900.00, 256.00, 825.00, 965.00, 158.00, 874.00, 2158.00 ]
        )

  quickCheck $ F.xnpv cf (fromGregorian 1994 8 5) 0.08 =~ 1105.0039750472524

  quickCheck $ F.xirr cf =~ Just 0.1607722112422215


  print "TWRR"
  quickCheck $ B.twrr (U.fromList [4,6,5.775,6.72,5.508])(U.fromList [1,-0.5,0.225,-0.6]) =~ 0.06159232319186159

  quickCheck $ B.twrrN 1.0 (U.fromList [100, 112, 142.64]) (U.fromList [0,20]) =~ 0.21027878787878795

  print "T-Bills"
  quickCheck $ B.tBillR 150 98_000 100_000 =~ 0.048
  quickCheck $ B.tBillD 0.048 150 100_000 =~ 2_000
  quickCheck $ B.holdPerYd 98 95 5 =~ 0.020408163265306145
  quickCheck $ B.moneyMktYd 150 98 95 5 =~ 0.04897959183673475

  print $ "Sharpe"
  quickCheck $ F.sharpe 1.58 9.26 22.36 =~ 0.3434704830053667

  print $ "Balance Sheets"
  
  -- let ch = S.Checker 5 "Funny"
  -- print $ ch^.statuz

  let ch = S.Checker 5 
  let sh = S.Shaker 7
  let cs = S.CheckShake ch sh 3 9

  let xs = cs&(S.chk.S.statuz) .~ 42
  let ys = ch&S.statuz .~ 23

  -- print $ (cs^.S.chk.S.statuz, xs^.S.chk.S.statuz, cs^.shk.statuz, cs^.statuz, cs^.S.checker)

  -- print $ ("Hi", ys)
  -- print $ ("Hi", xs)

  print $ "Check the Fintypes"
  quickCheck $ Cash =~ Cash
  quickCheck $ Cash /~ CurrentAdvances
  quickCheck $ OperatingRevenue =~ OperatingRevenue
  quickCheck $ CashFlowFinancing /~ CashFlowInvestments
  
  quickCheck $ S.stringToTyp ("Cash"::Text) == Just S.Cash
  quickCheck $ S.stringToTyp ("ComonStock"::Text) == (Nothing::Maybe BsTyp) 
  quickCheck $ S.stringToTyp ("BondsPayable"::Text) == Just S.BondsPayable

  quickCheck $ S.stringToTyp ("OtherIncome"::Text) == Just S.OtherIncome
  quickCheck $ S.stringToTyp ("OthIncome"::Text) == (Nothing::Maybe PlTyp)
  quickCheck $ S.stringToTyp ("Pat"::Text) == Just S.Pat

  quickCheck $ S.stringToTyp ("Fcfd"::Text) == Just S.Fcfd
  quickCheck $ S.stringToTyp ("FcFd"::Text) == (Nothing::Maybe CfTyp)
  quickCheck $ S.stringToTyp ("DisPpe"::Text) == Just S.DisPpe

  print $ "Balance Sheet"

  let bs = S.BalanceSheet {
      S.balanceSheetDatez = (fromGregorian 2018 3 31),
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (Cash,                30.45)
          ,   (CurrentReceivables,  80.56) 
          ]
    }

  let xs = bs & S.datez .~ (fromGregorian 2018 3 30) 
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
  quickCheck $ (bs^.S.rec) ! Cash =~ 24.45
  quickCheck $ bs !!> CurrentReceivables =~ 0.0
  quickCheck $ bs !!> Cash =~ 24.45
  quickCheck $ bs !!> CurrentAdvances =~ 25.0
  quickCheck $ bs !!> CurrentLoans =~ 44.56
  quickCheck $ bs !!? CurrentNotesPayable == Nothing
  quickCheck $ bs !!? Cash  =~ Just 24.45

  let pl = S.ProfitLoss {
      S.profitLossDateBegin = (fromGregorian 2018 3 31),
      S.profitLossDateEnd = (fromGregorian 2018 3 31),
      S.profitLossStatuz = S.Unset,
      S.profitLossRec = 
        Hm.fromList [ 
                (S.OperatingRevenue,    58.35)
            ,   (S.OtherExpenses,       41.58) 
          ]
    }

  quickCheck $ pl !!> OperatingRevenue =~ 58.35
  quickCheck $ pl !!> Pat =~ 0.0
  quickCheck $ pl !!? OperatingRevenue =~ Just 58.35
  quickCheck $ pl !!? Pat == Nothing

  let cf = S.CashFlow {
      S.cashFlowDateBegin = (fromGregorian 2018 3 31),
      S.cashFlowDateEnd = (fromGregorian 2018 3 31),
      S.cashFlowStatuz = S.Unset,
      S.cashFlowRec = 
        Hm.fromList [ 
              (S.CashFlowOperations,  38.35)
          ,   (S.OtherCfInvestments,  48.58) 
        ]
    }

  let xs = cf
  let cf = xs `addToItems` [
          (DisPpe, 23.58), (OtherCfInvestments, 20.00)
          ]

  -- print cf

  quickCheck $ cf !!> CashFlowOperations =~ 38.35
  quickCheck $ cf !!> CashFlowInvestments =~ 0.0
  quickCheck $ cf !!? CashFlowOperations =~ Just 38.35
  quickCheck $ cf !!? CashFlowInvestments == Nothing
  quickCheck $ cf !!> DisPpe =~ 23.58
  quickCheck $ cf !!> OtherCfInvestments =~ 68.58

  let xs = cf
  let cf = xs `updateItems` [
          (DisPpe, 22.25), (CfInvestmentDividends, 78.58)
          ]

  -- print cf

  quickCheck $ ((S.recToList cf) :: [(CfTyp,Double)]) =~ [
    (DisPpe,22.25),(CfInvestmentDividends,78.58),
    (OtherCfInvestments,68.58),(CashFlowOperations,38.35)]

  print "Accountz"

  let acz = S.Accountz {
      S.accountzDateBegin = (fromGregorian 2018 3 31)
    , S.accountzDateEnd   = (fromGregorian 2019 3 31)
    , S.accountzBalanceSheetBegin = Nothing
    , S.accountzBalanceSheetEnd = 
      Just $ Hm.fromList [
        (Cash,                30.45)
      , (CurrentReceivables,  80.65)
      ]
    , S.accountzProfitLoss = 
      Just $ Hm.fromList [
        (S.OperatingRevenue,    58.35)
      , (S.OtherExpenses,       41.58) 
      ]
    , S.accountzCashFlow = 
      Just $ Hm.fromList [ 
        (S.CashFlowOperations,  38.35)
      , (S.OtherCfInvestments,  48.58) 
      ]
    }

  -- print acz

  quickCheck $ (acz !>> Cash) =~ Just 30.45
  quickCheck $ (acz !>> CurrentLeasesLiability) =~ Just 0.0
  quickCheck $ (acz !^> Cash) =~ Nothing
  quickCheck $ (acz !^> OperatingRevenue) =~ Just 58.35
  quickCheck $ (acz !>> Pat) =~ Just 0.0

  let xz = acz !>~  [ 
          (Cash,                24.45)
        , (CurrentLoans,        34.56) 
        ]

  -- print "xz = "; print xz

  quickCheck $ (xz !>> Cash) =~ Just 24.45
  quickCheck $ (xz !>> OperatingRevenue) =~ Just 58.35

  quickCheck $ (xz `addToBeginItems` [(Cash, 2.34), (AccumulatedDepreciation, 5.67)]) =~ Nothing
  quickCheck $ (xz !^+ (Cash, 2.34)) =~ Nothing
  quickCheck $ (xz `updateBeginItems` [(Cash, 2.34), (AccumulatedDepreciation, 5.67)]) =~ Nothing
  quickCheck $ (xz !^% (Cash, 2.34)) =~ Nothing
  quickCheck $ (S.balShBegin xz) =~ Nothing

  let acz = xz !^~ [
            (Cash,                          88.68)
          , (AccumulatedDepreciation,       52.47) 
          ]

  -- print "acz = "; print acz
  let pz = acz

  let Just acz = do  -- Using do notation but not so useful
        x <- (!>+ (Cash, 10.0)) pz
        x <- (!>+ (CurrentAdvances, 15.0)) x
        x <- (!>+ (OperatingRevenue, -5.0)) x
        return x
  
  let Just acz = Just pz >>= (!>+ (Cash, 10.0)) >>= 
        (!>+ (CurrentAdvances, 15.0)) >>= 
        (!>+ (OperatingRevenue, -5.0)) >>=
        (!>+ (CashFlowOperations, 15.0)) >>=
        (!>+ (CashFlowInvestments, 25.0)) >>=
        (!^+ (Cash, 1.32)) >>=
        (!^+ (Cash, 0.00)) >>=
        (!^+ (CurrentPayables, 0.00)) >>=
        (!^+ (CurrentAdvances, 32.5)) >>=
        (!^+ (CashFlowInvestments, 5.0))

  quickCheck $ acz !^> Cash =~ Just 90.0
  quickCheck $ acz !>> Cash =~ Just 34.45
  quickCheck $ acz !^> CurrentLoans =~ Just 0.0
  quickCheck $ acz !^> OperatingRevenue =~ Just 53.35

  -- print "acz = "; print acz

  let Just xz = (acz `addToBeginItems` [(Cash, 10.0), (CurrentReceivables, 15.0)]) >>= 
        (`addToBeginItems` [(OperatingRevenue, -3.35), (Pat, 2.58)]) >>=
        (`addToBeginItems` [(Cash, 15.0), (AccumulatedDepreciation, -2.47)]) >>=
        (`subToBeginItems` [(CashFlowFinancing, -12.0), (CashFlowInvestments, 5.0)]) >>=
        (`addToEndItems` [(Cash, 0.55), (CurrentAdvances, -2.5), (AccumulatedDepreciation, 12.7)]) >>=
        (`addToEndItems` [(CashFlowFinancing, -2.0), (Fcff, 2.73)]) >>=
        (`addToEndItems` [(OperatingRevenue, -15.0), (Pbitda, 3.58)])

  quickCheck $ xz !>> Cash =~ Just 35.0
  quickCheck $ xz !^> Cash =~ Just 115.0
  quickCheck $ xz !^> OperatingRevenue =~ Just 35.0
  quickCheck $ xz !>> OtherExpenses =~ Just 41.58
  quickCheck $ xz !^> Pat =~ Just 2.58
  quickCheck $ xz !>> CashFlowFinancing =~ Just 10.0
  quickCheck $ xz !^> Pbitx =~ Just 0.0
  quickCheck $ xz !^> AccumulatedDepreciation =~ Just 50.0

  -- print "xz = "; print xz

  let Just acz = Just xz >>= (!>% (Cash, 15)) >>= (!^% (Cash, 34.0)) >>=
        (!^% (AccumulatedAmortization, 45.0)) >>=
        (!>% (AccumulatedOci, 24.25)) >>=
        (!^% (AccumulatedOci, 22.56)) >>=
        (!^% (OperatingRevenue, 93.57)) >>=
        (!^% (Pbt, 23.65)) >>=
        (!^% (Fcfd, 15.89)) >>=
        (!>% (CashFlowFinancing, -3.50))

  let Just xz = (acz `updateBeginItems` [(Pat, 22.65), (Depreciation, 56.58)]) >>=
        (`updateEndItems` [(CurrentLoans, 78.02), (IntangibleAssets, 65.43)]) >>=
        (`subToEndItems` [(Cash, 5.0), (AccumulatedOci, 1.0)]) >>=
        (`updateEndItems` [(NonOperatingRevenue, 67.65), (Amortization, 54.32)]) >>=
        (`updateBeginItems` [(OtherCfInvestments, 84.56), (StockSales, 22.54)])

  quickCheck $ xz !>> Cash =~ Just 10.0
  quickCheck $ xz !^> Cash =~ Just 34.0
  quickCheck $ xz !>> StockSales =~ Just 22.54
  quickCheck $ xz !^> Fcfd =~ Just 15.89
  quickCheck $ xz !>> AccumulatedOci =~ Just 23.25
  quickCheck $ xz !^> DisPpe =~ Just 0.0
  quickCheck $ xz !^> AccumulatedOci =~ Just 22.56
  quickCheck $ xz !>> OperatingRevenue =~ Just 93.57

  -- print "xz = "; print xz

  let 
    b1 = Just $ S.BalanceSheet {
      S.balanceSheetDatez = (fromGregorian 2018 3 31),
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (Cash,                30.45)
          ,   (CurrentReceivables,  80.56) 
          ]
    }
 
    b2 = Just $ S.BalanceSheet {
      S.balanceSheetDatez = (fromGregorian 2019 3 31),
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (Cash,                22.96)
          ,   (CurrentReceivables,  90.88) 
          ]
    }
 
    pl = Just $ S.ProfitLoss {
      S.profitLossDateBegin = (fromGregorian 2018 3 31),
      S.profitLossDateEnd = (fromGregorian 2019 3 31),
      S.profitLossStatuz = S.Unset,
      S.profitLossRec = 
        Hm.fromList [ 
                (S.OperatingRevenue,    58.35)
            ,   (S.OtherExpenses,       41.58) 
          ]
    }

    cf = Just $ S.CashFlow {
      S.cashFlowDateBegin = (fromGregorian 2018 3 31),
      S.cashFlowDateEnd = (fromGregorian 2019 3 31),
      S.cashFlowStatuz = S.Unset,
      S.cashFlowRec = 
        Hm.fromList [ 
              (S.CashFlowOperations,  38.35)
          ,   (S.OtherCfInvestments,  48.58) 
        ]
    }

  let Just mka = S.mkAccountz b1 b2 pl cf
  -- print $ "spl"; print $ S.splitAccountz mka

  let Just ska = mka `updateBeginStatement` S.BalanceSheet {
      S.balanceSheetDatez = (fromGregorian 2018 3 31),
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (S.CurrentAdvances,                 87.58)
          ,   (S.AccountPayables,                 25.98) 
          ]
    }

  quickCheck $ (ska !^> S.CurrentAdvances) =~ Just 87.58
  quickCheck $ (ska !^> S.Cash) =~ Just 0.0
  quickCheck $ (ska !>> S.Cash) =~ Just 22.96

  let Just ska = mka `updateEndStatement` S.BalanceSheet {
      S.balanceSheetDatez = (fromGregorian 2019 3 31),
      S.balanceSheetStatuz = S.Unset,
      S.balanceSheetRec = 
        Hm.fromList [ 
              (S.CurrentAdvances,                 87.58)
          ,   (S.AccountPayables,                 25.98) 
          ]
    }

  quickCheck $ (ska !>> S.CurrentAdvances) =~ Just 87.58
  quickCheck $ (ska !>> S.Cash) =~ Just 0.0
  quickCheck $ (ska !^> S.Cash) =~ Just 30.45


  let Just ska = mka `updateStatement` S.ProfitLoss {
      S.profitLossDateBegin = (fromGregorian 2018 3 31),
      S.profitLossDateEnd = (fromGregorian 2019 3 31),
      S.profitLossStatuz = S.Unset,
      S.profitLossRec = 
        Hm.fromList [ 
              (S.OperatingRevenue,                62.58)
          ,   (S.Pat,                             12.57) 
          ]
    }

  quickCheck $ (ska !^> S.OperatingRevenue) =~ Just 62.58

  let Just ska = mka `updateBeginStatement` S.CashFlow {
      S.cashFlowDateBegin = (fromGregorian 2018 3 31),
      S.cashFlowDateEnd = (fromGregorian 2019 3 31),
      S.cashFlowStatuz = S.Unset,
      S.cashFlowRec = 
        Hm.fromList [ 
              (S.CashFlowInvestments,               63.45)
          ,   (S.Fcfd,                              72.12) 
          ]
    }

  quickCheck $ (ska !^> S.Fcfd) =~ Just 72.12

  -- print "mka"; print mka
  -- print "ska"; print ska

  print "JSON"
  let (Just b1, Just b2, Just pl, Just cf) = S.splitAccountz xz

  let b1j = S.recToJSON $ b1 ^. rec
  let b1jMod = T.replace "34.0" "-57.58" b1j
  let Just b1Rec = S.jsonToRec b1jMod

  quickCheck $ Hm.lookup Cash b1Rec =~ Just (-57.58)

  let b1jMod = T.replace "Cash" "Csah" b1j
  quickCheck $ (S.jsonToRec b1jMod :: Maybe S.BsMap) == Nothing

  let plj = S.recToJSON $ pl ^. rec
  let Just plRec = S.jsonToRec $ T.replace "54.32" "-28.78" plj 

  quickCheck $ Hm.lookup Amortization plRec =~ Just (-28.78)
  quickCheck $ (S.jsonToRec $ T.replace "Pbt" "Ptb" plj :: Maybe S.PlMap) == Nothing

  let cfj = S.recToJSON $ cf ^. rec
  let Just cfRec = S.jsonToRec $ T.replace "54.32" "-28.78" cfj 
  
  -- print "cfj"; print $ cfj
  quickCheck $ Hm.lookup CashFlowInvestments cfRec =~ Just 25.0
  quickCheck $ (S.jsonToRec $ T.replace "Fcff" "Fcfr" cfj :: Maybe S.CfMap) == Nothing

  let pz = xz & balanceSheetBegin .~ Nothing
  -- print "pz = "; print pz

  let jz = S.accountzToJson pz
  -- print "pz json ="; print jz

  -- let gz = T.replace "cashFlow" "csahFl" jz

  quickCheck $ (S.jsonToAccountz $ T.replace "cashFlow" "csahFl" $ jz)=~ Nothing
  quickCheck $ (S.jsonToAccountz $ T.replace "Fcfd" "FcFd" $ jz) =~ Nothing
  quickCheck $ ((S.jsonToAccountz $ T.replace "Fcfd" "Fcfe" $ jz) >>= \x -> x !^> Fcfe) =~ Just 15.89

  quickCheck $ ((S.jsonToAccountz $ T.replace "3.58" "-8.95" $ jz) >>= \x -> x !^> Pbitda) =~ Just (-8.95)
  quickCheck $ (S.jsonToAccountz $ T.replace "3.58" "-8.9x5" $ jz) =~ Nothing
  quickCheck $ pz ^. balanceSheetBegin == Nothing

  print $ "Equality of records / accountz etc"

  let rb = Hm.fromList [(Cash, 10.23), (CurrentAdvances, 56.25), (AccountPayables, 0.0)] :: S.BsMap
  let tb = Hm.fromList [(Cash, 10.23), (CurrentAdvances, 56.25)] :: S.BsMap

  quickCheck $ rb `eqlRec` tb
  quickCheck $ tb `eqlRec` rb

  let rb = Hm.fromList [(Cash, 10.23), (AccumulatedDepreciation, 56.25), (AccountPayables, 0.0)] :: S.BsMap

  quickCheck $ rb `notEqlRec` tb
  quickCheck $ tb `notEqlRec` rb

  let pz = xz; tz = xz
  quickCheck $ pz =~ xz

  let Just pz = xz !^% (Cash, 34.0)
  quickCheck $ pz =~ xz

  let Just pz = xz !^% (Pbt, 34.0)
  quickCheck $ pz /~ xz

  let Just pz = xz !^% (Fcfe, 2.65)
  quickCheck $ pz /~ xz

  let Just pz = xz !^% (Fcfe, 0.0)
  quickCheck $ pz =~ xz

  let pz = xz & balanceSheetBegin .~ Nothing
  let az = xz & balanceSheetEnd .~ Nothing
  let bz = xz & balanceSheetBegin .~ Nothing
  quickCheck $ pz /~ xz
  quickCheck $ pz =~ bz
  quickCheck $ az /~ bz

  let xz = pz & dateBegin .~ (let Just x = S.getEOMonth "2018-03-15" in x)
  quickCheck $ pz =~ xz
  quickCheck $ xz =~ xz

  let xz = pz & dateBegin .~ (let Just x = S.getEOMonth "2018-05-15" in x)
  quickCheck $ pz /~ xz

  let (bBg, Just bEd, Just pl, Just cf) = S.splitAccountz xz

  quickCheck $ bBg =~ Nothing

  let b1 = bEd
  quickCheck $ b1 =~ bEd

  let b1 = bEd & datez .~ (fromGregorian 2015 03 31)
  quickCheck $ b1 /~ bEd

  -- print $ b1
  quickCheck $ b1 /~ (b1 !!% (Cash,34.0))
  quickCheck $ b1 =~ (b1 !!% (Cash,10.0))

  let p1 = pl
  quickCheck $ p1 =~ pl

  let p1 = pl !!+ (Salaries, -5.0)
  quickCheck $ p1 /~ pl
  let p1 = pl & dateBegin .~ (fromGregorian 2015 03 31)
  quickCheck $ p1 /~ pl

  let c1 = cf
  quickCheck $ cf =~ c1

  let c1 = cf !!+ (Fcfe, 2.3)
  quickCheck $ c1 /~ cf
  let c1 = cf & dateBegin .~ (fromGregorian 2015 03 31)
  quickCheck $ c1 /~ cf

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
          (ChgInventories, 34.34)
        , (ChgReceivables, 23.21)
        , (Fcfe, 0.0)
        ]

  -- print $ "cz = "; print $ cz

  -- print $ "cz = "; print $ S.cleanAccountz cz



  let xz = S.jsonToAccountz "{\"dateBegin\":\"2007-03-31\",\"dateEnd\":\"2008-03-31\",\"balanceSheetBegin\":null,\"balanceSheetEnd\":{\"OtherCurrentLiabilities\":67620.0,\"RawMaterials\":24220.0,\"AccountReceivables\":11310.0,\"Cash\":23970.0,\"PlantPropertyEquipment\":108310.0,\"LongTermLoans\":62810.0,\"CommonStock\":3860.0,\"LongTermInvestmentsMv\":48080.0,\"AccumulatedDepreciation\":54440.0,\"RetainedEarnings\":74540.0,\"OtherLongTermAssets\":-3910.0,\"WorkInProgress\":50650.0,\"CurrentInvestmentsMv\":49100.0,\"CurrentPayables\":48470.0},\"profitLoss\":{\"Pbitda\":26580.0,\"OperatingRevenue\":285380.0,\"OtherExpenses\":20704.0,\"Depreciation\":6520.0,\"Interest\":4260.0,\"CostMaterial\":183748.0,\"DirectExpenses\":15528.0,\"Pbt\":25760.0,\"Pat\":20290.0,\"OtherIncome\":9960.0,\"Salaries\":12940.0,\"TaxesCurrent\":5409.6},\"cashFlow\":{\"ChgInvestments\":-8150.0,\"InvestmentsCapDevp\":2090.0,\"CfInvestmentInterest\":1260.0,\"CfInvestmentDividends\":1440.0,\"DebtRepay\":-28310.0,\"OtherCfInvestments\":3940.0,\"CashFlowOperations\":61650.0,\"InterestFin\":-5670.0,\"InvestmentsLoans\":-530.0,\"OtherCfFinancing\":19700.0,\"DebtIssue\":32330.0,\"InvestmentsPpe\":-50360.0,\"Dividends\":-6750.0,\"AcqEquityAssets\":-6940.0}}" 


  let cz = S.jsonToAccountz "{\"dateBegin\":\"2008-03-31\",\"dateEnd\":\"2009-03-31\",\"balanceSheetBegin\":{\"OtherCurrentLiabilities\":67620.0,\"RawMaterials\":24220.0,\"AccountReceivables\":11310.0,\"Cash\":23970.0,\"PlantPropertyEquipment\":108310.0,\"LongTermLoans\":62810.0,\"CommonStock\":3860.0,\"LongTermInvestmentsMv\":48080.0,\"AccumulatedDepreciation\":54440.0,\"RetainedEarnings\":74540.0,\"OtherLongTermAssets\":-3910.0,\"WorkInProgress\":50650.0,\"CurrentInvestmentsMv\":49100.0,\"CurrentPayables\":48470.0},\"balanceSheetEnd\":{\"OtherCurrentLiabilities\":70860.0,\"RawMaterials\":22300.0,\"AccountReceivables\":12060.0,\"Cash\":11420.0,\"PlantPropertyEquipment\":139050.0,\"LongTermLoans\":131660.0,\"CommonStock\":5140.0,\"LongTermInvestmentsMv\":61080.0,\"AccumulatedDepreciation\":62600.0,\"RetainedEarnings\":117160.0,\"OtherLongTermAssets\":-11430.0,\"WorkInProgress\":69470.0,\"CurrentInvestmentsMv\":129680.0,\"CurrentPayables\":46200.0},\"profitLoss\":{\"Pbitda\":11560.0,\"OperatingRevenue\":251500.0,\"OtherExpenses\":21594.6,\"Depreciation\":8750.0,\"Interest\":8110.0,\"CostMaterial\":177555.6,\"DirectExpenses\":16795.8,\"Pbt\":10140.0,\"Pat\":10010.0,\"OtherIncome\":15430.0,\"Salaries\":14396.400000000001,\"TaxesCurrent\":101.4},\"cashFlow\":{\"ChgInvestments\":15620.0,\"CfInvestmentInterest\":1370.0,\"CfInvestmentDividends\":4580.0,\"StockSales\":41100.0,\"DebtRepay\":-31790.0,\"OtherCfInvestments\":-1470.0,\"CashFlowOperations\":12840.0,\"InterestFin\":1210.0,\"DebtIssue\":76960.0,\"InvestmentsPpe\":-124430.0,\"Dividends\":-6420.0,\"AcqEquityAssets\":-1510.0}}"

  -- print $ "xz = "; print $ xz
  -- print $ "cz = "; print $ cz

  print $ "Bye"

