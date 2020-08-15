{-# LANGUAGE NumericUnderscores, OverloadedStrings, Strict #-}



module Main where


import Utilz.Numeric ((=~),(/~),dot,(+^), (-^), (*^), (/^), DVec)
import qualified Utilz.Numeric as Nu

import qualified Data.Vector.Unboxed as U

import qualified Finz as F
import qualified Finz.Statements as S
import Finz.Statements (HasStatuz(..), HasChk (..), HasShk(..)) -- pollution
import Finz.Statements (BsTyp (..), PlTyp (..), CfTyp (..), GetRecords (..)) 
import Finz.Statements (GetAccountz(..), GetStatementz (..)  )
-- pollution

import Data.Time (Day, fromGregorian)

import qualified Data.HashMap.Strict as Hm
import Data.HashMap.Strict ((!), lookupDefault)
import Data.Text (Text)

import Control.Lens ((^.),(.~),(&),at)

-- import Test.Hspec
import Test.QuickCheck 



main :: IO ()
main = do

  putStr "timeMul "
  quickCheck $ F.timeMul (0.06/12) (-120) =~ 0.54963273336

  putStr "timeMulSub "
  quickCheck $ F.timeMulSub 0.06 (-10) 12 =~ 0.54963273336

  putStr "Rate "; quickCheck $ F.rate 7.35 8.52 5 =~ (-0.02911107103)
  putStr "Period"; quickCheck $ F.period 100 50 0.07 =~ 10.244768351058712

  print "PV "
  quickCheck $ F.pv 10_000_000 0.09 5 =~ 6_499_313.862983452
  quickCheck $ F.pvm 12_704_891.6109538 0.06 4 12 =~ 10_000_000
  quickCheck $ F.pvc 11_735.108709918102 0.08 2 =~ 10_000

  print "FV "
  quickCheck $ F.fv 6_499_313.862983452 0.09 5 =~ 10_000_000
  quickCheck $ F.fvm 10_000_000 0.06 4 12 =~ 12_704_891.6109538
  quickCheck $ F.fvc 10_000 0.08 2 =~ 11_735.108709918102

  print "PV Annuity "
  quickCheck $ F.pvAnnuity 7.33764573879378 0.08 30 12 =~ 1000
  quickCheck $ F.pvAnnuityCont 100 0.05 =~ 2000
  quickCheck $ F.fvAnnuity 2000 0.24 5 3 =~ 54_304.2278549568
  quickCheck $ F.pmt 1000 0.08 30 12 =~ 7.33764573879378
  quickCheck $ F.fmt 54_304.2278549568 0.24 5 3 =~ 2000

  quickCheck $ F.pv (F.pvAnnuity 1e+6 0.05 30 1) 0.05 9 =~ 9_909_218.99605011

  print "Effective rates"
  quickCheck $ F.effectiveRate 0.08 2 =~ 0.0816
  quickCheck $ F.effectiveRateCont 0.08 =~ 0.08328706767495864

  print "NPV calculations"

  quickCheck $ F.npv 0.45 (U.fromList [0.25,6.25,3.5,4.5,1.25]) (U.fromList [-6.25,1.2,1.25,3.6,2.5]) 0.08 =~ 0.36962283798505946

  quickCheck $ F.npvT (U.fromList [0.25,6.25,3.5,4.5,1.25]) (U.fromList [-6.25,1.2,1.25,3.6,2.5]) 0.08 =~ 0.3826480347907877

  quickCheck $ F.npvN 1.45 (U.fromList [1000,2000,4000,5000,6000]) 0.05 =~ 14_709.923338335733

  quickCheck $ F.npvNT (U.fromList [-2,0.5,0.75,1.35]) 0.1 =~ 0.08865514650638584726040

  quickCheck $ F.irr (U.fromList [-2.0, 0.5, 0.75, 1.35]) =~ (Just 0.12129650314520722)

  quickCheck $ F.xirr (U.fromList [0.125,0.29760274,0.49760274,0.55239726,0.812671233]) (U.fromList [-10.25,-2.5,3.5,9.5,1.25]) =~ (Just  0.31813386476788824)

  print "TWRR"
  quickCheck $ F.twrr (U.fromList [4,6,5.775,6.72,5.508])(U.fromList [1,-0.5,0.225,-0.6]) =~ 0.06159232319186159

  quickCheck $ F.twrrN 1.0 (U.fromList [100, 112, 142.64]) (U.fromList [0,20]) =~ 0.21027878787878795

  print "T-Bills"
  quickCheck $ F.tBillR 150 98_000 100_000 =~ 0.048
  quickCheck $ F.tBillD 0.048 150 100_000 =~ 2_000
  quickCheck $ F.holdPerYd 98 95 5 =~ 0.020408163265306145
  quickCheck $ F.moneyMktYd 150 98 95 5 =~ 0.04897959183673475

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

  print $ (cs^.S.chk.S.statuz, xs^.S.chk.S.statuz, cs^.shk.statuz, cs^.statuz, cs^.S.checker)

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
  let cf = xs !!++ [
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
  let cf = xs !!%% [
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

  -- print $ xz !^++ [(Cash, 2.34), (AccumulatedDepreciation, 5.67)] -- Nothing
  -- print $ xz !^+ (Cash, 2.34) -- Nothing
  -- print $ xz !^%% [(Cash, 2.34), (AccumulatedDepreciation, 5.67)] -- Nothing
  -- print $ xz !^% (Cash, 2.34) -- Nothing
  -- print $ S.balShBegin xz -- Nothing

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

  let Just xz = (acz !^++ [(Cash, 10.0), (CurrentReceivables, 15.0)]) >>= 
        (!^++ [(OperatingRevenue, -3.35), (Pat, 2.58)]) >>=
        (!^++ [(Cash, 15.0), (AccumulatedDepreciation, -2.47)]) >>=
        (!^++ [(CashFlowFinancing, 12.0), (CashFlowInvestments, -5.0)]) >>=
        (!>++ [(Cash, 0.55), (CurrentAdvances, -2.5), (AccumulatedDepreciation, 12.7)]) >>=
        (!>++ [(CashFlowFinancing, -2.0), (Fcff, 2.73)]) >>=
        (!>++ [(OperatingRevenue, -15.0), (Pbitda, 3.58)])

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
        (!>% (AccumulatedOci, 23.25)) >>=
        (!^% (AccumulatedOci, 22.56)) >>=
        (!^% (OperatingRevenue, 93.57)) >>=
        (!^% (Pbt, 23.65)) >>=
        (!^% (Fcfd, 15.89)) >>=
        (!>% (CashFlowFinancing, -3.50))

  let Just xz = (acz !^%% [(Pat, 22.65), (Depreciation, 56.58)]) >>=
        (!>%% [(CurrentLoans, 78.02), (IntangibleAssets, 65.43)]) >>=
        (!>++ [(Cash, -5.0)]) >>=
        (!>%% [(NonOperatingRevenue, 67.65), (Amortization, 54.32)]) >>=
        (!^%% [(OtherCfInvestments, 84.56), (StockSales, 22.54)])

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

  let Just ska = mka !^%* S.BalanceSheet {
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

  let Just ska = mka !>%* S.BalanceSheet {
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


  let Just ska = mka !^%* S.ProfitLoss {
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

  let Just ska = mka !^%* S.CashFlow {
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

  print $ "Bye"

