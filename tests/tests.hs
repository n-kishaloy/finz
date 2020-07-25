{-# LANGUAGE NumericUnderscores, OverloadedStrings #-}



module Main where


import Utilz.Numeric ((=~),(/~),dot,(+^), (-^), (*^), (/^), DVec)
import qualified Utilz.Numeric as Nu

import qualified Data.Vector.Unboxed as U

import qualified Finz as F
import qualified Finz.Statements as S
import Finz.Statements (HasStatuz(..), HasChk (..), HasShk(..)) -- pollution
import Finz.Statements (BsTyp (..), PlTyp (..), CfTyp (..), GetRecords (..)) 
-- pollution

import Data.Time (Day, fromGregorian)

import qualified Data.HashMap.Strict as Hm
import Data.HashMap.Strict ((!), lookupDefault)
import qualified Data.ByteString.Char8 as C

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

  print $ ("Hi", ys)
  print $ ("Hi", xs)

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

  print bs

  let xs = bs
  let bs = xs !!+ (CurrentLoans, 10.0)

  let xs = bs
  let bs = xs !!+ (CurrentAdvances, 25.0)
  print bs

  print $ bs^.S.rec.at S.Cash
  quickCheck $ (bs^.S.rec) ! Cash =~ 24.45
  quickCheck $ bs !!> CurrentReceivables =~ 0.0
  quickCheck $ bs !!> Cash =~ 24.45
  quickCheck $ bs !!> CurrentAdvances =~ 25.0
  quickCheck $ bs !!> CurrentLoans =~ 44.56
  quickCheck $ bs !!? CurrentNotesPayable == Nothing
  quickCheck $ bs !!? Cash  =~ Just 24.45


  let pl = S.ProfitLoss {
      S.profitLossBeginDate = (fromGregorian 2018 3 31),
      S.profitLossEndDate = (fromGregorian 2018 3 31),
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
      S.cashFlowBeginDate = (fromGregorian 2018 3 31),
      S.cashFlowEndDate = (fromGregorian 2018 3 31),
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

  print cf

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

  print cf

  quickCheck $ ((S.recToList cf) :: [(CfTyp,Double)]) =~ [
    (DisPpe,22.25),(CfInvestmentDividends,78.58),
    (OtherCfInvestments,68.58),(CashFlowOperations,38.35)]

  print $ "Check the Fintypes"
  quickCheck $ Cash =~ Cash
  quickCheck $ Cash /~ CurrentAdvances
  quickCheck $ OperatingRevenue =~ OperatingRevenue
  quickCheck $ CashFlowFinancing /~ CashFlowInvestments

  quickCheck $ S.bsTypToString S.Cash == ("Cash"::C.ByteString)
  quickCheck $ S.bsTypToString S.CommonStock /= ("ComonStock"::C.ByteString)
  quickCheck $ S.bsTypToString S.BondsPayable == ("BondsPayable"::C.ByteString)

  quickCheck $ S.bsStringToTyp ("Cash"::C.ByteString) == Just S.Cash
  quickCheck $ S.bsStringToTyp ("ComonStock"::C.ByteString) == Nothing
  quickCheck $ S.bsStringToTyp ("BondsPayable"::C.ByteString) == Just S.BondsPayable

  quickCheck $ S.plTypToString S.OtherIncome == ("OtherIncome"::C.ByteString)
  quickCheck $ S.plTypToString S.Pbtx /= ("Pat"::C.ByteString)
  quickCheck $ S.plTypToString S.Pat == ("Pat"::C.ByteString)

  quickCheck $ S.plStringToTyp ("OtherIncome"::C.ByteString) == Just S.OtherIncome
  quickCheck $ S.plStringToTyp ("OthIncome"::C.ByteString) == Nothing
  quickCheck $ S.plStringToTyp ("Pat"::C.ByteString) == Just S.Pat

  quickCheck $ S.cfTypToString S.Fcfd == ("Fcfd"::C.ByteString)
  quickCheck $ S.cfTypToString S.Fcfe /= ("Fcff"::C.ByteString)
  quickCheck $ S.cfTypToString S.DisPpe == ("DisPpe"::C.ByteString)

  quickCheck $ S.cfStringToTyp ("Fcfd"::C.ByteString) == Just S.Fcfd
  quickCheck $ S.cfStringToTyp ("FcFd"::C.ByteString) == Nothing
  quickCheck $ S.cfStringToTyp ("DisPpe"::C.ByteString) == Just S.DisPpe


  print $ "Bye"

