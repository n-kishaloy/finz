{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Strict, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Finz.Statements
( BalanceSheet (..), ProfitLoss (..), CashFlow (..), Statementz (..)
, Accountz (..), GetAccountz (..) --, (!^>)
, BsTyp (..), PlTyp (..), CfTyp (..), Statuz (..)
, BsMap, PlMap, CfMap
, HasStatuz (..), HasRec (..)
, HasDatez (..), HasBeginDate (..), HasEndDate (..)
, Checker (..), Shaker (..), CheckShake (..), HasChk (..), HasShk (..)
, HasChecker(..)
, GetRecords (..)
, GetPSQLArray (..), GetJsonz (..)
, bsStringToTyp, plStringToTyp , cfStringToTyp

) where

import GHC.Generics (Generic)
import Data.Hashable
import Data.Time (Day)
import qualified Data.HashMap.Strict as Hm
import qualified Data.Text as T
import Data.Text (Text)

import Utilz.Numeric (Approx (..))

import Control.Lens
-- import Control.Lens.TH

data Checker = Checker { checkerStatuz :: Int } deriving (Show)
makeFields ''Checker

data Shaker = Shaker { shakerStatuz :: Int } deriving (Show)
makeFields ''Shaker

data CheckShake = CheckShake 
  { checkShakeChk :: Checker
  , checkShakeShk :: Shaker
  , checkShakeStatuz :: Int 
  , checkShakeChecker :: Int
  } deriving (Show)

makeFields ''CheckShake

class FinType a 
class FinStat a

data Statuz = Unset | Actual | Estimated deriving (Show, Eq)

data BsTyp = 
  Cash                          |
  CurrentReceivables            |
  CurrentLoans                  |
  CurrentAdvances               |
  OtherCurrentAssets            |
  CurrentInvestmentsBv          |
  CurrentInvestmentsMv          |
  RawMaterials                  |
  WorkInProgress                |
  FinishedGoods                 |
  AccountReceivables            |
  LongTermLoans                 |
  LongTermAdvances              |
  LongTermInvestmentsBv         |   
  LongTermInvestmentsMv         |
  OtherLongTermAssets           | 
  PlantPropertyEquipment        |
  AccumulatedDepreciation       |
  LeasingRentalAssset           |
  CapitalWip                    |
  OtherTangibleAssets           |  
  IntangibleAssets              |
  IntangibleAssetsDevelopment   |
  AccumulatedAmortization       |   
  Assets                        |  
  CurrentPayables               |
  CurrentBorrowings             |
  CurrentNotesPayable           |
  OtherCurrentLiabilities       |
  InterestPayable               |
  CurrentProvisions             |
  CurrentTaxPayables            |
  LiabilitiesSaleAssets         |
  CurrentLeasesLiability        |
  AccountPayables               |
  LongTermBorrowings            |
  BondsPayable                  |
  DeferredTaxLiabilities        |
  LongTermLeasesLiability       |
  DeferredCompensation          |
  DeferredRevenues              |
  CustomerDeposits              |
  OtherLongTermLiabilities      |
  PensionProvision              |
  LongTermProvisions            |
  Liabilities                   |
  CommonStock                   |
  PreferredStock                |
  PdInCapAbovePar               |
  PdInCapTreasuryStock          |
  RevaluationReserves           |
  Reserves                      |
  RetainedEarnings              |
  AccumulatedOci                |
  MinorityInterests             |
  Equity
  deriving (Eq, Show, Ord, Generic, FinType, Enum, Bounded)

instance Hashable BsTyp

type BsMap = Hm.HashMap BsTyp Double

data BalanceSheet = BalanceSheet 
  { balanceSheetDatez       :: Day
  , balanceSheetStatuz      :: Statuz
  , balanceSheetRec         :: BsMap
  } deriving (Show, FinStat)

makeFields ''BalanceSheet

data PlTyp = 
  OperatingRevenue              |
  NonOperatingRevenue           |
  ExciseStaxLevy                |
  OtherIncome                   |
  CostMaterial                  |
  DirectExpenses                |
  Salaries                      |
  AdministrativeExpenses        |
  ResearchNDevelopment          |
  OtherOverheads                |
  OtherOperativeExpenses        |
  OtherExpenses                 |
  ExceptionalItems              |  
  Pbitda                        |
  Depreciation                  |
  Amortization                  |
  Pbitx                         |
  Interest                      |
  Pbtx                          |
  ExtraordinaryItems            |
  PriorYears                    |
  Pbt                           |
  TaxesCurrent                  |
  TaxesDeferred                 |
  Pat                           |
  GainsLossesForex              |
  GainsLossesActurial           |
  GainsLossesSales              |
  FvChgAvlSale                  |
  OtherDeferredTaxes            |
  OtherComprehensiveIncome      |
  TotalComprehensiveIncome 
  deriving (Eq, Show, Ord, Generic, FinType, Enum, Bounded)

instance Hashable PlTyp
type PlMap = Hm.HashMap PlTyp Double

data ProfitLoss = ProfitLoss
  { profitLossBeginDate     :: Day
  , profitLossEndDate       :: Day
  , profitLossStatuz        :: Statuz
  , profitLossRec           :: PlMap
  } deriving (Show, FinStat)

makeFields ''ProfitLoss

data CfTyp = 
  ChgInventories                |
  ChgReceivables                |
  ChgLiabilities                |
  ChgProvisions                 |
  OtherCfOperations             |
  CashFlowOperations            |
  InvestmentsPpe                |
  InvestmentsCapDevp            |
  InvestmentsLoans              |
  AcqEquityAssets               |
  DisEquityAssets               |
  DisPpe                        |
  ChgInvestments                |
  CfInvestmentInterest          |
  CfInvestmentDividends         |
  OtherCfInvestments            |
  CashFlowInvestments           |
  StockSales                    |
  StockRepurchase               |
  DebtIssue                     |
  DebtRepay                     |
  InterestFin                   |
  Dividends                     |
  DonorContribution             |
  OtherCfFinancing              |
  CashFlowFinancing             |
  NetCashFlow                   |
  Fcff                          |
  Fcfe                          |
  Fcfd                 
  deriving (Eq, Show, Ord, Generic, FinType, Enum, Bounded)

instance Hashable CfTyp
type CfMap = Hm.HashMap CfTyp Double

data CashFlow = CashFlow
  { cashFlowBeginDate       :: Day
  , cashFlowEndDate         :: Day
  , cashFlowStatuz          :: Statuz
  , cashFlowRec             :: CfMap
  } deriving (Show, FinStat)

makeFields ''CashFlow

data Statementz = Statementz
  { statementzBeginDate         :: Day
  , statementzEndDate           :: Day
  , statementzBalanceSheetBegin :: Maybe BalanceSheet
  , statementzBalanceSheetEnd   :: Maybe BalanceSheet
  , statementzProfitLoss        :: Maybe ProfitLoss
  , statementzCashFlow          :: Maybe CashFlow
  } deriving (Show)

makeFields ''Statementz

instance Approx BsTyp where x =~ y = (x == y)
instance Approx PlTyp where x =~ y = (x == y)
instance Approx CfTyp where x =~ y = (x == y)

-- TODO: Add your code here
instance Approx BalanceSheet where 
  x =~ y = undefined 

instance Approx ProfitLoss where 
  x =~ y = undefined

instance Approx CashFlow where 
  x =~ y = undefined

instance Approx Statementz where 
  x =~ y = undefined

class GetRecords a b where
  (!!>) :: (FinStat a, FinType b) => a -> b -> Double         -- Get
  (!!?) :: (FinStat a, FinType b) => a -> b -> Maybe Double   -- Maybe Get
  (!!~) :: (FinStat a, FinType b) => a -> [(b,Double)] -> a   -- Set/Reset 
  (!!+) :: (FinStat a, FinType b) => a -> (b,Double) -> a     -- Add/Create 

  (!!++) :: (FinStat a, FinType b) => a -> [(b,Double)] -> a -- List Add
  (!!++) x [] = x
  (!!++) x (y:ys) = (x !!+ y) !!++ ys

  (!!%) :: (FinStat a, FinType b) => a -> (b,Double) -> a     -- Update/Create

  (!!%%) :: (FinStat a, FinType b) => a -> [(b,Double)] -> a -- List Upd
  (!!%%) x [] = x
  (!!%%) x (y:ys) = (x !!% y) !!%% ys

  recToList :: (FinStat a, FinType b) => a -> [(b,Double)]    -- Rec to List

instance GetRecords BalanceSheet BsTyp where
  (!!>) x t = Hm.lookupDefault 0.0 t (x^.rec)
  (!!?) x t = Hm.lookup t (x^.rec)
  (!!~) x r = x & rec .~ (Hm.fromList r)
  (!!+) x (k,v) = x & rec .~ (Hm.insertWith (\nw ol -> nw+ol) k v (x^.rec))
  (!!%) x (k,v) = x & rec .~ (Hm.insert k v (x^.rec))
  recToList x = Hm.toList (x^.rec)

instance GetRecords ProfitLoss PlTyp where
  (!!>) x t = Hm.lookupDefault 0.0 t (x^.rec)
  (!!?) x t = Hm.lookup t (x^.rec)
  (!!~) x r = x & rec .~ (Hm.fromList r)
  (!!+) x (k,v) = x & rec .~ (Hm.insertWith (\nw ol->nw+ol) k v (x^.rec))
  (!!%) x (k,v) = x & rec .~ (Hm.insert k v (x^.rec))
  recToList x = Hm.toList (x^.rec)

instance GetRecords CashFlow CfTyp where
  (!!>) x t = Hm.lookupDefault 0.0 t (x^.rec)
  (!!?) x t = Hm.lookup t (x^.rec)
  (!!~) x r = x & rec .~ (Hm.fromList r)
  (!!+) x (k,v) = x & rec .~ (Hm.insertWith (\nw ol -> nw+ol) k v (x^.rec))
  (!!%) x (k,v) = x & rec .~ (Hm.insert k v (x^.rec))
  recToList x = Hm.toList (x^.rec)

class GetJsonz a where
  toJsonz :: FinStat a => a -> String
  fromJsonz :: FinStat a => String -> a

rdJson :: String -> Statementz  
rdJson s = undefined

wrJson :: Statementz -> String
wrJson s = undefined

bsTypMap = Hm.fromList $ zip ((T.pack . show) <$> xf) xf 
  where xf = enumFrom minBound::[BsTyp] 

bsStringToTyp :: Text -> Maybe BsTyp
bsStringToTyp s = Hm.lookup s bsTypMap

plTypMap = Hm.fromList $ zip ((T.pack . show) <$> xf) xf 
  where xf = enumFrom minBound::[PlTyp] 

plStringToTyp :: Text -> Maybe PlTyp
plStringToTyp s = Hm.lookup s plTypMap

cfTypMap = Hm.fromList $ zip ((T.pack . show) <$> xf) xf 
  where xf = enumFrom minBound::[CfTyp] 

cfStringToTyp :: Text -> Maybe CfTyp
cfStringToTyp s = Hm.lookup s cfTypMap

data Accountz = Accountz
  { accountzBeginDate         :: Day
  , accountzEndDate           :: Day
  , accountzBalanceSheetBegin :: Maybe BsMap
  , accountzBalanceSheetEnd   :: Maybe BsMap
  , accountzProfitLoss        :: Maybe PlMap
  , accountzCashFlow          :: Maybe CfMap
  } deriving (Show, FinStat)

makeFields ''Accountz

class GetAccountz a where
  (!>>) :: FinType a => Accountz -> a -> Maybe Double             -- Get
  (!^>) :: FinType a => Accountz -> a -> Maybe Double
  (!^>) = (!>>)

  (!>~) :: FinType a => Accountz -> [(a,Double)] -> Accountz      -- Set/Reset
  (!^~) :: FinType a => Accountz -> [(a,Double)] -> Accountz      -- Set/Reset
  (!^~) = (!>~)

  (!>+) :: FinType a => Accountz -> (a,Double) -> Maybe Accountz  -- Add/Create
  (!^+) :: FinType a => Accountz -> (a,Double) -> Maybe Accountz  -- Add/Create
  (!^+) = (!>+)

  (!>%) :: FinType a => Accountz -> (a,Double) -> Maybe Accountz  -- Upd/Create
  (!^%) :: FinType a => Accountz -> (a,Double) -> Maybe Accountz  -- Upd/Create
  (!^%) = (!>%)

  (!>++) :: FinType a => Accountz -> [(a,Double)] -> Maybe Accountz  -- List Add
  x !>++ [] = return x
  x !>++ (y:ys) = x !>+ y >>= (!>++ ys)

  (!^++) :: FinType a => Accountz -> [(a,Double)] -> Maybe Accountz  -- List Add
  x !^++ [] = return x
  x !^++ (y:ys) = x !^+ y >>= (!^++ ys)

  (!>%%) :: FinType a => Accountz -> [(a,Double)] -> Maybe Accountz  -- List Upd
  x !>%% [] = return x
  x !>%% (y:ys) = x !>% y >>= (!>%% ys)

  (!^%%) :: FinType a => Accountz -> [(a,Double)] -> Maybe Accountz  -- List Upd
  x !^%% [] = return x
  x !^%% (y:ys) = x !^% y >>= (!^%% ys)



instance GetAccountz BsTyp where
  (!^>) x t = do p <- x^.balanceSheetBegin; return $ Hm.lookupDefault 0.0 t p
  (!>>) x t = do p <- x ^. balanceSheetEnd; return $ Hm.lookupDefault 0.0 t p

  (!^~) x r = x & balanceSheetBegin .~ (Just (Hm.fromList r))
  (!>~) x r = x & balanceSheetEnd .~ (Just (Hm.fromList r))

  (!^+) x (k,v) = do 
    p <- (x ^. balanceSheetBegin)
    return $ x&balanceSheetBegin .~(Just (Hm.insertWith (\nw ol->nw+ol) k v p))

  (!>+) x (k,v) = do 
    p <- (x ^. balanceSheetEnd)
    return $ x & balanceSheetEnd .~ (Just (Hm.insertWith (\nw ol->nw+ol) k v p))

  (!^%) x (k,v) = do 
    p <- (x ^. balanceSheetBegin)
    return $ x & balanceSheetBegin .~ (Just (Hm.insert k v p))

  (!>%) x (k,v) = do 
    p <- (x ^. balanceSheetEnd)
    return $ x & balanceSheetEnd .~ (Just (Hm.insert k v p))



instance GetAccountz PlTyp where
  (!>>) x t = do p <- x^.profitLoss; return $ Hm.lookupDefault 0.0 t p
  (!>~) x r = x & profitLoss .~ (Just (Hm.fromList r))

  (!>+) x (k,v) = do 
    p <- (x ^. profitLoss)
    return $ x & profitLoss .~ (Just (Hm.insertWith (\nw ol->nw+ol) k v p))

  (!>%) x (k,v) = do 
    p <- (x ^. profitLoss)
    return $ x & profitLoss .~ (Just (Hm.insert k v p))



instance GetAccountz CfTyp where
  (!>>) x t = do p <- x^.cashFlow; return $ Hm.lookupDefault 0.0 t p
  (!>~) x r = x & cashFlow .~ (Just (Hm.fromList r))

  (!>+) x (k,v) = do 
    p <- (x ^. cashFlow)
    return $ x & cashFlow .~ (Just (Hm.insertWith (\nw ol->nw+ol) k v p))

  (!>%) x (k,v) = do 
    p <- (x ^. cashFlow)
    return $ x & cashFlow .~ (Just (Hm.insert k v p))


balShBegin :: Accountz -> BalanceSheet
balShBegin x = undefined

balShEnd :: Accountz -> BalanceSheet
balShEnd x = undefined

profLoss :: Accountz -> ProfitLoss
profLoss x = undefined

cashFl :: Accountz -> CashFlow
cashFl x = undefined

class FinType a => GetPSQLArray a where
  readPSQLArray :: [Text] -> Hm.HashMap a Double
  writePSQLArray :: Hm.HashMap a Double -> [Text]

instance GetPSQLArray BsTyp where
  readPSQLArray x = undefined
  writePSQLArray s = undefined

instance GetPSQLArray PlTyp where
  readPSQLArray x = undefined
  writePSQLArray s = undefined

instance GetPSQLArray CfTyp where
  readPSQLArray x = undefined
  writePSQLArray s = undefined


