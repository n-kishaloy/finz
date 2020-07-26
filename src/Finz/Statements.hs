{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Strict, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Finz.Statements
( BalanceSheet (..), ProfitLoss (..), CashFlow (..), Statementz (..)
, BsTyp (..), PlTyp (..), CfTyp (..), Statuz (..)
, BsMap, PlMap, CfMap
, HasStatuz (..), HasRec (..)
, HasDatez (..), HasBeginDate (..), HasEndDate (..)
, Checker (..), Shaker (..), CheckShake (..), HasChk (..), HasShk (..)
, HasChecker(..)
, GetRecords (..)
, GetPSQLArray (..), GetJsonz (..)
, bsTypToString, bsStringToTyp
, plTypToString, plStringToTyp
, cfTypToString, cfStringToTyp

) where

import GHC.Generics (Generic)
import Data.Hashable
import Data.Time (Day)
import qualified Data.HashMap.Strict as Hm
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
  deriving (Eq, Show, Ord, Generic, FinType)

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
  deriving (Eq, Show, Ord, Generic, FinType)

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
  deriving (Eq, Show, Ord, Generic, FinType)

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
  (!!~) :: (FinStat a, FinType b) => a -> [(b,Double)] -> a   -- Set => Reset 
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

bsTypToString :: BsTyp -> Text
bsTypToString x = case x of
  Cash -> "Cash"
  CurrentReceivables -> "CurrentReceivables"
  CurrentLoans -> "CurrentLoans"
  CurrentAdvances -> "CurrentAdvances"
  OtherCurrentAssets -> "OtherCurrentAssets"
  CurrentInvestmentsBv -> "CurrentInvestmentsBv"
  CurrentInvestmentsMv -> "CurrentInvestmentsMv"
  RawMaterials -> "RawMaterials"
  WorkInProgress -> "WorkInProgress"
  FinishedGoods -> "FinishedGoods"
  AccountReceivables -> "AccountReceivables"
  LongTermLoans -> "LongTermLoans"
  LongTermAdvances -> "LongTermAdvances"
  LongTermInvestmentsBv -> "LongTermInvestmentsBv"
  LongTermInvestmentsMv -> "LongTermInvestmentsMv"
  OtherLongTermAssets -> "OtherLongTermAssets"
  PlantPropertyEquipment -> "PlantPropertyEquipment"
  AccumulatedDepreciation -> "AccumulatedDepreciation"
  LeasingRentalAssset -> "LeasingRentalAssset"
  CapitalWip -> "CapitalWip"
  OtherTangibleAssets -> "OtherTangibleAssets"
  IntangibleAssets -> "IntangibleAssets"
  IntangibleAssetsDevelopment -> "IntangibleAssetsDevelopment"
  AccumulatedAmortization -> "AccumulatedAmortization"
  Assets -> "Assets"
  CurrentPayables -> "CurrentPayables"
  CurrentBorrowings -> "CurrentBorrowings"
  CurrentNotesPayable -> "CurrentNotesPayable"
  OtherCurrentLiabilities -> "OtherCurrentLiabilities"
  InterestPayable -> "InterestPayable"
  CurrentProvisions -> "CurrentProvisions"
  CurrentTaxPayables -> "CurrentTaxPayables"
  LiabilitiesSaleAssets -> "LiabilitiesSaleAssets"
  CurrentLeasesLiability -> "CurrentLeasesLiability"
  AccountPayables -> "AccountPayables"
  LongTermBorrowings -> "LongTermBorrowings"
  BondsPayable -> "BondsPayable"
  DeferredTaxLiabilities -> "DeferredTaxLiabilities"
  LongTermLeasesLiability -> "LongTermLeasesLiability"
  DeferredCompensation -> "DeferredCompensation"
  DeferredRevenues -> "DeferredRevenues"
  CustomerDeposits -> "CustomerDeposits"
  OtherLongTermLiabilities -> "OtherLongTermLiabilities"
  PensionProvision -> "PensionProvision"
  LongTermProvisions -> "LongTermProvisions"
  Liabilities -> "Liabilities"
  CommonStock -> "CommonStock"
  PreferredStock -> "PreferredStock"
  PdInCapAbovePar -> "PdInCapAbovePar"
  PdInCapTreasuryStock -> "PdInCapTreasuryStock"
  RevaluationReserves -> "RevaluationReserves"
  Reserves -> "Reserves"
  RetainedEarnings -> "RetainedEarnings"
  AccumulatedOci -> "AccumulatedOci"
  MinorityInterests -> "MinorityInterests"
  Equity -> "Equity"

bsStringToTyp :: Text -> Maybe BsTyp
bsStringToTyp s = case s of
  "Cash" -> Just Cash
  "CurrentReceivables" -> Just CurrentReceivables
  "CurrentLoans" -> Just CurrentLoans
  "CurrentAdvances" -> Just CurrentAdvances
  "OtherCurrentAssets" -> Just OtherCurrentAssets
  "CurrentInvestmentsBv" -> Just CurrentInvestmentsBv
  "CurrentInvestmentsMv" -> Just CurrentInvestmentsMv
  "RawMaterials" -> Just RawMaterials
  "WorkInProgress" -> Just WorkInProgress
  "FinishedGoods" -> Just FinishedGoods
  "AccountReceivables" -> Just AccountReceivables
  "LongTermLoans" -> Just LongTermLoans
  "LongTermAdvances" -> Just LongTermAdvances
  "LongTermInvestmentsBv" -> Just LongTermInvestmentsBv
  "LongTermInvestmentsMv" -> Just LongTermInvestmentsMv
  "OtherLongTermAssets" -> Just OtherLongTermAssets
  "PlantPropertyEquipment" -> Just PlantPropertyEquipment
  "AccumulatedDepreciation" -> Just AccumulatedDepreciation
  "LeasingRentalAssset" -> Just LeasingRentalAssset
  "CapitalWip" -> Just CapitalWip
  "OtherTangibleAssets" -> Just OtherTangibleAssets
  "IntangibleAssets" -> Just IntangibleAssets
  "IntangibleAssetsDevelopment" -> Just IntangibleAssetsDevelopment
  "AccumulatedAmortization" -> Just AccumulatedAmortization
  "Assets" -> Just Assets
  "CurrentPayables" -> Just CurrentPayables
  "CurrentBorrowings" -> Just CurrentBorrowings
  "CurrentNotesPayable" -> Just CurrentNotesPayable
  "OtherCurrentLiabilities" -> Just OtherCurrentLiabilities
  "InterestPayable" -> Just InterestPayable
  "CurrentProvisions" -> Just CurrentProvisions
  "CurrentTaxPayables" -> Just CurrentTaxPayables
  "LiabilitiesSaleAssets" -> Just LiabilitiesSaleAssets
  "CurrentLeasesLiability" -> Just CurrentLeasesLiability
  "AccountPayables" -> Just AccountPayables
  "LongTermBorrowings" -> Just LongTermBorrowings
  "BondsPayable" -> Just BondsPayable
  "DeferredTaxLiabilities" -> Just DeferredTaxLiabilities
  "LongTermLeasesLiability" -> Just LongTermLeasesLiability
  "DeferredCompensation" -> Just DeferredCompensation
  "DeferredRevenues" -> Just DeferredRevenues
  "CustomerDeposits" -> Just CustomerDeposits
  "OtherLongTermLiabilities" -> Just OtherLongTermLiabilities
  "PensionProvision" -> Just PensionProvision
  "LongTermProvisions" -> Just LongTermProvisions
  "Liabilities" -> Just Liabilities
  "CommonStock" -> Just CommonStock
  "PreferredStock" -> Just PreferredStock
  "PdInCapAbovePar" -> Just PdInCapAbovePar
  "PdInCapTreasuryStock" -> Just PdInCapTreasuryStock
  "RevaluationReserves" -> Just RevaluationReserves
  "Reserves" -> Just Reserves
  "RetainedEarnings" -> Just RetainedEarnings
  "AccumulatedOci" -> Just AccumulatedOci
  "MinorityInterests" -> Just MinorityInterests
  "Equity" -> Just Equity
  otherwise -> Nothing

plTypToString :: PlTyp -> Text
plTypToString x = case x of
  OperatingRevenue -> "OperatingRevenue"
  NonOperatingRevenue -> "NonOperatingRevenue"
  ExciseStaxLevy -> "ExciseStaxLevy"
  OtherIncome -> "OtherIncome"
  CostMaterial -> "CostMaterial"
  DirectExpenses -> "DirectExpenses"
  Salaries -> "Salaries"
  AdministrativeExpenses -> "AdministrativeExpenses"
  ResearchNDevelopment -> "ResearchNDevelopment"
  OtherOverheads -> "OtherOverheads"
  OtherOperativeExpenses -> "OtherOperativeExpenses"
  OtherExpenses -> "OtherExpenses"
  ExceptionalItems -> "ExceptionalItems"
  Pbitda -> "Pbitda"
  Depreciation -> "Depreciation"
  Amortization -> "Amortization"
  Pbitx -> "Pbitx"
  Interest -> "Interest"
  Pbtx -> "Pbtx"
  ExtraordinaryItems -> "ExtraordinaryItems"
  PriorYears -> "PriorYears"
  Pbt -> "Pbt"
  TaxesCurrent -> "TaxesCurrent"
  TaxesDeferred -> "TaxesDeferred"
  Pat -> "Pat"
  GainsLossesForex -> "GainsLossesForex"
  GainsLossesActurial -> "GainsLossesActurial"
  GainsLossesSales -> "GainsLossesSales"
  FvChgAvlSale -> "FvChgAvlSale"
  OtherDeferredTaxes -> "OtherDeferredTaxes"
  OtherComprehensiveIncome -> "OtherComprehensiveIncome"
  TotalComprehensiveIncome -> "TotalComprehensiveIncome"

plStringToTyp :: Text -> Maybe PlTyp
plStringToTyp s = case s of
  "OperatingRevenue" -> Just OperatingRevenue
  "NonOperatingRevenue" -> Just NonOperatingRevenue
  "ExciseStaxLevy" -> Just ExciseStaxLevy
  "OtherIncome" -> Just OtherIncome
  "CostMaterial" -> Just CostMaterial
  "DirectExpenses" -> Just DirectExpenses
  "Salaries" -> Just Salaries
  "AdministrativeExpenses" -> Just AdministrativeExpenses
  "ResearchNDevelopment" -> Just ResearchNDevelopment
  "OtherOverheads" -> Just OtherOverheads
  "OtherOperativeExpenses" -> Just OtherOperativeExpenses
  "OtherExpenses" -> Just OtherExpenses
  "ExceptionalItems" -> Just ExceptionalItems
  "Pbitda" -> Just Pbitda
  "Depreciation" -> Just Depreciation
  "Amortization" -> Just Amortization
  "Pbitx" -> Just Pbitx
  "Interest" -> Just Interest
  "Pbtx" -> Just Pbtx
  "ExtraordinaryItems" -> Just ExtraordinaryItems
  "PriorYears" -> Just PriorYears
  "Pbt" -> Just Pbt
  "TaxesCurrent" -> Just TaxesCurrent
  "TaxesDeferred" -> Just TaxesDeferred
  "Pat" -> Just Pat
  "GainsLossesForex" -> Just GainsLossesForex
  "GainsLossesActurial" -> Just GainsLossesActurial
  "GainsLossesSales" -> Just GainsLossesSales
  "FvChgAvlSale" -> Just FvChgAvlSale
  "OtherDeferredTaxes" -> Just OtherDeferredTaxes
  "OtherComprehensiveIncome" -> Just OtherComprehensiveIncome
  "TotalComprehensiveIncome" -> Just TotalComprehensiveIncome
  otherwise -> Nothing


cfTypToString :: CfTyp -> Text
cfTypToString x = case x of
  ChgInventories -> "ChgInventories"
  ChgReceivables -> "ChgReceivables"
  ChgLiabilities -> "ChgLiabilities"
  ChgProvisions -> "ChgProvisions"
  OtherCfOperations -> "OtherCfOperations"
  CashFlowOperations -> "CashFlowOperations"
  InvestmentsPpe -> "InvestmentsPpe"
  InvestmentsCapDevp -> "InvestmentsCapDevp"
  InvestmentsLoans -> "InvestmentsLoans"
  AcqEquityAssets -> "AcqEquityAssets"
  DisEquityAssets -> "DisEquityAssets"
  DisPpe -> "DisPpe"
  ChgInvestments -> "ChgInvestments"
  CfInvestmentInterest -> "CfInvestmentInterest"
  CfInvestmentDividends -> "CfInvestmentDividends"
  OtherCfInvestments -> "OtherCfInvestments"
  CashFlowInvestments -> "CashFlowInvestments"
  StockSales -> "StockSales"
  StockRepurchase -> "StockRepurchase"
  DebtIssue -> "DebtIssue"
  DebtRepay -> "DebtRepay"
  InterestFin -> "InterestFin"
  Dividends -> "Dividends"
  DonorContribution -> "DonorContribution"
  OtherCfFinancing -> "OtherCfFinancing"
  CashFlowFinancing -> "CashFlowFinancing"
  NetCashFlow -> "NetCashFlow"
  Fcff -> "Fcff"
  Fcfe -> "Fcfe"
  Fcfd -> "Fcfd"

cfStringToTyp :: Text -> Maybe CfTyp
cfStringToTyp s = case s of
  "ChgInventories" -> Just ChgInventories
  "ChgReceivables" -> Just ChgReceivables
  "ChgLiabilities" -> Just ChgLiabilities
  "ChgProvisions" -> Just ChgProvisions
  "OtherCfOperations" -> Just OtherCfOperations
  "CashFlowOperations" -> Just CashFlowOperations
  "InvestmentsPpe" -> Just InvestmentsPpe
  "InvestmentsCapDevp" -> Just InvestmentsCapDevp
  "InvestmentsLoans" -> Just InvestmentsLoans
  "AcqEquityAssets" -> Just AcqEquityAssets
  "DisEquityAssets" -> Just DisEquityAssets
  "DisPpe" -> Just DisPpe
  "ChgInvestments" -> Just ChgInvestments
  "CfInvestmentInterest" -> Just CfInvestmentInterest
  "CfInvestmentDividends" -> Just CfInvestmentDividends
  "OtherCfInvestments" -> Just OtherCfInvestments
  "CashFlowInvestments" -> Just CashFlowInvestments
  "StockSales" -> Just StockSales
  "StockRepurchase" -> Just StockRepurchase
  "DebtIssue" -> Just DebtIssue
  "DebtRepay" -> Just DebtRepay
  "InterestFin" -> Just InterestFin
  "Dividends" -> Just Dividends
  "DonorContribution" -> Just DonorContribution
  "OtherCfFinancing" -> Just OtherCfFinancing
  "CashFlowFinancing" -> Just CashFlowFinancing
  "NetCashFlow" -> Just NetCashFlow
  "Fcff" -> Just Fcff
  "Fcfe" -> Just Fcfe
  "Fcfd" -> Just Fcfd
  otherwise -> Nothing



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


