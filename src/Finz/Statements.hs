{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- {-# LANGUAGE InstanceSigs #-}

module Finz.Statements
(   BalanceSheet (..), ProfitLoss (..), CashFlow (..), Statementz (..)
,   BsTyp (..), PlTyp (..), CfTyp (..), Statuz (..)
,   HasStatuz (..), HasRec (..)
,   HasDatez (..), HasBeginDate (..), HasEndDate (..)
,   Checker (..), Shaker (..), CheckShake (..), HasChk (..), HasShk (..)
,   HasChecker(..)
,   GetRecords (..)

) where

import GHC.Generics (Generic)
import Data.Hashable
import Data.Time (Day)
import qualified Data.HashMap.Strict as Hm

import Utilz.Numeric (Approx (..))

import Control.Lens
-- import Control.Lens.TH

data Checker = Checker { checkerStatuz :: !Int } deriving (Show)
makeFields ''Checker

data Shaker = Shaker { shakerStatuz :: !Int } deriving (Show)
makeFields ''Shaker

data CheckShake = CheckShake 
    {   checkShakeChk :: !Checker
    ,   checkShakeShk :: !Shaker
    ,   checkShakeStatuz :: !Int 
    ,   checkShakeChecker :: !Int
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

data BalanceSheet = BalanceSheet 
    {   balanceSheetDatez       :: !Day
    ,   balanceSheetStatuz      :: !Statuz
    ,   balanceSheetRec         :: !(Hm.HashMap BsTyp Double)

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

data ProfitLoss = ProfitLoss
    {   profitLossBeginDate     :: !Day
    ,   profitLossEndDate       :: !Day
    ,   profitLossStatuz        :: !Statuz
    ,   profitLossRec           :: !(Hm.HashMap PlTyp Double)

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

data CashFlow = CashFlow
    {   cashFlowBeginDate       :: !Day
    ,   cashFlowEndDate         :: !Day
    ,   cashFlowStatuz          :: !Statuz
    ,   cashFlowRec             :: !(Hm.HashMap CfTyp Double)

    } deriving (Show, FinStat)

makeFields ''CashFlow

data Statementz = Statementz
    {   statementzBeginDate         :: !Day
    ,   statementzEndDate           :: !Day
    ,   statementzBalanceSheetBegin :: !BalanceSheet
    ,   statementzBalanceSheetEnd   :: !BalanceSheet
    ,   statementzProfitLoss        :: !ProfitLoss
    ,   statementzCashFlow          :: !CashFlow

    } deriving (Show)

makeFields ''Statementz

instance Approx BsTyp where x =~ y = (x == y)
instance Approx PlTyp where x =~ y = (x == y)
instance Approx CfTyp where x =~ y = (x == y)

-- TODO: Add your code here
instance Approx BalanceSheet where 
    (!x) =~ (!y) = undefined 

instance Approx ProfitLoss where 
    (!x) =~ (!y) = undefined

instance Approx CashFlow where 
    (!x) =~ (!y) = undefined

instance Approx Statementz where 
    (!x) =~ (!y) = undefined

class GetRecords a b where
    (!!>) :: (FinStat a, FinType b) => a -> b -> Double         -- Get
    (!!?) :: (FinStat a, FinType b) => a -> b -> Maybe Double   -- Maybe Get
    (!!~) :: (FinStat a, FinType b) => a -> [(b,Double)] -> a   -- Set => Reset 
    (!!+) :: (FinStat a, FinType b) => a -> (b,Double) -> a     -- Add/Create 

    (!!++) :: (FinStat a, FinType b) => a -> [(b,Double)] -> a -- List Add
    (!!++) !x [] = x
    (!!++) !x (!y:(!ys)) = (x !!+ y) !!++ ys

    (!!%) :: (FinStat a, FinType b) => a -> (b,Double) -> a     -- Update/Create

    (!!%%) :: (FinStat a, FinType b) => a -> [(b,Double)] -> a -- List Upd
    (!!%%) !x [] = x
    (!!%%) !x (!y:(!ys)) = (x !!% y) !!%% ys

    recToList :: (FinStat a, FinType b) => a -> [(b,Double)]    -- Rec to List

instance GetRecords BalanceSheet BsTyp where
    (!!>) !x !t = Hm.lookupDefault 0.0 t (x^.rec)
    (!!?) !x !t = Hm.lookup t (x^.rec)
    (!!~) !x !r = x & rec .~ (Hm.fromList r)
    (!!+) !x (!k,!v) = x & rec .~ (Hm.insertWith (\nw ol -> nw+ol) k v (x^.rec))
    (!!%) !x (!k,!v) = x & rec .~ (Hm.insert k v (x^.rec))
    recToList !x = Hm.toList (x^.rec)

instance GetRecords ProfitLoss PlTyp where
    (!!>) !x !t = Hm.lookupDefault 0.0 t (x^.rec)
    (!!?) !x !t = Hm.lookup t (x^.rec)
    (!!~) !x !r = x & rec .~ (Hm.fromList r)
    (!!+) !x (!k,(!v)) = x & rec .~ (Hm.insertWith (\nw ol->nw+ol) k v (x^.rec))
    (!!%) !x (!k,!v) = x & rec .~ (Hm.insert k v (x^.rec))
    recToList !x = Hm.toList (x^.rec)

instance GetRecords CashFlow CfTyp where
    (!!>) !x !t = Hm.lookupDefault 0.0 t (x^.rec)
    (!!?) !x !t = Hm.lookup t (x^.rec)
    (!!~) !x !r = x & rec .~ (Hm.fromList r)
    (!!+) !x (!k,!v) = x & rec .~ (Hm.insertWith (\nw ol -> nw+ol) k v (x^.rec))
    (!!%) !x (!k,!v) = x & rec .~ (Hm.insert k v (x^.rec))
    recToList !x = Hm.toList (x^.rec)

class GetJsonz a where
    toJsonz :: FinStat a => a -> String
    fromJsonz :: FinStat a => String -> a

rdJson :: String -> Statementz  
rdJson s = undefined

wrJson :: Statementz -> String
wrJson s = undefined