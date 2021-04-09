{-|
Module      : Finance.Statements
Description : Implement __Statements__ modules for the __finz__ library
Copyright   : (c) 2020 Kishaloy Neogi
License     : MIT
Maintainer  : Kishaloy Neogi
Email       : nkishaloy@yahoo.com

The module implements the Statements module which defines all the items that
go in preparing Account Statements, with definition of items that go in Balance 
Sheet, Income Statements and Cash Flow Statements.

The module also implements the individual items that go in each of these 
statements like Cash, Income, Tax etc.

Analysis of all these Statements as well as way to convert to and from JSON 
is also implemented. 

You may see the github repository at <https://github.com/n-kishaloy/finz>

-}

{-# LANGUAGE NumericUnderscores, OverloadedLists #-}
{-# LANGUAGE Strict, OverloadedStrings, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module Finance.Statements
( BalanceSheet (..), ProfitLoss (..), CashFlow (..), Statement (..)
, Account (..), GetAccount (..), GetStatement (..), GetRecords (..)
, Company (..), HasCode (..), HasAffiliated (..), HasConsolidated (..)
, HasDocs (..), HasSharePrices (..), HasRate (..), HasBeta (..)
, Param (..), HasPU (..), HasPW (..), HasPE (..), HasPD (..)
, eqlRec, notEqlRec, maybeEqlRec, notMaybeEqlRec
, cleanRecord, cleanAccount, cleanNSetAccount, cleanNSetAccountVec
, setAccount
, sortAccountVec, sortCompanyDocs, sortCheckCompany
, balShBegin, balShEnd, profLoss, cashFl, mkAccount, splitAccount
, BsTyp (..), PlTyp (..), CfTyp (..), Statuz (..)
, BsMap, PlMap, CfMap
, HasStatuz (..), HasRec (..)
, HasDatez (..), HasDateBegin (..), HasDateEnd (..), HasBalanceSheetBegin (..)
, HasBalanceSheetEnd (..), HasProfitLoss (..), HasCashFlow (..)
, Checker (..), Shaker (..), CheckShake (..), HasChk (..), HasShk (..)
, HasChecker(..), accountToJson, jsonToAccount, companyToJson, jsonToCompany
, FinType (..), FinStat
, checkBsMap, checkPlMap, checkCfMap
, getEOMonth
, credit, debit, transact, transactSeries
, checkCompany, setCompany
) where

import GHC.Generics (Generic)
import Data.Hashable ( Hashable )
import Data.Time (Day, fromGregorian, parseTimeM, defaultTimeLocale, toGregorian)
import qualified Data.HashMap.Strict as Hm
import qualified Data.HashSet as Hs
import Data.Bifunctor (second)

import qualified Finance.Base as F

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Vector as V
import Data.Vector.Algorithms.Merge (sortBy)
import Data.Vector ((!))

import Data.List (foldl')
import qualified Data.Aeson as As
import Data.Scientific (toRealFloat)

import Data.Approx

-- import Debug.Trace (trace, traceM)

import Control.Monad (forM)
import Control.Lens
import Data.Ord (comparing)
import Data.Function ((&))
-- import Control.Lens.TH

-- debug = flip trace


newtype Checker = Checker { checkerStatuz :: Int } deriving (Show)
makeFields ''Checker

newtype Shaker = Shaker { shakerStatuz :: Int } deriving (Show)
makeFields ''Shaker

data CheckShake = CheckShake 
  { checkShakeChk :: Checker
  , checkShakeShk :: Shaker
  , checkShakeStatuz :: Int 
  , checkShakeChecker :: Int
  } deriving (Show)

makeFields ''CheckShake

class (Show a, Eq a, Hashable a, Ord a, Enum a) => FinType a where

  typList :: [a]

  typVec :: V.Vector a
  typVec = V.fromList typList

  {-|@calcElem x h = Calculate and set the derived items in statements@
  The calulated elements includes Current Assets, Assets, Equity,  -}
  calcElem :: Hm.HashMap a Double -> Hm.HashMap a Double
  calcElem p = 
    foldl' (\h (z,y0,y1)-> Hm.insert z (adder y0 - adder y1) h) p calcComb
    where adder = foldl' (\y w -> y + Hm.lookupDefault 0.0 w p) 0.0

  {-|@reduceMap x = Remove derived items in statements@
  The removed elements includes Current Assets, Assets, Equity, 
  -}
  reduceMap :: Maybe (Hm.HashMap a Double) -> Maybe (Hm.HashMap a Double)
  reduceMap = ((\x -> foldl' (flip Hm.delete) x calcVec) <$>)

  calcComb :: [(a,[a],[a])]

  calcSet :: Hs.HashSet a
  calcSet = Hs.fromList $ (\(x,_,_) -> x) <$> calcComb

  calcVec :: V.Vector a
  calcVec = V.fromList $ Hs.toList calcSet

  isCalc :: a -> Bool
  isCalc = flip Hs.member calcSet
  {-# INLINE isCalc #-}

  checkCalc :: a -> b -> b
  checkCalc t x = if isCalc t then error ("Calc item : "++show t) else x
  {-# INLINE checkCalc #-}

  removeCalc :: [(a,Double)] -> [(a,Double)]
  removeCalc = filter (not . isCalc . fst)
  
  setMap :: Maybe (Hm.HashMap a Double) -> Maybe (Hm.HashMap a Double)
  setMap = (calcElem <$>) 

  typMap :: Hm.HashMap Text a



class FinStat a

data Statuz = Unset | Actual | Estimated deriving (Show, Eq)

{-|
data BsTyp defines all entries that go in a Balance Sheet.
-}
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
  CurrentAssets                 |
  AccountReceivables            |
  LongTermLoanAssets            |
  LongTermAdvances              |
  LongTermInvestmentsBv         |   
  LongTermInvestmentsMv         |
  OtherLongTermAssets           | 
  PlantPropertyEquipment        |
  AccumulatedDepreciation       |
  NetPlantPropertyEquipment     |
  LeasingRentalAssets           |
  CapitalWip                    |
  OtherTangibleAssets           |  
  IntangibleAssets              |
  IntangibleAssetsDevelopment   |
  AccumulatedAmortization       |   
  NetIntangibleAssets           |
  LongTermAssets                |
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
  CurrentLiabilities            |
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
  LongTermLiabilities           |
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
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)

instance Hashable BsTyp

type BsMap = Hm.HashMap BsTyp Double

data BalanceSheet = BalanceSheet 
  { balanceSheetDatez       :: Day
  , balanceSheetStatuz      :: Statuz
  , balanceSheetRec         :: BsMap
  } deriving (Show, FinStat)

makeFields ''BalanceSheet

{-|
data PlTyp defines all entries that go in a Profit Loss Statement.
-}
data PlTyp = 
  OperatingRevenue              |
  NonOperatingRevenue           |
  ExciseStaxLevy                |
  OtherIncome                   |
  Revenue                       |
  CostMaterial                  |
  DirectExpenses                |
  COGS                          |
  Salaries                      |
  AdministrativeExpenses        |
  ResearchNDevelopment          |
  OtherOverheads                |
  OtherOperativeExpenses        |
  OtherExpenses                 |
  ExceptionalItems              |  
  GrossProfit                   |
  Pbitda                        |
  Depreciation                  |
  AssetImpairment               |
  LossDivestitures              |
  Amortization                  |
  Pbitx                         |
  InterestRevenue               |
  InterestExpense               |
  CostDebt                      |
  OtherFinancialRevenue         |
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
  FvChangeAvlSale               |
  OtherDeferredTaxes            |
  OtherComprehensiveIncome      |
  TotalComprehensiveIncome 
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)

instance Hashable PlTyp
type PlMap = Hm.HashMap PlTyp Double

data ProfitLoss = ProfitLoss
  { profitLossDateBegin     :: Day
  , profitLossDateEnd       :: Day
  , profitLossStatuz        :: Statuz
  , profitLossRec           :: PlMap
  } deriving (Show, FinStat)

makeFields ''ProfitLoss

{-|
data CfTyp defines all entries that go in a Cash Flow Statement.
-}
data CfTyp = 
  ChangeInventories             |
  ChangeReceivables             |
  ChangeLiabilities             |
  ChangeProvisions              |
  OtherCfOperations             |
  CashFlowOperations            |
  InvestmentsPpe                |
  InvestmentsCapDevp            |
  InvestmentsLoans              |
  AcqEquityAssets               |
  DisEquityAssets               |
  DisPpe                        |
  ChangeInvestments             |
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
  Fcfs                          |
  Fcfe                          |
  Fcfd                 
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)

instance Hashable CfTyp
type CfMap = Hm.HashMap CfTyp Double

data CashFlow = CashFlow
  { cashFlowDateBegin       :: Day
  , cashFlowDateEnd         :: Day
  , cashFlowStatuz          :: Statuz
  , cashFlowRec             :: CfMap
  } deriving (Show, FinStat)

makeFields ''CashFlow

instance FinType BsTyp where 
  typList = enumFrom minBound::[BsTyp]

  typMap = Hm.fromList $ zip (T.pack . show <$> (typList::[BsTyp])) typList 

  calcComb = []

instance FinType PlTyp where 
  typList = enumFrom minBound::[PlTyp]

  typMap = Hm.fromList $ zip (T.pack . show <$> (typList::[PlTyp])) typList 

  calcComb = 
    [ (Revenue,
      [OperatingRevenue, NonOperatingRevenue],
      [ExciseStaxLevy]
      )
    , (COGS,
      [CostMaterial, DirectExpenses],
      []
      )
    , (GrossProfit,
      [Revenue],
      [COGS]
      )
    , (Pbitda,
      [GrossProfit, OtherIncome],
      [Salaries, AdministrativeExpenses, ResearchNDevelopment, OtherOverheads, OtherOperativeExpenses, OtherExpenses, ExceptionalItems]
      )
    , (Pbitx,
      [Pbitda],
      [Depreciation, AssetImpairment, LossDivestitures, Amortization]
      )
    , (Pbtx,
      [Pbitx, InterestRevenue, OtherFinancialRevenue],
      [InterestExpense, CostDebt]
      )
    , (Pbt,
      [Pbtx],
      [ExtraordinaryItems, PriorYears]
      )
    , (Pat,
      [Pbt],
      [TaxesCurrent, TaxesDeferred]
      )
    , (OtherComprehensiveIncome,
      [GainsLossesForex, GainsLossesActurial, GainsLossesSales, FvChangeAvlSale],
      [OtherDeferredTaxes]
      )
    , (TotalComprehensiveIncome,
      [Pat, OtherComprehensiveIncome],
      []
      )
    ]


instance FinType CfTyp where 
  typList = enumFrom minBound::[CfTyp]

  typMap = Hm.fromList $ zip (T.pack . show <$> (typList::[CfTyp])) typList 

  calcComb = []

data Statement = Statement
  { statementDateBegin         :: Day
  , statementDateEnd           :: Day
  , statementBalanceSheetBegin :: Maybe BalanceSheet
  , statementBalanceSheetEnd   :: Maybe BalanceSheet
  , statementProfitLoss        :: Maybe ProfitLoss
  , statementCashFlow          :: Maybe CashFlow
  } deriving (Show)

makeFields ''Statement

instance Approx BsTyp where x =~ y = x == y
instance Approx PlTyp where x =~ y = x == y
instance Approx CfTyp where x =~ y = x == y

-- TODO: Add your code here
instance Approx BalanceSheet where 
  x =~ y = 
    (x ^. datez)  ==  (y ^. datez)   &&
    (x ^. statuz) ==  (y ^. statuz) &&
    (x ^. rec)  `eqlRec`  (y ^. rec)


instance Approx ProfitLoss where 
  x =~ y = 
    (x ^. dateBegin)  ==  (y ^. dateBegin)  &&
    (x ^. dateEnd)    ==  (y ^. dateEnd)    &&
    (x ^. statuz)     ==  (y ^. statuz)     &&
    (x ^. rec)  `eqlRec`  (y ^. rec)
     
instance Approx CashFlow where 
  x =~ y = 
    (x ^. dateBegin)  ==  (y ^. dateBegin)  &&
    (x ^. dateEnd)    ==  (y ^. dateEnd)    &&
    (x ^. statuz)     ==  (y ^. statuz)     &&
    (x ^. rec)  `eqlRec`  (y ^. rec)
     
instance Approx Statement where 
  x =~ y = undefined

{-|
Getter and Setter for BalanceSheet, ProfitLoss and CashFlow Items contained
in the HashMap. Items refer to BalanceSheet (Cash, Current Receivables etc), 
ProfitLoss (Revenue, Pat etc.) and CashFlow (FCFF, FCFE, etc).

*@!!> =@ Getter with default of 0.0
*@!!? =@ Getter as Maybe Double with no default and missing item gives Nothing
*@!!~ =@ Setter with new data. All old items are lost
*@!!+ =@ Add / Create. If items exist then Add, otherwise Create
*@!!% =@ Update / Create. If items exist then Update, other Create

*@addToItems =@ Uses @!!+@ to Add / Create using a List
*@updateToItems =@ Uses @!!%@ to Update / Create using a List
-}
class (FinStat a, FinType b) => GetRecords a b where
  (!!>) :: a -> b -> Double         -- Get
  (!!?) :: a -> b -> Maybe Double   -- Maybe Get
  (!!~) :: a -> [(b,Double)] -> a   -- Set/Reset 
  (!!+) :: a -> (b,Double) -> a     -- Add/Create 
  (!!%) :: a -> (b,Double) -> a     -- Update/Create

  addToItems :: a -> [(b,Double)] -> a -- List Add
  addToItems x [] = x
  addToItems x (y:ys) = (x !!+ y) `addToItems` ys

  updateItems :: a -> [(b,Double)] -> a -- List Upd
  updateItems x [] = x
  updateItems x (y:ys) = (x !!% y) `updateItems` ys

  recToList :: a -> [(b,Double)]    -- Rec to List

instance GetRecords BalanceSheet BsTyp where
  (!!>) x t = Hm.lookupDefault 0.0 t (x^.rec)
  (!!?) x t = Hm.lookup t (x^.rec)
  (!!~) x r = x & rec .~ Hm.fromList r
  (!!+) x (k,v) = x & rec .~ Hm.insertWith (+) k v (x^.rec)
  (!!%) x (k,v) = x & rec .~ Hm.insert k v (x^.rec)
  recToList x = Hm.toList (x^.rec)

instance GetRecords ProfitLoss PlTyp where
  (!!>) x t = Hm.lookupDefault 0.0 t (x^.rec)
  (!!?) x t = Hm.lookup t (x^.rec)
  (!!~) x r = x & rec .~ Hm.fromList r
  (!!+) x (k,v) = x & rec .~ Hm.insertWith (+) k v (x^.rec)
  (!!%) x (k,v) = x & rec .~ Hm.insert k v (x^.rec)
  recToList x = Hm.toList (x^.rec)

instance GetRecords CashFlow CfTyp where
  (!!>) x t = Hm.lookupDefault 0.0 t (x^.rec)
  (!!?) x t = Hm.lookup t (x^.rec)
  (!!~) x r = x & rec .~ Hm.fromList r
  (!!+) x (k,v) = x & rec .~ Hm.insertWith (+) k v (x^.rec)
  (!!%) x (k,v) = x & rec .~ Hm.insert k v (x^.rec)
  recToList x = Hm.toList (x^.rec)

{-|@eqlRec x y = check equality of HashMaps x and y@

*x = HashMap of Finance Type
*y = HashMap of Finance Type

The equality uses a default of '0' for keys not available as it is the
default in Financial Statements.
-}
eqlRec :: FinType a => Hm.HashMap a Double -> Hm.HashMap a Double -> Bool 
eqlRec x y = fz x y && fz y x where
    fz p q = foldl' (f p) True $ Hm.toList q
    f p t (k,v) = t && (Hm.lookupDefault 0.0 k p =~ v)  

{-|@notEqlRec x y = not $ eqlRec x y@-}
notEqlRec :: FinType a => Hm.HashMap a Double -> Hm.HashMap a Double -> Bool
notEqlRec x y = not $ eqlRec x y

{-|@
maybeEqlRec x y = Wrapper on eqlRec to have Maybe HashMap type

maybeEqlRec Nothing Nothing = True
maybeEqlRec (Just x) (Just y) = eqlRec x y
maybeEqlRec _ _ = False
@
-}
maybeEqlRec :: FinType a => Maybe (Hm.HashMap a Double) -> Maybe (Hm.HashMap a Double) -> Bool
maybeEqlRec Nothing Nothing = True
maybeEqlRec (Just x) (Just y) = eqlRec x y
maybeEqlRec _ _ = False

{-|@notMaybeEqlRec x y = not $ maybeEqlRec x y@-}
notMaybeEqlRec :: FinType a => Maybe (Hm.HashMap a Double) -> Maybe (Hm.HashMap a Double) -> Bool
notMaybeEqlRec x y = not $ maybeEqlRec x y

infix 4 `eqlRec`, `notEqlRec`, `maybeEqlRec`, `notMaybeEqlRec`

-- |Financial statements corresponding to a period between __DateBegin__ and __DateEnd__. 
data Account = Account
  { accountDateBegin         :: Day
  , accountDateEnd           :: Day
  , accountBalanceSheetBegin :: Maybe BsMap
  , accountBalanceSheetEnd   :: Maybe BsMap
  , accountProfitLoss        :: Maybe PlMap
  , accountCashFlow          :: Maybe CfMap
  } deriving (FinStat)

makeFields ''Account

{-|
Getter and Setter for Account. The functions are common between the 3 different
type of Statements and it is the /type/ of the /key/ which determines, which 
statement is chosen.

However, there are 2 Balance Sheets one at the beginning and one at the end. 
Hence there are 2 sets of operators for accessing the same. 

Operators
==========
For BalanceSheet 
    Begin : @!^...@ 
    End   : @!>...@  

For ProfitLoss and CashFlow, either may be used. 

*@!^> & !>> =@ Get data 
*@!^~ & !^~ =@ Set / Reset. Old data is lost
*@!^+ & !>+ =@ Add / Create data. If old data exists Add, otherwise Create
*@!^- & !>- =@ Subtract / Create -ve data. If old data exists Subtract, otherwise Create -ve valued entry.
*@!^% & !>% =@ Update / Create data. If old data exists Update, otherwise Create
*@addToBeginItems =@ Use !^+ using a List 
*@addToEndItems   =@ Use !>+ using a List
*@updateToBeginItems =@ Use !^% using a List 
*@updateToEndItems   =@ Use !>% using a List
*@stringToType =@ Convert String to FinType. Expected FinType (BlTyp, PlTyp or CfTyp) has to be specified
*@recToJson =@ Convert a Record (BsMap, PlMap or CfMap) to JSON
*@jsonToRec =@ Convert JSON to Record. The type of the output has to be 
provided separately as otherwise, the function has no way of knowing if 
the Text represents BsMap, PlMap or CfMap .
-}
class FinType a => GetAccount a where
  (!>>) :: Account -> a -> Maybe Double             -- Get
  (!^>) :: Account -> a -> Maybe Double
  (!^>) = (!>>)

  (!>~) :: Account -> [(a,Double)] -> Account      -- Set/Reset
  (!^~) :: Account -> [(a,Double)] -> Account      -- Set/Reset
  (!^~) = (!>~)

  (!>+) :: Account -> (a,Double) -> Maybe Account  -- Add/Create
  (!^+) :: Account -> (a,Double) -> Maybe Account  -- Add/Create
  (!^+) = (!>+)

  (!>-) :: Account -> (a,Double) -> Maybe Account  -- Subtract/Create -ve
  (!^-) :: Account -> (a,Double) -> Maybe Account  -- Subtract/Create -ve
  x !>- (k,v) = x !>+ (k,-v) 
  x !^- (k,v) = x !^+ (k,-v) 

  (!>%) :: Account -> (a,Double) -> Maybe Account  -- Upd/Create
  (!^%) :: Account -> (a,Double) -> Maybe Account  -- Upd/Create
  (!^%) = (!>%)

  addToEndItems :: Account -> [(a,Double)] -> Maybe Account  -- List Add
  x `addToEndItems` [] = return x
  x `addToEndItems` (y:ys) = x !>+ y >>= (`addToEndItems` ys)

  addToBeginItems :: Account -> [(a,Double)] -> Maybe Account  -- List Add
  x `addToBeginItems` [] = return x
  x `addToBeginItems` (y:ys) = x !^+ y >>= (`addToBeginItems` ys)

  subToEndItems :: Account -> [(a,Double)] -> Maybe Account  -- List Add
  x `subToEndItems` [] = return x
  x `subToEndItems` (y:ys) = x !>- y >>= (`subToEndItems` ys)

  subToBeginItems :: Account -> [(a,Double)] -> Maybe Account  -- List Add
  x `subToBeginItems` [] = return x
  x `subToBeginItems` (y:ys) = x !^- y >>= (`subToBeginItems` ys)

  updateEndItems :: Account -> [(a,Double)] -> Maybe Account  -- List Upd
  x `updateEndItems` [] = return x
  x `updateEndItems` (y:ys) = x !>% y >>= (`updateEndItems` ys)

  updateBeginItems :: Account -> [(a,Double)] -> Maybe Account  -- List Upd
  x `updateBeginItems` [] = return x
  x `updateBeginItems` (y:ys) = x !^% y >>= (`updateBeginItems` ys)

  stringToTyp :: Text -> Maybe a
  stringToTyp s = Hm.lookup s typMap

  recToJSON :: Show a =>  Hm.HashMap a Double -> Text
  recToJSON s = T.pack (concat (["{",ps,"}"]::[String])) where
    _:ps = foldl' f "" $ Hm.toList s
    f v (x,y) = if y =~ 0.0 then v else concat ([v,",\"",show x,"\":", show y] :: [String])

  jRec :: (Hashable a, Eq a) => As.Object -> Maybe (Hm.HashMap a Double)
  jRec x = Hm.fromList <$> foldl' f (Just []) (Hm.toList x) 
    where
    f :: GetAccount a => Maybe [(a,Double)] -> (Text,As.Value) -> Maybe [(a,Double)]
    f (Just x) (u, As.Number v) = case stringToTyp u of 
      Just p -> Just $ (p,toRealFloat v::Double):x 
      _ -> Nothing
    f _ _ = Nothing

  jsonToRec :: (Hashable a, Eq a) => Text -> Maybe (Hm.HashMap a Double)
  jsonToRec s = As.decodeStrict (encodeUtf8 s) >>= jRec

instance Show Account where
  show y = 
    let 
      prx :: FinType a => Maybe (Hm.HashMap a Double) -> String
      prx Nothing = "Nothing\n"
      prx (Just x) = concat $ (\y -> show y ++ "\n") <$> Hm.toList x

      -- TODO : Add pretty printing of Items
      -- shwLn :: (FinType, Double)
      -- shwLn (u,v) = p ++ "\n" where
      --   ft = show u; 
    in 
      "************** Account ***********\n\n" ++
      "Begin Date : " ++ show (y ^. dateBegin) ++ "\n" ++
      "End Date   : " ++ show (y ^. dateEnd) ++ "\n" ++
      "\n******* Balance Sheet Begin *******\n" ++ 
      prx (y ^. balanceSheetBegin) ++
      "\n******** Balance Sheet End ********\n" ++ 
      prx (y ^. balanceSheetEnd) ++
      "\n*********** Profit Loss ***********\n" ++ 
      prx (y ^. profitLoss) ++
      "\n************ Cash Flow ************\n" ++ 
      prx (y ^. cashFlow) ++
      "\n*************** END ***************\n"

-- |DEPRECATED : This is Just proof of concept -- Not to be used.
-- Use show instead
prnx :: Account -> String
prnx y = 
  let 
    prx :: FinType a => Maybe (Hm.HashMap a Double) -> Maybe String
    prx Nothing = Just "Nothing\n"
    prx (Just x) = concat <$> forM (Hm.toList x) (\u -> return $ show u ++ "\n")
  in 
    "************** Account ***********\n\n" ++
    "Begin Date : " ++ show (y ^. dateBegin) ++ "\n" ++
    "End Date   : " ++ show (y ^. dateEnd) ++ "\n" ++ 
    (let Just v = prx $ y ^. balanceSheetBegin in v) ++
    (let Just v = prx $ y ^. balanceSheetEnd in v) 


instance Approx Account where
  x =~ y = 
    (x ^. dateBegin)          ==            (y ^. dateBegin)          &&
    (x ^. dateEnd)            ==            (y ^. dateEnd)            &&
    (x ^. balanceSheetBegin)  `maybeEqlRec` (y ^. balanceSheetBegin)  &&
    (x ^. balanceSheetEnd)    `maybeEqlRec` (y ^. balanceSheetEnd)    &&
    (x ^. profitLoss)         `maybeEqlRec` (y ^. profitLoss)         &&
    (x ^. cashFlow)           `maybeEqlRec` (y ^. cashFlow)
    

instance GetAccount BsTyp where
  (!^>) x t = do p <- x^.balanceSheetBegin; return $ Hm.lookupDefault 0.0 t p
  (!>>) x t = do p <- x ^. balanceSheetEnd; return $ Hm.lookupDefault 0.0 t p

  (!^~) x r = x & balanceSheetBegin ?~ Hm.fromList r
  (!>~) x r = x & balanceSheetEnd ?~ Hm.fromList r

  (!^+) x (k,v) = do 
    p <- x ^. balanceSheetBegin
    return $ x & balanceSheetBegin ?~ Hm.insertWith (+) k v p

  (!>+) x (k,v) = do 
    p <- x ^. balanceSheetEnd
    return $ x & balanceSheetEnd ?~ Hm.insertWith (+) k v p

  (!^%) x (k,v) = do 
    p <- x ^. balanceSheetBegin
    return $ x & balanceSheetBegin ?~ Hm.insert k v p

  (!>%) x (k,v) = do 
    p <- x ^. balanceSheetEnd
    return $ x & balanceSheetEnd ?~ Hm.insert k v p


instance GetAccount PlTyp where
  (!>>) x t = do p <- x^.profitLoss; return $ Hm.lookupDefault 0.0 t p
  (!>~) x r = x & profitLoss ?~ Hm.fromList r

  (!>+) x (k,v) = do 
    p <- x ^. profitLoss
    return $ x & profitLoss ?~ Hm.insertWith (+) k v p

  (!>%) x (k,v) = do 
    p <- x ^. profitLoss
    return $ x & profitLoss ?~ Hm.insert k v p


instance GetAccount CfTyp where
  (!>>) x t = do p <- x^.cashFlow; return $ Hm.lookupDefault 0.0 t p
  (!>~) x r = x & cashFlow ?~ Hm.fromList r

  (!>+) x (k,v) = do 
    p <- x ^. cashFlow
    return $ x & cashFlow ?~ Hm.insertWith (+) k v p

  (!>%) x (k,v) = do 
    p <- x ^. cashFlow
    return $ x & cashFlow ?~ Hm.insert k v p


-- |Extract Balance Sheet for Begin period
balShBegin :: Account -> Maybe BalanceSheet
balShBegin x = BalanceSheet (x ^. dateBegin) Actual <$> x ^. balanceSheetBegin
  
-- |Extract Balance Sheet for End period
balShEnd :: Account -> Maybe BalanceSheet
balShEnd x = BalanceSheet (x ^. dateEnd) Actual <$> (x ^. balanceSheetEnd)

-- |Extract Profit Loss Statement
profLoss :: Account -> Maybe ProfitLoss
profLoss x = ProfitLoss (x^.dateBegin) (x^.dateEnd) Actual <$> (x^.profitLoss) 

-- |Extract Cash Flow statement
cashFl :: Account -> Maybe CashFlow
cashFl x = CashFlow (x ^. dateBegin) (x ^. dateEnd) Actual <$> (x ^. cashFlow) 

-- |Combine Begin & End Balance Sheet, Profit Loss Statement and Cash Flow 
-- Statement to create Account
mkAccount :: Maybe BalanceSheet -> Maybe BalanceSheet -> Maybe ProfitLoss -> Maybe CashFlow -> Maybe Account
mkAccount _ _ Nothing _ = Nothing
mkAccount bsBeg bsEnd (Just pl) cf = 
  if d1 == dtbs1 && d2 == dtbs2 && d1 == dtcf1 && d2 == dtcf2
  then Just $ Account d1 d2 bsBg bsEn (Just (pl^.rec)) cfMp
  else Nothing
  where
    d1 = pl ^. dateBegin
    d2 = pl ^. dateEnd

    datBS :: Maybe BalanceSheet -> Day -> (Day, Maybe BsMap)
    datBS Nothing dt = (dt, Nothing)
    datBS (Just xj) _ = (xj^.datez, Just (xj^.rec)) 

    datCF :: Maybe CashFlow -> Day -> Day -> (Day, Day, Maybe CfMap)
    datCF Nothing dt1 dt2 = (dt1, dt2, Nothing)
    datCF (Just xj) _ _ = (xj^.dateBegin, xj^.dateEnd, Just (xj^.rec)) 

    (dtbs1,bsBg) = datBS bsBeg d1
    (dtbs2,bsEn) = datBS bsEnd d2
    (dtcf1,dtcf2,cfMp) = datCF cf d1 d2

-- |Split Account into its constituent Balance Sheets, Profit Loss Statement
-- and Cash FLow Statement.
splitAccount :: Account -> (Maybe BalanceSheet, Maybe BalanceSheet, Maybe ProfitLoss, Maybe CashFlow)
splitAccount x = (balShBegin x, balShEnd x, profLoss x, cashFl x)

-- |@cleanRecord mp = Remove all 0 value items@
cleanRecord :: FinType a => Hm.HashMap a Double -> Hm.HashMap a Double
cleanRecord = Hm.fromList . filter ((/=0.0).snd) . (second (fromInteger.round) <$>) . Hm.toList

-- |@cleanAccount ac = Clean all items in Account from the HashMaps@
cleanAccount :: Account -> Account
cleanAccount ac = Account (ac ^. dateBegin) (ac ^. dateEnd) bB bE pl cf where
  bB  = cleanRecord <$> ac ^. balanceSheetBegin 
  bE  = cleanRecord <$> ac ^. balanceSheetEnd 
  pl  = cleanRecord <$> ac ^. profitLoss 
  cf  = cleanRecord <$> ac ^. cashFlow 

{-|
Convert Account to JSON. Typical JSON representation looks as below

@
{"dateBegin":"2018-03-31","dateEnd":"2019-03-31","balanceSheetBegin":null,
"balanceSheetEnd":{"Cash":10.0,"AccumulatedDepreciation":12.7,
"CurrentLoans":78.02,"AccumulatedOci":23.25,"IntangibleAssets":65.43,
"CurrentAdvances":12.5},"profitLoss":{"Pbitda":3.58,
"OperatingRevenue":93.57,"OtherExpenses":41.58,"Depreciation":56.58,
"NonOperatingRevenue":67.65,"Amortization":54.32,"Pbt":23.65,"Pat":22.65},
"cashFlow":{"Fcfd":15.89,"CashFlowInvestments":25.0,"Fcff":2.73,
"StockSales":22.54,"OtherCfInvestments":84.56,
"CashFlowOperations":53.35,"CashFlowFinancing":-3.5}}
@
-}
accountToJson :: Account -> Text
accountToJson x = 
  T.concat["{",dbeg,",",dend,",",b1J,",",b2J,",",plJ,",",cfJ,"}"] 
  where
  dbeg = T.concat["\"dateBegin\":\"", T.pack $ show (x ^. dateBegin),"\""]
  dend = T.concat["\"dateEnd\":\"", T.pack $ show (x ^. dateEnd),"\""]

  b1 = x ^. balanceSheetBegin; b1J = T.concat ["\"balanceSheetBegin\":",b1x]
  b1x = maybe "null" recToJSON b1

  b2 = x ^. balanceSheetEnd; b2J = T.concat ["\"balanceSheetEnd\":",b2x]
  b2x = maybe "null" recToJSON b2

  pl = x ^. profitLoss; plJ = T.concat ["\"profitLoss\":",plx]
  plx = maybe "null" recToJSON pl

  cf = x ^. cashFlow; cfJ = T.concat ["\"cashFlow\":",cfx]
  cfx = maybe "null" recToJSON cf

jAcc :: As.Object -> Maybe Account
jAcc rz = do
  let
    parseObj :: (GetAccount a, FinType a) => As.Value -> Maybe (Maybe (Hm.HashMap a Double))
    parseObj (As.Object x) = Just <$> jRec x 
    parseObj As.Null = Just Nothing
    parseObj _ = Nothing

  (As.String dB) <- Hm.lookup "dateBegin" rz; dBeg <- getEOMonth dB
  (As.String dE) <- Hm.lookup "dateEnd" rz; dEnd <- getEOMonth dE
  bsB <- Hm.lookup "balanceSheetBegin" rz >>= parseObj 
  bsE <- Hm.lookup "balanceSheetEnd" rz >>= parseObj 
  plX <- Hm.lookup "profitLoss" rz >>= parseObj 
  cfX <- Hm.lookup "cashFlow" rz >>= parseObj 

  return $ Account dBeg dEnd bsB bsE plX cfX

-- |Convert JSON to Account
jsonToAccount :: Text -> Maybe Account
jsonToAccount s = As.decodeStrict (encodeUtf8 s) >>= jAcc

getEOMonth :: Text -> Maybe Day
getEOMonth x = do
  y <- parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack x) :: Maybe Day
  let (u,v,_) = toGregorian y
  return $ fromGregorian u v (if v == 6 || v == 9 then 30 else 31) 

class FinStat a => GetStatement a where
  toJsonz :: a -> String
  fromJsonz :: String -> a

  updateEndStatement :: Account -> a -> Maybe Account
  updateBeginStatement :: Account -> a -> Maybe Account
  updateStatement :: Account -> a -> Maybe Account
  updateBeginStatement = updateEndStatement
  updateStatement = updateEndStatement

instance GetStatement BalanceSheet where
  updateBeginStatement x s = if x^.dateBegin == s^.datez 
    then Just $ x & balanceSheetBegin ?~ s ^. rec
    else Nothing

  updateEndStatement x s = if x^.dateEnd == s^.datez 
    then Just $ x & balanceSheetEnd ?~ s ^. rec
    else Nothing

instance GetStatement ProfitLoss where
  updateEndStatement x s = if x^.dateBegin == s^.dateBegin && x^.dateEnd == s^.dateEnd
    then Just $ x & profitLoss ?~ s ^. rec
    else Nothing

instance GetStatement CashFlow where
  updateEndStatement x s = if x^.dateBegin == s^.dateBegin && x^.dateEnd == s^.dateEnd
    then Just $ x & cashFlow ?~ s ^. rec
    else Nothing

data Param = Param
  { paramPU :: Double
  , paramPW :: Double
  , paramPE :: Double
  , paramPD :: Double 
  } deriving (Show)

makeFields ''Param

instance Approx Param where
  x =~ y = 
    (x ^. pU) =~ (y ^. pU) &&
    (x ^. pW) =~ (y ^. pW) &&
    (x ^. pE) =~ (y ^. pE) &&
    (x ^. pD) =~ (y ^. pD) 
    

data Company = Company
  { companyCode         ::  Text
  , companyAffiliated   ::  Maybe (Hm.HashMap Text Double)
  , companyConsolidated ::  Bool
  , companyDocs         ::  V.Vector Account
  , companySharePrices  ::  Maybe F.DTrend
  , companyRate         ::  Maybe (F.TrendData Param)
  , companyBeta         ::  Maybe (F.TrendData Param)

  } deriving (Show)

makeFields ''Company

instance Approx Company where
  x =~ y =
    (x ^. code)         =~            (y ^.code)          &&
    (x ^. consolidated) ==            (y ^. consolidated) &&
    (x ^. docs)         =~            (y ^. docs)        

companyToJson :: Company -> Text 
companyToJson cp = 
  T.concat 
    [ "{"
    ,"\"code\":\"",cp ^. code,"\""
    , ",\"affiliated\":null"
    , ",\"consolidated\":",if cp ^.consolidated then "true," else "false,"
    , "\"docs\":", T.replace "}{" "},{" $ T.concat (("["::Text):(accountToJson <$> V.toList (cp ^. docs))++["]"::Text])
    , ",\"shareprices\":null"
    , ",\"rate\":null"
    , ",\"beta\":null"
    , "}"
    ]

jsonToCompany :: Text -> Maybe Company
jsonToCompany s = do
  rz <- As.decodeStrict (encodeUtf8 s) :: Maybe (Hm.HashMap Text As.Value)
  (As.String cd) <- Hm.lookup "code" rz
  (As.Bool cns) <- Hm.lookup "consolidated" rz

  (As.Array dtz) <- Hm.lookup "docs" rz
  dz <- traverse jAcc ((\(As.Object x) -> x) <$> dtz) :: Maybe (V.Vector Account)

  let
    aff = Nothing 
    rat = Nothing 
    shp = Nothing 
    bet = Nothing 

  return (Company cd aff cns dz shp rat bet)


-- |@debit bs t v = debit value v from entry t in Balance Sheet bs@
debit :: BsMap -> BsTyp -> Double -> BsMap
debit bs t v = checkCalc t $ case t of 
  Cash                          -> adder v
  CurrentReceivables            -> adder v
  CurrentLoans                  -> adder v
  CurrentAdvances               -> adder v
  OtherCurrentAssets            -> adder v
  CurrentInvestmentsBv          -> adder v
  CurrentInvestmentsMv          -> adder v
  RawMaterials                  -> adder v
  WorkInProgress                -> adder v
  FinishedGoods                 -> adder v
  AccountReceivables            -> adder v
  LongTermLoanAssets            -> adder v
  LongTermAdvances              -> adder v
  LongTermInvestmentsBv         -> adder v   
  LongTermInvestmentsMv         -> adder v
  OtherLongTermAssets           -> adder v 
  PlantPropertyEquipment        -> adder v
  AccumulatedDepreciation       -> adder (-v) -- Contra Account
  LeasingRentalAssets           -> adder v    
  CapitalWip                    -> adder v
  OtherTangibleAssets           -> adder v  
  IntangibleAssets              -> adder v
  IntangibleAssetsDevelopment   -> adder v
  AccumulatedAmortization       -> adder (-v) -- Contra Account
  CurrentPayables               -> adder (-v)
  CurrentBorrowings             -> adder (-v)
  CurrentNotesPayable           -> adder (-v)
  OtherCurrentLiabilities       -> adder (-v)
  InterestPayable               -> adder (-v)
  CurrentProvisions             -> adder (-v)
  CurrentTaxPayables            -> adder (-v)
  LiabilitiesSaleAssets         -> adder (-v)
  CurrentLeasesLiability        -> adder (-v)
  AccountPayables               -> adder (-v)
  LongTermBorrowings            -> adder (-v)
  BondsPayable                  -> adder (-v)
  DeferredTaxLiabilities        -> adder (-v)
  LongTermLeasesLiability       -> adder (-v)
  DeferredCompensation          -> adder (-v)
  DeferredRevenues              -> adder (-v)
  CustomerDeposits              -> adder (-v)
  OtherLongTermLiabilities      -> adder (-v)
  PensionProvision              -> adder (-v)
  LongTermProvisions            -> adder (-v)
  CommonStock                   -> adder (-v)
  PreferredStock                -> adder (-v)
  PdInCapAbovePar               -> adder (-v)
  PdInCapTreasuryStock          -> adder (-v)
  RevaluationReserves           -> adder (-v)
  Reserves                      -> adder (-v)
  RetainedEarnings              -> adder (-v)
  AccumulatedOci                -> adder (-v)
  MinorityInterests             -> adder (-v)
  _                             -> error "Derived Balance Sheet Item"
  where adder x = Hm.insertWith (+) t x bs; {-# INLINE adder #-}
{-# INLINE debit #-}

-- |@credit bs t v = credit value v from entry t in Balance Sheet bs@
credit :: BsMap -> BsTyp -> Double -> BsMap
credit bs a0 val = debit bs a0 (-val); {-# INLINE credit #-}

{-|@transact bs deb crd val = Double entry of value 'val'@

*deb = Type of Entry to be Debited
*crd = Type of Entry to be Credited
*val = Value of the Entry
-}
transact :: BsMap -> BsTyp -> BsTyp -> Double -> BsMap
transact bs deb crd val = credit (debit bs deb val) crd val

{-|@transactSeries balanceSheetMap [(debit, credit, value)] = Effect series of transaction on Balance Sheet Map@

*balanceSheetMap  =   HashMap of BalanceSheet
*debit            =   Item to be debited from
*credit           =   Item to be credited from
*value            =   Value of the transaction

*[(debit, credit, value)\] = List of Transactions to be affected
-}
transactSeries :: BsMap -> [(BsTyp, BsTyp, Double)] -> BsMap
transactSeries = foldl' (\y (db,cr,v) -> transact y db cr v)


{-|@checkBsMap bs = Evaluate consistency of BsMap@

In case calculated items has not been set then the function internally 
sets them. 
-}
checkBsMap :: BsMap -> Bool
checkBsMap bs = True

{-|@checkPlMap pl = Evaluate consistency of PlMap@

In case calculated items has not been set then the function internally 
sets them. 
-}
checkPlMap :: PlMap -> Bool
checkPlMap pl = True

{-|@checkCfMap cf = Evaluate consistency of CfMap@

In case calculated items has not been set then the function internally 
sets them. 
-}
checkCfMap :: CfMap -> Bool
checkCfMap cf = True

-- |Check if two accounts x and y are consective or not
-- Check that dates and Balanxe Sheets match
isConsecutiveAccount :: Account -> Account -> Bool
isConsecutiveAccount x y = 
  x ^. dateEnd == y ^. dateBegin && 
  x ^. balanceSheetEnd `maybeEqlRec` y ^. balanceSheetBegin

-- |Check if consecutive Account in a Vec is consistent
-- Basically checking account0.bal_shEnd == account1.bal_shBegin && 
-- account0.dateEnd == account1.dateBegin && account0.dateEnd /~ None
-- && account1.dateBegin /~ None
checkAccountVec :: V.Vector Account -> Bool
checkAccountVec vx = foldl' f (checkAccount (vx!0)) ([0..(length vx - 2)]::V.Vector Int) where
  f y i = y && isConsecutiveAccount (vx!i) (vx!(i+1)) && checkAccount (vx!(i+1))

-- |Check Account for consistency
checkAccount :: Account -> Bool
checkAccount (Account _ _ bB bE pl cf) = 
  let
    ckBS (Just b) = checkBsMap b
    ckBS Nothing  = True 

    ckPL (Just p) = checkPlMap p 
    ckPL Nothing  = True 

    ckCF (Just c) = checkCfMap c
    ckCF Nothing  = True 

  in
    ckBS bB && ckBS bE && ckPL pl && ckCF cf
-- Checks BalanceSheet >> ProfitLoss >> CashFlow >> Combination

-- |Check connecting Dates & BalanceSheet then all Account
checkCompany :: Company -> Bool
checkCompany cp = checkAccountVec (cp ^. docs)

reduceAccount :: Account -> Account
reduceAccount (Account dB dE bB bE pl cf) = Account dB dE bB1 bE1 pl1 cf1 where
  bB1 = reduceMap bB 
  bE1 = reduceMap bE 
  pl1 = reduceMap pl 
  cf1 = reduceMap cf 

reduceAccountVec :: V.Vector Account -> V.Vector Account
reduceAccountVec = (reduceAccount <$>)

reduceCompany :: Company -> Company
reduceCompany (Company cd af cn d0 sp rt bt) = 
  Company cd af cn (reduceAccountVec d0) sp rt bt

setAccount :: Account -> Account
setAccount (Account dB dE bB bE pl cf) = Account dB dE bB1 bE1 pl1 cf1 where
  bB1 = setMap bB 
  bE1 = setMap bE 
  pl1 = setMap pl 
  cf1 = setMap cf 

setAccountVec :: V.Vector Account -> V.Vector Account
setAccountVec = (setAccount <$>)

setCompany :: Company -> Maybe Company
setCompany (Company cd af cn d0 sp rt bt) = do 
  let d1 = setAccountVec d0
  d2 <- intpAccount d1

  return (Company cd af cn d2 sp rt bt)

cleanNSetAccount :: Account -> Account
cleanNSetAccount = setAccount . cleanAccount

cleanNSetAccountVec :: V.Vector Account -> V.Vector Account
cleanNSetAccountVec = (cleanNSetAccount <$>)

-- |Interpolate missing Financial Statements
intpAccount :: V.Vector Account -> Maybe (V.Vector Account)
intpAccount v0 = do
  let v1 = v0 -- TODO: Add your code here
  return $ cleanNSetAccountVec v1

-- | Sort Account vector by dateEnd. Note no sanctity check of dates are
-- performed here. If needed, plz use checkAccounts for check of 
-- consistency of dates.
sortAccountVec :: V.Vector Account -> V.Vector Account
sortAccountVec v = (v!) . snd <$> V.modify (sortBy (comparing fst)) q
  where q = (\i -> (v!i ^. dateEnd,i)) <$> V.fromList [0..(length v-1)]

sortCompanyDocs :: Company -> Company
sortCompanyDocs x = x & docs .~ sortAccountVec (x ^. docs)

sortCheckCompany :: Company -> Maybe Company
sortCheckCompany x = do
  let v = x & docs .~ sortAccountVec (x ^. docs)
  if checkCompany v then Just v else Nothing

findFCFF :: Company -> Maybe Company
findFCFF cp = undefined 

-- |Calculate Beta
findBeta :: Company -> Either String Company
findBeta x = undefined