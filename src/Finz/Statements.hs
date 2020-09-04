{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Strict, OverloadedStrings, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Finz.Statements
( BalanceSheet (..), ProfitLoss (..), CashFlow (..), Statementz (..)
, Accountz (..), GetAccountz (..), GetStatementz (..), GetRecords (..)
, balShBegin, balShEnd, profLoss, cashFl, mkAccountz, splitAccountz
, BsTyp (..), PlTyp (..), CfTyp (..), Statuz (..)
, BsMap, PlMap, CfMap
, HasStatuz (..), HasRec (..)
, HasDatez (..), HasDateBegin (..), HasDateEnd (..), HasBalanceSheetBegin (..)
, HasBalanceSheetEnd (..), HasProfitLoss (..), HasCashFlow (..)
, Checker (..), Shaker (..), CheckShake (..), HasChk (..), HasShk (..)
, HasChecker(..), accountzToJson, jsonToAccountz
, FinType, FinStat
, getEOMonth

) where

import GHC.Generics (Generic)
import Data.Hashable
import Data.Time (Day, fromGregorian, parseTimeM, defaultTimeLocale, toGregorian)
import qualified Data.HashMap.Strict as Hm

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Read (rational)
import Data.Text.Encoding (encodeUtf8)

import Data.List (foldl')
import qualified Data.Aeson as As
import Data.Scientific (toRealFloat)

import Utilz.Numeric (Approx (..))

import Debug.Trace (trace, traceM)

import Control.Monad (forM)
import Control.Lens
-- import Control.Lens.TH

debug = flip trace


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

class (Show a, Eq a, Hashable a, Ord a) => FinType a 
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
  { profitLossDateBegin     :: Day
  , profitLossDateEnd       :: Day
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
  { cashFlowDateBegin       :: Day
  , cashFlowDateEnd         :: Day
  , cashFlowStatuz          :: Statuz
  , cashFlowRec             :: CfMap
  } deriving (Show, FinStat)

makeFields ''CashFlow

data Statementz = Statementz
  { statementzDateBegin         :: Day
  , statementzDateEnd           :: Day
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
  x =~ y = 
    ( (x ^. datez)  ==  (y ^. datez)   &&
      (x ^. statuz) ==  (y ^. statuz) &&
      (x ^. rec)    =~  (y ^. rec)
    ) 

instance Approx ProfitLoss where 
  x =~ y = 
    ( (x ^. dateBegin)  ==  (y ^. dateBegin)  &&
      (x ^. dateEnd)    ==  (y ^. dateEnd)    &&
      (x ^. statuz)     ==  (y ^. statuz)     &&
      (x ^. rec)        =~  (y ^. rec)
    ) 

instance Approx CashFlow where 
  x =~ y = 
    ( (x ^. dateBegin)  ==  (y ^. dateBegin)  &&
      (x ^. dateEnd)    ==  (y ^. dateEnd)    &&
      (x ^. statuz)     ==  (y ^. statuz)     &&
      (x ^. rec)        =~  (y ^. rec)
    ) 

instance Approx Statementz where 
  x =~ y = undefined

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
  (!!~) x r = x & rec .~ (Hm.fromList r)
  (!!+) x (k,v) = if v =~ 0.0 then x else 
    x & rec .~ (Hm.insertWith (\nw ol -> nw+ol) k v (x^.rec))
  (!!%) x (k,v) = if v =~ 0.0 then x else 
    x & rec .~ (Hm.insert k v (x^.rec))
  recToList x = Hm.toList (x^.rec)

instance GetRecords ProfitLoss PlTyp where
  (!!>) x t = Hm.lookupDefault 0.0 t (x^.rec)
  (!!?) x t = Hm.lookup t (x^.rec)
  (!!~) x r = x & rec .~ (Hm.fromList r)
  (!!+) x (k,v) = if v =~ 0.0 then x else 
    x & rec .~ (Hm.insertWith (\nw ol->nw+ol) k v (x^.rec))
  (!!%) x (k,v) = if v =~ 0.0 then x else 
    x & rec .~ (Hm.insert k v (x^.rec))
  recToList x = Hm.toList (x^.rec)

instance GetRecords CashFlow CfTyp where
  (!!>) x t = Hm.lookupDefault 0.0 t (x^.rec)
  (!!?) x t = Hm.lookup t (x^.rec)
  (!!~) x r = x & rec .~ (Hm.fromList r)
  (!!+) x (k,v) = if v =~ 0.0 then x else 
    x & rec .~ (Hm.insertWith (\nw ol -> nw+ol) k v (x^.rec))
  (!!%) x (k,v) = if v =~ 0.0 then x else 
    x & rec .~ (Hm.insert k v (x^.rec))
  recToList x = Hm.toList (x^.rec)

instance FinType a => Approx (Hm.HashMap a Double) where
  x =~ y = (fz x y) && (fz y x) where
    fz p q = foldl' (f p) True $ Hm.toList q
    f p t z = t && ((Hm.lookupDefault 0.0 k p) =~ v) where (k,v) = z 

bsTypMap = Hm.fromList $ zip ((T.pack . show) <$> xf) xf 
  where xf = enumFrom minBound::[BsTyp] 

plTypMap = Hm.fromList $ zip ((T.pack . show) <$> xf) xf 
  where xf = enumFrom minBound::[PlTyp] 

cfTypMap = Hm.fromList $ zip ((T.pack . show) <$> xf) xf 
  where xf = enumFrom minBound::[CfTyp] 

data Accountz = Accountz
  { accountzDateBegin         :: Day
  , accountzDateEnd           :: Day
  , accountzBalanceSheetBegin :: Maybe BsMap
  , accountzBalanceSheetEnd   :: Maybe BsMap
  , accountzProfitLoss        :: Maybe PlMap
  , accountzCashFlow          :: Maybe CfMap
  } deriving (FinStat)

makeFields ''Accountz

class FinType a => GetAccountz a where
  (!>>) :: Accountz -> a -> Maybe Double             -- Get
  (!^>) :: Accountz -> a -> Maybe Double
  (!^>) = (!>>)

  (!>~) :: Accountz -> [(a,Double)] -> Accountz      -- Set/Reset
  (!^~) :: Accountz -> [(a,Double)] -> Accountz      -- Set/Reset
  (!^~) = (!>~)

  (!>+) :: Accountz -> (a,Double) -> Maybe Accountz  -- Add/Create
  (!^+) :: Accountz -> (a,Double) -> Maybe Accountz  -- Add/Create
  (!^+) = (!>+)

  (!>%) :: Accountz -> (a,Double) -> Maybe Accountz  -- Upd/Create
  (!^%) :: Accountz -> (a,Double) -> Maybe Accountz  -- Upd/Create
  (!^%) = (!>%)

  addToEndItems :: Accountz -> [(a,Double)] -> Maybe Accountz  -- List Add
  x `addToEndItems` [] = return x
  x `addToEndItems` (y:ys) = x !>+ y >>= (`addToEndItems` ys)

  addToBeginItems :: Accountz -> [(a,Double)] -> Maybe Accountz  -- List Add
  x `addToBeginItems` [] = return x
  x `addToBeginItems` (y:ys) = x !^+ y >>= (`addToBeginItems` ys)

  updateEndItems :: Accountz -> [(a,Double)] -> Maybe Accountz  -- List Upd
  x `updateEndItems` [] = return x
  x `updateEndItems` (y:ys) = x !>% y >>= (`updateEndItems` ys)

  updateBeginItems :: Accountz -> [(a,Double)] -> Maybe Accountz  -- List Upd
  x `updateBeginItems` [] = return x
  x `updateBeginItems` (y:ys) = x !^% y >>= (`updateBeginItems` ys)

  stringToTyp :: Text -> Maybe a

  recToJSON :: Show a =>  Hm.HashMap a Double -> Text
  recToJSON s = T.pack (concat ["{",ps,"}"]) where
    _:ps = foldl' f "" $ Hm.toList s
    f v (x,y) = if y =~ 0.0 then v else concat [v,",\"",show x,"\":", show y] 

  jRec :: (Hashable a, Eq a) => As.Object -> Maybe (Hm.HashMap a Double)
  jRec x = (foldl' f (Just []) (Hm.toList x)) >>= return . Hm.fromList
    where
    f :: GetAccountz a => Maybe [(a,Double)] -> (Text,As.Value) -> Maybe [(a,Double)]
    f (Just x) (u, As.Number v) = case stringToTyp u of 
      Just p -> Just $ (p,(toRealFloat v)::Double):x 
      otherwise -> Nothing
    f _ _ = Nothing

  jsonToRec :: (Hashable a, Eq a) => Text -> Maybe (Hm.HashMap a Double)
  jsonToRec s = As.decodeStrict (encodeUtf8 s) >>= jRec

instance Show Accountz where
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
      "************** ACCOUNTZ ***********\n\n" ++
      "Begin Date : " ++ (show (y ^. dateBegin)) ++ "\n" ++
      "End Date   : " ++ (show (y ^. dateEnd)) ++ "\n" ++
      "\n******* Balance Sheet Begin *******\n" ++ 
      (prx $ y ^. balanceSheetBegin) ++
      "\n******** Balance Sheet End ********\n" ++ 
      (prx $ y ^. balanceSheetEnd) ++
      "\n*********** Profit Loss ***********\n" ++ 
      (prx $ y ^. profitLoss) ++
      "\n************ Cash Flow ************\n" ++ 
      (prx $ y ^. cashFlow) ++
      "\n*************** END ***************\n"

-- DEPRECATED : This is Just proof of concept -- Not to be used
prnx :: Accountz -> String
prnx y = 
  let 
    prx :: FinType a => Maybe (Hm.HashMap a Double) -> Maybe String
    prx Nothing = Just "Nothing\n"
    prx (Just x)=concat <$> (forM (Hm.toList x) $ \u -> return $ (show u)++"\n")
  in 
    "************** ACCOUNTZ ***********\n\n" ++
    "Begin Date : " ++ (show (y ^. dateBegin)) ++ "\n" ++
    "End Date   : " ++ (show (y ^. dateEnd)) ++ "\n" ++ 
    (let Just v = prx $ y ^. balanceSheetBegin in v) ++
    (let Just v = prx $ y ^. balanceSheetEnd in v) 


instance Approx Accountz where
  x =~ y = 
    ( (x ^. dateBegin)          ==  (y ^. dateBegin)          &&
      (x ^. dateEnd)            ==  (y ^. dateEnd)            &&
      (x ^. balanceSheetBegin)  =~  (y ^. balanceSheetBegin)  &&
      (x ^. balanceSheetEnd)    =~  (y ^. balanceSheetEnd)    &&
      (x ^. profitLoss)         =~  (y ^. profitLoss)         &&
      (x ^. cashFlow)           =~  (y ^. cashFlow)
    )

instance GetAccountz BsTyp where
  (!^>) x t = do p <- x^.balanceSheetBegin; return $ Hm.lookupDefault 0.0 t p
  (!>>) x t = do p <- x ^. balanceSheetEnd; return $ Hm.lookupDefault 0.0 t p

  (!^~) x r = x & balanceSheetBegin .~ (Just (Hm.fromList r))
  (!>~) x r = x & balanceSheetEnd .~ (Just (Hm.fromList r))

  (!^+) x (k,v) = if v =~ 0.0 then (Just x) else do 
    p <- (x ^. balanceSheetBegin)
    return $ x&balanceSheetBegin .~(Just (Hm.insertWith (\nw ol->nw+ol) k v p))

  (!>+) x (k,v) = if v =~ 0.0 then (Just x) else do 
    p <- (x ^. balanceSheetEnd)
    return $ x & balanceSheetEnd .~ (Just (Hm.insertWith (\nw ol->nw+ol) k v p))

  (!^%) x (k,v) = if v =~ 0.0 then (Just x) else do 
    p <- (x ^. balanceSheetBegin)
    return $ x & balanceSheetBegin .~ (Just (Hm.insert k v p))

  (!>%) x (k,v) = if v =~ 0.0 then (Just x) else do 
    p <- (x ^. balanceSheetEnd)
    return $ x & balanceSheetEnd .~ (Just (Hm.insert k v p))

  stringToTyp s = Hm.lookup s bsTypMap

instance GetAccountz PlTyp where
  (!>>) x t = do p <- x^.profitLoss; return $ Hm.lookupDefault 0.0 t p
  (!>~) x r = x & profitLoss .~ (Just (Hm.fromList r))

  (!>+) x (k,v) = if v =~ 0.0 then (Just x) else do 
    p <- (x ^. profitLoss)
    return $ x & profitLoss .~ (Just (Hm.insertWith (\nw ol->nw+ol) k v p))

  (!>%) x (k,v) = if v =~ 0.0 then (Just x) else do 
    p <- (x ^. profitLoss)
    return $ x & profitLoss .~ (Just (Hm.insert k v p))

  stringToTyp s = Hm.lookup s plTypMap

instance GetAccountz CfTyp where
  (!>>) x t = do p <- x^.cashFlow; return $ Hm.lookupDefault 0.0 t p
  (!>~) x r = x & cashFlow .~ (Just (Hm.fromList r))

  (!>+) x (k,v) = if v =~ 0.0 then (Just x) else do 
    p <- (x ^. cashFlow)
    return $ x & cashFlow .~ (Just (Hm.insertWith (\nw ol->nw+ol) k v p))

  (!>%) x (k,v) = if v =~ 0.0 then (Just x) else do 
    p <- (x ^. cashFlow)
    return $ x & cashFlow .~ (Just (Hm.insert k v p))

  stringToTyp s = Hm.lookup s cfTypMap

balShBegin :: Accountz -> Maybe BalanceSheet
balShBegin x = (x ^. balanceSheetBegin) >>= 
  return . BalanceSheet (x ^. dateBegin) Actual 
  
balShEnd :: Accountz -> Maybe BalanceSheet
balShEnd x = (x ^. balanceSheetEnd) >>= 
  return . BalanceSheet (x ^. dateEnd) Actual

profLoss :: Accountz -> Maybe ProfitLoss
profLoss x = (x ^. profitLoss) >>= 
  return . ProfitLoss (x ^. dateBegin) (x ^. dateEnd) Actual

cashFl :: Accountz -> Maybe CashFlow
cashFl x = (x ^. cashFlow) >>= 
  return . CashFlow (x ^. dateBegin) (x ^. dateEnd) Actual

mkAccountz :: Maybe BalanceSheet -> Maybe BalanceSheet -> Maybe ProfitLoss -> Maybe CashFlow -> Maybe Accountz
mkAccountz _ _ Nothing _ = Nothing
mkAccountz bsBeg bsEnd (Just pl) cf = 
  if d1 == dtbs1 && d2 == dtbs2 && d1 == dtcf1 && d2 == dtcf2
  then Just $ Accountz d1 d2 bsBg bsEn (Just (pl^.rec)) cfMp
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

splitAccountz :: Accountz -> (Maybe BalanceSheet, Maybe BalanceSheet, Maybe ProfitLoss, Maybe CashFlow)
splitAccountz x = (balShBegin x, balShEnd x, profLoss x, cashFl x)


accountzToJson :: Accountz -> Text
accountzToJson x = 
  T.concat["{",dbeg,",",dend,",",b1J,",",b2J,",",plJ,",",cfJ,"}"] 
  where
  dbeg = T.concat["\"dateBegin\":\"", T.pack $ show (x ^. dateBegin),"\""]
  dend = T.concat["\"dateEnd\":\"", T.pack $ show (x ^. dateEnd),"\""]

  b1 = x ^. balanceSheetBegin; b1J = T.concat ["\"balanceSheetBegin\":",b1x]
  b1x = case b1 of Nothing -> "null"; Just b1q -> recToJSON b1q

  b2 = x ^. balanceSheetEnd; b2J = T.concat ["\"balanceSheetEnd\":",b2x]
  b2x = case b2 of Nothing -> "null"; Just b2q -> recToJSON b2q

  pl = x ^. profitLoss; plJ = T.concat ["\"profitLoss\":",plx]
  plx = case pl of Nothing -> "null"; Just plq -> recToJSON plq

  cf = x ^. cashFlow; cfJ = T.concat ["\"cashFlow\":",cfx]
  cfx = case cf of Nothing -> "null"; Just cfq -> recToJSON cfq

jsonToAccountz :: Text -> Maybe Accountz
jsonToAccountz s =  do
  let 
    parseObj :: (GetAccountz a, FinType a) => As.Value -> Maybe (Maybe (Hm.HashMap a Double))
    parseObj (As.Object x) = jRec x >>= return . Just 
    parseObj As.Null = Just Nothing
    parseObj _ = Nothing

  rz <- As.decodeStrict (encodeUtf8 s) :: Maybe As.Object
  (As.String dB) <- Hm.lookup "dateBegin" rz; dBeg <- getEOMonth dB
  (As.String dE) <- Hm.lookup "dateEnd" rz; dEnd <- getEOMonth dE
  bsB <- Hm.lookup "balanceSheetBegin" rz >>= parseObj 
  bsE <- Hm.lookup "balanceSheetEnd" rz >>= parseObj 
  plX <- Hm.lookup "profitLoss" rz >>= parseObj 
  cfX <- Hm.lookup "cashFlow" rz >>= parseObj 

  return $ Accountz dBeg dEnd bsB bsE plX cfX

getEOMonth :: Text -> Maybe Day
getEOMonth x = do
  y <- parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack x) :: Maybe Day
  let (u,v,_) = (toGregorian y)
  return $ fromGregorian u v (if v == 6 || v == 9 then 30 else 31) 

class FinStat a => GetStatementz a where
  toJsonz :: a -> String
  fromJsonz :: String -> a

  updateEndStatement :: Accountz -> a -> Maybe Accountz
  updateBeginStatement :: Accountz -> a -> Maybe Accountz
  updateStatement :: Accountz -> a -> Maybe Accountz
  updateBeginStatement = updateEndStatement
  updateStatement = updateEndStatement

instance GetStatementz BalanceSheet where
  updateBeginStatement x s = if x^.dateBegin == s^.datez 
    then Just $ x & balanceSheetBegin .~ (Just (s ^. rec))
    else Nothing

  updateEndStatement x s = if x^.dateEnd == s^.datez 
    then Just $ x & balanceSheetEnd .~ (Just (s ^. rec))
    else Nothing

instance GetStatementz ProfitLoss where
  updateEndStatement x s = if x^.dateBegin == s^.dateBegin && x^.dateEnd == s^.dateEnd
    then Just $ x & profitLoss .~ (Just (s ^. rec))
    else Nothing

instance GetStatementz CashFlow where
  updateEndStatement x s = if x^.dateBegin == s^.dateBegin && x^.dateEnd == s^.dateEnd
    then Just $ x & cashFlow .~ (Just (s ^. rec))
    else Nothing

-- type Trend = [(Day,Double)]

-- data Company = Company
--   { companyCode         ::  Text
--   , companyMarket       ::  [(Text,Double)]
--   , companyConsolidated ::  Bool
--   , companyDocs         ::  [Accountz]
--   , companySharePrices  ::  [(Day,Double)]
--   , companyBetaU        ::  [(Day,Double)]

--   }