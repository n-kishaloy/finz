{-# LANGUAGE NumericUnderscores, OverloadedStrings, Strict #-}

module Main where

import Numeric.Utils (dot,(+^), (-^), (*^), (/^), DVec)
import qualified Numeric.Utils as Nu
import qualified Numeric.Optima as Op
import Data.Approx

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

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

  


  print $ "Bye"

