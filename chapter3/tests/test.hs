import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [ qcProps]

floorSqrt :: Integer -> Integer
floorSqrt = floor .sqrt . fromInteger 

isqrt :: Integer  -> Integer
isqrt x = fst (until m_unit (mShrink x) (mBound x))
              where m_unit (m,n) = ((m+1) >= n)



mBound x = (0,x+10) 

type Interval = (Integer , Integer )

mChoose:: Interval -> Integer 
mChoose (m,n) = (m+n) `div` 2

mShrink x (m,n) 
          | p*p <=x = (p,n) -- In this case p<= sqrt (x)
          | p<0 = error "Does not work on negative numbers"
          | otherwise  = (m,p) -- In this case p> sqrt(x)
          where  p = mChoose (m,n)






qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "two definitions of sqrt agree" $
      \x  -> (x >= 0) QC.==> floorSqrt x ==  isqrt x  ]
