import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List (sort)
import Data.Ord 
import Data.Set (Set, fromList, member, disjoint)
import GHC.Read (list)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [ qcProps]

allPairs:: [(Int,Int)]
allPairs = concat [
  [(x,z) |  x <- [0..(z-1)] ] ++ [(z,y)| y <- [0..z] ] | 
  z <- [0..]
  ]

first100 = take 10000  allPairs

set100 = fromList first100

myListDisjoint :: Ord a => [a] -> [a] -> Bool 
myListDisjoint [] ys = True
myListDisjoint xs [] = True 
myListDisjoint (x:xs) (y:ys)
            | x == y = False
            | x < y = myListDisjoint xs (y:ys)
            | x> y = myListDisjoint (x:xs) ys

myListDisjoint _ _ = error "Unexpected case"

myListDisjointSort :: Ord a => [a] -> [a] -> Bool 
myListDisjointSort xs ys = myListDisjoint (sort (xs)) (sort (ys))

setListDisjoint:: Ord a => [a] -> [a] -> Bool
setListDisjoint xs ys =  disjoint (fromList xs ) (fromList  ys)

-- Exercise E
cubeRoot :: Integer -> Integer

cubeRoot x = round((fromInteger x)**(1/3))


isCube  :: Integer -> Bool

isCube x=  let y = cubeRoot x
            in (y*y*y) == x



ramaPairs :: Integer -> [(Integer, Integer)]

ramaPairs x = [
  (a, b ) | a<- [0.. cubeRoot x ], b<-[cubeRoot (x-a*a*a)], a^3+b^3==x, a<= b    ]


rama  = [x | x<- [0..] , length(ramaPairs x)>= 2]


-- Exercise H 
myTake :: Int -> [a] -> [a]
myTake n [] = []
myTake 0 xs = []
myTake n (x:xs) 
              | n>0 =  x : (myTake (n-1) xs)
              | otherwise = error "unexpected"


qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "Example tuples included in all pairs" $
      \x y  -> and [x >= 0, x<=50 ,y>=0, y<=50  ] QC.==>  member (x,y)  set100,
      QC.testProperty "Two definitions of disjoint agree" $
      (\list1 list2 -> (myListDisjointSort (list1::[Int]) (list2::[Int])) == (setListDisjoint list1 list2)),

      QC.testProperty  "two definitions of take should match" (\list1 n -> (n>=0) QC.==> (myTake (n::Int) (list1::[Int])) == (take n list1)   )
      
      ]

unitTests = testGroup "Unit tests"
  [ 
    testCase "First rama pairs" $
      ((ramaPairs 1729) @?= [(1,12), (9,10)])  ,
    
    testCase "First Ramanujan number" $
      rama!!0 `compare` 1729 @?= EQ,
    testCase "Second Ramanujan number" $
      rama!!1 `compare` 4104 @?= EQ

  ]
