import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List (sort, groupBy)
import Data.Ord 
import Data.Set (Set, fromList, member, disjoint)
import GHC.Read (list)
import GHC.Real (reduce)
import Data.IntMap (restrictKeys)
import Data.Bits (Bits(xor))
import Data.Char (isSpace)
import Data.Maybe


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [ qcProps]

type Matrix a = [Row a]
type Row a = [a]
type FMatrix = Matrix Float

addOne :: FMatrix -> FMatrix
addOne m = [ [ val+1 | val <- row ] | row <-  m]

sumMatrix :: FMatrix -> Float

sumMatrix m = sum [sum row| row <- m ]

addMatrixes :: FMatrix -> FMatrix -> FMatrix

addMatrixes a b = [zipWith (+) r1 r2| (r1,r2) <- zip a b   ]

transpose :: FMatrix -> FMatrix
transpose  [] = [[] | x<- [1..]]
transpose (xs:xss) = zipWith (:) xs (transpose xss)


transposev2 :: FMatrix -> FMatrix
transposev2 ([]:_)  =[]  -- case where there are no columns
transposev2 xss = map head xss: transposev2 (map tail xss)

matMul :: FMatrix -> FMatrix ->FMatrix
matMul a b = [ [sum (zipWith (*) ra rbt ) | rbt <- bt  ] | ra <- a  ]
              where bt = transpose b

noDups :: Eq a => [a] -> Bool
noDups [] = True 
noDups (x:xs) = all (/=x) xs &&  noDups xs

noDups2 :: (Eq a, Ord a) => [a] -> Bool 
-- noDups2 [] = True 
noDups2 x =  and  (zipWith (/=) sx ssx    )
                where sx = sort x 
                      ssx = tail sx


nub1 :: Eq a => [a] -> [a] 
nub1 [] = []
nub1 (x:xs) =  x : (nub1 . filter (/=x)) xs 


nub2 :: (Eq a, Ord a) => [a] -> [a]
nub2 = (map head) . (groupBy (==)) . sort

dropWhile2 :: (a -> Bool) -> [a] -> [a]

dropWhile2 p [] = []

dropWhile2 p  (x:xs )
          | (p x)  = dropWhile2 p xs
          | otherwise = (x:xs)

takeWhile2   :: (a -> Bool) -> [a] -> [a]

takeWhile2 p []  = []

takeWhile2 p  (x:xs )
          | (p x)  = x: (takeWhile2 p xs)
          | otherwise = []


whiteSpace :: Char -> Bool 
whiteSpace  = isSpace 
-- whiteSpace x = (x==' ')  || x == '\n'   || x=='\v' || x == '\t'

words2_prep :: String -> [String]



-- words2_prep x =  w1 : words2_prep rest
--       where trimmed = dropWhile2  whitespace x
--             w1 = takeWhile2 (not whiteSpace) trimmed
--             rest=dropWhile2 (not whiteSpace) trimmed

words2_prep [] = []

words2_prep (x:xs)
     | whiteSpace x = (words2_prep . (dropWhile2 whiteSpace)) xs
     | not (whiteSpace x) = takeWhile2  (not . whiteSpace) (x:xs): (words2_prep . dropWhile2 (not . whiteSpace)) (x:xs)  


words2_prep _ =error "unexpected"


words2 = words2_prep 


data Inf a =  II | JI a
    deriving (Eq, Show) 

instance (Ord a) => Ord (Inf a) where
   II <= II = True
   II <= JI a = False 
   JI a <= II = True
   JI a <= JI b = a<=b 




myMin :: [Integer] -> Inf Integer

         

myMin [] = II
myMin (x:xs)  = if JI x<m then JI x else m
      where m = myMin xs
            









qcProps = testGroup "(checked by QuickCheck)"
  [ 

      QC.testProperty  "two definitions of noDups agree" 
      (\list1  -> length (list1:: [Int]) <30 QC.==>  noDups list1 == noDups2 list1   )
      
      ,QC.testProperty  "two definitions of nub agree (ignoring order)" 
      (\list1  -> length (list1:: [Int]) <30 QC.==>  sort(nub1 list1) == nub2 list1   )
      ,QC.testProperty  "two definitions of  words should agree" 
      (\string1  ->   words string1 == words2 string1   )
      ,QC.testProperty  "Min is invariant to order" 
      (\list1  ->   myMin (sort list1) == myMin list1   )
      ]
example_matrix = [[1,2 ],[3,4 ]]

prod_result = [[7,10],[15,22]]

example_list = [1,2,3,4,0, 20]


unitTests = testGroup "Unit tests"
  [ 
    testCase "add one to a matrix" $ addOne example_matrix @?= [[2.0 ,3.0],[4.0,5.0]]  
    ,testCase "sum a matrix" $ sumMatrix example_matrix @?= 10  
    ,testCase "add two matrixes" $ addMatrixes example_matrix example_matrix @?= [[2,4],[6,8]] 
    ,testCase "multiply two matrixes" $ matMul example_matrix example_matrix @?= prod_result 
    ,testCase "two definitions of tranpose agree" $ transpose example_matrix @?=  transposev2 example_matrix
    ,testCase "find the min in an example" $ JI 0 @?=  myMin example_list

  ]
