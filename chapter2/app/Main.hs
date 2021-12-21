module Main where
import Data.Char


combine :: [Char] -> [[Char]] -> [[Char]]
combine x y  
     | x==[]  = []
     | x/=[]  = [[head x] ++ head y] ++ combine (tail x) (tail y)

doublemap x = map( map (x))

upper_first :: String -> [Char]
upper_first x = (map head (doublemap toUpper (words x)))

lower_last :: String -> [[Char]]
lower_last x = map tail (words x)

modernize :: String -> String
modernize x = unwords ( combine (upper_first x)   (lower_last x))

result = modernize "the morphology of prex - an essay in meta-algorithmics"

--result = combine ['a'] ["bcd"]



main :: IO ()
main = putStrLn  result
