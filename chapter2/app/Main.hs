module Main where
import Data.Char 
import Text.Read (Lexeme(String))



-- combine :: String -> [String] -> [String]
-- combine x y
--      | null x  = []
--      | x/=[]  = ([head x] ++ head y) : combine (tail x) (tail y)

-- doublemap x = map( map x)

-- upper_first :: String -- ^ 
--   -> String
-- upper_first x = map head (doublemap toUpper (words x))

-- lower_last :: String -> [String]
-- lower_last x = map tail (words x)

-- modernize :: String -> String
-- modernize x = unwords ( combine (upper_first x)   (lower_last x))

-- result = modernize "the morphology of prex - an essay in meta-algorithmics"

-- --result = combine ['a'] ["bcd"]


-- first :: (a-> Bool) -> [a] -> Maybe a

-- first  p xs 
--      | null xs = Nothing 
--      | otherwise = Just (head  (filter p  xs))

-- greaterthanzero x = x>0


myexp :: Integer  ->  Integer  -> Integer 
myexp x n | n == 0 = 1
          | n == 1 = x
          | n < 0 = error "negative"
          | even n =  let val = myexp x (div n 2) in val * val
          | odd  n =  x* myexp x (n-1)
          | otherwise = error "unexpected"


months = ["blank", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]


lastchar :: String -> Char 
lastchar  = head .reverse 

addsuffix :: String -> String 
addsuffix x 
        | lastchar x == '1' = x ++ "st"
        | lastchar x == '2' = x ++ "nd"
        | lastchar x == '3' = x ++ "rd"
        | otherwise = x ++ "th"


showDate :: (Int , Int , Int ) -> String 
showDate (x,y,z) = addsuffix(show x ) ++" " ++ (months !! y)  ++ ", " ++show z


type CIN = String

makechunk :: [a] -> [[a]]
makechunk xs 
           | null xs = []
           | otherwise = [[head xs ]] ++ makechunk ( tail xs)

toInts:: CIN -> [Int]

toInts x = map read  (makechunk x) :: [Int]

addSum:: CIN -> CIN
addSum x = show(sum(toInts x))


valid :: CIN -> Bool 
valid x = addSum(take 8 x)  == reverse(take 2 (reverse x))
           

-- result = show (valid "6324513428")
result = show (valid "6324513429")

prepare :: String -> String

prepare = map toLower . (filter (isAlpha ) ) 
palindrome :: String -> Bool
palindrome x =  let y= prepare x in  y == reverse y


main :: IO ()
-- main = putStrLn (showDate (21,11,2020)  )
-- main = putStrLn (show (myexp 2 5))
--main = putStrLn  result
--main = putStrLn (first greaterthanzero [-1, 0 ,1 ,2 ])
-- main = do {
--      case first greaterthanzero [-1,0,1,2] of
--           Just n -> putStrLn "got result"
--           Nothing -> putStrLn "No result"
-- }
-- main = putStrLn result
main = do {
    putStrLn "enter a string:";
    dat <- getLine;
    putStrLn  ( if palindrome dat then "Yes!" else "No!")
}