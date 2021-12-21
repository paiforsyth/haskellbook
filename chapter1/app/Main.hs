module Main where

song n = if n==0 then ""
         else song (n-1) ++ "\n" ++ verse n

verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

numbers = ["Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]
lower_numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

phrase1 n  
   | n==1 = "One man went to mow"
   | n>1  = numbers!!n ++ " men went to mow"

line1 n = phrase1 n ++ "\n"


line2 n = "Went to mow a meadow" ++ "\n"

phrase3 n 
   | n==1    = "one man and his dog"
   | n>1    =  lower_numbers!!n ++ " men, " ++ phrase3 (n-1)

full_phrase3 n 
   | n==1    = "One man and his dog"
   | n>1    =  numbers!!n ++ " men, " ++ phrase3 (n-1)

line3 n = full_phrase3 n ++ "\n"

line4   = line2 


main :: IO ()
main = putStrLn (song 5)
