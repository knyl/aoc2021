module Day1 where

import Prelude

main = do
  input <- readFile "day1.txt"
  let numbers = map (read :: String -> Int) (lines input)
  print $ length $ filter id (isIncreasing numbers)
  print $ length $ filter id (isIncreasing (slidingWindows numbers))


isIncreasing :: [Int] -> [Bool]
isIncreasing (_:[]) = []
isIncreasing (x1:x2:tail) = (x1 < x2) : isIncreasing (x2:tail)

slidingWindows :: [Int] -> [Int]
slidingWindows (_:_:[]) = []
slidingWindows (x1:x2:x3:tail) = x1+x2+x3 : slidingWindows (x2:x3:tail)