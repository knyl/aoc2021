module Day2 where

{-# LANGUAGE ViewPatterns #-}

import Data.List
import Prelude

data Instruction = Forward Int | Up Int | Down Int
data Position = SimplePosition Int Int | AimPosition Int Int Int

main = do
  input <- readFile "resources/day2.txt"
  let instructions = map toInstruction (lines input)
  let finalPosition1 = foldl applyPosition (SimplePosition 0 0) instructions
  let finalPosition2 = foldl applyPosition (AimPosition 0 0 0) instructions
  print $ getResult finalPosition1
  print $ getResult finalPosition2

toInstruction :: String -> Instruction
toInstruction (stripPrefix "forward " -> Just number) = Forward (read number)
toInstruction (stripPrefix "up "      -> Just number) = Up (read number)
toInstruction (stripPrefix "down "    -> Just number) = Down (read number)

applyPosition :: Position -> Instruction -> Position
applyPosition (SimplePosition x depth)  (Forward amount) = SimplePosition (x + amount) depth
applyPosition (SimplePosition x depth)  (Up amount)      = SimplePosition x (depth - amount)
applyPosition (SimplePosition x depth)  (Down amount)    = SimplePosition x (depth + amount)
applyPosition (AimPosition x depth aim) (Forward amount) = AimPosition (x + amount) (depth + aim * amount) aim
applyPosition (AimPosition x depth aim) (Up amount)      = AimPosition x depth (aim - amount)
applyPosition (AimPosition x depth aim) (Down amount)    = AimPosition x depth (aim + amount)

getResult :: Position -> Int
getResult (SimplePosition x depth) = x * depth
getResult (AimPosition x depth _)  = x * depth
