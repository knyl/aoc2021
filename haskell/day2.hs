module Day2 where

{-# LANGUAGE ViewPatterns #-}

import Data.List
import Prelude

data Instruction = Forward Int | Up Int | Down Int
data Position = Position Int Int | AimPos Int Int Int

main = do
  input <- readFile "resources/day2.txt"
  let instructions = map toInstruction (lines input)
  let finalPosition1 = foldl applyPosition (Position 0 0) instructions
  let finalPosition2 = foldl applyPosition (AimPos 0 0 0) instructions
  print $ getResult finalPosition1
  print $ getResult finalPosition2

toInstruction :: String -> Instruction
toInstruction (stripPrefix "forward " -> Just number) = Forward (read number)
toInstruction (stripPrefix "up " -> Just number) = Up (read number)
toInstruction (stripPrefix "down " -> Just number) = Down (read number)

applyPosition :: Position -> Instruction -> Position
applyPosition (Position x depth) (Forward amount) = Position (x + amount) depth
applyPosition (Position x depth) (Up amount) = Position x (depth - amount)
applyPosition (Position x depth) (Down amount) = Position x (depth + amount)

applyPosition (AimPos x depth aim) (Forward amount) = AimPos (x + amount) (depth + aim * amount) aim
applyPosition (AimPos x depth aim) (Up amount) = AimPos x depth (aim - amount)
applyPosition (AimPos x depth aim) (Down amount) = AimPos x depth (aim + amount)

getResult :: Position -> Int
getResult (Position x depth) = x * depth
getResult (AimPos x depth _) = x * depth
