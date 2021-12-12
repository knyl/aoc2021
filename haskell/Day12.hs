module Day3 where

import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char
import Prelude
import Debug.Trace
import Data.Maybe

type Graph = Map.Map String [String]
type Path = [String]

main = do
  input <- readFile "resources/day12.txt"
  let edges = map (splitOn "-") (lines input)
  let edgesReverse = map reverse edges
  let graph = Map.fromListWith (++) (mapMaybe toTuple (edges ++ edgesReverse))
  let paths1 = findPaths1 graph
  let paths2 = findPaths2 graph
  print $ length paths1
  print $ length paths2

toTuple :: [a] -> Maybe (a, [a])
toTuple [] = Nothing
toTuple [n1, n2] = Just (n1, [n2])
toTuple _ = Nothing

findPaths1 :: Graph -> [Path]
findPaths1 graph = findPaths1' graph Set.empty [] "start"

findPaths1' :: Graph -> Set.Set String -> Path -> String -> [Path]
findPaths1' _ _ path "end" = [reverse ("end" : path)]
findPaths1' graph visited path node =
  let allNeighbours = Map.findWithDefault [] node graph
      neighbours = filter (canVisit visited) allNeighbours
      updatedVisited = Set.insert node visited
  in concatMap (findPaths1' graph updatedVisited (node:path)) neighbours

canVisit :: Set.Set String -> String -> Bool
canVisit visited node = Set.notMember node visited || all isUpper node


findPaths2 :: Graph -> [Path]
findPaths2 graph = findPaths2' graph Set.empty [] False "start"

findPaths2' :: Graph -> Set.Set String -> Path -> Bool -> String -> [Path]
findPaths2' _ _ path _ "end" = [reverse ("end" : path)]
findPaths2' graph visited path True node =
  let neighbours = filter (canVisit visited) $ getNeighbours graph node
      updatedVisited = Set.insert node visited
  in concatMap (findPaths2' graph updatedVisited (node:path) True) neighbours
findPaths2' graph visited path False node =
  let neighbours = filter (/= "start") $ getNeighbours graph node
      updatedVisited = Set.insert node visited
  in concatMap (\x -> findPaths2' graph updatedVisited (node:path) (secondVisit visited x) x) neighbours

getNeighbours :: Graph -> String -> [String]
getNeighbours graph node = Map.findWithDefault [] node graph

secondVisit :: Set.Set String -> String -> Bool
secondVisit visited node = Set.member node visited && all isLower node