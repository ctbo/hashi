-- Hashi.hs 
-- a solver for Hashiwokakero puzzles
-- Copyright (C) 2013 by Harald Bögeholz
-- See LICENSE file for license information

{-# LANGUAGE  QuasiQuotes #-}

module Hashi ( Problem
             , State
             , readProblem
             , solve
             , showStateEPS) where

import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
import Data.List (find, nub)
import Data.Maybe (maybeToList)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Heredoc

data Field = Water | Island Int deriving (Eq)
instance Show Field where
    show Water = "."
    show (Island x) = show x

isIsland :: Field -> Bool
isIsland (Island _) = True
isIsland _          = False

readField :: Char -> Either String Field
readField '.' = Right Water
readField '1' = Right $ Island 1
readField '2' = Right $ Island 2
readField '3' = Right $ Island 3
readField '4' = Right $ Island 4
readField '5' = Right $ Island 5
readField '6' = Right $ Island 6
readField '7' = Right $ Island 7
readField '8' = Right $ Island 8
readField c = Left $ "Invalid character " ++ show c ++ "."

type ProblemList = [[Field]]

readProblemList ::  String -> Either String ProblemList
readProblemList = (mapM . mapM) readField . lines

type Index = (Int, Int)
type Problem = Array Index Field

readProblem :: String -> Either String Problem
readProblem s = do
            pl <- readProblemList s
            when (null pl) $ Left "Problem is empty."
            let columns = length $ head pl
            when (columns == 0) $ Left "Problem starts with an empty line."
            unless (all ((== columns) . length) pl) $ Left "Problem not rectangular."
            let rows = length pl
            return $ listArray ((0, 0), (rows-1, columns-1)) $ concat pl 

data Bridges = Bridges { topB :: Int
                       , rightB :: Int
                       , bottomB :: Int
                       , leftB :: Int
                       } deriving (Eq, Show)

allBridges :: [Bridges]
allBridges = [Bridges t r b l | t <- [0 .. 2]
                              , r <- [0 .. 2]
                              , b <- [0 .. 2]
                              , l <- [0 .. 2]]

nBridges :: Int -> [Bridges]
nBridges n = filter f allBridges
    where f (Bridges t r b l) = t+r+b+l == n

data IslandState = IslandState { iConstraint :: Int
                               , topNeighbor :: [Index]
                               , rightNeighbor :: [Index]
                               , bottomNeighbor :: [Index]
                               , leftNeighbor :: [Index]
                               , rightXings :: [Index] -- islands whose bottom bridges cross with our right
                               , bottomXings :: [Index] -- islands whose right bridges cross with our bottom
                               , iBridges :: [Bridges]
                               } deriving (Eq, Show)
type State = Map.Map Index IslandState

stateFromProblem :: Problem -> State
stateFromProblem p = state
    where ((0, 0), (rn, cn)) = bounds p
          state = Map.fromList $ map f islands
          islands = [e | e@(_, Island _) <- assocs p]
          f (i, Island n) = (i, newisland)
              where newisland = IslandState n (top i) (right i) (bottom i) (left i) rx bx bridges
                    bridges = filter h $ nBridges n
                    h (Bridges t r b l) =  (not (null (topNeighbor newisland)) || t == 0)
                                        && (not (null (rightNeighbor newisland)) || r == 0)
                                        && (not (null (bottomNeighbor newisland)) || b == 0)
                                        && (not (null (leftNeighbor newisland)) || l == 0)
                    rx = map fst $ filter (xing (i, newisland)) (Map.assocs state)
                    bx = map fst $ filter (flip xing (i, newisland)) (Map.assocs state)
          f (_, Water) = undefined -- f is only called on islands
          top    (r, c) = maybeToList $ find islandIndex [(rr, c) | rr <- [r-1, r-2 .. 0]]
          right  (r, c) = maybeToList $ find islandIndex [(r, cc) | cc <- [c+1 .. cn]]
          bottom (r, c) = maybeToList $ find islandIndex [(rr, c) | rr <- [r+1 .. rn]]
          left   (r, c) = maybeToList $ find islandIndex [(r, cc) | cc <- [c-1, c-2 .. 0]]
          islandIndex i = isIsland (p!i)
          xing ((r1, c1), s1) ((r2, c2), s2) = case (rightNeighbor s1, bottomNeighbor s2) of
                   ([(_, c1')], [(r2', _)]) -> r2 < r1 && r1 < r2' && c1 < c2 && c2 < c1'
                   _                        -> False

narrow :: Set.Set Index -> State -> [State]
narrow seed state = if Set.null seed then [state] else result
    where (i, seed') = Set.deleteFindMin seed
          island = state Map.! i
          bs = iBridges island
          bs' = filter (noxings rightB rightXings bottomB)
              $ filter (noxings bottomB bottomXings rightB)
              $ filter (match topB topNeighbor bottomB)
              $ filter (match rightB rightNeighbor leftB)
              $ filter (match bottomB bottomNeighbor topB)
              $ filter (match leftB leftNeighbor rightB) bs
          result = if null bs'
                   then []
                   else if bs == bs' 
                        then narrow seed' state
                        else let newSeeds = Set.fromList $ concatMap ($island) 
                                                                     [topNeighbor, rightNeighbor
                                                                     , bottomNeighbor, leftNeighbor
                                                                     , rightXings, bottomXings]
                             in narrow (Set.union seed' newSeeds)
                                       (Map.insert i (island {iBridges = bs'}) state)
          match thisB neighbor otherB b = case neighbor island of
                []   -> True
                [i'] -> thisB b `elem` (map otherB $ iBridges $ state Map.! i')
                _    -> undefined -- there is at most one neighbor
          noxings thisB others otherB b = thisB b == 0
                || all (\o -> 0 `elem` map otherB (iBridges (state Map.! o))) (others island)

narrowAll :: State -> [State]
narrowAll s = narrow (Set.fromList (Map.keys s)) s

connectedComponents :: State -> [Set.Set Index]
connectedComponents state = cc [] Set.empty (Set.fromList (Map.keys state))
    where cc cs seed unvisited = 
             if Set.null seed
             then if Set.null unvisited
                  then cs
                  else let (seed', unvisited') = Set.deleteFindMin unvisited
                       in cc (Set.empty : cs) (Set.singleton seed') unvisited'
             else cc (Set.insert v (head cs) : tail cs) seed'' unvisited''
                  where (v, seed''') = Set.deleteFindMin seed
                        unvisited'' = Set.delete v unvisited
                        island = state Map.! v
                        conn = Set.fromList $ concatMap (($island).snd) 
                                                        $ filter f [(topB, topNeighbor)
                                                                   , (rightB, rightNeighbor)
                                                                   , (bottomB, bottomNeighbor)
                                                                   , (leftB, leftNeighbor)]
                        f (fB, _) = not $ 0 `elem` (map fB (iBridges island))
                        seed'' = (Set.union seed''' conn) Set.\\ head cs


showStateEPS :: State -> String
showStateEPS state = [fileAsString|hashiheader.eps|]
                   ++ concatMap bridges (Map.assocs state)
                   ++ concatMap circle (Map.assocs state)
    where circle ((r, c), island) = show r ++ " " ++ show c ++ " " ++ show (iConstraint island) ++ " circle\n"
          bridges ((r, c), island) = right ++ down
              where right = g rightB rightNeighbor
                    down = g bottomB bottomNeighbor
                    g fB fN = case nub $ map fB $ iBridges island of
                                [n] -> if n>0
                                       then let (r', c') = head (fN island)
                                            in show r ++ " " ++ show c ++ " " 
                                               ++ show r' ++ " " ++ show c' ++ " "
                                               ++ show n ++ " bridge\n"
                                       else ""
                                _   -> ""

solve :: Problem -> [State]
solve p = concatMap solve' $ narrowAll $ stateFromProblem p

solve' :: State -> [State]
solve' state = case (connectedComponents state, find uncertain $ Map.assocs state) of
               ([_], Nothing) -> [state]
               (_,   Nothing) -> []
               ([_], Just a)  -> solve'' state a
               (cs,  Just a)  -> if all unfinished cs 
                                 then solve'' state a
                                 else []
    where uncertain (_, island) = length (iBridges island) > 1
          unfinished = (any (\i -> length (iBridges (state Map.! i)) > 1)) . Set.elems

solve'' :: State -> (Index, IslandState) -> [State]
solve'' state (i, island) = concatMap f $ iBridges island
    where f b = [Map.insert i (island {iBridges = [b]}) state] >>= narrow seed >>= solve'
          seed = Set.fromList $ concatMap ($island) [topNeighbor, rightNeighbor
                                                    ,bottomNeighbor, leftNeighbor
                                                    ,rightXings, bottomXings
                                                    ]
