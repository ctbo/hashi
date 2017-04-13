-- Hashi.hs 
-- a solver for Hashiwokakero puzzles
-- Copyright (C) 2013 by Harald Bögeholz
-- See LICENSE file for license information

{-# LANGUAGE  QuasiQuotes #-}

module Hashi ( Problem
             , State
             , readProblem
             , layerRange
             , solve
             , showStateEPS) where

import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
import Data.List (find, nub, break)
import Data.Maybe (maybeToList)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Heredoc

data Field = Water | Island Int deriving (Eq)
instance Show Field where
    show Water = "."
    show (Island 10) = "a"
    show (Island 11) = "b"
    show (Island 12) = "c"
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
readField '9' = Right $ Island 9
readField 'a' = Right $ Island 10
readField 'b' = Right $ Island 11
readField 'c' = Right $ Island 12
readField c = Left $ "Invalid character " ++ show c ++ "."

type ProblemList = [[[Field]]]

splitLayers :: String -> [String]
splitLayers cs = case break (== '_') cs of
                   (layer, []) -> [layer]
                   (layer, '_':'\n':rest) -> layer : splitLayers rest


readProblemList ::  String -> Either String ProblemList
readProblemList = (mapM . mapM . mapM) readField . map lines . splitLayers

type Index = (Int, Int, Int)
type Problem = Array Index Field

layerRange :: Problem -> (Int, Int)
layerRange p = (l0, ln)
   where ((l0,_,_),(ln,_,_)) = bounds p

readProblem :: String -> Either String Problem
readProblem s = do
            pl <- readProblemList s
            when (null pl) $ Left "Problem is empty."
            let rows = length $ head pl
            when (rows == 0) $ Left "Problem starts with an empty layer."
            unless (all ((== rows) . length) pl) $ Left "Problem not a cuboid."
            let columns = length $ head $ head pl
            when (columns == 0) $ Left "Problem starts with an empty line."
            unless (all ((== columns) . length) (concat pl)) $ Left "Problem not a cuboid."
            let layers = length pl
            return $ listArray ((0, 0, 0), (layers-1, rows-1, columns-1)) $ concat $ concat pl 


data Bridges = Bridges { topB :: Int
                       , rightB :: Int
                       , bottomB :: Int
                       , leftB :: Int
                       , upB :: Int
                       , downB :: Int
                       } deriving (Eq, Show)

allBridges :: [Bridges]
allBridges = [Bridges t r b l u d | t <- [0 .. 2]
                                  , r <- [0 .. 2]
                                  , b <- [0 .. 2]
                                  , l <- [0 .. 2]
                                  , u <- [0 .. 2]
                                  , d <- [0 .. 2]]

nBridges :: Int -> [Bridges]
nBridges n = filter f allBridges
    where f (Bridges t r b l u d) = t+r+b+l+u+d == n

data IslandState = IslandState { iConstraint :: Int
                               , topNeighbor :: [Index]
                               , rightNeighbor :: [Index]
                               , bottomNeighbor :: [Index]
                               , leftNeighbor :: [Index]
                               , upNeighbor :: [Index]
                               , downNeighbor :: [Index]
                               , rightXings :: [Index] -- islands whose bottom or up bridges cross with our right
                               , bottomXings :: [Index] -- islands whose right or up bridges cross with our bottom
                               , upXings :: [Index] -- islands whose bottom or right bridges cross with our up
                               , iBridges :: [Bridges]
                               } deriving (Eq, Show)
type State = Map.Map Index IslandState

stateFromProblem :: Problem -> State
stateFromProblem p = state
    where ((0, 0, 0), (rn, cn, ln)) = bounds p
          state = Map.fromList $ map f islands
          islands = [e | e@(_, Island _) <- assocs p]
          f (i, Island n) = (i, newisland)
              where newisland = IslandState n (top i) (right i) (bottom i) (left i) (up i) (down i) rx bx ux bridges
                    bridges = filter h $ nBridges n
                    h (Bridges t r b l u d) =  (not (null (topNeighbor newisland)) || t == 0)
                                            && (not (null (rightNeighbor newisland)) || r == 0)
                                            && (not (null (bottomNeighbor newisland)) || b == 0)
                                            && (not (null (leftNeighbor newisland)) || l == 0)
                                            && (not (null (upNeighbor newisland)) || u == 0)
                                            && (not (null (downNeighbor newisland)) || d == 0)
                    rx = map fst $ filter (rxing (i, newisland)) (Map.assocs state)
                    bx = map fst $ filter (bxing (i, newisland)) (Map.assocs state)
                    ux = map fst $ filter (uxing (i, newisland)) (Map.assocs state)
          f (_, Water) = undefined -- f is only called on islands
          top    (r, c, l) = maybeToList $ find islandIndex [(rr, c, l) | rr <- [r-1, r-2 .. 0]]
          right  (r, c, l) = maybeToList $ find islandIndex [(r, cc, l) | cc <- [c+1 .. cn]]
          bottom (r, c, l) = maybeToList $ find islandIndex [(rr, c, l) | rr <- [r+1 .. rn]]
          left   (r, c, l) = maybeToList $ find islandIndex [(r, cc, l) | cc <- [c-1, c-2 .. 0]]
          up     (r, c, l) = maybeToList $ find islandIndex [(r, c, ll) | ll <- [l+1 ..ln]]
          down   (r, c, l) = maybeToList $ find islandIndex [(r, c, ll) | ll <- [l-1, l-2 .. 0]]
          islandIndex i = isIsland (p!i)
          rxing ((r1, c1, l1), s1) ((r2, c2, l2), s2) = 
                ( case (rightNeighbor s1, bottomNeighbor s2) of
                    ([(_, c1', _)], [(r2', _, _)]) -> r2 < r1 && r1 < r2' && c1 < c2 && c2 < c1'
                    _                              -> False
                ) || case (rightNeighbor s1, upNeighbor s2) of
                     ([(_, c1', _)], [(_, _, l2')]) -> l2 < l1 && l1 < l2' && c1 < c2 && c2 < c1'
                     _                              -> False
          bxing ((r1, c1, l1), s1) ((r2, c2, l2), s2) = 
                ( case (bottomNeighbor s1, rightNeighbor s2) of
                    ([(r1', _, _)], [(_, c2', _)]) -> r1 < r2 && r2 < r1' && c2 < c1 && c1 < c2'
                    _ -> False
                ) || case (bottomNeighbor s1, upNeighbor s2) of
                       ([(r1', _, _)], [(_, _, l2')]) -> r1 < r2 && r2 < r1' && l2 < l1 && l1 < l2'
                       _ -> False
          uxing ((r1, c1, l1), s1) ((r2, c2, l2), s2) = 
                ( case (upNeighbor s1, bottomNeighbor s2) of
                  ([(_, _, l1')], [(r2',_, _)]) -> l1 < l2 && l2 < l1' && r2 < r1 && r1 < r2'
                  _ -> False
                ) || case (upNeighbor s1, rightNeighbor s2) of
                     ([(_, _, l1')], [(_, c2', _)]) -> l1 < l2 && l2 < l1' && c2 < c1 && c1 < c2'
                     _ -> False
          
narrow :: Set.Set Index -> State -> [State]
narrow seed state = if Set.null seed then [state] else result
    where (i, seed') = Set.deleteFindMin seed
          island = state Map.! i
          bs = iBridges island
          bs' = filter (noxings rightB rightXings bottomB)
              $ filter (noxings rightB rightXings upB)
              $ filter (noxings bottomB bottomXings rightB)
              $ filter (noxings bottomB bottomXings upB)
              $ filter (noxings upB upXings rightB)
              $ filter (noxings upB upXings bottomB)
              $ filter (match topB topNeighbor bottomB)
              $ filter (match rightB rightNeighbor leftB)
              $ filter (match bottomB bottomNeighbor topB)
              $ filter (match leftB leftNeighbor rightB)
              $ filter (match upB upNeighbor downB)
              $ filter (match downB downNeighbor upB) bs
          result = if null bs'
                   then []
                   else if bs == bs' 
                        then narrow seed' state
                        else let newSeeds = Set.fromList $ concatMap ($island) 
                                                                     [topNeighbor, rightNeighbor
                                                                     , bottomNeighbor, leftNeighbor
                                                                     , upNeighbor, downNeighbor
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
                                                                   , (leftB, leftNeighbor)
                                                                   , (upB, upNeighbor)
                                                                   , (downB, downNeighbor)
                                                                   ]
                        f (fB, _) = not $ 0 `elem` (map fB (iBridges island))
                        seed'' = (Set.union seed''' conn) Set.\\ head cs


showStateEPS :: Int -> State -> String
showStateEPS layer state = [fileAsString|hashiheader.eps|]
                        ++ concatMap bridges islands
                        ++ concatMap circle islands
    where islands = filter (\((l,_,_),_) -> l == layer) $ Map.assocs state
          circle ((_, r, c), island) = show r ++ " " ++ show c ++ " " ++ show (iConstraint island) ++ " circle\n"
          bridges ((_, r, c), island) = right ++ down
              where right = g upB upNeighbor
                    down = g rightB rightNeighbor
                    g fB fN = case nub $ map fB $ iBridges island of
                                [n] -> if n>0
                                       then let (_, r', c') = head (fN island)
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
                                                    , bottomNeighbor, leftNeighbor
                                                    , upNeighbor, downNeighbor
                                                    , rightXings, bottomXings, upXings
                                                    ]
