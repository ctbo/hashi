-- Hashi.hs 
-- a solver for Hashiwokakero puzzles
-- Copyright (C) 2017 by Harald Bögeholz
-- See LICENSE file for license information

{-# LANGUAGE  QuasiQuotes #-}

module Hashi ( Problem
             , State
             , readProblem
             , is3D
             , solve
             , showStateEPS
             , x3dshowState) where

import Data.Array.IArray
import Control.Monad
import Data.List (find, nub)
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

is3D :: Problem -> Bool
is3D p = ln > 0
  where (_,(ln,_,_)) = bounds p

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
    where ((0, 0, 0), (ln, rn, cn)) = bounds p
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
          top    (l, r, c) = maybeToList $ find islandIndex [(l, rr, c) | rr <- [r-1, r-2 .. 0]]
          right  (l, r, c) = maybeToList $ find islandIndex [(l, r, cc) | cc <- [c+1 .. cn]]
          bottom (l, r, c) = maybeToList $ find islandIndex [(l, rr, c) | rr <- [r+1 .. rn]]
          left   (l, r, c) = maybeToList $ find islandIndex [(l, r, cc) | cc <- [c-1, c-2 .. 0]]
          up     (l, r, c) = maybeToList $ find islandIndex [(ll, r, c) | ll <- [l+1 ..ln]]
          down   (l, r, c) = maybeToList $ find islandIndex [(ll, r, c) | ll <- [l-1, l-2 .. 0]]
          islandIndex i = isIsland (p!i)
          rxing ((l1,r1, c1), s1) ((l2, r2, c2), s2) = 
                ( case (rightNeighbor s1, bottomNeighbor s2) of
                    ([(_, c1', _)], [(_, _, r2')]) -> r2 < r1 && r1 < r2' && c1 < c2 && c2 < c1'
                    _                              -> False
                ) || case (rightNeighbor s1, upNeighbor s2) of
                     ([(_, c1', _)], [(l2', _, _)]) -> l2 < l1 && l1 < l2' && c1 < c2 && c2 < c1'
                     _                              -> False
          bxing ((l1, r1, c1), s1) ((l2, r2, c2), s2) = 
                ( case (bottomNeighbor s1, rightNeighbor s2) of
                    ([(_, _, r1')], [(_, c2', _)]) -> r1 < r2 && r2 < r1' && c2 < c1 && c1 < c2'
                    _ -> False
                ) || case (bottomNeighbor s1, upNeighbor s2) of
                       ([(_, _, r1')], [(l2', _, _)]) -> r1 < r2 && r2 < r1' && l2 < l1 && l1 < l2'
                       _ -> False
          uxing ((l1, r1, c1), s1) ((l2, r2, c2), s2) = 
                ( case (upNeighbor s1, bottomNeighbor s2) of
                  ([(l1', _, _)], [(_,_, r2')]) -> l1 < l2 && l2 < l1' && r2 < r1 && r1 < r2'
                  _ -> False
                ) || case (upNeighbor s1, rightNeighbor s2) of
                     ([(l1', _, _)], [(_, c2', _)]) -> l1 < l2 && l2 < l1' && c2 < c1 && c1 < c2'
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

-- EPS output ------------------------------------

showStateEPS :: State -> String
showStateEPS state = [fileAsString|hashiheader.eps|]
                  ++ concatMap bridges islands
                  ++ concatMap circle islands
    where islands = Map.assocs state
          circle ((_, r, c), island) = show r ++ " " ++ show c ++ " " ++ show (iConstraint island) ++ " circle\n"
          bridges ((_, r, c), island) = right ++ bottom
              where right = g rightB rightNeighbor
                    bottom = g bottomB bottomNeighbor
                    g fB fN = case nub $ map fB $ iBridges island of
                                [n] -> if n>0
                                       then let (_, r', c') = head (fN island)
                                            in show r ++ " " ++ show c ++ " " 
                                               ++ show r' ++ " " ++ show c' ++ " "
                                               ++ show n ++ " bridge\n"
                                       else ""
                                _   -> ""

-- X3D/HTML output ------------------------------

type X3dVec3d = (Float, Float, Float)

(.+) :: X3dVec3d -> X3dVec3d -> X3dVec3d
(x, y, z) .+ (x', y', z') = (x+x', y+y', z+z')

(./) :: X3dVec3d -> Float -> X3dVec3d
(x, y, z) ./ f = (x/f, y/f, z/f)

x3dxform :: Index -> X3dVec3d
x3dxform (l, r, c) = (3.0 * fromIntegral c, -3.0 * fromIntegral r, 3.0 * fromIntegral l)

x3dshowVec3d :: X3dVec3d -> String
x3dshowVec3d (x, y, z) = "\"" ++ show x ++ " " ++ show y ++ " " ++ show z ++ "\""

x3dshowindex :: Index -> String
x3dshowindex = x3dshowVec3d . x3dxform

x3dshowText :: Index -> String -> String
x3dshowText i t  =  "<transform translation=" ++ x3dshowindex i ++ ">\n"
                ++ "  <shape>\n"
                ++ "    <appearance><material diffuseColor=\"0 0 1\"></material></appearance>\n"
                ++ "    <text string=\"" ++ t ++ "\" solid=\"false\" />\n"
                ++ "  </shape>\n"
                ++ "</transform>\n"

x3drotRight :: String
x3drotRight = "0 0 1 1.5707963"

x3drotBottom :: String
x3drotBottom = "0 0 1 0"

x3drotUp :: String
x3drotUp = "1 0 0 1.5707963"

x3dshowBridge :: X3dVec3d -> X3dVec3d -> String -> String
x3dshowBridge center scale rotation
    = "<transform translation=" ++ x3dshowVec3d center
       ++ " scale="++ x3dshowVec3d scale 
       ++ " rotation=\"" ++ rotation ++ "\">\n"
   ++ "  <shape> \n"
   ++ "    <appearance><material diffuseColor=\".5 .5 .5\"></material></appearance> \n"
   ++ "    <cylinder/>\n"
   ++ "  </shape> \n"
   ++ "</transform>\n"

x3dshowRightBridge :: Index -> Index -> Int -> String
x3dshowRightBridge i@(_,_,ci) j@(_,_,cj) n = 
      case n of
           1 -> x3dshowBridge center scale x3drotRight
           2 -> x3dshowBridge (center .+ (0,  0.15, 0)) scale x3drotRight
             ++ x3dshowBridge (center .+ (0, -0.15, 0)) scale x3drotRight
           _ -> ""
    where center = (x3dxform i .+ x3dxform j) ./ 2
          scale = (0.1, 1.5 * fromIntegral (cj-ci) - 0.7, 0.1)

x3dshowBottomBridge :: Index -> Index -> Int -> String
x3dshowBottomBridge i@(_,ri,_) j@(_,rj,_) n = 
      case n of
           1 -> x3dshowBridge center scale x3drotBottom
           2 -> x3dshowBridge (center .+ ( 0.15, 0, 0)) scale x3drotBottom
             ++ x3dshowBridge (center .+ (-0.15, 0, 0)) scale x3drotBottom
           _ -> ""
    where center = (x3dxform i .+ x3dxform j) ./ 2
          scale = (0.1, 1.5 * fromIntegral (rj-ri) - 0.7, 0.1)

x3dshowUpBridge :: Index -> Index -> Int -> String
x3dshowUpBridge i@(li,_,_) j@(lj,_,_) n = 
      case n of
           1 -> x3dshowBridge center scale x3drotUp
           2 -> x3dshowBridge (center .+ (0,  0.15, 0)) scale x3drotUp
             ++ x3dshowBridge (center .+ (0, -0.15, 0)) scale x3drotUp
           _ -> ""
    where center = (x3dxform i .+ x3dxform j) ./ 2
          scale = (0.1, 1.5 * fromIntegral (lj-li) - 0.5, 0.1)

x3dshowState :: State -> String
x3dshowState state =  [fileAsString|hashiheader.html|]
                   ++ concatMap number islands
                   ++ concatMap bridges islands
                   ++ [fileAsString|hashifooter.html|]
    where islands =  Map.assocs state
          number (i, island) =  x3dshowText i $ show (iConstraint island)
          bridges (i, island) = right ++ bottom ++ up
            where right = g rightB rightNeighbor x3dshowRightBridge
                  bottom = g bottomB bottomNeighbor x3dshowBottomBridge
                  up = g upB upNeighbor x3dshowUpBridge
                  g fB fN fX = case nub $ map fB $ iBridges island of
                                 [n] -> if n > 0
                                        then fX i (head (fN island)) n
                                        else ""
                                 _   -> ""

