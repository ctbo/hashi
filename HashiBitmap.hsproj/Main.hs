-- Hashi.hs 
-- a solver for Hashiwokakero puzzles
-- Copyright (C) 2017 by Harald Bögeholz
-- See LICENSE file for license information

{-# LANGUAGE  QuasiQuotes #-}

import System.Environment
import Control.Monad
import Data.List (elemIndex, find, foldl', tails, delete)
import Data.Bits
import Data.Maybe (fromJust)

import Heredoc

-- utility functions ------------

bitOr :: [Bitvector] -> Bitvector
bitOr = foldl' (.|.) 0

bitAnd :: [Bitvector] -> Bitvector
bitAnd = foldl' (.&.) (-1)

(.+) ::(Num t) => (t, t, t) -> (t, t, t) -> (t, t, t)
(x, y, z) .+ (x', y', z') = (x+x', y+y', z+z')

(.-) ::(Num t) => (t, t, t) -> (t, t, t) -> (t, t, t)
(x, y, z) .- (x', y', z') = (x-x', y-y', z-z')

(.*) ::(Num t) => (t, t, t) -> t -> (t, t, t)
(x, y, z) .* f = (x*f, y*f, z*f)

(./) ::(Fractional t) => (t, t, t) -> t -> (t, t, t)
(x, y, z) ./ f = (x/f, y/f, z/f)

(.>=) :: (Ord t) => (t, t, t) -> (t, t, t) -> Bool
(x, y, z) .>= (x', y', z') = x >= x' && y >= y' && z >= z'

argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f (x:xs) = fst $ foldr step (x, f x) xs
  where step a' (a, b) = if f a' > b then (a', f a') else (a, b)

sndOf3 :: (a, b, c) -> b
sndOf3 (_, x, _) = x

-- --------------------------------

type Bitvector = Integer
type Index = (Int, Int, Int)
type Direction = (Int, Int, Int)

pdirections :: [Direction]
pdirections = [ (0,0,1)
              , (0,1,0)
              , (1,0,0)
              ]

mdirections :: [Direction]
mdirections = map (.* (-1)) pdirections

directions :: [Direction]
directions = pdirections ++ mdirections

ndirections = length directions
npdirections = length pdirections

closestNeighbor :: [Index] -> Int -> Index -> Direction -> Maybe Index
closestNeighbor islands m i0 di = find isIsland [i0 .+ (di.*f) | f <- [1..m]]
  where isIsland j = j `elem` islands

-- component-wise max for bounding box
maxIndex :: [Index] -> Index
maxIndex = foldr1 (\(x,y,z) (x',y',z') -> (max x x', max y y', max z z'))

readProblem :: String -> Either String Problem
readProblem s = do
    (_, islands) <- foldM collect ((0,0,0), []) s
    return $ compileProblem islands
  where collect (idx@(l,r,c), islands) ch =
          case ch of
            '\n' -> if c == 0
                       then Right ((l+1, 0, 0), islands)
                       else Right ((l, r+1, 0), islands)
            '.'  -> Right ((l, r, c+1), islands)
            _    -> case elemIndex ch ".123456789abc" of
                      Just n -> Right ((l, r, c+1), (idx,n):islands)
                      _      -> Left $ "Invalid character: '" ++ [ch] 
                                    ++ "' at layer " ++ show l 
                                    ++ ", row " ++ show r 
                                    ++ ", column " ++ show c
                   
type BridgeConfig = [Int] -- number of bridges corresponding to directions
type BridgeList = [(Index, Int)] -- bridge destination and multiplicity

data CompiledIsland = CompiledIsland { iIndex :: Index
                                     , iLabel :: Int
                                     , iBridgeVectors :: [(BridgeList, Bitvector, Bitvector)]
                                     , iFootprint :: Bitvector
                                     } deriving (Eq, Show)

type Problem = [CompiledIsland]    
               
compileProblem :: [(Index, Int)] -> Problem
compileProblem rawIslands = map compileIsland rawIslands
  where islandIndices = map fst rawIslands
        bbox@(bbl,bbr,bbc) = maxIndex islandIndices .+ (1,1,1)
        diameter = maximum [bbl, bbr, bbc]
        bridgeBitsStart = bbl*bbr*bbc
        bridgeBitsOffset i = bridgeBitsStart + 3 * ndirections * fromJust (elemIndex i islandIndices)
        compileIsland (i, nbridges) = CompiledIsland { iIndex = i
                                                     , iLabel = nbridges
                                                     , iBridgeVectors = bvs
                                                     , iFootprint = bitOr $ map sndOf3 bvs
                                                     }
          where neighbors = map (closestNeighbor islandIndices diameter i) directions
                bcs = bridgeConfigurations neighbors nbridges
                bvs = map f bcs
                f bc = (bc2bl bc, bc2bv bc, bc2bv2 bc)
                bc2bl :: BridgeConfig -> BridgeList
                bc2bl = concat . zipWith g neighbors
                g Nothing _ = []
                g (Just _) 0 = []
                g (Just i) n = [(i, n)]
                bc2bv bc = shift (bridges2bits bc) (bridgeBitsOffset i)    -- our bridges
                       .|. bitOr (zipWith3 other bc neighbors [3,4,5,0,1,2]) -- block non-matching neighbors
                       .|. bitOr (zipWith3 drawBridge bc neighbors pdirections)
                       .|. idxbit i
                other :: Int -> Maybe Index -> Int -> Bitvector
                other b n d = case n of
                                Nothing -> 0
                                Just j -> shift (7 `xor` (shift 1 b)) (3*d + bridgeBitsOffset j)
                drawBridge b n d 
                  | b == 0 = 0
                  | otherwise = case n of
                                   Nothing -> 0 -- can't happen
                                   Just ni -> bitOr $ map idxbit $ takeWhile (/= ni) [i .+ (d .* k) | k <- [1..]]
                idxbit (l, r, c) = shift 1 $ (l*bbr+r)*bbc + c
                bc2bv2 bc = bitOr $ idxbit i : map (idxbit.fst) (bc2bl bc)

bridgeConfigurations :: [Maybe t] -> Int -> [BridgeConfig]
bridgeConfigurations [] 0 = [[]]
bridgeConfigurations [] _ = []
bridgeConfigurations (Nothing:bs) n = map (0:) $ bridgeConfigurations bs n
bridgeConfigurations (_:bs) n =  concatMap (\i -> map (i:) $ bridgeConfigurations bs (n-i)) [0..2]

bridges2bits :: [Int] -> Bitvector
bridges2bits [] = 0
bridges2bits (b:bs) = shift (bridges2bits bs) 3 .|. shift 1 b


heuristicSort :: Problem -> Problem
heuristicSort [] = []
heuristicSort [c] = [c]
heuristicSort cs = bestStart : heuristicSort' (iFootprint bestStart) rest
  where allPairs = [(x, y) | t <- tails cs, not (null t), let x = head t
                           , y <- tail t]
        overlap (x, y) = popCount $ iFootprint x .&. iFootprint y
        bestPair = argmax overlap allPairs
        bestStart = argmax (popCount . iFootprint) [fst bestPair, snd bestPair]
        rest = delete bestStart cs
        heuristicSort' :: Bitvector -> Problem -> Problem
        heuristicSort' bv [] = []
        heuristicSort' bv cs = best : heuristicSort' bv' (delete best cs)
          where best = argmax (\c -> popCount (bv .&. iFootprint c)) cs
                bv' = bv .|. iFootprint best
        
-- narrow -------------------------------------------

narrowDown :: Problem -> Problem
narrowDown p = if p' /= p then narrowDown p' else p
  where p' = narrow p

narrow :: Problem -> Problem
narrow cis = narrow' cis and1s 0
  where and1s = map bvAnd cis
        bvAnd ci = bitAnd $ map sndOf3 $ iBridgeVectors ci

narrow' :: Problem -> [Bitvector] -> Bitvector -> Problem
narrow' [] _ _ = []
narrow' (ci:cis) (and1:and1s) and2 = ci {iBridgeVectors = bvLegal} : narrow' cis and1s (and2 .|. bvAnd)
    where constraint = bitOr and1s .|. and2
          bvLegal = filter (\(_,b,_) -> b .&. constraint == 0) $ iBridgeVectors ci
          bvAnd = bitAnd $ map sndOf3 bvLegal
          
-- connected components ------------------------------

ccUpdate :: [Bitvector] -> Bitvector -> [Bitvector]
ccUpdate cc c = foldr f [c] cc
  where f y (x:xs) = if x .&. y /= 0
                     then (x .|. y):xs
                     else x:y:xs

-- solve ------------------------------

data SolvedIsland = SolvedIsland { sIndex :: Index
                                 , sLabel :: Int
                                 , sBridgeList :: BridgeList
                                 , sBV :: Bitvector
                                 } deriving (Eq, Show)

type Solution = [SolvedIsland]

solve :: Problem -> [Solution]
solve = solve' 0 [] [] . heuristicSort

solve' :: Bitvector -> [Bitvector] -> Solution -> Problem -> [Solution]
solve' _ _ sis [] = [sis]
solve' b cc sis (ci:cis) = concatMap recurse next
  where step (bc, bv, bv2) = if b .&. bv == 0 && (null cis || not blocked)
                           then [(b', cc', SolvedIsland (iIndex ci) (iLabel ci) bc (b.|.bv))]
                           else []
           where b' = b .|. bv
                 cc' = ccUpdate cc bv2
                 blocked = head cc' .&. b' == head cc'
        next :: [(Bitvector, [Bitvector], SolvedIsland)]
        next = concatMap step (iBridgeVectors ci)
        recurse :: (Bitvector, [Bitvector], SolvedIsland) -> [Solution]
        recurse (b', cc', si) = solve' b' cc' (si:sis) cis

-- debugging helper ---------------------

showBitvector :: Index -> Bitvector -> String
showBitvector (bbl, bbr, bbc) b = field ++ showIslands 0 (shiftR b (bbl*bbr*bbc))
  where field = concatMap field' [0..bbl-1]
        field' l = concatMap (field'' l) [0..bbr-1] ++ "\n"
        field'' l r = concatMap (field''' l r) [0..bbc-1] ++ "\n"
        field''' l r c = if testBit b ((l*bbr+r)*bbc + c) then "+" else "."
        showIslands :: Int -> Bitvector -> String
        showIslands _ 0 = ""
        showIslands n b = concatMap (showIslands' b) [0..5] 
                        ++ "-- " ++ show n ++ "\n"
                        ++ showIslands (n+1) (shiftR b 18)
        showIslands' :: Bitvector -> Int -> String
        showIslands' b d = concatMap (showIslands'' b d) [0..2] ++ " "
        showIslands'' :: Bitvector -> Int -> Int -> String
        showIslands'' b d i = if testBit b (d*3+i) then show i else "."


-- X3D/HTML output ------------------------------

type X3dVec3d = (Float, Float, Float)

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
 
x3dShowCylinder :: X3dVec3d -> X3dVec3d -> String -> String
x3dShowCylinder center scale rotation
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
           1 -> x3dShowCylinder center scale x3drotRight
           2 -> x3dShowCylinder (center .+ (0,  0.15, 0)) scale x3drotRight
             ++ x3dShowCylinder (center .+ (0, -0.15, 0)) scale x3drotRight
           _ -> ""
    where center = (x3dxform i .+ x3dxform j) ./ 2
          scale = (0.1, 1.5 * fromIntegral (cj-ci) - 0.7, 0.1)

x3dshowBottomBridge :: Index -> Index -> Int -> String
x3dshowBottomBridge i@(_,ri,_) j@(_,rj,_) n = 
      case n of
           1 -> x3dShowCylinder center scale x3drotBottom
           2 -> x3dShowCylinder (center .+ ( 0.15, 0, 0)) scale x3drotBottom
             ++ x3dShowCylinder (center .+ (-0.15, 0, 0)) scale x3drotBottom
           _ -> ""
    where center = (x3dxform i .+ x3dxform j) ./ 2
          scale = (0.1, 1.5 * fromIntegral (rj-ri) - 0.7, 0.1)

x3dshowUpBridge :: Index -> Index -> Int -> String
x3dshowUpBridge i@(li,_,_) j@(lj,_,_) n = 
      case n of
           1 -> x3dShowCylinder center scale x3drotUp
           2 -> x3dShowCylinder (center .+ (0,  0.15, 0)) scale x3drotUp
             ++ x3dShowCylinder (center .+ (0, -0.15, 0)) scale x3drotUp
           _ -> ""
    where center = (x3dxform i .+ x3dxform j) ./ 2
          scale = (0.1, 1.5 * fromIntegral (lj-li) - 0.5, 0.1)

x3dshowBridge :: Index -> Index -> Int -> String
x3dshowBridge i j n = if c /= 0 then x3dshowRightBridge i j n
                      else if r /= 0 then x3dshowBottomBridge i j n
                      else x3dshowUpBridge i j n
  where (_,r,c) = j .- i

x3dshowSolution :: Solution -> String
x3dshowSolution sis = [fileAsString|hashiheader.html|]
                   ++ concatMap island sis
                   ++ [fileAsString|hashifooter.html|]
  where island si = x3dshowText (sIndex si) (show (sLabel si))
                 ++ concatMap (bridge (sIndex si)) (sBridgeList si)
        bridge i (j, n) = if j .>= i
                          then x3dshowBridge i j n
                          else []

-- main ------------------------------

main :: IO ()
main = do
     args <- getArgs
     case args of
          [filename] -> do
                     s <- readFile filename
                     work s filename
          [] -> error "Usage: solve filename\nWill write solution to filename.solution.html"
          _  -> error "Too many arguments."
  where work s basename = case readProblem s of
             Left e -> putStrLn e
             Right p -> do
                   let filename = basename ++ ".solution.html"
                   putStrLn $ "Will write first solution to '" ++ filename ++ "'."
                   let solutions = solve p
                   when (not (null solutions)) $ do
                        writeFile filename $ x3dshowSolution $ head solutions
                        putStrLn $ "Wrote '" ++ filename ++ "'."
                        putStrLn "Counting all solutions ..."
                   putStrLn $ "Total number of solutions: " ++ show (length solutions)
