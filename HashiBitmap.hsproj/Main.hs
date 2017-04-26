-- Hashi.hs 
-- a solver for Hashiwokakero puzzles
-- Copyright (C) 2017 by Harald Bögeholz
-- See LICENSE file for license information

{-# LANGUAGE  QuasiQuotes #-}

import System.Environment
import Control.Monad
import Data.List (elemIndex, find, foldl', delete, partition)
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
argmax _ [] = error "argmax on empty list"
argmax f (x:xs) = fst $ foldr step (x, f x) xs
  where step a' (a, b) = if f a' > b then (a', f a') else (a, b)

argmin :: (Ord b) => (a -> b) -> [a] -> a
argmin _ [] = error "argmin on empty list"
argmin f (x:xs) = fst $ foldr step (x, f x) xs
  where step a' (a, b) = if f a' < b then (a', f a') else (a, b)

fstOf3 :: (a, b, c) -> a
fstOf3 (x, _, _) = x

sndOf3 :: (a, b, c) -> b
sndOf3 (_, x, _) = x

lengthLT :: Int -> [a] -> Bool
lengthLT n xs = length (take n xs) < n

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

ndirections :: Int
ndirections = length directions

closestNeighbor :: [Index] -> Int -> Index -> Direction -> Maybe Index
closestNeighbor islands m i0 di = find isIsland [i0 .+ (di.*f) | f <- [1..m]]
  where isIsland j = j `elem` islands

-- component-wise max for bounding box
maxIndex :: [Index] -> Index
maxIndex = foldr1 (\(x,y,z) (x',y',z') -> (max x x', max y y', max z z'))

type BridgeConfig = [Int] -- number of bridges corresponding to directions
type BridgeList = [(Index, Int)] -- bridge destination and multiplicity

data CompiledIsland = CompiledIsland { iIndex :: Index
                                     , iLabel :: Int
                                     , iBridgeVectors :: [(BridgeList, Bitvector, Bitvector)]
                                     , iFootprint :: Bitvector
                                     } deriving (Eq, Show)

type Problem = [CompiledIsland] 
-- a solution differs from a problem only in that all iBridgeVectors have exactly one element   
type Solution = [CompiledIsland]
               
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
                   
compileProblem :: [(Index, Int)] -> Problem
compileProblem rawIslands = map compileIsland rawIslands
  where islandIndices = map fst rawIslands
        (bbl,bbr,bbc) = maxIndex islandIndices .+ (1,1,1)
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
                g (Just j) n = [(j, n)]
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
        
-- narrow -------------------------------------------

narrowDown :: Bitvector -> Problem -> Problem
narrowDown bv p = if p' /= p then narrowDown bv p' else p
  where p' = narrow bv p

narrow :: Bitvector -> Problem -> Problem
narrow and2 cis = narrow' cis and1s and2
  where and1s = map bvAnd cis
        bvAnd ci = bitAnd $ map sndOf3 $ iBridgeVectors ci

narrow' :: Problem -> [Bitvector] -> Bitvector -> Problem
narrow' [] _ _ = []
narrow' (_:_) [] _ = error "internal error in narrow': This can't happen."
narrow' (ci:cis) (_:and1s) and2 = ci {iBridgeVectors = bvLegal} : narrow' cis and1s (and2 .|. bvAnd)
    where constraint = bitOr and1s .|. and2
          bvLegal = filter (\(_,b,_) -> b .&. constraint == 0) $ iBridgeVectors ci
          bvAnd = bitAnd $ map sndOf3 bvLegal
          
-- connected components ------------------------------

ccUpdate :: [Bitvector] -> Bitvector -> [Bitvector]
ccUpdate cc c = foldr f [c] cc
  where f _ [] = error "ccUpdate: this can't happen."
        f y (x:xs) = if x .&. y /= 0
                     then (x .|. y):xs
                     else x:y:xs

-- solve ------------------------------

data State = State { stBV :: Bitvector      -- fixed islands
                   , stCC :: [Bitvector]    -- connected components
                   , stSolution :: Solution -- partial solution
                   }

solve :: Problem -> [Solution]
solve p = map stSolution $ solve' (State 0 [] []) p

solve' :: State -> Problem -> [State]
solve' state [] = if length (stCC state) == 1 then [state] else []
solve' state p = if blocked then [] else concatMap next fixedStates
  where blocked = any (\cc -> cc .&. stBV state == cc) $ stCC state
        p' = narrowDown (stBV state) p
        (fixed, open) = partition (lengthLT 2 . iBridgeVectors) p'
        fixedStates = foldM integrate state fixed
        next s = case open of
                   [] -> solve' s []
                   cis -> concatMap (flip solve' rest) $ integrate s smallest
                        where smallest = argmin (length . iBridgeVectors) cis
                              rest = delete smallest cis
                              
integrate :: State -> CompiledIsland -> [State]
integrate state ci = concatMap f $ iBridgeVectors ci
  where f bridges@(_, bv, bv2) = if stBV state .&. bv == 0
                                  then [ State { stBV = stBV state .|. bv
                                               , stCC = ccUpdate (stCC state) bv2
                                               , stSolution = ci { iBridgeVectors = [bridges] } 
                                                            : stSolution state
                                               }
                                       ]
                                  else []

-- debugging helper ---------------------

showBitvector :: Index -> Bitvector -> String
showBitvector (bbl, bbr, bbc) bv = field ++ showIslands 0 (shiftR bv (bbl*bbr*bbc))
  where field = concatMap field' [0..bbl-1]
        field' l = concatMap (field'' l) [0..bbr-1] ++ "\n"
        field'' l r = concatMap (field''' l r) [0..bbc-1] ++ "\n"
        field''' l r c = if testBit bv ((l*bbr+r)*bbc + c) then "+" else "."
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
x3dshowSolution cis = [fileAsString|hashiheader.html|]
                   ++ concatMap island cis
                   ++ [fileAsString|hashifooter.html|]
  where island ci = x3dshowText (iIndex ci) (show (iLabel ci))
                 ++ concatMap (bridge (iIndex ci)) (fstOf3 (head (iBridgeVectors ci)))
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
