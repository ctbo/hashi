import Control.Monad
import Data.List (elemIndex, find, foldl')
import Data.Bits
import Data.Maybe (fromJust)

type Bitvector = Integer
type Index = (Int, Int, Int)
type Direction = (Int, Int, Int)
type Problem = [(Index, Int)]

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

-- readProblem :: String -> Either String Problem
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
                      _      -> Left $ "Invalid character: '" ++ [ch] ++ "' at layer " ++ show l ++ ", row " ++ show r ++ ", column " ++ show c
                   
type BridgeConfig = [Int] -- number of bridges corresponding to directions
   
data Island = Island { iIndex :: Index
                     , iLabel :: Int
                     , iBridgeVectors :: [(BridgeConfig, Bitvector)]
                     , iFootprint :: Bitvector
                     } deriving (Show)
                   
-- compileProblem :: [(Index, Int)] -> Problem
compileProblem rawIslands = map compileIsland rawIslands
  where islandIndices = map fst rawIslands
        bbox@(bbl,bbr,bbc) = maxIndex islandIndices .+ (1,1,1)
        diameter = maximum [bbl, bbr, bbc]
        bridgeBitsStart = bbl*bbr*bbc
        bridgeBitsOffset i = bridgeBitsStart + 3 * ndirections * fromJust (elemIndex i islandIndices)
        compileIsland (i, nbridges) = Island { iIndex = i
                                             , iLabel = nbridges
                                             , iBridgeVectors = bvs
                                             , iFootprint = bitOr $ map snd bvs
                                             }
          where neighbors = map (closestNeighbor islandIndices diameter i) directions
                bcs = bridgeConfigurations neighbors nbridges
                bvs = map f bcs
                f bc = (bc, bc2bv bc)
                bc2bv bc = shift (bridges2bits bc) (bridgeBitsOffset i)    -- our bridges
                       .|. bitOr (zipWith3 other bc neighbors [3,4,5,0,1,2]) -- block non-matching neighbors
                       .|. bitOr (zipWith3 drawBridge bc neighbors pdirections)
                other :: Int -> Maybe Index -> Int -> Bitvector
                other b n d = case n of
                                Nothing -> 0
                                Just j -> shift (7 `xor` (shift 1 b)) $ d + bridgeBitsOffset j
                drawBridge b n d 
                  | b == 0 = 0
                  | otherwise = case n of
                                   Nothing -> 0 -- can't happen
                                   Just ni -> bitOr $ map idxbit $ takeWhile (/= ni) [i .+ (d .* k) | k <- [0..]]
                idxbit (l, r, c) = shift 1 $ (l*bbr+r)*bbc + c

bridgeConfigurations :: [Maybe t] -> Int -> [BridgeConfig]
bridgeConfigurations [] 0 = [[]]
bridgeConfigurations [] _ = []
bridgeConfigurations (Nothing:bs) n = map (0:) $ bridgeConfigurations bs n
bridgeConfigurations (_:bs) n =  concatMap (\i -> map (i:) $ bridgeConfigurations bs (n-i)) [0..2]

bridges2bits :: [Int] -> Bitvector
bridges2bits [] = 0
bridges2bits (b:bs) = shift (bridges2bits bs) 3 .|. shift 1 b

  
bitOr :: [Bitvector] -> Bitvector
bitOr = foldl' (.|.) 0


(.+) ::(Num t) => (t, t, t) -> (t, t, t) -> (t, t, t)
(x, y, z) .+ (x', y', z') = (x+x', y+y', z+z')

(.-) ::(Num t) => (t, t, t) -> (t, t, t) -> (t, t, t)
(x, y, z) .- (x', y', z') = (x-x', y-y', z-z')

(.*) ::(Num t) => (t, t, t) -> t -> (t, t, t)
(x, y, z) .* f = (x*f, y*f, z*f)
