import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
import Data.List (find)
import Data.Maybe (maybeToList)
import qualified Data.Map as Map
import qualified Data.Set as Set

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

sampleProblem :: String
sampleProblem = unlines
    [ "2.2..2."
    , "......1"
    , "6.5.3.."
    , ".1....3"
    , "3.1..1."
    , ".3..8.5"
    , "4.2...."
    , ".2..5.2"
    , "2..1..."
    , "..2.5.3"
    ]

sampleProblemHard :: String
sampleProblemHard = unlines
    ["2.2...3.3"
    ,".2..3..3."
    ,"3....1..3"
    ,".3.2...2."
    ,"4.3.2...3"
    ,".3.2.3.3."
    ,"3.....2.4"
    ,".3..3..1."
    ,"3.2...4.5"
    ,".2...5.2."
    ,"3.3.2.3.2"
    ,".2...3..."
    ,"3..3..6.2"
    ]

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
                               , topNeighbor :: Maybe Index
                               , rightNeighbor :: Maybe Index
                               , bottomNeighbor :: Maybe Index
                               , leftNeighbor :: Maybe Index
                               , iBridges :: [Bridges]
                               } deriving (Eq, Show)
type State = Map.Map Index IslandState

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a+c, b+d)

stateFromProblem :: Problem -> State
stateFromProblem p = state
    where ((0, 0), (rn, cn)) = bounds p
          state = Map.fromList $ map f islands
          islands = [e | e@(_, Island _) <- assocs p]
          f (i, Island n) = (i, newisland)
              where newisland = IslandState n (top i) (right i) (bottom i) (left i) bridges
                    bridges = filter h $ nBridges n
                    h (Bridges t r b l) =  (topNeighbor newisland /= Nothing || t == 0)
                                        && (rightNeighbor newisland /= Nothing || r == 0)
                                        && (bottomNeighbor newisland /= Nothing || b == 0)
                                        && (leftNeighbor newisland /= Nothing || l == 0)
          top    (r, c) = find islandIndex [(rr, c) | rr <- [r-1, r-2 .. 0]]
          right  (r, c) = find islandIndex [(r, cc) | cc <- [c+1 .. cn]]
          bottom (r, c) = find islandIndex [(rr, c) | rr <- [r+1 .. rn]]
          left   (r, c) = find islandIndex [(r, cc) | cc <- [c-1, c-2 .. 0]]
          islandIndex i = isIsland (p!i)


-- |'blockingPairs' returns all pairs '(i1, i2)' of indexes of islands where the 
-- bridge to the right from 'i1' would cross the bridge to the bottom from 'i2'
blockingPairs :: State -> [(Index, Index)]
blockingPairs s = map xtract $ filter xing pairs
    where pairs = [(a1, a2) | a1 <- Map.assocs s, a2 <- Map.assocs s]
          xtract ((i1, _), (i2, _)) = (i1, i2)
          xing (((r1, c1), s1), ((r2, c2), s2)) = case (rightNeighbor s1, bottomNeighbor s2) of
                   (Just (_, c1'), Just (r2', _)) -> r2 < r1 && r1 < r2' && c1 < c2 && c2 < c1'
                   otherwise                      -> False

narrow :: Set.Set Index -> State -> [State]
narrow seed state = if Set.null seed then [state] else result
    where (i, seed') = Set.deleteFindMin seed
          island = state Map.! i
          bs = iBridges island
          bs' = filter (match topB topNeighbor bottomB)
              $ filter (match rightB rightNeighbor leftB)
              $ filter (match bottomB bottomNeighbor topB)
              $ filter (match leftB leftNeighbor rightB) bs
          result = if null bs'
                   then []
                   else if bs == bs' 
                        then narrow seed' state
                        else let newSeeds = Set.fromList $ concatMap (maybeToList.($island)) 
                                                                     [topNeighbor, rightNeighbor
                                                                     , bottomNeighbor, leftNeighbor]
                             in narrow (Set.union seed' newSeeds)
                                       (Map.insert i (island {iBridges = bs'}) state)
          match thisB neighbor otherB b = case neighbor island of
                Nothing -> True
                Just i' -> thisB b `elem` (map otherB $ iBridges $ state Map.! i')

counts :: State -> [Int]
counts = (map (length.iBridges)) . Map.elems

