-- solve.hs 
-- a solver for Hashiwokakero puzzles
-- Copyright (C) 2013 by Harald Bögeholz
-- See LICENSE file for license information

import Hashi
import System.Environment
import Control.Monad (when)

main :: IO ()
main = do
     args <- getArgs
     case args of
          [filename] -> do
                     s <- readFile filename
                     work s filename
          [] -> error "Usage: solve filename\nWill write solution to filename.solution.eps"
          _  -> error "Too many arguments."
  where work s basename = case readProblem s of
             Left e -> putStrLn e
             Right p -> do
                   putStrLn $ "Will write first solution to '" ++ basename ++ ".layer*.eps'."
                   let solutions = solve p
                   when (not (null solutions)) $ writeSolution $ head solutions
                   putStrLn $ "Total number of solutions: " ++ show (length solutions)
                     where writeSolution s = mapM_ (writeLayer s) [l0..ln]
                           (l0, ln) = layerRange p
                           writeLayer s l = writeFile (basename++".layer"++show l++".eps") $ showStateEPS l s

