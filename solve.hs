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
                   let filename = basename ++ ".solution" ++ if is3D p then ".html" else ".eps"
                   putStrLn $ "Will write first solution to '" ++ filename ++ "'."
                   let solutions = solve p
                   when (not (null solutions)) $ writeFile filename $ showState $ head solutions
                   putStrLn $ "Total number of solutions: " ++ show (length solutions)
                     where showState = if is3D p then x3dshowState else showStateEPS
