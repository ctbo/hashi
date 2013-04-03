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
                     work s (filename ++ ".solution.eps")
          [] -> error "Usage: solve filename [#solutions]"
          _  -> error "Too many arguments."
  where work s outfile = case readProblem s of
             Left e -> putStrLn e
             Right p -> do
                   putStrLn $ "Will write first solution to '" ++ outfile ++ "'."
                   let solutions = solve p
                   when (not (null solutions)) $ writeFile outfile $ showStateEPS $ head solutions
                   putStrLn $ "Total number of solutions: " ++ show (length solutions)
