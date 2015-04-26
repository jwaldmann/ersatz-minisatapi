{-# language FlexibleContexts #-}

module Ersatz.Solver.Minisat.API where

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.IntSet as S
import qualified Data.IntMap as M

import Ersatz
import Control.Lens 
import qualified MiniSat as API
import Control.Monad.State

import System.IO
import Control.Exception (bracket, finally, mask_, onException )
import Control.Concurrent.Async

minisatapi :: MonadIO m => Solver SAT m
minisatapi problem = do
  let a = problem ^. lastAtom
  liftIO $ withNewSolverAsync $ \ solver -> do
    API.minisat_set_verbosity solver 0
    table <- M.fromList <$> ( forM [1..a] $ \ v -> do
      l <- API.newLit solver
      return (v, l) )
    forM_ (dimacsClauses problem) $ \clause -> do
      API.addClause solver $ do
         lit <- S.toList clause
         return $ (if lit < 0 then API.neg else id)
                $ table M.! abs lit
    v <- API.minisat_num_vars solver
    c <- API.minisat_num_clauses solver
    hPutStrLn stderr $ unwords
      [ "CNF", show v, "vars", show c , "clauses"]

    ret <- API.limited_solve solver []
    if ret == API.l_True then do
      result <- M.fromList <$> ( forM [1..a] $ \ v -> do
          Just b <- API.modelValue solver $ table M.! v
          return (v,b) )
      return (Satisfied , result)
    else
      return (Unsatisfied, M.empty)

withNewSolverAsync h =
  bracket newSolver API.deleteSolver $ \  s -> do
    mask_ $ withAsync (h s) $ \ a -> do
      wait a `onException` API.minisat_interrupt s

newSolver =
  do s <- API.minisat_new
     -- https://github.com/niklasso/minisat-haskell-bindings/issues/6
     -- eliminate s True 
     return s
