{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.CP.FD.GecodeExample (
  example_main_gecode,
  example_sat_main_gecode,
  example_sat_main_void_gecode,
  example_sat_main_single_gecode,
  example_sat_main_single_expr_gecode,
  example_sat_main_coll_expr_gecode,
  example_min_main_gecode,
  example_min_main_void_gecode,
  example_min_main_single_gecode,
  example_min_main_single_expr_gecode,
  example_min_main_coll_expr_gecode,
  module Control.CP.FD.Example 
) where

import System.Environment (getArgs)

import Control.CP.FD.Gecode.CodegenSolver
import Control.CP.FD.Gecode.Common
import Control.CP.FD.OvertonFD.OvertonFD
import Control.CP.SearchTree
import Control.CP.FD.FD

import Control.CP.FD.Example
import Control.CP.FD.Gecode.Runtime
import Control.CP.FD.Gecode.RuntimeSearch

import Control.Monad.Cont

setSearchMinimize :: Tree (FDInstance (GecodeWrappedSolver SearchGecodeSolver)) ()
setSearchMinimize = do
  term <- label $ 
    do
      x <- getMinimizeTerm
      return $ return x
  label $ do
    liftFD $ liftGC $ Control.CP.FD.Gecode.RuntimeSearch.setOptions (\o -> o { minimizeVar = term })
    return $ return ()

example_main_gecode :: ExampleModel [String] -> ExampleModel ModelInt -> ExampleModel ModelCol -> Bool -> IO ()
example_main_gecode f fx fcx typ = do
  args <- getArgs
  case args of
    ("gecode_compile":r) -> putStr $ generateGecode ((f r) :: Tree (FDInstance (GecodeWrappedSolver CodegenGecodeSolver)) ModelCol)
    ("gen_gecode_compile":r) -> putStr $ generateGecode ((\x -> codegenOptionset (\c -> c { noGenSearch=True }) >> fx x) :: ModelInt -> Tree (FDInstance (GecodeWrappedSolver CodegenGecodeSolver)) ModelCol)
    ("gen_gecode_compile_notrail":r) -> putStr $ generateGecode ((\x -> codegenOptionset (\c -> c { noTrailing=True, noGenSearch=True }) >> fx x) :: ModelInt -> Tree (FDInstance (GecodeWrappedSolver CodegenGecodeSolver)) ModelCol)
    ("gen_gecode_compile_gensrch":r) -> putStr $ generateGecode ((\x -> codegenOptionset (\c -> c { noGenSearch=False }) >> fx x) :: ModelInt -> Tree (FDInstance (GecodeWrappedSolver CodegenGecodeSolver)) ModelCol)
    ("gecode_run":r) -> print $ runSolve typ $ ((f r) :: Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) ModelCol) >>= labeller
    ("gecode_run_cont":r) -> print $ runSolve typ $ ((runContT (f r >>= labeller) Return) :: Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) [Integer])
    ("gecode_search":r) -> print $ runSolve typ $ ((f r >>= (\x -> setSearchMinimize >> return x)) :: Tree (FDInstance (GecodeWrappedSolver SearchGecodeSolver)) ModelCol) >>= labelCol
    ("overton_run":r) -> print $ runSolve typ $ ((f r) :: Tree (FDInstance OvertonFD) ModelCol) >>= labeller
    [] -> putStr "Solver type required: one of gecode_compile, gen_gecode_compile, gecode_run, gecode_run_cont, overton_run\n"
    (a:r) -> putStr ("Unsupported solver: " ++ a ++ "\n")

example_min_main_gecode :: ExampleMinModel [String] -> ExampleMinModel ModelInt -> ExampleMinModel ModelCol -> IO ()
example_min_main_gecode f fx fcx = example_main_gecode (postMinimize f) (postMinimize fx) (postMinimize fcx) True

example_sat_main_gecode :: ExampleModel [String] -> ExampleModel ModelInt -> ExampleModel ModelCol -> IO ()
example_sat_main_gecode f fx fcx = example_main_gecode f fx fcx False

example_sat_main_void_gecode :: ExampleModel () -> IO ()
example_sat_main_void_gecode f = example_sat_main_gecode (const $ f ()) (const $ f ()) (const $ f ())

example_min_main_void_gecode :: ExampleMinModel () -> IO ()
example_min_main_void_gecode f = example_min_main_gecode (const $ f ()) (const $ f ()) (const $ f ())

example_sat_main_single_gecode :: Read n => ExampleModel n -> IO ()
example_sat_main_single_gecode f = example_sat_main_gecode (f . read . head) (error "Uncompilable model") (error "Uncompilable model")

example_min_main_single_gecode :: Read n => ExampleMinModel n -> IO ()
example_min_main_single_gecode f = example_min_main_gecode (f . read . head) (error "Uncompilable model") (error "Uncompilable model")

example_sat_main_single_expr_gecode :: ExampleModel ModelInt -> IO ()
example_sat_main_single_expr_gecode f = example_sat_main_gecode (f . fromInteger . read . head) f (\x -> f $ x!(cte 0))

example_min_main_single_expr_gecode :: ExampleMinModel ModelInt -> IO ()
example_min_main_single_expr_gecode f = example_min_main_gecode (f . fromInteger . read . head) f (\x -> f $ x!(cte 0))

example_sat_main_coll_expr_gecode :: ExampleModel ModelCol -> IO ()
example_sat_main_coll_expr_gecode f = example_sat_main_gecode (f . list . foldr (++) [] . map (map fromInteger . read . (\x -> "[" ++ x ++ "]"))) (f. list . (\x -> [x])) f

example_min_main_coll_expr_gecode :: ExampleMinModel ModelCol -> IO ()
example_min_main_coll_expr_gecode f = example_min_main_gecode (f . list . foldr (++) [] . map (map fromInteger . read . (\x -> "[" ++ x ++ "]"))) (f. list . (\x -> [x])) f
