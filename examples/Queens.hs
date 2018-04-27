{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Control.CP.FD.GecodeExample

import System.Environment

noattack i j qi qj = do
  qi        @/=  qj
  qi  +  i  @/=  qj  +  j
  qi  -  i  @/=  qj  -  j

model :: ExampleModel ModelInt
model n = exists $ \p -> do
  size p @= n
  p `allin` (cte 0,n-1)
  allDiff p
  loopall (cte 0,n-2) $ \i -> 
    loopall (i+1,n-1) $ \j ->
      noattack i j (p!i) (p!j)
  return p

main = getArgs >>= \ case
  [] -> withArgs ["gecode_run", "16"] $ example_sat_main_single_expr_gecode model
  _  -> example_sat_main_single_expr_gecode model

