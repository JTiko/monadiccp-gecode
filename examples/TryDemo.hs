{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{- 
 - 	Monadic Constraint Programming
 - 	http://www.cs.kuleuven.be/~toms/Haskell/
 - 	Tom Schrijvers & Pieter Wuille
 -}

import Control.CP.FD.GecodeExample

main = example_sat_main_void_gecode model

model :: ExampleModel ()
model _ = exists $ \col -> do
  [a,b] <- colList col 2
  a @: (cte 1, cte 5)
  b @: (cte 0, cte 4)
  a - b @= 1
  (a @= 2) @|| (a @= 3) @|| (a @= 4)
  return col

