module OutputInterpreter (
    I,
    runI,
    emptyState,

    sStep
  ) where

import Prelude hiding (lookup)

import While

type Z = Integer

-- | In lecture, we represented the state as a function. Here we represent state
-- as an association list instead.
type State = [(Var,Z)]

emptyState :: State
emptyState = []

newtype I a = I { runI :: State -> (a, [Z], State) }

instance Monad I where
    return x   =  I $ \s -> (x, [], s)
    comp >>= f = I $ \s -> let (x, o1, s1) = runI comp s
                               (y, o2, s2) = runI (f x) s1
                           in
                             (y, o1 ++ o2, s2)

getState :: I State
getState = I $ \s -> (s, [], s)

putState :: State -> I ()
putState s = I $ \_ -> ((), [], s)

lookup :: Var -> I Z
lookup v0 = do { s <- getState
               ; return (look v0 s)
               }
  where
    look :: Var -> State -> Z
    look v []                      = error $ "undefined variable " ++ show v
    look v ((v',n):vs) | v' == v   = n
                       | otherwise = look v vs

extend :: Var -> Z -> I ()
extend v n = do { vs <- getState
                ; putState $ (v,n) : filter (\(v', _) -> v' /= v) vs
                }

output :: Z -> I ()
output i = I $ \s -> ((), [i], s)

-- | The semantic function for arithmetic expressions
aSem :: Aexp -> I Z
aSem (Const x)   = return x
aSem (Var v)     = lookup v
aSem (Add a1 a2) = do { x <- aSem a1
                      ; y <- aSem a2
                      ; return $ x + y
                      }
aSem (Sub a1 a2) = do { x <- aSem a1
                      ; y <- aSem a2
                      ; return $ x - y
                      }
aSem (Mul a1 a2) = do { x <- aSem a1
                      ; y <- aSem a2
                      ; return $ x * y
                      }

-- | The semantic function for boolean expressions
bSem :: Bexp -> I Bool
bSem BTrue       = return True
bSem BFalse      = return False
bSem (Eq a1 a2)  = do { x <- aSem a1
                      ; y <- aSem a2
                      ; return $ x == y
                      }
bSem (Le a1 a2)  = do { x <- aSem a1
                      ; y <- aSem a2
                      ; return $ x <= y
                      }
bSem (Not b)     = do { x <- bSem b
                      ; return $ not x
                      }
bSem (And b1 b2) = do { x <- bSem b1
                      ; y <- bSem b2
                      ; return $ x && y
                      }

-- | The semantic function for statements
	   
sStep :: Stm -> I ()
sStep (Assign v x) = do { y <- aSem x
                        ; extend v y
                        }
sStep Skip         = return ()
sStep (Seq s1 s2)  = do { sStep s1
                        ; sStep s2
                        }
sStep (If b s1 s2) = do { x <- bSem b
                        ; if x then sStep s1 else sStep s2
                        }
sStep (While b s1) = do { x <- bSem b
                        ; if x then sStep (Seq s1 (While b s1)) else sStep Skip
                        }
sStep (Out a)      = do { i <- aSem a
                        ; output i
                        }

-- | 2 hours time

{- 
	Note the changes you had to make to the sStep function from Problem 2 to get it to work with the new monad. Now imagine that you had to modify the interpreter from Problem 1 to keep track of output values. Would that have required more modification? Why or why not?

	Problem 1 would require modification to keep track of the output values.
	You would need to to have set up an extra argument for sStep function parameter. This will hold the output values. So each case of the sStep function will need that new argument. 
	

-}
