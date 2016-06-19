module MonadicInterpreter (
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

-- | The empty state contains no bindings for any variables---it as an empty
-- association list.
emptyState :: State
emptyState = []

-- | The 'I' monad is our interpreter monad. Computations in the 'I' monad require a
-- value of type 'State' and return a result along with a new 'State'. This is
-- much like the "random value" example from lecture, but instead of needing a
-- seed to calculate a value, we need an association list. Read about @newtype@
-- declarations in LYAH if you don't understand the definition of 'I' or what
-- 'runI' does.
newtype I a = I { runI :: State -> (a, State) }

-- 'I' is a data constructor that takes a single value, a function. Remember,
-- '$' is just function application.
instance Monad I where
    -- Returning a value doesn't change the state
    return x = I $ \s -> (x, s)
    -- Recall that the type of '(>>=)' is @m a -> (a -> m b) -> m b@. That means
    -- @act@ has type @I a@ and @f@ has type @a -> I b@. First, we need to run
    -- the computation @act@ by giving it the current state. This produces a
    -- value, @x@, and a new state, @s'@. Then we need to call @f@, passing it
    -- the argument @x@, to get a new computation. Finally, we run this
    -- computation with the new state @s'@.
    comp >>= f  = I $ \s -> let (x, s') = runI comp s
                            in
                              runI (f x) s'

-- | Get the current state.
getState :: I State
getState = I $ \s -> (s, s)

-- | Put a new state. This throws away the current state.
putState :: State -> I ()
putState s = I $ \_ -> ((), s)

-- | Look up a variable in the current state. Note that we first get the current
-- state, which is an association list, and then find the variable in the
-- association list.
lookup :: Var -> I Z
lookup v0 = do { s <- getState
               ; return (look v0 s)
               }
  where
    look :: Var -> State -> Z
    look v []                      = error $ "undefined variable " ++ show v
    look v ((v',n):vs) | v' == v   = n
                       | otherwise = look v vs

-- | Extend the current state with a new variable binding.
extend :: Var -> Z -> I ()
extend v n = do { vs <- getState
                ; putState $ (v,n) : filter (\(v', _) -> v' /= v) vs
                }

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
sStep (Out _)      = return ()
sStep (Seq s1 s2)  = do { sStep s1
                        ; sStep s2
                        }
sStep (If b s1 s2) = do { x <- bSem b
                        ; if x then sStep s1 else sStep s2
                        }
sStep (While b s1) = do { x <- bSem b
                        ; if x then sStep (Seq s1 ( While b s1 ) ) else sStep Skip
                        }



-- | 2.5 hours
