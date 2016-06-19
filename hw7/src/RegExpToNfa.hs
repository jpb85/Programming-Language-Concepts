-- 1 hours


module RegExpToNfa (regExpToNfa) where

import Nfa
import RegExp
import Set

-- | Convert a regular expression to an NFA whose states are integers.
--
-- NFAs built by 'regExpToNfa' obey a few invariants that make them easier to
-- work with:
--
-- 1) All states are consecutively numbered from 0.
-- 2) State 0 is the start state.
-- 3) The returned NFA has a single accept state, and it is the
--    highest-numbered state.
regExpToNfa :: RegExp -> Nfa Int
regExpToNfa Empty =
    Nfa (fromList [0..1]) -- Two states, 0 and 1
        empty             -- No moves (transitions)
        0                 -- 0 is the start state
        (singleton 1)     -- There is a single accept state, state 1

regExpToNfa Epsilon =
    Nfa (fromList [0..1])       -- Two states, 0 and 1
        (singleton (Emove 0 1)) -- One move, an epsilon transition from state 0
                                -- to state 1
        0                       -- 0 is the start state
        (singleton 1)           -- There is a single accept state, state 1

regExpToNfa (Lit c) = Nfa (fromList [0..1])   (singleton (Move 0 c 1))  0  (singleton 1)

regExpToNfa (Cat r s) = Nfa (qs1 `union` qs2)
        (move1 `union` move2 `union` fromList catMoves)  mstar1 (singleton (acceptState snfa))
   where
    rnfa@(Nfa qs1 move1 mstar1 _ ) = numberNfaFrom 0 $ regExpToNfa r
    snfa@(Nfa qs2 move2 mstar2 _ ) =  numberNfaFrom (nfaSize rnfa) $ regExpToNfa s
    catMoves = [Emove (acceptState rnfa) mstar2]
    
    
    

regExpToNfa (Alt r s) =
    Nfa (r_qs    `union` s_qs    `union` fromList [q0,qf])
        (r_moves `union` s_moves `union` fromList altMoves)
        q0
        (singleton qf)
  where
    -- The NFA corresponding to the regular expression r, suitably renumbered.
    r_nfa@(Nfa r_qs r_moves r_start _) =
        numberNfaFrom 1 $ regExpToNfa r

    -- The NFA corresponding to the regular expression s, suitably renumbered.
    s_nfa@(Nfa s_qs s_moves s_start _) =
        numberNfaFrom (nfaSize r_nfa + 1) $ regExpToNfa s

    -- These are our new states: q0 is the new start state, and qf is the new
    -- accepting state. We have to maintain the following invariants:
    -- 2) State 0 is the start state.
    -- 3) The returned NFA has a single accept state, and it is the
    --    highest-numbered state.
    q0 = 0
    qf = nfaSize r_nfa + nfaSize s_nfa + 1

    -- These are the additional transitions for our NFA.
    altMoves = [ Emove q0 r_start
               , Emove q0 s_start
               , Emove (acceptState r_nfa) qf
               , Emove (acceptState s_nfa) qf]

regExpToNfa (Star r) =
    Nfa (qs `union` fromList [q0,qf])
        (moves `union` fromList starMoves)
        q0
        (singleton qf)
  where
    r_nfa@(Nfa qs moves start _) =
        numberNfaFrom 1 $ regExpToNfa r

    q0 = 0
    qf = nfaSize r_nfa + 1

    starMoves = [ Emove q0 start
                , Emove q0 qf
                , Emove (acceptState r_nfa) qf
                , Emove (acceptState r_nfa) start]

-- | For an NFA with a single accept state, return the accept state. In general,
-- an NFA can have more than one accept state; however, the NFAs that we
-- construct from regular expressions obey the invariant that an NFA has only a
-- single accept state.
acceptState :: Nfa q -> q
acceptState (Nfa _ _ _ f) =
    case toList f of
      [q] -> q
      _   -> error "acceptState: NFA does not have a single accept state"
      
      
 
