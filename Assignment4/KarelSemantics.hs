module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r = not (test t w r)
test (Facing c) _ r = getFacing r == c
test (Clear d)  w r = isClear (neighbor (getFacing r) (getPos r)) w 
test Beeper     w r = hasBeeper (getPos r) w
test Empty      _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt Move       _ w r = let f = neighbor (getFacing r) (getPos r) 
                        in if isClear f w
                              then OK w (setPos f r)  
                              else Error ("World is blocked at: " ++ show f)
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper  _ w r = let e = isEmpty r
                            f = hasBeeper (getPos r) w
                        in case (e, f) of
                             (False, False) -> OK (incBeeper (getPos r) w) (decBag r)
                             (False, True)  -> Error ("Beeper already at: " ++ show (getPos r))
                             (True, False)  -> Error ("Out of beepers")
                             (True, True)   -> Error ("Out of beepers and beeper already at: " ++ show (getPos r)) 
stmt (Turn d)   _ w r = OK w (setFacing (cardTurn d (getFacing r)) r) 
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
