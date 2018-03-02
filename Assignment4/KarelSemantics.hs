module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState

-- Authors:
--   Ethan Braun (braune)
--   Samuel Cooney (cooneys)

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r = not (test t w r)
test (Facing c) _ r = getFacing r == c
test (Clear d)  w r = isClear (neighbor (getFacing r) (getPos r)) w 
test Beeper     w r = hasBeeper (getPos r) w
test Empty      _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown      _ _ r = Done r

stmt Move          _ w r = let f = neighbor (getFacing r) (getPos r) 
                           in if isClear f w
                                 then OK w (setPos f r) 
                                 else Error ("Blocked at: " ++ show f)

stmt PickBeeper    _ w r = let p = getPos r
                           in if hasBeeper p w
                                 then OK (decBeeper p w) (incBag r)
                                 else Error ("No beeper to pick at: " ++ show p)

stmt PutBeeper     _ w r = case isEmpty r of
                             False -> OK (incBeeper (getPos r) w) (decBag r)
                             True  -> Error ("No beeper to put.")

stmt (Turn d)      _ w r = OK w (setFacing (cardTurn d (getFacing r)) r) 

stmt (Call m)      d w r = case lookup m d of
                             (Just s) -> stmt s d w r
                             _        -> Error ("Undefined macro: " ++ m)

stmt (Iterate n s) d w r = if n > 0
                              then case (stmt s d w r) of
                                     (OK w' r') -> stmt (Iterate (n-1) s) d w' r'
                                     e          -> e 
                              else OK w r 

stmt (If t s p)    d w r = if test t w r
                              then stmt s d w r
                              else stmt p d w r

stmt (While t s)   d w r = if test t w r
                              then case (stmt s d w r) of
                                     (OK w' r') -> stmt (While t s) d w' r'
                                     e          -> e 
                              else OK w r
stmt (Block x)     d w r = case x of
                             (h:t)  -> case (stmt h d w r) of
                                            (OK w' r') -> stmt (Block t) d w' r'
                                            e          -> e
                             []     -> OK w r

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
