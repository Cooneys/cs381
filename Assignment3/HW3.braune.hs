module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- Authors:
--   Ethan Braun (braune)
--   Samuel Cooney (cooneys)


-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd c (m, (x, y)) = case c of
                      (Pen Up)   -> ((Up, (x, y)), Nothing)
                      (Pen Down) -> ((Down, (x, y)), Nothing)
                      (Move q r) -> case m of
                                      Up -> ((m, (q, r)), Nothing) 
                                      Down -> ((m, (q, r)), Just ((x, y), (q, r)))

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog (c:cs) s = let o = cmd c s
                  in case o of
                       (s', Nothing) -> prog cs s'
                       (s', Just l)  -> (fst o', l : (snd o'))
                                          where
                                            o' = prog cs s'
prog [] s = (s, [])


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
line :: Int -> Int -> Int -> Int -> [Cmd]
line a b x y = [Pen Up, Move a b, Pen Down, Move x y, Pen Up]

rect:: Int -> Int -> Int -> Int -> [Cmd]
rect x y w h = (line x y (x+w) y) ++ (line (x+w) y (x+w) (y+h)) ++ (line (x+w) (y+h) x (y+h)) ++ (line x (y+h) x y) 

cube :: Int -> Int -> Int -> Int -> [Cmd]
cube x y l hl = (rect x y l l) ++ (rect xa ya l l) ++ (line x y xa ya) ++ (line xb y xc ya) ++ (line xb yb xc yc) ++ (line x yb xa yc) 
               where
                 xa = x+hl
                 xb = x+l
                 xc = x+(3*hl)
                 ya = y+hl
                 yb = y+l
                 yc = y+(3*hl)

amazing :: Prog
amazing = (cube 29 13 10 5) ++ (cube 20 5 20 10) ++ (line xa ya xb yb) ++ (line xf ya xe yb) ++ (line xg yd xh yc) ++ (line xc yc xd yd) ++ (line xa yf xb ye) ++ (line xf yf xe ye) ++ (line xg yg xh yh) ++ (line xd yg xc yh)
            where
              xa = 20
              xb = 29
              xc = 30
              xd = 34
              xe = 39
              xf = 40
              xg = 44
              xh = 50
              ya = 5
              yb = 13
              yc = 15
              yd = 18
              ye = 23
              yf = 25
              yg = 28
              yh = 35

