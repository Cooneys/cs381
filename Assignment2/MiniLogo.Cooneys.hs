module MiniLogo where

import Prelude hiding (Num)


--
-- * Syntax of MiniLogo
-- Authors: Sam Cooney  - cooneys
--          Ethan Braun - braune
--

-- Grammar for MiniLogo:
-- num   ::= (any natural number)
-- var   ::= (any variable name)
-- macro ::= (any macro name)
-- 
-- prog  ::= ε | cmd ; prog 
--
-- mode  ::= down | up
--
-- expr  ::= var
--        |  num
--        |  expr + expr
--
-- cmd   ::= pen mode
--        |  move ( expr , expr )
--        |  define macro ( var* ) { prog }
--        |  call macro ( expr* )
--

--
-- Task 1
--
-- Encode the grammar as a set of Haskell data types
--

type Num = Int

type Var = [Char]

type Macro = [Char]

--
-- Pen status, down is drawing, up is not drawing
--

data Mode = Down | Up
  deriving (Eq,Show)

data Expr = Vari Var
          | Numb Num
          | Add Expr Expr
  deriving (Eq,Show)

data Cmd = Pen Mode
         | Move ( Expr , Expr )
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq,Show)

type Prog = [Cmd]

--
-- Task 2
--
-- Concrete MiniLogo syntax:
--
-- define line (x1, y1, x2, y2) {
--    pen up; move (x1, y1)
--    pen down; move (x2, y2)
--    pen up;
-- }
--
-- Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas) 
-- draws a line segment from (x1,y1) to (x2,y2)
--
-- Abstract MiniLogo syntax:
--

line :: Cmd
line = Define "line" ["x1","y1","x2","y2"] [Pen Up, Move ( Vari "x1", Vari "y1"), Pen Down, Move (Vari "x2", Vari "y2"), Pen Up]

--
-- Task 3
--
-- Concrete MiniLogo syntax:
--
-- define nix (x, y, w, h) {
--    line(x, y, x + w, y + h)
--    line(x, y + h, x + w, y)
-- }
-- Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h) that 
-- draws a big “X” of width w and height h, starting from position (x,y). Your definition 
-- should not contain any move commands.
--
-- Abstract MiniLogo syntax
--

nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"] [Call "line" [Vari "x", Vari "y", Add (Vari "x") (Vari "w"), Add (Vari "y") (Vari "h")], Call "line" [Add (Vari "x") (Vari "h"), Vari "y", Vari "x", Add (Vari "y") (Vari "h")]]

--
-- Task 4
--
-- Define a Haskell Function steps :: Int -> Prog that constructs a  MiniLogo program that
-- draws a staircase of n steps startign from (0, 0). 
--

steps :: Int -> Prog
steps 0 = []
steps i = Call "line" [Numb i, Numb i, Numb (i-1), Numb i] : Call "line" [Numb (i-1), Numb i, Numb (i-1), Numb (i-1)] : steps (i-1)

--
-- Task 5
--
-- Define a haskell function macros :: Prog -> [Macro] that returns a list of all the names
-- Of all the macros that are define anywhere in a given MiniLogo program.
--

macros :: Prog -> [Macro]
macros []                = []
macros (Pen _ : t)       = macros t
macros (Call _ _ : t)    = macros t
macros (Move _ _ : t)    = macros t
macros ((_ _ v _) : t) = v : macros t









