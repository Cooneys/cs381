module MiniLogo where
import Prelude hiding (Num, Var)

-- Task 1
type Num = Int
type Var = String
type Macro = [Char]

type Prog = [Cmd]

data Mode = Down | Up

data Expr = Str Var
          | Numb Num

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Expr] Prog
         | Call Macro [Expr]


-- Task 2

-- Concrete MiniLogo syntax:

-- define line (x1, y1, x2, y2) {
--    pen up; move (x1, y1)
--    pen down; move (x2, y2)
--    pen up;
-- }

-- Abstract MiniLogo syntax:
line :: Cmd
line = Define "line" [Str "x1", Str "y1", Str "x2", Str "y2"] [Pen Up, Move (Str "x1") (Str "y1"), Pen Down, Move (Str "x2") (Str "y2"), Pen Up]


-- Task 3

-- Concrete MiniLogo syntax:

-- define nix (x, y, w, h) {
--    line(x, y, x + w, y + h)
--    line(x, y + h, x + w, y)
-- }

-- Abstract MiniLogo syntax
nix :: Cmd
nix = Define "nix" [Str "x", Str "y", Str "w", Str "h"] [Call "line" [Str "x", Str "y", Str "x + w", Str "y + h"], Call "line" [Str "x", Str "y + h", Str "x + w", Str "y"]]

