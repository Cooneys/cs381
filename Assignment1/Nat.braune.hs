module Nat where 
import Prelude hiding (Enum(..), sum)

-- Ethan Braun (braune) & Samuel Cooney (cooneys)

--
-- * Part 2: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred Zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--   
--pred = undefined
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ a) = a

-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
--isZero = undefined
isZero :: Nat -> Bool
isZero Zero = True
isZero a    = False

-- | Convert a natural number to an integer.
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
--toInt = undefined
natVal :: Nat -> Int -> Int
natVal Zero a = a
natVal n a    = natVal (pred n) (a + 1)

toInt :: Nat -> Int
toInt a = natVal a 0

-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--   
--add = undefined
add :: Nat -> Nat -> Nat
add Zero b = b
add a b    = add (pred a) (Succ b)

-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
--sub = undefined
sub :: Nat -> Nat -> Nat
sub a Zero = a
sub Zero b = Zero
sub a b    = sub (pred a) (pred b)

-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
--gt = undefined
gt :: Nat -> Nat -> Bool
gt Zero b = False
gt a Zero = True
gt a b    = gt (pred a) (pred b)


-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
--mult = undefined
multHelper :: Nat -> Nat -> Nat -> Nat
multHelper (Succ Zero) _ x = x
multHelper rem base cur    = multHelper (pred rem) base (add base cur) 

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult a b    = multHelper a b b 

-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
--sum = undefined
sum :: [Nat] -> Nat
sum l = foldr add Zero l

-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
--odds = undefined
dSucc :: Nat -> Nat
dSucc a = Succ (Succ a)

odds :: [Nat]
odds = iterate dSucc one
