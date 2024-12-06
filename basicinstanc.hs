
-- here can not able to print it because print function relies on the show type class to convert 
-- a value into a string for output 
--  By default, your custom data type Rat does not automatically have an 
--  instance of Show. To fix this, you need to define a Show instance for your Rat type.

-- instance Show Rat, you're saying: "I want my custom type Rat to be part of the Show type class
data Rat = MkRat {num::Int,denom::Int}
-- The implementations of the type class's functions for that data type.
--An instance connects a specific data type to a type class
--define our show instance 
-- oh so her 
-- instance Show Rat where
--     show r = show (num r) ++ "/" ++ show (denom r)

instance Show Rat where 
  show::Rat ->String 
  show r  =show (num r ) ++ "/" ++ show(denom r)

--main =print(MkRat 2 3)
-- show::Rat ->String 
-- show r  =show (num r) ++ "/" ++ show (denom r)
simplify (MkRat { num , denom }) =
  MkRat { num = sgn * abs num `div` n , denom = abs denom `div` n }
  where
    n = gcd (abs num) (abs denom)
    sgn | num>0 && denom>0 || num<0 && denom<0 = 1
        | num<0 && denom>0 || num>0 && denom<0 = -1
        | num == 0 && denom /= 0               = 0
        | denom == 0                           = error "denominator cannot be 0"

--main = print (simplify $ MkRat 2 3 )

simplify1(MkRat {num, denom}) = MkRat {num = sgn * abs num `div` n, denom = abs denom `div` n}
  where
    n = gcd (abs num) (abs denom)
    sgn
      | num == 0 = 0
      | denom == 0 = error "denominator can not be 0"
      | num * denom > 0 = 1
      | num * denom < 0 = -1

--main = print (simplify1 $ MkRat {num = 2, denom = 3})


data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
instance Show Day where 
  show::Day ->String --not necessary to write this line as Show already provides the  type signature  for show 
  show Mon ="M"
  show Tue ="T"
  show Wed ="W"
  show Thu ="TH"
  show Fri ="F"
  show Sat ="S"
  show sun ="Su"

-- Since print relies on show, the result is displayed as the string returned by your custom implementation of show
--main =print( Mon )


--Num is a Type Class 
instance Num Rat where
  (+)::Rat ->Rat ->Rat 
  MkRat a b + MkRat c d  =simplify $ MkRat ( a*d +c*b)(b*d)  
  (*)::Rat -> Rat ->Rat 
  MkRat a b * MkRat c d =simplify $ MkRat (a*c)(b*d)
  abs::Rat -> Rat
  abs (MkRat a b ) = MkRat (abs a )(abs b)
  fromInteger :: Integer  ->Rat --It only converts an Integer to types that are instances of the Num type class.
  fromInteger a  = MkRat  (fromInteger a)  1
  negate :: Rat -> Rat
  negate (MkRat a b) = simplify $ MkRat (-a) b
  signum::Rat -> Rat
  signum (MkRat a b) --you must wrap it in the parenthesis for proper parsing 
   | a == 0 = MkRat 0 1 
   | a * b > 0 = MkRat 1 1
   | otherwise = MkRat (-1) 1 


--main =print $ signum $ MkRat 0 1 
--because the Num typeclass is defined to use Integer in its specification,
--main = print $ MkRat 1 3 + MkRat 2 3
--main =print (fromInteger 2::Rat)
--main =print $ negate $ MkRat 2 3 

-- type which does not have type instances (Integer->Bool)
instance Eq Rat where 
  (==)::Rat -> Rat ->Bool
  u == v = a==c && b==d
   where 
    MkRat a b =simplify u
    MkRat c d =simplify v

    
-- main =print $ MkRat 1 3 == MkRat 3 9
--now Ord 

-- 0rdering is  new data type of three LT|EQ|GT less then equal to greater thenn
-- Ord 


-- In your code, 
-- you're defining the Ord instance for the Rat type, 
-- which enables comparison operations such as >=, <=, <, >, and equality checks like ==. Here's how the comparison works:


-- The compare function returns one of the three possible values:
-- LT (less than)
-- EQ (equal)
-- GT (greater than)
-- Other comparison operators like >= and < are derived from compare.
instance Ord Rat where 
  compare :: Rat ->Rat ->Ordering
  compare u v =compare (a*d)(c*d)
   where
    MkRat a b = simplify u
    MkRat c d = simplify v

-- main =print $ MkRat 2 10 >= MkRat 4 20
-- now num type class 


-- class Num a where
--   (+) :: a -> a -> a          -- Addition
--   (-) :: a -> a -> a          -- Subtraction
--   (*) :: a -> a -> a          -- Multiplication
--   negate :: a -> a            -- Negation (unary minus)
--   abs :: a -> a               -- Absolute value
--   signum :: a -> a            -- Sign of a number (-1, 0, or 1)
--   fromInteger :: Integer -> a -- Convert an Integer to this type

instance Num Bool where 
  False + b = b
  True + b  = not b
  False * b =False
  True * b = b
  abs b = b
  signum b = b
  negate b = b
  fromInteger = odd 


-- main =print(1::Bool)
-- main =print(2::Bool)
--main =print(3::Bool)
-- main =print(1+2::Bool)


data Tree a = Node a (Tree a) (Tree a) | Leaf
-- For the type Tree a to be an instance of Show, 
-- the type a (the type of values stored in the tree) must already be an instance of Show.
instance Show a => Show (Tree a) where
  show Leaf = "L"
  show (Node a t1 t2) = "(N " ++ show a ++ " " ++ show t1 ++ " " ++ show t2 ++ ")"

-- so that s why if a is instance if Show thats the reason we are able to
--    use show function here show (Node a t1 t2) = "(N " ++ show a ++ " " ++ show t1 ++ " " ++ show t2 ++ ")"
main = print $ Node 1 (Node 2 Leaf Leaf) Leaf
--main = print $ Node 1 Leaf (Node 2 Leaf Leaf)

instance Show a  => Show (Tree a ) where 
  show Leaf ="L"
  show (Node a t1 t2 )  ="(N" ++show a ++ ""

-- Show (Tree a) (Instance Declaration):

-- This specifies that the Tree a type will implement the Show type class.
-- It means you are defining how show works for values of type Tree a.

-- show 1 is "1".
-- Recursive calls:
-- show (Node 2 Leaf Leaf)
-- show Leaf

-- then left subtree
-- show (Node 2 Leaf Leaf)
-- = "(N " ++ show 2 ++ " " ++ show Leaf ++ " " ++ show Leaf ++ ")"
-- show (Node 1 (Node 2 Leaf Leaf) Leaf)
-- = "(N 1 (N 2 L L) L)"

instance Eq a => Eq (Tree a) where
  Leaf         == Leaf            = True
  Node a t1 t2 == Node a' t1' t2' = a == a' && t1 == t1' && t2 == t2'
  _            == _               = False


-- main = print $ Node 1 (Node 2 Leaf Leaf) Leaf == Node 1 (Node 2 Leaf Leaf) Leaf
-- main = print $ Node 1 (Node 2 Leaf Leaf) Leaf == Node 1 (Node 3 Leaf Leaf) Leaf
-- main = print $ Node 1 Leaf (Node 2 Leaf Leaf) == Leaf


-- functor type class class Functor f where
--   fmap :: (a -> b) -> f a -> f b

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node a t1 t2) = Node (f a) (mapTree f t1) (mapTree f t2)

-- this function applies a given function f (of type a -> b) to every value in a Tree a, creating a new Tree b.

instance Functor Tree where
  fmap = mapTree

-- main = print $ fmap (+1) (Node 1 (Node 2 Leaf Leaf) Leaf)