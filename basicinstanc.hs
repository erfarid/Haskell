
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
instance Show Rat where
    show r = show (num r) ++ "/" ++ show (denom r)



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

main = print (simplify $ MkRat 2 3 )