-- Edited by Hajiyev Hajiaga


-- Hello world!
greeting :: String -> String -> String -> String
greeting x y z = x ++ y ++ z
--main = print( greeting "hello" " " "world" ) -- "hello world"

---------------------------------------------------------------------------------------
---- Define a function maxi with two arguments that delivers the maximum of the two.

maxi :: Int -> Int -> Int
--maxi a b = max a b --the maximum is the build in function here
maxi a b 
 |a > b = a
 |otherwise = b

--main = print( maxi 34 56 ) -- 56

---------------------------------------------------------------------------------------
---- Triple a number.

triple :: Int -> Int
triple x  = x*3
--main = print( triple 5 ) -- 15

---------------------------------------------------------------------------------------
---- Compute the cube of a number.
cube :: Int -> Int
cube x =x^3
--main = print( cube 4 )  -- 64
-- main = print( cube 8  ) -- 512

---------------------------------------------------------------------------------------
---- Check if a number is the sum of two other given numbers in any order.
    
issum :: Int -> Int -> Int -> Bool
issum x y z = (x + y == z) || (y + z == x) ||(z+x==y)

--main = print( issum 10 6 3 )  -- False
-- main = print( issum 10 6 4 )  -- True

---------------------------------------------------------------------------------------
---- Check if a number is odd. -- odd or even

isoddnr :: Int -> Bool
isoddnr x
 | even x  =False
 | otherwise = True    
--main = print(isoddnr 5) -- True
--main = print(isoddnr 6) -- False

---------------------------------------------------------------------------------------
---- Check if a number is multiple of 10.

ismult10 :: Int -> Bool
ismult10  x
 |mod x 10==0=True
 |otherwise =False 
--ismult10 x = x _ 10 == 0

--main = print( ismult10 20 ) -- True
-- main = print( ismult10 201 ) -- False

---------------------------------------------------------------------------------------
---- Write a function which returns true if a is divisible by b. 

divBy :: Int -> Int -> Bool
divBy a b 
 |mod a b==0=True
 |otherwise = False
--main = print (divBy 10 2) -- True
-- main = print (divBy 10 3) -- False
-- main = print (divBy 10 0) -- "Dvision by 0"

-- Difference between mod and rem
-- (-7) mod 3 returns 2 because the result takes the sign of the divisor (3), which is positive.
-- (-7) rem 3 returns -1 because the result takes the sign of the dividend (-7), which is negative.

---------------------------------------------------------------------------------------
---- Write a function which returns true if a is divisible by b or vice versa. Fill in the blanks

divAny :: Int -> Int -> Bool
divAny x y 
 |rem x y ==0 && rem y x ==0 =True
 |otherwise =False
--main =print (divAny  2 2 ) 
-- divAny a b = a _ b == 0 _ b _ a == 0

---------------------------------------------------------------------------------------
---- Given three integer numbers a, b and c, check if both a and b have the same remainder when divided by c.

sameRem :: Int -> Int -> Int -> Bool
sameRem a b c
 |rem a c == rem b c =True
 |otherwise =False
--main = print( sameRem 12 4 4) -- True
--main = print( sameRem 12 4 3) -- False
-- main = print( sameRem 13 4 3) -- True

---------------------------------------------------------------------------------------
---- Given two integers and a boolean value, check if the first integer is even, the second divisible by 13 and the boolean value is True.
-- Fill in the missing boolean operators.

check :: Int -> Int -> Bool -> Bool
check x y b  
 |even x && rem y 13==0 && b =True --a `rem`2 
 |otherwise = False
--main = print( check 4 26 True) -- True
-- main = print( check 5 26 True) -- False
-- main = print( check 5 23 True) -- False

--Recursion is a function that calls itself. We are trying to break questions into small parts.
---------------------------------------------------------------------------------------
---- Write a function that takes two arguments, say n and x, and computes their power,
-- in 2 versions - with recursion and without recursion.

power :: Int -> Int -> Int
power a b  =  a ^ b
 
--main = print( power 2 5 ) -- 32

powerrec  :: Int -> Int -> Int
powerrec x n
 |n==0 =1
 |otherwise = x * (power x (n-1) )

--powerrec _ 0 =1
--powerrec a b =a * (powerrec a ())
--main = print( powerrec 2 0) --  1
--main = print( powerrec 2 4 ) -- 16

---------------------------------------------------------------------------------------
---- Write a function which calculates the sum of the digits of a number.

digitSum :: Int -> Int
digitSum x
 |x==0=0
 |otherwise = rem x 10 + digitSum (x `div`10)
--main = print(digitSum 1234) -- 10
--main = print(1234/10)
--this always give us the quetient 
---------------------------------------------------------------------------------------

---- Write a function multiplyUntilOne that takes an integer n and returns the result of multiplying 
-- n by itself n times, then decreasing n by 1 after each multiplication until n reaches 1.

sumpowers :: Int -> Int
sumpowers n
  | n < 0     = error "Negative number"  -- handle negative numbers
  | n == 0    = 0                        -- base case for n = 0
  | otherwise = n^n + sumpowers (n - 1)  -- recursive summation of n^n


--main = print(sumpowers 5) -- 3413
-- main = print(sumpowers (-34)) -- Negative number
-- main = print(sumpowers 9) -- 405071317

---------------------------------------------------------------------------------------
---- Sum of squares
-- Compute the sum of the squares of numbers from 1 to n.

squareSum :: Int -> Int
squareSum 0 = 0  -- base case: if n is 0, the sum is 0
squareSum n = n^2 + squareSum (n - 1)  -- recursive case: n^2 + sum of squares of (n - 1)

--main = print(squareSum 100) --  338350
--another way of solving the same task 
squareSumex :: Int -> Int
squareSumex x 
  | x == 0    = 0  -- base case: if x is 0, return 0
  | otherwise = x^2 + squareSumex (x - 1) 
--main =print(squareSumex 5) 
-- Examples
--main = print(squareSum 5) -- 55
-- main = print(squareSum 0) -- 0
  -- 338350

---------------------------------------------------------------------------------------
---- Given a positive integer, find the sum of the odd numbers up to that number starting from 1.

sumOdd :: Int -> Int
sumOdd x
 |x<=0=0
 |odd x = x + sumOdd(x-1)
 |otherwise = sumOdd(x-1)


--main =print(sumOdd 21)
 -- 121
--sumOdd 10 //25 = 9+7+5+3+1
-- sumOdd -13 // n has to be positive

---------------------------------------------------------------------------------------
---- Compute for a given positive n the sum of 2i*(2i+1), for i from 1 to n. E.g. for n=3 the sum is 68.

f :: Int -> Int
f x 
 |x<=0=0
 |otherwise=2*x*(2*x+1) + f(x-1)

--main = print( f 0 )
 -- 0
--main = print( f 3 )
 -- 68

---------------------------------------------------------------------------------------
---- Write GetLastPositive function
-- Returns the number decreased by the last digit if positive, otherwise returns -1.

getLastPositive :: Int -> Int
getLastPositive x
  | x < 0     = -1               -- If the number is negative, return -1
  | otherwise = x - (rem x 10)    -- Subtract the last digit from the number if it's positive


--main = print (getLastPositive 5856)
   -- 5850  
-- main = print( getLastPositive 689255) -- 689250
--main = print( getLastPositive 0)      -- 0
-- main = print( getLastPositive 8)      -- 0
-- main = print( getLastPositive (-8554)) -- -1

---------------------------------------------------------------------------------------
---- Convert digit to string
-- Convert an integer from 0 to 3 into a word, otherwise return "Not less or equal to 5".

digitToString :: Int -> String
digitToString x
 | x == 1 = "one"
 | x == 2 = "two"
 | x == 3 = "Three"
 |otherwise ="Not less or equal to 5"

--main = print( digitToString 3) -- "Three"
-- main = print( digitToString 8) -- "Not less or equal to 5"
--main = print( digitToString (-1)) -- "Not less or equal to 5"

---------------------------------------------------------------------------------------
---- Average of 5 numbers
-- Compute the average of 5 numbers.

av5 :: Int -> Int -> Int -> Int -> Int -> Double
av5 a b c d e  = fromIntegral(a+b+c+d+e)/5.0

--main = print( av5 1 2 3 4 5) -- 3.0
-- main = print(av5 3 5 7 9 10) -- 6.8

---------------------------------------------------------------------------------------
---- Odd-even operation
-- Return the product if both numbers are odd, sum if both are even, otherwise return 0.

oddEven :: Int -> Int -> Int
oddEven x y 
 |odd x && odd y =  x * y
 |even x && even y =x + y
 |otherwise =0


--main = print(oddEven 474 8983) -- 0
--main = print(oddEven 6 6) -- 12
-- main = print(oddEven 7 7) -- 49

---------------------------------------------------------------------------------------
---- Are numbers sorted?
-- Check if 5 numbers are sorted in increasing order.

isSorted :: Int -> Int -> Int -> Int -> Int -> Bool
isSorted a b c d x
 |a<=b && b<=c && c<=d && d<=x = True
 |otherwise =False

--main = print(isSorted 1 1 1 1 1) -- True
--main = print(isSorted 1 2 3 4 5) -- True
--main = print(isSorted 4 3 2 1 0) -- False

---------------------------------------------------------------------------------------
---- Transform days into years, weeks, and days.
-- Convert the number of days into a string of years, weeks, and days.

transform :: Int -> String
transform x = show (x `div`365) ++ " year " ++ show((x `mod `365) `div` 7) ++ "  week " ++ show ((x `mod`365)`mod`7) ++ " days "

--main = print(transform 375) -- "1 year 1 week 3 days"
-- main = print(transform 365) -- "1 year 0 week 0 days"
-- main = print(transform 1050) -- "2 year 45 week 5 days"
-- main = print(transform 2500) -- "6 year 44 week 2 days"

-- / this operator is specifically for float and double for integer we use `div`
-- mod returns the remainter of the numbers

---------------------------------------------------------------------------------------
---- Armstrong number
--  If sum of cubes of each digit of the number is equal to the number itself, then the number is called an   Armstrong number.
--  153 = 1^3 + 5^3 + 3^3
--  Given a positive integer number, write a function to determine whether it is an Armstrong number or not.


armstrong :: Int -> Bool
armstrong x = x == sumOfCubes x

sumOfCubes :: Int -> Int
sumOfCubes 0 = 0
sumOfCubes n = (rem n 10) ^ 3 + sumOfCubes (n `div` 10)
--div gives us the quetient 
--main = print(armstrong 153) -- True
--main = print( armstrong 370) -- True
-- main = print( armstrong 0) -- True
-- main = print( armstrong 12) -- False
