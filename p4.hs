-- 1 ----------------------------------------------------------------------
-- Write a recursive function that computes the n-th multiple of an x plus 10 (n*x+10).
f1 :: Int -> Int -> Int
f1 0 x = 10
f1 n x = x + f1 (n - 1) x

-- main = print (f1 5 2) -- 20

-- 2 ----------------------------------------------------------------------
-- Add 2 to every odd number of a list, and subtract 2 from every even number.
f2 :: [Int] -> [Int]
f2 [] = []
f2 (x : xs)
    | odd x = (x + 2) : f2 xs
    | otherwise = (x - 2) : f2 xs

--main = print (f2 [1..5]) -- [3,0,5,2,7]

-- 3 ----------------------------------------------------------------------
-- Write a function for the square, the cube, and so on up to the n-th power of a number,
-- so that increasing powers of a number are obtained in a list.
f5 :: Int -> Int -> [Int]
f5 1 x = []
f5 n x = f5 (n - 1) x ++ [x ^ n]

-- main = print (f5 5 2)  -- [4,8,16,32]

-- 4 ----------------------------------------------------------------------
-- Replicate n>0 times a list.
f6 :: Int -> [Int] -> [[Int]]
f6 0 x = []
f6 n x = [x] ++ f6 (n - 1) x

--f6 n x = [x] ++ f6 (n - 1) x

--main = print (f6 3 [1..5]) -- [[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]


-- 5 ----------------------------------------------------------------------
-- Insert 0 at the middle of each sublist.
f7 :: [[Int]] -> [[Int]]
f7 [] = []
f7 (x : xs) = (take (length x `div` 2) x ++ [0] ++ drop (length x `div` 2) x) : f7 xs


f71::[[Int]] -> [[Int]]
f71 [] = []
f71(x:xs) =(take (length x `div` 2)x ++[0] ++ drop (length x `div` 2)x):f71 xs
--f7 (x : xs) = (insert0 x) : f7 xs

--insert0 :: [Int] -> [Int]
--insert0 l = take m l ++ [0] ++ drop m l  
--  where m = length l `div` 2

--main = print (f71 [[1..10], [1..11], [], [1], [1,2]]) 
-- [[1,2,3,4,5,0,6,7,8,9,10],[1,2,3,4,5,0,6,7,8,9,10,11],[0],[0,1],[1,0,2]]

-- 6 ----------------------------------------------------------------------
-- Extract the elements smaller then the head element of a list. Assume that the list is not empty.
f8 :: [Int] -> [Int]
f8 [] = []
f8 (h : t) = f8a h t

f8a :: Int -> [Int] -> [Int]
f8a h [] = []
f8a h (x : xs)
    | x < h = x : f8a h xs
    | otherwise = f8a h xs


-- same question here i am doing it again 

f81::[Int] ->[Int]
f81 []= []
f81(h:hs) = f8c h hs

f8c::Int -> [Int] ->[Int]
f8c h [] =[]
f8c h (x:xs)
  |h > x =x:f8c h xs
  |otherwise =f8c h xs
--element  smaller then head 

--main = print (f81 [5,1,2,3,4,5,3,6,7,1,8]) -- [1,2,3,4,3,1]

-- 7 ----------------------------------------------------------------------
-- Eliminate in a list, the sublists that are longer or equal to 10.
cond9 :: [Int] -> Bool
cond9 x = length x < 10

f9 :: [[Int]] -> [[Int]]
f9 [] = []
f9 (x : xs)
    | cond9 x = x : f9 xs
    | otherwise = f9 xs

-- main = print (f9 [[1..10], [1..11], [1..5], []]) -- [[1,2,3,4,5],[]]


task7::[[Int]] -> [[Int]]
task7 [] = []
task7 (x:xs)
 |length x >= 10 = task7 xs
 |otherwise = x:task7 xs

--main =print(task7 [[1..10], [1..11], [1..5], []])

-- 8 ----------------------------------------------------------------------
-- Compute the greatest common divisor in a recursive function.
f10 :: Int -> Int -> Int
f10 a b
    | a > b = f10 (a - b) b
    | b > a = f10 a (b - a)
    | otherwise = a

-- i can do GCD like this using recursion 

task10::Int ->Int -> Int
task10 a b 
 |a>b =task10 (a-b) b
 |b>a =task10 a (b-a)
 |otherwise =b

--main =print(task10 5 17)
--in this algorithm we are subtracting that way that both the numbers will become equal

--f10 :: Int -> Int -> Int
--f10 a 0 = a
--f10 0 b = b
--f10 a b
--    | a > b = f10 (rem a b) b
--    | b > a = f10 a (rem b a)
--    | otherwise = a

task10a:: Int -> Int -> Int
task10a a 0 = a
task10a 0 b = b
task10a a b 
  |a>b =task10a (rem a b) b
  |b>a =task10a a (rem b a)
  |otherwise = a --here that means when both the numbers are equal

--here finding gcd using rem

--main = print (f10 24 12) -- 12
--main = print (f10 270 192) -- 6

-- 9 ----------------------------------------------------------------------
-- Given a list of Ints, remove the element at the given position.
remElemAt :: Int -> [Int] -> [Int]
remElemAt i list = take i list ++ drop (i + 1) list



rematIndex ::Int-> [Int] ->[Int]
rematIndex x [] =[]
rematIndex x ls = take x ls ++ drop (x+1) ls 

--main =print(rematIndex 2 [1,2,3,6,4,5])
 
-- main = print (remElemAt 6 [1..7]) -- [1,2,3,4,5,6]
-- main = print (remElemAt 2 [1..7]) -- [1,2,4,5,6,7]
-- main = print (remElemAt 9 [1..7]) -- [1,2,3,4,5,6,7]


-- 10 ----------------------------------------------------------------------
-- Switch places the first and last element of a 3 element list.

reorder :: [String] -> [String]
reorder [t, b, h] = [h, b, t] -- reorder (t:b:h:[]) = h:b:t:[]
 

reordera :: [String] -> [String]
reordera [t, b ,h] = [ h, b, t]

--main=print(reordera ["a","b","c"])

-- main = print (reorder ["tail", "body", "head"])              -- ["head", "body", "tail"]
-- main = print (reorder ["tails", "body", "heads"] )           -- ["heads", "body", "tails"]
-- main = print (reorder ["ground", "rainbow", "sky"])          -- ["sky", "rainbow", "ground"]

-- 11 ----------------------------------------------------------------------
-- Write a function to convert a list of a person's names into initials (first letter sepparated by a '.').

initials :: [String] -> String
initials [] = []
initials (x : xs) = head x : '.' : initials xs



-- main = print (initials ["Sam", "Harris"]) -- "S.H."
-- main = print (initials ["Howard", "Phillips", "Lovecraft"]) -- "H.P.L."

-- 12 ----------------------------------------------------------------------
-- Given a list of integers, find the minimum of a list (assume the list is not empty).
minimum1 :: [Int] -> Int
minimum1 [x] = x
minimum1 (x : y : xs)
    | x < y = minimum1 (x : xs)
    | otherwise = minimum1 (y : xs)




-- [1,0,3,4,5]
-- minimum1 [0,3,4,5]
-- minimum1 [0,4,5]
-- minimum1 [0,5]
-- minimum1 [0]

-- main = print (minimum1 [1..5])        -- 1
-- main = print (minimum1 [10,9,8,7,6])  -- 6
-- main = print (minimum1 [8,6,4,10,12]) -- 4

minimum2 :: [Int] -> Int
minimum2 [x] = x
minimum2 (x : xs) = min x (minimum2 xs)



-- minimum2 [1,2,0,-1] -> min 1 (min 2 (min 0 (-1)))
-- minimum2 [1,2,0,-1] -> min 1 (min 2 -1)
-- minimum2 [1,2,0,-1] -> min 1 -1
-- minimum2 [1,2,0,-1] -> -1

-- main = print (minimum2 [1..5])        -- 1
-- main = print (minimum2 [10,9,8,7,6])  -- 6
-- main = print (minimum2 [8,6,4,10,12]) -- 4

minimum3 :: [Int] -> Int
minimum3 x = minimum x

--main = print (minimum3 [1..5]) -- 1

-- 13 ----------------------------------------------------------------------
-- Print the max and min number of a string

maxmin :: [Int] -> String
maxmin l = auxMaxMin (tail l) (head l) (head l)

--here tail l means every element accept the first element 

auxMaxMin :: [Int] -> Int -> Int -> String
auxMaxMin [] max min = "max = " ++ show max ++ ", min = " ++ show min
auxMaxMin (x : xs) max min
  | x > max = auxMaxMin xs x min
  | x < min = auxMaxMin xs max x
  | otherwise = auxMaxMin xs max min



--show is predified function use to  convert int to string type 
--main = print (maxmin [4,6,2,1,9,63,-134,566]) -- "max = 566, min = -134"
-- main = print (maxmin [-52, 56, 30, 29, -54, 0, -110]) -- "max = 56, min = -110"
-- main = print (maxmin [5]) -- "max = 5, min = 5"



-- P1 ----------------------------------------------------------------------
-- Compute the triple of the negative elements of a list up to the first positive number.

f3 :: [Int] -> [Int]
f3 [] = []  -- Base case: if the list is empty, return an empty list
f3 (x:xs)
  | x < 0 = x * 3 : f3 xs  -- Multiply negative numbers by 3 and continue processing
  | otherwise = []  -- Stop recursion when the first non-negative number is encountered

--main = print (f3 [-1, -3, -5, -5, 2, -4, -5])


-- P2 ----------------------------------------------------------------------
-- Write a function that keeps the non-zero elements of a list and then multiply by 2 every element.

f4 :: [Int] -> [Int]
f4 [] =[]
f4 (x:xs)
  |x /=0 =x*2:f4 xs
  |otherwise =f4 xs
--main = print (f4 [1,2,3,0,5,0,6,0,0,0,0]) -- [2,4,6,10,12]


-- P3 ----------------------------------------------------------------------
-- Function to check if a single Boolean value is True
mul :: Bool -> Bool
mul x
  | x == True = True
  | otherwise = False

-- Function to check if a list contains at least one True value
ifOneTrue :: [Bool] -> Bool
ifOneTrue [] = False
ifOneTrue (x:xs) = mul(x || ifOneTrue xs)

--easy methof

ifOneTrue1 :: [Bool] -> Bool
ifOneTrue1 [] = False
ifOneTrue1 (x:xs) = x || ifOneTrue xs

  --  False || False || False || False

--main = print (ifOneTrue [False, False, False]) -- False

--ifOneTrue' :: [Bool] -> Bool -- (use the 'or' function)

-- main = print (ifOneTrue' [False, False, False]) -- False



-- P4 ----------------------------------------------------------------------
-- Check if all elements of a list of Booleans are True.

--ifAllTrue :: [Bool] -> Bool

--  True && False && True && True

-- main = print (ifAllTrue [True, False, True]) -- False

--ifAllTrue' :: [Bool] -> Bool -- (use the 'and' function)

-- main = print (ifAllTrue' [True, False, True]) -- False



-- P5 ----------------------------------------------------------------------
-- Write a function that checks if at least one of the elements in a list is even.

--isOneEven :: [Int] -> Bool

-- main = print (isOneEven [1,1,3])   -- False
-- main = print (isOneEven [1..9])    -- True
-- main = print (isOneEven [2,4..14]) -- True
-- main = print (isOneEven [])        -- False


-- P6 ----------------------------------------------------------------------
-- Write a function that checks if all of the elements in a list are even.

--allEven :: [Int] -> Bool

-- main = print (allEven [2,4,6])   -- True -- [2,4,6] -> even 2 && even 4 && even 6 && True
-- main = print (allEven [1..9])    -- False
-- main = print (allEven [2,4..14]) -- True
-- main = print (allEven [])        -- True



-- P7 ----------------------------------------------------------------------
-- Collect the divisors of a number in a list.

--divisors :: Int -> [Int] -- (use a list to accumulate the values)

-- main = print (divisors 18) -- [1,2,3,6,9,18]

--divisors2 :: Int -> [Int] -- (build a list recursively)

-- main = print (divisors2 18) -- [1,2,3,6,9,18]



-- P8 ----------------------------------------------------------------------
-- Delete every second element from a list.

--del2 :: [Int] -> [Int]

-- main = print (del2 [1..10]) -- [1,3,5,7,9]
-- main = print (del2 [1..11]) -- [1,3,5,7,9,11]
