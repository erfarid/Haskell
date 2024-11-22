{-neptune code  -wefkhb

Write a function that counts the number of unique elements in a list of integers.
Do this without using higher order functions.
For example:
countUnique [1, 2, 2, 3, 4, 4, 5] == 5, because there are 5 unique elements: 1, 2, 3, 4, 5
-}

compare::Int [Int] ->Bool
compare x [] = False

countUnique :: [Int] -> Int
countUnique xs = length (map (\x -> helper x xs == 1) xs)

--main = print (countUnique [1, 2, 2, 3, 4, 4, 5])  -- 5
-- main = print (countUnique [1, 1, 1])  -- 1
-- main = print (countUnique [])  -- 0
-- main = print (countUnique [5, 6, 5, 6, 7, 8])  -- 4