{-# LANGUAGE ParallelListComp #-}

--new to learn scanl ,iterate,
main :: IO ()


-- Higher order function foldr, foldl, dropWhile, scan, iterate


--concat  ->same work as flatten 
--concatMap ->combination of map and concat 
--concatMap (\x -> [x, x+1]) [1, 2, 3]
-- Result: [1, 2, 2, 3, 3, 4]


--scanl (+) 0 [1, 2, 3, 4]
-- instead of returning just a final result, they return a list of intermediate results.
-- Result: [0, 1, 3, 6, 10] we have scanl and scanr

-- 1.  Write a function that calculates the sum of all numbers in a list that are divisible by 3 and 5 using a foldl and filter function.
hof1 :: [Int] -> Int
hof1 ls =foldl (+) 0 (filter(\x->x `mod` 3==0 && x `mod` 5==0 ) ls)
--main = print(hof1 [1..20]) -- 15
--main = print(hof1 [1..100]) -- 315


-- 2. Given a list of Strings, write a function that concatenates them all together with a space between them.
--    Hint: use foldr.
hof2 :: [String] -> String
hof2 = foldr1 (\a b -> a ++ " " ++ b)
--here foldr1 dont need any initial value it uses first element of the list as a accumulator 
--main = print(hof2 ["Hello", "world"]) -- "Hello world"
--main = print(hof2 ["I" , "am", "a", "programmer"]) -- "I am a programmer"


-- 3. Given a list of Integers. Write a function that drops elements while the element is positive.
--    Hint: use dropWhile.
hof3 :: [Int] -> [Int]
hof3 ls  =dropWhile(\x->x>=0) ls 
--main = print(hof3 [10,9..(-10)]) -- [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]


-- 4. Write a function to print first 10 even numbers starting from 0.
--    Use take and iterate.
hof4 :: [Int]
hof4 = take 10 (iterate(+2) 0)
--main = print(hof4) -- [0,2,4,6,8,10,12,14,16,18]


hof42 :: [Int]
hof42 = take 10 (filter (\x -> x `mod` 2 == 0) [0..])


--main = print(hof42) -- [0,2,4,6,8,10,12,14,16,18]


-- 5. Write a function that generates the list of all powers of 2.
--    Use iterate.
hof5 :: [Int]
hof5  =take 10 (map(\x->2^x)[0,1..])
--main = print(take 10 hof5) -- [1,2,4,8,16,32,64,128,256,512]


-- 6. Given a list of Integers. Use scanl to calculate the sum of all elements.
hof6 :: [Int] -> [Int]
hof6 ls =scanl (+) 0 ls

--main = print(hof6 [1..10]) -- [0,1,3,6,10,15,21,28,36,45,55]


-- Tuple zip, zipWith

-- 2. Given a tupple of three elements, write three function that extract the first, second, and third element of the tuple.
fstTriple :: (a, b, c) -> a
fstTriple (a,_,_) =  a


sndTriple :: (a, b, c) -> b
sndTriple(_ , b , _ ) = b


thdTriple :: (a, b, c) -> c
thdTriple (_,_,c) =c
t = (1,2,3)

--main = print(fstTriple t) -- 1
--main = print(sndTriple t) -- 2
--main = print(thdTriple t) -- 3


-- 4. Given two lists of Integers of the same length.
--    Write a function that pairs them up and computes the maximum of each pair.

t2 :: [Int] -> [Int] -> [Int]
t2 list1 list2 = [max x y | (x, y) <- zip list1 list2]
--main = print(t2 [1..10] [10,9..1]) -- [10,9,8,7,6,6,7,8,9,10]



-- List comprehensions



-- 2. Generate the following list [(1,1),(1,2),(2,1),(2,2)]
l2 :: [(Int, Int)]
l2 = [(x,y) |x<- [1,2], y<-[1,2]]

--main = print(l2)

-- 4. Generate the list [(1,5),(2,6),(3,7),(4,8),(5,9),(6,10)]
l4 :: [(Int, Int)]
l4 =[(x,y)|(x,y)<- zip [1..6] [5..10]]
--main = print(l4)


-- 5. Generate the list [1,2,2,3,3,3,4,4,4,4,...,10,..,10]

--concat  ->same work as flatten 
--concatMap ->combination of map and concat 
--concatMap (\x -> [x, x+1]) [1, 2, 3]
-- Result: [1, 2, 2, 3, 3, 4]
l5 :: [Int]
l5 =concatMap(\x->take x (repeat x )) [1..10]

l51 :: [Int]
l51 = [x | x<-[1..10], y<-[1..x]]


l52 :: [Int]
l52 = concat (map (\x -> take x (repeat x)) [1..10])
--main = print(l5)
--main = print(l51)
--main = print(l52)


-- 6. Generate the list [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
l6 :: [[Int]]
l6  =map (\x -> take x (repeat x)) [1..10]
--main = print(l6)


-- 7. Generate 100 Pythagorean triples : [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]
--l7 :: [(Int, Int, Int)]

-- just c is restricted, not the number of pairs
l71 = [ (a,b,c) | c <- [1..100], b<-[1..c], a<-[1..b] , a*a + b*b == c*c ]

-- a,b,c restricted but not the nr of pairs
l72 =  take 100 [ (a,b,c) | a <- [1..100], b<-[1..100], c <- [1..100] , a*a + b*b == c*c && a<b && b<c ]

--main = print(l7)
--main = print(l71)
--main = print(l72)


-- 9. Generate the following list [4, 16, 36, 64, 100, 144, 196, 256, 324, 400]
l9 :: [Int]
l9 =[x^2|x<-[2,4..20]]

l92 :: [Int]
l92 = map(\x->x*x) [2,4..20]

--main = print(l9)
--main = print(l92)


-- 10. List powers of 2 from 1 to 10.
l10 :: [Int]
l10 = [2^x |x<-[1..10]]


--main = print(l10)


-- 11. List the divisors of 90.
l11 :: [Int]
l11 = [x  | x<- [1..90] , 90 `mod` x==0 ]

l22::[Int]
l22 =filter(\x->90 `mod` x==0) [1..90]
--main = print(l11)


-- 12. List dominoes: [(0,0),(0,1),(1,1),(0,2),(1,2),(2,2),...(9,9)]
-- Domino (1,0) is not in the list because it is already in it as (0,1).
l12 :: [(Int, Int)]
--l12 = [(a, b) | a <- [0..9], b <- [a..9]]
l12 = concat [[(x, y) | y <- [0..x]] | x <- [0..9]]
--main = print(l12)


-- 13. Construct the list [(1,'a'),(2,'b'),...(26,'z')], i.e. pair up numbers with abc letters.
l13 :: [(Int, Char)]
l13 = zip[1..26] ['a' ..'z']

--main = print(l13)


-- 14. Generate a list of length 10 whose elements are False, True, False, True... (alternating)
l14 :: [Bool]
l14 =[even x |x <-[1..10]]


l42 :: [Bool]
l42 = take 10 (cycle[False,True])



--main = print(l14)
--main = print(l42)


-- 18. Generate the following list [(1,1),(2,2),(3,3),(4,4),(5,5)]
l18 :: [(Int, Int)]
l18  =zip [1..5] [1..5]
--main = print(l18)




-- 19. Generate [(1,2,3),(2,4,6),(3,6,9),(4,8,12),(5,10,15)]
l19 :: [(Int, Int, Int)]
l19 =  [(a,2*a,3*a) |a<- [1..5] ]
--main = print(l19)

l192 :: [(Int, Int, Int)]
l192  =zip3 [1..5] [2,4..10] [3,6..15]
 

l193 :: [(Int, Int, Int)]
l193 =zipWith3 (\x y z -> (x, y, z)) [1..5] [2,4..10] [3,6..15]
--main = print(l192)

-- 21. Generate 5 tuples like [(1,2),(2,3),(3,4),(4,5),(5,6)]
increase :: [(Int, Int)]
increase = [(a, a+1) | a <- [1..5]]
--main = print(increase)


-- 22. Make triple tuples like [(1,2,3),(4,5,6),(7,8,9),(10,11,12),(13,14,15)]
tripl :: [(Int, Int, Int)]
tripl = [(x, x+1, x+2) | x <- [1, 4..13]]

--main = print(tripl)


-- 23. Given a list of lists, transform it into tuples of sublists such that two continuous sublists form pairs
-- (if there are odd number of sublists the last has as pair the empty list)

pairs1 :: [[Int]] -> [([Int], [Int])]
pairs1 ls = zip ls (tail ls ++ [[]])


--main =print(tail [1..5])
--main = print (pairs1 [[1, 2, 3], [5, 6], [7, 8, 9, 10], [11, 3], [1..5]])


pairs :: [[Int]] -> [([Int], [Int])]
pairs [] = []  -- If the input list is empty, return an empty list
pairs [x] = [(x, [])]  -- If there's only one sublist, pair it with an empty list
pairs (x:y:xs) = (x, y) : pairs xs  -- Pair the first two sublists and recursively pair the rest


--main = print(pairs [[1,2,3], [5,6], [7,8,9,10], [11,3], [1..5]])
--main = print(pairs [[1,2,3], [5,6], [7,8,9,10], [11,3]])


-- 25. Generate quadruples of a number, its square, its cube, and its biquadratic (power 4)
-- where the number are in the 1..20 interval

quadruple :: [(Int, Int, Int, Int)]
quadruple  =[(a,a^2,a^3,a^4)|a<-[1..20]]
--main = print(quadruple)


-- 26. Form triple tuples of 3 lists selecting one element from each list.
-- You must use list comprehension.
-- E.g. for ([1..10],[20..25],[35..47]) the result is 
-- [(1,20,35),(2,21,36),(3,22,37),(4,23,38),(5,24,39),(6,25,40)]

-- Extract the first of a triple.
--fst3 :: (a,b,c) -> a

-- Extract the second of a triple.
--snd3 :: (a,b,c) -> b

-- Extract the third element of a triple.
--thd3 :: (a,b,c) -> c

tri :: ([Int], [Int], [Int]) -> [(Int, Int, Int)]
tri (a, b, c) = [(a !! i, b !! i, c !! i) | i <- [0..min (length a) (min (length b) (length c)) - 1]]

tri2 :: ([Int], [Int], [Int]) -> [(Int, Int, Int)]
tri2 (a,b,c) =[(x,y,z) |(x,y)<-zip a b ,z<-c]


tri3 :: ([Int], [Int], [Int]) -> [(Int, Int, Int)]
tri3 (a,b,c) = zip3 a b c

--main = print(tri ([1..10],[20..25],[35..47]))
--main = print(tri2 ([1..10],[20..25],[35..47]))
--main = print(tri3 ([1..10],[20..25],[35..47]))


-- 27. Write a function duplicates which checks if there are neighbor duplicates in a list.
duplic :: [Int] -> Bool
duplic [x] = False
duplic  [] = False
duplic (x:y:xs) =x == y || duplic (y:xs)

--main = print(duplic [1, 1]) -- True
--main = print(duplic [2]) -- False
--main = print(duplic [1, 2, 3, 4, 5, 6, 7, 8, 9]) -- False
--main = print(duplic [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0]) -- True
--main = print(duplic [1, 2, 3, 4, 4]) -- True


-- 28. Write a function that removes neighbor duplicates in a list.
duplicRem :: [Int] -> [Int]
duplicRem [] = []  -- Empty list case
duplicRem [x] = [x]  -- Base case: a single element, just return it
duplicRem (x:y:xs)  -- Pattern for lists with at least two elements
  | x == y    = duplicRem (y:xs)  -- Skip duplicate and continue
  | otherwise = x : duplicRem (y:xs)  -- Keep x and check the rest of the list

--main = print (duplicRem [1, 1, 0, 5, 0, 0, 6, 0, 0, 0, 7, 5, 0, 0, 0, 0, 8, 0, 5, 0, 0, 0])


-- 29. Transform the sub-sub lists into one list of sublists
f :: [[[Int]]] -> [[Int]]
--f xs = [y | z <- xs, y <- z]
f ls =concat ls

main = print(f [[[1,2,3], [3,4,5]], [[1,2,3], [3,4,5], [7,8,9]]]) -- [[1,2,3],[3,4,5],[1,2,3],[3,4,5],[7,8,9]]

