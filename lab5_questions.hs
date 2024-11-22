--main :: IO ()

---- Recursion, List

-- 1. Multiply the digits of a number e.g. for 123 is 6.

prodDigits :: Int -> Int
prodDigits 0 = 1  -- Base case: the product of digits of 0 is 1
prodDigits x = (x `mod` 10) * prodDigits (x `div` 10)  -- Multiply the last digit with the product of the rest

--main = print(prodDigits 503) -- 0
--main = print(prodDigits 54213) -- 120


-- 2. Sum numbers from 12..N in a recursive function, where N is positive.

fn :: Int -> Int
fn x
  |x<=0 = error "N can not be zero or negative or less then 11"
  |x==12=12
  |otherwise = x + fn(x-1)

--main = print(fn (-10)) -- N can not be zero or negative or less then 11
--main = print(fn 14)  -- 39

--12+13+14 =30
-- 3. Compute the sum 1+ 1*2 + 1*2*3+ 1*2*3*4+ 1*2*3*4*5+ ...+1*2*3*...*n 
-- where n is a positive number.

helper::Int ->Int
helper x
  |x==0=1
  |otherwise =x*helper(x-1)

--main =print(helper(3))

sump :: Int -> Int
sump x 
  |x==1=1
  |otherwise =helper(x) + sump(x-1)

--main = print(sump 5) -- 153

----Higher order function


-- 4. Cut a list in 4 parts quarter, middle, third quarter. 
-- E.g. cut [1..10] -> [[1,2], [3,4,5], [6,7], [8,9,10]]

cut :: [Int] -> [[Int]]
cut x  =[take q1 x,take q2 (drop q1 x),take q1 (drop (q1+q2)x),take (a-b+1) (drop(a+b )x)  ]
 where
    n=length x
    q1 =n `div` 4
    q2 =n `div` 2 -q1
    a =n `div` 2
    b= a `div `2
    

--main =print(splitAt 3 [1,2,3,4,4,6,7])
--main = print (cut [1..10]) -- [[1,2],[3,4,5],[6,7],[8,9,10]]
--main = print(cut [1..11]) -- [[1,2],[3,4,5],[6,7,8],[9,10,11]]
--main = print(cut []) --[[],[],[],[]]
--main = print(cut [21]) --[[],[],[],[21]]
--main = print(cut [1..21]) -- [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20,21]]


-- 5. Extract the third element of a non-empty list. 

m2 :: [Int] -> Int
m2 [] =error "error your list is empty"
m2 x 
 |length x<3 = error "length less than 3"
 |otherwise =x!!2

--main =print(null [])
--this function cheak whether the list is empty or not 

--main = print(m2 [1..5]) -- 3
--main = print(m2 [1..4]) -- 3
--main = print(m2 [1]) -- length less than 3 
--main = print(m2 []) -- your list is empty


-- 6. Triple every element of a list

f1 :: [Int] -> [Int]
f1 x = map((*3))x

--main = print(f1 [1,5,3,1,6])  -- [3,15,9,3,18]


-- 7. Compute the square of positives and change the sign of negatives.

f2 :: [Int] -> [Int]
f2 [] =[]
f2 (x:xs)
  | x > 0 = x * x : f2 xs
  | otherwise = -x : f2 xs
--main = print(f2 [1, 2, 0, -2, 3, -4]) -- [1,4,0,2,9,4]


---- Higher order functions

-- Earlier exemples rewritten with higher order functions: map, foldr, filter, takeWhile. 
-- Operations with lists: write functions for the followings


-- 1.Keep the head of every sublist (assume sublists are not empty).
-- e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [1, 3, 5]
heads :: [[Int]] -> [Int]
heads ls =map(\x->head x) ls

--main = print(heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]]) -- [1,3,5]

-- 2. Keep the tails of a list.
-- e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [[2, 3], [4], [7, 8, 9]] 
tails :: [[Int]] -> [[Int]]
tails ls =map(\x->tail x) ls

--main = print(tails [[1, 2, 3], [3, 4], [5, 7, 8, 9]]) -- [[2, 3], [4], [7, 8, 9]] 


-- 3. Add 100 to the numbers of a list.
--g :: Int -> Int

add100 :: [Int] -> [Int]
add100 ls =map((+100)) ls

add100' :: [Int] -> [Int]
add100'[] =[]
add100' (x:xs) =x+100 : add100' xs
--main = print(add100 [1..8]) -- [101,102,103,104,105,106,107,108]
--main = print(add100' [1..8]) -- [101,102,103,104,105,106,107,108]


-- 4. Triple the elements of a list.
triples :: [Int] -> [Int]
triples ls =map(*3)ls

--main = print(triples [1..5]) -- [3,6,9,12,15]

triples2 :: [Int] -> [Int]
triples2[] =[]
triples2 (x:xs) =x*3:triples2 xs
--main = print(triples2 [1..5]) -- [3,6,9,12,15]


-- 5. Check if the numbers of a list are odd.
isoddnrs :: [Int] -> [Bool]
isoddnrs ls =map(\x->odd x) ls

isoddnrs2 :: [Int] -> [Bool]
isoddnrs2[] =[]
isoddnrs2(x:xs)
 |odd x =True:isoddnrs2 xs
 |otherwise  =False:isoddnrs2 xs

--main = print(isoddnrs [1..5]) -- [True,False,True,False,True]
--main = print(isoddnrs2 [1..5]) -- [True,False,True,False,True]


-- 6. Check if the numbers of a list are multiple of 10.
ismult10 :: [Int] -> [Bool]
ismult10 ls =map(\x->x `mod`10==0) ls
 --remember mod is used for remainder
 --where as div is used for quetient 

--main = print(ismult10 [1..20])
--[False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,True]


-- 7. Collect in a list the last digits of the numbers of a list.
lastdigits :: [Int] -> [Int]
lastdigits ls = map(\x->x `mod` 10) ls

--main = print(lastdigits [1..35])
-- [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5]


-- 8. Compute the cube of the numbers of a list.
cubes :: [Int] -> [Int]
cubes ls = map(\x->x^3) ls

--main = print(cubes [1..10]) -- [1,8,27,64,125,216,343,512,729,1000]
--main = print(cubes []) -- []


cubes2 :: [Int] -> [Int]
cubes2[] =[]
cubes2 (x:xs) =x^3:cubes2 xs

--main = print(cubes2 [1..10]) -- [1,8,27,64,125,216,343,512,729,1000]

--cubes3 :: [Int] -> [Int]


--main = print(cubes3 [1..10]) -- [1,8,27,64,125,216,343,512,729,1000]


-- do not confuse cubes of number with powers of 3 with !!!
powersof3 :: [Int] -> [Int]
powersof3 ls =map(\x ->3^x) ls

--main = print(powersof3 [1..10]) -- [3,9,27,81,243,729,2187,6561,19683,59049]


powersof33 :: [Int] -> [Int]
powersof33 [] =[]
powersof33 (x:xs) =3^x : powersof33 xs

--main = print(powersof33 [1..10]) --[3,9,27,81,243,729,2187,6561,19683,59049]


-- 9. Reverse every sublist of a list.
revsub :: [[Int]] ->  [[Int]]
revsub ls =map((reverse)) ls

--main = print(revsub [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[3,2,1],[6,5],[],[10,9,8,7]]



rev::[Int] ->[Int]
rev [] =[]
rev(x:xs) =rev xs ++ [x]
 
 
 --rev xs:x can not write it like that beacuse we can not append element in the list 
revsub1::[[Int]] ->[[Int]]
revsub1 [] = []
revsub1 (x:xs) =rev x:revsub1 xs


--main = print(revsub1 [[1,2,3],[5,6],[],[7,8,9,10]])

-- 10. Keep the last elements of the sublists of a list in one list 
-- (assume the sublists are not empty).
-- E.g. [[1,2,3],[5,6],[1],[7,8,9,10]] -> [3,6,1,10]
lasts :: [[Int]] -> [Int]
lasts ls =map(\x->x !! (length x-1))ls

--main = print(lasts [[1,2,3],[5,6],[1],[7,8,9,10]]) -- [3,6,1,10]



-- 11. Delete the last element of each sublist of a list.
-- E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] -> [[1,2],[5],[],[7,8,9]]
lastdel :: [[Int]] -> [[Int]]
lastdel ls = map(\x->take (length x-1) x ) ls

--main = print(lastdel [[1,2,3],[5,6],[7,8,9,10]]) -- [[1,2],[5],[7,8,9]]


-- 12. Instert 0 in front of every sublist of a list.
-- E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is 
-- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]
ins0 :: [[Int]] -> [[Int]]
ins0 ls =map(\x->0:x)ls

--main = print(ins0 [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]


-- insert 0 at the end!!
ins0' :: [[Int]] -> [[Int]]
ins0' ls = map(\x->x++[0]) ls 

--main = print(ins0' [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]


-- 13. Compute the squares of the elements of a list using map.
-- [1, 2, 3] -> [1, 4, 9]
sq :: Int -> Int
sq  x=x*x

sqrs :: [Int] -> [Int]
sqrs [] =[]
sqrs (x:xs) =sq x:sqrs xs

--main = print(sqrs [1..10]) -- [1,4,9,16,25,36,49,64,81,100]


-- 14. Same as 13. with lambda expression.
sqrs_lambda :: [Int] -> [Int]
sqrs_lambda ls = map(\x->x*x) ls

--main = print(sqrs_lambda [1..10]) -- [1,4,9,16,25,36,49,64,81,100]


-- review foldr
--main = print(foldr (+) 1 [4,5,6])  --(4 + (5 + (6 + 1))) 16


-- 15.  Add the numbers from 1..N (N positive and not 0) using foldr.
addn :: Int -> Int
addn ls =foldr (+) 0 [1..ls]

--main = print(addn 5) -- 15
--main = print(addn 0) -- 0
--main = print(addn (-2))  -- 0
--main = print(addn 10) --55


-- 16. Compute the product of the elements of a list using foldr.
prodf :: [Int] -> Int
prodf ls =foldr (*) 1 ls

prodf2 :: [Int] -> Int
prodf2 [] = 1
prodf2(x:xs) =x * prodf2 xs

--main = print(prodf [1,5,2,4]) -- 40
--main = print(prodf2 [1,5,2,4]) -- 40


-- 17. Compute 1*1 + 2*2 + ... + n*n  for n positive using map and foldr.
sumsqr :: Int -> Int
sumsqr x = foldr (+) 0 (map(\x->x*x) [1..x])

-- creates the [1..5] list
-- then maps x*x to every element from the list [1,4,9,16,25]
-- then sums up using foldr 55

--main = print(sumsqr 5) -- 55
--main = print(sumsqr 0) -- 0 -- the list is [1..0] which is empty, then foldr has as result 0
--main = print(sumsqr (-4)) -- 0 -- same here


-- 18. Write a function for the square of every element of a list and sublists.
-- [[1,2],[3,4,5,6],[7,8]]  ->  [[1,4],[9,16,25,36],[49,64]]  
-- hint: map in map
fa :: [Int]-> [Int]
fa ls =map(\x->x*x)ls 

--main = print(fa [1..5]) --[1,4,9,16,25]

f4 :: [[Int]] -> [[Int]]
f4 lsls =map(\x->fa x) lsls

f4' :: [[Int]] -> [[Int]]
f4' ls =map(\x->map(\a->a*a)x)ls

--main = print(f4 [[1,2],[3,4,5,6],[7,8]]) -- [[1,4],[9,16,25,36],[49,64]]
--main = print(f4' [[1,2],[3,4,5,6],[7,8]]) -- [[1,4],[9,16,25,36],[49,64]]


-- 19. Replicate n>0 times the element of a list e.g. n=3 [3..6] ->
-- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
rep :: Int -> Int -> [Int]
rep 1 num =[num]
rep cnt num  = [num] ++rep (cnt-1) num 

--main = print(rep 3 7) -- [7,7,7]
--main =print(rep 4 8)
rep1 :: Int -> Int -> [Int]
rep1 cnt num =replicate cnt num

--main = print(rep1 3 7) -- [7,7,7] 

f5 :: Int -> [Int] -> [[Int]]
f5 x ls  =map(\a->rep x a) ls

--main = print(f5 3 [3..6]) -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

f51 :: Int -> [Int] -> [[Int]]
f51 n xs = [rep n x | x <- xs]
--by using the list comprehension 
--main = print(f5 3 [3..6]) -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

--f52 :: Int -> [Int] -> [[Int]]


--main = print(f51 3 [3..6]) -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]


-- review filter
-- 20. Compute the double of the positive elements of a list [1, 2, -2, 3, -4] -> [2, 4, 6]
-- hints: first filter it then use map 
f20 :: [Int] -> [Int]
f20 ls  =map(\a->a*2) (filter(\x->x>0) ls)

--main = print(f20 [1, 2, (-2), 3, (-4)]) -- [2, 4, 6]

f20' :: [Int] -> [Int]
f20'[] =[]
f20'(x:xs)
 |x>0 =x*2:f20' xs
 |otherwise =f20' xs

--main = print(f20' [1, 2, (-2), 3, (-4)]) -- [2, 4, 6]


-- 21. Filter the elements smaller then n, e.g. n=3 [1,5,3,2,1,6,4,3,2,1] -> [1,2,1,2,1]
f7 :: Int -> [Int] -> [Int]
f7 num ls =filter(\x-> x < num ) ls
--main = print(f7 3 [1,5,3,2,1,6,4,3,2,1])  -- [1,2,1,2,1]


-- 22. Using notempty, eliminate the empty lists from a list of lists. 
-- [[1,2,3],[],[3,4,5],[2,2],[],[],[]] -> [[1,2,3], [3,4,5], [2,2]]

notempty :: [Int] -> Bool
notempty x = not (x == [])

f8 :: [[Int]] -> [[Int]]
f8 ls  =filter (\x -> notempty x) ls

--main = print(f8 [[1,2,3],[],[3,4,5],[2,2],[],[],[]]) -- [[1,2,3],[3,4,5],[2,2]]


-- 23. Compute the sum of the sublists using foldr [[1,2,3], [3,4,5], [2,2]] -> [6, 12, 4]
f9 :: [[Int]] -> [Int]
f9 ls =map(\x->foldr (+) 0 x) ls

--main = print(f9 [[1,2,3], [3,4,5], [2,2]]) -- [6,12,4]


-- 24. Write a function that keeps the integers of a list up to the first 0 encounterred 
-- and then divides by 2 every element [1, 2, -2, 3, 0, -4] -> [0, 1, -1, 1]
-- hints: use takeWhile then map
f3 :: [Int] -> [Int]
f3 ls = map(\x->x `div `2)(takeWhile(\x->x/=0) ls)

--main = print(f3 [1, 2, (-2), 3, 0, (-4)]) -- [0, 1, -1, 1]


-- 25. Insert the sum of elements of the sublist as last element in every sublist of a list.
insLast :: [Int] -> [Int] 
insLast ls =ls ++[sum (ls)]

--main =print(insLast[1,3,4])

insSum :: [[Int]] -> [[Int]]
insSum ls = map(\x->insLast x) ls

insSum2 :: [[Int]] -> [[Int]]
insSum2 ls  =map(\a-> a ++[sum(a)]) ls

--main = print(insSum [[1,2], [3,4,5], [6,5,9,7], [], [8]]) -- [[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]
--main = print(insSum2 [[1,2], [3,4,5], [6,5,9,7], [], [8]]) -- [[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]


-- 26. Write a function that checks if each elements in the list appear even times.
-- E.g. checkEven [1,1,2,2,2,2,3,5,3,5] = True  
checkAux :: [Int] -> Int -> Bool
checkAux ls a =even(length(map(\x->x==a) ls))

--main = print(checkAux  [1,1,1,4,5] 1)
checkEven :: [Int] -> Bool
checkEven ls = and (map(\x->checkAux ls x) ls)


--and only operates on the list of boolean values 

--main = print(checkEven [1,1,2,2,2,2,3,5,3,5]) -- True
--main = print(checkEven [1,1,2,2,1]) -- False
--main = print(checkEven []) -- True


-- 27. Insert x as second element in every sublist of a list.
-- if the sublist was empty then x will be the only element in the new sublist. 
-- [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10 -> [[1,10,2], [3,10,4,5], [6,10,5,9,7], [10], [8,10]]
insAux :: Int -> [Int]  -> [Int]
insAux num ls =take 1 ls ++ [num] ++drop 1 ls
 

--main =print(insAux 2 [1,3,4]) 

insertAtTwo :: [[Int]] -> Int -> [[Int]]
insertAtTwo ls num =map(\x->insAux num x) ls 

--main = print(insertAtTwo [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10)
-- [[1,10,2],[3,10,4,5],[6,10,5,9,7],[10],[8,10]]


-- 28. Given a list of lists, for each list, extract the first, middle and last element.
aux28::[Int] ->[Int]
aux28 ls
 |length ls<3= []
 |otherwise   =[head ls,ls!!x,last ls]
  where
   x= length ls `div` 2

austry::[Int] ->[Int]
austry ls =[head ls,ls!!x,last ls]
 where
  x=length ls `div` 2 
--main=print(austry [1,2,3,4,4,5])
extract3 :: [[Int]] -> [[Int]]
extract3 ls=map(\x-> aux28 x) ls

--main =print(last[1,2,3,4,5])
main = print(extract3 [[1..9], [2..6], [3..11], [1..10]]) -- [[1,5,9],[2,4,6],[3,7,11],[1,6,10]]
--main = print(extract3 []) --[]
