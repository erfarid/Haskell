{- Progress Task -}

{- Your neptun code : wefkhb-}

{-
Given a list of lists of integers and a target integer, return True 
if every element in each sublist is less than the target integer; otherwise, return False.

Example:
Input: [[1, 2, 3], [4, 5, 13], [7, 8, 9]], Target: 10
Output: False
(Explanation: One element  in second sublist is greater than 10.)

--}




helper :: [Int] -> Int -> Bool
helper [] _ = True
helper (x:xs) num = (x < num) && helper xs num

checkAllLess :: [[Int]] -> Int -> Bool
checkAllLess [] _ = True
checkAllLess (xs:xss) num = helper xs num && checkAllLess xss num


--main = print(checkAllLess [[1, 2, 3], [4, 5, 13], [7, 8, 9]] 10)  -- False
--main = print(checkAllLess [[10, 11, 12], [13, 14, 15]] 17) -- True
--main = print(checkAllLess [[2, 3, 4], [1, 5, 6]] 7) -- True
-- main = print(checkAllLess [[0, 1, 2], [3, 4, 5]] 1) -- False

