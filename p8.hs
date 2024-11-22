{-# LANGUAGE ParallelListComp #-}

e1 :: [(Int,Int)]
e1 = [(x,y) | x <- [1, 2], y <- [3, 4]]
--main = print(e1) --[(1,3),(1,4),(2,3),(2,4)]

e2 :: [(Int,Int)]
e2 = [(x,y) | x <- [1, 2] | y <- [3, 4]]
--main = print(e2) --[(1,3),(2,4)]

e3 :: [Int]
e3 = [x + y | x <- [1, 2], y <- [3, 4]]
--main = print(e3) --[4,5,5,6]

e4 :: [Int]
e4 = [x + y | x <- [1, 2] | y <- [3, 4]]
--main = print(e4) --[4,6]


--Task 1: Multiply Corresponding Elements
--Given two lists of numbers, compute a list where each element is the product of the corresponding elements of the two lists.
multiplyElements :: [Int] -> [Int] -> [Int]
multiplyElements ls1 ls2 = [a*b | a<-ls1 | b<-ls2] 
--main = print (multiplyElements [2, 3, 4] [5, 6, 7]) -- [10, 18, 28]

--Task 2: Combine Names and Scores 
--Given two lists—one of names and one of scores—combine them into a list of strings with the format "Name: Score".

combineNamesAndScores :: [String] -> [Int] -> [String]
combineNamesAndScores ls1 ls2 =[a ++ ":" ++  show(b) | a <- ls1 | b <- ls2]

--main = print (combineNamesAndScores ["Alice", "Bob", "Charlie"] [85, 92, 78])
-- Output: ["Alice: 85", "Bob: 92", "Charlie: 78"]

--Task 3: Compute Squares and Cubes 
--Given two lists, compute a list of tuples where the first element is the square of the number from the first list and the second element is the cube of the number from the second list.

computeSquaresAndCubes :: [Int] -> [Int] -> [(Int, Int)]
computeSquaresAndCubes ls1 ls2 =[(a*a ,b^3)| a <-ls1 |b <- ls2]

--main = print (computeSquaresAndCubes [1, 2, 3] [4, 5, 6])
-- Output: [(1, 64), (4, 125), (9, 216)]

--Task 4: Filtered Pairing
--Given two lists, create pairs of elements (x, y) where both elements satisfy a condition: x must be even, and y must be odd.

filteredPairs :: [Int] -> [Int] -> [(Int, Int)]
filteredPairs ls1 ls2 =[(x,y) |x <-ls1 , y<-ls2 ,even x && odd y]
main = print (filteredPairs [1, 2, 3, 4] [5, 6, 7, 8])
-- Output: [(2, 5), (4, 7)]

--Task 5: Weighted Sum of Two Lists  
--Given two lists of numbers, compute their weighted sum using a given list of weights. Each weight applies to both corresponding elements.

weightedSum :: [Double] -> [Double] -> [Double] -> [Double]
weightedSum xs ys ws = [w * (x + y) | (x, y, w) <- zip3 xs ys ws]

--main = print (weightedSum [1.0, 2.0, 3.0] [4.0, 5.0, 6.0] [0.5, 1.5, 2.0])

--main = print (weightedSum [1.0, 2.0, 3.0] [4.0, 5.0, 6.0] [0.5, 1.5, 2.0])
-- Output: [2.5, 10.5, 18.0]


