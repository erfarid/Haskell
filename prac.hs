import Data.Char
import Data.List
fibnum::Int ->Int
fibnum 0 =0
fibnum 1 =1
fibnum n =fibnum(n-1) + fibnum(n-2)

main =print(fibnum 6)

genNfib::Int ->[Int]
genNfib x =[fibnum a|a<-[0..x-1]]



--main =print(genNfib 5)
fibList :: [Int] -> [[Int]]
fibList ls =[genNfib a |a<-ls]

--main = print (fibList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
-- [[0],[0,1],[0,1,1],[0,1,1,2],[0,1,1,2,3],[0,1,1,2,3,5],
--  [0,1,1,2,3,5],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8],
--  [0,1,1,2,3,5,8]]