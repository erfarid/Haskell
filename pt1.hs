{- Progress Task 1 -}

{- Your neptune code :wefkhb -}

{- 
    Create a function getQuarter that takes an integer 
    (from 1 to 12) representing a month and returns the corresponding quarter of the year in English.
    If the input is 1, 2, or 3, return "First Quarter".
    If the input is 4, 5, or 6, return "Second Quarter".
    If the input is 7, 8, or 9, return "Third Quarter".
    If the input is 10, 11, or 12, return "Fourth Quarter".
    If the input is anything else, return "Invalid month".
-}


getQuarter :: Int -> String
getQuarter x
  | x >= 1 && x <= 3  = "First Quarter"
  | x >= 4 && x <= 6  = "Second Quarter"
  | x >= 7 && x <= 9  = "Third Quarter"
  | x >= 10 && x <= 12 = "Fourth Quarter"
  | otherwise      = "Invalid month"

--main = print (getQuarter 2)    -- "First Quarter"
--main = print (getQuarter 5)    -- "Second Quarter"
-- main = print (getQuarter 8)    -- "Third Quarter"
--main = print (getQuarter 11)   -- "Fourth Quarter"
--main = print (getQuarter 13)   -- "Invalid month"
paddingLast :: String -> Int -> String -> String
paddingLast ch x str
    | x <  0 = error"num must be >= 0"
    | otherwise = str ++ concat(replicate x ch)
main = print(paddingLast "*" 5 "apple") -- "apple*****"

