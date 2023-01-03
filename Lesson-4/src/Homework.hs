-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

getFour :: [([Int], [Int])] -> Int
getFour [(x1, [x2_1, x2_2]), y] = x2_2

getFour' :: [([Int], [Int])] -> Int
getFour' [(_, [_, x]), _] = x
-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
checkLengthOfList :: [a] -> [a]
checkLengthOfList obj = case length obj >= 3 of 
    True -> []
    False -> obj

checkLengthOfList' :: [a] -> [a]
checkLengthOfList' obj = 
    let 
        checkLength x =
            if length x >= 3 then True else False
        remove x = []
    in if checkLength obj then remove obj else obj


-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
addThreeElementOfTuple :: (Int, Int, Int) -> Int
addThreeElementOfTuple (x, y, z) = x + y + z

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
checkListEmptyOrNot :: [a] -> Bool
checkListEmptyOrNot []          = True
checkListEmptyOrNot args        = False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
implementationOfTail :: [Int] -> [Int]
implementationOfTail    []      =   []
implementationOfTail    args    =   tail args

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)
increaseIfEven :: Int -> Int
increaseIfEven  a = case a `mod` 2 of
    0   ->      a + 1

