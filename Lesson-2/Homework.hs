-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y / z)

f2 :: Float -> Float -> Float -> Float
f2 x y z = sqrt (x / y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: [Int] -> [Int] -> [Int] -> Bool
f4 x y z = x == (y ++ z)

-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?

--   ANS
--     To make sure that the input and output have types that we want to
--     No execution when input output hace wrong defined types

-- Question 3
-- Why should you define type signatures for variables? How can they help you?

-- ANS
--   To control the type of variables
--   No allow to define wrong type of variables
--   Help us when develop the function with these variables are the parameters

-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.

-- ANS
-- There're some functions below:
--   show ::a -> String
--   read ::String -> a

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?

-- ANS
--   Yes we can
--   Example below:
a::[[Integer]]
a = [[1 ,2 ,3], [4, 5, 6]]

--   The way to access inner:
getElementByIndexes i j = a !! i !! j