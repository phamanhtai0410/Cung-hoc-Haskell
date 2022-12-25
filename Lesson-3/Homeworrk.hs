module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

combineString :: String -> String -> String
combineString x y
  | length x > length y = y ++ x ++ y
  | length x == length y = ""
  | otherwise = x ++ y ++ x

-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly.
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
checkMonthlyConsumption :: Float -> Float -> Float -> String
checkMonthlyConsumption consumptionPerHour numberHoursPerDay maxAllowed
  | monthlyUsage == maxAllowed = "Equal to the allowed"
  | monthlyUsage > maxAllowed = "Bigger than the allowed"
  | otherwise = "Smaller than the allowed"
  where
    monthlyUsage = consumptionPerHour * numberHoursPerDay * 30

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.
-- In the previous function, return the excess/savings of consumption as part of the message.
checkMonthlyConsumptionAndExessOrSaving :: Float -> Float -> Float -> String
checkMonthlyConsumptionAndExessOrSaving consumptionPerHour numberHoursPerDay maxAllowed
  | monthlyUsage == maxAllowed = "Equal to the allowed"
  | monthlyUsage > maxAllowed = "Bigger than the allowed - Excess = " ++ (show (monthlyUsage - maxAllowed))
  | otherwise = "Smaller than the allowed - Savings = " ++ (show (maxAllowed - monthlyUsage))
  where
    monthlyUsage = consumptionPerHour * numberHoursPerDay * 30

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
splitSmaller :: Float -> Float -> Float -> Float
splitSmaller x y z =
  let func1 = x ** y
      func2 = y ** z
      func3 = z ** x
   in func1 + func2 + func3

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.
quotient :: Float -> Float -> String
quotient x y
  | x == 0 || y == 0 = "Division to zero is not possible"
  | otherwise =
    if x > y
      then show (x / y)
      else show (y / x)

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.
sumOfSquareProductAndQuotient :: Float -> Float -> Float
sumOfSquareProductAndQuotient x y
  | x == 0 || y == 0 = 0
  | otherwise = sumSquare
  where
    sumSquare =
      let _product = x * y
          _quotient = x / y
       in (_product * _product) + (_quotient * _quotient)

sumOfSquareProductAndQuotient' :: Float -> Float -> Float
sumOfSquareProductAndQuotient' x y
  | x == 0 || y == 0 = 0
  | otherwise =
    let squareProduct = squareProductInWhere
          where
            squareProductInWhere = (x * y) * (x * y)
        squareQuotient = squareQuotientInWhere
          where
            squareQuotientInWhere = (x / y) * (x / y)
     in squareProduct + squareQuotient
