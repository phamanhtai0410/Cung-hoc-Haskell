-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?
-- ANSWER:
    --  The `Bounded` type class defines a set of methods for types that have an upper and lower bound. The Bounded type class provides the following two methods:

    --  minBound :: a: This method returns the minimum bound value of the type a.

    --  maxBound :: a: This method returns the maximum bound value of the type a.

    --  Note that the Bounded type class is a subclass of the Enum type class, so types that are instances of Bounded are also instances of Enum.

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.

-- ANSWER:
    --  `Int` and `Word` have some important differences.

    --  `Int` is a signed integer type, which means that it can represent both positive and negative integers. On the other hand, `Word` is an unsigned integer type, which means that it can only represent non-negative integers.

    --  In terms of their minBound and maxBound values, the differences between Int and Word can be seen. For a `Int` on most common platforms, minBound is -2147483648 (-2^31) and maxBound is 2147483647 (2^31 - 1), while for a `Word`, minBound is 0 and maxBound is 4294967295 (2^32 - 1). These values will depend on the number of bits used to represent each type on the specific platform.

    --  - 0 +
    -- - 2 ^ 31 => -1 => 2 ^ 31
    -- 0 => 1  
    -- 1 -> 2 ^ 31 -1 => 2^31 -1
    -- = 2 ^31 + 1 + 2^31 -1 = 2 x 2 ^31 => 2 ^32 => 32 bits

    -- 0 => 1
    -- 1 -> 2^32 -1
    -- = 2 ^32 => 32 bits

    --  In general, you would use Int when you need to represent signed integers and Word when you need to represent unsigned integers. The choice of which one to use depends on the specific requirements of your use case.

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?

-- ANSWER:
 In Haskell, the Enum type class provides a set of methods for types that have a concept of enumeration. The Enum type class provides the following methods:

     succ :: a -> a: This method returns the successor of a given value. 5 => 6

     pred :: a -> a: This method returns the predecessor of a given value. 5 => 4

     toEnum :: Int -> a: This method converts an Int value to an enumeration value of type a.

     fromEnum :: a -> Int: This method converts an enumeration value of type a to an Int value.

     enumFrom :: a -> [a]: This method returns an infinite list of values starting from a given value.

     enumFromThen :: a -> a -> [a]: This method returns an infinite list of values starting from the first given value and stepping by the second given value.
        enumFromThen 1 2 => [1, 3, 5, 7, ...]

     enumFromTo :: a -> a -> [a]: This method returns a finite list of values starting from the first given value and ending at the second given value.
        [1..9]
     enumFromThenTo :: a -> a -> a -> [a]: This method returns a finite list of values starting from the first given value, stepping by the second given value, and ending at the third given value.
    
    class Enum:
        succ()
        ...
    
    Int = Enum()
    Char = Enum()

For example, Bool is an instance of the Enum type class, so you can use `succ` and `pred` to find the successor and predecessor of a Bool value. You can also use `enumFrom` and `enumFromTo` to generate lists of Bool values.

Note that the Enum type class is a subclass of the Eq and Ord type classes, so types that are instances of Enum are also instances of Eq and Ord.

-- Question 4
-- Add type signatures to the functions below and use type variables and type classes.
-- Then uncomment the functions and try to compile.

-- f1 x y z = show (x / y) ++ z
-- f2 x = if x == maxBound then minBound else succ x

f1 :: (Fractional a, Show a) => a -> a -> String -> String
f1 x y z = show (x / y) ++ z

f2 :: (Bounded a, Eq a, Enum a) => a -> a
f2 x = if x == maxBound then minBound else succ x

-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.

-- ANSWER:
--  In Haskell, there are several numeric type classes that provide methods for changing between numeric types:

--      "Num": This is the most general numeric type class. It provides methods for addition, subtraction, multiplication, and negation. Types that are instances of Num include `Int`, Float, Double, and Complex.

--      "Integral": This type class is a subclass of Num and provides methods for integer arithmetic. Types that are instances of Integral include Int and Integer.

--      "Fractional": This type class is also a subclass of Num and provides methods for floating point arithmetic. Types that are instances of Fractional include Float and Double.

--      "Real": This type class provides methods for real numbers. Types that are instances of Real include Float, Double, and Integer.

--      "RealFrac": This type class is a subclass of Real and Fractional, and provides methods for real fractional numbers. Types that are instances of RealFrac include Float and Double.

--  Each of these type classes provides methods to convert between types that are instances of the same class. For example, you can use the fromIntegral method to convert an Int value to a Float value. Similarly, you can use the realToFrac method to convert a value of any type that is an instance of Real to a value of any type that is an instance of Fractional.