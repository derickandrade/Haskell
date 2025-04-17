-- define a constant
integer :: Int
integer = 42

answer :: String
answer = "42"

-- simple square function
square :: Int -> Int
square x = x * x

-- verify equality between 3 parameters
allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a == b) && (a == c)

-- return the biggest of two values
maxNumber :: Int -> Int -> Int
maxNumber a b = if a >= b then a else b

-- add double
addDouble :: Int -> Int -> Int
addDouble x y = 2 * (x + y)

-- factorial function
factorial :: Int -> Int
factorial x = if x == 0 then 1 else x * factorial (x - 1)

-- return if 4 numbers are equal
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (allEqual a b c) && (a == d)

-- return how many numbers are equal
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | allEqual a b c = 3
    | (a == b) || (a == c) || (b == c) = 2
    | otherwise = 0

sales :: Int -> Int
sales n = n

totalSales :: Int -> Int
totalSales 0 = sales 0
totalSales n = totalSales (n - 1) + sales n

maxSales :: Int -> Int
maxSales 0 = sales 0
maxSales n = maxNumber (maxSales (n - 1)) (sales n)

makeSpaces :: Int -> String
makeSpaces 0 = ""
makeSpaces n = " " ++ makeSpaces (n - 1)

pushRight :: Int -> String -> String
pushRight n s = makeSpaces n ++ s

averageSales :: Int -> Float
averageSales n = fromIntegral(totalSales n) / fromIntegral(n)

main = do
    putStrLn $ "integer = " ++ show integer 
    putStrLn $ "answer = " ++ answer -- only the string is shown
    putStrLn $ "answer = " ++ show answer -- the string is shown in quotation marks
    putStrLn $ "square = " ++ show (square 3)
    putStrLn $ "1 = 1 = 1 " ++ show (allEqual 1 1 1)
    putStrLn $ "1 = 2 = 1 " ++ show (allEqual 1 2 1)
    putStrLn $ "is 1 or 2 bigger " ++ show (maxNumber 1 2)
    putStrLn $ "2 * (3 + 4) = " ++ show (addDouble 3 4)
    putStrLn $ "2 * (14 + 2) = " ++ show (addDouble 2 (addDouble 3 4))
    putStrLn $ "5! = " ++ show (factorial 5)
    putStrLn $ "3 = 3 = 3 = 3 " ++ show (all4Equal 3 3 3 3)
    putStrLn $ "3 = 4 = 3 = 4 " ++ show (all4Equal 3 4 3 4)
    putStrLn $ "how many equal 1 2 3 = " ++ show (howManyEqual 1 2 3)
    putStrLn $ "how many equal 1 1 3 = " ++ show (howManyEqual 1 1 3)
    putStrLn $ "how many equal 1 1 1 = " ++ show (howManyEqual 1 1 1)
    putStrLn $ "totalSales 4 = " ++ show (totalSales 4)
    putStrLn $ "maxSales 4 = " ++ show (maxSales 4)
    putStrLn $ "a" ++ makeSpaces 3 ++ "b"
    putStrLn $ pushRight 3 "Hello" ++ " World"
    putStrLn $ "average sales 4 = " ++ show(averageSales 4)
