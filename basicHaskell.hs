-- define a constant
integer  Int
integer = 42

answer  [Char]
answer = 42

-- simple square function
square  Int - Int
square x = x  x

-- verify equality between 3 parameters
allEquall  Int - Int - Int - Bool
allEquall a b c = (a == b) && (a == c)

-- return the biggest of two values
maxNumber  Int - Int - Int
maxNumber a b  a = b = a
               otherwise = b

-- add double
addDouble  Int - Int - Int
addDouble x y = 2  (x + y)

-- factorial function
factorial  Int - Int
factorial x  x == 0 = 1
             otherwise = x  (factorial (x-1))

-- return if 4 numbers are equall
all4Equall  Int - Int - Int - Int - Bool
all4Equall a b c d = (allEquall a b c) && (a == d)

-- return how many numbers are equall
howManyEquall  Int - Int - Int - Int
howManyEquall a b c  allEquall a b c = 3
                     (a == b)  (a == c)  (b == c) = 2
                     otherwise = 0

sales  Int - Int
sales n = n

{-totalSales  Int - Int
totalSales n  n == 0 = sales 0
              otherwise = totalSales (n-1) + sales n-}
             
-- pattern matching
totalSales  Int - Int
totalSales 0 = sales 0
totalSales n = totalSales (n-1) + sales n

maxSales  Int - Int
maxSales 0 = sales 0
maxSales n = maxNumber (maxSales(n-1)) (sales n)

makeSpaces  Int - String
makeSpaces 0 = 
makeSpaces n  n  0 =   ++ makeSpaces (n-1)

pushRight  Int - String - String
pushRight n s = s ++ (makeSpaces n)

main = do
     putStrLn $ integer =  ++ show integer 
     putStrLn $ answer =  ++ answer -- aparece só a string
     putStrLn $ answer =  ++ show (answer) -- aparece a string entre aspas
     putStrLn $ square =  ++ show (square 3) -- show square 3 não funciona sem parênteses
     putStrLn $ 1 = 1 = 1  ++ show (allEquall 1 1 1)
     putStrLn $ 1 = 2 = 1  ++ show (allEquall 1 2 1)
     putStrLn $ is 1 or 2 bigger  ++ show(maxNumber 1 2)
     putStrLn $ 2  (3 + 4) =  ++ show(addDouble 3 4)
     putStrLn $ 2  (14 + 2) =  ++ show(addDouble 2 (addDouble 3 4))
     putStrLn $ 5! =  ++ show(factorial 5)
     putStrLn $ 3 = 3 = 3 = 3  ++ show(all4Equall 3 3 3 3)
     putStrLn $ 3 = 4 = 3 = 4  ++ show(all4Equall 3 4 3 4)
     putStrLn $ how many equal 1 2 3 =  ++ show(howManyEquall 1 2 3)
     putStrLn $ how many equal 1 1 3 =  ++ show(howManyEquall 1 1 3)
     putStrLn $ how many equal 1 1 1 =  ++ show(howManyEquall 1 1 1)
     putStrLn $ totalSales 4 =  ++ show(totalSales 4)
     putStrLn $ maxSales 4 =   ++ show(maxSales 4)
     putStrLn $ a ++ (makeSpaces 3) ++ b
     putStrLn $ pushRight 3 Hello ++ World
